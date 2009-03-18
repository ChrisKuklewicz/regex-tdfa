-- | This is the non-capturing form of Text.Regex.TDFA.NewDFA.String
module Text.Regex.TDFA.NewDFA.Engine_NC(execMatch) where

import Control.Monad(when,forM,forM_,liftM2,foldM,join,MonadPlus(..),filterM)
import Data.Array.Base(unsafeRead,unsafeWrite,STUArray(..))
-- #ifdef __GLASGOW_HASKELL__
import GHC.Arr(STArray(..))
import GHC.ST(ST(..))
import GHC.Prim(MutableByteArray#,RealWorld,Int#,sizeofMutableByteArray#,unsafeCoerce#)
{-
-- #else
import Control.Monad.ST(ST)
import Data.Array.ST(STArray)
-- #endif
-}
import Prelude hiding ((!!))

import Data.Array.MArray(MArray(..),unsafeFreeze,getAssocs)
import Data.Array.IArray(Array,bounds,assocs)
--import qualified Data.Foldable as F
import qualified Data.IntMap.CharMap2 as CMap(lookup,findWithDefault)
import Data.IntMap(IntMap)
import qualified Data.IntMap as IMap(null,toList,lookup,insert,keys,member)
import Data.Ix(Ix,rangeSize,range)
import Data.Maybe(catMaybes,listToMaybe)
import Data.Monoid(Monoid(..))
--import Data.IntSet(IntSet)
import qualified Data.IntSet as ISet(toAscList,null)
import qualified Data.Array.ST
import Data.Array.IArray((!))
import qualified Data.Array.MArray as MA
import Data.List(partition,sort,foldl',sortBy,groupBy)
import Data.STRef
import qualified Control.Monad.ST.Lazy as L
import qualified Control.Monad.ST.Strict as S
import Data.Sequence(Seq,ViewL(..),viewl)
import qualified Data.Sequence as Seq
import qualified Data.ByteString.Char8 as SBS
import qualified Data.ByteString.Lazy.Char8 as LBS

import Text.Regex.Base(MatchArray,MatchOffset,MatchLength)
import qualified Text.Regex.TDFA.IntArrTrieSet as Trie
import Text.Regex.TDFA.Common hiding (indent)
import Text.Regex.TDFA.NewDFA.Uncons(Uncons(uncons))

-- import Debug.Trace

-- trace :: String -> a -> a
-- trace _ a = a

err :: String -> a
err s = common_error "Text.Regex.TDFA.NewDFA.Engine_NC"  s

{-# INLINE (!!) #-}
(!!) :: (MArray a e (S.ST s),Ix i) => a i e -> i -> S.ST s e
(!!) = MA.readArray -- unsafeRead
{-# INLINE set #-}
set :: (MArray a e (S.ST s),Ix i) => a i e -> i -> e -> S.ST s ()
set = MA.writeArray -- unsafeWrite

{-# SPECIALIZE execMatch :: Regex -> Position -> Char -> ([] Char) -> [MatchArray] #-}
{-# SPECIALIZE execMatch :: Regex -> Position -> Char -> (Seq Char) -> [MatchArray] #-}
{-# SPECIALIZE execMatch :: Regex -> Position -> Char -> SBS.ByteString -> [MatchArray] #-}
{-# SPECIALIZE execMatch :: Regex -> Position -> Char -> LBS.ByteString -> [MatchArray] #-}
execMatch :: Uncons text => Regex -> Position -> Char -> text -> [MatchArray]
execMatch (Regex { regex_dfa = (DFA {d_id=didIn,d_dt=dtIn})
                 , regex_init = startState
                 , regex_b_index = b_index
                 , regex_b_tags = b_tags_all
                 , regex_trie = trie
                 , regex_tags = aTags
                 , regex_groups = aGroups
                 , regex_compOptions = CompOption { multiline = newline }
                 , regex_execOptions = ExecOption { captureGroups = capture
                                                  , testMatch = _checkMatch }})
          offsetIn prevIn inputIn = L.runST runCaptureGroup where

  !test = mkTest newline         

  runCaptureGroup = {-# SCC "runCaptureGroup" #-} do
    obtainNext <- L.strictToLazyST constructNewEngine
    let loop = do vals <- L.strictToLazyST obtainNext
                  if null vals -- force vals before defining valsRest
                    then return []
                    else do valsRest <- loop
                            return (vals ++ valsRest)
    loop

  constructNewEngine :: S.ST s (S.ST s [MatchArray])
  constructNewEngine =  {-# SCC "constructNewEngine" #-} do
    storeNext <- newSTRef undefined
    writeSTRef storeNext (goNext storeNext)
    let obtainNext = join (readSTRef storeNext)
    return obtainNext

  goNext storeNext = {-# SCC "goNext" #-} do
    (SScratch s1In s2In winQ) <- newScratch b_index
    set s1In startState offsetIn
    writeSTRef storeNext (err "obtainNext called while goNext is running!")
    eliminatedStateFlag <- newSTRef False
    eliminatedRespawnFlag <- newSTRef False
    let next s1 s2 did dt offset prev input = {-# SCC "goNext.next" #-}
          case dt of
            Testing' {dt_test=wt,dt_a=a,dt_b=b} ->
              if test wt offset prev input
                then next s1 s2 did a offset prev input
                else next s1 s2 did b offset prev input
            Simple' {dt_win=w,dt_trans=t, dt_other=o}
              | IMap.null w ->
                  case uncons input of
                    Nothing -> finalizeWinners
                    Just (c,input') -> do
                      case CMap.findWithDefault o c t of
                        Transition {trans_many=DFA {d_id=did',d_dt=dt'},trans_how=dtrans} ->
                          findTrans s1 s2 did' dt' dtrans offset c input'
              | otherwise -> do
                  (did',dt') <- processWinner s1 did dt w offset
                  next' s1 s2 did' dt' offset prev input

        next' s1 s2 did dt offset prev input = {-# SCC "goNext'.next" #-}
          case dt of
            Testing' {dt_test=wt,dt_a=a,dt_b=b} ->
              if test wt offset prev input
                then next' s1 s2 did a offset prev input
                else next' s1 s2 did b offset prev input
            Simple' {dt_win=w,dt_trans=t, dt_other=o} ->
              case uncons input of
                Nothing -> finalizeWinners
                Just (c,input') -> do
                  case CMap.findWithDefault o c t of
                    Transition {trans_many=DFA {d_id=did',d_dt=dt'},trans_how=dtrans} ->
                      findTrans s1 s2 did' dt' dtrans offset c input'

        findTrans s1 s2 did' dt' dtrans offset prev' input' =  {-# SCC "goNext.findTrans" #-} do
          --
          let findTransTo (destIndex,sources) = do
                val <- if IMap.null sources then return (succ offset)
                         else return . minimum =<< mapM (s1 !!) (IMap.keys sources)
                set s2 destIndex val
                return val
          earlyStart <- fmap minimum $ mapM findTransTo (IMap.toList dtrans)
          --
          earlyWin <- readSTRef (mq_earliest winQ)
          if earlyWin < earlyStart
            then do
              winnersR <- getMQ earlyStart winQ
              writeSTRef storeNext (next s2 s1 did' dt' (succ offset) prev' input')
              mapM wsToGroup (reverse winnersR)
            else do
              let offset' = succ offset in seq offset' $ next s2 s1 did' dt' offset' prev' input'

        processWinner s1 did dt w offset = {-# SCC "goNext.newWinnerThenProceed" #-} do
          let getStart (sourceIndex,_) = s1 !! sourceIndex
          vals <- mapM getStart (IMap.toList w)
          let low = minimum vals   -- perhaps a non-empty winner
              high = maximum vals  -- perhaps an empty winner
          if low < offset
            then do
              putMQ (WScratch low offset) winQ
              when (high==offset || IMap.member startState w) $
                putMQ (WScratch offset offset) winQ
              let keepState i1 = do
                    startsAt <- s1 !! i1
                    let keep = (startsAt <= low) || (offset <= startsAt)
                    if keep
                      then return True
                      else if i1 == startState
                             then {- check for additional empty winner -}
                                  set s1 i1 (succ offset) >> return True
                             else writeSTRef eliminatedStateFlag True >> return False
              states' <- filterM keepState (ISet.toAscList did)
              flag <- readSTRef eliminatedStateFlag
              if flag
                then do
                  writeSTRef eliminatedStateFlag False
                  let DFA {d_id=did',d_dt=dt'} = Trie.lookupAsc trie states'
                  return (did',dt')
                else do
                  return (did,dt)
            else do
               -- offset == low == minimum vals == maximum vals == high; vals == [offset]
               putMQ (WScratch offset offset) winQ
               return (did,dt)

        finalizeWinners = do
          winnersR <- readSTRef (mq_list winQ)
          resetMQ winQ
          writeSTRef storeNext (return [])
          mapM wsToGroup (reverse winnersR)

    -- goNext then ends with the next statement
    next s1In s2In didIn dtIn offsetIn prevIn inputIn

----

{-# INLINE mkTest #-}
mkTest :: Uncons text => Bool -> WhichTest -> Index -> Char -> text -> Bool
mkTest isMultiline = if isMultiline then test_multiline else test_singleline
  where test_multiline Test_BOL _off prev _input = prev == '\n'
        test_multiline Test_EOL _off _prev input = case uncons input of
                                                     Nothing -> True
                                                     Just (next,_) -> next == '\n'
        test_singleline Test_BOL off _prev _input = off == 0
        test_singleline Test_EOL _off _prev input = case uncons input of
                                                      Nothing -> True
                                                      _ -> False

----

{- MUTABLE WINNER QUEUE -}

data MQ s = MQ { mq_earliest :: !(STRef s Position)
               , mq_list :: !(STRef s [WScratch])
               }

newMQ :: S.ST s (MQ s)
newMQ = do
  earliest <- newSTRef maxBound
  list <- newSTRef []
  return (MQ earliest list)

resetMQ :: MQ s -> S.ST s ()
resetMQ (MQ {mq_earliest=earliest,mq_list=list}) = do
  writeSTRef earliest maxBound
  writeSTRef list []

putMQ :: WScratch -> MQ s -> S.ST s ()
putMQ ws@(WScratch {ws_start=start,ws_stop=stop}) (MQ {mq_earliest=earliest,mq_list=list}) = do
  startE <- readSTRef earliest
  if start <= startE
    then writeSTRef earliest start >> writeSTRef list [ws]
    else do
      old <- readSTRef list
      let !rest = dropWhile (\ w -> start <= ws_start w) old 
          !new = ws : rest
      writeSTRef list new

getMQ :: Position -> MQ s -> ST s [WScratch]
getMQ pos (MQ {mq_earliest=earliest,mq_list=list}) = do
  old <- readSTRef list
  case span (\ w -> pos <= ws_start w) old of
    ([],ans) -> do
      writeSTRef earliest maxBound
      writeSTRef list []
      return ans
    (new,ans) -> do
      writeSTRef earliest (ws_start (last new))
      writeSTRef list new
      return ans

{- MUTABLE SCRATCH DATA STRUCTURES -}

data SScratch s = SScratch { _s_1 :: !(MScratch s)
                           , _s_2 :: !(MScratch s)
                           , _s_mq :: !(MQ s)
                           }
type MScratch s = STUArray s Index Position
data WScratch = WScratch {ws_start,ws_stop :: !Position}
  deriving Show

{- DEBUGGING HELPERS -}
{- CREATING INITIAL MUTABLE SCRATCH DATA STRUCTURES -}

{-# INLINE newA #-}
newA :: (MArray (STUArray s) e (ST s)) => (Tag,Tag) -> e -> S.ST s (STUArray s Tag e)
newA b_tags initial = newArray b_tags initial

{-# INLINE newA_ #-}
newA_ :: (MArray (STUArray s) e (ST s)) => (Tag,Tag) -> S.ST s (STUArray s Tag e)
newA_ b_tags = newArray_ b_tags

newScratch :: (Index,Index) -> S.ST s (SScratch s)
newScratch b_index = do
  s1 <- newMScratch b_index
  s2 <- newMScratch b_index
  winQ <- newMQ
  return (SScratch s1 s2 winQ)

newMScratch :: (Index,Index) -> S.ST s (MScratch s)
newMScratch b_index = newA b_index (-1)

{- CONVERT WINNERS TO MATCHARRAY -}

wsToGroup :: WScratch -> ST s MatchArray
wsToGroup (WScratch start stop) = do
  ma <- newArray (0,0) (start,stop-start)  :: ST s (STArray s Int (MatchOffset,MatchLength))
  unsafeFreeze ma

