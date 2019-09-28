-- | This is the code for the main engine.  This captures the posix
-- subexpressions.  There is also a non-capturing engine, and a
-- testing engine.
-- 
-- It is polymorphic over the internal Uncons type class, and
-- specialized to produce the needed variants.
module Text.Regex.TDFA.NewDFA.Engine_FA(execMatch) where

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
import Control.Monad(when,unless,forM,forM_,liftM2,foldM)
import Data.Array.MArray(MArray(..))
import Data.Array.Unsafe(unsafeFreeze)
import Data.Array.IArray(Array,bounds,assocs,Ix(range))
import qualified Data.IntMap.CharMap2 as CMap(findWithDefault)
import Data.IntMap(IntMap)
import qualified Data.IntMap as IMap(null,toList,lookup,insert)
import Data.Maybe(catMaybes)
import Data.Monoid(Monoid(..))
import qualified Data.IntSet as ISet(toAscList,null)
import Data.Array.IArray((!))
import Data.List(sortBy,groupBy)
import Data.STRef(STRef,newSTRef,readSTRef,writeSTRef)
import qualified Control.Monad.ST.Strict as S(ST,runST)
import Data.Sequence(Seq,ViewL(..),viewl)
import qualified Data.Sequence as Seq(null)
import qualified Data.ByteString.Char8 as SBS(ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS(ByteString)

import Text.Regex.Base(MatchArray,MatchOffset,MatchLength)
import Text.Regex.TDFA.Common hiding (indent)
import Text.Regex.TDFA.NewDFA.Uncons(Uncons(uncons))
import Text.Regex.TDFA.NewDFA.MakeTest(test_singleline,test_multiline)

--import Debug.Trace

-- trace :: String -> a -> a
-- trace _ a = a

err :: String -> a
err s = common_error "Text.Regex.TDFA.NewDFA.Engine_FA"  s

{-# INLINE (!!) #-}
(!!) :: (MArray a e (S.ST s),Ix i) => a i e -> Int -> S.ST s e
(!!) = unsafeRead
{-# INLINE set #-}
set :: (MArray a e (S.ST s),Ix i) => a i e -> Int -> e -> S.ST s ()
set = unsafeWrite

noSource :: ((Index, Instructions),STUArray s Tag Position,OrbitLog)
noSource = ((-1,err "noSource"),err "noSource",err "noSource")
 
{-# SPECIALIZE execMatch :: Regex -> Position -> Char -> ([] Char) -> [MatchArray] #-}
{-# SPECIALIZE execMatch :: Regex -> Position -> Char -> (Seq Char) -> [MatchArray] #-}
{-# SPECIALIZE execMatch :: Regex -> Position -> Char -> SBS.ByteString -> [MatchArray] #-}
{-# SPECIALIZE execMatch :: Regex -> Position -> Char -> LBS.ByteString -> [MatchArray] #-}
execMatch :: Uncons text => Regex -> Position -> Char -> text -> [MatchArray]
execMatch (Regex { regex_dfa =  DFA {d_id=didIn,d_dt=dtIn}
                 , regex_init = startState
                 , regex_b_index = b_index
                 , regex_b_tags = b_tags_all
                 , regex_tags = aTags
                 , regex_groups = aGroups
                 , regex_compOptions = CompOption { multiline = newline } } )
          offsetIn prevIn inputIn = S.runST goNext where

  b_tags :: (Tag,Tag)
  !b_tags = b_tags_all

  orbitTags :: [Tag]
  !orbitTags = map fst . filter ((Orbit==).snd) . assocs $ aTags

  !test = mkTest newline         

  comp :: C s
  comp = {-# SCC "matchHere.comp" #-} ditzyComp'3 aTags

  goNext :: ST s [MatchArray]
  goNext = {-# SCC "goNext" #-} do
    (SScratch s1In s2In (winQ,blank,which)) <- newScratch b_index b_tags
    spawnAt b_tags blank startState s1In offsetIn
    let next s1 s2 did dt offset prev input = {-# SCC "goNext.next" #-}
          case dt of
            Testing' {dt_test=wt,dt_a=a,dt_b=b} ->
              if test wt offset prev input
                then next s1 s2 did a offset prev input
                else next s1 s2 did b offset prev input
            Simple' {dt_win=w,dt_trans=t,dt_other=o} -> do
              unless (IMap.null w) $
                processWinner s1 w offset
              case uncons input of
                Nothing -> finalizeWinner
                Just (c,input') ->
                  case CMap.findWithDefault o c t of
                    Transition {trans_single=DFA {d_id=did',d_dt=dt'},trans_how=dtrans}
                      | ISet.null did' -> finalizeWinner
                      | otherwise -> findTrans s1 s2 did did' dt' dtrans offset c input'

-- compressOrbits gets all the current Tag-0 start information from
-- the NFA states; then it loops through all the Orbit tags with
-- compressOrbit.
--
-- compressOrbit on such a Tag loops through all the NFS states'
-- m_orbit record, discardind ones that are Nothing and discarding
-- ones that are too new to care about (after the cutoff value).
--
-- compressOrbit then groups the Orbits records by the Tag-0 start
-- position and the basePos position.  Entried in different groups
-- will never be comparable in the future so they can be processed
-- separately.  Groups could probably be even more finely
-- distinguished, as a futher optimization, but the justification will
-- be tricky.
--
-- Current Tag-0 values are at most offset and all newly spawned
-- groups will have Tag-0 of at least (succ offset) so the current
-- groups are closed to those spawned in the future.  The basePos may
-- be as large as offset and may be overwritten later with values of
-- offset or larger (and this will also involve deleting the Orbits
-- record).  Thus there could be a future collision between a current
-- group with basePos==offset and an updated record that acquires
-- basePos==offset.  By excluding groups with basePos before the
-- current offset the collision between existing and future records
-- is avoided.
--
-- An entry in a group can only collide with that group's
-- descendents. compressOrbit sends each group to the compressGroup
-- command.
--
-- compressGroup on a single record checks whether it's Seq can be
-- cleared and if so it will clear it (and set ordinal to Nothing but
-- this this not particularly important).
--
-- compressGroup on many records sorts and groups the members and zips
-- the groups with their new ordinal value.  The comparision is based
-- on the old ordinal value, then the inOrbit value, and then the (Seq
-- Position) data.
--
-- The old ordinals of the group will all be Nothing or all be Just,
-- but this condition is neither checked nor violations detected.
-- This comparision is justified because once records get different
-- ordinals assigned they will never change places.
--
-- The inOrbit Bool is only different if one of them has set the stop
-- position to at most (succ offset).  They will obly be compared if
-- the other one leaves, an its stop position will be at least offset.
-- The previous sentence is justified by inspectin of the "assemble"
-- function in the TDFA module: there is no (PostUpdate
-- LeaveOrbitTask) so the largest possible value for the stop Tag is
-- (pred offset). Thus the record with inOrbit==False would beat (be
-- GT than) the record with inOrbit==True.
--
-- The Seq comparison is safe because the largest existing Position
-- value is (pred offset) and the smallest future Position value is
-- offset.  The previous sentence is justified by inspectin of the
-- "assemble" function in the TDFA module: there is no (PostUpdate
-- EnterOrbitTags) so the largest possible value in the Seq is (pred
-- offset).
--
-- The updated Orbits get the new ordinal value and an empty (Seq
-- Position).

        compressOrbits s1 did offset = do
          let getStart state = do start <- maybe (err "compressOrbit,1") (!! 0) =<< m_pos s1 !! state
                                  return (state,start)
              cutoff = offset - 50 -- Require: cutoff <= offset, MAGIC TUNABLE CONSTANT 50
          ss <- mapM getStart (ISet.toAscList did)
          let compressOrbit tag = do
                mos <- forM ss ( \ p@(state,_start) -> do
                                  mo <- fmap (IMap.lookup tag) (m_orbit s1 !! state)
                                  case mo of
                                    Just orbits | basePos orbits < cutoff -> return (Just (p,orbits))
                                                | otherwise -> return Nothing
                                    _ -> return Nothing )
                let compressGroup [((state,_),orbit)] | Seq.null (getOrbits orbit) = return ()
                                                      | otherwise =
                      set (m_orbit s1) state 
                      . (IMap.insert tag $! (orbit { ordinal = Nothing, getOrbits = mempty}))
                      =<< m_orbit s1 !! state

                    compressGroup gs = do
                      let sortPos (_,b1) (_,b2) = compare (ordinal b1) (ordinal b2) `mappend`
                                                  compare (inOrbit b2) (inOrbit b1) `mappend`
                                                  comparePos (viewl (getOrbits b1)) (viewl (getOrbits b2))
                          groupPos (_,b1) (_,b2) = ordinal b1 == ordinal b2 && getOrbits b1 == getOrbits b2
                          gs' = zip [(1::Int)..] (groupBy groupPos . sortBy sortPos $ gs)
                      forM_ gs' $ \ (!n,eqs) -> do
                        forM_ eqs $ \ ((state,_),orbit) ->
                          set (m_orbit s1) state
                           . (IMap.insert tag $! (orbit { ordinal = Just n, getOrbits = mempty }))
                            =<< m_orbit s1 !! state
                let sorter ((_,a1),b1) ((_,a2),b2) = compare a1 a2 `mappend` compare (basePos b1) (basePos b2)
                    grouper ((_,a1),b1) ((_,a2),b2) = a1==a2 && basePos b1 == basePos b2
                    orbitGroups = groupBy grouper . sortBy sorter . catMaybes $ mos
                mapM_ compressGroup orbitGroups
          mapM_ compressOrbit orbitTags

-- findTrans has to (part 1) decide, for each destination, "which" of
-- zero or more source NFA states will be the chosen source.  Then it
-- has to (part 2) perform the transition or spawn.  It keeps track of
-- the starting index while doing so, and compares the earliest start
-- with the stored winners.  (part 3) If some winners are ready to be
-- released then the future continuation of the search is placed in
-- "storeNext".  If no winners are ready to be released then the
-- computation continues immediately.

        findTrans s1 s2 did did' dt' dtrans offset prev' input' =  {-# SCC "goNext.findTrans" #-} do
          -- findTrans part 0
          -- MAGIC TUNABLE CONSTANT 100 (and 100-1). TODO: (offset .&. 127 == 127) instead?
          when (not (null orbitTags) && (offset `rem` 100 == 99)) (compressOrbits s1 did offset)
          -- findTrans part 1
          let findTransTo (destIndex,sources) | IMap.null sources =
                set which destIndex noSource
                                              | otherwise = do
                let prep (sourceIndex,(_dopa,instructions)) = {-# SCC "goNext.findTrans.prep" #-} do
                      pos <- maybe (err $ "findTrans,1 : "++show (sourceIndex,destIndex,did')) return
                               =<< m_pos s1 !! sourceIndex
                      orbit <- m_orbit s1 !! sourceIndex
                      let orbit' = maybe orbit (\ f -> f offset orbit) (newOrbits instructions)
                      return ((sourceIndex,instructions),pos,orbit')
                    challenge x1@((_si1,ins1),_p1,_o1) x2@((_si2,ins2),_p2,_o2) = {-# SCC "goNext.findTrans.challenge" #-} do
                      check <- comp offset x1 (newPos ins1) x2 (newPos ins2)
                      if check==LT then return x2 else return x1
                (first:rest) <- mapM prep (IMap.toList sources)
                set which destIndex =<< foldM challenge first rest
          let dl = IMap.toList dtrans
          mapM_ findTransTo dl
          -- findTrans part 2
          let performTransTo (destIndex,_sources) = {-# SCC "goNext.findTrans.performTransTo" #-} do
                x@((sourceIndex,_instructions),_pos,_orbit') <- which !! destIndex
                unless (sourceIndex == (-1)) $
                  (updateCopy x offset s2 destIndex)
          mapM_ performTransTo dl
          -- findTrans part 3
          let offset' = succ offset in seq offset' $ next s2 s1 did' dt' offset' prev' input'

-- The "newWinnerThenProceed" can find both a new non-empty winner and
-- a new empty winner.  A new non-empty winner can cause some of the
-- NFA states that comprise the DFA state to be eliminated, and if the
-- startState is eliminated then it must then be respawned.  And
-- imperative flag setting and resetting style is used.
--
-- A non-empty winner from the startState might obscure a potential
-- empty winner (form the startState at the current offset).  This
-- winEmpty possibility is also checked for. (unit test pattern ".*")
-- (futher test "(.+|.+.)*" on "aa\n")

        {-# INLINE processWinner #-}
        processWinner s1 w offset = {-# SCC "goNext.newWinnerThenProceed" #-} do
          let prep x@(sourceIndex,instructions) = {-# SCC "goNext.newWinnerThenProceed.prep" #-} do
                pos <- maybe (err "newWinnerThenProceed,1") return =<< m_pos s1 !! sourceIndex
                startPos <- pos !! 0
                orbit <- m_orbit s1 !! sourceIndex
                let orbit' = maybe orbit (\ f -> f offset orbit) (newOrbits instructions)
                return (startPos,(x,pos,orbit'))
              challenge x1@((_si1,ins1),_p1,_o1) x2@((_si2,ins2),_p2,_o2) = {-# SCC "goNext.newWinnerThenProceed.challenge" #-} do
                check <- comp offset x1 (newPos ins1) x2 (newPos ins2)
                if check==LT then return x2 else return x1
          prep'd <- mapM prep (IMap.toList w)
          case map snd prep'd of
            [] -> return ()
            (first:rest) -> newWinner offset =<< foldM challenge first rest

        newWinner preTag ((_sourceIndex,winInstructions),oldPos,_newOrbit) = {-# SCC "goNext.newWinner" #-} do
          newerPos <- newA_ b_tags
          copySTU oldPos newerPos
          doActions preTag newerPos (newPos winInstructions)
          putMQ (WScratch newerPos) winQ

        finalizeWinner = do
          mWinner <- readSTRef (mq_mWin winQ)
          case mWinner of
            Nothing -> return []
            Just winner -> resetMQ winQ >> mapM (tagsToGroupsST aGroups) [winner]

    -- goNext then ends with the next statement
    next s1In s2In didIn dtIn offsetIn prevIn inputIn

{-# INLINE doActions #-}
doActions :: Position -> STUArray s Tag Position -> [(Tag, Action)] -> ST s ()
doActions preTag pos ins = mapM_ doAction ins where
  postTag = succ preTag
  doAction (tag,SetPre) = set pos tag preTag
  doAction (tag,SetPost) = set pos tag postTag
  doAction (tag,SetVal v) = set pos tag v

----

{-# INLINE mkTest #-}
mkTest :: Uncons text => Bool -> WhichTest -> Index -> Char -> text -> Bool
mkTest isMultiline = if isMultiline then test_multiline else test_singleline

----

{- MUTABLE WINNER QUEUE -}

newtype MQ s = MQ { mq_mWin :: STRef s (Maybe (WScratch s)) }

newMQ :: S.ST s (MQ s)
newMQ = do
  mWin <- newSTRef Nothing
  return (MQ mWin)

resetMQ :: MQ s -> S.ST s ()
resetMQ (MQ {mq_mWin=mWin}) = do
  writeSTRef mWin Nothing

putMQ :: WScratch s -> MQ s -> S.ST s ()
putMQ ws (MQ {mq_mWin=mWin}) = do
  writeSTRef mWin (Just ws)

{- MUTABLE SCRATCH DATA STRUCTURES -}

data SScratch s = SScratch { _s_1 :: !(MScratch s)
                           , _s_2 :: !(MScratch s)
                           , _s_rest :: !( MQ s
                                        , BlankScratch s
                                        , STArray s Index ((Index,Instructions),STUArray s Tag Position,OrbitLog)
                                        )
                           }
data MScratch s = MScratch { m_pos :: !(STArray s Index (Maybe (STUArray s Tag Position)))
                           , m_orbit :: !(STArray s Index OrbitLog)
                           }
newtype BlankScratch s = BlankScratch { _blank_pos :: (STUArray s Tag Position)
                                      }
newtype WScratch s = WScratch { w_pos :: (STUArray s Tag Position)
                              }

{- DEBUGGING HELPERS -}

{-
indent :: String -> String
indent xs = ' ':' ':xs

showMS :: MScratch s -> Index -> ST s String
showMS s i = do
  ma <- m_pos s !! i
  mc <- m_orbit s !! i
  a <- case ma of
        Nothing -> return "No pos"
        Just pos -> fmap show (getAssocs pos)
  let c = show mc
  return $ unlines [ "MScratch, index = "++show i
                   , indent a
                   , indent c]

showWS :: WScratch s -> ST s String
showWS (WScratch pos) = do
  a <- getAssocs pos
  return $ unlines [ "WScratch" 
                   , indent (show a)]
-}
{- CREATING INITIAL MUTABLE SCRATCH DATA STRUCTURES -}

{-# INLINE newA #-}
newA :: (MArray (STUArray s) e (ST s)) => (Tag,Tag) -> e -> S.ST s (STUArray s Tag e)
newA b_tags initial = newArray b_tags initial

{-# INLINE newA_ #-}
newA_ :: (MArray (STUArray s) e (ST s)) => (Tag,Tag) -> S.ST s (STUArray s Tag e)
newA_ b_tags = newArray_ b_tags

newScratch :: (Index,Index) -> (Tag,Tag) -> S.ST s (SScratch s)
newScratch b_index b_tags = do
  s1 <- newMScratch b_index
  s2 <- newMScratch b_index
  winQ <- newMQ
  blank <- fmap BlankScratch (newA b_tags (-1))
  which <- (newArray b_index ((-1,err "newScratch which 1"),err "newScratch which 2",err "newScratch which 3"))
  return (SScratch s1 s2 (winQ,blank,which))

newMScratch :: (Index,Index) -> S.ST s (MScratch s)
newMScratch b_index = do
  pos's <- newArray b_index Nothing
  orbit's <- newArray b_index mempty
  return (MScratch pos's orbit's)

{- COMPOSE A FUNCTION CLOSURE TO COMPARE TAG VALUES -}

newtype F s = F ([F s] -> C s)
type C s = Position
        -> ((Int, Instructions), STUArray s Tag Position, IntMap Orbits)
        -> [(Int, Action)]
        -> ((Int, Instructions), STUArray s Tag Position, IntMap Orbits)
        -> [(Int, Action)]
        -> ST s Ordering

{-# INLINE orderOf #-}
orderOf :: Action -> Action -> Ordering
orderOf post1 post2 =
  case (post1,post2) of
    (SetPre,SetPre) -> EQ
    (SetPost,SetPost) -> EQ
    (SetPre,SetPost) -> LT
    (SetPost,SetPre) -> GT
    (SetVal v1,SetVal v2) -> compare v1 v2
    _ -> err $ "bestTrans.compareWith.choose sees incomparable "++show (post1,post2)

ditzyComp'3 :: forall s. Array Tag OP -> C s
ditzyComp'3 aTagOP = comp0 where
  (F comp1:compsRest) = allcomps 1

  comp0 :: C s
  comp0 preTag x1@(_state1,pos1,_orbit1') np1 x2@(_state2,pos2,_orbit2') np2 = do
    c <- liftM2 compare (pos2!!0) (pos1!!0) -- reversed since Minimize
    case c of
      EQ -> comp1 compsRest preTag x1 np1 x2 np2
      answer -> return answer

  allcomps :: Tag -> [F s]
  allcomps tag | tag > top = [F (\ _ _ _ _ _ _ -> return EQ)]
               | otherwise = 
    case aTagOP ! tag of
      Orbit -> F (challenge_Orb tag) : allcomps (succ tag)
      Maximize -> F (challenge_Max tag) : allcomps (succ tag)
      Ignore -> F (challenge_Ignore tag) : allcomps (succ tag)
      Minimize -> err "allcomps Minimize"
   where top = snd (bounds aTagOP)

  challenge_Ignore !tag (F next:comps) preTag x1 np1 x2 np2 =
    case np1 of
      ((t1,_):rest1) | t1==tag ->
        case np2 of
          ((t2,_):rest2) | t2==tag -> next comps preTag x1 rest1 x2 rest2
          _ -> next comps preTag x1 rest1 x2 np2
      _ -> do
        case np2 of
          ((t2,_):rest2) | t2==tag -> next comps preTag x1 np1 x2 rest2
          _ ->  next comps preTag x1 np1 x2 np2
  challenge_Ignore _ [] _ _ _ _ _ = err "impossible 2347867"

  challenge_Max !tag (F next:comps) preTag x1@(_state1,pos1,_orbit1') np1 x2@(_state2,pos2,_orbit2') np2 =
    case np1 of
      ((t1,b1):rest1) | t1==tag ->
        case np2 of
          ((t2,b2):rest2) | t2==tag ->
            if b1==b2 then next comps preTag x1 rest1 x2 rest2
              else return (orderOf b1 b2)
          _ -> do
            p2 <- pos2 !! tag
            let p1 = case b1 of SetPre -> preTag
                                SetPost -> succ preTag
                                SetVal v -> v
            if p1==p2 then next comps preTag x1 rest1 x2 np2
              else return (compare p1 p2)
      _ -> do
        p1 <- pos1 !! tag
        case np2 of
          ((t2,b2):rest2) | t2==tag -> do
            let p2 = case b2 of SetPre -> preTag
                                SetPost -> succ preTag
                                SetVal v -> v
            if p1==p2 then next comps preTag x1 np1 x2 rest2
              else return (compare p1 p2)
          _ -> do
            p2 <- pos2 !! tag
            if p1==p2 then next comps preTag x1 np1 x2 np2
              else return (compare p1 p2)
  challenge_Max _ [] _ _ _ _ _ = err "impossible 9384324"

  challenge_Orb !tag (F next:comps) preTag x1@(_state1,_pos1,orbit1') np1 x2@(_state2,_pos2,orbit2') np2 = 
    let s1 = IMap.lookup tag orbit1'
        s2 = IMap.lookup tag orbit2'
    in case (s1,s2) of
         (Nothing,Nothing) -> next comps preTag x1 np1 x2 np2
         (Just o1,Just o2) | inOrbit o1 == inOrbit o2 ->
            case compare (ordinal o1) (ordinal o2) `mappend`
                 comparePos (viewl (getOrbits o1)) (viewl (getOrbits o2)) of
              EQ -> next comps preTag x1 np1 x2 np2
              answer -> return answer
         _ -> err $ unlines [ "challenge_Orb is too stupid to handle mismatched orbit data :"
                           , show(tag,preTag,np1,np2)
                           , show s1
                           , show s2
                           ]
  challenge_Orb _ [] _ _ _ _ _ = err "impossible 0298347"

comparePos :: (ViewL Position) -> (ViewL Position) -> Ordering
comparePos EmptyL EmptyL = EQ
comparePos EmptyL _      = GT
comparePos _      EmptyL = LT
comparePos (p1 :< ps1) (p2 :< ps2) = 
  compare p1 p2 `mappend` comparePos (viewl ps1) (viewl ps2)

{- CONVERT WINNERS TO MATCHARRAY -}

tagsToGroupsST :: forall s. Array GroupIndex [GroupInfo] -> WScratch s -> S.ST s MatchArray
tagsToGroupsST aGroups (WScratch {w_pos=pos})= do
  let b_max = snd (bounds (aGroups))
  ma <- newArray (0,b_max) (-1,0) :: ST s (STArray s Int (MatchOffset,MatchLength))
  startPos0 <- pos !! 0
  stopPos0 <- pos !! 1
  set ma 0 (startPos0,stopPos0-startPos0)
  let act _this_index [] = return ()
      act this_index ((GroupInfo _ parent start stop flagtag):gs) = do
        flagVal <- pos !! flagtag
        if (-1) == flagVal then act this_index gs
          else do
        startPos <- pos !! start
        stopPos <- pos !! stop
        (startParent,lengthParent) <- ma !! parent
        let ok = (0 <= startParent &&
                  0 <= lengthParent &&
                  startParent <= startPos &&
                  stopPos <= startPos + lengthParent)
        if not ok then act this_index gs
          else set ma this_index (startPos,stopPos-startPos)
  forM_ (range (1,b_max)) $ (\i -> act i (aGroups!i))
  unsafeFreeze ma

{- MUTABLE TAGGED TRANSITION (returning Tag-0 value) -}

{-# INLINE spawnAt #-}
-- Reset the entry at "Index", or allocate such an entry.
-- set tag 0 to the "Position"
spawnAt :: (Tag,Tag) -> BlankScratch s -> Index -> MScratch s -> Position -> S.ST s ()
spawnAt b_tags (BlankScratch blankPos) i s1 thisPos = do
  oldPos <- m_pos s1 !! i
  pos <- case oldPos of
           Nothing -> do
             pos' <- newA_ b_tags
             set (m_pos s1) i (Just pos')
             return pos'
           Just pos -> return pos
  copySTU blankPos pos
  set (m_orbit s1) i $! mempty
  set pos 0 thisPos

{-# INLINE updateCopy #-}
updateCopy :: ((Index, Instructions), STUArray s Tag Position, OrbitLog)
           -> Index
           -> MScratch s
           -> Int
           -> ST s ()
updateCopy ((_i1,instructions),oldPos,newOrbit) preTag s2 i2 = do
  b_tags <- getBounds oldPos
  newerPos <- maybe (do
    a <- newA_ b_tags
    set (m_pos s2) i2 (Just a)
    return a) return =<< m_pos s2 !! i2
  copySTU oldPos newerPos
  doActions preTag newerPos (newPos instructions)
  set (m_orbit s2) i2 $! newOrbit

{- USING memcpy TO COPY STUARRAY DATA -}

-- #ifdef __GLASGOW_HASKELL__
foreign import ccall unsafe "memcpy"
    memcpy :: MutableByteArray# RealWorld -> MutableByteArray# RealWorld -> Int# -> IO ()

{-
Prelude Data.Array.Base> :i STUArray
data STUArray s i e
  = STUArray !i !i !Int (GHC.Prim.MutableByteArray# s)
  -- Defined in Data.Array.Base
-}
-- This has been updated for ghc 6.8.3 and still works with ghc 6.10.1
{-# INLINE copySTU #-}
copySTU :: (Show i,Ix i,MArray (STUArray s) e (S.ST s)) => STUArray s i e -> STUArray s i e -> S.ST s () -- (STUArray s i e)
copySTU _souce@(STUArray _ _ _ msource) _destination@(STUArray _ _ _ mdest) =
-- do b1 <- getBounds s1
--  b2 <- getBounds s2
--  when (b1/=b2) (error ("\n\nWTF copySTU: "++show (b1,b2)))
  ST $ \s1# ->
    case sizeofMutableByteArray# msource        of { n# ->
    case unsafeCoerce# memcpy mdest msource n# s1# of { (# s2#, () #) ->
    (# s2#, () #) }}
{-
#else /* !__GLASGOW_HASKELL__ */

copySTU :: (MArray (STUArray s) e (S.ST s))=> STUArray s Tag e -> STUArray s Tag e -> S.ST s (STUArray s i e)
copySTU source destination = do
  b@(start,stop) <- getBounds source
  b' <- getBounds destination
  -- traceCopy ("> copySTArray "++show b) $ do
  when (b/=b') (fail $ "Text.Regex.TDFA.RunMutState copySTUArray bounds mismatch"++show (b,b'))
  forM_ (range b) $ \index ->
    set destination index =<< source !! index
  return destination
#endif /* !__GLASGOW_HASKELL__ */
-}
