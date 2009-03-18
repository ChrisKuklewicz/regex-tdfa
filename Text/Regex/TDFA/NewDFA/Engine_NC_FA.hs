-- | This is the non-capturing form of Text.Regex.TDFA.NewDFA.String
module Text.Regex.TDFA.NewDFA.Engine_NC_FA(execMatch) where

import Control.Monad(when,unless,forM,forM_,liftM2,foldM,join,MonadPlus(..),filterM)
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
import qualified Data.Foldable as F
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
import qualified Data.Array.MArray
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

--import Debug.Trace

-- trace :: String -> a -> a
-- trace _ a = a

err :: String -> a
err s = common_error "Text.Regex.TDFA.NewDFA"  s

{-# INLINE (!!) #-}
(!!) :: (MArray a e (S.ST s),Ix i) => a i e -> Int -> S.ST s e
(!!) = unsafeRead
{-# INLINE set #-}
set :: (MArray a e (S.ST s),Ix i) => a i e -> Int -> e -> S.ST s ()
set = unsafeWrite

{-# SPECIALIZE execMatch :: Regex -> Position -> Char -> ([] Char) -> [MatchArray] #-}
{-# SPECIALIZE execMatch :: Regex -> Position -> Char -> (Seq Char) -> [MatchArray] #-}
{-# SPECIALIZE execMatch :: Regex -> Position -> Char -> SBS.ByteString -> [MatchArray] #-}
{-# SPECIALIZE execMatch :: Regex -> Position -> Char -> LBS.ByteString -> [MatchArray] #-}
execMatch :: Uncons text => Regex -> Position -> Char -> text -> [MatchArray]
execMatch (Regex { regex_dfa = DFA {d_dt=dtIn} })
          offsetIn prevIn inputIn = S.runST goNext where

  test Test_BOL off _input = off == 0
  test Test_EOL _off input = case uncons input of
                               Nothing -> True
                               _ -> False

  goNext = {-# SCC "goNext" #-} do
    winQ <- newSTRef Nothing
    let next dt offset input = {-# SCC "goNext.next" #-}
          case dt of
            Testing' {dt_test=wt,dt_a=a,dt_b=b} ->
              if test wt offset input
                then next a offset input
                else next b offset input
            Simple' {dt_win=w,dt_trans=t, dt_other=o} -> do
              unless (IMap.null w) $
                writeSTRef winQ (Just offset)
              case uncons input of
                Nothing -> finalizeWinner
                Just (c,input') -> do
                  case CMap.findWithDefault o c t of
                    Transition {trans_single=DFA {d_id=did',d_dt=dt'}}
                      | ISet.null did' -> finalizeWinner
                      | otherwise ->
                          let offset' = succ offset
                          in seq offset' $ next dt' offset' input'

        finalizeWinner = do
          mWinner <- readSTRef winQ
          case mWinner of
            Nothing -> return []
            Just winner -> mapM (makeGroup offsetIn) [winner]

    next dtIn offsetIn inputIn

----

{- CONVERT WINNERS TO MATCHARRAY -}

makeGroup :: Position -> Position -> ST s MatchArray
makeGroup start stop = do
  ma <- newArray (0,0) (start,stop-start)  :: ST s (STArray s Int (MatchOffset,MatchLength))
  unsafeFreeze ma
