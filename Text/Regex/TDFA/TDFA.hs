-- | "Text.Regex.TDFA.TDFA" converts the QNFA from TNFA into the DFA.
-- A DFA state corresponds to a Set of QNFA states, repesented as list
-- of Index which are used to lookup the DFA state in a lazy Trie
-- which holds all possible subsets of QNFA states.
module Text.Regex.TDFA.TDFA(patternToDFA,DFA(..),DT(..)
                           ,examineDFA,isDFAFrontAnchored
                           ,nfaToDFA,dfaMap) where

--import Control.Arrow((***))
import Control.Monad.Instances()
import Control.Monad.RWS
import Data.Array.IArray(Array,(!),bounds)
import Data.IntMap(IntMap)
import qualified Data.IntMap as IMap
import qualified Data.IntSet as ISet(empty,singleton,null)
import Data.List(foldl')
import Data.IntMap.EnumMap(EnumMap)
import qualified Data.IntMap.EnumMap as EMap(elems,insert,member,empty,toAscList,fromDistinctAscList)
import Data.IntMap.CharMap(CharMap)
import qualified Data.IntMap.CharMap as Map(elems,insert,member,empty,toAscList,fromDistinctAscList)
import qualified Data.Map
import Data.Maybe(isJust)

import Text.Regex.TDFA.Common
import Text.Regex.TDFA.IntArrTrieSet(TrieSet)
import qualified Text.Regex.TDFA.IntArrTrieSet as Trie(lookupAsc,fromSinglesMerge)
import Text.Regex.TDFA.Pattern(Pattern)
import Text.Regex.TDFA.RunMutState(compareWith,toInstructions)
import Text.Regex.TDFA.TNFA(patternToNFA)
-- import Debug.Trace

{- By Chris Kuklewicz, 2007. BSD License, see the LICENSE file. -}

err :: String -> a
err s = common_error "Text.Regex.TDFA.TDFA"  s

dlose :: DFA
dlose = DFA { d_id = ISet.empty
            , d_dt = Simple' { dt_win = IMap.empty
                             , dt_trans = Map.empty
                             , dt_other = Nothing } }

{-
-- Specilized utility
ungroupBy :: (a->x) -> ([a]->y) -> [[a]] -> [(x,y)]
ungroupBy f g = map helper where
  helper [] = (err "empty group passed to ungroupBy",g [])
  helper x@(x1:_) = (f x1,g x)
-}
-- dumb smart constructor for tracing construction (I wanted to monitor laziness)
{-# INLINE makeDFA #-}
makeDFA :: SetIndex -> DT -> DFA
makeDFA i dt = DFA i dt

-- Note that no CompOption parameter is needed.
nfaToDFA :: ((Index,Array Index QNFA),Array Tag OP,Array GroupIndex [GroupInfo])
         -> (DFA,Index,Array Tag OP,Array GroupIndex [GroupInfo])
nfaToDFA ((startIndex,aQNFA),aTagOp,aGroupInfo) = (dfa,startIndex,aTagOp,aGroupInfo) where
  dfa = indexesToDFA [startIndex]

  indexesToDFA = {-# SCC "nfaToDFA.indexesToDFA" #-} Trie.lookupAsc trie  -- Lookup in cache
    where trie :: TrieSet DFA
          trie = Trie.fromSinglesMerge dlose mergeDFA (bounds aQNFA) indexToDFA

  indexToDFA :: Index -> DFA  -- used to seed the Trie from the NFA
  indexToDFA i = {-# SCC "nfaToDFA.indexToDFA" #-} makeDFA (ISet.singleton source) (qtToDT qtIn)
    where
      (QNFA {q_id = source,q_qt = qtIn}) = aQNFA!i
      qtToDT :: QT -> DT
      qtToDT (Testing {qt_test=wt, qt_dopas=dopas, qt_a=a, qt_b=b}) =
          Testing' { dt_test = wt
                   , dt_dopas = dopas
                   , dt_a = qtToDT a
                   , dt_b = qtToDT b }
      qtToDT (Simple {qt_win=w, qt_trans=t, qt_other=o}) =
        Simple' { dt_win = makeWinner
                , dt_trans = fmap qtransToDFA t
                , dt_other = if IMap.null o then Nothing else Just (qtransToDFA o)}
        where
          makeWinner :: IntMap {- Index -} Instructions --  (RunState ())
          makeWinner | noWin w = IMap.empty
                     | otherwise = IMap.singleton source (cleanWin w)

          qtransToDFA :: QTrans -> (DFA,DTrans)
          qtransToDFA qtrans = {-# SCC "nfaToDFA.indexToDFA.qtransToDFA" #-}
                               (indexesToDFA destinations,dtrans)
            where
              dtrans :: DTrans
              dtrans = IMap.fromDistinctAscList . mapSnd (IMap.singleton source) $ best
              destinations :: [Index]
              destinations = map fst best
              best :: [(Index,(DoPa,Instructions))]
              best = pickQTrans aTagOp $ qtrans

  -- The DFA states are built up by merging the singleton ones converted from the NFA
  mergeDFA :: DFA -> DFA -> DFA
  mergeDFA d1 d2 = {-# SCC "nfaToDFA.mergeDFA" #-} makeDFA i dt
    where
      i = d_id d1 `mappend` d_id d2
      dt = d_dt d1 `mergeDT` d_dt d2
      mergeDT,nestDT :: DT -> DT -> DT
      mergeDT (Simple' w1 t1 o1) (Simple' w2 t2 o2) = Simple' w t o
        where
          w = w1 `mappend` w2
          t = fuseDTrans -- t1 o1 t2 o2
          o = case (o1,o2) of
                (Just o1', Just o2') -> Just (mergeDTrans o1' o2')
                _                    -> o1 `mplus` o2
          -- This is very much like mergeQTrans
          mergeDTrans :: (DFA,DTrans) -> (DFA,DTrans) -> (DFA,DTrans)
          mergeDTrans (_,dt1) (_,dt2) = (indexesToDFA (IMap.keys dtrans),dtrans)
            where dtrans = IMap.unionWith IMap.union dt1 dt2
          -- This is very much like fuseQTrans
          fuseDTrans :: CharMap (DFA,DTrans)
          fuseDTrans = Map.fromDistinctAscList (fuse l1 l2)
            where
              l1 = Map.toAscList t1
              l2 = Map.toAscList t2
              merge_o1 = case o1 of Nothing -> id
                                    Just o1' -> mergeDTrans o1'
              merge_o2 = case o2 of Nothing -> id
                                    Just o2' -> mergeDTrans o2'
              fuse [] y = if isJust o1 then mapSnd merge_o1 y else y
              fuse x [] = if isJust o2 then mapSnd merge_o2 x else x
              fuse x@((xc,xa):xs) y@((yc,ya):ys) = 
                case compare xc yc of
                  LT -> (xc,merge_o2 xa) : fuse xs y
                  EQ -> (xc,mergeDTrans xa ya) : fuse xs ys
                  GT -> (yc,merge_o1 ya) : fuse x ys
      mergeDT dt1@(Testing' wt1 dopas1 a1 b1) dt2@(Testing' wt2 dopas2 a2 b2) =
        case compare wt1 wt2 of
          LT -> nestDT dt1 dt2
          EQ -> Testing' { dt_test = wt1
                         , dt_dopas = dopas1 `mappend` dopas2
                         , dt_a = mergeDT a1 a2
                         , dt_b = mergeDT b1 b2 }
          GT -> nestDT dt2 dt1
      mergeDT dt1@(Testing' {}) dt2 = nestDT dt1 dt2
      mergeDT dt1 dt2@(Testing' {}) = nestDT dt2 dt1
      nestDT dt1@(Testing' {dt_a=a,dt_b=b}) dt2 = dt1 { dt_a = mergeDT a dt2, dt_b = mergeDT b dt2 }
      nestDT _ _ = err "nestDT called on Simple -- cannot happen"

patternToDFA :: CompOption -> (Pattern,(GroupIndex, DoPa)) -> (DFA,Index,Array Tag OP,Array GroupIndex [GroupInfo])
patternToDFA compOpt pattern = nfaToDFA (patternToNFA compOpt pattern)

dfaMap :: DFA -> Data.Map.Map SetIndex DFA
dfaMap = seen (Data.Map.empty) where
  seen old d@(DFA {d_id=i,d_dt=dt}) =
    if i `Data.Map.member` old
      then old
      else let new = Data.Map.insert i d old
           in foldl' seen new (flattenDT dt)

flattenDT :: DT -> [DFA]
flattenDT (Simple' {dt_trans=mt,dt_other=mo}) = map fst . maybe id (:) mo . Map.elems $ mt
flattenDT (Testing' {dt_a=a,dt_b=b}) = flattenDT a ++ flattenDT b

examineDFA :: (DFA,Index,Array Tag OP,Array GroupIndex [GroupInfo]) -> String
examineDFA (dfa,_,_,_) = unlines $ map show $ Data.Map.elems $ dfaMap dfa

{-

fillMap :: Tag -> IntMap (Position,Bool)
fillMap tag = IMap.fromDistinctAscList [(t,(-1,True)) | t <- [0..tag] ]

diffMap :: IntMap (Position,Bool) -> IntMap (Position,Bool) -> [(Index,(Position,Bool))]
diffMap old new = IMap.toList (IMap.differenceWith (\a b -> if a==b then Nothing else Just b) old new)

examineDFA :: (DFA,Index,Array Tag OP,Array GroupIndex [GroupInfo]) -> String
examineDFA (dfa,_,aTags,_) = unlines $ map (examineDFA' (snd . bounds $ aTags)) (Map.elems $ dfaMap dfa)

examineDFA' :: Tag -> DFA -> String
examineDFA' maxTag = showDFA (fillMap maxTag)

{-
instance Show DFA where
  show (DFA {d_id=i,d_dt=dt}) = "DFA {d_id = "++show (ISet.toList i)
                            ++"\n    ,d_dt = "++ show dt
                            ++"\n}"
-}
-- instance Show DT where show = showDT

showDFA :: IntMap (Position,Bool) -> DFA -> String
showDFA m (DFA {d_id=i,d_dt=dt}) = "DFA {d_id = "++show (ISet.toList i)
                               ++"\n    ,d_dt = "++ showDT m dt
                               ++"\n}"
-}



-- pick QTrans can be told the unique source and knows all the
-- destinations (hmm...along with qt_win)!  So if in ascending destination order the last source
-- is free to mutatate the old state.  If the QTrans has only one
-- entry then all we need to do is mutate that entry when making a
-- transition.
-- 
pickQTrans :: Array Tag OP -> QTrans -> [({-Destination-}Index,(DoPa,Instructions))]
pickQTrans op tr = mapSnd (bestTrans op) . IMap.toList $ tr

cleanWin :: WinTags -> Instructions
cleanWin = toInstructions

bestTrans :: Array Tag OP -> [TagCommand] -> (DoPa,Instructions)
bestTrans _ [] = err "bestTrans : There were no transition choose from!"
bestTrans aTagOP (f:fs) | null fs = canonical f
                        | otherwise = foldl' pick (canonical f) fs where
  canonical :: TagCommand -> (DoPa,Instructions)
  canonical (dopa,spec) = (dopa, toInstructions spec)
  pick :: (DoPa,Instructions) -> TagCommand -> (DoPa,Instructions)
  pick win@(dopa1,Instructions {newPos = winPos}) (dopa2,spec) =
    let next@(Instructions {newPos = nextPos}) = toInstructions spec
    in case compareWith choose winPos nextPos of
         GT -> win
         LT -> (dopa2,next)
         EQ -> if dopa1 >= dopa2 then win else (dopa2,next) -- no deep reason not to just pick win
  choose Nothing Nothing = EQ
  choose Nothing x = flipOrder (choose x Nothing)
  choose (Just (tag,post)) Nothing =
    case aTagOP!tag of
      Maximize -> GT
      Minimize -> LT
      Orbit -> err $ "bestTrans.choose : Very Unexpeted Orbit in Just Nothing: "++show (tag,post,aTagOP,f:fs)
  choose (Just (tag,post1)) (Just (_,post2)) =
    case aTagOP!tag of
      Maximize -> compare post1 post2
      Minimize -> (flip compare) post1 post2
      Orbit -> err $ "bestTrans.choose : Very Unexpeted Orbit in Just Just: "++show (tag,(post1,post2),aTagOP,f:fs)


isDTLosing :: DT -> Bool
isDTLosing (Testing' {dt_a=a,dt_b=b}) = isDTLosing a && isDTLosing b
isDTLosing (Simple' {dt_win=w,dt_trans=t,dt_other=o}) | not (IMap.null w) = False
                                                      | Just (dfa,_) <- o, not (ISet.null (d_id dfa)) = False
                                                      | otherwise =
  let destinations = map (d_id . fst) . Map.elems $ t
  in all ISet.null destinations -- True for empty list of destinations

-- Assumes that Test_BOL is the smallest (and therefore always first) test
isDTFrontAnchored :: DT -> Bool
isDTFrontAnchored (Testing' {dt_test=wt,dt_b=b}) | wt == Test_BOL = isDTLosing b
isDTFrontAnchored _ = False

isDFAFrontAnchored :: DFA -> Bool
isDFAFrontAnchored = isDTFrontAnchored . d_dt
