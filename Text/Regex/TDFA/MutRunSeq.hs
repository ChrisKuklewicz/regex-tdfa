-- | "Text.Regex.TDFA.RunSeq" is Run adapted for Seq
module Text.Regex.TDFA.MutRunSeq (findMatch,findMatchAll,countMatchAll) where

import Control.Monad(MonadPlus(..))
import Control.Monad.ST
import Data.Array.IArray((!),array,bounds)
import Data.List(maximumBy)
import qualified Data.Map as Map(lookup,null)
import Data.IntMap(IntMap)
import qualified Data.IntMap as IMap
import Data.Maybe(isJust,isNothing)
import Data.Sequence as S(Seq,ViewL(..))
import qualified Data.Sequence as S(viewl,null,drop)

import Text.Regex.Base(MatchArray,RegexOptions(..))
import Text.Regex.TDFA.Common
import Text.Regex.TDFA.RunMutState -- (makeTagComparer,tagsToGroups,update,newScratchMap)
import Text.Regex.TDFA.Wrap()
-- import Debug.Trace

{- By Chris Kuklewicz, 2007. BSD License, see the LICENSE file. -}

-- err :: String -> a
-- err = common_error "Text.Regex.TDFA.Run"

{-# INLINE findMatch #-}
findMatch :: Regex -> Seq Char -> Maybe MatchArray
findMatch regexIn stringIn = loop 0 '\n' stringIn where
  loop offset prev input =
    let result = matchHere regexIn offset prev input
    in if isJust result then result
         else case S.viewl input of
                EmptyL -> Nothing
                (prev' :< input') -> let offset' = succ offset
                                     in seq offset' $
                                        loop offset' prev' input'

{-# INLINE findMatchAll #-}
findMatchAll :: Regex -> Seq Char -> [MatchArray]
findMatchAll regexIn stringIn = loop 0 '\n' stringIn where
  loop offset prev input =
    case matchHere regexIn offset prev input of
      Nothing -> case S.viewl input of
                   EmptyL -> []
                   (prev' :< input') -> let offset' = succ offset
                                        in seq offset' $
                                           loop offset' prev' input'
      Just ma -> ma : let (start,len) = ma!0
                      in if S.null input || len == 0 then []
                           else let offset' = start + len          -- start >= offset; len > 0
                                    skip = (offset' - offset) -- skip >= 0
                                    post = S.drop (pred skip) input
                                in case S.viewl post of
                                     EmptyL -> [] -- really an error, should not happen
                                     (prev' :< input') -> seq offset' $
                                                          loop offset' prev' input'

{-# INLINE countMatchAll #-}
countMatchAll :: Regex -> Seq Char -> Int
countMatchAll regexIn stringIn = loop 0 '\n' stringIn $! 0 where
  regex = setExecOpts (ExecOption {captureGroups = False,testMatch = False}) regexIn
  loop offset prev input count =
    case matchHere regex offset prev input of
      Nothing -> case S.viewl input of
                   EmptyL -> count
                   (prev' :< input') -> let offset' = succ offset
                                        in seq offset' $ 
                                           loop offset' prev' input' $! count
      Just ma -> let (start,len) = ma!0
                 in if S.null input then count
                      else let offset' = start + len     -- start >= offset; len > 0
                               skip = (offset' - offset) -- skip >= 0
                               post = S.drop (pred skip) input
                           in case S.viewl post of
                                EmptyL -> count -- really an error, should not happen
                                (prev' :< input') -> seq offset' $
                                                     loop offset' prev' input' $! succ count

{-# INLINE matchHere #-}
matchHere :: Regex -> Position -> Char -> Seq Char
          -> Maybe MatchArray
matchHere regexIn offsetIn prevIn inputIn = ans where
  ans = if captureGroups (regex_execOptions regexIn)
          then runHerePure
          else let winOff = runHereNoCap Nothing (d_dt (regex_dfa regexIn)) offsetIn prevIn inputIn
               in case winOff of
                    Nothing -> Nothing
                    Just offsetEnd -> Just (array (0,0) [(0,(offsetIn,offsetEnd-offsetIn))])

  aTagOP = regex_tags regexIn
  b = bounds aTagOP

--  initialScratchMap = newScratchMap regexIn offsetIn
--  comp = makeTagComparer (regex_tags regexIn)

  test_multiline wt _ prev input =
    case wt of Test_BOL -> prev == '\n'
               Test_EOL -> case S.viewl input of
                             EmptyL -> True
                             (next :< _) -> next == '\n'

  test_singleline wt off _ input =
    case wt of Test_BOL -> off == 0
               Test_EOL -> S.null input

  test = if multiline (regex_compOptions regexIn) then test_multiline else test_singleline

  runHerePure :: Maybe MatchArray
  runHerePure = {-# SCC "runHerePure" #-}
    runST (do (SScratch s1 s2 w0) <- newScratch regexIn offsetIn
              answer <- runHere Nothing (d_dt (regex_dfa regexIn)) s1 s2 offsetIn prevIn inputIn
              case answer of
                Nothing -> return Nothing
                Just w -> do s <- freezeScratch w
                             let g = tagsToGroups (regex_groups regexIn) s
                             return (Just g)
          )

  runHere :: forall s. Maybe (WScratch s) -> DT
          -> MScratch s -> MScratch s
          -> Position -> Char -> Seq Char
          -> ST s (Maybe (WScratch s))
  runHere winning dt s1 s2 off prev input = 
    let followTrans :: DTrans -> ST s ()
        followTrans dtrans = mapM_ updateDest (IMap.toList dtrans)
        updateDest :: ({-Dest-}Index,IntMap {-Source-} (DoPa,Instructions)) -> ST s ()
        updateDest (destIndex,sourceIns) | IMap.null sourceIns = err "matchHere.runHere.updateDest found null sourceIns"
                                         | IMap.size sourceIns == 1 = do
          let [(sourceIndex,(_,instructions))] = IMap.toList sourceIns
          forceUpdate s1 sourceIndex instructions off s2 destIndex
                                         | otherwise = do
          let ((si0,(_,ins0)):rest) = IMap.toList sourceIns
          forceUpdate s1 si0 ins0 off s2 destIndex
          let fight (sourceIndex,(_,instructions)) = do
                result <- challenge aTagOP s2 destIndex instructions off s1 sourceIndex
                case result of
                  LT -> forceUpdate s1 sourceIndex instructions off s2 destIndex
                  _ -> return ()
          mapM_ fight rest
        updateWinner :: IntMap {-Source-} Instructions -> ST s (Maybe (WScratch s))
        updateWinner sourceIns | IMap.null sourceIns = return winning
                               | IMap.size sourceIns == 1 = do
          w@(WScratch p f o) <- case winning of
                                  Nothing -> newWScratch_ b
                                  Just win -> return win
          let [(sourceIndex,instructions)] = IMap.toList sourceIns
          forceUpdateW s1 sourceIndex instructions off p f o
          return (Just w)
                               | otherwise = do
          let ((si0,ins0):rest) = IMap.toList sourceIns
          w@(WScratch p f o) <- case winning of 
                                  Nothing -> newWScratch_ b
                                  Just win -> return win
          forceUpdateW s1 si0 ins0 off p f o
          let fight (sourceIndex,instructions) = do
                result <- challengeW aTagOP p f o instructions off s1 sourceIndex
                case result of
                  LT -> forceUpdateW s1 sourceIndex instructions off p f o
                  _ -> return ()
          mapM_ fight rest
          return (Just w)

    in case dt of
         Simple' {dt_win=w, dt_trans=t, dt_other=o} -> do
           winning' <- updateWinner w
           case S.viewl input of
             EmptyL -> return winning'
             (c :< input') -> case Map.lookup c t `mplus` o of
                                Nothing -> return winning'
                                Just (dfa,trans) -> let dt' = d_dt dfa
                                                        off' = succ off
                                                        prev' = c
                                                    in seq off' $ do
                                                       mapM_ updateDest (IMap.toList trans)
                                                       runHere winning' dt' s2 s1 off' prev' input'
         Testing' {dt_test=wt,dt_a=a,dt_b=b} ->
           if test wt off prev input
             then runHere winning a s1 s2 off prev input
             else runHere winning b s1 s2 off prev input

  runHereNoCap winning dt off prev input =
    case dt of
      Simple' {dt_win=w, dt_trans=t, dt_other=o} ->
        let winning' = if IMap.null w then winning else Just off
        in seq winning' $
           if Map.null t && isNothing o then winning' else
             case S.viewl input of
               EmptyL -> winning'
               (c :< input') -> case Map.lookup c t `mplus` o of
                                 Nothing -> winning'
                                 Just (dfa,_) -> let dt' = d_dt dfa
                                                     off' = succ off
                                                     prev' = c
                                                 in seq off' $
                                                    runHereNoCap winning' dt' off' prev' input'
      Testing' {dt_test=wt,dt_a=a,dt_b=b} ->
        if test wt off prev input
          then runHereNoCap winning a off prev input
          else runHereNoCap winning b off prev input


{-
The test cases that set the Maximize cases in makeTagComparer

*Text.Regex.TDFA.Run> let r=makeRegex "((.)(.*)|.+)*|." :: Regex in matchHere r 0 '\n' "a"
Just (fromList *** Exception: tv2 longer :(
array (0,7) [(0,Minimize),(1,Maximize),(2,Maximize),(3,Minimize),(4,Maximize),(5,Maximize),(6,Maximize),(7,Maximize)],
fromList [(0,0),(1,1)],
fromList [(0,0),(1,1),(2,1),(3,0),(4,1),(5,1),(6,1)])

array (1,3) [(1,GroupInfo {thisIndex = 1, parentIndex = 0, startTag = 3, stopTag = 4})
            ,(2,GroupInfo {thisIndex = 2, parentIndex = 1, startTag = 3, stopTag = 6})
            ,(3,GroupInfo {thisIndex = 3, parentIndex = 1, startTag = 6, stopTag = 5})]


*Text.Regex.TDFA.Run> let r=makeRegex "((.)(.*)|.+)*|(.)" :: Regex in matchHere r 0 '\n' "a"
Just (fromList *** Exception: tv2 without tv1 :(
array (0,8) [(0,Minimize),(1,Maximize),(2,Maximize),(3,Minimize),(4,Maximize),(5,Maximize),(6,Maximize),(7,Maximize),(8,Maximize)],
fromList [(0,0),(1,1),(8,1)],
fromList [(0,0),(1,1),(2,1),(3,0),(4,1),(5,1),(6,1)])  -- preferred

array (1,4) [(1,GroupInfo {thisIndex = 1, parentIndex = 0, startTag = 3, stopTag = 4})
            ,(2,GroupInfo {thisIndex = 2, parentIndex = 1, startTag = 3, stopTag = 6})
            ,(3,GroupInfo {thisIndex = 3, parentIndex = 1, startTag = 6, stopTag = 5})
            ,(4,GroupInfo {thisIndex = 4, parentIndex = 0, startTag = 0, stopTag = 8})]


*Text.Regex.TDFA.Run> let r=makeRegex "((.)(.*)|.+)*|." :: Regex in matchHere r 0 '\n' "aa"
Just (fromList *** Exception: tv1 without tv2 : (
array (0,7) [(0,Minimize),(1,Maximize),(2,Maximize),(3,Minimize),(4,Maximize),(5,Maximize),(6,Maximize),(7,Maximize)],
fromList [(0,0),(1,2),(2,2),(3,0),(4,2),(5,2),(6,1)],   -- (.)(.*) is tagged 3.6.54 and is the preferred answer
fromList [(0,0),(1,2),(2,2),(3,0),(4,2),(7,1)])         -- .+ branch is ..* which is tagged 3.7.*4

-}
