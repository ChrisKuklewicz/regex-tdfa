-- | This is a version of Run that caters to Data.ByteString.Char8
module Text.Regex.TDFA.MutRunBS(findMatch,findMatchAll,countMatchAll) where

import Control.Monad(MonadPlus(..))
import Control.Monad.ST
import Data.Array.IArray((!),array,bounds)
import Data.Array.MArray
import qualified Data.ByteString.Char8 as B
import Data.IntMap(IntMap)
import qualified Data.IntMap as IMap
import Data.List(maximumBy)
import qualified Data.Map as Map(lookup)
import Data.Maybe(isJust)

import Text.Regex.Base(MatchArray)
import Text.Regex.TDFA.Common
import Text.Regex.TDFA.RunMutState -- (makeTagComparer,tagsToGroups,update,newScratchMap)
-- import Debug.Trace

{- By Chris Kuklewicz, 2007. BSD License, see the LICENSE file. -}

-- err :: String -> a
-- err = common_error "Text.Regex.TDFA.RunBS"

{-# INLINE findMatch #-}
findMatch :: Regex -> B.ByteString -> Maybe MatchArray
findMatch regexIn input = loop 0 where
  final = B.length input
  loop offset =
    let result = matchHere regexIn offset input
    in if isJust result then result
         else if offset == final then Nothing
                else let offset' = succ offset
                     in seq offset' $ loop offset'

{-# INLINE findMatchAll #-}
findMatchAll :: Regex -> B.ByteString -> [MatchArray]
findMatchAll regexIn input = loop 0 where
  final = B.length input
  loop offset =
    case matchHere regexIn offset input of
      Nothing -> if offset == final then []
                   else let offset' = succ offset
                        in seq offset' $ loop offset'
      Just ma -> ma : let (start,len) = ma!0
                      in if offset==final || len==0 then []
                           else let offset' = start + len
                                in seq offset' $ loop offset'

{-# INLINE countMatchAll #-}
countMatchAll :: Regex -> B.ByteString -> Int
countMatchAll regexIn input = loop 0 $! 0 where
  final = B.length input
  loop offset count =
    case matchHere regexIn offset input of
      Nothing -> if offset == final then count
                   else let offset' = succ offset
                        in seq offset' $ loop offset' $! count
      Just ma -> let (start,len) = ma!0
                 in if offset==final || len==0 then count
                      else let offset' = start + len
                           in seq offset' $ loop offset' $! succ count

{-# INLINE matchHere #-}
matchHere :: Regex -> Position -> B.ByteString -> Maybe MatchArray
matchHere regexIn offsetIn input = ans where
  ans = if captureGroups (regex_execOptions regexIn)
          then runHerePure
          else let winOff = runHereNoCap Nothing (d_dt (regex_dfa regexIn)) offsetIn
               in case winOff of
                    Nothing -> Nothing
                    Just offsetEnd -> Just (array (0,0) [(0,(offsetIn,offsetEnd-offsetIn))])

  aTagOP = regex_tags regexIn
  b = bounds aTagOP
  final = B.length input

  test = if multiline (regex_compOptions regexIn)
           then test_multiline
           else test_singleline
  test_multiline wt off =
    case wt of Test_BOL -> off == 0 || '\n' == B.index input (pred off)
               Test_EOL -> off == final || '\n' == B.index input off
  test_singleline wt off =
    case wt of Test_BOL -> off == 0
               Test_EOL -> off == final
  
  runHerePure :: Maybe MatchArray
  runHerePure = {-# SCC "runHerePure" #-}
    runST (do (SScratch s1 s2 w0) <- newScratch regexIn offsetIn
              answer <- runHere Nothing (d_dt (regex_dfa regexIn)) s1 s2 offsetIn
              case answer of
                Nothing -> return Nothing
                Just w -> do s <- freezeScratch w
                             let g = tagsToGroups (regex_groups regexIn) s
                             return (Just g)
          )

  runHere :: forall s. Maybe (WScratch s) -> DT
          -> MScratch s -> MScratch s
          -> Position
          -> ST s (Maybe (WScratch s))
  runHere winning dt s1 s2 off =
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
           if off==final then return winning' else
             let c = B.index input off
             in case Map.lookup c t `mplus` o of
                  Nothing -> return winning'
                  Just (dfa,trans) -> let dt' = d_dt dfa
                                          off' = succ off
                                      in seq off' $ do
                                         mapM_ updateDest (IMap.toList trans)
                                         runHere winning' dt' s2 s1 off'
         Testing' {dt_test=wt,dt_a=a,dt_b=b} ->
           if test wt off
             then runHere winning a s1 s2 off
             else runHere winning b s1 s2 off

  runHereNoCap winning dt off =
    case dt of
      Simple' {dt_win=w, dt_trans=t, dt_other=o} ->
        let winning' = if IMap.null w then winning else Just off
        in seq winning' $
           if off==final then winning' else
             let c = B.index input off
             in case Map.lookup c t `mplus` o of
                  Nothing -> winning'
                  Just (dfa,_) -> let dt' = d_dt dfa
                                      off' = succ off
                                  in seq off' $ runHereNoCap winning' dt' off'
      Testing' {dt_test=wt,dt_a=a,dt_b=b} ->
        if test wt off
          then runHereNoCap winning a off
          else runHereNoCap winning b off
