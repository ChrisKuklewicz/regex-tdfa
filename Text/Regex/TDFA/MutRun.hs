{-# GHC_OPTIONS -fbang-patterns #-}
-- | "Text.Regex.TDFA.Run" is the main module for matching a DFA
-- against a String.  Many of the associated functions are exported to
-- other modules to help match against other types.
module Text.Regex.TDFA.MutRun (findMatch,findMatchAll,countMatchAll) where

import Control.Monad(MonadPlus(..),forM)
import Control.Monad.ST(ST,runST)
import qualified Control.Monad.ST.Lazy as Lazy(ST,runST,strictToLazyST)
import Data.Array.IArray((!),array,bounds)
import Data.Array.MArray -- ((!),array,bounds)
import Data.List(maximumBy)
import qualified Data.Map as Map(lookup,null)
import Data.IntMap(IntMap)
import qualified Data.IntMap as IMap
import Data.Maybe(isJust,isNothing)

import Text.Regex.Base(MatchArray,RegexOptions(..))
import Text.Regex.TDFA.Common
import Text.Regex.TDFA.RunMutState -- (makeTagComparer,tagsToGroups,update,newScratchMap)
import Text.Regex.TDFA.Wrap()
import Debug.Trace

{- By Chris Kuklewicz, 2007. BSD License, see the LICENSE file. -}

update = undefined

-- err :: String -> a
-- err = common_error "Text.Regex.TDFA.Run"

{-# INLINE findMatch #-}
findMatch :: Regex -> String -> Maybe MatchArray
findMatch regexIn stringIn = case matchHere regexIn 0 '\n' stringIn of
                               [] -> Nothing
                               [ma] -> Just ma

{-# INLINE findMatchAll #-}
findMatchAll :: Regex -> String -> [MatchArray]
findMatchAll regexIn stringIn = matchHere regexIn 0 '\n' stringIn

{-# INLINE countMatchAll #-}
countMatchAll :: Regex -> String -> Int
countMatchAll regexIn stringIn = length (matchHere regex 0 '\n' stringIn) where
  regex = setExecOpts (ExecOption {captureGroups = False,testMatch = False}) regexIn

{-# INLINE matchHere #-}
matchHere :: Regex -> Position -> Char -> String -> [MatchArray]
matchHere regexIn offsetIn prevIn inputIn = ans where
  ans = if captureGroups (regex_execOptions regexIn)
          then runHerePure
          else let dtIn = (d_dt (regex_dfa regexIn))
                   go !off !prev !input = 
                     case runHereNoCap Nothing dtIn off prev input of
                       Nothing -> case input of
                                    [] -> []
                                    (prev':input') -> let off' = succ off
                                                      in go off' prev' input'
                       Just (off',prev',input') ->
                         let ma = array (0,0) [(0,(offsetIn,len))]
                             len = off'-offsetIn
                             rest = if len == 0 || null input then []
                                      else go off' prev' input'
                         in (ma:rest)
               in go offsetIn prevIn inputIn
-- XXX add frontAnchored support

--  initialScratchMap = newScratchMap regexIn offsetIn
  aTagOP = regex_tags regexIn
  b = bounds aTagOP

  test_multiline wt _ prev input =
    case wt of Test_BOL -> prev == '\n'
               Test_EOL -> case input of
                             [] -> True
                             (next:_) -> next == '\n'

  test_singleline wt off _ input =
    case wt of Test_BOL -> off == 0
               Test_EOL -> null input

  test = if multiline (regex_compOptions regexIn) then test_multiline else test_singleline
  
  runHerePure :: [MatchArray]
  runHerePure = {-# SCC "runHerePure" #-} Lazy.runST (do
    (SScratch s1 s2 w0) <- Lazy.strictToLazyST (newScratch regexIn offsetIn)
    let go !off !prev !input = {-# SCC "runHerePure.go" #-} do
          answer <- Lazy.strictToLazyST (runHere Nothing (d_dt (regex_dfa regexIn)) s1 s2 off prev input)
          case answer of
            Nothing -> case input of
                         [] -> return []
                         (prev':input') -> let off' = succ off
                                           in do () <- Lazy.strictToLazyST (resetScratch regexIn off' s1 w0)
                                                 go off' prev' input'
            Just (w,(off',prev',input')) -> do
              ma <- Lazy.strictToLazyST (tagsToGroupsST (regex_groups regexIn) w)
              let len = snd (ma!0)
              rest <- if len==0 || null input' then return []
                        else do () <- Lazy.strictToLazyST (resetScratch regexIn off' s1 w0)
                                go off' prev' input'
              return (ma:rest)
    go offsetIn prevIn inputIn
                                                     ) -- end Lazy.runST

  runHere :: forall s. Maybe (WScratch s,(Position,Char,String)) -> DT
          -> MScratch s -> MScratch s
          -> Position -> Char -> String
          -> ST s (Maybe (WScratch s,(Position,Char,String)))
  runHere !winning !dt !s1 !s2 !off !prev !input = {-# SCC "runHere" #-}
    let followTrans :: DTrans -> ST s ()
        followTrans dtrans | IMap.size dtrans /=1 = mapM_ updateDest (IMap.toList dtrans)
                           | otherwise = let (x@(destIndex,sourceIns):_) = IMap.toList dtrans
                                         in if IMap.size sourceIns > 1 then updateDest x
                                              else let [(sourceIndex,(_,instructions))] = IMap.toList sourceIns
                                                   in updateSwap s1 sourceIndex instructions off s2 destIndex

-- XXXX Change to keep track of winning source Index, then use swap if it is unique

        updateDest :: ({-Dest-}Index,IntMap {-Source-} (DoPa,Instructions)) -> ST s ()
        updateDest !(!destIndex,!sourceIns) | IMap.null sourceIns = err "matchHere.runHere.updateDest found null sourceIns"
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
        updateWinner :: IntMap {-Source-} Instructions -> ST s (Maybe (WScratch s,(Position,Char,String)))
        updateWinner !sourceIns | IMap.null sourceIns = return winning
                                | IMap.size sourceIns == 1 = do
          w@(WScratch p f o) <- case winning of
                                  Nothing -> newWScratch_ b
                                  Just (win,_) -> return win
          let [(sourceIndex,instructions)] = IMap.toList sourceIns
          forceUpdateW s1 sourceIndex instructions off p f o
          return (Just (w,(off,prev,input)))
                               | otherwise = do
          let ((si0,ins0):rest) = IMap.toList sourceIns
          w@(WScratch p f o) <- case winning of 
                                  Nothing -> newWScratch_ b
                                  Just (win,_) -> return win
          forceUpdateW s1 si0 ins0 off p f o
          let fight (sourceIndex,instructions) = do
                result <- challengeW aTagOP p f o instructions off s1 sourceIndex
                case result of
                  LT -> forceUpdateW s1 sourceIndex instructions off p f o
                  _ -> return ()
          mapM_ fight rest
          return $ Just (w,(off,prev,input))

    in case dt of
         Simple' {dt_win=w, dt_trans=t, dt_other=o} -> do
            winning' <- updateWinner w
            case input of
              [] -> return winning'
              (c:input') ->
                case Map.lookup c t `mplus` o of
                  Nothing -> return winning'
                  Just (dfa,trans) -> let dt' = d_dt dfa
                                          off' = succ off
                                          prev' = c
                                      in seq off' $ do
                                         followTrans trans
                                         runHere winning' dt' s2 s1 off' prev' input'
         Testing' {dt_test=wt,dt_a=a,dt_b=b} ->
           if test wt off prev input
             then runHere winning a s1 s2 off prev input
             else runHere winning b s1 s2 off prev input

  runHereNoCap winning !dt !off !prev !input =  {-# SCC "runHereNoCap" #-}
    case dt of
      Simple' {dt_win=w, dt_trans=t, dt_other=o} ->
        let winning' = if IMap.null w then winning else Just (off,prev,input)
        in -- seq winning' $
           if Map.null t && isNothing o then winning' else
             case input of
               [] -> winning'
               (c:input') ->
                 case Map.lookup c t `mplus` o of
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

showA :: (MArray a (Maybe b) m,Ix i,Show i) => a i (Maybe b) -> m String
showA a = do
  b <- getBounds a
  ss <- forM (range b) $ \i -> do
            v <- readArray a i
            return $ maybe (i,"Nothing") (const (i,"Just")) v
  return (show ss)

{-
            s1s <- sequence [ showA (m_pos s1)
                            , showA (m_flag s1)
                            , showA (m_orbit s1)]
            s2s <- sequence [ showA (m_pos s2)
                            , showA (m_flag s2)
                            , showA (m_orbit s2)]
            let ws = case winning of
                       Nothing -> "Not Winning"
                       Just _ -> "Winning"
            trace ("\n> runHere/Simple' : "++show (ws,s1s,s2s)) $ do
-}
