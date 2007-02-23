-- | "Text.Regex.TDFA.Run" is the main module for matching a DFA
-- against a String.  Many of the associated functions are exported to
-- other modules to help match against other types.
module Text.Regex.TDFA.MutRun (findMatch,findMatchAll,countMatchAll) where

import Control.Monad(MonadPlus(..),forM,foldM,when)
import Control.Monad.ST(ST,runST)
import qualified Control.Monad.ST.Lazy as Lazy(ST,runST,strictToLazyST)
import Data.Array.Base(unsafeRead,unsafeWrite)
import Data.Array.IArray((!),array,bounds)
import Data.Array.MArray -- ((!),array,bounds)
import Data.List(maximumBy)
import qualified Data.Map as Map(lookup,null)
import Data.IntMap(IntMap)
import qualified Data.IntMap as IMap
import Data.Maybe(isJust,isNothing)
import Data.STRef

import Text.Regex.Base(MatchArray,RegexOptions(..))
import Text.Regex.TDFA.Common
import Text.Regex.TDFA.TDFA(isDFAFrontAnchored)
import Text.Regex.TDFA.RunMutState -- (makeTagComparer,tagsToGroups,update,newScratchMap)
import Text.Regex.TDFA.Wrap()
-- import Debug.Trace

{- By Chris Kuklewicz, 2007. BSD License, see the LICENSE file. -}

{-# INLINE lazy #-}
lazy :: ST s a -> Lazy.ST s a
lazy = Lazy.strictToLazyST

err :: String -> a
err = common_error "Text.Regex.TDFA.MutRun"

{-# INLINE findMatch #-}
findMatch :: Regex -> String -> Maybe MatchArray
findMatch regexIn stringIn = case matchHere regexIn 0 '\n' stringIn of
                               [] -> Nothing
                               (ma:_) -> Just ma

{-# INLINE findMatchAll #-}
findMatchAll :: Regex -> String -> [MatchArray]
findMatchAll regexIn stringIn = matchHere regexIn 0 '\n' stringIn

{-# INLINE countMatchAll #-}
countMatchAll :: Regex -> String -> Int
countMatchAll regexIn stringIn = length (matchHere regex 0 '\n' stringIn) where
  regex = setExecOpts (ExecOption {captureGroups = False,testMatch = False}) regexIn

matchHere :: Regex -> Position -> Char -> String -> [MatchArray]
matchHere regexIn offsetIn prevIn inputIn = ans where
  ans = if captureGroups (regex_execOptions regexIn) && (1<=rangeSize (bounds (regex_groups regexIn)))
          then runHerePure
          else let dtIn = (d_dt (regex_dfa regexIn))
                   go !off !prev !input = 
                     case runHereNoCap Nothing dtIn off prev input of
                       Nothing -> case input of
                                    [] -> []
                                    (prev':input') -> let off' = succ off
                                                      in go off' prev' input'
                       Just (off',prev',input') ->
                         let len = off'-off
                             ma = array (0,0) [(0,(off,len))]
                             rest = if len == 0 || null input then []
                                      else go off' prev' input'
                         in (ma:rest)
               in go offsetIn prevIn inputIn
-- XXX add frontAnchored support
-- XXX Check for no capture groups

  frontAnchored = (not (multiline (regex_compOptions regexIn)))
               && isDFAFrontAnchored (regex_dfa regexIn)

  test_multiline wt _ prev input =
    case wt of Test_BOL -> prev == '\n'
               Test_EOL -> case input of
                             [] -> True
                             (next:_) -> next == '\n'

  test_singleline wt off _ input =
    case wt of Test_BOL -> off == 0
               Test_EOL -> null input

  test = if multiline (regex_compOptions regexIn) then test_multiline else test_singleline
  comp = makeTagComparer (regex_tags regexIn)
  
  runHerePure :: [MatchArray]
  runHerePure = {-# SCC "runHerePure" #-} Lazy.runST (do
    (findTrans,updateWinner,performTrans) <- lazy (newTagEngine regexIn)
    let -- runHere :: Maybe (WScratch s,(Position,Char,String)) -> DT
        --         -> MScratch s -> MScratch s
        --         -> Position -> Char -> String
        --         -> ST s (Maybe (WScratch s,(Position,Char,String)))
        runHere winning !dt !s1 !s2 !off !prev input = {-# SCC "runHere" #-}
          case dt of
            Testing' {dt_test=wt,dt_a=a,dt_b=b} ->
              if test wt off prev input
                then runHere winning a s1 s2 off prev input
                else runHere winning b s1 s2 off prev input
            Simple' {dt_win=w, dt_trans=t, dt_other=o} -> do
              case input of
                [] -> updateWinner s1 (off,prev,input) winning w
                (c:input') ->
                  case Map.lookup c t `mplus` o of
                    Nothing -> updateWinner s1 (off,prev,input) winning w
                    Just (dfa,trans) -> do
                      findTrans s1 off trans
                      winning' <- updateWinner s1 (off,prev,input) winning w
                      performTrans s1 s2 off trans
                      runHere winning' (d_dt dfa) s2 s1 (succ off) c input'
        -- end of runHere

    -- body of runHerePure continues
    (SScratch s1 s2 w0) <- lazy (newScratch regexIn offsetIn)
    let go !off !prev !input = {-# SCC "runHerePure.go" #-} do
          answer <- lazy (runHere Nothing (d_dt (regex_dfa regexIn)) s1 s2 off prev input)
          case answer of
            Nothing -> case input of
                         [] -> return []
                         (prev':input') -> let off' = succ off
                                           in do () <- lazy (resetScratch regexIn off' s1 w0)
                                                 go off' prev' input'
            Just (w,x@(off',prev',input')) -> do
              ma <- lazy (tagsToGroupsST (regex_groups regexIn) w)
              let len = snd (ma!0)
              rest <- if len==0 || null input' then return []
                        else do () <- lazy (resetScratch regexIn off' s1 w0)
                                go off' prev' input'
              return (ma:rest)
    go offsetIn prevIn inputIn ) -- end Lazy.runST
  -- end of runHerePure

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
-- | Used by some debug/tracing
showArr :: (MArray a (b) m,Ix i,Show i,Show b) => a i (b) -> m String
showArr a = do
  ss <- getAssocs a
  return (show ss)
-}

 