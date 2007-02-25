-- | "Text.Regex.TDFA.Run" is the main module for matching a DFA
-- against a String.  Many of the associated functions are exported to
-- other modules to help match against other types.
module Text.Regex.TDFA.MutRunSeq (findMatch,findMatchAll,countMatchAll) where

import Control.Monad(MonadPlus(..))
import Control.Monad.ST(ST)
import qualified Control.Monad.ST.Lazy as Lazy(ST,runST,strictToLazyST)
import Data.Array.IArray((!),array,bounds)
import Data.Array.MArray(rangeSize)
import qualified Data.IntMap.CharMap as Map(lookup,null)
import qualified Data.IntMap as IMap(null)
import Data.Maybe(isNothing)
import Data.Sequence as S(Seq,ViewL(..))
import qualified Data.Sequence as S(viewl,null)

import Text.Regex.Base(MatchArray,RegexOptions(..))
import Text.Regex.TDFA.Common
import Text.Regex.TDFA.TDFA(isDFAFrontAnchored)
import Text.Regex.TDFA.RunMutState(newTagEngine,tagsToGroupsST,newScratch,resetScratch,SScratch(..))
import Text.Regex.TDFA.Wrap()
-- import Debug.Trace

{- By Chris Kuklewicz, 2007. BSD License, see the LICENSE file. -}

{-# INLINE lazy #-}
lazy :: ST s a -> Lazy.ST s a
lazy = Lazy.strictToLazyST

-- err :: String -> a
-- err = common_error "Text.Regex.TDFA.MutRun"

{-# INLINE findMatch #-}
findMatch :: Regex -> Seq Char -> Maybe MatchArray
findMatch regexIn stringIn = case matchHere regexIn 0 '\n' stringIn of
                               [] -> Nothing
                               (ma:_) -> Just ma

{-# INLINE findMatchAll #-}
findMatchAll :: Regex -> Seq Char -> [MatchArray]
findMatchAll regexIn stringIn = matchHere regexIn 0 '\n' stringIn

{-# INLINE countMatchAll #-}
countMatchAll :: Regex -> Seq Char -> Int
countMatchAll regexIn stringIn = length (matchHere regex 0 '\n' stringIn) where
  regex = setExecOpts (ExecOption {captureGroups = False,testMatch = False}) regexIn

{-
There are four possible routines use by matchHere, depending on
whether it needs to collect submatch data and whether the pattern is
only permitted to start matching at offsetIn==0.
-}
matchHere :: Regex -> Position -> Char -> Seq Char -> [MatchArray]
matchHere regexIn offsetIn prevIn inputIn = ans where
  ans = if subCapture then runHerePure else noCap
    where subCapture = captureGroups (regex_execOptions regexIn)
                    && (1<=rangeSize (bounds (regex_groups regexIn)))

  frontAnchored = (not (multiline (regex_compOptions regexIn)))
               && isDFAFrontAnchored (regex_dfa regexIn)

  -- Select which style of ^ $ tests are performed.
  test | multiline (regex_compOptions regexIn) = test_multiline
       | otherwise = test_singleline
    where test_multiline Test_BOL _off prev _input = prev == '\n'
          test_multiline Test_EOL _off _prev input = case S.viewl input of
                                                       EmptyL -> True
                                                       (next :< _) -> next == '\n'
          test_singleline Test_BOL off _prev _input = off == 0
          test_singleline Test_EOL _off _prev input = S.null input
  
  runHerePure :: [MatchArray]
  runHerePure = Lazy.runST (do
    (!findTrans,!updateWinner,!performTrans) <- lazy (newTagEngine regexIn)
    let -- runHere :: Maybe (WScratch s,(Position,Char,Seq Char)) -> DT
        --         -> MScratch s -> MScratch s
        --         -> Position -> Char -> Seq Char
        --         -> ST s (Maybe (WScratch s,(Position,Char,Seq Char)))
        runHere winning !dt !s1 !s2 !off !prev !input = {-# SCC "runHere" #-}
          case dt of
            Testing' {dt_test=wt,dt_a=a,dt_b=b} ->
              if test wt off prev input
                then runHere winning a s1 s2 off prev input
                else runHere winning b s1 s2 off prev input
            Simple' {dt_win=w, dt_trans=t, dt_other=o} -> do
              case S.viewl input of
                EmptyL -> updateWinner s1 (off,prev,input) winning w
                (c :< input') ->
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
            Nothing -> case S.viewl input of
                         EmptyL -> return []
                         (prev' :< input') ->
                            let off' = succ off
                            in do () <- lazy (resetScratch regexIn off' s1 w0)
                                  go off' prev' input'
            Just (w,(off',prev',input')) -> do
              ma <- lazy (tagsToGroupsST (regex_groups regexIn) w)
              let len = snd (ma!0)
              rest <- if len==0 || S.null input' then return []
                        else do () <- lazy (resetScratch regexIn off' s1 w0)
                                go off' prev' input'
              return (ma:rest)
    if frontAnchored
      then if offsetIn/=0 then return [] 
             else do
               answer <- lazy (runHere Nothing (d_dt (regex_dfa regexIn)) s1 s2 offsetIn prevIn inputIn)
               case answer of
                 Nothing -> return []
                 Just (w,_) -> do
                   ma <- lazy (tagsToGroupsST (regex_groups regexIn) w)
                   return (ma:[])
      else go offsetIn prevIn inputIn ) -- end Lazy.runST
  -- end of runHerePure

  noCap = {-# SCC "noCap" #-}
    let dtIn = (d_dt (regex_dfa regexIn))
        go !off !prev !input = 
          case runHereNoCap Nothing dtIn off prev input of
            Nothing -> case S.viewl input of
                         EmptyL -> []
                         (prev' :< input') -> let off' = succ off
                                              in go off' prev' input'
            Just (off',prev',input') ->
              let len = off'-off
                  ma = array (0,0) [(0,(off,len))]
                  rest = if len == 0 || S.null input then []
                           else go off' prev' input'
              in (ma:rest)
    in if frontAnchored
         then if offsetIn /= 0 then []
                else case runHereNoCap Nothing dtIn offsetIn prevIn inputIn of
                       Nothing -> []
                       Just (off',_prev',_input') ->
                         let len = off'-offsetIn
                             ma = array (0,0) [(0,(offsetIn,len))]
                         in (ma:[])
         else go offsetIn prevIn inputIn

  runHereNoCap winning !dt !off !prev !input =  {-# SCC "runHereNoCap" #-}
    case dt of
      Simple' {dt_win=w, dt_trans=t, dt_other=o} ->
        let winning' = if IMap.null w then winning else Just (off,prev,input)
        in seq winning' $
           if Map.null t && isNothing o then winning' else
             case S.viewl input of
               EmptyL -> winning'
               (c :< input') ->
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
