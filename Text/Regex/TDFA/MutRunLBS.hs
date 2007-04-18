-- | "Text.Regex.TDFA.Run" is the main module for matching a DFA
-- against a String.  Many of the associated functions are exported to
-- other modules to help match against other types.
module Text.Regex.TDFA.MutRunLBS (findMatch,findMatchAll,countMatchAll) where

import Control.Monad(MonadPlus(..))
import Control.Monad.ST(ST)
import qualified Control.Monad.ST.Lazy as Lazy(ST,runST,strictToLazyST)
import Data.Array.IArray((!),array,bounds)
import Data.Array.MArray(rangeSize)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.IntMap.CharMap(CharMap(..))
import qualified Data.IntMap as IMap(null,lookup)

import Text.Regex.Base(MatchArray,RegexOptions(..))
import Text.Regex.TDFA.Common
import Text.Regex.TDFA.TDFA(isDFAFrontAnchored)
import Text.Regex.TDFA.RunMutState(TagEngine(..),newTagEngine2,tagsToGroupsST,newScratch,resetScratch,SScratch(..))
import Text.Regex.TDFA.Wrap()
-- import Debug.Trace

{- By Chris Kuklewicz, 2007. BSD License, see the LICENSE file. -}

{-# INLINE lazy #-}
lazy :: ST s a -> Lazy.ST s a
lazy = Lazy.strictToLazyST

{-# INLINE index #-}
index :: B.ByteString -> Int -> Int
index input off = fromEnum (B.index input (toEnum off))

-- err :: String -> a
-- err = common_error "Text.Regex.TDFA.MutRunLBS"

{-# INLINE findMatch #-}
findMatch :: Regex -> B.ByteString -> Maybe MatchArray
findMatch regexIn inputIn = case matchHere regexIn 0 inputIn of
                               [] -> Nothing
                               (ma:_) -> Just ma

{-# INLINE findMatchAll #-}
findMatchAll :: Regex -> B.ByteString -> [MatchArray]
findMatchAll regexIn inputIn = matchHere regexIn 0 inputIn

{-# INLINE countMatchAll #-}
countMatchAll :: Regex -> B.ByteString -> Int
countMatchAll regexIn inputIn = length (matchHere regex 0 inputIn) where
  regex = setExecOpts (ExecOption {captureGroups = False,testMatch = False}) regexIn

{-
There are four possible routines use by matchHere, depending on
whether it needs to collect submatch data and whether the pattern is
only permitted to start matching at offsetIn==0.
-}
matchHere :: Regex -> Position -> B.ByteString -> [MatchArray]
matchHere regexIn offsetIn inputIn = ans where
  ans = if subCapture then runHerePure else noCap
    where subCapture = captureGroups (regex_execOptions regexIn)
                    && (1<=rangeSize (bounds (regex_groups regexIn)))

  frontAnchored = (not (multiline (regex_compOptions regexIn)))
               && isDFAFrontAnchored (regex_dfa regexIn)

  final = fromEnum (B.length inputIn)

  test | multiline (regex_compOptions regexIn) = test_multiline
       | otherwise = test_singleline
    where test_multiline Test_BOL off = off == 0 || newline == index inputIn (pred off)
          test_multiline Test_EOL off = off == final || newline == index inputIn off
          test_singleline Test_BOL off = off == 0
          test_singleline Test_EOL off = off == final
          newline = fromEnum '\n'

  runHerePure :: [MatchArray]
  runHerePure = Lazy.runST (do
    TagEngine findTrans updateWinner performTrans <- lazy (newTagEngine2 regexIn)
    let -- runHere :: Maybe (WScratch s,(Position,Char,String)) -> DT
        --         -> MScratch s -> MScratch s
        --         -> Position
        --         -> ST s (Maybe (WScratch s,(Position,Char,String)))
        runHere winning dt s1 s2 off = {-# SCC "runHere" #-}
          s1 `seq` s2 `seq` off `seq`
          case dt of
            Testing' {dt_test=wt,dt_a=a,dt_b=b} ->
              if test wt off
                then runHere winning a s1 s2 off
                else runHere winning b s1 s2 off
            Simple' {dt_win=w, dt_trans=(CharMap t), dt_other=o} -> do
              if off==final then updateWinner s1 off winning w else do
                case IMap.lookup (index inputIn off) t `mplus` o of
                  Nothing -> updateWinner s1 off winning w
                  Just (dfa,trans) -> do
                    findTrans s1 off trans
                    winning' <- updateWinner s1 off winning w
                    performTrans s1 s2 off trans
                    runHere winning' (d_dt dfa) s2 s1 (succ off)
        -- end of runHere
    -- body of runHerePure continues
    (SScratch s1 s2 w0) <- lazy (newScratch regexIn offsetIn)
    let go off = {-# SCC "runHerePure.go" #-} off `seq` do
          answer <- lazy (runHere Nothing (d_dt (regex_dfa regexIn)) s1 s2 off)
          case answer of
            Nothing -> if off==final
                         then return []
                         else do let off' = succ off
                                 () <- lazy (resetScratch regexIn off' s1 w0)
                                 go off'
            Just (w,off') -> do
              ma <- lazy (tagsToGroupsST (regex_groups regexIn) w)
              let len = snd (ma!0)
              rest <- if len==0 || off'==final then return []
                        else do () <- lazy (resetScratch regexIn off' s1 w0)
                                go off'
              return (ma:rest)
    if frontAnchored
      then if offsetIn/=0 then return [] 
             else do
               answer <- lazy (runHere Nothing (d_dt (regex_dfa regexIn)) s1 s2 offsetIn)
               case answer of
                 Nothing -> return []
                 Just (w,_) -> do
                   ma <- lazy (tagsToGroupsST (regex_groups regexIn) w)
                   return (ma:[])
      else go offsetIn ) -- end Lazy.runST
  -- end of runHerePure

  noCap = {-# SCC "noCap" #-}
    let dtIn = (d_dt (regex_dfa regexIn))
        go off =
          case runHereNoCap Nothing dtIn off of
            Nothing -> if off==final then [] else go (succ off)
            Just off' ->
              let len = off'-off
                  ma = array (0,0) [(0,(off,len))]
                  rest = if len == 0 || off'==final then []
                           else go off'
              in (ma:rest)
    in if frontAnchored
         then if offsetIn /= 0 then []
                else case runHereNoCap Nothing dtIn offsetIn of
                       Nothing -> []
                       Just off' ->
                         let len = off'-offsetIn
                             ma = array (0,0) [(0,(offsetIn,len))]
                         in (ma:[])
         else go offsetIn

  runHereNoCap winning dt off =  {-# SCC "runHereNoCap" #-}
    off `seq`
    case dt of
      Simple' {dt_win=w, dt_trans=(CharMap t), dt_other=o} ->
        let winning' = if IMap.null w then winning else Just off
        in seq winning' $
           if off==final then winning'
             else case IMap.lookup (index inputIn off) t `mplus` o of
                    Nothing -> winning'
                    Just (DFA {d_dt=dt'},_) ->
                      runHereNoCap winning' dt' (succ off)
      Testing' {dt_test=wt,dt_a=a,dt_b=b} ->
        if test wt off
          then runHereNoCap winning a off
          else runHereNoCap winning b off
