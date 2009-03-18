-- | Like Engine, but merely checks to see whether any match at all is found.
-- 
module Text.Regex.TDFA.NewDFA.Tester(matchTest) where

import Control.Monad(MonadPlus(..))
import qualified Data.IntMap.CharMap2 as CMap(findWithDefault)
import qualified Data.IntMap as IMap
import qualified Data.IntSet as ISet(null)

import Data.Sequence(Seq,ViewL(..),viewl)
import qualified Data.Sequence as Seq
import qualified Data.ByteString.Char8 as SBS
import qualified Data.ByteString.Lazy.Char8 as LBS

import Text.Regex.Base()
import Text.Regex.TDFA.Common hiding (indent)
import Text.Regex.TDFA.NewDFA.Uncons (Uncons(uncons))

{-# SPECIALIZE matchTest :: Regex -> ([] Char) -> Bool #-}
{-# SPECIALIZE matchTest :: Regex -> (Seq Char) -> Bool #-}
{-# SPECIALIZE matchTest :: Regex -> SBS.ByteString -> Bool #-}
{-# SPECIALIZE matchTest :: Regex -> LBS.ByteString -> Bool #-}
matchTest :: Uncons text => Regex -> text -> Bool
matchTest (Regex { regex_dfa = dfaIn
                 , regex_isFrontAnchored = ifa
                 , regex_compOptions = CompOption { multiline = newline } } )
          inputIn = ans where

  ans = case ifa of
          True -> single0 (d_dt dfaIn) inputIn
          False -> multi0 (d_dt dfaIn) inputIn

  {-# NOINLINE test0 #-}
  {-# NOINLINE test #-}
  !test0 = mkTest0 newline
  !test = mkTest newline         

  multi0 (Testing' {dt_test=wt,dt_a=a,dt_b=b}) input =
    if test0 wt input
      then multi0 a input
      else multi0 b input
  multi0 (Simple' {dt_win=w,dt_trans=t, dt_other=o}) input
    | IMap.null w =
        case uncons input of
          Nothing -> False
          Just (c,input') ->
            case CMap.findWithDefault o c t of
              Transition {trans_many=DFA {d_dt=dt'}} -> multi dt' c input'
    | otherwise = True

  multi (Testing' {dt_test=wt,dt_a=a,dt_b=b}) prev input =
    if test wt prev input
      then multi a prev input
      else multi b prev input
  multi (Simple' {dt_win=w,dt_trans=t, dt_other=o}) _prev input
    | IMap.null w =
        case uncons input of
          Nothing -> False
          Just (c,input') ->
            case CMap.findWithDefault o c t of
              Transition {trans_many=DFA {d_dt=dt'}} -> multi dt' c input'
    | otherwise = True

  single0 (Testing' {dt_test=wt,dt_a=a,dt_b=b}) input =
    if testFA0 wt input
      then single0 a input
      else single0 b input
  single0 (Simple' {dt_win=w,dt_trans=t, dt_other=o}) input
    | IMap.null w =
        case uncons input of
             Nothing -> False
             Just (c,input') ->
               case CMap.findWithDefault o c t of
                 Transition {trans_single=DFA {d_id=did',d_dt=dt'}}
                   | ISet.null did' -> False
                   | otherwise -> single dt' input'
    | otherwise = True

  single (Testing' {dt_test=wt,dt_a=a,dt_b=b}) input =
    if testFA wt input
      then single a input
      else single b input
  single (Simple' {dt_win=w,dt_trans=t, dt_other=o}) input
    | IMap.null w =
        case uncons input of
             Nothing -> False
             Just (c,input') ->
               case CMap.findWithDefault o c t of
                 Transition {trans_single=DFA {d_id=did',d_dt=dt'}}
                   | ISet.null did' -> False
                   | otherwise -> single dt' input'
    | otherwise = True

testFA0,testFA :: Uncons text => WhichTest -> text -> Bool
testFA0 Test_BOL _input = True
testFA0 Test_EOL input = case uncons input of
                           Nothing -> True
                           _ -> False
testFA Test_BOL _input = False
testFA Test_EOL input = case uncons input of
                          Nothing -> True
                          _ -> False

{-# INLINE mkTest0 #-}
mkTest0 :: Uncons text => Bool -> WhichTest -> text -> Bool
mkTest0 isMultiline = if isMultiline then test_multiline else test_singleline
  where test_multiline Test_BOL _input = True
        test_multiline Test_EOL input = case uncons input of
                                          Nothing -> True
                                          Just (next,_) -> next == '\n'
        test_singleline Test_BOL _input = True
        test_singleline Test_EOL input = case uncons input of
                                           Nothing -> True
                                           _ -> False

{-# INLINE mkTest #-}
mkTest :: Uncons text => Bool -> WhichTest -> Char -> text -> Bool
mkTest isMultiline = if isMultiline then test_multiline else test_singleline
  where test_multiline Test_BOL prev _input = prev == '\n'
        test_multiline Test_EOL _prev input = case uncons input of
                                                Nothing -> True
                                                Just (next,_) -> next == '\n'
        test_singleline Test_BOL _prev _input = False
        test_singleline Test_EOL _prev input = case uncons input of
                                                Nothing -> True
                                                _ -> False
