{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans #-}
{-| 
This modules provides 'RegexMaker' and 'RegexLike' instances for using
'ByteString' with the DFA backend ("Text.Regex.Lib.WrapDFAEngine" and
"Text.Regex.Lazy.DFAEngineFPS").  This module is usually used via
import "Text.Regex.TDFA".

This exports instances of the high level API and the medium level
API of 'compile','execute', and 'regexec'.
-}
module Text.Regex.TDFA.Sequence(
  Regex
 ,CompOption
 ,ExecOption
 ,compile
 ,execute
 ,regexec
 ) where

import qualified Data.Sequence as S
import Data.Sequence (ViewL(EmptyL,(:<)))

import Text.Regex.Base(MatchArray,RegexContext(..),RegexMaker(..),RegexLike(..))
import Text.Regex.Base.Impl(polymatch,polymatchM)
import Text.Regex.TDFA.Common(common_error,Regex(..),CompOption,ExecOption(captureGroups))
import Text.Regex.TDFA.String() -- piggyback on RegexMaker for String
import Text.Regex.TDFA.TDFA(patternToRegex)
import Text.Regex.TDFA.Wrap()
import Text.Regex.TDFA.ReadRegex(parseRegex)
import qualified Data.Foldable as F(toList)

import Data.Array.IArray((!),listArray,elems,bounds)
import Data.Maybe(listToMaybe)
import Text.Regex.TDFA.NewDFA.Engine(execMatch)
import Text.Regex.TDFA.NewDFA.Tester as Tester(matchTest)

{- By Chris Kuklewicz, 2007. BSD License, see the LICENSE file. -}

instance RegexContext Regex (S.Seq Char) (S.Seq Char) where
  match = polymatch
  matchM = polymatchM

instance RegexMaker Regex CompOption ExecOption (S.Seq Char) where
  makeRegexOptsM c e source = either fail return $ compile c e source

instance RegexLike Regex (S.Seq Char) where
  matchOnce r s = listToMaybe (matchAll r s)
  matchAll r s = execMatch r 0 '\n' s
  matchCount r s = length (matchAll r' s)
    where r' = r { regex_execOptions = (regex_execOptions r) {captureGroups = False} }
  matchTest = Tester.matchTest
  matchOnceText regex source = 
    fmap (\ma -> let (o,l) = ma!0
                 in (S.take o source
                    ,fmap (\ol@(off,len) -> (S.take len (S.drop off source),ol)) ma
                    ,S.drop (o+l) source))
         (matchOnce regex source)
  matchAllText regex source =
    map (fmap (\ol@(off,len) -> (S.take len (S.drop off source),ol)))
        (matchAll regex source)

{-# INLINE toList #-}
toList :: S.Seq Char -> [Char]
toList s = expand (S.viewl s) where
  expand EmptyL = []
  expand (c :< cs) = c : expand (S.viewl cs)

compile :: CompOption -- ^ Flags (summed together)
        -> ExecOption -- ^ Flags (summed together)
        -> (S.Seq Char) -- ^ The regular expression to compile
        -> Either String Regex -- ^ Returns: the compiled regular expression
compile compOpt execOpt bs =
  case parseRegex (toList bs) of
    Left err -> Left ("parseRegex for Text.Regex.TDFA.ByteString failed:"++show err)
    Right pattern -> Right (patternToRegex pattern compOpt execOpt)

execute :: Regex      -- ^ Compiled regular expression
        -> (S.Seq Char) -- ^ ByteString to match against
        -> Either String (Maybe MatchArray)
execute r bs = Right (matchOnce r bs)

regexec :: Regex      -- ^ Compiled regular expression
        -> (S.Seq Char) -- ^ ByteString to match against
        -> Either String (Maybe ((S.Seq Char), (S.Seq Char), (S.Seq Char), [(S.Seq Char)]))
regexec r bs =
  case matchOnceText r bs of
    Nothing -> Right (Nothing)
    Just (pre,mt,post) ->
      let main = fst (mt!0)
          rest = map fst (tail (elems mt)) -- will be []
      in Right (Just (pre,main,post,rest))
