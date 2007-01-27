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

import Data.Array((!),elems)
import Data.Sequence as S

import Text.Regex.Base(MatchArray,RegexContext(..),RegexMaker(..),RegexLike(..))
import Text.Regex.Base.Impl(polymatch,polymatchM)
import Text.Regex.TDFA.String() -- piggyback on RegexMaker for String
import Text.Regex.TDFA.TDFA(patternToDFA)
import Text.Regex.TDFA.RunSeq(findMatch,findMatchAll,countMatchAll)
import Text.Regex.TDFA.Wrap(Regex(..),CompOption,ExecOption)
import Text.Regex.TDFA.ReadRegex(parseRegex)

{- By Chris Kuklewicz, 2007. BSD License, see the LICENSE file. -}

instance RegexContext Regex (S.Seq Char) (S.Seq Char) where
  match = polymatch
  matchM = polymatchM

instance RegexMaker Regex CompOption ExecOption (S.Seq Char) where
  makeRegexOptsM c e source = makeRegexOptsM c e (toList source)

instance RegexLike Regex (S.Seq Char) where
  matchOnce = findMatch
  matchAll = findMatchAll
  matchCount = countMatchAll
-- matchTest
-- matchOnceText
-- matchTextAll

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
    Right pattern ->
      let (dfa,i,tags,groups) = patternToDFA compOpt pattern
      in Right (Regex dfa i tags groups compOpt execOpt)

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
