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
import Text.Regex.TDFA.TDFA(patternToRegex)
import Text.Regex.TDFA.Wrap(Regex(..),CompOption,ExecOption)
import Text.Regex.TDFA.ReadRegex(parseRegex)
import qualified Data.Foldable as F(toList)

{- By Chris Kuklewicz, 2007. BSD License, see the LICENSE file. -}

instance RegexContext Regex (S.Seq Char) (S.Seq Char) where
  match = polymatch
  matchM = polymatchM

instance RegexMaker Regex CompOption ExecOption (S.Seq Char) where
  makeRegexOptsM c e source = either fail return $ compile c e source

instance RegexLike Regex (S.Seq Char) where
  matchOnce r = matchOnce r . F.toList
  matchAll r = matchAll r . F.toList
  matchCount r = matchCount r . F.toList
  matchTest r = matchTest r . F.toList
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
