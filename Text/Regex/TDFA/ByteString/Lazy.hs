{-# OPTIONS_GHC -fno-warn-orphans #-}
{-| 
This modules provides 'RegexMaker' and 'RegexLike' instances for using
'ByteString' with the DFA backend ("Text.Regex.Lib.WrapDFAEngine" and
"Text.Regex.Lazy.DFAEngineFPS").  This module is usually used via
import "Text.Regex.TDFA".

This exports instances of the high level API and the medium level
API of 'compile','execute', and 'regexec'.
-}
module Text.Regex.TDFA.ByteString.Lazy(
  Regex
 ,CompOption
 ,ExecOption
 ,compile
 ,execute
 ,regexec
 ) where

import Data.Array((!),elems)
import qualified Data.ByteString.Lazy.Char8 as L

import Text.Regex.Base(MatchArray,RegexContext(..),RegexMaker(..),RegexLike(..))
import Text.Regex.Base.Impl(polymatch,polymatchM)
import Text.Regex.TDFA.ReadRegex(parseRegex)
import Text.Regex.TDFA.String() -- piggyback on RegexMaker for String
import Text.Regex.TDFA.TDFA(patternToRegex)
import Text.Regex.TDFA.Wrap(Regex(..),CompOption,ExecOption)

{- By Chris Kuklewicz, 2007. BSD License, see the LICENSE file. -}

instance RegexContext Regex L.ByteString L.ByteString where
  match = polymatch
  matchM = polymatchM

instance RegexMaker Regex CompOption ExecOption L.ByteString where
  makeRegexOptsM c e source = makeRegexOptsM c e (L.unpack source)

instance RegexLike Regex L.ByteString where
  matchOnce r = matchOnce r . L.unpack
  matchAll r = matchAll r . L.unpack
  matchCount r = matchCount r . L.unpack
  matchTest r = matchTest r . L.unpack
  matchOnceText regex source = 
    fmap (\ma ->
            let (o32,l32) = ma!0
                o = fi o32
                l = fi l32
            in (L.take o source
               ,fmap (\ol@(off32,len32) ->
                        let off = fi off32
                            len = fi len32
                        in (L.take len (L.drop off source),ol)) ma
               ,L.drop (o+l) source))
         (matchOnce regex source)
  matchAllText regex source =
    map (fmap (\ol@(off32,len32) -> (L.take (fi len32) (L.drop (fi off32) source),ol)))
        (matchAll regex source)

fi = fromIntegral

compile :: CompOption -- ^ Flags (summed together)
        -> ExecOption -- ^ Flags (summed together)
        -> L.ByteString -- ^ The regular expression to compile
        -> Either String Regex -- ^ Returns: the compiled regular expression
compile compOpt execOpt bs =
  case parseRegex (L.unpack bs) of
    Left err -> Left ("parseRegex for Text.Regex.TDFA.ByteString failed:"++show err)
    Right pattern -> Right (patternToRegex pattern compOpt execOpt)

execute :: Regex      -- ^ Compiled regular expression
        -> L.ByteString -- ^ ByteString to match against
        -> Either String (Maybe MatchArray)
execute r bs = Right (matchOnce r bs)

regexec :: Regex      -- ^ Compiled regular expression
        -> L.ByteString -- ^ ByteString to match against
        -> Either String (Maybe (L.ByteString, L.ByteString, L.ByteString, [L.ByteString]))
regexec r bs =
  case matchOnceText r bs of
    Nothing -> Right (Nothing)
    Just (pre,mt,post) ->
      let main = fst (mt!0)
          rest = map fst (tail (elems mt)) -- will be []
      in Right (Just (pre,main,post,rest))
