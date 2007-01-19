{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans #-}
{-| 
This modules provides 'RegexMaker' and 'RegexLike' instances for using
'ByteString' with the DFA backend ("Text.Regex.Lib.WrapDFAEngine" and
"Text.Regex.Lazy.DFAEngineFPS").  This module is usually used via
import "Text.Regex.TDFA".

This exports instances of the high level API and the medium level
API of 'compile','execute', and 'regexec'.
-}
module Text.Regex.TDFA.ByteString(
  Regex
 ,CompOption
 ,ExecOption
 ,compile
 ,execute
 ,regexec
 ) where

import Data.Maybe
import Data.Array
import Text.Regex.TDFA.ReadRegex(parseRegex)
import qualified Data.ByteString.Char8 as B
import Text.Regex.TDFA.String() -- piggyback on RegexMaker for String
-- import Text.Regex.TDFA.CorePattern
-- import Text.Regex.TDFA.TNFA
import Text.Regex.TDFA.TDFA
import Text.Regex.TDFA.RunBS
import Text.Regex.TDFA.Wrap(Regex(..),CompOption,ExecOption)
import Text.Regex.Base.Impl(polymatch,polymatchM)
import Text.Regex.Base

instance RegexContext Regex B.ByteString B.ByteString where
  match = polymatch
  matchM = polymatchM

instance RegexMaker Regex CompOption ExecOption B.ByteString where
  makeRegexOptsM c e source = makeRegexOptsM c e (B.unpack source)

instance RegexLike Regex B.ByteString where
  matchOnce = findMatch
  matchAll = findMatchAll
-- matchTest
-- matchOnceText
-- matchCount
-- matchTextAll

compile :: CompOption -- ^ Flags (summed together)
        -> ExecOption -- ^ Flags (summed together)
        -> B.ByteString -- ^ The regular expression to compile
        -> Either String Regex -- ^ Returns: the compiled regular expression
compile compOpt execOpt bs =
  case parseRegex (B.unpack bs) of
    Left err -> Left ("parseRegex for Text.Regex.TDFA.ByteString failed:"++show err)
    Right pattern ->
      let (dfa,i,tags,groups) = patternToDFA pattern compOpt
      in Right (Regex dfa i tags groups compOpt execOpt)

execute :: Regex      -- ^ Compiled regular expression
        -> B.ByteString -- ^ ByteString to match against
        -> Either String (Maybe (Array Int (Int,Int)))
execute r bs = Right (matchOnce r bs)

regexec :: Regex      -- ^ Compiled regular expression
        -> B.ByteString -- ^ ByteString to match against
        -> Either String (Maybe (B.ByteString, B.ByteString, B.ByteString, [B.ByteString]))
regexec r bs =
  case matchOnceText r bs of
    Nothing -> Right (Nothing)
    Just (pre,mt,post) ->
      let main = fst (mt!0)
          rest = map fst (tail (elems mt)) -- will be []
      in Right (Just (pre,main,post,rest))
