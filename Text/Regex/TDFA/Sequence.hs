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

import Data.Maybe
import Data.Array
import Text.Regex.TDFA.ReadRegex(parseRegex)
import Data.Sequence(ViewL(..))
import qualified Data.Sequence as S
import Text.Regex.TDFA.String() -- piggyback on RegexMaker for String
import Text.Regex.TDFA.TDFA
import Text.Regex.TDFA.Run
import Text.Regex.TDFA.Wrap(Regex(..),CompOption,ExecOption)
import Text.Regex.Base.Impl(polymatch,polymatchM)
import Text.Regex.Base

instance RegexContext Regex (S.Seq Char) (S.Seq Char) where
  match = polymatch
  matchM = polymatchM

instance RegexMaker Regex CompOption ExecOption (S.Seq Char) where
  makeRegexOptsM c e source = makeRegexOptsM c e (toList source)

instance RegexLike Regex (S.Seq Char) where
  matchOnce = findMatch isNull headTail
  matchAll = findMatchAll isNull headTail
  matchCount = countMatchAll isNull headTail
-- matchTest
-- matchOnceText
-- matchTextAll

{-# INLINE isNull #-}
isNull :: S.Seq Char -> Bool
isNull = S.null
{-# INLINE headTail #-}
headTail :: S.Seq Char -> (Char,S.Seq Char)
headTail s = let (h :< t) = S.viewl s in (h,t)
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
      let (dfa,i,tags,groups) = patternToDFA pattern compOpt
      in Right (Regex dfa i tags groups compOpt execOpt)

execute :: Regex      -- ^ Compiled regular expression
        -> (S.Seq Char) -- ^ ByteString to match against
        -> Either String (Maybe (Array Int (Int,Int)))
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
