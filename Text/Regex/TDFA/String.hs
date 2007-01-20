{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans #-}
{-| 
This modules provides 'RegexMaker' and 'RegexLike' instances for using
'String' with the TDFA backend ("Text.Regex.Lib.WrapTDFAEngine" and
"Text.Regex.Lazy.TDFAEngine").  This module is usually used via import
"Text.Regex.TDFA".

This exports instances of the high level API and the medium level
API of 'compile','execute', and 'regexec'.
-}
module Text.Regex.TDFA.String(
  -- ** Types
  Regex
 ,MatchOffset
 ,MatchLength
 ,CompOption
 ,ExecOption
  -- ** Medium level API functions
 ,compile
 ,execute
 ,regexec
 ) where

import Data.Array

import Text.Regex.Base.RegexLike(RegexMaker(..),RegexLike(..),RegexContext(..),MatchOffset,MatchLength,MatchArray)

import Text.Regex.TDFA.ReadRegex(parseRegex)
import Text.Regex.TDFA.Wrap(Regex(..),CompOption,ExecOption)
-- import Text.Regex.TDFA.CorePattern
-- import Text.Regex.TDFA.TNFA
import Text.Regex.TDFA.TDFA
import Text.Regex.TDFA.Run
--import Text.Regex.TDFA.Engine(findRegex,matchesRegex,countRegex,accept)
import Text.Regex.Base.Impl(polymatch,polymatchM)
-- import Text.Regex.Base

unwrap :: Either String v -> v
unwrap x = case x of Left err -> error ("Text.Regex.TDFA.String died: "++ err)
                     Right v -> v

compile  :: CompOption -- ^ Flags (summed together)
         -> ExecOption -- ^ Flags (summed together)
         -> String     -- ^ The regular expression to compile (ASCII only, no null bytes)
         -> Either String Regex -- ^ Returns: the compiled regular expression
compile compOpt execOpt source =
  case parseRegex source of
    Left err -> Left ("parseRegex for Text.Regex.TDFA.String failed:"++show err)
    Right pattern ->
      let (dfa,i,tags,groups) = patternToDFA compOpt pattern
      in Right (Regex dfa i tags groups compOpt execOpt)

instance RegexMaker Regex CompOption ExecOption String where
  makeRegexOpts c e source = unwrap (compile c e source)
  makeRegexOptsM c e source = either fail return $ compile c e source

execute :: Regex      -- ^ Compiled regular expression
        -> String     -- ^ String to match against
        -> Either String (Maybe MatchArray)
execute r s = Right (matchOnce r s)

regexec :: Regex      -- ^ Compiled regular expression
        -> String     -- ^ String to match against
        -> Either String (Maybe (String, String, String, [String]))
regexec r s =
  case matchOnceText r s of
    Nothing -> Right Nothing
    Just (pre,mt,post) ->
      let main = fst (mt!0)
          rest = map fst (tail (elems mt)) -- will be []
      in Right (Just (pre,main,post,rest))

{-# INLINE headTail #-}
headTail :: String -> (Char,String)
headTail s = (head s,tail s)

-- Minimal defintion for now
instance RegexLike Regex String where
  matchOnce = findMatch null headTail
  matchAll = findMatchAll null headTail
  matchCount = countMatchAll null headTail
-- matchTest
-- matchOnceText
-- matchTextAll

instance RegexContext Regex String String where
  match = polymatch
  matchM = polymatchM
