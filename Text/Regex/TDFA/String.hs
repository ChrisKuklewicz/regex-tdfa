{-# OPTIONS_GHC -fno-warn-orphans #-}
{- | 
This modules provides 'RegexMaker' and 'RegexLike' instances for using
'String' with the TDFA backend.

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

import Data.Array((!),elems)

import Text.Regex.Base.Impl(polymatch,polymatchM)
import Text.Regex.Base.RegexLike(RegexMaker(..),RegexLike(..),RegexContext(..),MatchOffset,MatchLength,MatchArray)
import Text.Regex.TDFA.Common(common_error)
import qualified Text.Regex.TDFA.NewDFA as N(matchAll,matchOnce,matchCount,matchTest)
import Text.Regex.TDFA.ReadRegex(parseRegex)
import Text.Regex.TDFA.TDFA(patternToRegex)
import Text.Regex.TDFA.Wrap(Regex(..),CompOption,ExecOption)

{- By Chris Kuklewicz, 2007. BSD License, see the LICENSE file. -}

err :: String -> a
err = common_error "Text.Regex.TDFA.String"

unwrap :: Either String v -> v
unwrap x = case x of Left msg -> err ("Text.Regex.TDFA.String died: "++msg)
                     Right v -> v

compile  :: CompOption -- ^ Flags (summed together)
         -> ExecOption -- ^ Flags (summed together)
         -> String     -- ^ The regular expression to compile (ASCII only, no null bytes)
         -> Either String Regex -- ^ Returns: the compiled regular expression
compile compOpt execOpt source =
  case parseRegex source of
    Left msg -> Left ("parseRegex for Text.Regex.TDFA.String failed:"++show msg)
    Right pattern -> Right (patternToRegex pattern compOpt execOpt)

instance RegexMaker Regex CompOption ExecOption String where
  makeRegexOpts c e source = unwrap (compile c e source)
  makeRegexOptsM c e source = either fail return $ compile c e source

execute :: Regex      -- ^ Compiled regular expression
        -> String     -- ^ String to match against
        -> Either String (Maybe MatchArray)
execute r s = Right (N.matchOnce r s)

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

-- Minimal defintion for now
instance RegexLike Regex String where
  matchOnce = N.matchOnce
  matchAll = N.matchAll
  matchCount = N.matchCount
  matchTest = N.matchTest
-- matchOnceText
-- matchTextAll

instance RegexContext Regex String String where
  match = polymatch
  matchM = polymatchM
