{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-| 
The "Text.Regex.TDFA" module provides a backend for regular
expressions. To use it should be imported along with
"Text.Regex.Base".  If you import this along with other backends, then
you should do so with qualified imports, perhaps renamed for
convenience.

Todo:
  runBool case for aborting on shortest match
  frontAnchored
  Cleanup locations of helper functions
  Decide whether to nix DoPa or just replace with Int
  Make untagged TDFA for non-capturing cases
  Pull starTrans into ReadRegex
  Consider replacing Pattern with CorePattern entirely
  Remove parent info from GroupInfo and/or reduce tag resetting workload
    (try to shift work from doing resets to post-processing)

Beyond posix:
  non-capturing groups
  Inverted tests and additional tests
  lazy instead of greedy
  possessive instead of greedy
  leftmost branch instead of leftmost/longest (open/close group instead of tagging)
-}

module Text.Regex.TDFA(getVersion
                      ,module Text.Regex.TDFA.Wrap
                      ,module Text.Regex.TDFA.String
--                      ,module Text.Regex.TDFA.ByteString
--                      ,module Text.Regex.TDFA.ByteString.Lazy
--                      ,module Text.Regex.TDFA.Sequence
                      ,module Text.Regex.Base) where

import Text.Regex.TDFA.Wrap(Regex,CompOption(..),ExecOption(..),(=~),(=~~))
import Text.Regex.TDFA.String()
--import Text.Regex.TDFA.Sequence()
--import Text.Regex.TDFA.ByteString()
--import Text.Regex.TDFA.ByteString.Lazy()
import Text.Regex.Base
import Data.Version(Version(..))

getVersion :: Version
getVersion = Version { versionBranch = [0,50]
                     , versionTags = ["tdfa","unstable"]
                     }

