{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{- Wrong answer

"A\n_"
"(^|()|.|()){0,3}()"

("TDFA   ",("",array (0,4) [(0,("",(0,0))),(1,("",(-1,0))),(2,("",(-1,0))),(3,("",(-1,0))),(4,("",(0,0)))],"A\n_"))

-}


{-| 
The "Text.Regex.TDFA" module provides a backend for regular
expressions. To use it should be imported along with
"Text.Regex.Base".  If you import this along with other backends, then
you should do so with qualified imports, perhaps renamed for
convenience.

Todo:
  compNoCapture to avoid creating any tags and optimize inStar stuff...
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
{-
                      ,module Text.Regex.TDFA.ByteString
                      ,module Text.Regex.TDFA.ByteString.Lazy
                      ,module Text.Regex.TDFA.Sequence
-}
                      ,module Text.Regex.Base) where

import Data.Version(Version(..))
import Text.Regex.Base
import Text.Regex.TDFA.String()
{-
import Text.Regex.TDFA.ByteString()
import Text.Regex.TDFA.ByteString.Lazy()
import Text.Regex.TDFA.Sequence()
-}
import Text.Regex.TDFA.Wrap(Regex,CompOption(..),ExecOption(..),(=~),(=~~))

getVersion :: Version
getVersion = Version { versionBranch = [0,90]
                     , versionTags = ["tdfa","unstable"]
                     }
