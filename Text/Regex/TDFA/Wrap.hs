{-# OPTIONS_GHC -fglasgow-exts  -fno-warn-orphans #-}
{-|
The "Text.Regex.Lib.WrapDFAEngine" provides the backend for
"Text.Regex.DFA".  This provides the 'Regex' type and 'RegexOptions'
instance for them, and 'RegexMaker' instances for 'String' and 'ByteString'.

Details on the DFA engine can be found in "Text.Regex.DFA" and license
information in "Text.Regex.Lazy.DFAEngine".
-}
module Text.Regex.TDFA.Wrap(Regex(..),CompOption(..),ExecOption(..),(=~),(=~~)) where

import Text.Regex.Base.RegexLike(RegexMaker(..),RegexOptions(..),RegexContext(..))
import Text.Regex.TDFA.Common -- (CompOption(..),ExecOption(..))
import Text.Regex.TDFA.TDFA
import Text.Regex.TDFA.CorePattern
import Text.Regex.TDFA.Pattern
import Data.Array.IArray

-- | The DFA backend specific 'Regex' type, used by this module's '=~'
-- and '=~~' operators.
data Regex = Regex {regex_dfa::DFA
                   ,regex_init::Index
                   ,regex_tags::Array Tag OP
                   ,regex_groups::Array PatternIndex [GroupInfo]
                   ,regex_compOptions::CompOption
                   ,regex_execOptions::ExecOption}

instance RegexOptions Regex CompOption ExecOption where
  blankCompOpt = defaultCompOpt
  blankExecOpt = defaultExecOpt
  defaultCompOpt = CompOption {caseSensitive = True,multiline = True}
  defaultExecOpt = ExecOption {captureGroups = True}
  setExecOpts e r = r {regex_execOptions=e}
  getExecOpts r = regex_execOptions r

-- | This is the pure functional matching operator.  If the target
-- cannot be produced then some empty result will be returned.  If
-- there is an error in processing, then 'error' will be called.
(=~) :: (RegexMaker Regex CompOption ExecOption source,RegexContext Regex source1 target)
     => source1 -> source -> target
(=~) x r = let q :: Regex
               q = makeRegex r
           in match q x

-- | This is the monadic matching operator.  If a single match fails,
-- then 'fail' will be called.
(=~~) :: (RegexMaker Regex CompOption ExecOption source,RegexContext Regex source1 target,Monad m)
      => source1 -> source -> m target
(=~~) x r = let q :: Regex
                q = makeRegex r
            in matchM q x
