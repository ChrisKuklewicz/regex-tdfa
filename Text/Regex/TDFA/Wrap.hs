{-# OPTIONS -fno-warn-orphans #-}
-- | "Text.Regex.TDFA.Wrap" provides the instance of RegexOptions and
-- the definition of (=~) and (=~~).  This is all re-exported by
-- "Text.Regex.TDFA".

module Text.Regex.TDFA.Wrap(Regex(..),CompOption(..),ExecOption(..),(=~),(=~~)) where

{- By Chris Kuklewicz, 2007. BSD License, see the LICENSE file. -}

import Text.Regex.Base.RegexLike(RegexMaker(..),RegexOptions(..),RegexContext(..))
import Text.Regex.TDFA.Common(CompOption(..),ExecOption(..),Regex(..))

instance RegexOptions Regex CompOption ExecOption where
  blankCompOpt = defaultCompOpt
  blankExecOpt = defaultExecOpt
  defaultCompOpt = CompOption { caseSensitive = True
                              , multiline = True
                              , rightAssoc = True
                              , lastStarGreedy = False
                              }
  defaultExecOpt = ExecOption { captureGroups = True
                              , testMatch = False
                              }
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
(=~~) x r = do (q :: Regex) <- q = makeRegexM r
               matchM q x
