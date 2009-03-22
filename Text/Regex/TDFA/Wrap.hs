-- | "Text.Regex.TDFA.Wrap" provides the instance of RegexOptions and
-- the definition of (=~) and (=~~).  This is all re-exported by
-- "Text.Regex.TDFA".

module Text.Regex.TDFA.Wrap(Regex(..),CompOption(..),ExecOption(..),(=~),(=~~)) where

{- By Chris Kuklewicz, 2007-2009. BSD License, see the LICENSE file. -}

import Text.Regex.Base.RegexLike(RegexMaker(..),RegexContext(..))
import Text.Regex.TDFA.Common(CompOption(..),ExecOption(..),Regex(..))

-- | This is the pure functional matching operator.  If the target
-- cannot be produced then some empty result will be returned.  If
-- there is an error in processing, then 'error' will be called.
(=~) :: (RegexMaker Regex CompOption ExecOption source,RegexContext Regex source1 target)
     => source1 -> source -> target
(=~) x r = let make :: RegexMaker Regex CompOption ExecOption a => a -> Regex
               make = makeRegex
           in match (make r) x

-- | This is the monadic matching operator.  If a single match fails,
-- then 'fail' will be called.
(=~~) :: (RegexMaker Regex CompOption ExecOption source,RegexContext Regex source1 target,Monad m)
      => source1 -> source -> m target
(=~~) x r = do let make :: (RegexMaker Regex CompOption ExecOption a, Monad m) => a -> m Regex
                   make = makeRegexM
               q <- make r
               matchM q x
