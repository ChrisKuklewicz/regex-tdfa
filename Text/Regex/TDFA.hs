{-| 

The "Text.Regex.TDFA" module provides a backend for regular
expressions. It provides instances for the classes defined and
documented in "Text.Regex.Base" and re-exported by this module.  If
you import this along with other backends then you should do so with
qualified imports (with renaming for convenience).

This regex-tdfa package implements, correctly, POSIX extended regular
expressions.  It is highly unlikely that the regex-posix package on
your operating system is correct, see
http://www.haskell.org/haskellwiki/Regex_Posix for examples of your
OS's bugs.

This package does provide captured parenthesized subexpressions.

Depending on the text being searched this package supports Unicode.
The [Char] and (Seq Char) text types support Unicode.  The ByteString
and ByteString.Lazy text types only support ASCII.  It is possible to
support utf8 encoded ByteString.Lazy by using regex-tdfa and
regex-tdfa-utf8 packages together  (required the utf8-string package).

As of version 1.1.1 the following GNU extensions are recognized, all
anchors:

\` at beginning of entire text

\' at end of entire text

\< at beginning of word

\> at end of word

\b at either beginning or end of word

\B at neither beginning nor end of word

Where the "word" boundaries means between characters that are and are
not in the [:word:] character class which contains [a-zA-Z0-9_].  Note
that \< and \b may match before the entire text and \> and \b may
match at the end of the entire text.

There is no locale support, so collating elements like [.ch.] are
simply ignored and equivalence classes like [=a=] are converted to
just [a].  The character classes like [:alnum:] are supported over
ASCII only, valid classes are alnum, digit, punct, alpha, graph,
space, blank, lower, upper, cntrl, print, xdigit, word.

This package does not provide "basic" regular expressions.  This
package does not provide back references inside regular expressions.

The package does not provide Perl style regular expressions.  Please
look at the regex-pcre and pcre-light packages instead.

-}

module Text.Regex.TDFA(getVersion_Text_Regex_TDFA
                      ,(=~),(=~~)
                      ,module Text.Regex.TDFA.Common
                      ,module Text.Regex.Base) where

import Data.Version(Version)
import Text.Regex.Base
import Text.Regex.TDFA.String()
import Text.Regex.TDFA.ByteString()
import Text.Regex.TDFA.ByteString.Lazy()
import Text.Regex.TDFA.Sequence()
import Text.Regex.TDFA.Common(Regex,CompOption(..),ExecOption(..))
--import Text.Regex.TDFA.Wrap(Regex,CompOption(..),ExecOption(..),(=~),(=~~))

import Paths_regex_tdfa(version)

getVersion_Text_Regex_TDFA :: Version
getVersion_Text_Regex_TDFA = version


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
