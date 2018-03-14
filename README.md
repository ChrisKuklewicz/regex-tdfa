# regex-tdfa

This is [regex-tdfa](http://hackage.haskell.org/package/regex-tdfa) which is a pure Haskell regular expression library (for POSIX extended regular expressions) originally written by Christopher Kuklewicz.

The name "tdfa" stands for Tagged-DFA.

## The Relevant Links

Terse documentation is available in [Text.Regex.TDFA haddock](http://hackage.haskell.org/package/regex-tdfa-1.2.2/docs/Text-Regex-TDFA.html).

This was also documented at the [Haskell wiki](https://wiki.haskell.org/Regular_expressions#regex-tdfa).  The original Darcs repository was at [code.haskell.org](http://code.haskell.org/regex-tdfa/).  When not updated, this was forked and maintained by Roman Cheplyaka as [regex-tdfa-rc](http://hackage.haskell.org/package/regex-tdfa-rc).

The new git repository is at [github](https://github.com/ChrisKuklewicz/regex-tdfa), which is primarily maintained by [Artyom (neongreen)](https://github.com/neongreen).

## Known Bugs and Infelicities

* Regexes with large character classes combined with `{m,n}` are very slow and memory-hungry ([#14][]).

  > An example of such a regex is `^[\x0020-\xD7FF]{1,255}$`.

* POSIX submatch semantics are broken in some rare cases ([#12][]).

[#12]: https://github.com/ChrisKuklewicz/regex-tdfa/issues/12
[#14]: https://github.com/ChrisKuklewicz/regex-tdfa/issues/14

## About this package

This was inspired by the algorithm (and Master's thesis) behind the regular expression library known as [TRE or libtre](https://github.com/laurikari/tre/).  This was created by Ville Laurikari and tackled the difficult issue of efficient sub-match capture for POSIX regular expressions.

By building on this thesis and adding a few more optimizations, regex-tdfa matching input text of length N should have O(N) runtime, and should have a maximum memory bounded by the pattern size that does not scale with N. It should do this while returning well defined (and correct) values for the parenthesized sub-matches.

Regardless of performance, nearly every single OS and Libra for POSIX regular expressions has bugs in sub-matches.  This was detailed on the [Regex POSIX Haskell wiki page](https://wiki.haskell.org/Regex_Posix), and can be demonstrated with the [regex-posix-unittest](http://hackage.haskell.org/package/regex-posix-unittest) suite of checks.  Test [regex-tdfa-unittest](http://hackage.haskell.org/package/regex-tdfa-unittest) should show regex-tdfa passing these same checks.  I owe my understanding of the correct behvior and many of these unit tests to Glenn Fowler at AT&T ("An Interpretation of the POSIX regex Standard").

## Other related packages

You can find several other related packages by searching for "tdfa" on [hackage](http://hackage.haskell.org/packages/search?terms=tdfa).

## Document notes

This was written 2016-04-30.
