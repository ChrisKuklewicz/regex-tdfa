{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-| 
The "Text.Regex.TDFA" module provides a backend for regular
expressions. To use it should be imported along with
"Text.Regex.Base".  If you import this along with other backends, then
you should do so with qualified imports, perhaps renamed for
convenience.

Todo:
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
                      ,module Text.Regex.TDFA.ByteString
                      ,module Text.Regex.TDFA.ByteString.Lazy
                      ,module Text.Regex.TDFA.Sequence
                      ,module Text.Regex.Base) where

import Text.Regex.TDFA.Wrap(Regex,CompOption(..),ExecOption(..),(=~),(=~~))
import Text.Regex.TDFA.String()
import Text.Regex.TDFA.Sequence()
import Text.Regex.TDFA.ByteString()
import Text.Regex.TDFA.ByteString.Lazy()
import Text.Regex.Base
import Data.Version(Version(..))

getVersion :: Version
getVersion = Version { versionBranch = [0,21]
                     , versionTags = ["tdfa","unstable"]
                     }

{-
-- trebug

Prelude Text.Regex.TRE Text.Regex.Base> let r=makeRegex  "((a)|(b*)|c(c*))*" :: Regex in match r "acbbacbb" :: MatchArray
array (0,4) [(0,(0,8)),(1,(6,2)),(2,(-1,0)),(3,(6,2)),(4,(6,2))]

Prelude Text.Regex.TRE Text.Regex.Base Text.Regex.Posix> let r=makeRegex  "(b*|c(c*))*" :: Text.Regex.TRE.Regex in match r "cbb" :: MatchArray
array (0,2) [(0,(0,3)),(1,(1,2)),(2,(1,2))]

The above is a bug.  the (c*) group should not match "bb".

-}
{- tofix:

Exceptions to fix:
*Text.Regex.TDFA.Live> "x" =~ "((x?)?x)*" :: MatchArray
array *** Exception: Ord instance for TagTask (called from TNFA.bestTrans.choose EQ branch) found (ResetTask,TagTask)

*Text.Regex.TDFA.Live> starTrans . fst . toP $ "((x?)?x)*"
PStar (PGroup 1 (PConcat [POr [PGroup 2 (POr [PChar {getDoPa = #1, getPatternChar = 'x'}
                                             ,PEmpty])
                              ,PEmpty]
                         ,PChar {getDoPa = #2, getPatternChar = 'x'}]))

*Text.Regex.TDFA.Live> toQ "((x?)?x)*"
(Q { nullQ = [(SetTestInfo [],[(1,PreUpdate TagTask)])]
  , takes = (0,Nothing)
  , preTag = Nothing
  , postTag = Just 1
  , tagged = True
  , wants = WantsQT
  , unQ = Star {getOrbit = Just 2, reset = [4,6], unStar = Q { nullQ = []
            , takes = (1,Just 2)
            , preTag = Nothing
            , postTag = Nothing
            , tagged = True
            , wants = WantsQNFA
            , unQ = Seq Q { nullQ = [(SetTestInfo [],[(5,PreUpdate TagTask),(3,PreUpdate TagTask),(6,PreUpdate TagTask)])]
                      , takes = (0,Just 1)
(start 1,2)           , preTag = Just 3
                      , postTag = Just 5
                      , tagged = True
                      , wants = WantsQNFA
                      , unQ = Or [Q { nullQ = [(SetTestInfo [],[(6,PreUpdate TagTask)])]
                                , takes = (0,Just 1)
                                , preTag = Nothing
(stop 2)                        , postTag = Just 6
                                , tagged = True
                                , wants = WantsQNFA
                                , unQ = Or [Q { nullQ = []
                                          , takes = (1,Just 1)
                                          , preTag = Nothing
                                          , postTag = Just 7
                                          , tagged = False
                                          , wants = WantsQNFA
                                          , unQ = OneChar (PChar {getDoPa = #1, getPatternChar = 'x'})
                                         },Q { nullQ = [(SetTestInfo [],[])]
                                          , takes = (0,Just 0)
                                          , preTag = Nothing
                                          , postTag = Nothing
                                          , tagged = False
                                          , wants = WantsEither
                                          , unQ = Empty
                                         }]
                               },Q { nullQ = [(SetTestInfo [],[])]
                                , takes = (0,Just 0)
                                , preTag = Nothing
                                , postTag = Nothing
                                , tagged = False
                                , wants = WantsEither
                                , unQ = Empty
                               }]
                     } Q { nullQ = []
                      , takes = (1,Just 1)
                      , preTag = Nothing
(stop 1)              , postTag = Just 4
                      , tagged = False
                      , wants = WantsQNFA
                      , unQ = OneChar (PChar {getDoPa = #2, getPatternChar = 'x'})
                     }
           }}
 },
array (0,7) [(0,Minimize),(1,Maximize),(2,Orbit),(3,Minimize),(4,Maximize),(5,Maximize),(6,Maximize),(7,Maximize)],
array (1,2) [(1,[GroupInfo {thisIndex = 1, parentIndex = 0, startTag = 3, stopTag = 4}])
            ,(2,[GroupInfo {thisIndex = 2, parentIndex = 1, startTag = 3, stopTag = 6}])])
*Text.Regex.TDFA.Live> 

*Text.Regex.TDFA.Live> display_NFA "((x?)?x)*"
QNFA {q_id = 0
     ,q_qt = {qt_win=[]
, qt_trans=[('x',[(1,[(#2,[(4,PostUpdate TagTask)])])])]
, qt_other=[]}
}
QNFA {q_id = 1
     ,q_qt = {qt_win=[(2,PreUpdate LeaveOrbitTask),(1,PreUpdate TagTask),(1,PreUpdate TagTask)]
, qt_trans=[('x',[(0,[(#1,[(4,PreUpdate ResetTask),(6,PreUpdate ResetTask),(2,PreUpdate EnterOrbitTask),(3,PreUpdate TagTask),(7,PostUpdate TagTask),(6,PostUpdate TagTask),(5,PostUpdate TagTask)])])
                 ,(1,[(#2,[(4,PreUpdate ResetTask),(6,PreUpdate ResetTask),(2,PreUpdate EnterOrbitTask),(3,PreUpdate TagTask),(5,PreUpdate TagTask),(4,PostUpdate TagTask)])
                     ,(#2,[(4,PreUpdate ResetTask),(6,PreUpdate ResetTask),(2,PreUpdate EnterOrbitTask),(3,PreUpdate TagTask),(6,PreUpdate TagTask),(5,PreUpdate TagTask),(4,PostUpdate TagTask)])])])]
, qt_other=[]}
}


cleaned up :

                 ,(1,[(#2,[(2,PreUpdate EnterOrbitTask),(3,PreUpdate TagTask),(4,PostUpdate TagTask),(5,PreUpdate TagTask),(6,PreUpdate ResetTask)])
                     ,(#2,[(2,PreUpdate EnterOrbitTask),(3,PreUpdate TagTask),(4,PostUpdate TagTask),(5,PreUpdate TagTask),(6,PreUpdate TagTask)])])])]

So the nested ? of (x?)? gives two paths of "emptiness".  Which path is better?
Given the 'x', if the match succeed the outer ? can always succeed and the inner can succeed or fail.  So the TagTask is preferred.

-}

{- 

*Text.Regex.TDFA.Live> "xx" =~ "((x?)*x)*" :: MatchArray

array *** Exception: makeTagComparer.compareOrbits: Nothing found in Scratch :
array (0,9) [(0,Minimize),(1,Maximize),(2,Orbit),(3,Minimize),(4,Maximize),(5,Maximize),(6,Orbit),(7,Minimize),(8,Maximize),(9,Maximize)]

"A" (fromList [(0,0),(2,0),(3,0),(4,2),(5,1),(7,0),(8,1),(9,1)],fromList []) -- should be ,fromList [(1,fromList [0])])

"B" (fromList [(0,0),(2,0),(3,1),(4,2),(5,1),(7,1),(8,1)],fromList [(2,fromList [0,1])])

"A" matched #1 then #2
"B" matched #2 then #2

*Text.Regex.TDFA.Live> starTrans . fst . toP $ "((x?)*x)*"

PStar (PGroup 1 (PConcat [PStar (PGroup 2 (POr [PChar {getDoPa = #1, getPatternChar = 'x'},PEmpty])),PChar {getDoPa = #2, getPatternChar = 'x'}]))
*Text.Regex.TDFA.Live> toQ $ "((x?)*x)*"
(Q { nullQ = [(SetTestInfo [],[(1,PreUpdate TagTask)])]
  , takes = (0,Nothing)
  , preTag = Nothing
  , postTag = Just 1
  , tagged = True
  , wants = WantsQT
  , unQ = Star {getOrbit = Just 2, reset = [4,8], unStar = Q { nullQ = []
            , takes = (1,Nothing)
            , preTag = Nothing
            , postTag = Nothing
            , tagged = True
            , wants = WantsQNFA
            , unQ = Seq Q { nullQ = [(SetTestInfo [],[(5,PreUpdate TagTask),(3,PreUpdate TagTask)])]
                      , takes = (0,Nothing)
start 1               , preTag = Just 3
                      , postTag = Just 5
                      , tagged = True
                      , wants = WantsQT
                      , unQ = Star {getOrbit = Just 6, reset = [8], unStar = Q { nullQ = [(SetTestInfo [],[(8,PreUpdate TagTask),(7,PreUpdate TagTask)])]
                                , takes = (0,Just 1)
start 2                         , preTag = Just 7
stop 2                          , postTag = Just 8
                                , tagged = True
                                , wants = WantsQNFA
                                , unQ = Or [Q { nullQ = []
                                          , takes = (1,Just 1)
                                          , preTag = Nothing
                                          , postTag = Just 9
                                          , tagged = False
                                          , wants = WantsQNFA
                                          , unQ = OneChar (PChar {getDoPa = #1, getPatternChar = 'x'})
                                         },Q { nullQ = [(SetTestInfo [],[])]
                                          , takes = (0,Just 0)
                                          , preTag = Nothing
                                          , postTag = Nothing
                                          , tagged = False
                                          , wants = WantsEither
                                          , unQ = Empty
                                         }]
                               }}
                     } Q { nullQ = []
                      , takes = (1,Just 1)
                      , preTag = Nothing
stop 1                , postTag = Just 4
                      , tagged = False
                      , wants = WantsQNFA
                      , unQ = OneChar (PChar {getDoPa = #2, getPatternChar = 'x'})
                     }
           }}
 },
array (0,9) [(0,Minimize),(1,Maximize),(2,Orbit),(3,Minimize),(4,Maximize),(5,Maximize),(6,Orbit),(7,Minimize),(8,Maximize),(9,Maximize)],
array (1,2) [(1,[GroupInfo {thisIndex = 1, parentIndex = 0, startTag = 3, stopTag = 4}])
            ,(2,[GroupInfo {thisIndex = 2, parentIndex = 1, startTag = 7, stopTag = 8}])])

*Text.Regex.TDFA.Live> display_NFA $ "((x?)*x)*"
QNFA {q_id = 0
     ,q_qt = {qt_win=[]
, qt_trans=[('x',[(0,[(#1,[(8,PreUpdate ResetTask),(6,PreUpdate EnterOrbitTask),(7,PreUpdate TagTask),(9,PostUpdate TagTask),(8,PostUpdate TagTask)])])
                 ,(1,[(#2,[(6,PreUpdate LeaveOrbitTask),(5,PreUpdate TagTask),(4,PostUpdate TagTask)])])])]
, qt_other=[]}
}
QNFA {q_id = 1
     ,q_qt = {qt_win=[(2,PreUpdate LeaveOrbitTask),(1,PreUpdate TagTask),(1,PreUpdate TagTask)]
, qt_trans=[('x',[(0,[(#1,[(4,PreUpdate ResetTask),(8,PreUpdate ResetTask),(2,PreUpdate EnterOrbitTask),(3,PreUpdate TagTask),(8,PreUpdate ResetTask),(6,PreUpdate EnterOrbitTask),(7,PreUpdate TagTask),(9,PostUpdate TagTask),(8,PostUpdate TagTask)])])
                 ,(1,[(#2,[(4,PreUpdate ResetTask),(8,PreUpdate ResetTask),(2,PreUpdate EnterOrbitTask),(3,PreUpdate TagTask),(8,PreUpdate TagTask),(7,PreUpdate TagTask),(5,PreUpdate TagTask),(4,PostUpdate TagTask)])])])]
, qt_other=[]}
}

*Text.Regex.TDFA.Live> putStr . examineDFA . toDFA $ "((x?)*x)*"

DFA {d_id = [0,1]
    ,d_dt = Simple' { dt_win = [(1,([(1,0),(2,-1)],["Leaving Orbit (2,0)"]))]
        , dt_trans = ('x',([0,1],[(0,[(0,(#1,([(7,0),(8,1),(9,1)],["Entering Orbit (6,0)"])))
                                     ,(1,(#1,([(3,0),(4,-1),(7,0),(8,1),(9,1)],["Entering Orbit (2,0)","Entering Orbit (6,0)"])))])
                                 ,(1,[(0,(#2,([(4,1),(5,0),(6,-1)],["Leaving Orbit (6,0)"])))
                                     ,(1,(#2,([(3,0),(4,1),(5,0),(7,0),(8,0)],["Entering Orbit (2,0)"])))])]))
        , dt_other = None
        }
}
DFA {d_id = [1]
    ,d_dt = Simple' { dt_win = [(1,([(1,0),(2,-1)],["Leaving Orbit (2,0)"]))]
        , dt_trans = ('x',([0,1],[(0,[(1,(#1,([(3,0),(4,-1),(7,0),(8,1),(9,1)],["Entering Orbit (2,0)","Entering Orbit (6,0)"])))])
                                 ,(1,[(1,(#2,([(3,0),(4,1),(5,0),(7,0),(8,0)],["Entering Orbit (2,0)"])))])]))
        , dt_other = None
        }
}
*Text.Regex.TDFA.Live> 
-}

{- Checking "xx" vs "((x*)*x)" in regress:

("xx","((x*)*x)",array *** Exception: WTF 2 w/o 1 : 3 :
 fromList [(#1,[])
          ,(#1,[(5,PreUpdate TagTask),(5,PreUpdate ResetTask),(3,PreUpdate EnterOrbitTask),(4,PreUpdate TagTask)])]

*Text.Regex.TDFA.Live> starTrans . fst . toP $ "((x*)*x)"
PGroup 1 (PConcat [PStar (PGroup 2 (PStar (PChar {getDoPa = #1, getPatternChar = 'x'}))),PChar {getDoPa = #2, getPatternChar = 'x'}])
*Text.Regex.TDFA.Live> toQ $ "((x*)*x)"
(Q { nullQ = []
  , takes = (1,Nothing)
  , preTag = Nothing
  , postTag = Nothing
  , tagged = True
  , wants = WantsQNFA
  , unQ = Seq Q { nullQ = [(SetTestInfo [],[(2,PreUpdate TagTask)])]
            , takes = (0,Nothing)
            , preTag = Nothing
            , postTag = Just 2
            , tagged = True
            , wants = WantsQT
            , unQ = Star {getOrbit = Just 3, reset = [5], unStar = Q { nullQ = [(SetTestInfo [],[(5,PreUpdate TagTask),(4,PreUpdate TagTask)])]
                      , takes = (0,Nothing)
                      , preTag = Just 4
                      , postTag = Just 5
                      , tagged = True
                      , wants = WantsQT
                      , unQ = Star {getOrbit = Nothing, reset = [], unStar = Q { nullQ = []
                                , takes = (1,Just 1)
                                , preTag = Nothing
                                , postTag = Nothing
                                , tagged = False
                                , wants = WantsQNFA
                                , unQ = OneChar (PChar {getDoPa = #1, getPatternChar = 'x'})
                               }}
                     }}
           } Q { nullQ = []
            , takes = (1,Just 1)
            , preTag = Nothing
            , postTag = Just 1
            , tagged = False
            , wants = WantsQNFA
            , unQ = OneChar (PChar {getDoPa = #2, getPatternChar = 'x'})
           }
 },array (0,5) [(0,Minimize),(1,Maximize),(2,Maximize),(3,Orbit),(4,Minimize),(5,Maximize)],array (1,2) [(1,[GroupInfo {thisIndex = 1, parentIndex = 0, startTag = 0, stopTag = 1}]),(2,[GroupInfo {thisIndex = 2, parentIndex = 1, startTag = 4, stopTag = 5}])])
*Text.Regex.TDFA.Live> display_NFA $ "((x*)*x)"
QNFA {q_id = 0
     ,q_qt = {qt_win=[(1,PreUpdate TagTask)]
, qt_trans=[]
, qt_other=[]}
}
QNFA {q_id = 1
     ,q_qt = {qt_win=[]
, qt_trans=[('x',[(0,[(#2,[(5,PreUpdate TagTask),(3,PreUpdate LeaveOrbitTask),(2,PreUpdate TagTask),(1,PostUpdate TagTask)])])
                 ,(1,[(#1,[])
                     ,(#1,[(5,PreUpdate TagTask),(5,PreUpdate ResetTask),(3,PreUpdate EnterOrbitTask),(4,PreUpdate TagTask)])])])]
, qt_other=[]}
}
QNFA {q_id = 2
     ,q_qt = {qt_win=[]
, qt_trans=[('x',[(0,[(#2,[(5,PreUpdate TagTask),(4,PreUpdate TagTask),(2,PreUpdate TagTask),(1,PostUpdate TagTask)])])
                 ,(1,[(#1,[(5,PreUpdate ResetTask),(3,PreUpdate EnterOrbitTask),(4,PreUpdate TagTask)])])])]
, qt_other=[]}
}
*Text.Regex.TDFA.Live> putStr . examineDFA . toDFA $ "((x*)*x)"
*** Exception: WTF 2 w/o 1
*Text.Regex.TDFA.Live> 

-}