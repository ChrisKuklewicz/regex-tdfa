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
getVersion = Version { versionBranch = [0,35]
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

{-
Turning off orbit with lastStarGreedy = True and running regress reveals:

("xx","((x*)*x)",array *** Exception: Text.Regex.TDFA.TNFA.bestTrans.choose Minimize 2 w/o 1 : 4 : fromList [(#1,[]),(#1,[(5,PreUpdate TagTask),(5,PreUpdate ResetTask),(4,PreUpdate TagTask)])]

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
 },
array (0,5) [(0,Minimize),(1,Maximize),(2,Maximize),(3,Orbit),(4,Minimize),(5,Maximize)],
array (1,2) [(1,[GroupInfo {thisIndex = 1, parentIndex = 0, startTag = 0, stopTag = 1}])
            ,(2,[GroupInfo {thisIndex = 2, parentIndex = 1, startTag = 4, stopTag = 5}])])

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

aka

, qt_trans=[('x',[(0,[(#2,[(5,PreUpdate TagTask),(2,PreUpdate TagTask),(1,PostUpdate TagTask)])])
                 ,(1,[(#1,[])
                     ,(#1,[(5,PreUpdate TagTask),(5,PreUpdate ResetTask),(4,PreUpdate TagTask)])])])]

The merging of the above causes the error.
Tag 4 is the smalled tag that is different and only the second has it and it is Minimized.
Clearly the first is preferred, as it uses the inner *, while the second loops the outer *.
So the absence of the tag is preferred.

, qt_other=[]}
}
QNFA {q_id = 2
     ,q_qt = {qt_win=[]
, qt_trans=[('x',[(0,[(#2,[(5,PreUpdate TagTask),(4,PreUpdate TagTask),(2,PreUpdate TagTask),(1,PostUpdate TagTask)])])
                 ,(1,[(#1,[(5,PreUpdate ResetTask),(3,PreUpdate EnterOrbitTask),(4,PreUpdate TagTask)])])])]

aka

, qt_trans=[('x',[(0,[(#2,[(5,PreUpdate TagTask),(4,PreUpdate TagTask),(2,PreUpdate TagTask),(1,PostUpdate TagTask)])])
                 ,(1,[(#1,[(5,PreUpdate ResetTask),(4,PreUpdate TagTask)])])])]

, qt_other=[]}
}


*Text.Regex.TDFA.Live> putStr . examineDFA . toDFA $ "((x*)*x)"

DFA {d_id = [0,1]
    ,d_dt = Simple' { dt_win = [(0,([(1,0)],[]))]
        , dt_trans = ('x',([0,1],[(0,[(1,(#2,([(1,1),(2,0),(3,-99),(5,0)],[])))]),(1,[(1,(#1,([],[])))])]))

        , dt_other = None
        }
}
DFA {d_id = [2]
    ,d_dt = Simple' { dt_win = []
        , dt_trans = ('x',([0,1],[(0,[(2,(#2,([(1,1),(2,0),(4,0),(5,0)],[])))]),(1,[(2,(#1,([(4,0),(5,-99)],[])))])]))

        , dt_other = None
        }
}
*Text.Regex.TDFA.Live> 
-}

{-

*Text.Regex.TDFA.Live> mapM_ putStrLn $ fullspec "((..)|(.)){2}"
((..)|(.)){2}
PConcat [PConcat [PGroup 1 (POr [PGroup 2 (PConcat [PDot {getDoPa = #1},PDot {getDoPa = #2}]),PGroup 3 (PDot {getDoPa = #3})]),PGroup 1 (POr [PGroup 2 (PConcat [PDot {getDoPa = #1},PDot {getDoPa = #2}]),PGroup 3 (PDot {getDoPa = #3})])]]
(Q { nullQ = []
  , takes = (2,Just 4)
  , preTag = Nothing
  , postTag = Nothing
  , tagged = True
  , wants = WantsQNFA
  , unQ = Seq Q { nullQ = []
            , takes = (1,Just 2)
            , preTag = Nothing
            , postTag = Just 2
            , tagged = True
            , wants = WantsQNFA
            , unQ = Or [Q { nullQ = []
                      , takes = (2,Just 2)
                      , preTag = Nothing
                      , postTag = Nothing
                      , tagged = True
                      , wants = WantsQNFA
                      , unQ = Seq Q { nullQ = []
                                , takes = (1,Just 1)
                                , preTag = Nothing
                                , postTag = Nothing
                                , tagged = False
                                , wants = WantsQNFA
                                , unQ = OneChar (PDot {getDoPa = #1})
                               } Q { nullQ = []
                                , takes = (1,Just 1)
                                , preTag = Nothing
stop 2                          , postTag = Just 3
                                , tagged = False
                                , wants = WantsQNFA
                                , unQ = OneChar (PDot {getDoPa = #2})
                               }
                     },Q { nullQ = []
                      , takes = (1,Just 1)
                      , preTag = Nothing
                      , postTag = Just 4
                      , tagged = True
                      , wants = WantsQNFA
                      , unQ = OneChar (PDot {getDoPa = #3})
                     }]
           } Q { nullQ = []
            , takes = (1,Just 2)
            , preTag = Nothing
            , postTag = Just 1
            , tagged = True
            , wants = WantsQNFA
            , unQ = Or [Q { nullQ = []
                      , takes = (2,Just 2)
                      , preTag = Nothing
                      , postTag = Nothing
                      , tagged = True
                      , wants = WantsQNFA
                      , unQ = Seq Q { nullQ = []
                                , takes = (1,Just 1)
                                , preTag = Nothing
                                , postTag = Nothing
                                , tagged = False
                                , wants = WantsQNFA
                                , unQ = OneChar (PDot {getDoPa = #1})
                               } Q { nullQ = []
                                , takes = (1,Just 1)
                                , preTag = Nothing
stop 2                          , postTag = Just 5
                                , tagged = False
                                , wants = WantsQNFA
                                , unQ = OneChar (PDot {getDoPa = #2})
                               }
                     },Q { nullQ = []
                      , takes = (1,Just 1)
                      , preTag = Nothing
                      , postTag = Just 6
                      , tagged = True
                      , wants = WantsQNFA
                      , unQ = OneChar (PDot {getDoPa = #3})
                     }]
           }
 },
array (0,6) [(0,Minimize),(1,Maximize),(2,Maximize),(3,Maximize),(4,Maximize),(5,Maximize),(6,Maximize)],
array (1,3) [(1,[GroupInfo {thisIndex = 1, parentIndex = 0, startTag = 2, stopTag = 1}
                ,GroupInfo {thisIndex = 1, parentIndex = 0, startTag = 0, stopTag = 2}])
            ,(2,[GroupInfo {thisIndex = 2, parentIndex = 1, startTag = 2, stopTag = 5}
                ,GroupInfo {thisIndex = 2, parentIndex = 1, startTag = 0, stopTag = 3}])
            ,(3,[GroupInfo {thisIndex = 3, parentIndex = 1, startTag = 2, stopTag = 6}
                ,GroupInfo {thisIndex = 3, parentIndex = 1, startTag = 0, stopTag = 4}])])
QNFA {q_id = 0
     ,q_qt = {qt_win=[(1,PreUpdate TagTask)]
, qt_trans=[]
, qt_other=[]}
}
QNFA {q_id = 1
     ,q_qt = {qt_win=[]
, qt_trans=[('\n',[])]
, qt_other=[(0,[(#2,[(5,PostUpdate TagTask),(1,PostUpdate TagTask)])])]}
}
QNFA {q_id = 2
     ,q_qt = {qt_win=[]
, qt_trans=[('\n',[])]
, qt_other=[(0,[(#3,[(6,PostUpdate TagTask),(1,PostUpdate TagTask)])]),(1,[(#1,[])])]}
}
QNFA {q_id = 3
     ,q_qt = {qt_win=[]
, qt_trans=[('\n',[])]
, qt_other=[(2,[(#2,[(3,PostUpdate TagTask),(2,PostUpdate TagTask)])])]}
}
QNFA {q_id = 4
     ,q_qt = {qt_win=[]
, qt_trans=[('\n',[])]
, qt_other=[(2,[(#3,[(4,PostUpdate TagTask),(2,PostUpdate TagTask)])]),(3,[(#1,[])])]}
}

DFA {d_id = []
    ,d_dt = Simple' { dt_win = []
        , dt_trans = 
        , dt_other = None
        }
}
DFA {d_id = [0]
    ,d_dt = Simple' { dt_win = [(0,([(1,0)],[]))]
        , dt_trans = 
        , dt_other = None
        }
}
DFA {d_id = [0,1]
    ,d_dt = Simple' { dt_win = [(0,([(1,0)],[]))]
        , dt_trans = ('\n',([],[]))

        , dt_other = ([0] , (0,[(1,(#2,([(1,1),(5,1)],[])))])
)
        }
}
DFA {d_id = [0,1,2]
    ,d_dt = Simple' { dt_win = [(0,([(1,0)],[]))]
        , dt_trans = ('\n',([],[]))

        , dt_other = ([0,1] , (0,[(1,(#2,([(1,1),(5,1)],[]))),(2,(#3,([(1,1),(6,1)],[])))])
(1,[(2,(#1,([],[])))])
)
        }
}
DFA {d_id = [2,3]
    ,d_dt = Simple' { dt_win = []
        , dt_trans = ('\n',([],[]))

        , dt_other = ([0,1,2] , (0,[(2,(#3,([(1,1),(6,1)],[])))])
(1,[(2,(#1,([],[])))])
(2,[(3,(#2,([(2,1),(3,1)],[])))])
)
        }
}
DFA {d_id = [4]
    ,d_dt = Simple' { dt_win = []
        , dt_trans = ('\n',([],[]))

        , dt_other = ([2,3] , (2,[(4,(#3,([(2,1),(4,1)],[])))])
(3,[(4,(#1,([],[])))])
)
        }
}

*Text.Regex.TDFA.Live> 

*Text.Regex.TDFA.Live> "aaa" =~  "((..)|(.)){2}" :: (String,MatchText String,String)
(""
,array (0,3) [(0,("aaa",(0,3)))
             ,(1,("a",(2,1)))
             ,(2,("aa",(0,2)))
             ,(3,("a",(2,1)))]
,"")

Note that  group 2 is not being reset properly
Checking parents:
(""
,array (0,3) [(0,("aaa",(0,3)))
             ,(1,("a",(2,1)))
             ,(2,("",(-1,0)))
             ,(3,("a",(2,1)))]
,"")

-}

{-
http://www.research.att.com/~gsf/testregex/
http://www.research.att.com/~gsf/testregex/categorize.dat

*Text.Regex.TDFA.Live> "xxxxxx" =~ "(...?.?)*" :: MatchArray
array (0,1) [(0,(0,6)),(1,(3,3))]

?E	(...?.?)*		xxxxxx	(0,6)(4,6)				REPEAT_LONGEST=first
|E	(...?.?)*		xxxxxx	(0,6)(2,6)				REPEAT_LONGEST=last
|E	(...?.?)*		xxxxxx	OK					REPEAT_LONGEST=unknown
;										REPEAT_LONGEST=bug

So what the hell is going on?  Turn on tracing:

*Text.Regex.TDFA.Live> "xxxxxx" =~ "(...?.?)*" :: MatchArray
array 
>2
Entering Orbit (2,0
               ,(fromList [(0,0)      ],fromList [])
               ,(fromList [(0,0),(2,0)],fromList [(2,fromList [0])]))
<

>1
<

>0
<

>2
Entering Orbit (2,3
               ,(fromList [(0,0),(2,0),(3,0),(5,3),(6,2),(7,3)],fromList [(2,fromList [0])])
               ,(fromList [(0,0),(2,0),(3,0),(5,3),(6,2),(7,3)],fromList [(2,fromList [0,3])]))
<

>3
<

>2
Entering Orbit (2,3
               ,(fromList [(0,0),(2,0),(3,0),(4,3),(5,2),(6,2),(8,3)],fromList [(2,fromList [0])])
               ,(fromList [(0,0),(2,0),(3,0),(4,3),(5,2),(6,2),(8,3)],fromList [(2,fromList [0,3])]))
<

>1
<

>0
<

>winning
Leaving Orbit (2,6
              ,(fromList [(0,0),(1,6),(2,0),(3,3),(5,6),(6,5),(7,6)],fromList [(2,fromList [0,3])])
              ,(fromList [(0,0),(1,6)      ,(3,3),(5,6),(6,5),(7,6)],fromList []))
<

>2
Entering Orbit (2,2
               ,(fromList [(0,0),(2,0),(3,0),(6,2)],fromList [(2,fromList [0])])
               ,(fromList [(0,0),(2,0),(3,0),(6,2)],fromList [(2,fromList [0,2])]))
<

>1
<

>2
Entering Orbit (2,4
               ,(fromList [(0,0),(2,0),(3,2),(5,2),(6,4)],fromList [(2,fromList [0,2])])
               ,(fromList [(0,0),(2,0),(3,2),(5,2),(6,4)],fromList [(2,fromList [0,2,4])]))
<

>3
<

>2
Entering Orbit (2,4
               ,(fromList [(0,0),(2,0),(3,0),(4,4),(5,3),(6,2),(7,3),(8,4)],fromList [(2,fromList [0])])
               ,(fromList [(0,0),(2,0),(3,0),(4,4),(5,3),(6,2),(7,3),(8,4)],fromList [(2,fromList [0,4])]))
<

>1
<

>winning
Leaving Orbit (2,6
              ,(fromList [(0,0),(1,6),(2,0),(3,4),(5,3),(6,6),(7,3),(8,4)],fromList [(2,fromList [0,4])])
              ,(fromList [(0,0),(1,6)      ,(3,4),(5,3),(6,6),(7,3),(8,4)],fromList []))
<

>0
<

>3
<

>3
<

>winning
Leaving Orbit (2,6
              ,(fromList [(0,0),(1,6),(2,0),(3,3),(4,6),(5,5),(6,5),(7,3),(8,6)],fromList [(2,fromList [0,3])])
              ,(fromList [(0,0),(1,6)      ,(3,3),(4,6),(5,5),(6,5),(7,3),(8,6)],fromList []))
<
(0,1) [(0,(0,6)),(1,(3,3))]
*Text.Regex.TDFA.Live> 

(...?.?)*
"(...?.?)*"

PStar (PGroup 1 (PConcat [PDot {getDoPa = #1}
                         ,PDot {getDoPa = #2}
                         ,POr [PDot {getDoPa = #3}
                              ,PEmpty]
                         ,POr [PDot {getDoPa = #4}
                              ,PEmpty]]))

(Q { nullQ = [(SetTestInfo [],[(1,PreUpdate TagTask)])]
  , takes = (0,Nothing)
  , preTag = Nothing
  , postTag = Just 1
  , tagged = True
  , wants = WantsQT
  , unQ = Star {getOrbit = Just 2, reset = [4], unStar = Q { nullQ = []
            , takes = (2,Just 4)
            , preTag = Nothing
            , postTag = Nothing
            , tagged = True
            , wants = WantsQNFA
            , unQ = Seq Q { nullQ = []
                      , takes = (2,Just 3)
                      , preTag = Nothing
                      , postTag = Nothing
                      , tagged = False
                      , wants = WantsQNFA
                      , unQ = Seq Q { nullQ = []
                                , takes = (2,Just 2)
                                , preTag = Nothing
                                , postTag = Nothing
                                , tagged = False
                                , wants = WantsQNFA
                                , unQ = Seq Q { nullQ = []
                                          , takes = (1,Just 1)
start 1                                   , preTag = Just 3
                                          , postTag = Nothing
                                          , tagged = False
                                          , wants = WantsQNFA
                                          , unQ = OneChar (PDot {getDoPa = #1})
                                         } Q { nullQ = []
                                          , takes = (1,Just 1)
                                          , preTag = Nothing
                                          , postTag = Just 6
                                          , tagged = False
                                          , wants = WantsQNFA
                                          , unQ = OneChar (PDot {getDoPa = #2})
                                         }
                               } Q { nullQ = [(SetTestInfo [],[(5,PreUpdate TagTask)])]
                                , takes = (0,Just 1)
                                , preTag = Nothing
                                , postTag = Just 5
                                , tagged = True
                                , wants = WantsQNFA
                                , unQ = Or [Q { nullQ = []
                                          , takes = (1,Just 1)
                                          , preTag = Nothing
                                          , postTag = Just 7
                                          , tagged = False
                                          , wants = WantsQNFA
                                          , unQ = OneChar (PDot {getDoPa = #3})
                                         },Q { nullQ = [(SetTestInfo [],[])]
                                          , takes = (0,Just 0)
                                          , preTag = Nothing
                                          , postTag = Nothing
                                          , tagged = False
                                          , wants = WantsEither
                                          , unQ = Empty
                                         }]
                               }
                     } Q { nullQ = [(SetTestInfo [],[(4,PreUpdate TagTask)])]
                      , takes = (0,Just 1)
                      , preTag = Nothing
stop 1                , postTag = Just 4
                      , tagged = True
                      , wants = WantsQNFA
                      , unQ = Or [Q { nullQ = []
                                , takes = (1,Just 1)
                                , preTag = Nothing
                                , postTag = Just 8
                                , tagged = False
                                , wants = WantsQNFA
                                , unQ = OneChar (PDot {getDoPa = #4})
                               },Q { nullQ = [(SetTestInfo [],[])]
                                , takes = (0,Just 0)
                                , preTag = Nothing
                                , postTag = Nothing
                                , tagged = False
                                , wants = WantsEither
                                , unQ = Empty
                               }]
                     }
           }}
 },

array (0,8) [(0,Minimize),(1,Maximize),(2,Orbit),(3,Minimize),(4,Maximize),(5,Maximize),(6,Maximize),(7,Maximize),(8,Maximize)],

array (1,1) [(1,[GroupInfo {thisIndex = 1, parentIndex = 0, startTag = 3, stopTag = 4}])])

QNFA {q_id = 0
     ,q_qt = {qt_win=[(4,PreUpdate TagTask),(2,PreUpdate LeaveOrbitTask),(1,PreUpdate TagTask),(1,PreUpdate TagTask)]
, qt_trans=[('\n',[])]
, qt_other=[(2,[(#1,[(4,PreUpdate TagTask),(4,PreUpdate ResetTask),(2,PreUpdate EnterOrbitTask),(3,PreUpdate TagTask)])])
           ,(3,[(#4,[(8,PostUpdate TagTask),(4,PostUpdate TagTask)])])]}
}
QNFA {q_id = 1
     ,q_qt = {qt_win=[(5,PreUpdate TagTask),(4,PreUpdate TagTask),(2,PreUpdate LeaveOrbitTask),(1,PreUpdate TagTask),(1,PreUpdate TagTask)]
, qt_trans=[('\n',[])]
, qt_other=[(0,[(#3,[(7,PostUpdate TagTask),(5,PostUpdate TagTask)])])
           ,(2,[(#1,[(5,PreUpdate TagTask),(4,PreUpdate TagTask),(4,PreUpdate ResetTask),(2,PreUpdate EnterOrbitTask),(3,PreUpdate TagTask)])])
           ,(3,[(#4,[(5,PreUpdate TagTask),(8,PostUpdate TagTask),(4,PostUpdate TagTask)])])]}
}
QNFA {q_id = 2
     ,q_qt = {qt_win=[]
, qt_trans=[('\n',[])]
, qt_other=[(1,[(#2,[(6,PostUpdate TagTask)])])]}
}
QNFA {q_id = 3
     ,q_qt = {qt_win=[(2,PreUpdate LeaveOrbitTask),(1,PreUpdate TagTask),(1,PreUpdate TagTask)]
, qt_trans=[('\n',[])]
, qt_other=[(2,[(#1,[(4,PreUpdate ResetTask),(2,PreUpdate EnterOrbitTask),(3,PreUpdate TagTask)])])]}
}

---------

DFA {d_id = []
    ,d_dt = Simple' { dt_win = []
        , dt_trans = 
        , dt_other = None
        }
}
DFA {d_id = [0,1,2,3]
    ,d_dt = Simple' { dt_win = [(0,([(1,0),(2,-1),(4,0)],["Leaving Orbit (2,0)"]))
                               ,(1,([(1,0),(2,-1),(4,0),(5,0)],["Leaving Orbit (2,0)"]))
                               ,(3,([(1,0),(2,-1)],["Leaving Orbit (2,0)"]))]
        , dt_other = ([0,1,2,3] ,
                      (0,[(1,(#3,([(5,1),(7,1)],[])))])
                      (1,[(2,(#2,([(6,1)],[])))])
                      (2,[(0,(#1,([(3,0),(4,-1)],["Entering Orbit (2,0)"])))
                         ,(1,(#1,([(3,0),(4,-1),(5,0)],["Entering Orbit (2,0)"])))
                         ,(3,(#1,([(3,0),(4,-1)],["Entering Orbit (2,0)"])))])
                      (3,[(0,(#4,([(4,1),(8,1)],[])))
                         ,(1,(#4,([(4,1),(5,0),(8,1)],[])))])
)
        }
}
DFA {d_id = [0,2,3]
    ,d_dt = Simple' { dt_win = [(0,([(1,0),(2,-1),(4,0)],["Leaving Orbit (2,0)"]))
                               ,(3,([(1,0),(2,-1)],["Leaving Orbit (2,0)"]))]
        , dt_other = ([1,2,3] ,
                      (1,[(2,(#2,([(6,1)],[])))])
                      (2,[(0,(#1,([(3,0),(4,-1)],["Entering Orbit (2,0)"])))
                         ,(3,(#1,([(3,0),(4,-1)],["Entering Orbit (2,0)"])))])
                      (3,[(0,(#4,([(4,1),(8,1)],[])))])
)
        }
}
DFA {d_id = [1]
    ,d_dt = Simple' { dt_win = [(1,([(1,0),(2,-1),(4,0),(5,0)],["Leaving Orbit (2,0)"]))]
        , dt_other = ([0,2,3] ,
                      (0,[(1,(#3,([(5,1),(7,1)],[])))])
                      (2,[(1,(#1,([(3,0),(4,-1),(5,0)],["Entering Orbit (2,0)"])))])
                      (3,[(1,(#4,([(4,1),(5,0),(8,1)],[])))])
)
        }
}
DFA {d_id = [1,2,3]
    ,d_dt = Simple' { dt_win = [(1,([(1,0),(2,-1),(4,0),(5,0)],["Leaving Orbit (2,0)"]))
                               ,(3,([(1,0),(2,-1)],["Leaving Orbit (2,0)"]))]
        , dt_other = ([0,1,2,3] ,
                      (0,[(1,(#3,([(5,1),(7,1)],[])))])
                      (1,[(2,(#2,([(6,1)],[])))])
                      (2,[(1,(#1,([(3,0),(4,-1),(5,0)],["Entering Orbit (2,0)"])))
                         ,(3,(#1,([(3,0),(4,-1)],["Entering Orbit (2,0)"])))])
                      (3,[(1,(#4,([(4,1),(5,0),(8,1)],[])))])
)
        }
}
DFA {d_id = [2]
    ,d_dt = Simple' { dt_win = []
        , dt_other = ([1] , (1,[(2,(#2,([(6,1)],[])))])
)
        }
}
DFA {d_id = [3]
    ,d_dt = Simple' { dt_win = [(3,([(1,0),(2,-1)],["Leaving Orbit (2,0)"]))]
        , dt_other = ([2] , (2,[(3,(#1,([(3,0),(4,-1)],["Entering Orbit (2,0)"])))])
)
        }
}

3 [3] 3/#1/2 [2] 2/#2/1 [1] 1/#3/0 [0,2,3] 0/#4/3 [1,2,3] 3/#1/2 [0,1,2,3] 2/#2/1 [0,1,2,3] 1/win

after accepting 5 characters this is at the [0,1,2,3] fixed point

winning stages:

array ___ (3,(fromList [(0,0)],fromList []))
(fromList [(0,0),(1,0)],fromList [])


@@@ (0,Just (fromList [(0,0),(1,0)],fromList []))

@@@ (1,Just (fromList [(0,0),(1,0)],fromList []))
___ (1,(fromList [(0,0),(2,0),(3,0),(6,2)],fromList [(2,fromList [0])]))
(fromList [(0,0),(1,2),(3,0),(4,2),(5,2),(6,2)],fromList [])


@@@ (2,Just (fromList [(0,0),(1,2),(3,0),(4,2),(5,2),(6,2)],fromList []))
___ (0,(fromList [(0,0),(2,0),(3,0),(5,3),(6,2),(7,3)],fromList [(2,fromList [0])]))
___ (3,(fromList [(0,0),(2,0),(3,0),(4,3),(5,2),(6,2),(8,3)],fromList [(2,fromList [0])]))
(fromList [(0,0),(1,3),(3,0),(4,3),(5,3),(6,2),(7,3)],fromList [])
(fromList [(0,0),(1,3),(3,0),(4,3),(5,2),(6,2),(8,3)],fromList [])


@@@ (3,Just (fromList [(0,0),(1,3),(3,0),(4,3),(5,3),(6,2),(7,3)],fromList []))
___ (1,(fromList [(0,0),(2,0),(3,2),(5,2),(6,4)],fromList [(2,fromList [0,2])]))
___ (3,(fromList [(0,0),(2,0),(3,0),(4,4),(5,3),(6,2),(7,3),(8,4)],fromList [(2,fromList [0])]))
(fromList [(0,0),(1,4),(3,2),(4,4),(5,4),(6,4)],fromList [])
(fromList [(0,0),(1,4),(3,0),(4,4),(5,3),(6,2),(7,3),(8,4)],fromList [])


@@@ (4,Just (fromList [(0,0),(1,4),(3,0),(4,4),(5,3),(6,2),(7,3),(8,4)],fromList []))
___ (0,(fromList [(0,0),(2,0),(3,2),(5,5),(6,4),(7,5)],fromList [(2,fromList [0,2])]))
___ (1,(fromList [(0,0),(2,0),(3,3),(5,3),(6,5),(7,3)],fromList [(2,fromList [0,3])]))
___ (3,(fromList [(0,0),(2,0),(3,2),(4,5),(5,4),(6,4),(8,5)],fromList [(2,fromList [0,2])]))
(fromList [(0,0),(1,5),(3,2),(4,5),(5,5),(6,4),(7,5)],fromList [])
(fromList [(0,0),(1,5),(3,3),(4,5),(5,5),(6,5),(7,3)],fromList [])
(fromList [(0,0),(1,5),(3,2),(4,5),(5,4),(6,4),(8,5)],fromList [])


@@@ (5,Just (fromList [(0,0),(1,5),(3,2),(4,5),(5,5),(6,4),(7,5)],fromList []))
___ (0,(fromList [(0,0),(2,0),(3,3),(5,6),(6,5),(7,6)],fromList [(2,fromList [0,3])]))
___ (1,(fromList [(0,0),(2,0),(3,4),(5,3),(6,6),(7,3),(8,4)],fromList [(2,fromList [0,4])]))
___ (3,(fromList [(0,0),(2,0),(3,3),(4,6),(5,5),(6,5),(7,3),(8,6)],fromList [(2,fromList [0,3])]))
(fromList [(0,0),(1,6),(3,3),(4,6),(5,6),(6,5),(7,6)],fromList [])
(fromList [(0,0),(1,6),(3,4),(4,6),(5,6),(6,6),(7,3),(8,4)],fromList [])
(fromList [(0,0),(1,6),(3,3),(4,6),(5,5),(6,5),(7,3),(8,6)],fromList [])


@@@ (6,Just (fromList [(0,0),(1,6),(3,3),(4,6),(5,6),(6,5),(7,6)],fromList []))
array (fromList [(0,0),(1,0)],fromList [])

array (0,8) [(0,Minimize),(1,Maximize),(2,Orbit),(3,Minimize),(4,Maximize),(5,Maximize),(6,Maximize),(7,Maximize),(8,Maximize)],
array (1,1) [(1,[GroupInfo {thisIndex = 1, parentIndex = 0, startTag = 3, stopTag = 4}])])

Shit -- how did it win without setting tag 4?  QNFA transitions and tag 4

3 -> win <>
3 -> 2   Reset        #1

2 -> 1   <>           #2

1 -> win PreUpdate
1 -> 2   Reset        #1
1 -> 0   <>           #3
1 -> 3   PostUpdate   #4

0 -> win PreUpdate
0 -> 2   Reset        #1
0 -> 3   PostUpdate   #4

Against "" it goes 3->win with blank tag 4
Winning in 1 or 0 sets tag 4
Going 1 or 0 to 3 sets tag 4

You cannot win with tag 4 unset unless you are against ""

[3] -> win <>
[3] -> 3/#1/2 -> [2] Reset
[2] -> 2/#2/1 -> [1] <>
[1] -> 1/win             PreUpdate
[1] -> 1/#1/2 -> [0,2,3] Reset
[1] -> 1/#3/0 -> [0,2,3] <>
[1] -> 1/#4/3 -> [0,2,3] PostUpdate
[0,2,3] -> 0/win             PreUpdate
[0,2,3] -> 3/win             <>
[0,2,3] -> 0/#1/2 -> [1,2,3] Reset
[0,2,3] -> 3/#1/2 -> [1,2,3] Reset
[0,2,3] -> 2/#2/1 -> [1,2,3] <>
[0,2,3] -> 0/#4/3 -> [1,2,3] PostUpdate
[1,2,3] -> 1/win               PreUpdate
[1,2,3] -> 3/win               <>
[1,2,3] -> 1/#1/2 -> [0,1,2,3] Reset
[1,2,3] -> 3/#1/2 -> [0,1,2,3] Reset
[1,2,3] -> 2/#2/1 -> [0,1,2,3] <>
[1,2,3] -> 1/#3/0 -> [0,1,2,3] <>
[1,2,3] -> 1/#4/3 -> [0,1,2,3] PostUpdate
[0,1,2,3] -> 0/win               PreUpdate
[0,1,2,3] -> 1/win               PreUpdate
[0,1,2,3] -> 3/win               <>
[0,1,2,3] -> 0/#1/2 -> [0,1,2,3] Reset
[0,1,2,3] -> 1/#1/2 -> [0,1,2,3] Reset
[0,1,2,3] -> 3/#1/2 -> [0,1,2,3] Reset
[0,1,2,3] -> 2/#2/1 -> [0,1,2,3] <>
[0,1,2,3] -> 1/#3/0 -> [0,1,2,3] <>
[0,1,2,3] -> 0/#4/3 -> [0,1,2,3] PostUpdate
[0,1,2,3] -> 1/#4/3 -> [0,1,2,3] PostUpdate

-}
