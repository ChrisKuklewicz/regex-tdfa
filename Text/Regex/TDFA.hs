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
getVersion = Version { versionBranch = [0,45]
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

{-
*Text.Regex.TDFA> "aaaaaa" =~ "(a*)+" :: MatchArray

array *** Exception: makeTagComparer.comp: non-identical orbit pos :
array (0,5) [(0,Minimize),(1,Maximize),(2,Maximize),(3,Orbit),(4,Minimize),(5,Maximize)]
(fromList [(0,0),(3,0),(4,1)],fromList [(3,fromList [0,1])])
(fromList [(0,0),(3,1),(4,1)],fromList [(3,fromList [1])])

*Text.Regex.TDFA.Live> putStr . unlines . fullspec $ "(a*)+"

(a*)+

"(a*)+"

PConcat [PConcat [PGroup 1 (PStar True (PChar {getDoPa = #1, getPatternChar = 'a'}))
                 ,PStar False (PGroup 1 (PStar True (PChar {getDoPa = #1, getPatternChar = 'a'})))]]

(Q { nullQ = [(SetTestInfo [],[(2,PreUpdate TagTask),(1,PreUpdate TagTask)])]
  , takes = (0,Nothing)
  , preTag = Nothing
  , postTag = Nothing
  , tagged = True
  , wants = WantsQT
  , unQ = Seq Q { nullQ = [(SetTestInfo [],[(2,PreUpdate TagTask)])]
            , takes = (0,Nothing)
            , preTag = Nothing
stop 1      , postTag = Just 2
            , tagged = True
            , wants = WantsQT
            , unQ = Star {getOrbit = Nothing, reset = [], firstNull = True, unStar = Q { nullQ = []
                      , takes = (1,Just 1)
                      , preTag = Nothing
                      , postTag = Nothing
                      , tagged = False
                      , wants = WantsQNFA
                      , unQ = OneChar (PChar {getDoPa = #1, getPatternChar = 'a'})
                     }}
           } Q { nullQ = [(SetTestInfo [],[(1,PreUpdate TagTask)])]
            , takes = (0,Nothing)
            , preTag = Nothing
            , postTag = Just 1
            , tagged = True
            , wants = WantsQT
            , unQ = Star {getOrbit = Just 3, reset = [2,5], firstNull = False, unStar = Q { nullQ = [(SetTestInfo [],[(5,PreUpdate TagTask),(4,PreUpdate TagTask)])]
                      , takes = (0,Nothing)
start 1               , preTag = Just 4
stop 1                , postTag = Just 5
                      , tagged = True
                      , wants = WantsQT
                      , unQ = Star {getOrbit = Nothing, reset = [], firstNull = True, unStar = Q { nullQ = []
                                , takes = (1,Just 1)
                                , preTag = Nothing
                                , postTag = Nothing
                                , tagged = False
                                , wants = WantsQNFA
                                , unQ = OneChar (PChar {getDoPa = #1, getPatternChar = 'a'})
                               }}
                     }}
           }
 },

array (0,5) [(0,Minimize),(1,Maximize),(2,Maximize),(3,Orbit),(4,Minimize),(5,Maximize)],

array (1,1) [(1,[GroupInfo {thisIndex = 1, parentIndex = 0, startTag = 4, stopTag = 5}
                ,GroupInfo {thisIndex = 1, parentIndex = 0, startTag = 0, stopTag = 2}])])

QNFA {q_id = 0
     ,q_qt = {qt_win=[(5,PreUpdate TagTask),(3,PreUpdate LeaveOrbitTask),(1,PreUpdate TagTask),(1,PreUpdate TagTask)]
, qt_trans=[('a',[(0,[(#1,[])
                     ,(#1,[(5,PreUpdate TagTask),(2,PreUpdate ResetTask),(5,PreUpdate ResetTask),(3,PreUpdate EnterOrbitTask),(4,PreUpdate TagTask)])])])]
, qt_other=[]}
}
QNFA {q_id = 1
     ,q_qt = {qt_win=[(2,PreUpdate TagTask),(3,PreUpdate LeaveOrbitTask),(1,PreUpdate TagTask),(1,PreUpdate TagTask)]
, qt_trans=[('a',[(0,[(#1,[(2,PreUpdate TagTask),(2,PreUpdate ResetTask),(5,PreUpdate ResetTask),(3,PreUpdate EnterOrbitTask),(4,PreUpdate TagTask)])])
                 ,(1,[(#1,[])])])]
, qt_other=[]}
}

----

DFA {d_id = [0,1]
    ,d_dt = Simple' { dt_win = [(0,([(1,0),(5,0)],["Leaving Orbit (3,0)"]))
                               ,(1,([(1,0),(2,0)],["Leaving Orbit (3,0)"]))]
        , dt_trans = ('a',([0,1],[(0,[(0,(#1,([(2,-1),(4,0),(5,-1)],["Entering Orbit (3,0)"])))
                                     ,(1,(#1,([(2,-1),(4,0),(5,-1)],["Entering Orbit (3,0)"])))])
                                 ,(1,[(1,(#1,([],[])))])]))

        , dt_other = None
        }
}
DFA {d_id = [1]
    ,d_dt = Simple' { dt_win = [(1,([(1,0),(2,0)],["Leaving Orbit (3,0)"]))]
        , dt_trans = ('a',([0,1],[(0,[(1,(#1,([(2,-1),(4,0),(5,-1)],["Entering Orbit (3,0)"])))])
                                 ,(1,[(1,(#1,([],[])))])]))

        , dt_other = None
        }
}

*Text.Regex.TDFA.Live> 

*Text.Regex.TDFA.Live> "aaaaaa" =~  "(a*)+" :: MatchArray
array 
@@@ (0
    ,fromList [(1
               ,(fromList [(0,0)]
                ,fromList []))]
    ,Just (fromList [(0,0),(1,0),(2,0)]
          ,fromList []))

@@@ (1
    ,fromList [(0,(fromList [(0,0),(3,0),(4,0)]
                  ,fromList [(3
                             ,fromList [0])]))
              ,(1,(fromList [(0,0)]
                  ,fromList []))]
    ,Just (fromList [(0,0),(1,1),(2,1)]
          ,fromList []))

*** Exception: makeTagComparer.comp: non-identical orbit pos :
array (0,5) [(0,Minimize),(1,Maximize),(2,Maximize),(3,Orbit),(4,Minimize),(5,Maximize)]
(fromList [(0,0),(3,0),(4,1)],fromList [(3,fromList [0,1])])
(fromList [(0,0),(3,1),(4,1)],fromList [(3,fromList [1])])

*Text.Regex.TDFA.Live> 

With some fixing it looks much more like "(a*)(a*)*" case:

DFA {d_id = [0,1]
    ,d_dt = Simple' { dt_win = [(0,([(1,0),(5,0)],["Leaving Orbit (3,0)"]))
                               ,(1,([(1,0),(2,0)],["Leaving Orbit (3,0)"]))]
        , dt_trans = ('a',([0,1],[(0,[(0,(#1,([],[])))
                                     ,(1,(#1,([(2,-1),(4,0),(5,-1)],["Entering Orbit (3,0)"])))])
                                 ,(1,[(1,(#1,([],[])))])]))

        , dt_other = None
        }
}
DFA {d_id = [1]
    ,d_dt = Simple' { dt_win = [(1,([(1,0),(2,0)],["Leaving Orbit (3,0)"]))]
        , dt_trans = ('a',([0,1],[(0,[(1,(#1,([(2,-1),(4,0),(5,-1)],["Entering Orbit (3,0)"])))])
                                 ,(1,[(1,(#1,([],[])))])]))

        , dt_other = None
        }
}


-}


{- from quick check

*Main> testit 100
("cAc","(((.|()))+|.?)().A",("","cA","c",["","","","",""]),"same")
*** Exception: Text.Regex.TDFA.TNFA.bestTrans.choose the EQ case has different ignore b1 and b2:

([(5,PostUpdate TagTask),(6,PostUpdate TagTask)]
,[(5,PreUpdate ResetTask),(7,PreUpdate ResetTask),(8,PreUpdate EnterOrbitTask),(9,PreUpdate TagTask),(10,PostUpdate TagTask),(11,PostUpdate TagTask),(12,PreUpdate ResetTask)]) :

fromList [(#1,[(6,PostUpdate TagTask),(5,PostUpdate TagTask)])
         ,(#1,[(7,PreUpdate TagTask),(5,PreUpdate TagTask),(5,PreUpdate ResetTask),(7,PreUpdate ResetTask),(10,PreUpdate ResetTask),(12,PreUpdate ResetTask),(8,PreUpdate EnterOrbitTask),(9,PreUpdate TagTask),(11,PostUpdate TagTask),(10,PostUpdate TagTask)])]

("AAAAAAAAAcccc___","((c)|(()|c))+(c)()(())*",("AAAAAAAAA","cccc","___",["","c","","","c","","",""]),("AAAAAAAAA","cccc","___",["","","","","c","","",""]))
*** Exception: Text.Regex.TDFA.TNFA.bestTrans.choose the EQ case has different ignore b1 and b2:
([(3,PostUpdate TagTask),(4,PostUpdate TagTask)]
,[(3,PreUpdate ResetTask),(4,PreUpdate ResetTask),(5,PreUpdate ResetTask),(6,PreUpdate ResetTask),(7,PreUpdate EnterOrbitTask),(8,PreUpdate TagTask),(9,PostUpdate TagTask),(10,PostUpdate TagTask),(11,PreUpdate ResetTask),(12,PreUpdate ResetTask)]) 
: 
fromList [(#1,[(4,PostUpdate TagTask),(3,PostUpdate TagTask)])
         ,(#1,[(6,PreUpdate TagTask),(5,PreUpdate TagTask),(3,PreUpdate TagTask),(3,PreUpdate ResetTask),(4,PreUpdate ResetTask),(5,PreUpdate ResetTask),(6,PreUpdate ResetTask),(9,PreUpdate ResetTask),(10,PreUpdate ResetTask),(11,PreUpdate ResetTask),(12,PreUpdate ResetTask),(7,PreUpdate EnterOrbitTask),(8,PreUpdate TagTask),(10,PostUpdate TagTask),(9,PostUpdate TagTask)])
         ,(#2,[(5,PostUpdate TagTask),(3,PostUpdate TagTask)])
         ,(#2,[(6,PreUpdate TagTask),(5,PreUpdate TagTask),(3,PreUpdate TagTask),(3,PreUpdate ResetTask),(4,PreUpdate ResetTask),(5,PreUpdate ResetTask),(6,PreUpdate ResetTask),(9,PreUpdate ResetTask),(10,PreUpdate ResetTask),(11,PreUpdate ResetTask),(12,PreUpdate ResetTask),(7,PreUpdate EnterOrbitTask),(8,PreUpdate TagTask),(11,PostUpdate TagTask),(9,PostUpdate TagTask)])]

("aaaaaBACbaCaCaCa","((.)*)(A|.?)|(()).",("","aaaaaBACbaCaCaCa","",["aaaaaBACbaCaCaCa","a","","",""]),"same")
*** Exception: Should not get here: Text.Regex.TNFA.ignoreCommand PreUpdate LeaveOrbitTask

("cAAAAB_\n","..|((.|c)?)*|B()",("","cA","AAAB_\n",["","",""]),("","cAAAAB_","\n",["","",""]))
*** Exception: Should not get here: Text.Regex.TNFA.ignoreCommand PreUpdate EnterOrbitTask

("aaaCaC\n","(\n)?.|()|(.+)|()(.|C|C)+|(a)*(()).?",("","a","aaCaC\n",["","","","","","","",""]),("","aaaCaC","\n",["","","aaaCaC","","","","",""]))
*** Exception: Should not get here: Text.Regex.TNFA.ignoreCommand PreUpdate EnterOrbitTask

("AAAAAA","(.|.+|A)+(())",("","AAAAAA","",["A","",""]),("","AAAAAA","",["AAAAAA","",""]))
*** Exception: Should not get here: Text.Regex.TNFA.ignoreCommand PreUpdate EnterOrbitTask

("aaaaaaAAAA","()|(a)*(.)*..",("","","aaaaaaAAAA",["","",""]),("","aaaaaaAAAA","",["","a","A"]))
"FAIL FAIL FAIL"
"aaaaaaAAAA"
"()|(a)*(.)*.."
("PCRE   ",("","","aaaaaaAAAA",["","",""]))
("Parsec ",("","","aaaaaaAAAA",["","",""]))
("Posix  ",("","aaaaaaAAAA","",["","a","A"]))
("TRE    ",("","aaaaaaAAAA","",["","","A"]))
("TDFA   ",("","aaaaaaAAAA","",["","a","A"]))
("Old    ",("","aaaaaaAAAA","",["","a","A"]))

("cccc_B_a___","(.)*(a|_|())*_|.|(B)+",("","cccc_B_a___","",["_","","",""]),"same")
"FAIL FAIL FAIL"
"cccc_B_a___"
"(.)*(a|_|())*_|.|(B)+"
("PCRE   ",("","cccc_B_a___","",["_","","",""]))
("Parsec ",("","cccc_B_a___","",["_","","",""]))
("Posix  ",("","cccc_B_a___","",["_","","",""]))
("TRE    ",("","cccc_B_a___","",["B","_","",""]))
("TDFA   ",("","cccc_B_a___","",["_","","",""]))
("Old    ",("","cccc_B_a___","",["_","","",""]))

("aaaa\n",".+.?(((.))+|(a|())+)|\n",("","aaaa","\n",["","","","",""]),"same")
*** Exception: Text.Regex.TDFA.TNFA.bestTrans.choose the EQ case has different ignore b1 and b2:
([(12,PostUpdate TagTask),(13,PostUpdate TagTask)]
,[(12,PreUpdate ResetTask),(14,PreUpdate ResetTask),(15,PreUpdate EnterOrbitTask),(16,PreUpdate TagTask),(17,PostUpdate TagTask),(18,PostUpdate TagTask),(19,PreUpdate ResetTask)])
 : 
fromList [(#4,[(4,PreUpdate TagTask),(3,PreUpdate TagTask),(13,PostUpdate TagTask),(12,PostUpdate TagTask)])
         ,(#4,[(4,PreUpdate TagTask),(3,PreUpdate TagTask),(14,PreUpdate TagTask),(12,PreUpdate TagTask),(12,PreUpdate ResetTask),(14,PreUpdate ResetTask),(17,PreUpdate ResetTask),(19,PreUpdate ResetTask),(15,PreUpdate EnterOrbitTask),(16,PreUpdate TagTask),(18,PostUpdate TagTask),(17,PostUpdate TagTask)])]

("\n","()(((\n)|()|\n|(\n)*)*)*()()",("","\n","",["","\n","","\n","","","",""]),("","\n","",["","","","","","","",""]))
"FAIL FAIL FAIL"
"\n"
"()(((\n)|()|\n|(\n)*)*)*()()"
("PCRE   ",("","\n","",["","\n","","\n","","","",""]))
("Parsec ",("","\n","",["","","","\n","","","",""]))
("Posix  ",("","\n","",["","","","","","","",""]))
("TRE    ",("","\n","",["","\n","\n","\n","","","",""]))
("TDFA   ",*** Exception: Should not get here: Text.Regex.TNFA.ignoreCommand PreUpdate EnterOrbitTask

("aaaaa\n\naaaaaa","(a)*(.|()|())|()",("","aaaaa","\n\naaaaaa",["a","","","",""]),"same")
"FAIL FAIL FAIL"
"aaaaa\n\naaaaaa"
"(a)*(.|()|())|()"
("PCRE   ",("","aaaaa","\n\naaaaaa",["a","","","",""]))
("Parsec ",("","aaaaa","\n\naaaaaa",["a","","","",""]))
("Posix  ",("","aaaaa","\n\naaaaaa",["a","","","",""]))
("TRE    ",("","aaaaa","\n\naaaaaa",["a","a","","",""]))
("TDFA   ",("","aaaaa","\n\naaaaaa",["a","","","",""]))
("Old    ",("","aaaaa","\n\naaaaaa",["a","","","",""]))

("\nbCbCbCCCCAaAaA","(\n|.*|(.+)|()|.|A|()|.)+|.|()|(())\n.",("","\nbCbCbCCCCAaAaA","",["","","","","","",""]),"same")
"FAIL FAIL FAIL"
"\nbCbCbCCCCAaAaA"
"(\n|.*|(.+)|()|.|A|()|.)+|.|()|(())\n."
("PCRE   ",("","\nbCbCbCCCCAaAaA","",["","","","","","",""]))
("Parsec ",("","\nbCbCbCCCCAaAaA","",["","","","","","",""]))
("Posix  ",("","\nbCbCbCCCCAaAaA","",["","","","","","",""]))
("TRE    ",("","\nbCbCbCCCCAaAaA","",["bCbCbCCCCAaAaA","","","","","",""]))
("TDFA   ",*** Exception: Text.Regex.TDFA.TNFA.bestTrans.choose the EQ case has different ignore b1 and b2:
([(5,PostUpdate TagTask),(6,PostUpdate TagTask)]
,[(5,PreUpdate ResetTask),(7,PreUpdate TagTask),(8,PreUpdate ResetTask),(9,PreUpdate ResetTask),(12,PreUpdate ResetTask),(14,PreUpdate EnterOrbitTask),(15,PreUpdate TagTask),(16,PostUpdate TagTask),(17,PostUpdate TagTask),(19,PreUpdate ResetTask),(20,PreUpdate ResetTask),(23,PreUpdate ResetTask)])
 : 
fromList [(#1,[(6,PostUpdate TagTask),(5,PostUpdate TagTask)])
         ,(#1,[(7,PreUpdate TagTask),(5,PreUpdate TagTask),(5,PreUpdate ResetTask),(8,PreUpdate ResetTask),(9,PreUpdate ResetTask),(12,PreUpdate ResetTask),(16,PreUpdate ResetTask),(19,PreUpdate ResetTask),(20,PreUpdate ResetTask),(23,PreUpdate ResetTask),(14,PreUpdate EnterOrbitTask),(15,PreUpdate TagTask),(17,PostUpdate TagTask),(16,PostUpdate TagTask)])
         ,(#1,[(9,PreUpdate TagTask),(5,PreUpdate TagTask),(5,PreUpdate ResetTask),(8,PreUpdate ResetTask),(9,PreUpdate ResetTask),(12,PreUpdate ResetTask),(16,PreUpdate ResetTask),(19,PreUpdate ResetTask),(20,PreUpdate ResetTask),(23,PreUpdate ResetTask),(14,PreUpdate EnterOrbitTask),(15,PreUpdate TagTask),(17,PostUpdate TagTask),(16,PostUpdate TagTask)])
         ,(#1,[(12,PreUpdate TagTask),(5,PreUpdate TagTask),(5,PreUpdate ResetTask),(8,PreUpdate ResetTask),(9,PreUpdate ResetTask),(12,PreUpdate ResetTask),(16,PreUpdate ResetTask),(19,PreUpdate ResetTask),(20,PreUpdate ResetTask),(23,PreUpdate ResetTask),(14,PreUpdate EnterOrbitTask),(15,PreUpdate TagTask),(17,PostUpdate TagTask),(16,PostUpdate TagTask)])]

("bbbbbaa_c_c_c_","()()|.|(())|(())(_)|c|.*|(.)*|a|(.|()|.)*|c",("","","bbbbbaa_c_c_c_",["","","","","","","","","",""]),("","bbbbbaa_c_c_c_","",["","","","","","","","","",""]))
*** Exception: Should not get here: Text.Regex.TNFA.ignoreCommand PreUpdate EnterOrbitTask

("baaC","(.|(.|(.?)+|())|()|(()))+(())?.",("","baaC","",["","","","","","","","",""]),"same")
"FAIL FAIL FAIL"
"baaC"
"(.|(.|(.?)+|())|()|(()))+(())?."
("PCRE   ",("","baaC","",["","","","","","","","",""]))
("Parsec ",("","baaC","",["","","","","","","","",""]))
("Posix  ",("","baaC","",["","","","","","","","",""]))
("TRE    ",("","baaC","",["baa","baa","a","","","","","",""]))
("TDFA   ",*** Exception: Text.Regex.TDFA.TNFA.bestTrans.choose the EQ case has different ignore b1 and b2:
([(3,PostUpdate TagTask),(4,PostUpdate TagTask)]
,[(3,PreUpdate ResetTask),(5,PreUpdate ResetTask),(6,PreUpdate ResetTask),(9,PreUpdate ResetTask),(13,PreUpdate ResetTask),(15,PreUpdate ResetTask),(16,PreUpdate ResetTask),(17,PreUpdate EnterOrbitTask),(18,PreUpdate TagTask),(19,PostUpdate TagTask),(20,PostUpdate TagTask),(21,PreUpdate ResetTask),(22,PreUpdate ResetTask),(25,PreUpdate ResetTask),(27,PreUpdate ResetTask),(29,PreUpdate ResetTask),(31,PreUpdate ResetTask),(32,PreUpdate ResetTask)])
 : 
fromList [(#1,[(4,PostUpdate TagTask),(3,PostUpdate TagTask)])
         ,(#1,[(6,PreUpdate TagTask),(3,PreUpdate TagTask),(3,PreUpdate ResetTask),(5,PreUpdate ResetTask),(6,PreUpdate ResetTask),(9,PreUpdate ResetTask),(13,PreUpdate ResetTask),(15,PreUpdate ResetTask),(16,PreUpdate ResetTask),(19,PreUpdate ResetTask),(21,PreUpdate ResetTask),(22,PreUpdate ResetTask),(25,PreUpdate ResetTask),(27,PreUpdate ResetTask),(29,PreUpdate ResetTask),(31,PreUpdate ResetTask),(32,PreUpdate ResetTask),(17,PreUpdate EnterOrbitTask),(18,PreUpdate TagTask),(20,PostUpdate TagTask),(19,PostUpdate TagTask)])
         ,(#1,[(9,PreUpdate TagTask),(11,PreUpdate LeaveOrbitTask),(8,PreUpdate TagTask),(5,PreUpdate TagTask),(3,PreUpdate TagTask),(3,PreUpdate ResetTask),(5,PreUpdate ResetTask),(6,PreUpdate ResetTask),(9,PreUpdate ResetTask),(13,PreUpdate ResetTask),(15,PreUpdate ResetTask),(16,PreUpdate ResetTask),(19,PreUpdate ResetTask),(21,PreUpdate ResetTask),(22,PreUpdate ResetTask),(25,PreUpdate ResetTask),(27,PreUpdate ResetTask),(29,PreUpdate ResetTask),(31,PreUpdate ResetTask),(32,PreUpdate ResetTask),(17,PreUpdate EnterOrbitTask),(18,PreUpdate TagTask),(20,PostUpdate TagTask),(19,PostUpdate TagTask)])
         ,(#1,[(15,PreUpdate TagTask),(5,PreUpdate TagTask),(3,PreUpdate TagTask),(3,PreUpdate ResetTask),(5,PreUpdate ResetTask),(6,PreUpdate ResetTask),(9,PreUpdate ResetTask),(13,PreUpdate ResetTask),(15,PreUpdate ResetTask),(16,PreUpdate ResetTask),(19,PreUpdate ResetTask),(21,PreUpdate ResetTask),(22,PreUpdate ResetTask),(25,PreUpdate ResetTask),(27,PreUpdate ResetTask),(29,PreUpdate ResetTask),(31,PreUpdate ResetTask),(32,PreUpdate ResetTask),(17,PreUpdate EnterOrbitTask),(18,PreUpdate TagTask),(20,PostUpdate TagTask),(19,PostUpdate TagTask)])
         ,(#1,[(16,PreUpdate TagTask),(3,PreUpdate TagTask),(3,PreUpdate ResetTask),(5,PreUpdate ResetTask),(6,PreUpdate ResetTask),(9,PreUpdate ResetTask),(13,PreUpdate ResetTask),(15,PreUpdate ResetTask),(16,PreUpdate ResetTask),(19,PreUpdate ResetTask),(21,PreUpdate ResetTask),(22,PreUpdate ResetTask),(25,PreUpdate ResetTask),(27,PreUpdate ResetTask),(29,PreUpdate ResetTask),(31,PreUpdate ResetTask),(32,PreUpdate ResetTask),(17,PreUpdate EnterOrbitTask),(18,PreUpdate TagTask),(20,PostUpdate TagTask),(19,PostUpdate TagTask)])
         ,(#2,[(6,PreUpdate TagTask),(3,PreUpdate TagTask),(3,PreUpdate ResetTask),(5,PreUpdate ResetTask),(6,PreUpdate ResetTask),(9,PreUpdate ResetTask),(13,PreUpdate ResetTask),(15,PreUpdate ResetTask),(16,PreUpdate ResetTask),(19,PreUpdate ResetTask),(21,PreUpdate ResetTask),(22,PreUpdate ResetTask),(25,PreUpdate ResetTask),(27,PreUpdate ResetTask),(29,PreUpdate ResetTask),(31,PreUpdate ResetTask),(32,PreUpdate ResetTask),(17,PreUpdate EnterOrbitTask),(18,PreUpdate TagTask),(23,PostUpdate TagTask),(21,PostUpdate TagTask),(19,PostUpdate TagTask)])
         ,(#2,[(7,PostUpdate TagTask),(5,PostUpdate TagTask),(3,PostUpdate TagTask)])
         ,(#2,[(9,PreUpdate TagTask),(11,PreUpdate LeaveOrbitTask),(8,PreUpdate TagTask),(5,PreUpdate TagTask),(3,PreUpdate TagTask),(3,PreUpdate ResetTask),(5,PreUpdate ResetTask),(6,PreUpdate ResetTask),(9,PreUpdate ResetTask),(13,PreUpdate ResetTask),(15,PreUpdate ResetTask),(16,PreUpdate ResetTask),(19,PreUpdate ResetTask),(21,PreUpdate ResetTask),(22,PreUpdate ResetTask),(25,PreUpdate ResetTask),(27,PreUpdate ResetTask),(29,PreUpdate ResetTask),(31,PreUpdate ResetTask),(32,PreUpdate ResetTask),(17,PreUpdate EnterOrbitTask),(18,PreUpdate TagTask),(23,PostUpdate TagTask),(21,PostUpdate TagTask),(19,PostUpdate TagTask)])
         ,(#2,[(15,PreUpdate TagTask),(5,PreUpdate TagTask),(3,PreUpdate TagTask),(3,PreUpdate ResetTask),(5,PreUpdate ResetTask),(6,PreUpdate ResetTask),(9,PreUpdate ResetTask),(13,PreUpdate ResetTask),(15,PreUpdate ResetTask),(16,PreUpdate ResetTask),(19,PreUpdate ResetTask),(21,PreUpdate ResetTask),(22,PreUpdate ResetTask),(25,PreUpdate ResetTask),(27,PreUpdate ResetTask),(29,PreUpdate ResetTask),(31,PreUpdate ResetTask),(32,PreUpdate ResetTask),(17,PreUpdate EnterOrbitTask),(18,PreUpdate TagTask),(23,PostUpdate TagTask),(21,PostUpdate TagTask),(19,PostUpdate TagTask)])
         ,(#2,[(16,PreUpdate TagTask),(3,PreUpdate TagTask),(3,PreUpdate ResetTask),(5,PreUpdate ResetTask),(6,PreUpdate ResetTask),(9,PreUpdate ResetTask),(13,PreUpdate ResetTask),(15,PreUpdate ResetTask),(16,PreUpdate ResetTask),(19,PreUpdate ResetTask),(21,PreUpdate ResetTask),(22,PreUpdate ResetTask),(25,PreUpdate ResetTask),(27,PreUpdate ResetTask),(29,PreUpdate ResetTask),(31,PreUpdate ResetTask),(32,PreUpdate ResetTask),(17,PreUpdate EnterOrbitTask),(18,PreUpdate TagTask),(23,PostUpdate TagTask),(21,PostUpdate TagTask),(19,PostUpdate TagTask)])]

("bababacBcCcbAbA",".(()|())|(.?)A.|.+|.|(C|.|((a)|(.)+)|())*(B)",("","b","ababacBcCcbAbA",["","","","","","","","","",""]),("","bababacBcCcbAbA","",["","","","","","","","","",""]))
*** Exception: Should not get here: Text.Regex.TNFA.ignoreCommand PreUpdate EnterOrbitTask


now smaller


("ccc","(c|.)+",("","ccc","",["c"]),"same")
*** Exception: Should not get here: Text.Regex.TNFA.ignoreCommand PreUpdate EnterOrbitTask

("AbAAA","(((())|.)+)A|()",("","A","bAAA",["","","","",""]),("","AbAAA","",["AbAA","","","",""]))
"FAIL FAIL FAIL"
"AbAAA"
"(((())|.)+)A|()"
("PCRE   ",("","A","bAAA",["","","","",""]))
("Parsec ",("","A","bAAA",["","","","",""]))
("Posix  ",("","AbAAA","",["AbAA","","","",""]))
("TRE    ",("","AbAAA","",["AbAA","A","","",""]))
("TDFA   ",*** Exception: Text.Regex.TDFA.TNFA.bestTrans.choose the EQ case has different ignore b1 and b2:
([(4,PostUpdate TagTask)]
,[(4,PreUpdate ResetTask),(5,PreUpdate ResetTask),(6,PreUpdate EnterOrbitTask),(7,PreUpdate TagTask),(8,PostUpdate TagTask),(9,PreUpdate ResetTask)])
 : 
fromList [(#1,[(4,PostUpdate TagTask)])
         ,(#1,[(5,PreUpdate TagTask),(4,PreUpdate TagTask),(4,PreUpdate ResetTask),(5,PreUpdate ResetTask),(8,PreUpdate ResetTask),(9,PreUpdate ResetTask),(6,PreUpdate EnterOrbitTask),(7,PreUpdate TagTask),(8,PostUpdate TagTask)])]


("BABccc\n","B|B|B|.|()|()|A|\n|()|(())|()|((B|(()))?)+|\n|.|(.+)|\n|\n()",("","B","ABccc\n",["","","","","","","","","","","",""]),("","BABccc","\n",["","","","","","","","","","","BABccc",""]))
*** Exception: Text.Regex.TDFA.TNFA.bestTrans.choose the EQ case has different ignore b1 and b2: ([(18,PreUpdate ResetTask),(19,PreUpdate ResetTask),(21,PreUpdate ResetTask),(22,PreUpdate EnterOrbitTask),(23,PreUpdate TagTask),(24,PostUpdate TagTask),(25,PostUpdate TagTask),(26,PostUpdate TagTask),(27,PreUpdate ResetTask)],[(18,PostUpdate TagTask),(19,PostUpdate TagTask),(20,PostUpdate TagTask)]) : fromList [(#7,[(18,PreUpdate TagTask),(18,PreUpdate ResetTask),(19,PreUpdate ResetTask),(21,PreUpdate ResetTask),(24,PreUpdate ResetTask),(25,PreUpdate ResetTask),(27,PreUpdate ResetTask),(22,PreUpdate EnterOrbitTask),(23,PreUpdate TagTask),(26,PostUpdate TagTask),(25,PostUpdate TagTask),(24,PostUpdate TagTask)]),(#7,[(20,PostUpdate TagTask),(19,PostUpdate TagTask),(18,PostUpdate TagTask)]),(#7,[(21,PreUpdate TagTask),(19,PreUpdate TagTask),(18,PreUpdate TagTask),(18,PreUpdate ResetTask),(19,PreUpdate ResetTask),(21,PreUpdate ResetTask),(24,PreUpdate ResetTask),(25,PreUpdate ResetTask),(27,PreUpdate ResetTask),(22,PreUpdate EnterOrbitTask),(23,PreUpdate TagTask),(26,PostUpdate TagTask),(25,PostUpdate TagTask),(24,PostUpdate TagTask)])]

("_bbac","()|((())|())|.|((())|(())|.)+|()|.+|()((()|b)?)",("","","_bbac",["","","","","","","","","","","","","","",""]),("","_bbac","",["","","","","","","","","","","","","","",""]))
"FAIL FAIL FAIL"
"_bbac"
"()|((())|())|.|((())|(())|.)+|()|.+|()((()|b)?)"
("PCRE   ",("","","_bbac",["","","","","","","","","","","","","","",""]))
("Parsec ",("","","_bbac",["","","","","","","","","","","","","","",""]))
("Posix  ",("","_bbac","",["","","","","","","","","","","","","","",""]))
("TRE    ",("","_bbac","",["","","","","","c","","","","","","","","",""]))
("TDFA   ",*** Exception: Text.Regex.TDFA.TNFA.bestTrans.choose the EQ case has different ignore b1 and b2: ([(10,PostUpdate TagTask)],[(10,PreUpdate ResetTask),(11,PreUpdate ResetTask),(12,PreUpdate ResetTask),(13,PreUpdate EnterOrbitTask),(14,PreUpdate TagTask),(15,PostUpdate TagTask),(16,PreUpdate ResetTask),(17,PreUpdate ResetTask)]) : fromList [(#2,[(10,PostUpdate TagTask)]),(#2,[(11,PreUpdate TagTask),(10,PreUpdate TagTask),(10,PreUpdate ResetTask),(11,PreUpdate ResetTask),(12,PreUpdate ResetTask),(15,PreUpdate ResetTask),(16,PreUpdate ResetTask),(17,PreUpdate ResetTask),(13,PreUpdate EnterOrbitTask),(14,PreUpdate TagTask),(15,PostUpdate TagTask)]),(#2,[(12,PreUpdate TagTask),(10,PreUpdate TagTask),(10,PreUpdate ResetTask),(11,PreUpdate ResetTask),(12,PreUpdate ResetTask),(15,PreUpdate ResetTask),(16,PreUpdate ResetTask),(17,PreUpdate ResetTask),(13,PreUpdate EnterOrbitTask),(14,PreUpdate TagTask),(15,PostUpdate TagTask)])]

("___","(.|())+|((()))()|()(.)",("","___","",["","","","","","","",""]),"same")
"FAIL FAIL FAIL"
"___"
"(.|())+|((()))()|()(.)"
("PCRE   ",("","___","",["","","","","","","",""]))
("Parsec ",("","___","",["","","","","","","",""]))
("Posix  ",("","___","",["","","","","","","",""]))
("TRE    ",("","___","",["_","","","","","","",""]))
("TDFA   ",*** Exception: Text.Regex.TDFA.TNFA.bestTrans.choose the EQ case has different ignore b1 and b2: ([(4,PostUpdate TagTask),(5,PostUpdate TagTask)],[(4,PreUpdate ResetTask),(6,PreUpdate ResetTask),(7,PreUpdate EnterOrbitTask),(8,PreUpdate TagTask),(9,PostUpdate TagTask),(10,PostUpdate TagTask),(11,PreUpdate ResetTask)]) : fromList [(#1,[(5,PostUpdate TagTask),(4,PostUpdate TagTask)]),(#1,[(6,PreUpdate TagTask),(4,PreUpdate TagTask),(4,PreUpdate ResetTask),(6,PreUpdate ResetTask),(9,PreUpdate ResetTask),(11,PreUpdate ResetTask),(7,PreUpdate EnterOrbitTask),(8,PreUpdate TagTask),(10,PostUpdate TagTask),(9,PostUpdate TagTask)])]

("a_","(.*|())((a|a|()|())*)*((())+)*",("","a_","",["a_","","","","","","","",""]),"same")
*** Exception: Should not get here: Text.Regex.TNFA.ignoreCommand PreUpdate EnterOrbitTask

("b__","((_|(b|.|.)+))(.)((()|.))",("","b__","",["b_","b_","_","_","","",""]),"same")
*** Exception: Should not get here: Text.Regex.TNFA.ignoreCommand PreUpdate EnterOrbitTask

("bbA","b|()|()|()(.)(.|())+|b",("","b","bA",["","","","","",""]),("","bbA","",["","","","b","",""]))
"FAIL FAIL FAIL"
"bbA"
"b|()|()|()(.)(.|())+|b"
("PCRE   ",("","b","bA",["","","","","",""]))
("Parsec ",("","b","bA",["","","","","",""]))
("Posix  ",("","bbA","",["","","","b","",""]))
("TRE    ",("","bbA","",["","","","b","A",""]))
("TDFA   ",*** Exception: Text.Regex.TDFA.TNFA.bestTrans.choose the EQ case has different ignore b1 and b2: ([(6,PostUpdate TagTask),(9,PostUpdate TagTask)],[(6,PreUpdate ResetTask),(10,PreUpdate ResetTask),(11,PreUpdate EnterOrbitTask),(12,PreUpdate TagTask),(13,PostUpdate TagTask),(14,PostUpdate TagTask),(15,PreUpdate ResetTask)]) : fromList [(#3,[(9,PostUpdate TagTask),(6,PostUpdate TagTask)]),(#3,[(10,PreUpdate TagTask),(6,PreUpdate TagTask),(6,PreUpdate ResetTask),(10,PreUpdate ResetTask),(13,PreUpdate ResetTask),(15,PreUpdate ResetTask),(11,PreUpdate EnterOrbitTask),(12,PreUpdate TagTask),(14,PostUpdate TagTask),(13,PostUpdate TagTask)])]

("AC\n","(.|((())+)|.)*A",("","A","C\n",["","","",""]),"same")
*** Exception: Should not get here: Text.Regex.TNFA.ignoreCommand PreUpdate EnterOrbitTask

-}

{-

*Main> tdfa "AC\n" "(.|.)*A" :: MatchArray
array *** Exception: Should not get here: Text.Regex.TNFA.ignoreCommand PreUpdate EnterOrbitTask

*Text.Regex.TDFA.Live> full "(.|.)*A"

(.|.)*A

"(.|.)*A"

PConcat [PStar True (PGroup 1 (POr [PDot {getDoPa = #1}
                                   ,PDot {getDoPa = #2}]))
        ,PChar {getDoPa = #3, getPatternChar = 'A'}]

(Q { nullQ = []
  , takes = (1,Nothing)
  , preTag = Nothing
  , postTag = Nothing
  , tagged = False
  , wants = WantsQNFA
  , unQ = Seq Q { nullQ = [(SetTestInfo [],[(2,PreUpdate TagTask)])]
            , takes = (0,Nothing)
            , preTag = Nothing
            , postTag = Just 2
            , tagged = True
            , wants = WantsQT
            , unQ = Star {getOrbit = Just 3, reset = [5], firstNull = True, unStar = Q { nullQ = []
                      , takes = (1,Just 1)
start 1               , preTag = Just 4
stop 1                , postTag = Just 5
                      , tagged = True
                      , wants = WantsQNFA
                      , unQ = Or [Q { nullQ = []
                                , takes = (1,Just 1)
                                , preTag = Nothing
                                , postTag = Nothing
                                , tagged = False
                                , wants = WantsQNFA
                                , unQ = OneChar (PDot {getDoPa = #1})
                               },Q { nullQ = []
                                , takes = (1,Just 1)
                                , preTag = Nothing
                                , postTag = Nothing
                                , tagged = False
                                , wants = WantsQNFA
                                , unQ = OneChar (PDot {getDoPa = #2})
                               }]
                     }}
           } Q { nullQ = []
            , takes = (1,Just 1)
            , preTag = Nothing
            , postTag = Just 1
            , tagged = False
            , wants = WantsQNFA
            , unQ = OneChar (PChar {getDoPa = #3, getPatternChar = 'A'})
           }
 },

array (0,5) [(0,Minimize),(1,Maximize),(2,Maximize),(3,Orbit),(4,Minimize),(5,Maximize)],

array (1,1) [(1,[GroupInfo {thisIndex = 1, parentIndex = 0, startTag = 4, stopTag = 5}])])

QNFA {q_id = 0
     ,q_qt = {qt_win=[(1,PreUpdate TagTask)]
, qt_trans=[]
, qt_other=[]}
}
QNFA {q_id = 1
     ,q_qt = {qt_win=[]
, qt_trans=[('\n',[])
           ,('A',[(0,[(#3,[(3,PreUpdate LeaveOrbitTask),(2,PreUpdate TagTask),(1,PostUpdate TagTask)])])
                 ,(1,[(#1,[(5,PreUpdate ResetTask),(3,PreUpdate EnterOrbitTask),(4,PreUpdate TagTask),(5,PostUpdate TagTask)])
                     ,(#2,[(5,PreUpdate ResetTask),(3,PreUpdate EnterOrbitTask),(4,PreUpdate TagTask),(5,PostUpdate TagTask)])])])]
, qt_other=[(1,[(#1,[(5,PreUpdate ResetTask),(3,PreUpdate EnterOrbitTask),(4,PreUpdate TagTask),(5,PostUpdate TagTask)])
               ,(#2,[(5,PreUpdate ResetTask),(3,PreUpdate EnterOrbitTask),(4,PreUpdate TagTask),(5,PostUpdate TagTask)])])]}
}

*** Exception: Should not get here: Text.Regex.TNFA.ignoreCommand PreUpdate EnterOrbitTask

More errMsg information:

*** Exception: Should not get here: Text.Regex.TNFA.ignoreCommand PreUpdate EnterOrbitTask :
fromList [(#1,[(5,PreUpdate ResetTask),(3,PreUpdate EnterOrbitTask),(4,PreUpdate TagTask),(5,PostUpdate TagTask)])
         ,(#2,[(5,PreUpdate ResetTask),(3,PreUpdate EnterOrbitTask),(4,PreUpdate TagTask),(5,PostUpdate TagTask)])]

*Text.Regex.TDFA.Live> 

COMPARE WITH

*Text.Regex.TDFA.Live> full "(.)*A"

(.)*A

"(.)*A"

PConcat [PStar True (PGroup 1 (PDot {getDoPa = #1}))
        ,PChar {getDoPa = #2, getPatternChar = 'A'}]

(Q { nullQ = []
  , takes = (1,Nothing)
  , preTag = Nothing
  , postTag = Nothing
  , tagged = False
  , wants = WantsQNFA
  , unQ = Seq Q { nullQ = [(SetTestInfo [],[(2,PreUpdate TagTask)])]
            , takes = (0,Nothing)
            , preTag = Nothing
            , postTag = Just 2
            , tagged = True
            , wants = WantsQT
            , unQ = Star {getOrbit = Just 3, reset = [5], firstNull = True, unStar = Q { nullQ = []
                      , takes = (1,Just 1)
start 1               , preTag = Just 4
stop 1                , postTag = Just 5
                      , tagged = True
                      , wants = WantsQNFA
                      , unQ = OneChar (PDot {getDoPa = #1})
                     }}
           } Q { nullQ = []
            , takes = (1,Just 1)
            , preTag = Nothing
            , postTag = Just 1
            , tagged = False
            , wants = WantsQNFA
            , unQ = OneChar (PChar {getDoPa = #2, getPatternChar = 'A'})
           }
 },

array (0,5) [(0,Minimize),(1,Maximize),(2,Maximize),(3,Orbit),(4,Minimize),(5,Maximize)],

array (1,1) [(1,[GroupInfo {thisIndex = 1, parentIndex = 0, startTag = 4, stopTag = 5}])])

QNFA {q_id = 0
     ,q_qt = {qt_win=[(1,PreUpdate TagTask)]
, qt_trans=[]
, qt_other=[]}
}
QNFA {q_id = 1
     ,q_qt = {qt_win=[]
, qt_trans=[('\n',[])
           ,('A',[(0,[(#2,[(3,PreUpdate LeaveOrbitTask),(2,PreUpdate TagTask),(1,PostUpdate TagTask)])])
                 ,(1,[(#1,[(5,PreUpdate ResetTask),(3,PreUpdate EnterOrbitTask),(4,PreUpdate TagTask),(5,PostUpdate TagTask)])])])]
, qt_other=[(1,[(#1,[(5,PreUpdate ResetTask),(3,PreUpdate EnterOrbitTask),(4,PreUpdate TagTask),(5,PostUpdate TagTask)])])]}
}

DFA {d_id = []
    ,d_dt = Simple' { dt_win = []
        , dt_trans = 
        , dt_other = None
        }
}
DFA {d_id = [0,1]
    ,d_dt = Simple' { dt_win = [(0,([(1,0)],[]))]
, dt_trans = ('\n',([],[]))
             ('A',([0,1],[(0,[(1,(#2,([(1,1),(2,0)],["Leaving Orbit (3,0)"])))])
                         ,(1,[(1,(#1,([(4,0),(5,1)],["Entering Orbit (3,0)"])))])]))

        , dt_other = ([1] , (1,[(1,(#1,([(4,0),(5,1)],["Entering Orbit (3,0)"])))])
)
        }
}
DFA {d_id = [1]
    ,d_dt = Simple' { dt_win = []
, dt_trans = ('\n',([],[]))
             ('A',([0,1],[(0,[(1,(#2,([(1,1),(2,0)],["Leaving Orbit (3,0)"])))])
                         ,(1,[(1,(#1,([(4,0),(5,1)],["Entering Orbit (3,0)"])))])]))

        , dt_other = ([1] , (1,[(1,(#1,([(4,0),(5,1)],["Entering Orbit (3,0)"])))])
)
        }
}

*Text.Regex.TDFA.Live> 
Fix attempt 1 (ignore True)


DFA {d_id = []
    ,d_dt = Simple' { dt_win = []
        , dt_trans = 
        , dt_other = None
        }
}
DFA {d_id = [0,1]
    ,d_dt = Simple' { dt_win = [(0,([(1,0)],[]))]
, dt_trans = ('\n',([],[]))
             ('A',([0,1],[(0,[(1,(#3,([(1,1),(2,0)],["Leaving Orbit (3,0)"])))])
                         ,(1,[(1,(#1,([(4,0),(5,1)],["Entering Orbit (3,0)"])))])]))

        , dt_other = ([1] , (1,[(1,(#1,([(4,0),(5,1)],["Entering Orbit (3,0)"])))])
)
        }
}
DFA {d_id = [1]
    ,d_dt = Simple' { dt_win = []
, dt_trans = ('\n',([],[]))
             ('A',([0,1],[(0,[(1,(#3,([(1,1),(2,0)],["Leaving Orbit (3,0)"])))])
                         ,(1,[(1,(#1,([(4,0),(5,1)],["Entering Orbit (3,0)"])))])]))

        , dt_other = ([1] , (1,[(1,(#1,([(4,0),(5,1)],["Entering Orbit (3,0)"])))])
)
        }
}
-}

{-
("B","..|(B)?.?",("","B","",["B"]),"same")
"FAIL FAIL FAIL"
"B"
"..|(B)?.?"
("PCRE   ",("","B","",["B"]))
("Parsec ",("","B","",["B"]))
("Posix  ",("","B","",["B"]))
("TRE    ",("","B","",[""]))
("TDFA   ",("","B","",["B"]))
("Old    ",("","B","",["B"]))


("\n\n","((.?))|(((\n|(())*))+)",("","","\n\n",["","","","","","",""]),("","\n\n","",["","","\n\n","","","",""]))
"FAIL FAIL FAIL"
"\n\n"
"((.?))|(((\n|(())*))+)"
("PCRE   ",("","","\n\n",["","","","","","",""]))
("Parsec ",("","","\n\n",["","","","","","",""]))
("Posix  ",("","\n\n","",["","","\n\n","","","",""]))
("TRE    ",("","\n\n","",["","","\n\n","\n","\n","",""]))
("TDFA   ",*** Exception: Text.Regex.TDFA.TNFA.bestTrans.choose the EQ case has different ignore b1 and b2:
([(5,PostUpdate TagTask),(6,PostUpdate TagTask)]
,[(5,PreUpdate ResetTask),(8,PreUpdate TagTask),(9,PreUpdate ResetTask),(10,PreUpdate EnterOrbitTask),(11,PreUpdate TagTask),(12,PostUpdate TagTask),(13,PostUpdate TagTask),(14,PreUpdate ResetTask),(16,PreUpdate ResetTask)])
 : 
fromList [(#2,[(6,PostUpdate TagTask),(5,PostUpdate TagTask)])
         ,(#2,[(9,PreUpdate TagTask),(8,PreUpdate TagTask),(5,PreUpdate TagTask),(5,PreUpdate ResetTask),(9,PreUpdate ResetTask),(12,PreUpdate ResetTask),(14,PreUpdate ResetTask),(16,PreUpdate ResetTask),(10,PreUpdate EnterOrbitTask),(11,PreUpdate TagTask),(13,PostUpdate TagTask),(12,PostUpdate TagTask)])]

("aaa","aa|(.)*.*",("","aa","a",[""]),("","aaa","",["a"]))
"FAIL FAIL FAIL"
"aaa"
"aa|(.)*.*"
("PCRE   ",("","aa","a",[""]))
("Parsec ",("","aa","a",[""]))
("Posix  ",("","aaa","",["a"]))
("TRE    ",("","aaa","",[""]))
("TDFA   ",("","aaa","",["a"]))
("Old    ",("","aaa","",["a"]))
False

("bCCba","((.+)*)+.(((.)+|C|.|())|((b)))",("","bCCba","",["","bCCb","","","","","",""]),("","bCCba","",["","","","","","","",""]))
"FAIL FAIL FAIL"
"bCCba"
"((.+)*)+.(((.)+|C|.|())|((b)))"
("PCRE   ",("","bCCba","",["","bCCb","","","","","",""]))
("Parsec ",("","bCCba","",["","bCCb","","","","","",""]))
("Posix  ",("","bCCba","",["","","","","","","",""]))
("TRE    ",("","bCCba","",["bCCb","bCCb","","","","","",""]))
("TDFA   ",("*** Exception: Text.Regex.TDFA.Run.makeTagComparer.comp: tv2 Orbit without tv1, bestCase GT : array (0,25) [(0,Minimize),(1,Maximize),(2,Maximize),(3,Maximize),(4,Maximize),(5,Orbit),(6,Minimize),(7,Maximize),(8,Maximize),(9,Orbit),(10,Minimize),(11,Maximize),(12,Orbit),(13,Minimize),(14,Maximize),(15,Maximize),(16,Maximize),(17,Maximize),(18,Maximize),(19,Maximize),(20,Maximize),(21,Orbit),(22,Minimize),(23,Maximize),(24,Maximize),(25,Maximize)]
(fromList [(0,0),(9,0),(10,0),(12,0),(13,0),(15,1)],fromList [(9,fromList [0]),(12,fromList [0])])
(fromList [(0,0),(5,0),(6,0),(8,1),(9,1),(10,1),(12,1),(13,1),(15,2)],fromList [(5,fromList [0]),(9,fromList [1]),(12,fromList [1])])

*Main> "bCCba" `tdfa`  "((.?)*)+" :: MatchArray
array *** Exception: Text.Regex.TDFA.Run.makeTagComparer.comp: tv2 Orbit without tv1, bestCase GT : array (0,13) [(0,Minimize),(1,Maximize),(2,Maximize),(3,Orbit),(4,Minimize),(5,Maximize),(6,Maximize),(7,Orbit),(8,Minimize),(9,Maximize),(10,Orbit),(11,Minimize),(12,Maximize),(13,Maximize)]
(fromList [(0,0),(4,0),(7,0),(8,0),(10,0),(11,1),(12,2),(13,2)],fromList [(7,fromList [0]),(10,fromList [0,1])])
(fromList [(0,0),(3,0),(4,0),(6,1),(7,1),(8,1),(10,1),(11,1),(12,2),(13,2)],fromList [(3,fromList [0]),(7,fromList [1]),(10,fromList [1])])


("AAAc","(A|A)((.*)*)+(A)?|.",("","AAAc","",["A","AAc","",""]),("","AAAc","",["A","","",""]))
"Fail FAIL FAIL"
"AAAc"
"(A|A)((.*)*)+(A)?|."
("PCRE   ",("","AAAc","",["A","AAc","",""]))
("Parsec ",("","AAAc","",["A","","",""]))
("Posix  ",("","AAAc","",["A","","",""]))
("TRE    ",("","AAAc","",["A","AAc","AAc",""]))
("TDFA   ",("*** Exception: Text.Regex.TDFA.Run.makeTagComparer.comp: tv2 Orbit without tv1, bestCase GT :
array (0,15) [(0,Minimize),(1,Maximize),(2,Maximize),(3,Maximize),(4,Maximize),(5,Maximize),(6,Orbit),(7,Minimize),(8,Maximize),(9,Orbit),(10,Minimize),(11,Maximize),(12,Orbit),(13,Minimize),(14,Maximize),(15,Maximize)]
(fromList [(0,0),(5,1)      ,(7,1),(9,1),(10,1),(12,1),(13,1)],fromList [(9,fromList [1]),(12,fromList [1])])
(fromList [(0,0),(5,1),(6,1),(7,1),(9,2),(10,2),(12,2),(13,2)],fromList [(6,fromList [1]),(9,fromList [2]),(12,fromList [2])])
-}
{-

*Text.Regex.TDFA.Live> tdfa "A" "((.*)*)+" :: MatchArray
array (0,2) [(0,(0,1)),(1,(0,1)),(2,(0,1))]
*Text.Regex.TDFA.Live> tdfa "AA" "((.*)*)+" :: MatchArray
array *** Exception: Text.Regex.TDFA.Run.makeTagComparer.comp: tv2 Orbit without tv1, bestCase GT :

array (0,11) [(0,Minimize),(1,Maximize),(2,Maximize),(3,Orbit),(4,Minimize),(5,Maximize),(6,Orbit),(7,Minimize),(8,Maximize),(9,Orbit),(10,Minimize),(11,Maximize)]

(fromList [(0,0)      ,(4,0),(6,0),(7,0),(9,0),(10,0)],fromList [                 (6,fromList [0]),(9,fromList [0])])
(fromList [(0,0),(3,0),(4,0),(6,1),(7,1),(9,1),(10,1)],fromList [(3,fromList [0]),(6,fromList [1]),(9,fromList [1])])

*Text.Regex.TDFA.Live> full "((.*)*)+" 

((.*)*)+

"((.*)*)+"

PConcat [PGroup 1 (PStar True (PGroup 2 (PStar True (PDot {getDoPa = #1}))))
        ,PStar False (PGroup 1 (PStar True (PGroup 2 (PStar True (PDot {getDoPa = #1})))))]

(Q { nullQ = [(SetTestInfo [],[(2,PreUpdate TagTask),(1,PreUpdate TagTask)])]
  , takes = (0,Nothing)
  , preTag = Nothing
  , postTag = Nothing
  , tagged = True
  , wants = WantsQT
  , unQ = Seq Q { nullQ = [(SetTestInfo [],[(2,PreUpdate TagTask)])]
            , takes = (0,Nothing)
            , preTag = Nothing
            , postTag = Just 2
            , tagged = True
            , wants = WantsQT
            , unQ = Star {getOrbit = Just 3, reset = [5,11], firstNull = True, unStar = Q { nullQ = [(SetTestInfo [],[(5,PreUpdate TagTask),(4,PreUpdate TagTask)])]
                      , takes = (0,Nothing)
                      , preTag = Just 4
                      , postTag = Just 5
                      , tagged = True
                      , wants = WantsQT
                      , unQ = Star {getOrbit = Nothing, reset = [], firstNull = True, unStar = Q { nullQ = []
                                , takes = (1,Just 1)
                                , preTag = Nothing
                                , postTag = Nothing
                                , tagged = False
                                , wants = WantsQNFA
                                , unQ = OneChar (PDot {getDoPa = #1})
                               }}
                     }}
           } Q { nullQ = [(SetTestInfo [],[(1,PreUpdate TagTask)])]
            , takes = (0,Nothing)
            , preTag = Nothing
            , postTag = Just 1
            , tagged = True
            , wants = WantsQT
            , unQ = Star {getOrbit = Just 6, reset = [2,5,8,9,11], firstNull = False, unStar = Q { nullQ = [(SetTestInfo [],[(8,PreUpdate TagTask),(7,PreUpdate TagTask)])]
                      , takes = (0,Nothing)
                      , preTag = Just 7
                      , postTag = Just 8
                      , tagged = True
                      , wants = WantsQT
                      , unQ = Star {getOrbit = Just 9, reset = [5,11], firstNull = True, unStar = Q { nullQ = [(SetTestInfo [],[(11,PreUpdate TagTask),(10,PreUpdate TagTask)])]
                                , takes = (0,Nothing)
                                , preTag = Just 10
                                , postTag = Just 11
                                , tagged = True
                                , wants = WantsQT
                                , unQ = Star {getOrbit = Nothing, reset = [], firstNull = True, unStar = Q { nullQ = []
                                          , takes = (1,Just 1)
                                          , preTag = Nothing
                                          , postTag = Nothing
                                          , tagged = False
                                          , wants = WantsQNFA
                                          , unQ = OneChar (PDot {getDoPa = #1})
                                         }}
                               }}
                     }}
           }
 },

array (0,11) [(0,Minimize),(1,Maximize),(2,Maximize),(3,Orbit),(4,Minimize),(5,Maximize),(6,Orbit),(7,Minimize),(8,Maximize),(9,Orbit),(10,Minimize),(11,Maximize)],

array (1,2) [(1,[GroupInfo {thisIndex = 1, parentIndex = 0, startTag = 7, stopTag = 8}
                ,GroupInfo {thisIndex = 1, parentIndex = 0, startTag = 0, stopTag = 2}])
            ,(2,[GroupInfo {thisIndex = 2, parentIndex = 1, startTag = 10, stopTag = 11}
                ,GroupInfo {thisIndex = 2, parentIndex = 1, startTag = 4, stopTag = 5}])])

QNFA {q_id = 0
     ,q_qt = {qt_win=[(11,PreUpdate TagTask),(9,PreUpdate LeaveOrbitTask),(8,PreUpdate TagTask),(6,PreUpdate LeaveOrbitTask),(1,PreUpdate TagTask),(1,PreUpdate TagTask)]
, qt_trans=[('\n',[])]
, qt_other=[(0,[(#1,[])
               ,(#1,[(11,PreUpdate TagTask),(5,PreUpdate ResetTask),(11,PreUpdate ResetTask),(9,PreUpdate EnterOrbitTask),(10,PreUpdate TagTask)])
               ,(#1,[(11,PreUpdate TagTask),(9,PreUpdate LeaveOrbitTask),(8,PreUpdate TagTask),(2,PreUpdate ResetTask),(5,PreUpdate ResetTask),(8,PreUpdate ResetTask),(9,PreUpdate ResetTask),(11,PreUpdate ResetTask),(6,PreUpdate EnterOrbitTask),(7,PreUpdate TagTask),(5,PreUpdate ResetTask),(11,PreUpdate ResetTask),(9,PreUpdate EnterOrbitTask),(10,PreUpdate TagTask)])])]}
}

QNFA {q_id = 1
     ,q_qt = {qt_win=[(5,PreUpdate TagTask),(3,PreUpdate LeaveOrbitTask),(2,PreUpdate TagTask),(6,PreUpdate LeaveOrbitTask),(1,PreUpdate TagTask),(1,PreUpdate TagTask)]
, qt_trans=[('\n',[])]
, qt_other=[(0,[(#1,[(5,PreUpdate TagTask),(3,PreUpdate LeaveOrbitTask),(2,PreUpdate TagTask),(2,PreUpdate ResetTask),(5,PreUpdate ResetTask),(8,PreUpdate ResetTask),(9,PreUpdate ResetTask),(11,PreUpdate ResetTask),(6,PreUpdate EnterOrbitTask),(7,PreUpdate TagTask),(5,PreUpdate ResetTask),(11,PreUpdate ResetTask),(9,PreUpdate EnterOrbitTask),(10,PreUpdate TagTask)])])
           ,(1,[(#1,[])
               ,(#1,[(5,PreUpdate TagTask),(5,PreUpdate ResetTask),(11,PreUpdate ResetTask),(3,PreUpdate EnterOrbitTask),(4,PreUpdate TagTask)])])]}
}
QNFA {q_id = 2
     ,q_qt = {qt_win=[(5,PreUpdate TagTask),(4,PreUpdate TagTask),(2,PreUpdate TagTask),(6,PreUpdate LeaveOrbitTask),(1,PreUpdate TagTask),(1,PreUpdate TagTask)]
, qt_trans=[('\n',[])]
, qt_other=[(0,[(#1,[(5,PreUpdate TagTask),(4,PreUpdate TagTask),(2,PreUpdate TagTask),(2,PreUpdate ResetTask),(5,PreUpdate ResetTask),(8,PreUpdate ResetTask),(9,PreUpdate ResetTask),(11,PreUpdate ResetTask),(6,PreUpdate EnterOrbitTask),(7,PreUpdate TagTask),(5,PreUpdate ResetTask),(11,PreUpdate ResetTask),(9,PreUpdate EnterOrbitTask),(10,PreUpdate TagTask)])])
           ,(1,[(#1,[(5,PreUpdate ResetTask),(11,PreUpdate ResetTask),(3,PreUpdate EnterOrbitTask),(4,PreUpdate TagTask)])])]}
}

DFA {d_id = []
    ,d_dt = Simple' { dt_win = []
        , dt_trans = 
        , dt_other = None
        }
}
DFA {d_id = [0,1]
    ,d_dt = Simple' { dt_win = [(0,([(1,0),(8,0),(11,0)],["Leaving Orbit (6,0)","Leaving Orbit (9,0)"]))
                               ,(1,([(1,0),(2,0),(5,0)],["Leaving Orbit (3,0)","Leaving Orbit (6,0)"]))]
        , dt_trans = ('\n',([],[]))

        , dt_other = ([0,1] , (0,[(0,(#1,([],[])))
                                 ,(1,(#1,([(2,-1),(5,-1),(7,0),(8,-1),(10,0),(11,-1)],["Leaving Orbit (3,0)","Entering Orbit (6,0)","Entering Orbit (9,0)"])))])
                              (1,[(1,(#1,([],[])))])
)
        }
}
DFA {d_id = [2]
    ,d_dt = Simple' { dt_win = [(2,([(1,0),(2,0),(4,0),(5,0)],["Leaving Orbit (6,0)"]))]
        , dt_trans = ('\n',([],[]))

        , dt_other = ([0,1] , (0,[(2,(#1,([(2,-1),(4,0),(5,-1),(7,0),(8,-1),(10,0),(11,-1)],["Entering Orbit (6,0)","Entering Orbit (9,0)"])))])
                              (1,[(2,(#1,([(4,0),(5,-1),(11,-1)],["Entering Orbit (3,0)"])))])
)
        }
}

*Text.Regex.TDFA.Live> 

-}


{-

to simplify the system:

Add to scratch : Map PatternIndex GroupInfo

data OP = Maximize (Maybe GroupInfo) | Minimize | Orbit
data TagTasks = TagTask
              | EnterOrbitTask | LeaveOrbitTask | ResetOrbitTask
              | ResetGroupTask
data TagUpdate = PreUpdate TagTask | PostUpdate TagTask
type TagList = [(Tag,TagUpdate)]

Since a Tag that finishes a group is always Maximize and a tag will
finish at most one group, this works fine.

Update a Minimize, Pre/PostUpdate TagTask
  Put current/succ offset in Map Tag Position

Update a Maximize Nothing, Pre/PostUpdate TagTask
  Put current/succ offset in Map Tag Position

Update a Maximize (Just gi), Pre/PostUpdate TagTask
  Put current/succ offset in Map Tag Position
  Put (this gi,gi) in Map PatternIndex GroupInfo

Update an Orbit, PreUpdate EnterOrbitTask
  If not in Map Tag Position: Put current offset in Map Tag Position
                              Put singleton Seq in Map Tag (Seq Position)
  If in map: Append current offset to Seq in Map Tag (Seq Position)

Update an Orbit, PreUpdate LeaveOrbitTask
  do nothing

Update an Orbit, PreUpdate ResetOrbitTask
  remove tag from Map Tag Position

Update a Maximize (Just gi), PreUpdate ResetGroupTask
  delete (this gi) from Map PatternIndex GroupInfo
  
And now for the spiffier bit:
  To simplify the TagList:
    reverse the TagList, since the last thing to be done to a tag is the one that matters
    do [] (tag:rest)
       if tag is Orbit then do (tag:old) rest
              is Maximize{}/Minimize then : do (tag:old) (nub tag rest)

To compare two simplified TagList, the case I need to think about is Orbit.
  If the first 



-}


{- Redesign from Scratch

Types of commands line up with different Scratch data.

Context: Map Tag (Maximize | Minimize)

Scratch is tp :: Map Tag Pos        -- main tags for subexpression work: Maximize and Minimize
           ol :: Map ??? (Seq Pos)  -- Stores the Orbits
           cg :: Map PatternIndex ??? -- Store for most recent groups

Runtime comparison:
  for two candidates go though their tags in ascending order
    if the smallest tags are equal then check their offsets, if equal then go to next higher tags
    if one has a Maximize tag the other does not then that one wins
    if one has a Minimize tag the other does not then ??? perhaps LT ???
 
Natural operations from building the NFA:
  PreUpdate some Tags with with the current offset
  PostUpdate some Tags with the current offset

  Enter the scope of a * for the 1st time and record the current offset in a fresh list
  Re-enter a * and append the current offset to the list
  Leave the scope of a *
  Re-enter the scope of a * and discard the old list and record the current offset in a fresh list

  ((.)+)+ has group 1 and group 2, expands to ((.)(.)*)((.)(.)*)*
  12 / /                                      12 /2 / /12 /2 / /

  (.){0,2} expands to ((.)(.)?)? and (.){0,3} expands to ((.)((.)(.)?)?)?
  1 /                  1 /1 /        1 /                  1 / 1 /1 /

  So the group clones can show up in many places and without a * in sight.
  No capturing group will ever share a closing tag.
  The closing tag of a group can be the opening tag of another group, including its clone.
  The opening tag of a group can be the opening tag of another group, perhaps not nested (e.g. begin of Or).

  Consider (.(a).|.(b).){0,2} and let A=(a) and B=(b) so (.A.|.B.){0,2} is ((.A.|.B.)(.A.|.B.)?)?
           1 2 /   3 / /              2 2 /     3 3 /    1 2   3 /          1 2   3 /1 2   3 /
  

-}

{-

New design repair:

*Text.Regex.TDFA.Live> full "(a*)*"

(a*)*

"(a*)*"

PStar True (PGroup (Just 1) (PStar True (PChar {getDoPa = #1, getPatternChar = 'a'})))
(Q { nullQ = [(SetTestInfo [],[(1,PreUpdate TagTask)])]
  , takes = (0,Nothing)
  , preTag = Nothing
  , postTag = Just 1
  , preReset = []
  , tagged = True
  , wants = WantsQT
  , unQ = Star {getOrbit = Just 2, resetOrbits = [], firstNull = True, unStar = Q { nullQ = [(SetTestInfo [],[(4,PreUpdate TagTask),(3,PreUpdate TagTask)])]
            , takes = (0,Nothing)
            , preTag = Just 3
            , postTag = Just 4
            , preReset = [4]
            , tagged = True
            , wants = WantsQT
            , unQ = Star {getOrbit = Nothing, resetOrbits = [], firstNull = True, unStar = Q { nullQ = []
                      , takes = (1,Just 1)
                      , preTag = Nothing
                      , postTag = Nothing
                      , preReset = []
                      , tagged = False
                      , wants = WantsQNFA
                      , unQ = OneChar (PChar {getDoPa = #1, getPatternChar = 'a'})
                     }}
           }}
 },

array (0,4) [(0,Minimize),(1,Maximize),(2,Orbit),(3,Minimize),(4,Maximize)],

array (1,1) [(1,[GroupInfo {thisIndex = 1, parentIndex = 0, startTag = 3, stopTag = 4, myChildren = []}])])

QNFA {q_id = 0
     ,q_qt = {qt_win=[(4,PreUpdate TagTask),(2,PreUpdate LeaveOrbitTask),(1,PreUpdate TagTask),(1,PreUpdate TagTask)]
, qt_trans=[('a',[(0,[(#1,[])
                     ,(#1,[(4,PreUpdate TagTask),(2,PreUpdate EnterOrbitTask),(4,PreUpdate ResetGroupStopTask),(3,PreUpdate TagTask)])])])]
, qt_other=[]}
}
QNFA {q_id = 1
     ,q_qt = {qt_win=[(4,PreUpdate TagTask),(3,PreUpdate TagTask),(1,PreUpdate TagTask),(1,PreUpdate TagTask)]
, qt_trans=[('a',[(0,[(#1,[(2,PreUpdate EnterOrbitTask),(4,PreUpdate ResetGroupStopTask),(3,PreUpdate TagTask)])])])]
, qt_other=[]}
}

DFA {d_id = [0]
    ,d_dt = Simple' { dt_win = [(0,([(1,(0,True)),(2,(-1,False)),(4,(0,True))],["Leaving Orbit 2"]))]
        , dt_trans = ('a',([0],[(0,[(0,(#1,([(3,(0,True)),(4,(0,False))],["Entering Orbit (2,0)","Reset Tag (4,0)"])))])]))

        , dt_other = None
        }
}
DFA {d_id = [1]
    ,d_dt = Simple' { dt_win = [(1,([(1,(0,True)),(3,(0,True)),(4,(0,True))],[]))]
        , dt_trans = ('a',([0],[(0,[(1,(#1,([(3,(0,True)),(4,(-1,False))],["Entering Orbit (2,0)","Reset Tag (4,0)"])))])]))

        , dt_other = None
        }
}

*Text.Regex.TDFA.Live> 


-}

{-

*Text.Regex.TDFA.Live> full "(a|b)?.*"

(a|b)?.*

"(a|b)?.*"

PConcat [POr [PGroup (Just 1) (POr [PChar {getDoPa = #1, getPatternChar = 'a'}
                                   ,PChar {getDoPa = #2, getPatternChar = 'b'}])
             ,PEmpty]
        ,PStar True (PDot {getDoPa = #3})]

(Q { nullQ = [(SetTestInfo [],[(2,PreUpdate TagTask),(1,PreUpdate TagTask)])]
  , takes = (0,Nothing)
  , preReset = []
  , preTag = Nothing
  , postTag = Nothing
  , tagged = True
  , wants = WantsQT
  , unQ = Seq Q { nullQ = [(SetTestInfo [],[(2,PreUpdate TagTask)])]
            , takes = (0,Just 1)
            , preReset = []
            , preTag = Nothing
            , postTag = Just 2
            , tagged = True
            , wants = WantsQNFA
            , unQ = Or [Q { nullQ = []
                      , takes = (1,Just 1)
                      , preReset = [3]
                      , preTag = Nothing
                      , postTag = Just 3
                      , tagged = True
                      , wants = WantsQNFA
                      , unQ = Or [Q { nullQ = []
                                , takes = (1,Just 1)
                                , preReset = []
                                , preTag = Nothing
                                , postTag = Nothing
                                , tagged = False
                                , wants = WantsQNFA
                                , unQ = OneChar (PChar {getDoPa = #1, getPatternChar = 'a'})
                               },Q { nullQ = []
                                , takes = (1,Just 1)
                                , preReset = []
                                , preTag = Nothing
                                , postTag = Nothing
                                , tagged = False
                                , wants = WantsQNFA
                                , unQ = OneChar (PChar {getDoPa = #2, getPatternChar = 'b'})
                               }]
                     },Q { nullQ = [(SetTestInfo [],[])]
                      , takes = (0,Just 0)
                      , preReset = []
                      , preTag = Nothing
                      , postTag = Nothing
                      , tagged = False
                      , wants = WantsEither
                      , unQ = Empty
                     }]
           } Q { nullQ = [(SetTestInfo [],[(1,PreUpdate TagTask)])]
            , takes = (0,Nothing)
            , preReset = []
            , preTag = Nothing
            , postTag = Just 1
            , tagged = True
            , wants = WantsQT
            , unQ = Star {getOrbit = Nothing, resetOrbits = [], firstNull = True, unStar = Q { nullQ = []
                      , takes = (1,Just 1)
                      , preReset = []
                      , preTag = Nothing
                      , postTag = Nothing
                      , tagged = False
                      , wants = WantsQNFA
                      , unQ = OneChar (PDot {getDoPa = #3})
                     }}
           }
 },

array (0,3) [(0,Minimize),(1,Maximize)
            ,(2,Maximize)                 -- after (a|b)? -> ((a|b)|()) to increase length of ?
            ,(3,Maximize)],               -- after a|b to get end of group 1 and prefer to Empty branch of ?

array (1,1) [(1,[GroupInfo {thisIndex = 1, parentIndex = 0, startTag = 0, stopTag = 3, myChildren = []}])])

QNFA {q_id = 0
     ,q_qt = {qt_win=[(1,PreUpdate TagTask),(1,PreUpdate TagTask)]
, qt_trans=[('\n',[])]
, qt_other=[(0,[(#3,[])])]}
}

QNFA {q_id = 1
     ,q_qt = {qt_win=[(2,PreUpdate TagTask),(1,PreUpdate TagTask),(1,PreUpdate TagTask)]
, qt_trans=[('\n',[])
           ,('a',[(0,[(#3,[(2,PreUpdate TagTask)])
                     ,(#1,[(3,PreUpdate ResetGroupStopTask),(3,PostUpdate TagTask),(2,PostUpdate TagTask)])])])
           ,('b',[(0,[(#3,[(2,PreUpdate TagTask)])
                     ,(#2,[(3,PreUpdate ResetGroupStopTask),(3,PostUpdate TagTask),(2,PostUpdate TagTask)])])])]
, qt_other=[(0,[(#3,[(2,PreUpdate TagTask)])])]}
}

DFA {d_id = []
    ,d_dt = Simple' { dt_win = []
        , dt_trans = 
        , dt_other = None
        }
}
DFA {d_id = [0]
    ,d_dt = Simple' { dt_win = [(0,([(1,(0,True))],[]))]
        , dt_trans = ('\n',([],[]))

        , dt_other = ([0] , (0,[(0,(#3,([],[])))])
)
        }
}
DFA {d_id = [1]
    ,d_dt = Simple' { dt_win = [(1,([(1,(0,True)),(2,(0,True))],[]))]
        , dt_trans = ('\n',([],[]))
('a',([0],[(0,[(1,(#3,([(2,(0,True))],[])))])]))
('b',([0],[(0,[(1,(#3,([(2,(0,True))],[])))])]))

        , dt_other = ([0] , (0,[(1,(#3,([(2,(0,True))],[])))])
)
        }
}

*Text.Regex.TDFA.Live> 

-}