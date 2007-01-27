-- | Common provides simple functions to the backend.  It defines most
-- of the data types.
module Text.Regex.TDFA.Common {- export everything -} where

import Text.Regex.Base(MatchOffset,MatchLength)
import Control.Monad.RWS(RWS)
import Data.Array.IArray(Array)
import Data.Map(Map)
import Data.Set(Set)
import Data.IntMap(IntMap)
import Data.IntSet(IntSet)
import Data.Sequence(Seq)
--import Debug.Trace

common_error s t = error ("Explict error in module "++s++" : "++t)

on :: (t1 -> t1 -> t2) -> (t -> t1) -> t -> t -> t2
f `on` g = (\x y -> (g x) `f` (g y))

-- after sort or sortBy the use of nub/nubBy can be replaced by norep/norepBy
norep :: (Eq a) => [a]->[a]
norep [] = []
norep x@[_] = x
norep (a:bs@(c:cs)) | a==c = norep (a:cs)
                    | otherwise = a:norep bs

norepBy :: (a -> a -> Bool) -> [a] -> [a]
norepBy _ [] = []
norepBy _ x@[_] = x
norepBy eqF (a:bs@(c:cs)) | a `eqF` c = norepBy eqF (a:cs)
                          | otherwise = a:norepBy eqF bs

mapFst :: (Functor f) => (t -> t2) -> f (t, t1) -> f (t2, t1)
mapFst f = fmap (\ (a,b) -> (f a,b))

mapSnd :: (Functor f) => (t1 -> t2) -> f (t, t1) -> f (t, t2)
mapSnd f = fmap (\ (a,b) -> (a,f b))

snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x

newtype DoPa = DoPa {dopaIndex :: Int} deriving (Eq,Ord)

instance Show DoPa where
  showsPrec p (DoPa {dopaIndex=i}) = ('#':) . showsPrec p i

-- | 'RegexOption' control whether the pattern is multiline or
-- case-sensitive like Text.Regex and whether to capture the subgroups
-- (\1, \2, etc).
data CompOption = CompOption { caseSensitive :: Bool
                             , multiline :: Bool
                             , rightAssoc :: Bool
                             , lastStarGreedy ::  Bool
                             }
data ExecOption = ExecOption { captureGroups :: Bool
                             , testMatch :: Bool
                             }

-- | 'MatchedStrings' is an IntMap where the keys are GroupIndex
-- numbers and the values are completed substring captures.
--
-- This has now been augmented to also remember the offset and length
-- of the matched string.
type MatchedStrings = IntMap (String,(MatchOffset,MatchLength))

type BoolMultiline = Bool
type BoolCaseSensitive = Bool
type StringInput = String
type StringBeforeMatch = String
type StringOfMatch = String
type StringAfterMatch = String
type StringSubgroups = String
type StringSubPattern = String
type AboutMatch = (StringBeforeMatch,StringOfMatch,StringAfterMatch,[StringSubgroups])

-- Used by CorePattern
type Tag = Int           -- identity of Position tag to set during a transition
type Index = Int         -- NFA node identity number
-- Used by TDFA
type SetIndex = IntSet {- Index -}
type Position = Int      -- index into the text being searched

-- | GroupIndex is for indexing submatches from  parenthesized groups (PGroup)
type GroupIndex = Int
data GroupInfo = GroupInfo {thisIndex,parentIndex::GroupIndex
                           ,startTag,stopTag::Tag
                           ,myChildren::[GroupIndex]           -- ^ groups with this as their parentIndex
                           } deriving Show

-- | The DFA backend specific 'Regex' type, used by this module's '=~'
-- and '=~~' operators.
data Regex = Regex {regex_dfa::DFA                             -- ^ starting DFA state
                   ,regex_init::Index                          -- ^ index of starting DFA state
                   ,regex_tags::Array Tag OP                   -- ^ information about each tag
                   ,regex_groups::Array GroupIndex [GroupInfo] -- ^ information about each group
                   ,regex_compOptions::CompOption              -- 
                   ,regex_execOptions::ExecOption}

data OP = Maximize | Minimize | Orbit deriving (Eq,Show)  -- whether to prefer large or smaller match indices

data QNFA = QNFA {q_id :: Index
                 ,q_qt :: QT}

type QTrans = IntMap {- Destination Index -} [TagCommand]

data QT = Simple {qt_win :: WinTags
                 ,qt_trans :: Map Char QTrans
                 ,qt_other :: QTrans}
        | Testing {qt_test :: WhichTest
                  ,qt_dopas :: Set DoPa
                  ,qt_a,qt_b :: QT}

-- during contruction
data TagTask = TagTask | ResetGroupStopTask
             | ResetOrbitTask | EnterOrbitTask | LeaveOrbitTask deriving (Show,Eq)
type TagTasks = [(Tag,TagTask)]
{-
instance Ord TagTask where
    compare a b | a==b = EQ
    compare TagTask ResetTask = GT  -- "x" =~ "((x?)?x)*" :: MatchArray should prefer inner ?'s Empty to outer ?'s Empty
    compare ResetTask TagTask = LT 
    compare a b = error ("Ord instance for TagTask (called from TNFA.bestTrans.choose EQ branch) found " ++ show (a,b))
-}
-- for QTrans
data TagUpdate = PreUpdate TagTask | PostUpdate TagTask deriving (Show,Eq)
type TagList = [(Tag,TagUpdate)]
type TagCommand = (DoPa,TagList)

type WinTags = TagList -- or TagTasks?

-- Perhaps still in use?  Magic numbers are bad, but...
updateReset,updatePreEnterOrbit,updatePreLeaveOrbit,updatePostEnterOrbit,updatePostLeaveOrbit :: Int
updateReset = -1
updatePreEnterOrbit = -2
updatePreLeaveOrbit = -3
updatePostEnterOrbit = -4
updatePostLeaveOrbit = -5

-- DFA

data DFA = DFA { d_id :: SetIndex, d_dt :: DT}

data DT = Simple' { dt_win :: IntMap {- Index -} (RunState ())
                  , dt_trans :: Map Char (DFA,DTrans)
                  , dt_other :: Maybe (DFA,DTrans) }
        | Testing' { dt_test :: WhichTest
                   , dt_dopas :: Set DoPa
                   , dt_a,dt_b :: DT}

type DTrans = IntMap {- Index of Destination -} (IntMap {- Index of Source -} (DoPa,RunState ()))
type DTrans' = [(Index, [(Index, (DoPa, ([(Tag, (Position,Bool))],[String])))])]

data WhichTest = Test_BOL | Test_EOL deriving (Show,Eq,Ord)  -- known predicates, todo: allow for inversion

-- Run

type RunState = RWS (Position,Position) [String] Scratch

type Scratch = (IntMap (Position,Bool)     -- Bool is False iff it was Reset
               ,IntMap {- Tag -} Orbits)

type Orbits = Seq Position
