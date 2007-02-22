{-# OPTIONS -funbox-strict-fields #-}
-- | Common provides simple functions to the backend.  It defines most
-- of the data types.  All modules should call error via the
-- common_error function below.
module Text.Regex.TDFA.Common {- export everything -} where

{- By Chris Kuklewicz, 2007. BSD License, see the LICENSE file. -}

--import Text.Regex.Base(MatchOffset,MatchLength)
import Text.Show.Functions()
import Control.Monad.State(State)
import Data.Array.IArray(Array)
import Data.Array.Unboxed(UArray)
import Data.Map as Map(Map,assocs)
import Data.Set(Set)
import Data.IntMap as IMap (IntMap,findWithDefault,assocs)
import Data.IntSet(IntSet)
import Data.Sequence(Seq)
--import Debug.Trace

{-# INLINE look #-}
look :: Int -> IntMap a -> a
look key imap = IMap.findWithDefault (common_error "Text.Regex.DFA.Common" ("key "++show key++" not found in look")) key imap

common_error :: String -> String -> a
common_error moduleName message =
  error ("Explict error in module "++moduleName++" : "++message)

on :: (t1 -> t1 -> t2) -> (t -> t1) -> t -> t -> t2
f `on` g = (\x y -> (g x) `f` (g y))

-- | after sort or sortBy the use of nub/nubBy can be replaced by norep/norepBy
norep :: (Eq a) => [a]->[a]
norep [] = []
norep x@[_] = x
norep (a:bs@(c:cs)) | a==c = norep (a:cs)
                    | otherwise = a:norep bs

-- | after sort or sortBy the use of nub/nubBy can be replaced by norep/norepBy
norepBy :: (a -> a -> Bool) -> [a] -> [a]
norepBy _ [] = []
norepBy _ x@[_] = x
norepBy eqF (a:bs@(c:cs)) | a `eqF` c = norepBy eqF (a:cs)
                          | otherwise = a:norepBy eqF bs

mapFst :: (Functor f) => (t -> t2) -> f (t, t1) -> f (t2, t1)
mapFst f = fmap (\ (a,b) -> (f a,b))

mapSnd :: (Functor f) => (t1 -> t2) -> f (t, t1) -> f (t, t2)
mapSnd f = fmap (\ (a,b) -> (a,f b))

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x

thd3 :: (a,b,c) -> c
thd3 (_,_,x) = x

flipOrder :: Ordering -> Ordering
flipOrder GT = LT
flipOrder LT = GT
flipOrder EQ = EQ

noWin :: WinTags -> Bool
noWin = null

-- | Used to track elements of the pattern that accept characters or 
-- are anchors
newtype DoPa = DoPa {dopaIndex :: Int} deriving (Eq,Ord)

instance Show DoPa where
  showsPrec p (DoPa {dopaIndex=i}) = ('#':) . showsPrec p i

-- | Control whether the pattern is multiline or
-- case-sensitive like Text.Regex and whether to capture the subgroups
-- (\1, \2, etc).
data CompOption = CompOption { caseSensitive :: Bool    -- ^ True by default
                             , multiline :: Bool        -- ^ True by default, implies "." and "[^a]" will not match '\n'
                             , rightAssoc :: Bool       -- ^ False (and therefore left associative) by default
                             , lastStarGreedy ::  Bool  -- ^ False by default.  This is POSIX correct by takes space and is slower.
                                                        -- Setting this to true will improve performance, and should be done
                                                        -- if you plan to set the captureGroups execoption to False.
                             } deriving (Read,Show)
data ExecOption = ExecOption { captureGroups :: Bool    -- ^ True by default.  Set to False to improve speed (and space).
                             , testMatch :: Bool        -- ^ False by default. Set to True to quickly return shortest match (w/o groups).
                             } deriving (Read,Show)

{-
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
-}

-- | Used by implementation to name certain Postions during matching
type Tag = Int           -- ^ identity of Position tag to set during a transition
-- | Internal use to indicate type of tag and preference for larger or smaller Positions
data OP = Maximize | Minimize | Orbit deriving (Eq,Show)
type Index = Int         -- ^ Internal NFA node identity number
type SetIndex = IntSet {- Index -} -- ^ Internal DFA identity is this Set of NFA Index
type Position = Int      -- ^ Index into the text being searched

-- | GroupIndex is for indexing submatches from capturing
-- parenthesized groups (PGroup/Group)
type GroupIndex = Int
-- | GroupInfo collects the parent and tag information for an instance 
-- of a group
data GroupInfo = GroupInfo {thisIndex,parentIndex::GroupIndex
                           ,startTag,stopTag::Tag
                           } deriving Show

-- | The TDFA backend specific 'Regex' type, used by this module's RegexOptions and RegexMaker
data Regex = Regex {regex_dfa::DFA                             -- ^ starting DFA state
                   ,regex_init::Index                          -- ^ index of starting DFA state
                   ,regex_tags::Array Tag OP                   -- ^ information about each tag
                   ,regex_groups::Array GroupIndex [GroupInfo] -- ^ information about each group
                   ,regex_compOptions::CompOption              -- 
                   ,regex_execOptions::ExecOption}

-- | Internal NFA node type
data QNFA = QNFA {q_id :: Index
                 ,q_qt :: QT}
-- | Internal to QNFA type.
data QT = Simple {qt_win :: WinTags -- ^ empty transitions to the virtual winning state
                 ,qt_trans :: Map Char QTrans -- ^ all ways to leave this QNFA to other or the same QNFA
                 ,qt_other :: QTrans -- ^ default ways to leave this QNFA to other or the same QNFA
                 }
        | Testing {qt_test :: WhichTest -- ^ The test to perform
                  ,qt_dopas :: Set DoPa  -- ^ location(s) of the anchor(s) in the original regexp
                  ,qt_a,qt_b :: QT -- ^ use qt_a if test is True, else use qt_b
                  }

-- | Internal type to represent the tagged transition from one QNFA to
-- another (or itself).  The key is the Index of the destination QNFA.
type QTrans = IntMap {- Destination Index -} [TagCommand]

-- | Known predicates, just Beginning of Line (^) and End of Line ($).
data WhichTest = Test_BOL | Test_EOL deriving (Show,Eq,Ord)

-- | The things that can be done with a Tag.  TagTask and
-- ResetGroupStopTask are for tags with Maximize or Minimize OP
-- values.  ResetOrbitTask and EnterOrbitTask and LeaveOrbitTask are
-- for tags with Orbit OP value.
data TagTask = TagTask | ResetGroupStopTask
             | ResetOrbitTask | EnterOrbitTask | LeaveOrbitTask deriving (Show,Eq)
-- | Ordered list of tags and their associated Task
type TagTasks = [(Tag,TagTask)]
-- | When attached to a QTrans the TagTask can be done before or after
-- accepting the character.
data TagUpdate = PreUpdate TagTask | PostUpdate TagTask deriving (Show,Eq)
-- | Ordered list of tags and their associated update operation.
type TagList = [(Tag,TagUpdate)]
-- | A TagList and the location of the item in the original pattern
-- that is being accepted.
type TagCommand = (DoPa,TagList)
-- | Ordered list of tags and their associated update operation to
-- perform on an empty transition to the virtual winning state.
type WinTags = TagList

-- | Internal DFA node, identified by the Set of indices of the QNFA
-- nodes it represents.
data DFA = DFA { d_id :: SetIndex, d_dt :: DT} deriving(Show)
-- | Internal to the DFA node
data DT = Simple' { dt_win :: IntMap {- Index -} Instructions -- ^ Actions to perform to win
                  , dt_trans :: Map Char (DFA,DTrans) -- ^ Transition to accept Char
                  , dt_other :: Maybe (DFA,DTrans) -- ^ Optional default accepting transition
                  }
        | Testing' { dt_test :: WhichTest -- ^ The test to perform
                   , dt_dopas :: Set DoPa -- ^ location(s) of the anchor(s) in the original regexp
                   , dt_a,dt_b :: DT      -- ^ use dt_a if test is True else use dt_b
                   }

instance Show DT where show = showDT

showDT :: DT -> String
showDT (Simple' w t o) = "Simple' { dt_win = " ++ (unlines . map show . IMap.assocs $ w)
                      ++ "\n        , dt_trans = " ++ (unlines . map (\(char,(dfa,dtrans)) -> "("++show char++", "++show (d_id dfa)++", "
                                                                                    ++ seeDTrans dtrans ++")") . Map.assocs $ t)
                      ++ "\n        , dt_other = " ++ maybe "None" (\(dfa,dtrans) -> "("++show (d_id dfa)++", "++ seeDTrans dtrans++")") o
                      ++ "\n        }"

showDT (Testing' wt d a b) = "Testing' { dt_test = " ++ show wt
                          ++ "\n         , dt_dopas = " ++ show d
                          ++ "\n         , dt_a = " ++ indent a
                          ++ "\n         , dt_b = " ++ indent b
                          ++ "\n         }"
 where indent = init . unlines . (\(h:t) -> h : (map (spaces ++) t)) . lines . showDT
       spaces = replicate 10 ' '

seeDTrans :: DTrans -> String
seeDTrans x = concatMap (\(dest,y) -> unlines . map (\(source,ins) -> show (dest,source,ins) ) . IMap.assocs $ y) (IMap.assocs x)


{-
data DT = Simple' { dt_win :: IntMap {- Index -} (RunState ()) -- ^ Actions to perform to win
                  , dt_trans :: Map Char (DFA,DTrans) -- ^ Transition to accept Char
                  , dt_other :: Maybe (DFA,DTrans) -- ^ Optional default accepting transition
                  }
        | Testing' { dt_test :: WhichTest -- ^ The test to perform
                   , dt_dopas :: Set DoPa -- ^ location(s) of the anchor(s) in the original regexp
                   , dt_a,dt_b :: DT      -- ^ use dt_a if test is True else use dt_b
                   }
-}
-- | Internal type to repesent the commands for the tagged transition.
-- The outer IntMap is for the destination Index and the inner IntMap
-- is for the Source Index.  This is convenient since all runtime data
-- going to the same destination must be compared to find the best.
type DTrans = IntMap {- Index of Destination -} (IntMap {- Index of Source -} (DoPa,Instructions))
-- type DTrans = IntMap {- Index of Destination -} (IntMap {- Index of Source -} (DoPa,RunState ()))
-- | Internal convenience type for the text display code
type DTrans' = [(Index, [(Index, (DoPa, ([(Tag, (Position,Bool))],[String])))])]


{-
-- | Internal type.  This is the Monad in which tag commands are
-- executed to modify the runtime data.
type RunState = RWS (Position,Position) [String] Scratch
-- | Internal type.  This is type of runtime data assoicated with a
-- particular Index.  It is updated on a transition from that Index
-- and comapred against all data with the same destination Index to
-- pick the best.
type Scratch = (IntMap (Position,Bool)     -- ^ Place for all tags, Bool is False iff it was Reset
               ,IntMap {- Tag -} Orbits)   -- ^ Place for record for each tag with OP of Orbit
-- | Positions for which a * was re-started while looping.  Need to
-- append locations but compare starting with front, so use Seq as a
-- Queue.
type Orbits = Seq Position    
-}

-- | Internal type to comapre two Scratch values and pick the "biggest"
type TagComparer = Scratch -> Scratch -> Ordering -- GT if first argument is the preferred one

data Scratch = Scratch
  { scratchPos :: !(UArray Tag Position)
  , scratchFlags :: !(UArray Tag Bool)
  , scratchOrbits :: !OrbitLog
  } deriving (Show) --- XXX shows function

data Orbits = Orbits
  { inOrbit :: !Bool        -- True if enterOrbit, False if LeaveOrbit
  , getOrbits :: !(Seq Position)
  } deriving (Show)

data Instructions = Instructions
  { newPos :: ![(Tag,Bool)] -- False is preUpdate, True is postUpdate
  , newFlags :: ![(Tag,Bool)]   -- apply to scratchFlags
  , newOrbits :: !(Maybe (Position -> OrbitTransformer))
  } deriving (Show)

-- type OrbitInstruction = Position -> IntMap {-Tag-} Orbits -> IntMap {-Tag-} Orbits
type OrbitInstruction = Position -> Orbits -> Orbits
type OrbitLog = IntMap Orbits
type OrbitTransformer = OrbitLog -> OrbitLog

type CompileInstructions a = State
  ( IntMap Bool
  , IntMap Bool
  , IntMap AlterOrbit
  ) a

data AlterOrbit = AlterReset                        -- Delete Orbits
                | AlterLeave                        -- set inOrbit to False if it exists
                | AlterModify { newInOrbit :: Bool  -- new inOrbit value
                              , freshOrbit :: Bool} -- True means getOrbits=Seq.empty
                  deriving (Show)                   -- False means try appening position or else Seq.empty
