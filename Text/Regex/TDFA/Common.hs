-- | Common supports the Lazy Parsec backend.  It defines all the data
-- types except Pattern and exports everything but the contructors of
-- Pattern.
module Text.Regex.TDFA.Common {- export everything -} where

import Data.IntMap(IntMap)
import Data.IntSet(IntSet)

--import Debug.Trace

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
  showsPrec p (DoPa {dopaIndex=i}) = showsPrec p i

newDoPa :: Int -> DoPa
newDoPa i = DoPa i

-- | PatternIndex is for indexing submatches from  parenthesized groups (PGroup)
type PatternIndex = Int

-- | 'RegexOption' control whether the pattern is multiline or
-- case-sensitive like Text.Regex and whether to capture the subgroups
-- (\1, \2, etc).
data CompOption = CompOption {caseSensitive :: Bool,multiline :: Bool}
data ExecOption = ExecOption {captureGroups::Bool}
{-
-- | This is a convenience value of RegexOption with multiline,
-- caseSensitive, and captureGroups all True and longestMatch False.
defaultRegexOption :: RegexOption
defaultRegexOption = RegexOption {multiline = True
                                 ,caseSensitive = True
                                 ,captureGroups = True
                                 ,strategy = Find_LongestMatch
                                 }
-}

-- | 'MatchedStrings' is an IntMap where the keys are PatternIndex
-- numbers and the values are completed substring captures.
--
-- This has now been augmented to also remember the offset and length
-- of the matched string.
type MatchedStrings = IntMap (String,(Int,Int))

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
-- used by TDFA
type SetIndex = IntSet {- Index -}
type Position = Int      -- index into the text being searched
type Delta = Position -> [(Tag,Position)]

data GroupInfo = GroupInfo {thisIndex,parentIndex::PatternIndex,startTag,stopTag::Tag} deriving Show

type WinTags = IntSet {- Tag -}

andTag :: Maybe Tag -> Maybe Tag -> [Tag]
andTag (Just a) (Just b) = [a,b]
andTag (Just a) Nothing  = [a]
andTag Nothing  (Just b) = [b]
andTag Nothing  Nothing  = []
