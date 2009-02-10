-- | The CorePattern module deconstructs the Pattern tree created by
-- ReadRegex.parseRegex and returns a simpler Q/P tree with
-- annotations at each Q node.  This will be converted by the TNFA
-- module into a QNFA finite automata.
--
-- Of particular note, this Pattern to Q/P conversion creates and
-- assigns all the internal Tags that will be used during the matching
-- process, and associates the captures groups with the tags that
-- represent their starting and ending locations and with their
-- immediate parent group.
--
-- Each Maximize and Minimize tag is held as either a preTag or a
-- postTag by one and only one location in the Q/P tree.  The Orbit
-- tags are each held by one and only one Star node.  Tags that stop a
-- Group are also held in perhaps numerous preReset lists.
--
-- The additional nullQ::nullView field of Q records the potentially
-- complex information about what tests and tags must be used if the
-- pattern unQ::P matches 0 zero characters.  There can be redundancy
-- in nullView, which is eliminated by cleanNullView.
--
-- Uses recursive do notation.
module Text.Regex.TDFA.CorePattern(Q(..),P(..),WhichTest(..),Wanted(..)
                                  ,TestInfo,OP(..),SetTestInfo(..),NullView
                                  ,patternToQ,cleanNullView,cannotAccept,mustAccept) where

import Control.Monad.RWS {- all -}
import Data.Array.IArray(Array,(!),accumArray,listArray)
import Data.List(sort)
import Data.IntMap.EnumMap(EnumMap)
import qualified Data.IntMap.EnumMap as Map(singleton,null,assocs,keysSet)
import Data.Maybe(isNothing)
import Data.IntSet.EnumSet(EnumSet)
import qualified Data.IntSet.EnumSet as Set(singleton,toList,isSubsetOf)
import Text.Regex.TDFA.Common {- all -}
import Text.Regex.TDFA.Pattern(Pattern(..),starTrans)
-- import Debug.Trace

{- By Chris Kuklewicz, 2007. BSD License, see the LICENSE file. -}

err :: String -> a
err = common_error "Text.Regex.TDFA.CorePattern"

debug :: (Show a) => a -> b -> b
debug _ = id

-- Core Pattern Language
data P = Empty
       | Or [Q]
       | Seq Q Q
       | Star { getOrbit :: Maybe Tag  -- tag to prioritize the need to keep track of length of each pass though q
              , resetOrbits :: [Tag]   -- child star's orbits to reset (ResetOrbitTask)
              , firstNull :: Bool      -- Usually True meaning the first pass may match 0 characters
              , unStar :: Q}
       | Test TestInfo                 -- Require the test to be true
       | OneChar Pattern               -- Bring the Pattern element that accepts a character
       | NonEmpty Q                    -- Don't let the Q pattern match nothing
         deriving (Show,Eq)

-- The diagnostics about the pattern
data Q = Q {nullQ :: NullView         -- Ordered list of nullable views
           ,takes :: (Position,Maybe Position)  -- Range of number of accepted characters
           ,preReset :: [Tag]         -- Tags to "reset" (ResetGroupStopTask) (Only immediate children)
           ,preTag,postTag :: Maybe Tag -- Tags assigned around this pattern (TagTask)
           ,tagged :: Bool            -- Whether this node should be tagged -- patternToQ use only
           ,childGroups :: Bool       -- Whether unQ has any PGroups -- patternToQ use only
           ,wants :: Wanted           -- What kind of continuation is used by this pattern
           ,unQ :: P} deriving (Eq)

type TestInfo = (WhichTest,DoPa)

-- This is newtype'd to allow control over class instances
-- This is a set of WhichTest where each test has associated pattern location information
newtype SetTestInfo = SetTestInfo {getTests :: EnumMap WhichTest (EnumSet DoPa)} deriving (Eq)

instance Monoid SetTestInfo where
  mempty = SetTestInfo mempty
  SetTestInfo x `mappend` SetTestInfo y = SetTestInfo (x `mappend` y)

instance Show SetTestInfo where
  show (SetTestInfo sti) = "SetTestInfo "++show (mapSnd (Set.toList) $ Map.assocs sti)

-- There may be several distinct ways for a subtree to conditionally
-- (i.e. with a Test) or unconditionally accept 0 characters.  These
-- are in the list in order of preference, with most preferred listed
-- first.
type NullView = [(SetTestInfo,WinTags)]  -- Ordered list of null views, each is a set of tests and tags

-- During the depth first traversal, children are told about tags by the parent
data HandleTag = NoTag             -- No tag at this boundary
               | Advice Tag        -- tag at this boundary, applied at higher level in tree
               | Apply Tag         -- tag at this boundary, may be applied at this node or passed to one child
                 deriving (Show)

-- Nodes in the tree are labeled by the type kind of continuation they
-- prefer to be passed when processing.  This makes it possible to
-- create a smaller number of QNFA states and avoid creating wasteful
-- QNFA states that won't be reachable in the final automata.
data Wanted = WantsQNFA | WantsQT | WantsBoth | WantsEither deriving (Eq,Show)

instance Show Q where
  show = showQ

showQ :: Q -> String
showQ q = "Q { nullQ = "++show (nullQ q)++
        "\n  , takes = "++show (takes q)++
        "\n  , preReset = "++show (preReset q)++
        "\n  , preTag = "++show (preTag q)++
        "\n  , postTag = "++show (postTag q)++
        "\n  , tagged = "++show (tagged q)++
        "\n  , wants = "++show (wants q)++
        "\n  , unQ = "++ indent (unQ q)++" }"
   where indent = unlines . (\(h:t) -> h : (map (spaces ++) t)) . lines . show
         spaces = replicate 10 ' '

-- Smart constructors for NullView
notNull :: NullView
notNull = []

emptyNull :: WinTags -> NullView
emptyNull tags = (mempty, tags) : []

testNull :: TestInfo -> WinTags -> NullView
testNull (w,d) tags = (SetTestInfo (Map.singleton w (Set.singleton d)), tags) : []

-- The NullViews are ordered, and later test sets that contain the
-- tests from any earlier entry will never be chosen.  This function
-- returns a list with these redundant elements removed.  Note that
-- the first unconditional entry in the list will be the last entry of
-- the returned list since the empty set is a subset of any other set.
cleanNullView :: NullView -> NullView
cleanNullView [] = []
cleanNullView (first@(SetTestInfo sti,_):rest) | Map.null sti = first : []  -- optimization
                                               | otherwise =
  first : cleanNullView (filter (not . (setTI `Set.isSubsetOf`) . Map.keysSet . getTests . fst) rest)
  where setTI = Map.keysSet sti

-- Ordered Sequence of two NullViews: all ordered combinations of tests and tags.
-- Order of <- s1 and <- s2 is deliberately chosen to maintain preference priority
mergeNullViews :: NullView -> NullView -> NullView
mergeNullViews s1 s2 = cleanNullView $ do
  (test1,tag1) <- s1
  (test2,tag2) <- s2
  return (mappend test1 test2,mappend tag1 tag2)
-- mergeNullViews = cleanNullView $ liftM2 (mappend *** mappend)

-- Prepend tags to nullView
addTagsToNullView :: WinTags -> NullView -> NullView
addTagsToNullView [] nv = nv
addTagsToNullView tags nv= do
  (test,tags') <- nv
  return (test,tags `mappend` tags')

-- For PGroup, need to prepend reset tasks before others in nullView
addResetsToNullView :: [Tag]-> NullView -> NullView
addResetsToNullView resetTags nv = [ (test, prepend tags) | (test,tags) <- nv ]
  where prepend = foldr (\h t -> (h:).t) id . map (\tag->(tag,PreUpdate ResetGroupStopTask)) $ resetTags

-- Concatenated two ranges of number of accepted characters
seqTake :: (Int, Maybe Int) -> (Int, Maybe Int) -> (Int, Maybe Int)
seqTake (x1,y1) (x2,y2) = (x1+x2,liftM2 (+) y1 y2)

-- Parallel combination of list of ranges of number of accepted characters
orTakes :: [(Int, Maybe Int)] -> (Int,Maybe Int)
orTakes [] = (0,Just 0)
orTakes ts = let (xs,ys) = unzip ts
             in (minimum xs, foldl1 (liftM2 max) ys)

-- Invariant: apply (toAdvice _ ) == mempty
apply :: HandleTag -> Maybe Tag
apply (Apply tag) = Just tag
apply _ = Nothing
toAdvice :: HandleTag -> HandleTag
toAdvice (Apply tag) = Advice tag
toAdvice s = s
noTag :: HandleTag -> Bool
noTag NoTag = True
noTag _ = False
fromHandleTag :: HandleTag -> Tag
fromHandleTag (Apply tag) = tag
fromHandleTag (Advice tag) = tag
fromHandleTag _ = error "fromHandleTag"

-- Shorthand for combining a preTag and a postTag
winTags :: Maybe Tag -> Maybe Tag -> WinTags
winTags (Just a) (Just b) = [(a,PreUpdate TagTask),(b,PreUpdate TagTask)]
winTags (Just a) Nothing  = [(a,PreUpdate TagTask)]
winTags Nothing  (Just b) = [(b,PreUpdate TagTask)]
winTags Nothing  Nothing  = mempty

-- Predicates on the range of number of accepted  characters
varies :: Q -> Bool
varies Q {takes = (_,Nothing)} = True
varies Q {takes = (x,Just y)} = x/=y

mustAccept :: Q -> Bool
mustAccept q = (0/=) . fst . takes $ q

canAccept :: Q -> Bool
canAccept q = maybe True (0/=) $ snd . takes $ q

cannotAccept :: Q -> Bool
cannotAccept q = maybe False (0==) $ snd . takes $ q

-- This converts then input Pattern to an analyzed Q structure with
-- the tags assigned.
--
-- The analysis is filled in by a depth first search and the tags are
-- created top down and passed to children.  Thus information flows up
-- from the dfs of the children and simultaneously down in the form of
-- pre and post HandleTag data.  This bidirectional flow is handled
-- declaratively by using the MonadFix (i.e. mdo) instance of State.
-- 
-- Invariant: A tag should exist in Q in exactly one place.  This is
-- because PGroup needs to know the tags are around precisely the
-- expression that it wants to record.  If the same tag were in other
-- branches then this would no longer be true.
--
-- This invariant is enforced by each node either taking
-- responsibility (apply) for a passed in / created tag or sending it
-- to exactly one child node.  Other child nodes need to receive it
-- via toAdvice.
--
-- There is a final "qwin of Q {postTag=ISet.singleton 1}" and an
-- implied initial index tag of 0.
-- 
-- favoring pushing Apply into the child postTag makes PGroup happier

type PM = RWS (Maybe GroupIndex) [Either Tag GroupInfo] ([OP]->[OP],Tag) 
type HHQ = HandleTag  -- m1 : info about left boundaary / preTag
        -> HandleTag  -- m2 : info about right boundary / postTag
        -> PM Q

-- There is no group 0 here, since it is always the whole match and has no parent of its own
makeGroupArray :: GroupIndex -> [GroupInfo] -> Array GroupIndex [GroupInfo]
makeGroupArray maxGroupIndex groups = accumArray (\earlier later -> later:earlier) [] (1,maxGroupIndex) filler
    where filler = map (\gi -> (thisIndex gi,gi)) groups

fromRight :: [Either Tag GroupInfo] -> [GroupInfo]
fromRight [] = []
fromRight ((Right x):xs) = x:fromRight xs
fromRight ((Left _):xs) = fromRight xs

partitionEither :: [Either Tag GroupInfo] -> ([Tag],[GroupInfo])
partitionEither = helper id id where
  helper :: ([Tag]->[Tag]) -> ([GroupInfo]->[GroupInfo]) -> [Either Tag GroupInfo] -> ([Tag],[GroupInfo])
  helper ls rs [] = (ls [],rs [])
  helper ls rs ((Right x):xs) = helper  ls      (rs.(x:)) xs
  helper ls rs ((Left  x):xs) = helper (ls.(x:)) rs       xs

-- Partial function: assumes starTrans has been run on the Pattern
patternToQ :: CompOption -> (Pattern,(GroupIndex,DoPa)) -> (Q,Array Tag OP,Array GroupIndex [GroupInfo])
patternToQ compOpt (pOrig,(maxGroupIndex,_)) = (tnfa,aTags,aGroups) where
  (tnfa,(tag_dlist,nextTag),groups) = runRWS monad startReader startState
  aTags = listArray (0,pred nextTag) (tag_dlist [])
  aGroups = makeGroupArray maxGroupIndex (fromRight groups)

  -- implicitly inside a PGroup 0 converted into a GroupInfo 0 undefined 0 1
  monad = go (starTrans pOrig) (Advice 0) (Advice 1)
  startReader :: Maybe GroupIndex
  startReader = Just 0                           -- start inside group 0, capturing enabled
  startState :: ([OP]->[OP],Tag)
  startState = ( (Minimize:) . (Maximize:) , 2)  -- Tag 0 is Minimized and Tag 1 is maximized.

  -- Specialize the monad operations and give more meaningful names
  makeOrbit :: PM (Maybe Tag)
  makeOrbit = do Apply x <- uniq Orbit
                 tell [Left x]
                 return (Just x)

  withOrbit :: PM a -> PM (a,[Tag])
  withOrbit = listens childStars
    where childStars x = let (ts,_) = partitionEither x in ts

  getParentIndex :: PM (Maybe GroupIndex)
  getParentIndex = ask

  makeGroup :: GroupInfo -> PM ()
  makeGroup = tell . (:[]) . Right

  nonCapture :: PM  a -> PM a
  nonCapture = local (const Nothing)

  withParent :: GroupIndex -> PM a -> PM (a,[Tag])
  withParent this = local (const (Just this)) . listens childGroupInfo
    where childGroupInfo x =
            let (_,gs) = partitionEither x
                children :: [GroupIndex]
                children = norep . sort . map thisIndex
                           -- filter to get only immediate children (efficiency)
                           . filter ((this==).parentIndex) $ gs
            in concatMap (map stopTag . (aGroups!)) (this:children)

  uniq :: OP -> PM HandleTag
  uniq newOp = do (op,s) <- get                -- generate the next tag with bias newOp
                  let op' = op . (newOp:)
                      s' = succ s
                  put $! debug ("\n"++show (s,newOp)++"\n") (op',s')
                  return (Apply s) -- someone will need to apply it

  -- Partial function: Must not pass in an empty list
  -- Policy choices:
  --  * pass tags to apply to children and have no preTag or postTag here (so none addded to nullQ)
  --  * middle 'mid' tag: give to left/front child as postTag so a Group there might claims as stopTag
  --  * if parent is Group then preReset will become non-empty
  combineConcat :: [Pattern] -> HHQ
  combineConcat | rightAssoc compOpt = (\ps -> foldr combineSeq (go (last ps)) (map go $ init ps))
                | otherwise          = (\ps -> foldl combineSeq (go (head ps)) (map go $ tail ps)) -- libtre default
    where combineSeq :: HHQ -> HHQ -> HHQ
          combineSeq pFront pEnd = (\ m1 m2 -> mdo
            let bothVary = varies qFront && varies qEnd
            a <- if noTag m1 && bothVary then uniq Minimize else return m1
            b <- if noTag m2 && bothVary then uniq Maximize else return m2
            mid <- case (noTag a,canAccept qFront,noTag b,canAccept qEnd) of
                     (False,False,_,_) -> return (toAdvice a)
                     (_,_,False,False) -> return (toAdvice b)
                     _ -> if tagged qFront || tagged qEnd then uniq Maximize else return NoTag
            qFront <- pFront a mid
            qEnd <- pEnd (toAdvice mid) b
            let wanted = if WantsEither == wants qEnd then wants qFront else wants qEnd
            return $ Q (mergeNullViews (nullQ qFront) (nullQ qEnd))
                       (seqTake (takes qFront) (takes qEnd))
                       [] Nothing Nothing
                       bothVary (childGroups qFront || childGroups qEnd) wanted
                       (Seq qFront qEnd)
                                   )
  go :: Pattern -> HHQ
  go pIn m1 m2 =
    let die = error $ "patternToQ cannot handle "++show pIn
        nil = return $ Q {nullQ=emptyNull (winTags (apply m1) (apply m2))
                         ,takes=(0,Just 0)
                         ,preReset=[],preTag=apply m1,postTag=apply m2
                         ,tagged=False,childGroups=False,wants=WantsEither
                         ,unQ=Empty}
        one = return $ Q {nullQ=notNull
                         ,takes=(1,Just 1)
                         ,preReset=[],preTag=apply m1,postTag=apply m2
                         ,tagged=False,childGroups=False,wants=WantsQNFA
                         ,unQ = OneChar pIn}
        test myTest = return $ Q {nullQ=testNull myTest (winTags (apply m1) (apply m2))
                                 ,takes=(0,Just 0)
                                 ,preReset=[],preTag=apply m1,postTag=apply m2
                                 ,tagged=False,childGroups=False,wants=WantsQT
                                 ,unQ=Test myTest }
    in case pIn of
         PEmpty -> nil
         POr [] -> nil
         POr [p] -> go p m1 m2
         POr ps -> mdo
           -- Exasperation: This POr recursive mdo is very easy to make loop and lockup the program
           let canVary = varies ans || childGroups ans -- childGroups detects that "abc|a(b)c" needs tags
           a <- if noTag m1 && canVary then uniq Minimize else return m1
           b <- if noTag m2 && canVary then uniq Maximize else return m2
           let aAdvice = toAdvice a
               bAdvice = toAdvice b
               -- Due to the recursive-do, it seems that I have to put the if canVary into the op'
               op' = if canVary then uniq Maximize else return bAdvice
           -- Preference for last branch is implicit: do not need op' to create uniq tag:
           cs <- fmap (++[bAdvice]) $ replicateM (pred $ length ps) op'
           qs <- mapM (\(p,c) -> go p aAdvice c) (zip ps cs)
           let wqs = map wants qs
               wanted = if any (WantsBoth==) wqs then WantsBoth
                          else case (any (WantsQNFA==) wqs,any (WantsQT==) wqs) of
                                 (True,True) -> WantsBoth
                                 (True,False) -> WantsQNFA
                                 (False,True) -> WantsQT
                                 (False,False) -> WantsEither
               nullView = addTagsToNullView (winTags (apply a) (apply b)) . cleanNullView . concatMap nullQ $ qs
               -- The nullView computed above takes the nullQ of the
               -- branches and combines them.  This assumes that the
               -- pre/post tags of the children are also part of the
               -- nullQ values.  So for consistency, POr must then add
               -- its own pre/post tags to its nullQ value.
           let ans = Q nullView
                       (orTakes . map takes $ qs)
                       [] (apply a) (apply b)
                       canVary (any childGroups qs) wanted
                       (Or qs)
           return ans
         PConcat [] -> nil -- fatal to pass [] to combineConcat
         PConcat ps -> combineConcat ps m1 m2
         PStar mayFirstBeNull p -> mdo
           let accepts    = canAccept q
               needsOrbit = varies q && childGroups q  -- otherwise it cannot matter or be observed which path is taken
               needsTags  = needsOrbit || accepts      -- important that needsOrbit implies needsTags
           a <- if noTag m1 && needsTags then uniq Minimize else return m1
           b <- if noTag m2 && needsTags then uniq Maximize else return m2
           c <- if needsOrbit then makeOrbit else return Nothing -- any Orbit tag is created after the pre and post tags
           let nullView | mayFirstBeNull = cleanNullView $ addTagsToNullView (winTags (apply a) (apply b)) (nullQ q) ++ skipView
                        | otherwise = skipView
                 where skipView = emptyNull (winTags (apply a) (apply b))
                       
           (q,resetTags) <- withOrbit (go p NoTag NoTag)
-- 2009-02-09 eliminate because this breaks (()*)* and ((.?)*)*
--           let nullView = emptyNull (winTags (apply a) (apply b)) -- chosen to represent skipping sub-pattern
           return $ Q nullView
                      (0,if accepts then Nothing else (Just 0))
                      [] (apply a) (apply b)
                      needsTags (childGroups q) WantsQT
                      (Star c resetTags mayFirstBeNull q)
         PCarat dopa -> test (Test_BOL,dopa)
         PDollar dopa -> test (Test_EOL,dopa)
         PChar {} -> one
         PDot {} -> one
         PAny {} -> one
         PAnyNot {} -> one
         PEscape {} -> one

         -- A PGroup node in the Pattern tree does not become a node
         -- in the Q/P tree. A PGroup can share and pass along a
         -- preTag (with Advice) with other branches, but will pass
         -- down an Apply postTag.
         --
         -- If the parent index is Nothing then this is part of a
         -- non-capturing subtree and ignored.
         PGroup Nothing p -> go p m1 m2
         PGroup (Just this) p -> do
           mParent <- getParentIndex
           case mParent of
             Nothing -> go p m1 m2
             Just parent -> do
               a <- if noTag m1 then uniq Minimize else return m1
               b <- if isNothing (apply m2) then uniq Maximize else return m2
               (q,resetTags) <- withParent this (go p a b)
               makeGroup (GroupInfo this parent (fromHandleTag a) (fromHandleTag b))
               return $ q { nullQ = addResetsToNullView resetTags (nullQ q)
                          , tagged = True
                          , childGroups = True
                          , preReset = resetTags `mappend` (preReset q) }

         -- A PNonCapture node in the Pattern tree does not become a
         -- node in the Q/P tree.  It sets the parent to Nothing while
         -- processing the sub-tree.
         PNonCapture p -> nonCapture (go p m1 m2)

         -- PNonEmpty means the child pattern p can be skipped by
         -- bypassing the pattern.  This is only used in the case p
         -- can accept 0 and can accept more than zero characters
         -- (thus the assertions, enforcted by CorePattern.starTrans).  The important thing about this case
         -- is intercept the "accept 0" possibility and replace with
         -- "skip".
         PNonEmpty p -> mdo
           let needsTags = canAccept q
           a <- if noTag m1 && needsTags then uniq Minimize else return m1
           b <- if noTag m2 && needsTags then uniq Maximize else return m2
           q <- go p (toAdvice a) (toAdvice b)
           when (not needsTags) (err $ "PNonEmpty could not accept characters: "++show (p,pOrig))
           when (mustAccept q) (err $ "patternToQ : PNonEmpty provided with a *mustAccept* pattern: "++show (p,pOrig))
           return $ Q (emptyNull (winTags (apply a) (apply b)))   -- The magic of NonEmpty
                      (0,snd (takes q))                           -- like Or
                      [] (apply a) (apply b)                      -- own the closing tag so it will not end a PGroup
                      needsTags (childGroups q) (wants q)         -- the test case is "x" =~ "(.|$){1,3}"
                      (NonEmpty q)

         -- these are here for completeness of the case branches, currently starTrans replaces them all
         PPlus {} -> die
         PQuest {} -> die
         PBound {} -> die
