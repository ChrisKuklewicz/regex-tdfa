module Text.Regex.TDFA.CorePattern(Q(..),P(..),WhichTest(..),Wanted(..),TestInfo,OP(..),SetTestInfo(..),NullView
                                  ,patternToQ,cleanNullView,cannotAccept) where

import Control.Monad.Fix()
import Control.Monad.RWS
import Text.Regex.TDFA.Common -- (Tag,GroupInfo(..),PatternIndex,Position,WinTags,DoPa,CompOption(..),mapSnd)
import Text.Regex.TDFA.Pattern(Pattern(..),starTrans)

import Data.Maybe
import Data.List
import Data.Array.IArray
import Data.Set(Set)
import qualified Data.Set as Set
-- import Data.IntSet(IntSet)
-- import qualified Data.IntSet as ISet
import Data.Map(Map)
import qualified Data.Map as Map
-- import Data.IntMap(IntMap)
-- import qualified Data.IntMap as IMap

-- import Debug.Trace

debug :: (Show a) => a -> b -> b
debug _ = id

-- Core Pattern Language
data P = Empty
       | Or [Q]
       | Seq Q Q
       | Star { getOrbit :: Maybe Tag  -- tag to prioritize the need to keep track of length of each pass though q
              , reset :: [Tag]         -- need to reset the contained groups by resetting these closing tags
              , unStar :: Q}
       | Test TestInfo
       | OneChar Pattern
         deriving (Show,Eq)

-- The diagnostics about the pattern
data Q = Q {nullQ :: NullView         -- Ordered list of nullable views
           ,takes :: (Position,Maybe Position)  -- Range of number of accepted characters
           ,preTag,postTag :: Maybe Tag -- Tags assigned around this pattern
           ,tagged :: Bool            -- Whether this node should be tagged (XXX only used in patternToQ/PStar)
           ,childGroups :: Bool       -- Whether unQ has any PGroups (XXX only used in patternToQ/POr)
           ,wants :: Wanted           -- What kind of continuation is used by this pattern
           ,unQ :: P} deriving (Eq)

type TestInfo = (WhichTest,DoPa)

-- This is newtype'd to allow for a custom Ord instance
-- This is a set of WhichTest where each test has associated pattern location information
newtype SetTestInfo = SetTestInfo {getTests :: Map WhichTest (Set DoPa)} deriving (Eq,Monoid)


-- During the depth first traversal, children are told about bounds by the parent
data HandleTag = NoTag | Advice Tag | Apply Tag deriving (Show)

type NullView = [(SetTestInfo,WinTags)]  -- Ordered list of null views, each is a set of tests and tags

data Wanted = WantsQNFA | WantsQT | WantsBoth | WantsEither deriving (Eq,Show)

instance Show Q where
  show = showQ

instance Show SetTestInfo where
  show (SetTestInfo sti) = "SetTestInfo "++show (mapSnd (Set.toList) $ Map.assocs sti)


-- Smart constructors for NullView
notNull :: NullView
notNull = []

emptyNull :: WinTags -> NullView
emptyNull tags = (mempty, tags) : []

testNull :: TestInfo -> WinTags -> NullView
testNull (w,d) tags = (SetTestInfo (Map.singleton w (Set.singleton d)), tags) : []

cleanNullView :: NullView -> NullView
cleanNullView [] = []
cleanNullView (first@(SetTestInfo sti,_):rest) | Map.null sti = first : []
                                               | otherwise =
  first : cleanNullView (filter (not . (setTI `Set.isSubsetOf`) . Map.keysSet . getTests . fst) rest)
  where setTI = Map.keysSet sti

mergeNullViews :: NullView -> NullView -> NullView
-- mergeNullViews = liftM2 (mappend *** mappend)
mergeNullViews s1 s2 = do
  (test1,tag1) <- s1
  (test2,tag2) <- s2
  return (mappend test1 test2,mappend tag1 tag2)

addTagsToNullView :: WinTags -> NullView -> NullView
addTagsToNullView [] nv = nv
addTagsToNullView tags nv= do
  (test,tags') <- nv
  return (test,tags `mappend` tags')

seqTake :: (Int, Maybe Int) -> (Int, Maybe Int) -> (Int, Maybe Int)
seqTake (x1,y1) (x2,y2) = (x1+x2,liftM2 (+) y1 y2)

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

winTags :: Maybe Tag -> Maybe Tag -> WinTags
winTags (Just a) (Just b) = [(b,PreUpdate TagTask),(a,PreUpdate TagTask)]
winTags (Just a) Nothing  = [(a,PreUpdate TagTask)]
winTags Nothing  (Just b) = [(b,PreUpdate TagTask)]
winTags Nothing  Nothing  = mempty

showQ :: Q -> String
showQ q = "Q { nullQ = "++show (nullQ q)++
        "\n  , takes = "++show (takes q)++
        "\n  , preTag = "++show (preTag q)++
        "\n  , postTag = "++show (postTag q)++
        "\n  , tagged = "++show (tagged q)++
        "\n  , wants = "++show (wants q)++
        "\n  , unQ = "++ indent (unQ q)++" }"
   where indent = unlines . (\(h:t) -> h : (map (spaces ++) t)) . lines . show
         spaces = replicate 10 ' '

varies :: Q -> Bool
varies Q {takes = (_,Nothing)} = True
varies Q {takes = (x,Just y)} = x/=y

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

{-
getChildTags :: Q -> [Tag]
getChildTags q = maybe id (:) (preTag q) $ maybe (childTags q) (:childTags q) (postTag q)
-}

type PM = RWS PatternIndex [Either Tag GroupInfo] ([OP]->[OP],Tag) 
type HHQ = HandleTag  -- m1 : info about left or pre-tag, currently NoTag or Advice and never Apply
        -> HandleTag  -- m2 : info about right or post-tag, currently NoTag or Apply, and top-level Advice
        -> PM Q

makeGroupArray :: PatternIndex -> [GroupInfo] -> Array PatternIndex [GroupInfo]
makeGroupArray maxPatternIndex groups = accumArray (\earlier later -> later:earlier) [] (1,maxPatternIndex) filler
    where filler = map (\gi -> (thisIndex gi,gi)) groups

fromRight :: [Either Tag GroupInfo] -> [GroupInfo]
fromRight [] = []
fromRight ((Right x):xs) = x:fromRight xs
fromRight ((Left _):xs) = fromRight xs

partEither :: [Either Tag GroupInfo] -> ([Tag],[GroupInfo])
partEither = helper id id where
  helper :: ([Tag]->[Tag]) -> ([GroupInfo]->[GroupInfo]) -> [Either Tag GroupInfo] -> ([Tag],[GroupInfo])
  helper ls rs [] = (ls [],rs [])
  helper ls rs ((Right x):xs) = helper  ls      (rs.(x:)) xs
  helper ls rs ((Left  x):xs) = helper (ls.(x:)) rs       xs

patternToQ :: CompOption -> (Pattern,(PatternIndex,Int)) -> (Q,Array Tag OP,Array PatternIndex [GroupInfo])
patternToQ compOpt (pOrig,(maxPatternIndex,_)) = (tnfa,aTags,aGroups) where
  (tnfa,(tag_dlist,nextTag),groups) = runRWS monad startReader startState
  aTags = listArray (0,pred nextTag) (tag_dlist [])
  aGroups = makeGroupArray maxPatternIndex (fromRight groups)
  -- implicitly inside a PGroup 0 converted into a GroupInfo 0 undefined 0 1
  monad = go (starTrans pOrig) (Advice 0) (Apply 1)
  startReader :: PatternIndex
  startReader = 0             -- start inside group 0
  startState :: ([OP]->[OP],Tag)
  startState = ( (Minimize:) . (Maximize:) , 2)  -- Tag 0 is Minimized and Tag 1 is maximized.
  -- Give the operations more meaningful names
  getParentIndex = ask
  withParent i = local (const i)
  makeGroup = tell . (:[]) . Right
  makeOrbit = do Apply x <- uniq Orbit
                 tell [Left x]
                 return (Just x)
  hearResets x = let (ts,gs) = partEither x
                     gs' = norep . sort .  map thisIndex $ gs
                 in ts ++ concatMap (map stopTag . (aGroups!)) gs'

  uniq newOp = do (op,s) <- get                -- generate the next tag with bias newOp
                  let op' = op . (newOp:)
                      s' = succ s
                  put $! debug ("\n"++show (s,newOp)++"\n") (op',s')
                  return (Apply s) -- someone will need to apply it

  combineConcat ps m1 m2 =
    if rightAssoc compOpt
      then foldr combineRight (go (last ps)) (init ps) m1 m2
    else foldl combineLeft  (go (head ps)) (tail ps) m1 m2    -- libtre default

  combineRight :: Pattern -> HHQ -> HHQ
  combineRight cFront pEnd m1 m2 = mdo
    let bothVary = varies qFront && varies qEnd
    a <- if noTag m1 && bothVary then uniq Minimize else return m1 -- Why was this Maximize?
    b <- if noTag m2 && bothVary then uniq Maximize else return m2
    mid <- case (noTag a,canAccept qFront,noTag b,canAccept qEnd) of
             (False,False,_,_) -> return (toAdvice a)
             (_,_,False,False) -> return (toAdvice b)
             _ -> if tagged qFront || tagged qEnd then uniq Maximize else return NoTag
    qFront <- go cFront a mid
    qEnd <- pEnd (toAdvice mid) b
    let wanted = if WantsEither == wants qEnd then wants qFront else wants qEnd
    return $ Q (mergeNullViews (nullQ qFront) (nullQ qEnd))
               (seqTake (takes qFront) (takes qEnd))
               Nothing Nothing
               bothVary (childGroups qFront || childGroups qEnd) wanted
               (Seq qFront qEnd)

  combineLeft :: HHQ -> Pattern -> HHQ
  combineLeft cFront pEnd m1 m2 = mdo
    let bothVary = varies qFront && varies qEnd
    a <- if noTag m1 && bothVary then uniq Minimize else return m1 -- Why was this Maximize?
    b <- if noTag m2 && bothVary then uniq Maximize else return m2
    mid <- case (noTag a, canAccept qFront, noTag b, canAccept qEnd) of
             (False,False,_    ,_    ) -> return (toAdvice a)
             (_    ,_    ,False,False) -> return (toAdvice b)
             _ -> if tagged qFront || tagged qEnd then uniq Maximize else return NoTag
    qFront <- cFront a mid
    qEnd <- go pEnd (toAdvice mid) b
    let wanted = if WantsEither == wants qEnd then wants qFront else wants qEnd
    return $ Q (mergeNullViews (nullQ qFront) (nullQ qEnd))
               (seqTake (takes qFront) (takes qEnd))
               Nothing Nothing
               bothVary (childGroups qFront || childGroups qEnd) wanted
               (Seq qFront qEnd)

  go :: Pattern -> HHQ
  go pIn m1 m2 =
    let die = error $ "patternToQ cannot handle "++show pIn -- assume (starTrans) has been run already
        nil = return $ Q {nullQ=emptyNull (apply m1 `winTags` apply m2) -- No tests, just empty
                         ,takes=(0,Just 0)
                         ,preTag=apply m1
                         ,postTag=apply m2
                         ,tagged=False
                         ,childGroups=False
                         ,wants=WantsEither
                         ,unQ=Empty}
        one = return $ Q {nullQ=notNull              -- one character accepted (just store P type)
                         ,takes=(1,Just 1)
                         ,preTag=apply m1
                         ,postTag=apply m2
                         ,tagged=False
                         ,childGroups=False
                         ,wants=WantsQNFA
                         ,unQ = OneChar pIn}
        test myTest = return $ Q {nullQ=testNull myTest (apply m1 `winTags` apply m2) -- anchor of some sort
                                 ,takes=(0,Just 0)
                                 ,preTag=apply m1
                                 ,postTag=apply m2
                                 ,tagged=False
                                 ,childGroups=False
                                 ,wants=WantsQT
                                 ,unQ=Test myTest }

    in case pIn of
         PEmpty -> nil
         POr [] -> nil
         POr [p] -> go p m1 m2
         POr ps -> mdo
           let canVary = varies ans || childGroups ans -- how to detect "abc|a(b)c" ? need childGroups
           a <- if noTag m1 && canVary then uniq Minimize else return m1
           b <- if noTag m2 && canVary then uniq Maximize else return m2
           let aAdvice = toAdvice a
               bAdvice = toAdvice b
               -- Due to the recursive-do, it seems that I have to put the if canVary into the op'
               op' = if canVary then uniq Maximize else return bAdvice
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
           let ans = Q nullView
                       (orTakes . map takes $ qs)
                       (apply a) (apply b)
                       canVary (any childGroups qs) wanted
                       (Or qs)
           return ans
         PConcat [] -> nil
         PConcat ps -> combineConcat ps m1 m2 -- unsafe to pass [] to combineConcat
         PStar p -> do
           q1 <- mdo
             let accepts = canAccept q
             a <- if noTag m1 && accepts then uniq Minimize else return m1
             b <- if noTag m2 && accepts then uniq Maximize else return m2
             c <- if varies q || childGroups q then makeOrbit else return Nothing
             -- end of each iteration is Maximize and is the beginning
             -- of the next so the start of each iteration should be a
             -- Maximize (except the first iteration, but this is taken
             -- care of by 'a' above').  To keep the interior pattern
             -- from making a Minimize tag and breaking the logic, a
             -- Maximize tag is always passed as the first tag
             (q,resetTags) <- listens hearResets $ go p NoTag NoTag
             let nullView = emptyNull (winTags (apply a) (apply b)
--                                       ++ map (\tag -> (tag,PreUpdate ResetTask)) resetGroups
--                                       ++ maybe [] (\tag -> [(tag,PreUpdate LeaveOrbitTask)]) (apply c)
                                      )
             let ans = Q nullView
                         (0,if accepts then Nothing else (Just 0))
                         (apply a) (apply b)
                         accepts (childGroups q) WantsQT
                         (Star c (norep . sort $ resetTags) q)
             return ans
           return q1
         PCarat dopa -> test (Test_BOL,dopa)
         PDollar dopa -> test (Test_EOL,dopa)
         PChar {} -> one
         PDot {} -> one
         PAny {} -> one
         PAnyNot {} -> one
         PEscape {} -> one

         -- a PGroup can share a preTag (with Advice) with other branches but must have a postTag of Apply
         PGroup this p -> do
           a <- if noTag m1 then uniq Minimize else return m1
           b <- if isNothing (apply m2) then uniq Maximize else return m2
           parent <- getParentIndex
           makeGroup (GroupInfo this parent (fromHandleTag a) (fromHandleTag b))
           q <- withParent this $ go p a b
           let ans = q {tagged = True,childGroups = True}
           return ans

         -- these are here for completeness of the case branches, currently starTrans replaces them all
         PPlus {} -> die
         PQuest {} -> die
         PBound {} -> die
