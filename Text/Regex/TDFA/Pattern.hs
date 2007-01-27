-- | This "Text.Regex.TDFA.Pattern" module provides the 'Pattern' data
-- type and its subtypes.  This 'Pattern' type is used to represent
-- the parsed form of a Regular Expression and is syntax independent.
--
-- It is possible to construct values of 'Pattern' that are invalid
-- regular expressions.
--
-- There are also several 
module Text.Regex.TDFA.Pattern
    (Pattern(..)
    ,PatternSet(..)
    ,PatternSetCharacterClass(..)
    ,PatternSetCollatingElement(..)
    ,PatternSetEquivalenceClass(..)
    ,GroupIndex
    ,DoPa(..)
    ,showPattern
    ,starTrans
    ,simplify'
    ,dfsPattern
    ,starTrans'
    ) where

{- By Chris Kuklewicz, 2006. BSD License, see the LICENSE file. -}

import Data.List(intersperse,partition)
import qualified Data.Set as Set(toAscList,toList)
import Data.Set(Set)
import Text.Regex.TDFA.Common(DoPa(..),GroupIndex)

-- | GroupIndex is for indexing submatches from  parenthesized groups (PGroup)
-- type GroupIndex = Int

data Pattern = PEmpty
             | PGroup  (Maybe GroupIndex) Pattern -- Nothing to indicate non matching
             | POr     [Pattern]
             | PConcat [Pattern]
             | PQuest  Pattern
             | PPlus   Pattern
             | PStar   Bool Pattern
             | PBound  Int (Maybe Int) Pattern
             -- The rest of these need an index of where in the regex string it is from
             | PCarat  {getDoPa::DoPa}
             | PDollar {getDoPa::DoPa}
             -- The following test and accept a single character
             | PDot    {getDoPa::DoPa}            -- Any character (newline?) at all
             | PAny    {getDoPa::DoPa,getPatternSet::PatternSet} -- Square bracketed things
             | PAnyNot {getDoPa::DoPa,getPatternSet::PatternSet} -- Inverted square bracketed things
             | PEscape {getDoPa::DoPa,getPatternChar::Char}       -- Backslashed Character
             | PChar   {getDoPa::DoPa,getPatternChar::Char}       -- Specific Character
             -- The following are semantic tags created in starTrans
             | PNonCapture Pattern
             | PNonEmpty Pattern
               deriving (Eq,Show)

showPattern :: Pattern -> String
showPattern pIn =
  case pIn of
    PEmpty -> "()"
    PGroup _ p -> paren (showPattern p)
    POr ps -> concat $ intersperse "|" (map showPattern ps)
    PConcat ps -> concatMap showPattern ps
    PQuest p -> (showPattern p)++"?"
    PPlus p -> (showPattern p)++"+"
    PStar _ p -> (showPattern p)++"*"
    PBound i (Just j) p | i==j -> showPattern p ++ ('{':show i)++"}"
    PBound i mj p -> showPattern p ++ ('{':show i) ++ maybe ",}" (\j -> ',':show j++"}") mj
    --
    PCarat _ -> "^"
    PDollar _ -> "$"
    PDot _ -> "."
    PAny _ ps -> ('[':show ps)++"]"
    PAnyNot _ ps ->  ('[':'^':show ps)++"]"
    PEscape _ c -> '\\':c:[]
    PChar _ c -> [c]
  where groupRange x n (y:ys) = if (fromEnum y)-(fromEnum x) == n then groupRange x (succ n) ys
                                else (if n <=3 then take n [x..]
                                      else x:'-':(toEnum (pred n+fromEnum x)):[]) ++ groupRange y 1 ys
        groupRange x n [] = if n <=3 then take n [x..]
                            else x:'-':(toEnum (pred n+fromEnum x)):[]
        paren s = ('(':s)++")"
       
data PatternSet = PatternSet (Maybe (Set Char))
                             (Maybe (Set PatternSetCharacterClass))
                             (Maybe (Set PatternSetCollatingElement))
                             (Maybe (Set PatternSetEquivalenceClass))
                             deriving (Eq)

instance Show PatternSet where
  showsPrec i (PatternSet s scc sce sec) =
    let (special,normal) = maybe ("","") ((partition (`elem` "]-")) . Set.toAscList) s
        charSpec = (if ']' `elem` special then (']':) else id) (byRange normal)
        scc' = maybe "" ((concatMap show) . Set.toList) scc
        sce' = maybe "" ((concatMap show) . Set.toList) sce
        sec' = maybe "" ((concatMap show) . Set.toList) sec
    in shows charSpec
       . showsPrec i scc' . showsPrec i sce' . showsPrec i sec'
       . if '-' `elem` special then showChar '-' else id
    where byRange xAll@(x:xs) | length xAll <=3 = xAll
                              | otherwise = groupRange x 1 xs
          byRange _ = undefined
          groupRange x n (y:ys) = if (fromEnum y)-(fromEnum x) == n then groupRange x (succ n) ys
                                  else (if n <=3 then take n [x..]
                                        else x:'-':(toEnum (pred n+fromEnum x)):[]) ++ groupRange y 1 ys
          groupRange x n [] = if n <=3 then take n [x..]
                              else x:'-':(toEnum (pred n+fromEnum x)):[]

newtype PatternSetCharacterClass   = PatternSetCharacterClass   {unSCC::String}
  deriving (Eq,Ord)
newtype PatternSetCollatingElement = PatternSetCollatingElement {unSCE::String}
  deriving (Eq,Ord)
newtype PatternSetEquivalenceClass = PatternSetEquivalenceClass {unSEC::String}
  deriving (Eq,Ord)

instance Show PatternSetCharacterClass where
  showsPrec _ p = showChar '[' . showChar ':' . shows (unSCC p) . showChar ':' . showChar ']'
instance Show PatternSetCollatingElement where
  showsPrec _ p = showChar '[' . showChar '.' . shows (unSCE p) . showChar '.' . showChar ']'
instance Show PatternSetEquivalenceClass where
  showsPrec _ p = showChar '[' . showChar '=' . shows (unSEC p) . showChar '=' . showChar ']'


-- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == 

-- We don't need Transitions.hs since I copied the modified starTrans code here

-- Do the transformation and simplification in a single traversal
starTrans :: Pattern -> Pattern
starTrans = dfsPattern (simplify' . starTrans')

-- | Apply a Pattern transfomation function depth first
dfsPattern :: (Pattern -> Pattern)  -- ^ The transformation function
           -> Pattern               -- ^ The Pattern to transform
           -> Pattern               -- ^ The transformed Pattern
dfsPattern f = dfs
 where unary c = f . c . dfs
       dfs pattern = case pattern of
                       POr ps -> f (POr (map dfs ps))
                       PConcat ps -> f (PConcat (map dfs ps))
                       PGroup i p -> unary (PGroup i) p
                       PQuest p -> unary PQuest p
                       PPlus p -> unary PPlus p
                       PStar i p -> unary (PStar i) p
                       PBound i mi p -> unary (PBound i mi) p
                       _ -> f pattern

unCapture = dfsPattern unCapture' where
  unCapture' (PGroup (Just _) p) = PGroup Nothing p
  unCapture' x = x

starTrans' :: Pattern -> Pattern
starTrans' pIn =
  case pIn of -- We know that "p" has been simplified in each of these cases:
    PQuest p -> POr [p,PEmpty]

{- The PStar should not capture 0 characters on its first iteration,
   so set its flag to False
 -}
    PPlus  p -> PConcat [p,simplify' $ PStar False p]

{- "An ERE matching a single character repeated by an '*' , '?' , or
   an interval expression shall not match a null expression unless
   this is the only match for the repetition or it is necessary to
   satisfy the exact or minimum number of occurrences for the interval
   expression."
 -}
-- Easy cases
    PBound i _        _ | i<0 -> PEmpty  -- malformed
    PBound i (Just j) _ | i>j -> PEmpty  -- malformed
    PBound _ (Just 0) _ -> PEmpty
    PBound 0 Nothing  p -> PStar True p
    PBound 0 (Just 1) p -> POr [p,PEmpty]

{- The iterations before the last required one cannot determine the
   group capture so change the PGroups to False or wrap in PNonCapture.
-}
    PBound i Nothing  p -> PConcat $ apply (p':) (pred i) [p,simplify' $ PStar False p]
      where p' = unCapture p -- XXX cleanup

{- p{0,2} is (pp?)? is p?p? and p{0,3} is (p(pp?)?)? is p?p?p?
   p{1,2} is pp{0,1} is pp?
   p{2,5} is ppp{0,3} is pp(p(pp?)?)? is ppp?p?p?

   But this is not always right.  Because if the second use of p in
   p?p? matches 0 characters then the perhaps non 0 character match of
   the first p is overwritten.

   We need a new operation "p!" that means "p?" unless "p" match 0
   characters, in which case skip p as if it failed in "p?"

   Call this (PNonEmpty p) in the Pattern type.  Note that if p cannot
   match 0 characters then p! is equivalent to p?
   The p{0,1} is still always p?
   Now p{0,2} means p?p! or (pp!)? and p{0,3} means (p(pp!)!)? or p?p!p!
   Equivalently p?p! and p?p!p!
   And p{2,2} is p'p and p{3,3} is p'p'p and p{4} is p'p'p'p
   The p{1,2} is pp! and p{1,3} is pp!p! or p(pp!)!
   And p{2,4} means p'pp!p! and p{3,6} is p'p'pp!p!p! or p'p'p(p(pp!)!)!

   But this second form still has a problem: the (pp!)! can have the first
   p match 0 and the second p match non-zero. This showed up for (.|$){1,3}
   since ($.!)! should not be a valid path but altered the qt_win commands.

   Thus only p'p'pp!p!p! has the right semantics.

   And by this logic, the PStar False is really p*!  So p{0,} is p*
   and p{1,} is pp*! and p{2,} is p'pp*! and p{3,} is p'p'pp*!
-}
    PBound 0 (Just j) p | cannotMatchNull p -> apply (quest' . (concat' p)) (pred j) (quest' p)
                        | canOnlyMatchNull p -> quest' p
                        | otherwise -> POr [ simplify' (PConcat (p : replicate (pred j) (nonEmpty' p))) , PEmpty ]
--                        | otherwise -> quest' . concat' p $ apply (nonEmpty' . (concat' p)) (j-2) (nonEmpty' p)
--
    PBound i (Just j) p | i == j    -> PConcat $ apply (p':) (pred i) [p]
                        | cannotMatchNull p -> PConcat $ apply (p':) (pred i) $ (p:) $ 
                                                 [apply (quest' . (concat' p)) (pred (j-i)) (quest' p)]
                        | canOnlyMatchNull p -> p
                        | otherwise -> PConcat $ (replicate (pred i) p') ++ p : (replicate (j-i) (nonEmpty' p))
--                        | otherwise -> PConcat $ apply (p':) (pred i) $ (p:) $
--                                         [apply (nonEmpty' . (concat' p)) (pred (j-i)) (nonEmpty' p)]
      where p' = unCapture p -- XXX cleanup
    -- Left intact
    PEmpty -> pass
    PGroup {} -> pass
    PStar {} -> pass
    POr {} -> pass
    PConcat {} -> pass
    PCarat {} -> pass
    PDollar {} -> pass
    PDot {} -> pass
    PAny {} -> pass
    PAnyNot {} -> pass
    PEscape {} -> pass
    PChar {} -> pass
  where
    quest' = (\p -> simplify' $ POr [p,PEmpty])  -- require p to have been simplified
    concat' a b = simplify' $ PConcat [a,b]      -- require a and b to have been simplified
    nonEmpty' = PNonEmpty
    apply f n x = foldr ($) x (replicate n f)
    pass = pIn

-- | Function to transform a pattern into an equivalent, but less
-- redundant form.  Nested 'POr' and 'PConcat' are flattened.
simplify' :: Pattern -> Pattern
simplify' x@(POr _) = 
  let ps' = case span notPEmpty (flatten x) of
              (notEmpty,[]) -> notEmpty
              (notEmpty,_:rest) -> notEmpty ++ (PEmpty:filter notPEmpty rest) -- keep 1st PEmpty only
  in case ps' of
       [] -> PEmpty
       [p] -> p
       _ -> POr ps'
simplify' x@(PConcat _) =
  let ps' = filter notPEmpty (flatten x)
  in case ps' of
       [] -> PEmpty
       [p] -> p
       _ -> PConcat ps' -- PConcat ps'
simplify' (PStar _ PEmpty) = PEmpty
simplify' other = other

-- | Function to flatten nested POr or nested PConcat applicataions.
flatten :: Pattern -> [Pattern]
flatten (POr ps) = (concatMap (\x -> case x of
                                       POr ps' -> ps'
                                       p -> [p]) ps)
flatten (PConcat ps) = (concatMap (\x -> case x of
                                           PConcat ps' -> ps'
                                           p -> [p]) ps)
flatten _ = error "flatten can only be applied to POr or PConcat"

notPEmpty :: Pattern -> Bool
notPEmpty PEmpty = False
notPEmpty _      = True


-- | If 'cannotMatchNull' returns 'True' then it is known that the
-- 'Pattern' will never accept an empty string.  If 'cannotMatchNull'
-- returns 'False' then it is possible but not definite that the
-- 'Pattern' could accept an empty string.
cannotMatchNull pIn =
  case pIn of
    PEmpty -> False
    PGroup _ p -> cannotMatchNull p
    POr [] -> False
    POr ps -> all cannotMatchNull ps
    PConcat [] -> False
    PConcat ps -> any cannotMatchNull ps
    PQuest _ -> False
    PPlus p -> cannotMatchNull p
    PStar {} -> False
    PBound 0 _ _ -> False
    PBound _ _ p -> cannotMatchNull p
    PCarat _ -> False
    PDollar _ -> False
    PNonCapture p -> cannotMatchNull p
    PNonEmpty _ -> False -- like PQuest
    _ -> True

-- | Determines if pIn will fail or accept [] and never accept any
-- characters. Treat PCarat and PDollar as True.
canOnlyMatchNull :: Pattern -> Bool
canOnlyMatchNull pIn =
  case pIn of
    PEmpty -> True
    PGroup _ p -> canOnlyMatchNull p
    POr [] -> True
    POr ps -> all canOnlyMatchNull ps
    PConcat [] -> True
    PConcat ps -> all canOnlyMatchNull ps
    PQuest p -> canOnlyMatchNull p
    PPlus p -> canOnlyMatchNull p
    PStar _ p -> canOnlyMatchNull p
    PBound _ (Just 0) _ -> True
    PBound _ _ p -> canOnlyMatchNull p
    PCarat _ -> True
    PDollar _ -> True
    _ ->False
