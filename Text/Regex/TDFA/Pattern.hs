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
  case pIn of
    -- We know that "p" has been simplified in each of these cases:
    PQuest p -> POr [p,PEmpty]
    PPlus  p -> PConcat [p,simplify' $ PStar False p]
    PBound i _        _ | i<0 -> PEmpty  -- malformed
    PBound i (Just j) _ | i>j -> PEmpty  -- malformed
    PBound _ (Just 0) _ -> PEmpty
    PBound 0 Nothing  p -> PStar True p
    PBound i Nothing  p -> PConcat $ apply (p':) (pred i) [p,simplify' $ PStar False p]
      where p' = unCapture p
    PBound 0 (Just 1) p -> POr [p,PEmpty]
    PBound 0 (Just j) p -> apply (quest' . (concat' p)) (pred j) (quest' p)
    PBound i (Just j) p | i == j    -> PConcat $ apply (p':) (pred i) [p]
                        | otherwise -> PConcat $
                                         apply (p:) (pred i) [p,simplify' . starTrans' $ PBound 0 (Just (j-i)) p]
      where p' = unCapture p
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
