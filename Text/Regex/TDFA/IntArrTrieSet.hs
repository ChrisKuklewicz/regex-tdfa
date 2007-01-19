module Text.Regex.TDFA.IntArrTrieSet where

import Data.List
import Data.Array

{-
This creates a Trie based on a finite set of Ints and is used to
memorize a function over the domain of combinations from that set.

To create one it needs 2 things
  * Range of keys to bound the space of combinations
  * Function from a Set (or sorted list) of keys to the item value

The Trie uses the Array type internally.
-}

data TrieSet v = TrieSet { value :: v
                         , next :: Array Int (TrieSet v) }

-- Lists of keys should be sorted.
lookupAsc :: TrieSet v -> [Int] -> v
lookupAsc (TrieSet {value=v,next=n}) = (\keys ->
  case keys of
    [] -> v
    (key:keys') -> lookupAsc (n!key) keys')

-- Lists of keys should be sorted.
fromBounds :: (Int,Int) -> ([Int] -> v) -> TrieSet v
fromBounds (start,stop) keysToValue = build id start where
  build keys low = TrieSet { value = keysToValue (keys [])
                           , next = listArray (low,stop)
                                    [build (keys.(x:)) (succ x) | x <- [low..stop] ] }

fromSingles ::  v -> (v->v->v) -> (Int,Int) -> (Int->v) -> TrieSet v
fromSingles emptyValue mergeValues bound keyToValue = trieSet where
  keysToValue' = assembleAsc emptyValue mergeValues keyToValue keysToValue
  keysToValue = lookupAsc trieSet
  trieSet = fromBounds bound keysToValue'

-- This takes a function which can merge a list of values and a method
-- to convert individual keys to values.  The list of keys must be
-- distince and in ascending order.
fromSingles' :: ([v]->v) -> (Int,Int)-> (Int->v) -> TrieSet v
fromSingles' mergeValues bound keyToValue = trieSet where
  keysToValue' = combiningAsc mergeValues keyToValue
  trieSet = fromBounds bound keysToValue'

combiningAsc :: ([v]->v) -> (Int->v) -> ([Int] -> v)
combiningAsc mergeValues keyToValue = mergeValues . map keyToValue

-- Helper function to create lookup function
assembleAsc :: v -> (v->v->v) -> (Int->v) -> ([Int] -> v) -> ([Int] -> v)
assembleAsc emptyValue mergeValues keyToValue keysToValue = keysToValue' where
  keysToValue' keys =
   case keys of
     [] -> emptyValue
     [key] -> keyToValue key
     _ -> mergeValues (keysToValue (init keys)) (keysToValue [last keys])

