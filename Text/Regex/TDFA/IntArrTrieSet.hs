{- |
This creates a lazy Trie based on a finite range of Ints and is used to
memorize a function over the subsets of this range.

To create a Trie you need two supply 2 things
  * Range of keys to bound
  * A function or functions used to construct the value for a subset of keys

The Trie uses the Array type internally.
-}
module Text.Regex.TDFA.IntArrTrieSet where

{- By Chris Kuklewicz, 2007. BSD License, see the LICENSE file. -}

import Data.Array.IArray(Array,(!),listArray)

data TrieSet v = TrieSet { value :: v
                         , next :: Array Int (TrieSet v) }

-- | This is the accessor for the Trie. The list of keys should be
-- sorted.
lookupAsc :: TrieSet v -> [Int] -> v
lookupAsc (TrieSet {value=v,next=n}) =
  (\keys -> case keys of [] -> v
                         (key:keys') -> lookupAsc (n!key) keys')

-- | This is a Trie constructor for a complete range of keys.
fromBounds :: (Int,Int)     -- ^ (lower,upper) range of keys, lower<=upper
           -> ([Int] -> v)  -- ^ Function from list of keys to its value.
                            --   It must work for distinct ascending lists.
           -> TrieSet v     -- ^ The constructed Trie
fromBounds (start,stop) keysToValue = build id start where
  build keys low = TrieSet { value = keysToValue (keys [])
                           , next = listArray (low,stop)
                                    [build (keys.(x:)) (succ x) | x <- [low..stop] ] }

-- | This is a Trie constructor for a complete range of keys that uses
-- a function from single values and a merge operation on values to
-- fill the Trie.
fromSinglesMerge :: v          -- ^ value for (lookupAsc trie [])
                 -> (v->v->v)  -- ^ merge operation on values
                 -> (Int,Int)  -- ^ (lower,upper) range of keys, lower<=upper
                 -> (Int->v)   -- ^ Function from a single key to its value
                 -> TrieSet v  -- ^ The constructed Trie
fromSinglesMerge emptyValue mergeValues bound keyToValue = trieSet where
  trieSet = fromBounds bound keysToValue'
  keysToValue' keys =
    case keys of
      [] -> emptyValue
      [key] -> keyToValue key
      _ -> mergeValues (keysToValue (init keys)) (keysToValue [last keys])
  keysToValue = lookupAsc trieSet

-- | This is a Trie constructor for a complete range of keys that uses
-- a function from single values and a sum operation of values to fill
-- the Trie.
fromSinglesSum :: ([v]->v)   -- ^ summation operation for values
               -> (Int,Int)  -- ^ (lower,upper) range of keys, lower <= upper
               -> (Int->v)   -- ^ Function from a single key to its value
               -> TrieSet v  -- ^ The constructed Trie
fromSinglesSum mergeValues bound keyToValue = trieSet where
  trieSet = fromBounds bound keysToValue'
  keysToValue' = mergeValues . map keyToValue
