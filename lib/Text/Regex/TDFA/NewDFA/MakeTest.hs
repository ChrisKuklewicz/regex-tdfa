module Text.Regex.TDFA.NewDFA.MakeTest(test_singleline,test_multiline) where

import qualified Data.IntSet as ISet(IntSet,member,fromAscList)
import Text.Regex.TDFA.Common(WhichTest(..),Index)
import Text.Regex.TDFA.NewDFA.Uncons(Uncons(uncons))

{-# INLINE test_singleline #-}
{-# INLINE test_multiline #-}
{-# INLINE test_common #-}
test_singleline,test_multiline,test_common :: Uncons text => WhichTest -> Index -> Char -> text -> Bool
test_multiline Test_BOL _off prev _input = prev == '\n'
test_multiline Test_EOL _off _prev input = case uncons input of
                                                     Nothing -> True
                                                     Just (next,_) -> next == '\n'
test_multiline test off prev input = test_common test off prev input

test_singleline Test_BOL off _prev _input = off == 0
test_singleline Test_EOL _off _prev input = case uncons input of
                                              Nothing -> True
                                              _ -> False
test_singleline test off prev input = test_common test off prev input

test_common Test_BOB off _prev _input = off==0
test_common Test_EOB _off _prev input = case uncons input of
                                          Nothing -> True
                                          _ -> False
test_common Test_BOW _off prev input = not (isWord prev) && case uncons input of
                                                            Nothing -> False
                                                            Just (c,_) -> isWord c
test_common Test_EOW _off prev input = isWord prev && case uncons input of
                                                        Nothing -> True
                                                        Just (c,_) -> not (isWord c)
test_common Test_EdgeWord _off prev input =
  if isWord prev
    then case uncons input of Nothing -> True
                              Just (c,_) -> not (isWord c)
    else case uncons input of Nothing -> False
                              Just (c,_) -> isWord c
test_common Test_NotEdgeWord _off prev input = not (test_common Test_EdgeWord _off prev input)

test_common Test_BOL _ _ _ = undefined
test_common Test_EOL _ _ _ = undefined

isWord :: Char -> Bool
isWord c = ISet.member (fromEnum c) wordSet
  where wordSet :: ISet.IntSet
        wordSet = ISet.fromAscList . map fromEnum $ "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz"
