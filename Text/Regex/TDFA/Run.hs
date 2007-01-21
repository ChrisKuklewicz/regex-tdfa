module Text.Regex.TDFA.Run where -- (findMatch,findMatchAll,countMatchAll) where

import Control.Monad
import Data.Array.IArray
-- import Data.Map(Map)
import qualified Data.Map as Map
import Data.IntMap(IntMap)
import qualified Data.IntMap as IMap
import Data.Monoid
import Data.Maybe
import Data.List

import Text.Regex.Base
import Text.Regex.TDFA.Common
import Text.Regex.TDFA.ReadRegex
import Text.Regex.TDFA.CorePattern
import Text.Regex.TDFA.TDFA
import Text.Regex.TDFA.Wrap

import Text.Regex.TDFA.TNFA (patternToNFA)

import Debug.Trace


toP = either (error.show) id . parseRegex 
toQ = patternToQ defaultCompOpt . toP
toNFA = patternToNFA defaultCompOpt . toP
toDFA = nfaToDFA . toNFA
display_NFA = mapM_ print . elems . (\(x,_,_) -> snd x) . toNFA 
display_DFA = mapM_ print . Map.elems . dfaMap . (\(x,_,_,_) -> x) . toDFA 


type TagValues = IntMap {- Tag -} Position
type TagComparer = TagValues -> TagValues -> Ordering -- GT is the preferred one

-- Returns GT if the first item is preferred. I could generate test
-- cases to be sure about all the Maximize cases.  The minimize cases
-- look stranger to trigger, so I am leaving them as errors until I am
-- certain what to put there.
makeTagComparer :: Array Tag OP -> TagComparer
makeTagComparer tags = (\ tv1 tv2 ->
  let tv1' = IMap.toAscList tv1
      tv2' = IMap.toAscList tv2
      errMsg s = error $ s ++ " : " ++ show (tags,tv1,tv2)
      comp ((t1,v1):rest1) ((t2,v2):rest2) =
        case compare t1 t2 of
               EQ -> case tags!t1 of
                       Minimize -> compare v2 v1 `mappend` comp rest1 rest2
                       Maximize -> compare v1 v2 `mappend` comp rest1 rest2
               LT -> case tags!t1 of
                       Maximize -> GT
                       Minimize -> errMsg "makeTagComparer: tv1 without tv2"
               GT -> case tags!t2 of 
                       Maximize -> LT
                       Minimize -> errMsg "makeTagComparer: tv2 without tv1"
      comp [] [] = EQ
      comp ((t1,_):_) [] = case tags!t1 of
                              Maximize -> GT
                              Minimize -> errMsg "makeTagComparer: tv1 longer"
      comp [] ((t2,_):_) = case tags!t2 of
                              Maximize -> LT
                              Minimize -> errMsg "makeTagComparer: tv2 longer"
  in comp tv1' tv2'
 )

{-# INLINE look #-}
look :: Int -> IntMap a -> a
look key imap = IMap.findWithDefault (error ("key "++show key++" not found in Text.Regex.TDFA.Run.look")) key imap


{-# INLINE tagsToGroups #-}
tagsToGroups :: Array PatternIndex [GroupInfo] -> IntMap Position -> MatchArray
tagsToGroups aGroups tags = groups -- trace (">><<< "++show (tags,filler)) groups
  where groups = array (0,snd (bounds aGroups)) filler
        filler = wholeMatch : map checkAll (assocs aGroups)
        wholeMatch = (0,(startPos,stopPos-startPos)) -- will not fail to return good positions
          where startPos = look 0 tags
                stopPos = look 1 tags
        checkAll (this_index,these_groups) = (this_index,if null good then (-1,0) else head good)
          where good = do (GroupInfo _ _ start stop) <- these_groups
                          startPos <- IMap.lookup start tags
                          stopPos <- IMap.lookup stop tags
                          return (startPos,stopPos-startPos)

{-# INLINE findMatch #-}
findMatch :: (Extract a) => (a -> Bool) -> (a->(Char,a)) 
          -> Regex -> a -> Maybe MatchArray
findMatch isNull headTail regexIn stringIn = loop 0 '\n' stringIn where
  loop offset prev input =
    let result = matchHere isNull headTail regexIn offset prev input
    in if isJust result then result
         else if isNull input then Nothing
                else let (prev',input') = headTail input
                         offset' = succ offset
                     in seq offset' $
                        loop offset' prev' input'

{-# INLINE findMatchAll #-}
findMatchAll :: (Extract a) => (a -> Bool) -> (a->(Char,a))
             -> Regex -> a -> [MatchArray]
findMatchAll isNull headTail regexIn stringIn = loop 0 '\n' stringIn where
  loop offset prev input =
    case matchHere isNull headTail regexIn offset prev input of
      Nothing -> if isNull input then []
                   else let (prev',input') = headTail input
                            offset' = succ offset
                        in seq offset' $
                           loop offset' prev' input'
      Just ma -> ma : let (start,len) = ma!0
                      in if isNull input || len == 0 then []
                           else let offset' = start + len          -- start >= offset; len > 0
                                    skip = (offset' - offset) -- skip >= 0
                                    post = after (pred skip) input
                                    (prev',input') = headTail post
                                in seq offset' $
                                   loop offset' prev' input'

{-# INLINE countMatchAll #-}
countMatchAll :: (Extract a) => (a -> Bool) -> (a->(Char,a))
              -> Regex -> a -> Int
countMatchAll isNull headTail regexIn stringIn = loop 0 '\n' stringIn $! 0 where
  regex = setExecOpts (ExecOption {captureGroups = False}) regexIn
  loop offset prev input count =
    case matchHere isNull headTail regex offset prev input of
      Nothing -> if isNull input then count
                   else let (prev',input') = headTail input
                            offset' = succ offset
                        in seq offset' $ 
                           loop offset' prev' input' $! count
      Just ma -> let (start,len) = ma!0
                 in if isNull input then count
                      else let offset' = start + len          -- start >= offset; len > 0
                               skip = (offset' - offset) -- skip >= 0
                               post = after (pred skip) input
                               (prev',input') = headTail post
                           in seq offset' $
                              loop offset' prev' input' $! succ count

{-# INLINE update #-}
update :: Delta -> Position -> IntMap Position -> (IntMap Position,[String])
update delta off oldMap = foldl (\(m,w) (tag,pos) ->
   case () of
     _ | 0 <= pos -> (IMap.insert tag pos m,w)
       | updateReset == pos -> (IMap.delete tag m,w)
       | updateEnterOrbit == pos -> (IMap.insert tag (-tag) m,w++["Enter orbit at "++show (tag,pos)])
       | updateLeaveOrbit == pos -> (IMap.delete tag m,w++["Leave orbit at "++show (tag,pos)])
       | otherwise -> error ("There was a weird update pos: "++show (tag,pos))
                                ) (oldMap,[]) (delta off)

{-# INLINE matchHere #-}
matchHere ::  (Extract a) => (a -> Bool) -> (a->(Char,a)) 
              -> Regex -> Position -> Char -> a
              -> Maybe MatchArray
matchHere isNull headTail regexIn offsetIn prevIn inputIn = ans where
  ans = if captureGroups (regex_execOptions regexIn)
          then fmap (tagsToGroups (regex_groups regexIn)) $
                 runHere Nothing (d_dt (regex_dfa regexIn)) initialTags offsetIn prevIn inputIn
          else let winOff = runHereNoCap Nothing (d_dt (regex_dfa regexIn)) offsetIn prevIn inputIn
               in case winOff of
                    Nothing -> Nothing
                    Just offsetEnd -> Just (array (0,0) [(0,(offsetIn,offsetEnd-offsetIn))])

  initialTags = IMap.singleton (regex_init regexIn) (IMap.singleton 0 offsetIn)
  comp = makeTagComparer (regex_tags regexIn)

  test_multiline wt _ prev input =
    case wt of Test_BOL -> prev == '\n'
               Test_EOL -> if isNull input then True
                             else let (next,_) = headTail input
                                  in next == '\n'

  test_singleline wt off _ input =
    case wt of Test_BOL -> off == 0
               Test_EOL -> isNull input

  test = if multiline (regex_compOptions regexIn) then test_multiline else test_singleline
  
  runHere winning dt tags off prev input =
    let best (destIndex,mSourceDelta) = (destIndex
                                        ,maximumBy comp 
                                         . map (\(m,w) -> trace ("\n>"++show destIndex++'\n':unlines w++"<") m)
                                         . map (\(sourceIndex,(_,delta)) ->
                                                update delta off (look sourceIndex tags))
                                         . IMap.toList $ mSourceDelta)
    in case dt of
         Simple' {dt_win=w, dt_trans=t, dt_other=o} ->
           let winning' = if IMap.null w then winning
                            else Just . maximumBy comp
                                      . map (\(m,w) -> trace ("\n>winning\n"++unlines w++"<") m)
                                      . map (\(sourceIndex,delta) ->
                                               update delta off (look sourceIndex tags))
                                      . IMap.toList $ w
        
           in seq winning' $
              if isNull input then winning' else
                let (c,input') = headTail input
                in case Map.lookup c t `mplus` o of
                     Nothing -> winning'
                     Just (dfa,trans) -> let dt' = d_dt dfa
                                             tags' = IMap.fromAscList
                                                     . map best
                                                     . IMap.toAscList $ trans
                                             off' = succ off
                                             prev' = c
                                         in seq off' $
                                            runHere winning' dt' tags' off' prev' input'
         Testing' {dt_test=wt,dt_a=a,dt_b=b} ->
           if test wt off prev input
             then runHere winning a tags off prev input
             else runHere winning b tags off prev input

  runHereNoCap winning dt off prev input =
    case dt of
      Simple' {dt_win=w, dt_trans=t, dt_other=o} ->
        let winning' = if IMap.null w then winning else Just off
        in seq winning' $
           if Map.null t && isNothing o || isNull input then winning' else
             let (c,input') = headTail input
             in case Map.lookup c t `mplus` o of
                  Nothing -> winning'
                  Just (dfa,_) -> let dt' = d_dt dfa
                                      off' = succ off
                                      prev' = c
                                  in seq off' $
                                     runHereNoCap winning' dt' off' prev' input'
      Testing' {dt_test=wt,dt_a=a,dt_b=b} ->
        if test wt off prev input
          then runHereNoCap winning a off prev input
          else runHereNoCap winning b off prev input


{-
-- old bug:

*Text.Regex.TDFA.Run> let r = makeRegex "(.*|..*)+(....|(.(.*)*.))(.?|.?)" :: Regex in matchHere r 0 '\n' "aaaaaa"
Just (fromList [(0,0),(1,6),(2,6),(3,4),(4,4),(5,4),(12,6),(13,5),(14,5),(18,6)],array (0,5) [(0,(0,6)),(1,(-1,0)),(2,(4,2)),(3,(4,2)),(4,(-1,0)),(5,*** Exception: (Array.!): undefined array element
-}

{-
The test cases that set the Maximize cases in makeTagComparer

*Text.Regex.TDFA.Run> let r=makeRegex "((.)(.*)|.+)*|." :: Regex in matchHere r 0 '\n' "a"
Just (fromList *** Exception: tv2 longer :(
array (0,7) [(0,Minimize),(1,Maximize),(2,Maximize),(3,Minimize),(4,Maximize),(5,Maximize),(6,Maximize),(7,Maximize)],
fromList [(0,0),(1,1)],
fromList [(0,0),(1,1),(2,1),(3,0),(4,1),(5,1),(6,1)])

array (1,3) [(1,GroupInfo {thisIndex = 1, parentIndex = 0, startTag = 3, stopTag = 4})
            ,(2,GroupInfo {thisIndex = 2, parentIndex = 1, startTag = 3, stopTag = 6})
            ,(3,GroupInfo {thisIndex = 3, parentIndex = 1, startTag = 6, stopTag = 5})]


*Text.Regex.TDFA.Run> let r=makeRegex "((.)(.*)|.+)*|(.)" :: Regex in matchHere r 0 '\n' "a"
Just (fromList *** Exception: tv2 without tv1 :(
array (0,8) [(0,Minimize),(1,Maximize),(2,Maximize),(3,Minimize),(4,Maximize),(5,Maximize),(6,Maximize),(7,Maximize),(8,Maximize)],
fromList [(0,0),(1,1),(8,1)],
fromList [(0,0),(1,1),(2,1),(3,0),(4,1),(5,1),(6,1)])  -- preferred

array (1,4) [(1,GroupInfo {thisIndex = 1, parentIndex = 0, startTag = 3, stopTag = 4})
            ,(2,GroupInfo {thisIndex = 2, parentIndex = 1, startTag = 3, stopTag = 6})
            ,(3,GroupInfo {thisIndex = 3, parentIndex = 1, startTag = 6, stopTag = 5})
            ,(4,GroupInfo {thisIndex = 4, parentIndex = 0, startTag = 0, stopTag = 8})]


*Text.Regex.TDFA.Run> let r=makeRegex "((.)(.*)|.+)*|." :: Regex in matchHere r 0 '\n' "aa"
Just (fromList *** Exception: tv1 without tv2 : (
array (0,7) [(0,Minimize),(1,Maximize),(2,Maximize),(3,Minimize),(4,Maximize),(5,Maximize),(6,Maximize),(7,Maximize)],
fromList [(0,0),(1,2),(2,2),(3,0),(4,2),(5,2),(6,1)],   -- (.)(.*) is tagged 3.6.54 and is the preferred answer
fromList [(0,0),(1,2),(2,2),(3,0),(4,2),(7,1)])         -- .+ branch is ..* which is tagged 3.7.*4

-}
