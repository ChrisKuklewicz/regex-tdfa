-- | "Text.Regex.TDFA.Run" is the main module for matching a DFA
-- against a String.  Many of the associated functions are exported to
-- other modules to help match against other types.
module Text.Regex.TDFA.Run (findMatch,findMatchAll,countMatchAll
                           ,makeTagComparer,tagsToGroups,update) where

import Control.Monad(guard,mplus)
import Control.Monad.RWS(execRWS)
import Data.Monoid(Monoid(..))
import Data.Array.IArray(Array,(!),array,bounds,assocs)
import Data.List(maximumBy)
import qualified Data.Map as Map(lookup,null)
import Data.IntMap(IntMap)
import qualified Data.IntMap as IMap
import Data.Maybe(isJust,isNothing)
import Data.Sequence(viewl,ViewL(..))

import Text.Regex.Base(MatchArray,RegexOptions(..))
import Text.Regex.TDFA.Common
import Text.Regex.TDFA.Wrap()
-- import Debug.Trace

{- By Chris Kuklewicz, 2007. BSD License, see the LICENSE file. -}

err :: String -> a
err = common_error "Text.Regex.TDFA.Run"

type TagComparer = Scratch -> Scratch -> Ordering -- GT if first argument is the preferred one

compareWith :: (Ord x,Monoid a) => (Maybe (x,b) -> Maybe (x,c) -> a) -> [(x,b)] -> [(x,c)] -> a
compareWith comp = cw where
  cw [] [] = comp Nothing Nothing
  cw xx@(x:xs) yy@(y:ys) =
    case compare (fst x) (fst y) of
      GT -> comp Nothing  (Just y) `mappend` cw xx ys
      EQ -> comp (Just x) (Just y) `mappend` cw xs ys
      LT -> comp (Just x) Nothing  `mappend` cw xs yy
  cw xx [] = foldr (\x rest -> comp (Just x) Nothing  `mappend` rest) mempty xx
  cw [] yy = foldr (\y rest -> comp Nothing  (Just y) `mappend` rest) mempty yy

makeTagComparer :: Array Tag OP -> TagComparer
makeTagComparer aTags = (\ tv1 tv2 ->
  let tv1' = IMap.toAscList . fst $ tv1
      tv2' = IMap.toAscList . fst $ tv2

      errMsg s = err $ "makeTagComparer : " ++ s ++ " : " ++ unlines [show aTags,show tv1,show tv2]

      tagComparer :: Maybe (Tag,(Position,Bool)) -> Maybe (Tag,(Position,Bool)) -> Ordering
      tagComparer = check where
        check (Just (tag,(pos1,_))) (Just (_,(pos2,_))) =
          case aTags!tag of
            Maximize -> compare pos1 pos2
            Minimize -> (flip compare) pos1 pos2
            Orbit -> compareOrbits (IMap.lookup tag (snd tv1)) (IMap.lookup tag (snd tv2))
        check all1@(Just (tag,(_,_))) Nothing =
          case aTags!tag of
            Maximize -> GT
            Minimize -> LT -- errMsg $ "tagComparer TODO: Minimize tag in 1st without one in 2nd "++show all
            Orbit -> errMsg $ "tagComparer TODO: Orbit tag in 1st without one in 2nd " ++ show all1
        check Nothing all2@(Just (tag,(_,_))) =
          case aTags!tag of
            Maximize -> LT
            Minimize -> GT -- errMsg $ "tagComparer TODO: Minimize tag in 2nd without one in 1st "++show all
            Orbit -> errMsg $ "tagComparer TODO: Orbit tag in 2nd without one in 1st " ++ show all2
        check Nothing Nothing = EQ
      compareOrbits (Just pos1) (Just pos2) = comparePos (viewl pos1) (viewl pos2)
        where comparePos EmptyL EmptyL = EQ
              comparePos EmptyL _ = GT
              comparePos _ EmptyL = LT
              comparePos (p1:<ps1) (p2:<ps2) = compare p1 p2 `mappend`
                                               comparePos (viewl ps1) (viewl ps2)
      compareOrbits e1 e2 = errMsg $ ("compareOrbits Nothing found in Scratch"++show (e1,e2))
    
  in compareWith tagComparer tv1' tv2')

{-# INLINE look #-}
look :: Int -> IntMap a -> a
look key imap = IMap.findWithDefault (error ("key "++show key++" not found in Text.Regex.TDFA.Run.look")) key imap


{-# INLINE tagsToGroups #-}
tagsToGroups :: Array GroupIndex [GroupInfo] -> Scratch -> MatchArray
tagsToGroups aGroups (tags,orbits) | False && not (IMap.null orbits) = -- XXX disable this check
  error ("tagsToGroups non null orbits :"++show (aGroups,tags,orbits))
                                   | otherwise = groups -- trace (">><<< "++show (tags,filler)) groups
  where groups = array (0,snd (bounds aGroups)) filler
        filler = wholeMatch : map checkAll (assocs aGroups)
        wholeMatch = (0,(startPos,stopPos-startPos)) -- will not fail to return good positions
          where startPos = fst $ look 0 tags
                stopPos = fst $ look 1 tags
        checkAll (this_index,these_groups) = (this_index,if null good then (-1,0) else head good)
          where good = do (GroupInfo _ parent start stop) <- these_groups -- Monad []
                          (startPos,_) <- IMap.lookup start tags
                          (stopPos,validStop) <- IMap.lookup stop tags
                          guard validStop
                          let (startParent,lengthParent) = groups!parent
                          guard (0 <= startParent &&
                                 0 <= lengthParent &&
                                 startParent <= startPos &&
                                 stopPos <= startPos + lengthParent)
                          return (startPos,stopPos-startPos)

{-# INLINE findMatch #-}
findMatch :: Regex -> String -> Maybe MatchArray
findMatch regexIn stringIn = loop 0 '\n' stringIn where
  loop offset prev input =
    let result = matchHere regexIn offset prev input
    in if isJust result then result
         else case input of
                [] -> Nothing
                (prev':input') -> 
                  let offset' = succ offset
                  in seq offset' $
                     loop offset' prev' input'

{-# INLINE findMatchAll #-}
findMatchAll :: Regex -> String -> [MatchArray]
findMatchAll regexIn stringIn = loop 0 '\n' stringIn where
  loop offset prev input =
    case matchHere regexIn offset prev input of
      Nothing -> case input of
                   [] -> []
                   (prev':input') ->
                     let offset' = succ offset
                     in seq offset' $
                        loop offset' prev' input'
      Just ma -> ma : let (start,len) = ma!0
                      in if null input || len == 0 then []
                           else let offset' = start + len     -- start >= offset; len > 0
                                    skip = (offset' - offset) -- skip >= 0
                                    (prev':input') = drop (pred skip) input
                                in seq offset' $
                                   loop offset' prev' input'

{-# INLINE countMatchAll #-}
countMatchAll :: Regex -> String -> Int
countMatchAll regexIn stringIn = loop 0 '\n' stringIn $! 0 where
  regex = setExecOpts (ExecOption {captureGroups = False,testMatch = False}) regexIn
  loop offset prev input count =
    case matchHere  regex offset prev input of
      Nothing -> case input of
                   [] -> count
                   (prev':input') ->
                     let offset' = succ offset
                     in seq offset' $ 
                        loop offset' prev' input' $! count
      Just ma -> let (start,len) = ma!0
                 in if null input then count
                      else let offset' = start + len     -- start >= offset; len > 0
                               skip = (offset' - offset) -- skip >= 0
                               (prev':input') = drop (pred skip) input
                           in seq offset' $
                              loop offset' prev' input' $! succ count

{-# INLINE update #-}
update :: RunState () -> Position -> Scratch -> (Scratch,[String])
update rs p sIn = execRWS rs (p,succ p) sIn

{-# INLINE matchHere #-}
matchHere :: Regex -> Position -> Char -> String
          -> Maybe MatchArray
matchHere regexIn offsetIn prevIn inputIn = ans where
  ans = if captureGroups (regex_execOptions regexIn)
          then fmap (tagsToGroups (regex_groups regexIn)) $
                 runHere Nothing (d_dt (regex_dfa regexIn)) initialScratchMap offsetIn prevIn inputIn
          else let winOff = runHereNoCap Nothing (d_dt (regex_dfa regexIn)) offsetIn prevIn inputIn
               in case winOff of
                    Nothing -> Nothing
                    Just offsetEnd -> Just (array (0,0) [(0,(offsetIn,offsetEnd-offsetIn))])

  initialScratchMap = IMap.singleton (regex_init regexIn) (IMap.singleton 0 (offsetIn,True),mempty)
  comp = makeTagComparer (regex_tags regexIn)

  test_multiline wt _ prev input =
    case wt of Test_BOL -> prev == '\n'
               Test_EOL -> case input of
                             [] -> True
                             (next:_) -> next == '\n'

  test_singleline wt off _ input =
    case wt of Test_BOL -> off == 0
               Test_EOL -> null input

  test = if multiline (regex_compOptions regexIn) then test_multiline else test_singleline
  
  runHere :: Maybe Scratch -> DT -> IntMap Scratch -> Position -> Char -> String -> Maybe Scratch
  runHere winning dt tags off prev input = 
    let best (destIndex,mSourceDelta) = (destIndex
                                        ,maximumBy comp 
                                         . map fst
--                                       . map (\(m,w) -> trace ("\n>"++show destIndex++'\n':unlines w++"<") (m,w))
                                         . map (\(sourceIndex,(_,rs)) ->
                                                update rs off (look sourceIndex tags))
                                         . IMap.toList $ mSourceDelta)
    in case dt of
         Simple' {dt_win=w, dt_trans=t, dt_other=o} ->
           let winning' = if IMap.null w then winning
                            else Just . maximumBy comp
--                                    . (\wins -> trace (unlines . map show $ wins) wins)
                                      . map fst
--                                    . map (\(m,written) -> trace ("\n>winning\n"++unlines written++"<") (m,w))
                                      . map (\(sourceIndex,rs) ->
                                               let scratch = look sourceIndex tags
                                               in -- trace ("### " ++ show (sourceIndex,scratch)) $
                                                  update rs off scratch)
                                      . IMap.toList $ w
       
           in seq winning' $ -- trace ("\n@@@ " ++ show (off,tags,winning')) $
              case input of
                [] -> winning'
                (c:input') ->
                  case Map.lookup c t `mplus` o of
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
           if Map.null t && isNothing o then winning' else
             case input of
               [] -> winning'
               (c:input') ->
                 case Map.lookup c t `mplus` o of
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
