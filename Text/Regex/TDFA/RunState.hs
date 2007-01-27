module Text.Regex.TDFA.RunState where

import Control.Monad.RWS
import Data.Array.IArray(Array,(!),array,bounds,assocs)
import Data.IntMap(IntMap)
import qualified Data.IntMap as IMap
import Data.Sequence as S(singleton,(|>),viewl,ViewL(..))

import Text.Regex.Base(MatchArray)
import Text.Regex.TDFA.Common

{- By Chris Kuklewicz, 2007. BSD License, see the LICENSE file. -}

err :: String -> a
err s = common_error "Text.Regex.TDFA.RunState"  s

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


newScratchMap :: Regex -> Position -> IntMap {- Index -} Scratch
newScratchMap regexIn offsetIn = IMap.singleton (regex_init regexIn) (IMap.singleton 0 (offsetIn,True),mempty)

{-# INLINE update #-}
update :: RunState () -> Position -> Scratch -> (Scratch,[String])
update rs p sIn = execRWS rs (p,succ p) sIn

-- Used to create actions for RunState in TDFA.hs :

askPre :: RunState Position
askPre = asks fst

askPost :: RunState Position
askPost = asks snd

modifyMap :: (IntMap (Position,Bool) -> IntMap (Position,Bool)) -> RunState ()
modifyMap f = do
  (m,s) <- get
  let m' = f m in seq m' $ do
  put (m',s)

resetFlag :: Tag -> IntMap {- Tag -} (Position,Bool) -> IntMap (Position,Bool)
resetFlag = IMap.adjust (\(pos,_) -> (pos,False))

----

resetTag :: Tag -> RunState ()
resetTag tag = do
  pos <- askPre
  tell ["Reset Tag "++show (tag,pos)]
  modifyMap (resetFlag tag)

setPreTag :: Tag -> RunState ()
setPreTag tag = do
  pos <- askPre
  modifyMap (IMap.insert tag (pos,True))

setPostTag :: Tag -> RunState ()
setPostTag tag = do
  pos <- askPost
  modifyMap (IMap.insert tag (pos,True))

----

resetOrbit :: Tag -> RunState ()
resetOrbit tag = do
  (m,s) <- get
  let m' = IMap.delete tag m
      s' = IMap.delete tag s
  pos <- askPre
  tell ["Reset Orbit "++show (tag,pos)]
  seq m' $ seq s' $ put (m',s')

enterOrbit :: Tag -> RunState ()
enterOrbit tag = do
  pos <- askPre
  (m,s) <- get
  let new =  (IMap.insert tag (pos,True) m, IMap.insert tag (S.singleton pos) s)
      (m',s') = case IMap.lookup tag m of
                  Nothing -> new
                  Just (_,False) -> new
                  Just (_,True) -> (m
                                   ,case IMap.lookup tag s of
                                      Nothing -> snd new
                                      Just s_old -> let s_new = (S.|>) s_old pos
                                                    in seq s_new $ IMap.insert tag s_new s)
  let msg = ["Entering Orbit "++show (tag,pos)]
  tell msg
  seq m' $ seq s' $ put (m',s')

leaveOrbit :: Tag -> RunState ()
leaveOrbit tag = do
  modifyMap (resetFlag tag)
  let msg = ["Leaving Orbit "++show tag]
  tell msg
