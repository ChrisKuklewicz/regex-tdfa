module Text.Regex.TDFA.RunState where

import Control.Monad.RWS
import Data.Array.IArray(Array,(!),array,bounds,assocs,accumArray,(//))
import Data.IntMap(IntMap)
import qualified Data.IntMap as IMap
import Data.Sequence as S((|>),viewl,ViewL(..)) -- empty XXX  XXX

import Text.Regex.Base(MatchArray)
import Text.Regex.TDFA.Common

--import Debug.Trace

{- By Chris Kuklewicz, 2007. BSD License, see the LICENSE file. -}

err :: String -> a
err s = common_error "Text.Regex.TDFA.RunState"  s

{-# INLINE tagsToGroups #-}
{-
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
-}
tagsToGroups :: Array GroupIndex [GroupInfo] -> Scratch -> MatchArray
tagsToGroups aGroups (Scratch {scratchPos=pos,scratchFlags=flags}) = groups
  where groups = array (0,snd (bounds aGroups)) filler
        filler = wholeMatch : map checkAll (assocs aGroups)
        wholeMatch = (0,(startPos,stopPos-startPos)) -- will not fail to return good positions
          where startPos = pos!0
                stopPos = pos!1
        checkAll (this_index,these_groups) = (this_index,if null good then (-1,0) else head good)
          where good = do (GroupInfo _ parent start stop) <- these_groups -- Monad []
                          guard (flags!stop)
                          let startPos = pos!start
                              stopPos = pos!stop
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
{-
makeTagComparer :: Array Tag OP -> TagComparer
makeTagComparer aTags = (\ tv1 tv2 ->
  let tv1' = IMap.toAscList . fst $ tv1
      tv2' = IMap.toAscList . fst $ tv2

      errMsg s = err $ "makeTagComparer : " ++ s ++ " : " ++ unlines [show aTags,show tv1,show tv2]

      check :: Maybe (Tag,(Position,Bool)) -> Maybe (Tag,(Position,Bool)) -> Ordering
      check Nothing Nothing = EQ
      check Nothing all2 = flipOrder (check all2 Nothing)
      check all1@(Just (tag,(_,_))) Nothing =
        case aTags!tag of
          Maximize -> GT
          Minimize -> LT
          Orbit -> errMsg $ "check Unexpected: Orbit tag in only one of the items: " ++ show all1
      check (Just (tag,(pos1,_))) (Just (_,(pos2,_))) =
        case aTags!tag of
          Maximize -> compare pos1 pos2
          Minimize -> (flip compare) pos1 pos2
          Orbit -> compareOrbits (IMap.lookup tag (snd tv1)) (IMap.lookup tag (snd tv2))
        where compareOrbits (Just seq1) (Just seq2) = {-# SCC "makeTagComparer compareOrbits" #-} comparePos (viewl seq1) (viewl seq2)
                where comparePos EmptyL EmptyL = EQ
                      comparePos EmptyL _ = GT
                      comparePos _ EmptyL = LT
                      comparePos (p1:<seq1') (p2:<seq2') =
                        compare p1 p2 `mappend` comparePos (viewl seq1') (viewl seq2')
              compareOrbits e1 e2 = errMsg $ ("check.compareOrbits Unexpected, Nothing found in Scratch"++show (e1,e2))

  in {-# SCC "makeTagComparer body" #-} compareWith check tv1' tv2')
-}
makeTagComparer :: Array Tag OP -> TagComparer
makeTagComparer aTags =
  let toFar = succ . snd . bounds $ aTags
  in (\ tv1@(Scratch {scratchPos=tp1,scratchFlags=tf1,scratchOrbits=to1})
        tv2@(Scratch {scratchPos=tp2,scratchFlags=tf2,scratchOrbits=to2}) ->
        let errMsg tag s =  err $ "makeTagComparer : " ++ s ++ " : " ++ unlines [show tag,show aTags,show tv1,show tv2]
            check tag | seq tag False = undefined
                      | tag==toFar = EQ
                      | op == Orbit =
              case (tf1!tag,tf2!tag) of
                (True,True) -> compareOrbits tag `mappend` check (succ tag)
                (False,False) -> check (succ tag)
                _ -> errMsg tag "check : Unexpected scratchFlags mismatch" tag
                      | otherwise =  {-# SCC "makeTagComparer.check_tags" #-}
              case (tp1!tag,tp2!tag) of
                (-1,-1) -> check (succ tag)
                ( _,-1) -> if op==Maximize then GT else LT
                (-1, _) -> if op==Maximize then LT else GT
                (p1,p2) -> (if op==Maximize then compare p1 p2 else (flip compare) p1 p2)
                           `mappend` check (succ tag)
              where op = aTags!tag
            compareOrbits tag | seq tag False = undefined
                              | otherwise = {-# SCC "makeTagComparer.check_orbits" #-}
              case (IMap.lookup tag to1,IMap.lookup tag to2) of
                (Nothing,Nothing) -> check (succ tag)
                (Just o1,Just o2) | inOrbit o1 == inOrbit o2 -> comparePos (viewl (getOrbits o1)) (viewl (getOrbits o2))
                                  | otherwise -> errMsg tag "compareOrbits slightly Unexpected inOrbit mismatch"
                _ -> errMsg tag "compareOrbits Unexpected: Orbit tag in only one of the items"
            comparePos EmptyL EmptyL = EQ
            comparePos EmptyL _      = GT
            comparePos _      EmptyL = LT
            comparePos (p1 :< ps1) (p2 :< ps2) = compare p1 p2 `mappend` comparePos (viewl ps1) (viewl ps2)
            answer = check 0
            -- msg = "\n>"++unlines [show aTags,show tv1,show tv2,show answer] -- for tracing
        in {-# SCC "makeTagComparer.check_0" #-} answer
     )

newScratchMap :: Regex -> Position -> IntMap {- Index -} Scratch
newScratchMap regexIn offsetIn = IMap.singleton (regex_init regexIn) initScratch
  where tagRange = bounds (regex_tags regexIn)
        initScratch = Scratch { scratchPos = accumArray (\_ new -> new) (-1) tagRange [(0,offsetIn)]
                              , scratchFlags = accumArray (\_ new -> new) False tagRange [(0,True)]
                              , scratchOrbits = mempty }

{-# INLINE update #-}
update :: Instructions -> Position -> Scratch -> Scratch
update todo pos sIn =
  Scratch { scratchPos = (scratchPos sIn) // (map (\(tag,post) -> if post then (tag,pos')
                                                                    else (tag,pos)) (newPos todo))
          , scratchFlags = (scratchFlags sIn) // (newFlags todo)
          , scratchOrbits = (newOrbits todo) pos (scratchOrbits sIn)
          }
  where pos' = succ pos
{-
        updateOrbits | IMap.null (newOrbits todo) = scratchOrbits sIn
                     | otherwise = let fs = sequence (IMap.elems (newOrbits todo)) pos -- Reader ((->) Position)
                                       f = foldl1 (.) fs
                                   in f (scratchOrbits sIn)
-}
{-
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
-}
----------------------

modifyPos :: Bool -> Tag -> CompileIntructions ()
modifyPos todo tag = do
  (a,b,c) <- get
  let a' = IMap.insert tag todo a
      b' = IMap.insert tag True b
  put (a',b',c)

setPreTag :: Tag -> CompileIntructions ()
setPreTag = modifyPos False

setPostTag :: Tag -> CompileIntructions ()
setPostTag = modifyPos True

resetTag :: Tag -> CompileIntructions ()
resetTag tag = do
  (a,b,c) <- get
  let b' = IMap.insert tag False b
  put (a,b',c)

modifyOrbit :: (IntMap AlterOrbit -> IntMap AlterOrbit) -> CompileIntructions ()
modifyOrbit f = do
  (a,b,c) <- get
  let c' = f c
  put (a,b,c')

modifyFlagOrbit :: Tag -> Bool -> (IntMap AlterOrbit -> IntMap AlterOrbit) -> CompileIntructions ()
modifyFlagOrbit tag flag f = do
  (a,b,c) <- get
  let b' = IMap.insert tag flag b
      c' = f c
  put (a,b',c')

resetOrbit :: Tag -> CompileIntructions ()
resetOrbit tag = modifyFlagOrbit tag False (IMap.insert tag AlterReset)

leaveOrbit :: Tag -> CompileIntructions ()
leaveOrbit tag = modifyOrbit escapeOrbit where
  escapeOrbit = IMap.insertWith setInOrbitFalse tag AlterLeave where
    setInOrbitFalse _ x@(AlterModify {}) = x {newInOrbit = False}
    setInOrbitFalse _ x = x

enterOrbit :: Tag -> CompileIntructions ()
enterOrbit tag = modifyFlagOrbit tag True changeOrbit where
  changeOrbit = IMap.insertWith overwriteOrbit tag appendNewOrbit

  appendNewOrbit = AlterModify {newInOrbit = True, freshOrbit = False} -- try to append
  startNewOrbit  = AlterModify {newInOrbit = True, freshOrbit = True}   -- will start a new series

  overwriteOrbit _ AlterReset = startNewOrbit
  overwriteOrbit _ AlterLeave = startNewOrbit
  overwriteOrbit _ (AlterModify {newInOrbit = False}) = startNewOrbit
  overwriteOrbit _ (AlterModify {newInOrbit = True}) =
    err $ "enterOrbit: Cannot enterOrbit twice in a row: " ++ show tag

alterOrbits :: [(Tag,AlterOrbit)] -> OrbitInstruction
alterOrbits [] = (\_ -> id)
alterOrbits ys = foldl merge x0 xs
  where merge :: OrbitInstruction -> (Tag,OrbitInstruction') -> OrbitInstruction
        merge ops (tag,Left f) = (\_ -> f . ops tag)
        merge ops (tag,Right g) = (\_ -> g tag . ops tag)
        ((_,e1):xs) = map (\x -> (fst x,alterOrbit x)) ys
        x0 = either (\f -> (\_ imap -> f imap)) (\g -> (\pos imap -> g pos imap)) e1

type OrbitInstruction' = Either (IntMap Orbits -> IntMap Orbits)
                                (Position -> IntMap Orbits -> IntMap Orbits)

alterOrbit :: (Tag,AlterOrbit) -> OrbitInstruction'
alterOrbit (tag,AlterReset) = Left $ IMap.delete tag
alterOrbit (tag,AlterLeave) = Left $ IMap.adjust escapeOrbit tag
  where escapeOrbit x = x {inOrbit = False}
alterOrbit (tag,AlterModify {newInOrbit = inOrbit',freshOrbit = True}) =
  Left $ IMap.insert tag (Orbits {inOrbit = inOrbit', getOrbits = mempty })
--  Right $ (\pos -> IMap.insert tag (Orbits {inOrbit = inOrbit', getOrbits = S.singleton ps }))
alterOrbit (tag,AlterModify {newInOrbit = inOrbit',freshOrbit = False}) =
  Right $ (\pos -> IMap.insertWith (\_ old -> old { getOrbits = getOrbits old |> pos} )
                                   tag
                                   (Orbits {inOrbit = inOrbit', getOrbits = mempty})) -- S.singleton pos}))

assemble :: TagList -> CompileIntructions ()
assemble spec = sequence_ . map helper $ spec where
  helper (tag,command) =
    case command of
      PreUpdate TagTask -> setPreTag tag
      PreUpdate ResetGroupStopTask -> resetTag tag
      PreUpdate ResetOrbitTask -> resetOrbit tag
      PreUpdate EnterOrbitTask -> enterOrbit tag
      PreUpdate LeaveOrbitTask -> leaveOrbit tag
      PostUpdate TagTask -> setPostTag tag
      PostUpdate ResetGroupStopTask -> resetTag tag
      _ -> err ("assemble : Weird orbit command: "++show (tag,update,spec))

toInstructions :: TagList -> Instructions
toInstructions spec =
  let todo = assemble spec
      initalState = (mempty,mempty,mempty)
      (a,b,c) = execState todo initalState
  in Instructions {newPos = IMap.toList a
                  ,newFlags = IMap.toList b
                  ,newOrbits = alterOrbits (IMap.toList c)}
