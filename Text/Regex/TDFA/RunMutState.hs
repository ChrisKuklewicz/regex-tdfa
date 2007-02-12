module Text.Regex.TDFA.RunMutState where

import Control.Monad(forM_,when,liftM3,guard)
import Control.Monad.ST.Strict as S (ST)
import qualified Control.Monad.ST.Lazy as L (ST)
import Control.Monad.State(MonadState(..),execState)

import Data.Array.Base(unsafeRead,unsafeWrite,STUArray(..))
import GHC.Arr(STArray(..))
import GHC.ST(ST(..))
import GHC.Prim(MutableByteArray#,RealWorld,Int#,sizeofMutableByteArray#,unsafeCoerce#)

import Data.Array.MArray(MArray(..),readArray,writeArray,newListArray,thaw,freeze,unsafeFreeze)
--import Data.Array.ST() -- (ST(..),STArray,STUArray)
import Data.Array.Unboxed(UArray)
import Data.Array.IArray(Array,(!),array,bounds,assocs) -- ,accumArray,(//))

import Data.IntMap(IntMap)
import qualified Data.IntMap as IMap(insert,toList,insertWith)
import Data.Ix(Ix(..))
import Data.Maybe(fromJust)
import Data.Monoid(Monoid(..))
import Data.Sequence as S((|>),viewl,ViewL(..),singleton) -- empty XXX  XXX

import Text.Regex.Base(MatchArray,MatchOffset,MatchLength)
import Text.Regex.TDFA.Common

-- import Debug.Trace

traceNew,traceCopy :: String -> x -> x
traceNew _ = id
traceCopy _ = id

{- By Chris Kuklewicz, 2007. BSD License, see the LICENSE file. -}

err :: String -> a
err s = common_error "Text.Regex.TDFA.RunMutState"  s

{-
copySTArray :: (MArray (STUArray s) e (ST s))=> STUArray s Tag e -> STUArray s Tag e -> ST s ()
copySTArray source destination = do
  b@(start,stop) <- getBounds source
  b' <- getBounds destination
  traceCopy ("> copySTArray "++show b) $ do
  when (b/=b') (fail $ "Text.Regex.TDFA.RunMutState copySTUArray bounds mismatch"++show (b,b'))
  forM_ (range b) $ \index ->
    unsafeRead source index >>= unsafeWrite destination index
-}
newA' :: (MArray (STArray s) e (ST s)) => (Tag,Tag) -> e -> ST s (STArray s Tag e)
newA' b_tags initial = traceNew ("> newA' "++show b_tags) $
                       newArray b_tags initial

newA'_ :: (MArray (STArray s) e (ST s)) => (Tag,Tag) -> ST s (STArray s Tag e)
newA'_ b_tags = traceNew ("> newA'_ "++show b_tags) $
                newArray_ b_tags

newA :: (MArray (STUArray s) e (ST s)) => (Tag,Tag) -> e -> ST s (STUArray s Tag e)
newA b_tags initial = traceNew ("> newA "++show b_tags) $
                      newArray b_tags initial

newA_ :: (MArray (STUArray s) e (ST s)) => (Tag,Tag) -> ST s (STUArray s Tag e)
newA_ b_tags = traceNew ("> newA_ "++show b_tags) $
               newArray_ b_tags

-- need forall
duplicateAInt :: forall s. (MArray (STUArray s) Int (ST s)) =>  STUArray s Tag Int -> ST s (STUArray s Tag Int)
duplicateAInt s = do i <- unsafeFreeze s :: ST s (UArray Tag Int)
                     traceCopy ("> duplicateAInt") $ do
                     thaw i

-- need forall
duplicateABool :: forall s. (MArray (STUArray s) Bool (ST s)) =>  STUArray s Tag Bool -> ST s (STUArray s Tag Bool)
duplicateABool s = do i <- unsafeFreeze s :: ST s (UArray Tag Bool)
                      traceCopy ("> duplicateABool") $ do
                      thaw i

data MScratch s = MScratch { m_pos :: STArray s Index (Maybe (STUArray s Tag Position))
                           , m_flag :: STArray s Index (Maybe (STUArray s Tag Bool))
                           , m_orbit :: STArray s Index (Maybe (STArray s Tag Orbits))
                           }
data SScratch s= SScratch { s_1 :: MScratch s
                          , s_2 :: MScratch s
                          }

data WScratch s = WScratch { w_pos :: STUArray s Tag Position
                           , w_flag :: STUArray s Tag Bool
                           , w_orbit :: STArray s Tag Orbits
                           }

freezeScratch :: WScratch s -> ST s Scratch
freezeScratch (WScratch p f o) = traceCopy ("> freezeScratch") $ do
                                 liftM3 Scratch (freeze p) (freeze f) (freeze o)

newWScratch_ :: (Tag,Tag) -> ST s (WScratch s)
newWScratch_ b_tags = liftM3 WScratch (newA_ b_tags) (newA_ b_tags) (newA'_ b_tags)

emptyOrbits :: Orbits
emptyOrbits = Orbits False mempty

fillArray m v = do
  x@(0,b) <- getBounds m
  forM_ (range x) (\i -> unsafeWrite m i v)
  return m

resetScratch :: Regex -> Position -> MScratch s -> ST s ()
resetScratch regexIn startPos s1 = do
  let i = regex_init regexIn
      b_tags = bounds (regex_tags regexIn)
  oldPos <- unsafeRead (m_pos s1) i
  initialPos <- case oldPos of
                  Nothing -> newA b_tags (-1)
                  Just pos -> fillArray pos (-1)
  unsafeWrite initialPos 0 startPos
  unsafeWrite (m_pos s1) i (Just initialPos)
  oldFlags <- unsafeRead (m_flag s1) i
  initFlags <- case oldFlags of
                 Nothing -> newA b_tags False
                 Just flags -> fillArray flags False
  unsafeWrite initFlags 0 True
  unsafeWrite (m_flag s1) i (Just initFlags)
  oldOrbits <- unsafeRead (m_orbit s1) i
  initialOrbits <- case oldOrbits of
                     Nothing -> newA' b_tags emptyOrbits
                     Just orbits -> fillArray orbits emptyOrbits
  unsafeWrite (m_orbit s1) i (Just initialOrbits)

newScratch :: Regex -> Position -> ST s (SScratch s)
newScratch regexIn startPos = do
  let i = regex_init regexIn
      b_index = (0,i)
--  trace ("\n> newScratch: "++show (b_index,b_tags,i,startPos)) $ do
  s@(SScratch {s_1=s1}) <- newSScratch b_index
  resetScratch regexIn startPos s1
  return s

newSScratch b_index = do
  s1 <- newMScratch b_index
  s2 <- newMScratch b_index
  return (SScratch s1 s2)

newMScratch b_index = do
  let n = rangeSize b_index
  pos <- newListArray b_index (replicate n Nothing)
  flag <- newListArray b_index (replicate n Nothing)
  orbit <- newListArray b_index (replicate n Nothing)
  return (MScratch pos flag orbit)

copyUpdate :: (MArray (STUArray s) e (ST s)) => STUArray s Tag e   -- source
                                             -> [(Tag,e)]          -- updates
                                             -> STUArray s Tag e   -- destination
                                             -> (ST s) ()
copyUpdate a1 changes a2 = do
  (start,stop) <- getBounds a1
  traceCopy ("> copySTU copyUpdate"++show (start,stop)) $ do
  copySTU a1 a2
  mapM_ (\(tag,v) -> unsafeWrite a2 tag v) changes
{-
copyUpdate a1 changes a2 = do
  (start,stop) <- getBounds a1
  traceCopy ("> copyUpdate"++show (start,stop)) $ do
  let act a b | seq a $ seq b $ False = undefined
      act _ x | x > stop = return ()
      act [] x = do unsafeRead a1 x >>= unsafeWrite a2 x
                    act [] $! succ x
      act todo@((t,v):rest) x | t==x = do unsafeWrite a2 x v
                                          act rest $! succ x
                              | otherwise =  do unsafeRead a1 x >>= unsafeWrite a2 x
                                                act todo $! succ x
  act changes start
-}
copyUpdateApplied :: STArray s Tag Orbits          -- source 
                  -> [(Tag, Orbits -> Orbits)]     -- changes
                  -> STArray s Tag Orbits          -- destination
                  -> ST s ()
copyUpdateApplied a1 changes a2 = do
  (start,stop) <- getBounds a1
  traceCopy ("> copyUpdateApplied"++ show (start,stop,length changes)) $ do
  let act a b | seq a $ seq b $ False = undefined
      act _ x | x > stop = return ()
      act [] x = do unsafeRead a1 x >>= unsafeWrite a2 x
                    act [] $! succ x
      act todo@((t,v):rest) x | t==x = do unsafeRead a1 x >>= return . v >>= unsafeWrite a2 x
                                          act rest $! succ x
                              | otherwise =  do unsafeRead a1 x >>= unsafeWrite a2 x
                                                act todo $! succ x
  act changes start

updateSwap :: MScratch s -> Index        -- source 
           -> Instructions -> Position   -- affects source
           -> MScratch s -> Index        -- destination
           -> ST s ()
updateSwap s1 i1 ins preTag s2 i2 = do
  b_index <- getBounds (m_pos s1)
  b_tags <- getBounds . fromJust =<< unsafeRead (m_pos s1) i1
--  trace ("\n> forceUpdate :"++show (b_index,b_tags,i1,ins,preTag,i2)) $ do
  pos1'@(Just pos1) <- unsafeRead (m_pos s1) i1
  flag1'@(Just flag1) <- unsafeRead (m_flag s1) i1
  orbit1'@(Just orbit1) <-unsafeRead (m_orbit s1) i1
  unsafeWrite (m_pos s1) i1 =<< unsafeRead (m_pos s2) i2
  unsafeWrite (m_flag s1) i1 =<< unsafeRead (m_flag s2) i2
  unsafeWrite (m_orbit s1) i1 =<< unsafeRead (m_orbit s2) i2
  unsafeWrite (m_pos s2) i2 pos1'
  unsafeWrite (m_flag s2) i2 flag1'
  unsafeWrite (m_orbit s2) i2 orbit1'
  let val x = if x then postTag else preTag where postTag = succ preTag
  mapM_ (\(tag,v) -> unsafeWrite pos1 tag (val v)) (newPos ins)
  mapM_ (\(tag,f) -> unsafeWrite flag1 tag (f)) (newFlags ins)
  mapM_ (\(tag,o) -> unsafeRead orbit1 tag >>= return . o preTag >>= unsafeWrite orbit1 tag) (newOrbits ins)

-- | Overwrite Index in s2!i2 (or allocate and fill space) with
-- updated s1!i1 using instructions ins and current position pos.
forceUpdate :: MScratch s -> Index        -- source 
            -> Instructions -> Position   -- affects source
            -> MScratch s -> Index        -- destination
            -> ST s ()
forceUpdate s1 i1 ins preTag s2 i2 = do
  b_index <- getBounds (m_pos s1)
  b_tags <- getBounds . fromJust =<< unsafeRead (m_pos s1) i1
--  trace ("\n> forceUpdate :"++show (b_index,b_tags,i1,ins,preTag,i2)) $ do
  pos2 <- maybe (do a <- newA_ b_tags
                    unsafeWrite (m_pos s2) i2 (Just a)
                    return a) return =<< unsafeRead (m_pos s2) i2
  flag2 <- maybe (do a <- newA_ b_tags
                     unsafeWrite (m_flag s2) i2 (Just a)
                     return a) return =<< unsafeRead (m_flag s2) i2
  orbit2 <- maybe (do a <- newA'_ b_tags
                      unsafeWrite (m_orbit s2) i2 (Just a)
                      return a) return =<< unsafeRead (m_orbit s2) i2
  forceUpdateW s1 i1 ins preTag pos2 flag2 orbit2

-- | Overwrite Index in s2!i2 (or allocate and fill space) with
-- updated s1!i1 using instructions ins and current position pos.
{-
forceUpdateW :: MScratch s -> Index        -- source 
             -> Instructions -> Position   -- affects source
             -> MScratch s -> Index        -- destination
             -> ST s ()
-}
forceUpdateW s1 i1 ins preTag pos2 flag2 orbit2 = do
  let val x = if x then postTag else preTag where postTag = succ preTag
--  trace ("\n> forceUpdateW :"++show (i1,ins,preTag)) $ do
  pos1 <- maybe (err $ "forceUpdate : m_pos s1 is Nothing" ++ show (i1,ins,preTag)) return =<< unsafeRead (m_pos s1) i1
  copyUpdate pos1 (mapSnd val $ newPos ins) pos2

  flag1 <- maybe (err "forceUpdate : m_flag s1 is Nothing") return =<< unsafeRead (m_flag s1) i1
  copyUpdate flag1 (newFlags ins) flag2

  orbit1 <- maybe (err "forceUpdate : m_orbit s1 is Nothing") return =<< unsafeRead (m_orbit s1) i1
  copyUpdateApplied orbit1 (mapSnd ($ preTag) (newOrbits ins)) orbit2

mergeIns ins preTag = 
  let postTag = succ preTag
      pos' = mapSnd (\x -> if x then postTag else preTag) (newPos ins)
      orb' = mapSnd ($ preTag) (newOrbits ins)
      merge all1@((t1,a1):rest1) all2@((t2,a2):rest2) | t1 < t2 = (t1,Left a1) : merge rest1 all2
                                                      | otherwise = (t2,Right a2) : merge all1 rest2
      merge [] [] = []
      merge all1 [] = map (\(t,a) -> (t,Left a)) all1
      merge [] all2 = map (\(t,a) -> (t,Right a)) all2
  in merge pos' orb'

-- | Determine whether s1!i1 is better (GT) or the instructions
-- modifying s2!i2 is better (LT).
challenge :: Array Index OP             -- context
          -> MScratch s -> Index        -- current winner
          -> Instructions -> Position   -- affect challenger
          -> MScratch s -> Index        -- challenger
          -> ST s Ordering
challenge aTagOP s1 i1 insIn preTag s2 i2 = do
  pos1 <- maybe (err "challenge : m_pos s1 is Nothing") return =<< unsafeRead (m_pos s1) i1
  flag1 <- maybe (err "challenge : m_flag s1 is Nothing") return =<< unsafeRead (m_flag s1) i1
  orbit1 <- maybe (err "challenge : m_orbit s1 is Nothing") return =<< unsafeRead (m_orbit s1) i1
--  trace ("\n> challenge : "++show (i1,insIn,preTag,i2)) $ do
  challengeW aTagOP pos1 flag1 orbit1 insIn preTag s2 i2

-- | Determine whether s1!i1 is better (GT) or the instructions
-- modifying s2!i2 is better (LT).
{-
challengeW :: Array Index OP             -- context
          -> MScratch s -> Index        -- current winner
          -> Instructions -> Position   -- affect challenger
          -> MScratch s -> Index        -- challenger
          -> ST s Ordering
-}
challengeW aTagOP pos1 flag1 orbit1 insIn preTag s2 i2 = do
  (start,stop) <- getBounds pos1
  pos2 <- maybe (err "challenge : m_pos s2 is Nothing") return =<< unsafeRead (m_pos s2) i2
  flag2 <- maybe (err "challenge : m_flag s2 is Nothing") return =<< unsafeRead (m_flag s2) i2
  orbit2 <- maybe (err "challenge : m_orbit s2 is Nothing") return =<< unsafeRead (m_orbit s2) i2
--   trace ("\n> challengeW : "++show (insIn,preTag,i2)) $ do
  let toFar = succ stop
  let check ins i | i == toFar = return EQ
                  | otherwise = case aTagOP ! i of
                                  Orbit -> checkOrbit ins i
                                  Maximize -> checkPos id ins i
                                  Minimize -> checkPos flipOrder ins i
      {-# INLINE checkPos #-}
      checkPos f ins i = do
        p1 <- unsafeRead pos1 i
        (p2,ins') <- case ins of 
                       ((t,Left p):rest) | t==i -> return (p,rest)
                       _ -> unsafeRead pos2 i >>= \p -> return (p,ins)
        case (p1,p2) of
          (-1,-1) -> check ins' $! succ i
          ( _,-1) -> return $ f GT
          (-1, _) -> return $ f LT
          (p1,p2) -> case compare p1 p2 of
                       EQ -> check ins' $! succ i
                       x -> return $ f x
      checkOrbit ins i = do
        f1 <- unsafeRead flag1 i
        f2 <- unsafeRead flag2 i
        (o2,ins') <- do o2' <- unsafeRead orbit2 i
                        case ins of
                          ((t,Right o2o):rest) | t==i -> return (o2o o2',rest)
                          _ -> return (o2',ins)
        case (f1,f2) of
          (True,True) -> do o1 <- unsafeRead orbit1 i
                            case compareOrbits (viewl (getOrbits o1)) (viewl (getOrbits o2)) of
                              EQ -> check ins' $! succ i
                              x -> return x
          (False,False) -> check ins' $! succ i
          _ ->  err $ "challenge : Unexpected flags mismatch: " ++ show i
      compareOrbits EmptyL EmptyL = EQ
      compareOrbits EmptyL _      = GT
      compareOrbits _      EmptyL = LT
      compareOrbits (p1 :< ps1) (p2 :< ps2) = compare p1 p2 `mappend` compareOrbits (viewl ps1) (viewl ps2)
  check (mergeIns insIn preTag) 0

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

-}

                          

{-
newScratchMap :: Regex -> Position -> IntMap {- Index -} Scratch
newScratchMap regexIn offsetIn = IMap.singleton (regex_init regexIn) initScratch
  where tagRange = bounds (regex_tags regexIn)
        initScratch = Scratch { scratchPos = accumArray (\_ new -> new) (-1) tagRange [(0,offsetIn)]
                              , scratchFlags = accumArray (\_ new -> new) False tagRange [(0,True)]
                              , scratchOrbits = mempty }
-}

{-
{-# INLINE update #-}
update :: Instructions -> Position -> Scratch -> Scratch
update todo pos sIn =
  Scratch { scratchPos = (scratchPos sIn) // (map (\(tag,post) -> if post then (tag,pos')
                                                                    else (tag,pos)) (newPos todo))
          , scratchFlags = (scratchFlags sIn) // (newFlags todo)
          , scratchOrbits = (newOrbits todo) pos (scratchOrbits sIn)
          }
  where pos' = succ pos

update :: ST s () -> Position -> Scratch -> (Scratch,[String])
update rs p sIn = execRWS rs (p,succ p) sIn
-}
----------------------

modifyPos :: Bool -> Tag -> CompileInstructions ()
modifyPos todo tag = do
  (a,b,c) <- get
  let a' = IMap.insert tag todo a
      b' = IMap.insert tag True b
  put (a',b',c)

setPreTag :: Tag -> CompileInstructions ()
setPreTag = modifyPos False

setPostTag :: Tag -> CompileInstructions ()
setPostTag = modifyPos True

resetTag :: Tag -> CompileInstructions ()
resetTag tag = do
  (a,b,c) <- get
  let b' = IMap.insert tag False b
  put (a,b',c)

modifyOrbit :: (IntMap AlterOrbit -> IntMap AlterOrbit) -> CompileInstructions ()
modifyOrbit f = do
  (a,b,c) <- get
  let c' = f c
  put (a,b,c')

modifyFlagOrbit :: Tag -> Bool -> (IntMap AlterOrbit -> IntMap AlterOrbit) -> CompileInstructions ()
modifyFlagOrbit tag flag f = do
  (a,b,c) <- get
  let b' = IMap.insert tag flag b
      c' = f c
  put (a,b',c')

resetOrbit :: Tag -> CompileInstructions ()
resetOrbit tag = modifyFlagOrbit tag False (IMap.insert tag AlterReset)

leaveOrbit :: Tag -> CompileInstructions ()
leaveOrbit tag = modifyOrbit escapeOrbit where
  escapeOrbit = IMap.insertWith setInOrbitFalse tag AlterLeave where
    setInOrbitFalse _ x@(AlterModify {}) = x {newInOrbit = False}
    setInOrbitFalse _ x = x

enterOrbit :: Tag -> CompileInstructions ()
enterOrbit tag = modifyFlagOrbit tag True changeOrbit where
  changeOrbit = IMap.insertWith overwriteOrbit tag appendNewOrbit

  appendNewOrbit = AlterModify {newInOrbit = True, freshOrbit = False} -- try to append
  startNewOrbit  = AlterModify {newInOrbit = True, freshOrbit = True}   -- will start a new series

  overwriteOrbit _ AlterReset = startNewOrbit
  overwriteOrbit _ AlterLeave = startNewOrbit
  overwriteOrbit _ (AlterModify {newInOrbit = False}) = startNewOrbit
  overwriteOrbit _ (AlterModify {newInOrbit = True}) =
    err $ "enterOrbit: Cannot enterOrbit twice in a row: " ++ show tag

alterOrbits :: [(Tag,AlterOrbit)] -> [(Tag,OrbitInstruction)]
alterOrbits = map alterOrbit

alterOrbit :: (Tag,AlterOrbit) -> (Tag,OrbitInstruction)
alterOrbit (tag,AlterModify {newInOrbit = inOrbit',freshOrbit = True}) =
  (tag,(const (const (Orbits {inOrbit = inOrbit', getOrbits = mempty }))))
alterOrbit (tag,AlterModify {newInOrbit = inOrbit',freshOrbit = False}) =
  (tag,(\pos -> (\old -> case old of
                           Orbits True prev -> Orbits {inOrbit = inOrbit', getOrbits = prev |> pos }
                           Orbits False _   -> Orbits {inOrbit = inOrbit', getOrbits = mempty})))
alterOrbit (tag,AlterReset) = (tag,const (const emptyOrbits))
alterOrbit (tag,AlterLeave) = (tag,const escapeOrbit)
  where escapeOrbit x = x {inOrbit = False}

assemble :: TagList -> CompileInstructions ()
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
      _ -> err ("assemble : Weird orbit command: "++show (tag,spec))

toInstructions :: TagList -> Instructions
toInstructions spec =
  let todo = assemble spec
      initalState = (mempty,mempty,mempty)
      (a,b,c) = execState todo initalState
  in Instructions {newPos = IMap.toList a
                  ,newFlags = IMap.toList b
                  ,newOrbits = alterOrbits (IMap.toList c)}

{-
alterOrbits :: [(Tag,AlterOrbit)] -> [(Tag,OrbitInstruction)]
alterOrbits [] = (\_ -> id)
alterOrbits ys = foldl merge x0 xs
  where merge :: OrbitInstruction -> (Tag,OrbitInstruction') -> OrbitInstruction
        merge ops (tag,Left f) = (\_ -> f . ops tag)
        merge ops (tag,Right g) = (\_ -> g tag . ops tag)
        ((_,e1):xs) = map (\x -> (fst x,alterOrbit x)) ys
        x0 = either (\f -> (\_ imap -> f imap)) (\g -> (\pos imap -> g pos imap)) e1

-}

tagsToGroupsST :: forall s. Array GroupIndex [GroupInfo] -> WScratch s -> ST s MatchArray
tagsToGroupsST aGroups (WScratch {w_pos=p,w_flag=f})= do
  let b_max = snd (bounds (aGroups))
  ma <- newArray (0,b_max) (-1,0) :: ST s (STArray s Int (MatchOffset,MatchLength))
  startPos0 <- unsafeRead p 0
  stopPos0 <- unsafeRead p 1
  unsafeWrite ma 0 (startPos0,stopPos0-startPos0)
  let get this_index [] = return ()
      get this_index ((GroupInfo _ parent start stop):gs) = do
        f <- unsafeRead f stop
        if not f then get this_index gs
          else do
        startPos <- unsafeRead p start
        stopPos <- unsafeRead p start
        (startParent,lengthParent) <- unsafeRead ma parent
        let ok = (0 <= startParent &&
                  0 <= lengthParent &&
                  startParent <= startPos &&
                  stopPos <= startPos + lengthParent)
        if not ok then get this_index gs
          else unsafeWrite ma this_index (startPos,stopPos-startPos)
  forM_ (range (1,b_max)) $ (\i -> get i (aGroups!i))
  unsafeFreeze ma

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

{-

data STUArray s i a = STUArray !i !i (GHC.Prim.MutableByteArray# s)

thawSTUArray :: Ix i => UArray i e -> ST s (STUArray s i e)
thawSTUArray (UArray l u arr#) = ST $ \s1# ->
    case sizeofByteArray# arr#          of { n# ->
    case newByteArray# n# s1#           of { (# s2#, marr# #) ->
    case unsafeCoerce# memcpy marr# arr# n# s2# of { (# s3#, () #) ->
    (# s3#, STUArray l u marr# #) }}}

foreign import ccall unsafe "memcpy"
    memcpy :: MutableByteArray# RealWorld -> ByteArray# -> Int# -> IO ()
-}

foreign import ccall unsafe "memcpy"
    memcpy :: MutableByteArray# RealWorld -> MutableByteArray# RealWorld -> Int# -> IO ()

copySTU :: Ix i => STUArray s i e -> STUArray s i e -> ST s ()
copySTU (STUArray _ _ msource) (STUArray _ _ mdest) = ST $ \s1# ->
    case sizeofMutableByteArray# msource        of { n# ->
    case unsafeCoerce# memcpy mdest msource n# s1# of { (# s2#, () #) ->
    (# s2#, () #) }}
