module Text.Regex.TDFA.RunMutState where

import Control.Monad(forM_,when,liftM,liftM2,liftM3,guard,foldM)
import Control.Monad.ST.Strict as S (ST)
import qualified Control.Monad.ST.Lazy as L (ST)
import Control.Monad.State(MonadState(..),execState)

import Data.Array.Base(unsafeRead,unsafeWrite,STUArray(..))
import GHC.Arr(STArray(..))
import GHC.ST(ST(..))
import GHC.Prim(MutableByteArray#,RealWorld,Int#,sizeofMutableByteArray#,unsafeCoerce#)

import Data.Array.MArray(MArray(..),readArray,writeArray,newListArray,thaw,freeze,unsafeFreeze,getAssocs)
--import Data.Array.ST() -- (ST(..),STArray,STUArray)
import Data.Array.Unboxed(UArray)
import Data.Array.IArray(Array,(!),array,bounds,assocs) -- ,accumArray,(//))

import Data.IntMap(IntMap)
import qualified Data.IntMap as IMap -- (insert,toList,insertWith)
import Data.Ix(Ix(..))
import Data.Maybe(fromJust)
import Data.Monoid(Monoid(..))
import Data.Sequence as S((|>),viewl,ViewL(..),singleton) -- empty XXX  XXX
import Data.STRef

import Text.Regex.Base(MatchArray,MatchOffset,MatchLength)
import Text.Regex.TDFA.Common

-- import Debug.Trace

{- By Chris Kuklewicz, 2007. BSD License, see the LICENSE file. -}

err :: String -> a
err s = common_error "Text.Regex.TDFA.RunMutState"  s

{-# INLINE newTagEngine #-}
newTagEngine regexIn = do
  (which,count) <- newBoard regexIn
  let comp = makeTagComparer (regex_tags regexIn)
  let findTrans s1 off trans = {-# SCC "findTrans" #-} (mapM_ findTrans' (IMap.toList trans)) where
        findTrans' (destIndex,sources) | IMap.null sources =
          writeArray which destIndex (-1,undefined,undefined)
                                       | otherwise =  {-# SCC "findTrans'" #-} do
          let (first:rest) = IMap.toList sources
              {-# INLINE prep #-}
              prep (sourceIndex,(_,instructions)) = {-# SCC "prep" #-} do
                p <- maybe (error "findtrans") return =<< unsafeRead (m_pos s1) sourceIndex
                o <- readArray (m_orbit s1) sourceIndex
                let o' = maybe o (\x -> x off o) (newOrbits instructions)
                return ((sourceIndex,instructions),p,o')
              challenge x1@(_,_,o1) y1 = {-# SCC "challenge" #-} do
                x2@(_,_,o2) <- prep y1
                check <- comp off x1 (newPos . snd . fst3 $ x1) x2 (newPos . snd . fst3 $ x2)
        {-
                debug1 <- getAssocs (snd3 x1)
                debug2 <- getAssocs (snd3 x2)
                () <- trace ("findTrans comp, pos="++show off'++", check="++show check
                             ++"\n"++show (debug1,fst3 x1,o1)
                             ++ "\n"++show (debug2,fst3 x2,o2)) (return ())
        -}
                if check==LT then return x2 else return x1
          x1 <- prep first
          ((sourceIndex',instructions'),_,orbit') <- foldM challenge x1 rest
          unsafeWrite which destIndex (sourceIndex',instructions',orbit')
          unsafeRead count sourceIndex' >>= (unsafeWrite count sourceIndex') . succ

  let {-# INLINE updateWinner #-}
      updateWinner s1 (off,prev,input) winning sources | IMap.null sources = return winning
                                                       | otherwise = {-# SCC "updateWinner" #-} do
        let (first:rest) = IMap.toList sources
            {-# INLINE prep #-}
            prep x@(sourceIndex,instructions) = do
              p <- maybe (error "updateWinner") return =<< unsafeRead (m_pos s1) sourceIndex
              o <- readArray (m_orbit s1) sourceIndex
              let o' = maybe o (\x -> x off o) (newOrbits instructions)
              return (x,p,o')
            challenge x1 y1 = do
              x2 <- prep y1
              check <- comp off x1 (dropWhile ((1>=).fst) . newPos . snd . fst3 $ x1)
                                x2 (dropWhile ((1>=).fst) . newPos . snd . fst3 $ x2)
      {-
                     debug1 <- getAssocs (snd3 x1)
                     debug2 <- getAssocs (snd3 x2)
                     () <- trace ("updateWinner comp, pos="++show off++", check="++show check
                                  ++"\n"++show (debug1,fst3 x1,thd3 x1)
                                  ++ "\n"++show (debug2,fst3 x2,thd3 x2)) (return ())
      -}
              if check==LT then return x2 else return x1
        x1 <- prep first
        ((sourceIndex',instructions'),_,o') <- foldM challenge x1 rest
        n <- unsafeRead count sourceIndex'
        w <- updateWinning s1 (sourceIndex',instructions',o') off n (fmap fst winning)
        return (Just (w,(off,prev,input)))

  let performTrans s1 s2 off dtrans | IMap.null dtrans = return ()
                                    | otherwise = {-# SCC "performTrans" #-} do
        mapM_ performTrans' (IMap.keys dtrans)
          where performTrans' destIndex =  {-# SCC "performTrans'" #-} do
                  i1@(sourceIndex,_instructions,_orbit) <- unsafeRead which destIndex
                  if sourceIndex == (-1) then return () else do
                  n <- unsafeRead count sourceIndex
                  unsafeWrite count sourceIndex (pred n)
                  if n==1 then updateSwap s1 i1 off s2 destIndex
                          else updateCopy s1 i1 off s2 destIndex
-- findTrans :: forall s. ({-Dest-}Index,IntMap {-Source-} (DoPa,Instructions)) -> ST s ()
-- updateWinner :: IntMap {- Source -} Instructions -> ST s (Maybe (WScratch s,(Position,Char,String)))
-- performTrans :: IntMap {-Dest-} (IntMap {-Source-} (DoPa,Instructions)) -> ST s ()
  return (findTrans,updateWinner,performTrans)

-- XXX change first element type to store winning orbit' and such?
newBoard :: Regex -> ST s (STArray s Index (Index,Instructions,OrbitLog)
                          ,STUArray s Index Int)
newBoard regexIn = do
  let bWhich = (0,regex_init regexIn) -- (-1) index is winning state
      bCount = (0,regex_init regexIn)
  liftM2 (,) (newListArray bWhich [(-1,error ("ins "++show i),error ("orbitlog"++show i)) | i <- range bWhich])
             (newArray bCount 0)

newA' :: (MArray (STArray s) e (ST s)) => (Tag,Tag) -> e -> ST s (STArray s Tag e)
newA' b_tags initial = -- traceNew ("> newA' "++show b_tags) $
                       newArray b_tags initial

newA'_ :: (MArray (STArray s) e (ST s)) => (Tag,Tag) -> ST s (STArray s Tag e)
newA'_ b_tags = -- traceNew ("> newA'_ "++show b_tags) $
                newArray_ b_tags

newA :: (MArray (STUArray s) e (ST s)) => (Tag,Tag) -> e -> ST s (STUArray s Tag e)
newA b_tags initial = -- traceNew ("> newA "++show b_tags) $
                      newArray b_tags initial

newA_ :: (MArray (STUArray s) e (ST s)) => (Tag,Tag) -> ST s (STUArray s Tag e)
newA_ b_tags = -- traceNew ("> newA_ "++show b_tags) $
               newArray_ b_tags

-- need forall
duplicateAInt :: forall s. (MArray (STUArray s) Int (ST s)) =>  STUArray s Tag Int -> ST s (STUArray s Tag Int)
duplicateAInt s = do i <- unsafeFreeze s :: ST s (UArray Tag Int)
--                     traceCopy ("> duplicateAInt") $ do
                     thaw i

-- need forall
duplicateABool :: forall s. (MArray (STUArray s) Bool (ST s)) =>  STUArray s Tag Bool -> ST s (STUArray s Tag Bool)
duplicateABool s = do i <- unsafeFreeze s :: ST s (UArray Tag Bool)
--                    traceCopy ("> duplicateABool") $ do
                      thaw i

showArr :: (MArray a (b) m,Ix i,Show i,Show b) => a i (b) -> m String
showArr a = do
  ss <- getAssocs a
  return (show ss)

seeMS :: MScratch s -> ST s String
seeMS s = do
  p <- getAssocs (m_pos s) >>= mapM (\(t,ma)-> (maybe (return []) getAssocs ma >>= return . ((,) t)))
  f <- getAssocs (m_flag s) >>= mapM (\(t,ma)-> (maybe (return []) getAssocs ma >>= return . ((,) t)))
  o <- getAssocs (m_orbit s) >>= mapM (\(t,o)-> return (IMap.toList o) >>= return . ((,) t))
  return $ unlines [show p,show f,show o]

data MScratch s = MScratch { m_pos :: !(STArray s Index (Maybe (STUArray s Tag Position)))
                           , m_flag :: !(STArray s Index (Maybe (STUArray s Tag Bool)))
                           , m_orbit :: !(STArray s Index OrbitLog) -- Fixed!
                           }
data SScratch s= SScratch { s_1 :: !(MScratch s)
                          , s_2 :: !(MScratch s)
                          , w_blank :: !(WScratch s)
                          }
data WScratch s = WScratch { w_pos :: !(STRef s (STUArray s Tag Position))
                           , w_flag :: !(STRef s (STUArray s Tag Bool))
                           , w_orbit :: !(STRef s OrbitLog)
                           }

seeWS :: WScratch s -> ST s String
seeWS w = do
  p <- getAssocs =<< readSTRef (w_pos w)
  f <- getAssocs =<< readSTRef (w_flag w)
  o <- return . IMap.toList =<< readSTRef (w_orbit w)
  return $ unlines [show p, show f, show o]

freezeScratch :: WScratch s -> ST s Scratch
freezeScratch (WScratch p f o) = do -- traceCopy ("> freezeScratch") $ do
                                 liftM3 Scratch (freeze =<< readSTRef p)
                                                (freeze =<< readSTRef f)
                                                (readSTRef o)

newWScratch :: (Tag,Tag) -> ST s (WScratch s)
newWScratch b_tags =  liftM3 WScratch (newSTRef =<< newA b_tags (-1))
                                      (newSTRef =<< newA b_tags False)
                                      (newSTRef mempty)

newWScratch_ :: (Tag,Tag) -> ST s (WScratch s)
newWScratch_ b_tags = liftM3 WScratch (newSTRef =<< newA_ b_tags)
                                      (newSTRef =<< newA_ b_tags)
                                      (newSTRef mempty)

fillArray :: STArray s Int Orbits -> Orbits -> ST s (STArray s Int Orbits)
fillArray m v = do
  x@(0,b) <- getBounds m
  forM_ (range x) (\i -> unsafeWrite m i v)
  return m

resetScratch :: Regex -> Position -> MScratch s -> WScratch s -> ST s ()
resetScratch regexIn startPos s1 w0 = do
  let i = regex_init regexIn
      b_tags = bounds (regex_tags regexIn)

  oldPos <- unsafeRead (m_pos s1) i
  initialPos <- case oldPos of
                  Nothing -> newA b_tags (-1)
                  Just pos -> do blank <- readSTRef (w_pos w0)
                                 copySTU blank pos
                                 return pos
  unsafeWrite initialPos 0 startPos
  unsafeWrite (m_pos s1) i (Just initialPos)

  oldFlags <- unsafeRead (m_flag s1) i
  initFlags <- case oldFlags of
                 Nothing -> newA b_tags False
                 Just flags -> do
                   blank <- readSTRef (w_flag w0)
                   copySTU blank flags
                   return flags
  unsafeWrite initFlags 0 True
  unsafeWrite (m_flag s1) i (Just initFlags)

  writeArray (m_orbit s1) i mempty

newScratch :: Regex -> Position -> ST s (SScratch s)
newScratch regexIn startPos = do
  let i = regex_init regexIn
      b_index = (0,i)
      b_tags = bounds (regex_tags regexIn)
--  trace ("\n> newScratch: "++show (b_index,b_tags,i,startPos)) $ do
  s@(SScratch {s_1=s1,w_blank=w0}) <- newSScratch b_index b_tags
  resetScratch regexIn startPos s1 w0
  return s

newSScratch b_index b_tags = do
  s1 <- newMScratch b_index
  s2 <- newMScratch b_index
  w0 <- newWScratch b_tags
  return (SScratch s1 s2 w0)

newMScratch b_index = do
  let n = rangeSize b_index
  pos <- newListArray b_index (replicate n Nothing)
  flag <- newListArray b_index (replicate n Nothing)
  orbit <- newListArray b_index (replicate n mempty)
  return (MScratch pos flag orbit)

{-# INLINE copyUpdateTags #-}
copyUpdateTags :: (MArray (STUArray s) Position (ST s))
                  => STUArray s Tag Position   -- source
                    -> [(Tag,Bool)]            -- updates
                    -> Position -> Position
                    -> STUArray s Tag Position   -- destination
                    -> (ST s) ()
copyUpdateTags !a1 !changes !pFalse !pTrue !a2 = do
  (start,stop) <- getBounds a1
--  traceCopy ("> copySTU copyUpdate"++show (start,stop)) $ do
  copySTU a1 a2
  mapM_ (\(tag,v) -> if v then unsafeWrite a2 tag pTrue
                          else unsafeWrite a2 tag pFalse) changes

{-# INLINE copyUpdateFlags #-}
copyUpdateFlags :: (MArray (STUArray s) Bool (ST s))
                   => STUArray s Tag Bool   -- source
                     -> [(Tag,Bool)]          -- updates
                     -> STUArray s Tag Bool   -- destination
                     -> (ST s) ()
copyUpdateFlags !a1 !changes !a2 = do
  (start,stop) <- getBounds a1
--  traceCopy ("> copySTU copyUpdate"++show (start,stop)) $ do
  copySTU a1 a2
  mapM_ (\(tag,v) -> unsafeWrite a2 tag v) changes

copyUpdateApplied :: STArray s Tag Orbits          -- source 
                  -> [(Tag, Orbits -> Orbits)]     -- changes
                  -> STArray s Tag Orbits          -- destination
                  -> ST s ()
copyUpdateApplied !a1 !changes !a2 = do
  (start,stop) <- getBounds a1
--  traceCopy ("> copyUpdateApplied"++ show (start,stop,length changes)) $ do
  let act a b | seq a $ seq b $ False = undefined
      act _ x | x > stop = return ()
      act [] x = do unsafeRead a1 x >>= unsafeWrite a2 x
                    act [] $! succ x
      act todo@((t,v):rest) x | t==x = do unsafeRead a1 x >>= return . v >>= unsafeWrite a2 x
                                          act rest $! succ x
                              | otherwise =  do unsafeRead a1 x >>= unsafeWrite a2 x
                                                act todo $! succ x
  act changes start

updateWinning :: MScratch s         -- source 
  -> ({-Source -} Index,Instructions,OrbitLog)
  -> Position
  -> Int
  -> Maybe (WScratch s)              -- destination
  -> ST s (WScratch s)
updateWinning !s1 (i1,ins,o) preTag n mw = do
  (Just pos1) <- unsafeRead (m_pos s1) i1
  (Just flag1) <- unsafeRead (m_flag s1) i1
  let val x = if x then postTag else preTag
      postTag = succ preTag
  if n==0
    then do
      mapM_ (\(tag,v) -> unsafeWrite pos1 tag (val v)) (newPos ins)
      mapM_ (\(tag,f) -> unsafeWrite flag1 tag (f)) (newFlags ins)
      case mw of
        Nothing -> liftM3 WScratch (newSTRef pos1) (newSTRef flag1) (newSTRef o)
        Just w -> do writeSTRef (w_pos w) pos1
                     writeSTRef (w_flag w) flag1
                     writeSTRef (w_orbit w) o
                     return w
    else do
      w <- case mw of
             Nothing -> getBounds pos1 >>= newWScratch_
             Just w -> return w
      pos2 <- readSTRef (w_pos w)
      flag2 <- readSTRef (w_flag w)
      copyUpdateTags pos1 (newPos ins) preTag postTag pos2
      copyUpdateFlags flag1 (newFlags ins) flag2
      writeSTRef (w_orbit w) o
      return w

updateSwap :: MScratch s         -- source 
           -> ({-Source -} Index,Instructions,OrbitLog)
           -> Position
           -> MScratch s -> Index        -- destination
           -> ST s ()
updateSwap !s1  (i1,ins,o) preTag s2 i2 = do
  -- obtain source
  pos1'@(Just pos1) <- unsafeRead (m_pos s1) i1
  flag1'@(Just flag1) <- unsafeRead (m_flag s1) i1
  -- preserve allocated storage in detination rather than cycle through GC
  unsafeWrite (m_pos s1) i1 =<< unsafeRead (m_pos s2) i2
  unsafeWrite (m_flag s1) i1 =<< unsafeRead (m_flag s2) i2
  -- put source in destination
  unsafeWrite (m_pos s2) i2 pos1'
  unsafeWrite (m_flag s2) i2 flag1'
  writeArray (m_orbit s2) i2 o           --- XXX ???
  let val x = if x then postTag else preTag where postTag = succ preTag
  mapM_ (\(tag,v) -> unsafeWrite pos1 tag (val v)) (newPos ins)
  mapM_ (\(tag,f) -> unsafeWrite flag1 tag (f)) (newFlags ins)

updateCopy :: MScratch s         -- source 
           -> ({-Source -} Index,Instructions,OrbitLog)
           -> Position
           -> MScratch s -> Index        -- destination
           -> ST s ()
updateCopy s1 (i1,ins,o) preTag s2 i2 = do
  pos1 <- maybe (err $ "forceUpdate : m_pos s1 is Nothing" ++ show (i1,ins,preTag)) return =<< unsafeRead (m_pos s1) i1
  flag1 <- maybe (err $ "forceUpdate : m_flag s1 is Nothing" ++ show (i1,ins,preTag)) return =<< unsafeRead (m_flag s1) i1
  b_tags <- getBounds pos1
  pos2 <- maybe (do a <- newA_ b_tags
                    unsafeWrite (m_pos s2) i2 (Just a)
                    return a) return =<< unsafeRead (m_pos s2) i2
  flag2 <- maybe (do a <- newA_ b_tags
                     unsafeWrite (m_flag s2) i2 (Just a)
                     return a) return =<< unsafeRead (m_flag s2) i2
  copyUpdateTags pos1 (newPos ins) preTag (succ preTag) pos2
  copyUpdateFlags flag1 (newFlags ins) flag2
  writeArray (m_orbit s2) i2 o

makeTagComparer :: Array Tag OP
                -> Position
		-> ((Int, Instructions), STUArray s Tag Position, IntMap Orbits)
		-> [(Int, Bool)]
		-> ((Int, Instructions), STUArray s Tag Position, IntMap Orbits)
		-> [(Int, Bool)]
		-> ST s Ordering
makeTagComparer aTagOP = foldr ($) end (map chooseBranch
                                            (dropWhile ((1>=).fst)
                                                       (assocs aTagOP)))
  where chooseBranch (tag,Maximize) = challenge_Max tag
        chooseBranch (tag,Minimize) = challenge_Min tag
        chooseBranch (tag,Orbit) = challenge_Orb tag
        end _ _ _ _ _ = return EQ

challenge_Orb tag next preTag x1@(_state1,pos1,orbit1') np1 x2@(_state2,pos2,orbit2') np2 = 
  let s1 = IMap.lookup tag orbit1'
      s2 = IMap.lookup tag orbit2'
  in case (s1,s2) of
       (Nothing,Nothing) -> next preTag x1 np1 x2 np2
       (Just o1,Just o2) | inOrbit o1 == inOrbit o2 ->
          case comparePos (viewl (getOrbits o1)) (viewl (getOrbits o2)) of
            EQ -> next preTag x1 np1 x2 np2
            answer -> return answer
       _ -> err $ "challenge_Orb is too stupid to handle mismatched orbit data :"
                  ++ show(tag,preTag,np1,np2)
  where comparePos :: (ViewL Position) -> (ViewL Position) -> Ordering
        comparePos EmptyL EmptyL = EQ
        comparePos EmptyL _      = GT
        comparePos _      EmptyL = LT
        comparePos (p1 :< ps1) (p2 :< ps2) = 
          compare p1 p2 `mappend` comparePos (viewl ps1) (viewl ps2)
 

-- challenge_pos takes the current winner and a challenger, each with instructions.
-- But the orbits are already modified.
challenge_Max tag next preTag x1@(_state1,pos1,orbit1') np1 x2@(_state2,pos2,orbit2') np2 = do
  (np1',p1) <- case np1 of
                 ((t,p):rest) | t==tag -> return (rest,if p then succ preTag else preTag)
                 _ -> liftM ((,) np1) (unsafeRead pos1 tag)
  (np2',p2) <- case np2 of
                 ((t,p):rest) | t==tag -> return (rest,if p then succ preTag else preTag)
                 _ -> liftM ((,) np2) (unsafeRead pos2 tag)
  case (p1,p2) of
    (-1,-1) -> next preTag x1 np1' x2 np2'
    (_ ,-1) -> return GT
    (-1, _) -> return LT
    _ -> let answer = compare p1 p2
         in if answer == EQ then next preTag x1 np1' x2 np2'
                            else return answer

-- challenge_pos takes the current winner and a challenger, each with instructions.
-- But the orbits are already modified.
challenge_Min tag next preTag x1@(_state1,pos1,orbit1') np1 x2@(_state2,pos2,orbit2') np2 = do
  (np1',p1) <- case np1 of
                 ((t,p):rest) | t==tag -> return (rest,if p then succ preTag else preTag)
                 _ -> liftM ((,) np1) (unsafeRead pos1 tag)
  (np2',p2) <- case np2 of
                 ((t,p):rest) | t==tag -> return (rest,if p then succ preTag else preTag)
                 _ -> liftM ((,) np2) (unsafeRead pos2 tag)
  case (p1,p2) of
    (-1,-1) -> next preTag x1 np1' x2 np2'
    (_ ,-1) -> return LT
    (-1, _) -> return GT
    _ -> let answer = compare p2 p1
         in if answer == EQ then next preTag x1 np1' x2 np2'
                            else return answer

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

alterOrbits :: [(Tag,AlterOrbit)] -> (Position -> OrbitTransformer)
alterOrbits x = let items = map alterOrbit x
                in (\pos m -> foldl (flip ($)) m (map ($ pos) items))

alterOrbit :: (Tag,AlterOrbit) -> (Position -> OrbitTransformer)
alterOrbit (tag,AlterModify {newInOrbit = inOrbit',freshOrbit = True}) =
  (\_ m -> IMap.insert tag (Orbits {inOrbit = inOrbit', getOrbits = mempty}) m)
alterOrbit (tag,AlterModify {newInOrbit = inOrbit',freshOrbit = False}) =
  (\pos m -> IMap.insertWithKey (updateOrbit pos) tag newOrbit m) where
  newOrbit = Orbits {inOrbit = inOrbit', getOrbits = mempty}
  updateOrbit pos _tag new old =
    let answer = case old of
                   Orbits True prev -> Orbits {inOrbit = inOrbit', getOrbits = prev |> pos }
                   Orbits False _   -> new
    in answer
alterOrbit (tag,AlterReset) = (\_ m -> IMap.delete tag m)
alterOrbit (tag,AlterLeave) = (\_ m -> 
    let old = IMap.lookup tag m
        answer = case old of
                   Nothing -> m
                   Just x -> IMap.insert tag (escapeOrbit x) m
    in answer)
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
                  ,newOrbits = if IMap.null c then Nothing else Just $ alterOrbits (IMap.toList c)}

tagsToGroupsST :: forall s. Array GroupIndex [GroupInfo] -> WScratch s -> ST s MatchArray
tagsToGroupsST aGroups (WScratch {w_pos=pRef,w_flag=fRef})= do
  let b_max = snd (bounds (aGroups))
  ma <- newArray (0,b_max) (-1,0) :: ST s (STArray s Int (MatchOffset,MatchLength))
  p <- readSTRef pRef
  f <- readSTRef fRef
  startPos0 <- unsafeRead p 0
  stopPos0 <- unsafeRead p 1
  unsafeWrite ma 0 (startPos0,stopPos0-startPos0)
  let get this_index [] = return ()
      get this_index ((GroupInfo _ parent start stop):gs) = do
        f <- unsafeRead f stop
        if not f then get this_index gs
          else do
        startPos <- unsafeRead p start
        stopPos <- unsafeRead p stop
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

foreign import ccall unsafe "memcpy"
    memcpy :: MutableByteArray# RealWorld -> MutableByteArray# RealWorld -> Int# -> IO ()

{-# INLINE copySTU #-}
copySTU :: (Show i,Ix i,MArray (STUArray s) e (ST s)) => STUArray s i e -> STUArray s i e -> ST s ()
copySTU !s1@(STUArray _ _ msource) !s2@(STUArray _ _ mdest) =
-- do b1 <- getBounds s1
--  b2 <- getBounds s2
--  when (b1/=b2) (error ("\n\nWTF copySTU: "++show (b1,b2)))
  ST $ \s1# ->
    case sizeofMutableByteArray# msource        of { n# ->
    case unsafeCoerce# memcpy mdest msource n# s1# of { (# s2#, () #) ->
    (# s2#, () #) }}

{-  Copied this block from GHC sources

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


{-  Commented out -- more useful to non-GHC compilers

copySTArray :: (MArray (STUArray s) e (ST s))=> STUArray s Tag e -> STUArray s Tag e -> ST s ()
copySTArray source destination = do
  b@(start,stop) <- getBounds source
  b' <- getBounds destination
  traceCopy ("> copySTArray "++show b) $ do
  when (b/=b') (fail $ "Text.Regex.TDFA.RunMutState copySTUArray bounds mismatch"++show (b,b'))
  forM_ (range b) $ \index ->
    unsafeRead source index >>= unsafeWrite destination index
-}
