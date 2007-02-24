
-- XXX design uncertainty:  should preResets be inserted into nullView?
-- if not, why not?

{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | "Text.Regex.TDFA.TNFA" converts the CorePattern Q/P data (and its
-- Pattern leafs) to a QNFA tagged non-deterministic finite automata.
-- 
-- This holds every possible way to follow one state by another, while
-- in the DFA these will be reduced by picking a single best
-- transition for each (soure,destination) pair.  The transitions are
-- heavily and often redundantly annotated with tasks to perform, and
-- this redundancy is reduced when picking the best transition.  So
-- far, keeping all this information has helped fix bugs in both the
-- design and implementation.
--
-- The QNFA for a Pattern with a starTraned Q/P form with N one
-- character accepting leaves has at most N+1 nodes.  These nodes
-- repesent the future choices after accepting a leaf.  The processing
-- of Or nodes often reduces this number by sharing at the end of the
-- different paths.  Turning off capturing while compiling the pattern
-- may (future extension) reduce this further for some patterns by
-- processing Star with optimizations.  This compact design also means
-- that tags are assigned not just to be updated before taking a
-- transition (PreUpdate) but also after the transition (PostUpdate).
-- 
-- Uses recursive do notation.

module Text.Regex.TDFA.TNFA(patternToNFA
                           ,QNFA(..),QT(..),QTrans,TagUpdate(..)) where

{- By Chris Kuklewicz, 2007. BSD License, see the LICENSE file. -}

import Control.Monad.State
import Data.Array.IArray(Array,array)
import Data.Char(toLower,toUpper,isAlpha,ord)
import Data.List(foldl')
import Data.IntMap.CharMap(CharMap(..))
import qualified Data.IntMap.CharMap as Map(size,null,toAscList,singleton,map)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IMap(size,toList,toAscList,null,unionWith,singleton,fromList,fromDistinctAscList)
import Data.Maybe(catMaybes)
import Data.Monoid(mempty,mappend)
import Data.IntSet.EnumSet(EnumSet)
import qualified Data.IntSet.EnumSet as Set(singleton,toList,insert)
import qualified Data.IntMap.EnumMap as EMap(null,keysSet,assocs)
import qualified Data.Set(insert,toAscList)

import Text.Regex.TDFA.Common
import Text.Regex.TDFA.CorePattern(Q(..),P(..),OP(..),WhichTest,cleanNullView,NullView
                                  ,SetTestInfo(..),Wanted(..),TestInfo
                                  ,mustAccept,cannotAccept,patternToQ)
import Text.Regex.TDFA.Pattern(Pattern(..))
import Text.Regex.TDFA.ReadRegex(decodePatternSet)
-- import Debug.Trace

err :: String -> a
err t = common_error "Text.Regex.TDFA.TNFA" t

debug :: (Show a) => a -> s -> s
debug _ s = s

instance Show QNFA where
  show (QNFA {q_id = i, q_qt = qt}) = "QNFA {q_id = "++show i
                                  ++"\n     ,q_qt = "++ show qt
                                  ++"\n}"

instance Show QT where
  show = showQT

showQT :: QT -> String
showQT (Simple win trans other) = "{qt_win=" ++ show win
                             ++ "\n, qt_trans=" ++ show (foo trans)
                             ++ "\n, qt_other=" ++ show (foo' other) ++ "}"
showQT (Testing test dopas a b) = "{Testing "++show test++" "++show (Set.toList dopas)
                              ++"\n"++indent a
                              ++"\n"++indent b++"}"
    where indent = init . unlines . map (spaces++) . lines . showQT
          spaces = replicate 9 ' '

foo :: CharMap QTrans -> [(Char,[(Index,[TagCommand])])]
foo = mapSnd foo' . Map.toAscList

foo' :: QTrans -> [(Index,[TagCommand])]
foo' = IMap.toList 

instance Eq QT where
  t1@(Testing {}) == t2@(Testing {}) =
    (qt_test t1) == (qt_test t2) && (qt_a t1) == (qt_a t2) && (qt_b t1) == (qt_b t2)
  (Simple w1 (CharMap t1) o1) == (Simple w2 (CharMap t2) o2) =
    w1 == w2 && eqTrans && eqQTrans o1 o2
    where eqTrans :: Bool
          eqTrans = (IMap.size t1 == IMap.size t2)
                    && and (zipWith together (IMap.toAscList t1) (IMap.toAscList t2))
            where together (c1,qtrans1) (c2,qtrans2) = (c1 == c2) && eqQTrans qtrans1 qtrans2
          eqQTrans :: QTrans -> QTrans -> Bool
          eqQTrans = (==)
  _ == _ = False

-- This uses the Eq QT instace above
-- ZZZ
mkTesting :: QT -> QT
mkTesting t@(Testing {qt_a=a,qt_b=b}) = if a==b then a else t -- Move to nfsToDFA XXX
mkTesting t = t

qtwin,qtlose :: QT
qtwin = Simple {qt_win=[(1,PreUpdate TagTask)],qt_trans=mempty,qt_other=mempty}
qtlose = Simple {qt_win=mempty,qt_trans=mempty,qt_other=mempty}

patternToNFA :: CompOption
             -> (Text.Regex.TDFA.Pattern.Pattern,(GroupIndex, DoPa))
             -> ((Index,Array Index QNFA)
                ,Array Tag OP
                ,Array GroupIndex [GroupInfo])
patternToNFA compOpt pattern =
  let (q,tags,groups) = patternToQ compOpt pattern
      msg = unlines [ show q ]
  in debug msg (qToNFA compOpt q,tags,groups)

-- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == 

-- dumb smart constructor used by qToQNFA
-- could replace with something that is
--  (*) Monadic, using uniq to auto generate the new i
--  (*) Puts the new QNFA into the State's (list->list) (so it is ascending in order)
--  (*) Actually creates a simple DFA instead?
mkQNFA :: Int -> QT -> QNFA
mkQNFA i qt = debug ("\n>QNFA id="++show i) $
  -- XXX Go through the qt and keep only the best tagged transition(s) to each state.
  QNFA i (debug ("\ngetting QT for "++show i) qt)

nullable :: Q -> Bool
nullable = not . null . nullQ

notNullable :: Q -> Bool
notNullable = null . nullQ

-- This asks if the preferred (i.e. first) NullView has no tests.
maybeOnlyEmpty :: Q -> Maybe WinTags
maybeOnlyEmpty (Q {nullQ = ((SetTestInfo sti,tags):_)}) = if EMap.null sti then Just tags else Nothing
maybeOnlyEmpty _ = Nothing

usesQNFA :: Q -> Bool
usesQNFA (Q {wants=WantsBoth}) = True
usesQNFA (Q {wants=WantsQNFA}) = True
usesQNFA _ = False

nullQT :: QT -> Bool
nullQT (Simple {qt_win=w,qt_trans=t,qt_other=o}) = noWin w && Map.null t && IMap.null o
nullQT _ = False

listTestInfo :: QT -> EnumSet WhichTest -> EnumSet WhichTest
listTestInfo qt s = execState (helper qt) s
  where helper (Simple {}) = return ()
        helper (Testing {qt_test = wt, qt_a = a, qt_b = b}) = do
          modify (Set.insert wt)
          helper a
          helper b
-- This is used to view "win" only through NullView
applyNullViews :: NullView -> QT -> QT
applyNullViews [] win = win
applyNullViews nvs win = foldl' (dominate win winTests) qtlose (reverse $ cleanNullView nvs) where
  winTests = listTestInfo win $ mempty

-- This is used to prefer to view "win" through NullView.  Losing is
-- replaced by the plain win.  This is employed by Star patterns to
-- express that the first iteration is allowed to match null, but
-- skipping the NullView occurs if the match fails.
preferNullViews :: NullView -> QT -> QT
preferNullViews [] win = win
preferNullViews nvs win = foldl' (dominate win winTests) win (reverse $ cleanNullView nvs) where
  winTests = listTestInfo win $ mempty

dominate :: QT -> EnumSet WhichTest -> QT -> (SetTestInfo,WinTags) -> QT
dominate win winTests lose x@(SetTestInfo sti,tags) = debug ("dominate "++show x) $
  let -- The winning states are reached through the SetTag
      win' = prependTags' tags win
      -- get the SetTestInfo 
      allTests = (listTestInfo lose $ EMap.keysSet sti) `mappend` winTests
      useTest _ [] w _ = w -- no more dominating tests to fail to choose lose, so just choose win
      useTest (aTest:tests) allD@((dTest,dopas):ds) w l =
        let (wA,wB,wD) = branches w
            (lA,lB,lD) = branches l
            branches qt@(Testing {}) | aTest==qt_test qt = (qt_a qt,qt_b qt,qt_dopas qt)
            branches qt = (qt,qt,mempty)
        in if aTest == dTest
             then Testing {qt_test = aTest
                          ,qt_dopas = (dopas `mappend` wD) `mappend` lD
                          ,qt_a = useTest tests ds wA lA
                          ,qt_b = lB}
             else Testing {qt_test = aTest
                          ,qt_dopas = wD `mappend` lD
                          ,qt_a = useTest tests allD wA lA
                          ,qt_b = useTest tests allD wB lB}
      useTest [] _ _  _ = err "This case in applyNullViews.useText cannot happen"
  in useTest (Set.toList allTests) (EMap.assocs sti) win' lose

applyTest :: TestInfo -> QT -> QT
applyTest (wt,dopa) qt | nullQT qt = qt
                       | otherwise = applyTest' qt where
  applyTest' :: QT -> QT
  applyTest' q@(Simple {}) =
    mkTesting $ Testing {qt_test = wt
                        ,qt_dopas = Set.singleton dopa
                        ,qt_a = q 
                        ,qt_b = qtlose}
  applyTest' q@(Testing {qt_test=wt'}) =
    case compare wt wt' of
      LT -> Testing {qt_test = wt
                    ,qt_dopas = Set.singleton dopa
                    ,qt_a = q
                    ,qt_b = qtlose}
      EQ -> q {qt_dopas = Set.insert dopa (qt_dopas q)
              ,qt_b = qtlose}
      GT -> q {qt_a = applyTest' (qt_a q)
              ,qt_b = applyTest' (qt_b q)}

mergeQT_2nd,mergeAltQT,mergeQT :: QT -> QT -> QT
mergeQT_2nd q1 q2 | nullQT q1 = q2  -- prefer winning with w1 then with w2
                  | otherwise = mergeQTWith (\_ w2 -> w2) q1 q2

mergeAltQT q1 q2 | nullQT q1 = q2  -- prefer winning with w1 then with w2
                 | otherwise = mergeQTWith (\w1 w2 -> if noWin w1 then w2 else w1) q1 q2
mergeQT q1 q2 | nullQT q1 = q2  -- union wins
              | nullQT q2 = q1  -- union wins
              | otherwise = mergeQTWith mappend q1 q2 -- no preference, win with combined SetTag XXX is the wrong thing! "(.?)*"

mergeQTWith :: (WinTags -> WinTags -> WinTags) -> QT -> QT -> QT
mergeQTWith mergeWins = merge where
  merge :: QT -> QT -> QT
  merge (Simple w1 t1 o1) (Simple w2 t2 o2) =
    let w' = mergeWins w1 w2
        t' = fuseQTrans t1 o1 t2 o2
        o' = mergeQTrans o1 o2
    in Simple w' t' o'
  merge s@(Simple {}) t@(Testing _ _ a b) = mkTesting $
    t {qt_a=(merge s a), qt_b=(merge s b)}
  merge t@(Testing _ _ a b) s@(Simple {}) = mkTesting $
    t {qt_a=(merge a s), qt_b=(merge b s)}
  merge t1@(Testing wt1 ds1 a1 b1) t2@(Testing wt2 ds2 a2 b2) = mkTesting $
    case compare wt1 wt2 of
      LT -> t1 {qt_a=(merge a1 t2), qt_b=(merge b1 t2)}
      EQ -> Testing {qt_test = wt1 -- same as wt2
                    ,qt_dopas = mappend ds1 ds2
                    ,qt_a = merge a1 a2
                    ,qt_b = merge b1 b2}
      GT -> t2 {qt_a=(merge t1 a2), qt_b=(merge t1 b2)}

  fuseQTrans :: (CharMap QTrans) -> QTrans
             -> (CharMap QTrans) -> QTrans
             -> CharMap QTrans
  fuseQTrans (CharMap t1) o1 (CharMap t2) o2 = CharMap (IMap.fromDistinctAscList (fuse l1 l2)) where
    l1 = IMap.toAscList t1
    l2 = IMap.toAscList t2
    fuse [] y  = mapSnd (mergeQTrans o1) y
    fuse x  [] = mapSnd (mergeQTrans o2) x
    fuse x@((xc,xa):xs) y@((yc,ya):ys) =
      case compare xc yc of
        LT -> (xc,mergeQTrans xa o2) : fuse xs y
        EQ -> (xc,mergeQTrans xa ya) : fuse xs ys
        GT -> (yc,mergeQTrans o1 ya) : fuse x  ys

  mergeQTrans :: QTrans -> QTrans -> QTrans
  mergeQTrans = IMap.unionWith mappend

-- Type of State monad used inside qToNFA
type S = State (Index                              -- Next available QNFA index
               ,[(Index,QNFA)]->[(Index,QNFA)])    -- DList of previous QNFAs

-- Type of continuation of the NFA, not much more complicated
type E = (TagTasks            -- Things to de before the Either QNFA QT
         ,Either QNFA QT)     -- The future, packged in the best way

type ActCont = (E, Maybe E, Maybe (TagTasks,QNFA))

newQNFA :: String -> QT -> S QNFA
newQNFA s qt = do
  (thisI,oldQs) <- get
  let futureI = succ thisI in seq futureI $ debug (">newQNFA< "++s++" : "++show thisI) $ do
  let qnfa =  mkQNFA thisI qt -- (strictQT qt) -- making strictQNFA kills test (1,11) ZZZ
  put (futureI, oldQs . ((thisI,qnfa):))
  return qnfa

-- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == 

fromQNFA :: QNFA -> E
fromQNFA qnfa = (mempty,Left qnfa)

fromQT :: QT -> E
fromQT qt = (mempty,Right qt)

-- Promises (Left qnfa)
asQNFA :: String -> E -> S E
asQNFA _ x@(_,Left _) = return x
asQNFA s (tags,Right qt) = do qnfa <- newQNFA s qt      -- YYY Policy choice: leave the tags
                              return (tags, Left qnfa)

getQNFA :: String -> E -> S QNFA
getQNFA _ ([],Left qnfa) = return qnfa
getQNFA s (tags,Left qnfa) = newQNFA s (prependTags' (promoteTasks PreUpdate tags) (q_qt qnfa))
getQNFA s (tags,Right qt) = newQNFA s (prependTags' (promoteTasks PreUpdate tags) qt)

getQT :: E -> QT
getQT (tags,cont) = prependTags' (promoteTasks PreUpdate tags) (either q_qt id cont)

addTest :: TestInfo -> E -> E
addTest ti (tags,Left qnfa) = (tags, Right $ applyTest ti (q_qt qnfa))
addTest ti (tags,Right qt) = (tags, Right $ applyTest ti qt)

promoteTasks :: (TagTask->TagUpdate) -> TagTasks -> TagList
promoteTasks promote tags = map (\(tag,task) -> (tag,promote task)) tags

demoteTags :: TagList -> TagTasks
demoteTags = map helper
  where helper (tag,PreUpdate tt) = (tag,tt)
        helper (tag,PostUpdate tt) = (tag,tt)

{-# INLINE addWinTags #-}
addWinTags :: WinTags -> (TagTasks,a) -> (TagTasks,a)
addWinTags wtags (tags,cont) = (demoteTags wtags `mappend` tags,cont)

{-# INLINE addTag' #-}
addTag' :: Tag -> (TagTasks,a) -> (TagTasks,a)
addTag' tag (tags,cont) = ((tag,TagTask):tags,cont)

{-# INLINE addGroupResets #-}
addGroupResets :: (Show a) => [Tag] -> (TagTasks,a) -> (TagTasks,a)
addGroupResets [] x = x
addGroupResets tags (tags',cont) = (foldr (:) tags' . map (\tag -> (tag,ResetGroupStopTask)) $ tags,cont)

addTag :: Maybe Tag -> E -> E
addTag Nothing e = e
addTag (Just tag) e = addTag' tag e

{- XXX use QT form instead
enterOrbit :: Maybe Tag -> E -> E
enterOrbit Nothing e = e
enterOrbit (Just tag) (tags,cont) = ((tag,EnterOrbitTask):tags,cont)
-}

addTestAC :: TestInfo -> ActCont -> ActCont
addTestAC ti (e,mE,_) = (addTest ti e
                        ,fmap (addTest ti) mE
                        ,Nothing)

addTagAC :: Maybe Tag -> ActCont -> ActCont
addTagAC Nothing ac = ac
addTagAC (Just tag) (e,mE,mQNFA) = (addTag' tag e
                                   ,fmap (addTag' tag) mE
                                   ,fmap (addTag' tag) mQNFA)

addGroupResetsAC :: [Tag] -> ActCont -> ActCont
addGroupResetsAC [] ac = ac
addGroupResetsAC tags (e,mE,mQNFA) = (addGroupResets tags e
                                     ,fmap (addGroupResets tags) mE
                                     ,fmap (addGroupResets tags) mQNFA)

addWinTagsAC :: WinTags -> ActCont -> ActCont
addWinTagsAC wtags (e,mE,mQNFA) = (addWinTags wtags e
                                  ,fmap (addWinTags wtags) mE
                                  ,fmap (addWinTags wtags) mQNFA)

getE :: ActCont -> E
getE (_,_,Just (tags,qnfa)) = (tags, Left qnfa)  -- consume optimized mQNFA value returned by Star
getE (eLoop,Just accepting,_) = mergeE eLoop accepting
getE (eLoop,Nothing,_) = eLoop

mergeE :: E -> E -> E
mergeE e1 e2 = fromQT (mergeQT (getQT e1) (getQT e2))

prependTag :: Maybe Tag -> QT -> QT
prependTag Nothing qt = qt
prependTag (Just tag) qt = prependTags' [(tag,PreUpdate TagTask)] qt

prependGroupResets :: [Tag] -> QT -> QT
prependGroupResets [] qt = qt
prependGroupResets tags qt = prependTags' [(tag,PreUpdate ResetGroupStopTask)|tag<-tags] qt

prependTags' :: TagList -> QT -> QT
prependTags' tcs' qt@(Testing {}) = qt { qt_a = prependTags' tcs' (qt_a qt)
                                       , qt_b = prependTags' tcs' (qt_b qt) }
prependTags' tcs' (Simple {qt_win=w,qt_trans=t,qt_other=o}) =
  Simple { qt_win = if noWin w then w else tcs' `mappend` w
         , qt_trans = Map.map prependQTrans t
         , qt_other = prependQTrans o }
  where prependQTrans = fmap (map (\(d,tcs) -> (d,tcs' `mappend` tcs)))

-- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == 

-- Initial preTag of 0th tag is implied
-- No other general pre-tags would be expected
qToNFA :: CompOption -> Q -> (Index,Array Index QNFA)
qToNFA compOpt qTop = (q_id startingQNFA
                      ,array (0,pred lastIndex) (table [])) where
  (startingQNFA,(lastIndex,table)) =
    runState (getTrans qTop (fromQT $ qtwin) >>= getQNFA "top level") startState
  startState = (0,id)

  -- This is the only place where PostUpdate is used
  newTrans :: String -> [Tag] -> Maybe Tag -> Pattern -> E -> S E
  newTrans s resets mPre pat (tags,cont) = do
    i <- case cont of
           Left qnfa -> return (q_id qnfa)     -- strictQNFA ZZZ no help
           Right qt -> do qnfa <- newQNFA s qt -- strictQT ZZZ no help
                          return (q_id qnfa)
    let post = promoteTasks PostUpdate tags
        pre = promoteTasks PreUpdate ([(tag,ResetGroupStopTask) | tag<-resets] ++ maybe [] (\tag -> [(tag,TagTask)]) mPre)
    return . fromQT $ acceptTrans pre pat post i -- fromQT $ strictQT no help

  getTrans,getTransTagless :: Q -> E -> S E
  getTrans qIn@(Q {preReset=resets,preTag=pre,postTag=post,unQ=pIn}) e = debug (">< getTrans "++show qIn++" <>") $
--    liftM strictE $ -- ZZZ causes stack overflow in test (1,36)
    case pIn of
      OneChar pat -> newTrans "getTrans/OneChar" resets pre pat . addTag post $ e
      Empty -> return . addGroupResets resets . addTag pre . addTag post $ e
      Test ti -> return . addGroupResets resets . addTag pre . addTest ti . addTag post $ e
      _ -> return . addGroupResets resets . addTag pre =<< getTransTagless qIn (addTag post $ e)

  getTransTagless qIn e = debug (">< getTransTagless "++show qIn++" <>") $
    case unQ qIn of
      Seq q1 q2 -> getTrans q1 =<< getTrans q2 e
      Or [] -> return e
      Or [q] -> getTrans q e
      Or qs -> do
        eqts <- if usesQNFA qIn
                  then do eQNFA <- asQNFA "getTransTagless/Or/usesQNFA" e
                          sequence [ getTrans q eQNFA | q <- qs ]
                  else sequence [ getTrans q e | q <- qs ]
        let qts = map getQT eqts
        return (fromQT (foldr1 mergeAltQT qts))

-- XXX add enter leaveOrbit to the e' capture path!

      Star mOrbit resetTheseOrbits mayFirstBeNull q ->
        let (e',clear) = -- debug ("\n>"++show e++"\n"++show q++"\n<") $
              if notNullable q then (e,True)  -- subpattern cannot be null
                else case maybeOnlyEmpty q of
                       Just [] -> (e,True)    -- True because null of subpattern is same as skipping subpattern
                       Just tagList -> (addWinTags tagList e,False) -- null of subpattern NOT same as skipping
                       _ -> (fromQT . preferNullViews (nullQ q) . getQT $ e,False)  -- is NOT same as skipping
        in if cannotAccept q then return e' else mdo
        mqt <- inStar q this
        (this,ans) <- case mqt of
                        Nothing -> err ("Weird pattern in getTransTagless/Star: " ++ show (qTop,qIn))
                        Just qt -> do
                          let qt' = resetOrbitsQT resetTheseOrbits . enterOrbitQT mOrbit $ qt
                              thisQT = mergeQT qt' . getQT . leaveOrbit mOrbit $ e -- capture of subpattern or leave via next pattern (avoid null of subpattern on way out)
                              ansE = fromQT . mergeQT qt' . getQT $ e' -- capture of subpattern or leave via null of subpattern
                          thisE <- if usesQNFA q
                                  then return . fromQNFA =<< newQNFA "getTransTagless/Star" thisQT
                                  else return . fromQT $ thisQT
                          return (thisE,ansE)
        return (if mayFirstBeNull then (if clear then this  -- optimization to possibly preserve QNFA
                                                 else ans)
                  else this)
      {- NonEmpty is like actNullable (Or [Empty,q]) without the extra tag to prefer the first Empty branch -}
      NonEmpty q -> debug ("\n> getTrans/NonEmpty"++show qIn)  $ do
        -- Assertion to check than Pattern.starTrans did its job right:
        when (cannotAccept q) (err $ "getTransTagless/NonEmpty : provided with a *cannotAccept* pattern: "++show (qTop,qIn))
        when (mustAccept q) (err $ "getTransTagless/NonEmpty : provided with a *mustAccept* pattern: "++show (qTop,qIn))
        let e' = case maybeOnlyEmpty qIn of
                   Just [] -> e
                   Just wtags -> addWinTags wtags e  -- I suspect this is stupidly duplicating tags
                   Nothing -> err $ "getTransTagless/NonEmpty is supposed to have an emptyNull nullView : "++show qIn
        mqt <- inStar q e
        return $ case mqt of
                   Nothing -> err ("Weird pattern in getTransTagless/NonEmpty: " ++ show (qTop,qIn))
                   Just qt -> fromQT . mergeQT_2nd qt . getQT $ e' -- ...and then this sets qt_win to exactly that of e'
      _ -> err ("This case in Text.Regex.TNFA.TNFA.getTransTagless cannot happen" ++ show (qTop,qIn))

  inStar,inStarNullableTagless :: Q -> E -> S (Maybe QT)
  inStar qIn@(Q {preReset=resets,preTag=pre,postTag=post}) eLoop | notNullable qIn =
    debug (">< inStar/1 "++show qIn++" <>") $
    return . Just . getQT =<< getTrans qIn eLoop
                                                 | otherwise =
    debug (">< inStar/2 "++show qIn++" <>") $
    return . fmap (prependGroupResets resets . prependTag pre) =<< inStarNullableTagless qIn (addTag post $ eLoop)
    
  inStarNullableTagless qIn eLoop = debug (">< inStarNullableTagless "++show qIn++" <>") $ do
    case unQ qIn of
      Empty -> return Nothing -- with Or this discards () branch in "(^|foo|())*"
      Or [] -> return Nothing
      Or [q] -> inStar q eLoop
      Or qs -> do
        mqts <- if usesQNFA qIn
                  then do eQNFA <- asQNFA "inStarNullableTagless/Or/usesQNFA" eLoop
                          sequence [ inStar q eQNFA | q <- qs ]
                  else sequence [inStar q eLoop | q <- qs ]
        let qts = catMaybes mqts
            mqt = if null qts then Nothing else Just (foldr1 mergeAltQT qts)
        return mqt
      Seq q1 q2 -> do (_,meAcceptingOut,_) <- actNullable q1 =<< actNullable q2 (eLoop,Nothing,Nothing)
                      return (fmap getQT meAcceptingOut)
      Star {} -> do (_,meAcceptingOut,_) <- actNullableTagless qIn (eLoop,Nothing,Nothing)
                    return (fmap getQT meAcceptingOut)
      NonEmpty {} -> debug ("\n> inStar/NonEmpty"++show qIn) $
                     do (_,meAcceptingOut,_) <- actNullableTagless qIn (eLoop,Nothing,Nothing)
                        return (fmap getQT meAcceptingOut)
      Test {} -> return Nothing -- with Or this discards ^ branch in "(^|foo|())*"
      OneChar {} -> err ("OneChar cannot have nullable True")

  {- act* functions

  These have a very complicated state that they receive and return as
  "the continuation".

   (E, Maybe E,Maybe (SetTag,QNFA))

  The first E is the source of the danger that must be avoided.  It
  starts out a reference to the QNFA/QT state that will be created by
  the most recent parent Star node.  Thus it is a recursive reference
  from the MonadFix machinery.  In particular, this value cannot be
  returned to the parent Star to be included in itself or we get a "let
  x = x" style infinite loop.

  As act* progresses the first E is actually modified to be the parent
  QNFA/QT as "seen" when all the elements to the right have accepted 0
  characters.  Thus it acquires tags and tests+tags (the NullView data
  is used for this purpose).

  The second item in the 3-tuple is a Maybe E.  This will be used as the
  source of the QT for this contents of the Star QNFA/QT.  It will be
  merged with the Star's own continuation data.  It starts out Nothing
  and stays that way as long as there are no accepting transitions in
  the Star's pattern.  This is value (via getQT) returned by inStar.

  The third item is a special optimization I added to reduce a source of
  orphaned QNFAs.  A Star within Act will often have to create a QNFA
  node.  This cannot go into the second Maybe E item as Just
  (SetTag,Left QNFA) because this QNFA can have pulled values from the
  recursive parent Star's QNFA/QT in the first E value.  Thus pulling
  getQT from it would likely cause an infinite loop.  To improve it
  further it can accumulate Tag information after being formed.

  When a non nullable Q is handled by act it checks to see if the third
  value is there, in which case it uses that QNFA as the total
  continuation.  Otherwise it merges the first E with any (Just E) in
  the second value to form the continuation.

  -}

  -- act,actNullable,actNullableTagless :: (E, Maybe E,Maybe (SetTag,QNFA)) -> Q -> S (E, Maybe E,Maybe (SetTag,QNFA))
  act,actNullable,actNullableTagless :: Q -> ActCont -> S ActCont
  act qIn c | nullable qIn = actNullable qIn c
            | otherwise = debug (">< act "++show qIn++" <>") $ do
    mqt <- return . Just =<< getTrans qIn ( getE $ c )
    return (err "qToNFA / act / no clear view",mqt,Nothing)  -- or "return (fromQT qtlose,mqt,Nothing)"

  actNullable qIn@(Q {preReset=resets,preTag=pre,postTag=post,unQ=pIn}) ac =
    debug (">< actNullable "++show qIn++" <>") $ do
    case pIn of
      Empty -> return . addGroupResetsAC resets . addTagAC pre . addTagAC post $ ac
      Test ti -> return . addGroupResetsAC resets . addTagAC pre . addTestAC ti . addTagAC post $ ac
      OneChar {} -> err ("OneChar cannot have nullable True ")
      _ -> return . addGroupResetsAC resets . addTagAC pre =<< actNullableTagless qIn ( addTagAC post $ ac )

  actNullableTagless qIn ac@(eLoop,mAccepting,mQNFA) = debug (">< actNullableTagless "++show (qIn)++" <>") $ do
    case unQ qIn of
      Seq q1 q2 -> actNullable q1 =<< actNullable q2 ac   -- We know q1 and q2 are nullable
                      
      Or [] -> return ac
      Or [q] -> actNullableTagless q ac
      Or qs -> do
        cqts <- do
          if all nullable qs
            then sequence [fmap snd3 $ actNullable q ac | q <- qs]
            else do
              e' <- asQNFA "qToNFA/actNullableTagless/Or" . getE $ ac
              let act' :: Q -> S (Maybe E)
                  act' q = return . Just =<< getTrans q e'
              sequence [ if nullable q then fmap snd3 $ actNullable q ac else act' q | q <- qs ]
        let qts = map getQT (catMaybes cqts)
            eLoop' = case maybeOnlyEmpty qIn of
                       Just wtags -> addWinTags wtags eLoop -- nullable without tests; avoid getQT
                       Nothing -> fromQT $ applyNullViews (nullQ qIn) (getQT eLoop)
            mAccepting' = if null qts
                            then fmap (fromQT . applyNullViews (nullQ qIn) . getQT) mAccepting
                            else Just (fromQT $ foldr1 mergeAltQT qts)
            mQNFA' = if null qts
                       then case maybeOnlyEmpty qIn of
                              Just wtags -> fmap (addWinTags wtags) mQNFA
                              Nothing -> Nothing
                       else Nothing
        return (eLoop',mAccepting',mQNFA')

      Star mOrbit resetTheseOrbits mayFirstBeNull q -> do
        let (ac0@(_,mAccepting0,_),clear) =
              if notNullable q
                then (ac,True)
                else case maybeOnlyEmpty q of
                       Just [] -> (ac,True)
                       Just wtags -> (addWinTagsAC wtags ac,False)
                       _ -> let nQ = fromQT . preferNullViews (nullQ q) . getQT
                            in ((nQ eLoop,fmap nQ mAccepting,Nothing),False)
        if cannotAccept q then return ac0 else mdo
-- XXX
-- mChildAccepting <- if nullable q then fmap snd3 $ actNullable q (this,Nothing,Nothing)
--                      else return . Just =<< getTrans q this
-- XXX  and then delete act
          (_,mChildAccepting, _ {-mChildQNFA-}) <- act q (this,Nothing,Nothing)
  -- XXX  if clear && isJust mChildQNFA then (childQNFA,Just (getQT childQNFA),(mempty,childQNFA))
          (thisAC@(this,_,_),ansAC) <- 
            case mChildAccepting of
              Nothing -> err $ "Weird pattern in getTransTagless/Star: " ++ show (qTop,qIn)
              Just childAccepting -> do
                let childQT = resetOrbitsQT resetTheseOrbits . enterOrbitQT mOrbit . getQT $ childAccepting
                    thisQT = mergeQT childQT . getQT . leaveOrbit mOrbit . getE $ ac
                    thisAccepting =
                      case mAccepting of
                        Just futureAccepting -> Just . fromQT . mergeQT childQT . getQT $ futureAccepting
                        Nothing -> Just . fromQT $ childQT
                thisAll <- if usesQNFA q
                             then do thisQNFA <- newQNFA "actNullableTagless/Star" thisQT
                                     return (fromQNFA thisQNFA, thisAccepting, Just (mempty,thisQNFA))
                             else return (fromQT thisQT, thisAccepting, Nothing)
                let skipQT = mergeQT childQT . getQT . getE $ ac0  -- for first iteration the continuation uses NullView
                    skipAccepting =
                      case mAccepting0 of
                        Just futureAccepting0 -> Just . fromQT . mergeQT childQT . getQT $ futureAccepting0
                        Nothing -> Just . fromQT $ childQT
                    ansAll = (fromQT skipQT, skipAccepting, Nothing)
                return (thisAll,ansAll)
          return (if mayFirstBeNull then (if clear then thisAC else ansAC)
                    else thisAC)
      NonEmpty q -> debug ("\n> actNullableTagless/NonEmpty"++show qIn) $ do
        -- We *know* that q is nullable from Pattern and CorePattern checks, but assert here anyway
        when (mustAccept q) (err $ "actNullableTagless/NonEmpty : provided with a *mustAccept* pattern: "++show (qTop,qIn))
        when (cannotAccept q) (err $ "actNullableTagless/NonEmpty : provided with a *cannotAccept* pattern: "++show (qTop,qIn))

        {- This is like actNullable (Or [Empty,q]) without the extra tag to prefer the first Empty branch -}
        let (clearE,_,_) = case maybeOnlyEmpty qIn of
                             Just [] -> ac
                             Just wtags -> addWinTagsAC wtags ac
                             Nothing -> err $ "actNullableTagless/NonEmpty is supposed to have an emptyNull nullView : "++show (qTop,qIn)
        (_,mChildAccepting,_) <- actNullable q ac
        case mChildAccepting of
          Nothing -> err  $ "Weird pattern in actNullableTagless/NonEmpty: " ++ show (qTop,qIn)
            -- cannotAccept q checked for this (and starTrans!)
          Just childAccepting -> do
            let childQT = getQT childAccepting
                thisAccepting = case mAccepting of
                                  Nothing -> Just . fromQT $ childQT
                                  Just futureAcceptingE -> Just . fromQT . mergeQT childQT . getQT $ futureAcceptingE
            return (clearE,thisAccepting,Nothing)
      _ -> err $ "This case in Text.Regex.TNFA.TNFA.actNullableTagless cannot happen: "++show (qTop,qIn)

  -- This is applied directly to any qt right before passing to mergeQT
  resetOrbitsQT :: [Tag] -> QT -> QT
  resetOrbitsQT | lastStarGreedy compOpt = const id
                | otherwise = (\ tags -> prependTags' [(tag,PreUpdate ResetOrbitTask)|tag<-tags])

  enterOrbitQT :: Maybe Tag -> QT -> QT
  enterOrbitQT | lastStarGreedy compOpt = const id
               | otherwise = maybe id (\tag->prependTags' [(tag,PreUpdate EnterOrbitTask)])

  leaveOrbit :: Maybe Tag -> E -> E
  leaveOrbit | lastStarGreedy compOpt = const id
             | otherwise = maybe id (\tag->(\(tags,cont)->((tag,LeaveOrbitTask):tags,cont)))

  dotTrans | multiline compOpt = Map.singleton '\n' mempty
           | otherwise = mempty

  addNewline | multiline compOpt = Data.Set.insert '\n'
             | otherwise = id

 
  toMap :: IntMap [(DoPa,[(Tag, TagUpdate)])] -> [Char] -> CharMap (IntMap [(DoPa,[(Tag, TagUpdate)])])
  toMap dest | caseSensitive compOpt = CharMap . IMap.fromDistinctAscList . map (\c -> (ord c,dest))
             | otherwise = CharMap . IMap.fromList . ($ []) 
                             . foldr (\c dl -> if isAlpha c
                                                 then (dl.((ord (toUpper c),dest):)
                                                         .((ord (toLower c),dest):)
                                                      )
                                                 else (dl.((ord c,dest):))
                                     ) id 

  acceptTrans :: TagList -> Pattern -> TagList -> Index -> QT
  acceptTrans pre pIn post i =
    let target = IMap.singleton i [(getDoPa pIn,pre++post)]
    in case pIn of
         PChar _ char ->
           let trans = toMap target [char]
           in Simple { qt_win = mempty, qt_trans = trans, qt_other = mempty }
         PEscape _ char ->
           let trans = toMap target [char]
           in Simple { qt_win = mempty, qt_trans = trans, qt_other = mempty }
         PDot _ -> Simple { qt_win = mempty, qt_trans = dotTrans, qt_other = target }
         PAny _ ps ->
           let trans = toMap target . Data.Set.toAscList . decodePatternSet $ ps
           in Simple { qt_win = mempty, qt_trans = trans, qt_other = mempty }
         PAnyNot _ ps ->
           let trans = toMap mempty . Data.Set.toAscList . addNewline . decodePatternSet $ ps
           in Simple { qt_win = mempty, qt_trans = trans, qt_other = target }
         _ -> err ("Cannot acceptTrans pattern "++show (qTop,pIn))
