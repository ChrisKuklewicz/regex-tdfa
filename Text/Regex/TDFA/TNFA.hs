{- Resetting tags.  There is a need to either create a parallel system
of tracking in progress group capture along the lines of regex-parsec,
or to clear some of the tags.

Why do tags needs to be cleared?  Because when looping from the end of
'a' in (a*) to the beginning all the internal captures are lost.

Where does this show up?  The PStar::Pattern to Star::P conversion is
a horizon which does not sink tags through or raise tags from.  The
continuation return from inStar can be preceded by resetting to (-1)
every tag inside the Star.  Thus making it as pristine as the first
pass. This has the advantage that no mucking with the GroupInfo is
required. It should also allow a return of the efficient Or tagging.

A future refinement could reset only the tags involved in
closing group captures.

This still requires ordering the tags instead of keeping them sorted.
-}

module Text.Regex.TDFA.TNFA(patternToNFA,pickQTrans,cleanWin,noWin
                           ,toP,toQ,toNFA,display_NFA
                           ,QNFA(..),QT(..),QTrans,TagUpdate(..)) where

import Text.Regex.TDFA.Pattern(Pattern(..))
import Text.Regex.TDFA.CorePattern(Q(..),P(..),OP(..),WhichTest,cleanNullView,NullView
                                 ,SetTestInfo(..),Wanted(..),TestInfo,cannotAccept,patternToQ)
import Text.Regex.TDFA.Common
import Text.Regex.TDFA.Wrap
import Text.Regex.Base(RegexOptions(defaultCompOpt))
-- (DoPa,Index,PatternIndex,GroupInfo,CompOption(..),on,mapFst,mapSnd,norep)
import Text.Regex.TDFA.ReadRegex(decodePatternSet,parseRegex)

import Data.Monoid
import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Char(toLower,toUpper,isAlpha)

import Data.Array.IArray
import Data.Set(Set)
import qualified Data.Set as Set
-- import Data.IntSet(IntSet)
-- import qualified Data.IntSet as ISet
import Data.Map(Map)
import qualified Data.Map as Map
-- import Data.IntMap(IntMap)
import qualified Data.IntMap as IMap

-- import Debug.Trace

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

foo :: Map Char QTrans -> [(Char,[(Index,[TagCommand])])]
foo = mapSnd foo' . Map.toAscList

foo' :: QTrans -> [(Index,[TagCommand])]
foo' = mapSnd Set.toList . IMap.toList 

instance Eq QT where
  t1@(Testing {}) == t2@(Testing {}) =
    (qt_test t1) == (qt_test t2) && (qt_a t1) == (qt_a t2) && (qt_b t1) == (qt_b t2)
  (Simple w1 t1 o1) == (Simple w2 t2 o2) =
    w1 == w2 && eqTrans && eqQTrans o1 o2
    where eqTrans :: Bool
          eqTrans = (Map.size t1 == Map.size t2)
                    && and (zipWith together (Map.toAscList t1) (Map.toAscList t2))
            where together (c1,qtrans1) (c2,qtrans2) = (c1 == c2) && eqQTrans qtrans1 qtrans2
          eqQTrans :: QTrans -> QTrans -> Bool
          eqQTrans = (==)
  _ == _ = False

-- This uses the Eq QT instace above
mkTesting :: QT -> QT
mkTesting t@(Testing {qt_a=a,qt_b=b}) = if a==b then a else t
mkTesting t = t

qtwin,qtlose :: QT
qtwin = Simple {qt_win=[(1,PreUpdate TagTask)],qt_trans=mempty,qt_other=mempty}
qtlose = Simple {qt_win=mempty,qt_trans=mempty,qt_other=mempty}

patternToNFA :: CompOption
             -> (Text.Regex.TDFA.Pattern.Pattern,(PatternIndex, Int))
             -> ((Index,Array Index QNFA)
                ,Array Tag OP
                ,Array PatternIndex [GroupInfo])
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
maybeOnlyEmpty (Q {nullQ = ((SetTestInfo sti,tags):_)}) | Map.null sti = Just tags
maybeOnlyEmpty _ = Nothing

usesQNFA :: Q -> Bool
usesQNFA (Q {wants=WantsBoth}) = True
usesQNFA (Q {wants=WantsQNFA}) = True
usesQNFA _ = False

nullQT :: QT -> Bool
nullQT (Simple {qt_win=w,qt_trans=t,qt_other=o}) = noWin w && Map.null t && IMap.null o
nullQT _ = False

listTestInfo :: QT -> Set WhichTest -> Set WhichTest
listTestInfo qt s = execState (helper qt) s
  where helper (Simple {}) = return ()
        helper (Testing {qt_test = wt, qt_a = a, qt_b = b}) = do
          modify (Set.insert wt)
          helper a
          helper b

applyNullViews :: NullView -> QT -> QT
applyNullViews [] win = win
applyNullViews nvs win = foldl' dominate qtlose (reverse $ cleanNullView nvs) where
  winTests = listTestInfo win $ mempty
  dominate :: QT -> (SetTestInfo,WinTags) -> QT
  dominate lose x@(SetTestInfo sti,tags) = debug ("dominate "++show x) $
    let -- The winning states are reached through the SetTag
        win' = prependTags' tags win
        -- get the SetTestInfo 
        allTests = (listTestInfo lose $ Map.keysSet sti) `mappend` winTests
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
        useTest [] _ _  _ = error "This case in Text.Regex.TNFA.TNFA.applyNullViews.useText cannot happen"
    in useTest (Set.toList allTests) (Map.assocs sti) win' lose

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

mergeAltQT,mergeQT :: QT -> QT -> QT
mergeAltQT q1 q2 | nullQT q1 = q2  -- prefer winning with w1 then with w2
                 | otherwise = mergeQTWith (\w1 w2 -> if noWin w1 then w2 else w1) q1 q2
mergeQT q1 q2 | nullQT q1 = q2  -- union wins
              | nullQT q2 = q1  -- union wins
              | otherwise = mergeQTWith mappend q1 q2 -- no preference, win with combined SetTag

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

  fuseQTrans :: (Map Char QTrans) -> QTrans -> (Map Char QTrans) -> QTrans -> Map Char QTrans
  fuseQTrans t1 o1 t2 o2 = Map.fromDistinctAscList (fuse l1 l2) where
    l1 = Map.toAscList t1
    l2 = Map.toAscList t2
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

newQNFA :: String -> QT -> S QNFA
newQNFA s qt = do
  (thisI,oldQs) <- get
  let futureI = succ thisI in seq futureI $ debug (">newQNFA< "++s++" : "++show thisI) $ do
  let qnfa = mkQNFA thisI qt
  put (futureI, oldQs . ((thisI,qnfa):))
  return qnfa

pickQTrans :: (Tag -> OP) -> QTrans -> [({-Destination-}Index,TagCommand)]
pickQTrans op tr = mapSnd (bestTrans op) . IMap.toList $ tr

cleanWin :: WinTags -> WinTags
cleanWin = sort . nubBy ((==) `on` fst) . reverse   -- ick, nub XXX

bestTrans :: (Tag -> OP) -> Set TagCommand -> TagCommand
bestTrans op s | len == 0 = error "There were no transitions in bestTrans"
               | len == 1 = canonical $ head l
               | otherwise = foldl' pick (canonical $ head l) (tail l) where
  len = Set.size s
  l = Set.toList s
  pick :: TagCommand -> TagCommand -> TagCommand
  pick t1_can@(_,tcs1_can) t2@(_,_) =
    let t2_can@(_,tcs2_can) = canonical t2
    in case choose tcs1_can tcs2_can of
         GT -> t1_can
         EQ -> t1_can
         LT -> t2_can
  canonical :: TagCommand -> TagCommand
  canonical (dopa,tcs) = (dopa,sort clean) -- keep only last setting or resetting
    where clean = nubBy ((==) `on` fst) . reverse $ tcs  -- ick, nub XXX

  -- choose trests Orbit and Minimize as the same
  choose :: TagList -> TagList -> Ordering
  choose ((t1,b1):rest1) ((t2,b2):rest2) =
    case compare t1 t2 of -- find and examine the smaller of t1 and t2
      LT -> if Maximize == op t1 then GT else LT
      EQ -> (if Maximize == op t1 then compare else flip compare) b1 b2 `mappend` choose rest1 rest2
      GT -> if Maximize == op t2 then LT else GT
  choose ((t1,_):_) [] = if Maximize == op t1 then GT else LT
  choose [] ((t2,_):_) = if Maximize == op t2 then LT else GT
  choose [] [] = EQ

noWin :: TagList -> Bool
noWin = null

{- XXX
andTag :: Maybe Tag -> Maybe Tag -> TagList
andTag (Just a) (Just b) = [(b,PreTag),(a,PreTag)]
andTag (Just a) Nothing  = [(a,PreTag)]
andTag Nothing  (Just b) = [(b,PreTag)]
andTag Nothing  Nothing  = []
-}
{-
prependTags :: [Tag] -> QT -> QT
prependTags tags qt | null tags = qt
                    | nullQT qt = qt
                    | otherwise = prependTags' [(tag,PreTag)|tag<-tags] qt
-}

{- XXX 
-- Modify and query the continuation
addTag :: Maybe Tag -> E -> E
addTag (Just tag) (tags,cont) = ((tag,PreTag):tags,cont)
addTag Nothing x = x

addTags :: TagList -> E -> E
--addTags tags (tags',cont) = ([(tag,PreTag)|tag<-tags] `mappend` tags',cont)
addTags tags (tags',cont) = (tags `mappend` tags',cont)

insertTag :: Maybe Tag -> [Tag] -> [Tag]
insertTag (Just tag) tags = (tag:tags)
insertTag Nothing tags = tags
-}

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

addTag :: Maybe Tag -> E -> E
addTag Nothing e = e
addTag (Just tag) e = addTag' tag e

{- XXX use QT form instead
enterOrbit :: Maybe Tag -> E -> E
enterOrbit Nothing e = e
enterOrbit (Just tag) (tags,cont) = ((tag,EnterOrbitTask):tags,cont)

-}

enterOrbitQT :: Maybe Tag -> QT -> QT
enterOrbitQT Nothing qt = qt
enterOrbitQT (Just tag) qt = prependTags' [(tag,PreUpdate EnterOrbitTask)] qt

leaveOrbitQT :: Maybe Tag -> E -> QT
leaveOrbitQT mt qt = getQT . leaveOrbit mt $ qt
  where leaveOrbit :: Maybe Tag -> E -> E
        leaveOrbit Nothing e = e
        leaveOrbit (Just tag) (tags,cont) = ((tag,LeaveOrbitTask):tags,cont)

type ActCont = (E, Maybe E, Maybe (TagTasks,QNFA))

addTestAC :: TestInfo -> ActCont -> ActCont
addTestAC ti (e,mE,_) = (addTest ti e
                            ,fmap (addTest ti) mE
                            ,Nothing)

addTagAC :: Maybe Tag -> ActCont -> ActCont
addTagAC Nothing ac = ac
addTagAC (Just tag) (e,mE,mQNFA) = (addTag' tag e
                                   ,fmap (addTag' tag) mE
                                   ,fmap (addTag' tag) mQNFA)

getE :: ActCont -> E
getE (_,_,Just (tags,qnfa)) = (tags, Left qnfa)  -- consume optimized mQNFA value returned by Star
getE (eLoop,Just accepting,_) = mergeE eLoop accepting
getE (eLoop,Nothing,_) = eLoop

mergeE :: E -> E -> E
mergeE e1 e2 = fromQT (mergeQT (getQT e1) (getQT e2))

-- This is applied directly to any qt right before passing to mergeQT
addResets :: [Tag] -> QT -> QT
addResets tags qt = prependTags' [(tag,PreUpdate ResetTask)|tag<-tags] qt

prependTag :: Maybe Tag -> QT -> QT
prependTag (Just tag) qt = prependTags' [(tag,PreUpdate TagTask)] qt
prependTag Nothing qt = qt

prependTags' :: TagList -> QT -> QT
prependTags' tcs' qt@(Testing {}) = qt { qt_a = prependTags' tcs' (qt_a qt)
                                       , qt_b = prependTags' tcs' (qt_b qt) }
prependTags' tcs' (Simple {qt_win=w,qt_trans=t,qt_other=o}) =
  Simple { qt_win = if noWin w then w else tcs' `mappend` w
         , qt_trans = Map.map prependQTrans t
         , qt_other = prependQTrans o }
  where prependQTrans = IMap.map (Set.map (\(d,tcs) -> (d,tcs' `mappend` tcs)))

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
  newTrans :: String -> Maybe Tag -> Pattern -> E -> S E
  newTrans s mPre pat (tags,cont) = do
    i <- case cont of
           Left qnfa -> return (q_id qnfa)
           Right qt -> do qnfa <- newQNFA s qt
                          return (q_id qnfa)
    let post = promoteTasks PostUpdate tags
    return . fromQT $ acceptTrans mPre pat post i

  modifyChars = if caseSensitive compOpt
                  then id
                  else norep . sort . ($ []) 
                         . foldr (\c dl -> if isAlpha c
                                             then (dl.(toUpper c:).(toLower c:))
                                             else (dl.(c:))) id 
  
  -- Alternative: Either (SetTag,QNFA) QT to send Tags that apply
  --  before going to the QNFA leftward, where they may someday reach
  --  a OneChar.  Essentially it accumulates postTags and preTags into
  --  effective postTags. The (getQT) abstraction will handle this
  --  well.  
  getTrans,getTransTagless :: Q -> E -> S E
  getTrans qIn@(Q {preTag=pre,postTag=post,unQ=pIn}) e = debug (">< getTrans "++show qIn++" <>") $
    case pIn of
      OneChar pat -> newTrans "getTrans/OneChar" pre pat . addTag post $ e
      Empty -> return . addTag pre . addTag post $ e
      Test ti -> return . addTag pre . addTest ti . addTag post $ e
      _ -> return . addTag pre =<< getTransTagless qIn (addTag post $ e)

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
      Star mOrbit tags q | cannotAccept q -> return e -- XXX may not by POSIX correct
                        | otherwise -> mdo
        mqt <- inStar q this
        this <- case mqt of
                  Nothing -> error ("Weird pattern in getTransTagless/Star: " ++ show qIn)
                  Just qt ->
                    let qt' = addResets tags . enterOrbitQT mOrbit $ qt
                        thisQT = mergeQT qt' . leaveOrbitQT mOrbit $ e
                    in if usesQNFA q
                         then return . fromQNFA =<< newQNFA "getTransTagless/Star" thisQT
                         else return . fromQT $ thisQT
        return this
      _ -> error ("This case in Text.Regex.TNFA.TNFA.getTransTagless cannot happen" ++ show qIn)

  inStar,inStarTagless :: Q -> E -> S (Maybe QT)
  inStar qIn@(Q {preTag=pre,postTag=post}) eLoop | notNullable qIn =
    debug (">< inStar/1 "++show qIn++" <>") $
    return . Just . getQT =<< getTrans qIn eLoop
                                                 | otherwise =
    debug (">< inStar/2 "++show qIn++" <>") $
    return . fmap (prependTag pre) =<< inStarTagless qIn (addTag post $ eLoop)
    
  inStarTagless qIn eLoop = debug (">< inStarTagless "++show qIn++" <>") $ do
    case unQ qIn of
      Empty -> return Nothing -- with Or this discards () branch in "(^|foo|())*"
      Or [] -> return Nothing
      Or [q] -> inStar q eLoop
      Or qs -> do
        mqts <- if usesQNFA qIn
                  then do eQNFA <- asQNFA "inStarTagless/Or/usesQNFA" eLoop
                          sequence [ inStar q eQNFA | q <- qs ]
                  else sequence [inStar q eLoop | q <- qs ]
        let qts = catMaybes mqts
            mqt = if null qts then Nothing else Just (foldr1 mergeAltQT qts)
        return mqt
      Seq q1 q2 -> do (_,meAcceptingOut,_) <- actNullable q1 =<< actNullable q2 (eLoop,Nothing,Nothing)
                      return (fmap getQT meAcceptingOut)
      Star {} -> do (_,meAcceptingOut,_) <- actNullableTagless qIn (eLoop,Nothing,Nothing)
                    return (fmap getQT meAcceptingOut)
      Test {} -> return Nothing -- with Or this discards ^ branch in "(^|foo|())*"
      OneChar {} -> error ("OneChar cannot have nullable True")

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
    return (error "qToNFA / act / no clear view",mqt,Nothing)  -- or "return (fromQT qtlose,mqt,Nothing)"

  actNullable qIn@(Q {preTag=pre,postTag=post,unQ=pIn}) ac =
    debug (">< actNullable "++show qIn++" <>") $ do
    case pIn of
      Empty -> return . addTagAC pre . addTagAC post $ ac
      Test ti -> return . addTagAC pre . addTestAC ti . addTagAC post $ ac
      OneChar {} -> error ("OneChar cannot have nullable True ")
      _ -> return . addTagAC pre =<< actNullableTagless qIn ( addTagAC post $ ac )

  actNullableTagless qIn ac@(eLoop,mAccepting,mQNFA) = debug (">< actNullableTagless "++show (qIn)++" <>") $ do
    case unQ qIn of
      Seq q1 q2 -> actNullable q1 =<< actNullable q2 ac   -- We know q1 and q2 are nullable
                      
      Or [] -> return ac
      Or [q] -> actNullableTagless q ac
      Or qs -> do
        cqts <- do
          if all nullable qs
            then sequence [act q ac | q <- qs]
            else do
              e' <- asQNFA "qToNFA/actNullableTagless/Or" . getE $ ac
              let act' :: Q -> S ActCont
                  act' q = do mqt <- return . Just =<< getTrans q e'
                              return (error "qToNFA/actNullableTagless/Or: no clear view", mqt,Nothing)
              sequence [ if nullable q then act q ac else act' q | q <- qs ]
        let qts = map getQT (catMaybes (map (\(_,mA,_) -> mA) cqts))
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

      Star mOrbit tags q | cannotAccept q -> return ac
                         | otherwise -> mdo
        (_,mE,_) <- act q  (this,Nothing,Nothing)
        ac'@(this,_,_) <- 
          case mE of
            Nothing -> error ("Weird pattern in getTransTagless/Star: " ++ show qIn)
            Just e -> let qt' = addResets tags . enterOrbitQT mOrbit . getQT $ e
                          thisQT = mergeQT qt' . leaveOrbitQT mOrbit . getE $ ac
                          mAccepting' = case mAccepting of
                                          Just eAccepting -> Just . fromQT . mergeQT qt' . getQT $ eAccepting
                                          Nothing -> Just . fromQT $ qt'
                      in if usesQNFA q
                           then do qnfa <- newQNFA "actNullableTagless/Star" thisQT
                                   return (fromQNFA qnfa, mAccepting', Just (mempty,qnfa))
                           else return (fromQT thisQT, mAccepting', Nothing)
        return ac'
      _ -> error ("This case in Text.Regex.TNFA.TNFA.actNullableTagless cannot happen: "++show qIn)

{- XXX change to getE which introduces the possibility of consuming an optimized mQNFA value from a different Star 
       let contQT = prependTags' [(orbit,LeaveOrbit)] $
                     case mAccepting of
                       Nothing -> getQT eLoop
                       Just accepting -> mergeQT (getQT eLoop) (getQT accepting)
        (_,mE,_) <- act (this,Nothing,Nothing) q
        let mqt = fmap (resetTags tags . getQT) mE
        (this,mQNFA') <- if usesQNFA q
                           then do qnfa <- newQNFA "actNullableTagless/Star" (maybe contQT (mergeQT contQT) mqt)
                                   return (fromQNFA qnfa,Just (mempty,qnfa)) -- create optimized mQNFA' value
                           else return $ (fromQT (maybe contQT (mergeQT contQT) mqt),Nothing)
        let mAccepting' = case (mAccepting,mqt) of
                            (Just eAccepting,Just qt) -> Just (fromQT $ mergeQT (getQT eAccepting) qt)
                            _ -> (fmap fromQT mqt) `mplus` mAccepting
        return (eLoop,mAccepting',mQNFA')
-}

  dotTrans = if multiline compOpt 
               then Map.singleton '\n' mempty
               else mempty

  addNewline = if multiline compOpt
                 then Set.insert '\n'
                 else id

  acceptTrans :: Maybe Tag -> Pattern -> TagList -> Index -> QT
  acceptTrans pre pIn post i =
    let target = IMap.singleton i $ Set.singleton (getDoPa pIn
                                                  ,maybe post (\tag->(tag,PreUpdate TagTask):post) pre)
        tomap = Map.fromDistinctAscList . map (\c -> (c,target)) . modifyChars
    in case pIn of
         PChar _ char ->
           let trans = tomap [char]
           in Simple { qt_win = mempty, qt_trans = trans, qt_other = mempty }
         PDot _ -> Simple { qt_win = mempty, qt_trans = dotTrans, qt_other = target }
         PAny _ ps ->
           let trans = tomap . Set.toAscList . decodePatternSet $ ps
           in Simple { qt_win = mempty, qt_trans = trans, qt_other = mempty }
         PAnyNot _ ps ->
           let trans = tomap . Set.toAscList . addNewline . decodePatternSet $ ps
           in Simple { qt_win = mempty, qt_trans = trans, qt_other = target }
         PEscape _ char ->
           let trans = tomap [char]
           in Simple { qt_win = mempty, qt_trans = trans, qt_other = mempty }
         _ -> error ("Cannot acceptTrans pattern "++show pIn)


{-
showQT' :: (Tag -> OP) -> QT -> String
showQT' f (Simple win trans other) = "{qt_win=" ++ show win
                              ++ "\n, qt_trans=" ++ show (mapSnd (cleanQTrans f) (Map.toList trans))
                              ++ "\n, qt_other=" ++ show (cleanQTrans f other) ++ "}"
showQT' f (Testing test dopas a b) = "{Testing "++show test++" "++show (Set.toList dopas)
                               ++"\n"++indent a
                               ++"\n"++indent b++"}"
    where indent = init . unlines . map (spaces++) . lines . (showQT' f)
          spaces = replicate 9 ' '

showQNFA' :: (Tag -> OP) -> QNFA -> String
showQNFA' f qnfa = "QNFA {q_id="++show (q_id qnfa)
                   ++",q_qt="++showQT' f (q_qt qnfa)++"}"


-- This asks if it is possible to get through all the elements
-- contained in Q without using any tests or accepting any characters.
isEmpty :: Q -> Bool
isEmpty q = mempty `elem` (map fst (nullQ q))

handles_postTag :: Q -> Bool
handles_postTag (Q {unQ=OneChar {}}) = True
handles_postTag _ = False

-- used in showQT'
cleanQTrans :: (Tag -> OP) -> QTrans -> [(Index,TagCommand)]
cleanQTrans op tr = map (\(i,ts) -> (i,bestTrans op ts)) . IMap.toList $ tr

nullE :: ([Tag],Either QNFA QT) -> Bool
nullE (_,cont) = nullQT . either q_qt id $ cont

-- Promises the snd part is Right _
asQT :: E -> E
asQT (tags,cont) = (tags,Right (either q_qt id cont))

-}

toP = either (error.show) id . parseRegex 
toQ = patternToQ defaultCompOpt . toP
toNFA = patternToNFA defaultCompOpt . toP
display_NFA = mapM_ print . elems . (\(x,_,_) -> snd x) . toNFA 
