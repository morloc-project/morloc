{- |
Module      : Morloc.CodeGenerator.Ownership
Description : Which values a pool's generated code borrows, shares or takes

The ownership decisions a member makes for a whole pool and for each
manifold, computed once from the IR (model/compiler/codegen/ownership.md).
A member supplies only what differs between languages ('OwnCaps') and
renders from the plans.
-}
module Morloc.CodeGenerator.Ownership
  ( OwnCaps (..)
  , PoolPlan (..)
  , LentResult (..)
  , CondLet (..)
  , condKey
  , condLetsIn
  , emptyPoolPlan
  , byValueIndices
  , planPool
  , letValue
  , ManifoldPlan (..)
  , planSerialManifold
  , planNativeManifold
  , LetMode (..)
  , letMode
  , unchangedSlot
  , changedSlots
  , LetPlan (..)
  , letPlan
  , nativeLets
  ) where

import Morloc.Data.Zip (indexed)
import Data.Foldable (asum, toList)
import Control.Monad.Identity (Identity, runIdentity)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Morloc.CodeGenerator.Grammars.Translator.Imperative
  ( RNode (..)
  , rnodeChildren
  , rnodeLet
  , rnodeNames
  , rnodeBinds
  , consumableProjectionLets
  , consumedParams
  , borrowableRoot
  , isBorrowableProjection
  , loopBorrowLocalsExcept
  , ownedPartLets
  , usesIndex
  , nativeManifoldsUnder
  )
import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.Grammars.Common (capturesNothing, foldFnPositions)

-- | What a language's representation decides about ownership.
data OwnCaps = OwnCaps
  { ocCheap :: TypeF -> Bool
  -- ^ a type whose copy costs nothing
  , ocTagAfterTake :: Bool
  -- ^ whether a tag test may read a value after a field was taken from it
  , ocLends :: NativeExpr -> Bool
  -- ^ whether a part of a value, as this projection reads it, has a value of
  -- its own type in memory to lend a reference to (OWN-6)
  , ocSharedTakes :: Bool
  -- ^ whether a field may be taken out of a `data` arm whatever built it,
  -- the arm's own count of holders deciding at run time (CPP-6, EVAL-7)
  , ocTakesElems :: Bool
  -- ^ whether an element can be taken out of a list the frame owns while
  -- another part of the list is lent (OWN-5); where it cannot, the element
  -- is lent too
  , ocGivesToSources :: Bool
  -- ^ whether a sourced function may take an argument it is given whole,
  -- and a closure may be given its argument, so that a parameter passed
  -- whole to a sourced call, and a closure's bound parameter, have a
  -- by-value form (OWN-7, CPP-1)
  }

-- | Whether a projection may be borrowed rather than copied (OWN-6).
lendable :: OwnCaps -> NativeExpr -> Bool
lendable caps e = not (ocCheap caps (typeFof e)) && ocLends caps e

-- | The decisions that span a pool's manifolds.
data PoolPlan = PoolPlan
  { ppTakes :: Set.Set (Int, Int)
  -- ^ (top-level manifold, let index) pairs whose payload projection takes
  -- the payload (OWN-5)
  , ppByValue :: Map.Map Int (Set.Set Int)
  -- ^ per fully applied native manifold, the parameter positions it takes by
  -- value (OWN-7); also a fold's step and combine the fold alone calls
  -- (COPY-7)
  , ppReleases :: Set.Set (Int, Int)
  -- ^ the takes of a constructor field after which nothing names the value,
  -- which release its arm (CPP-6)
  , ppKeptAlways :: Map.Map Int (Set.Set Int)
  -- ^ per manifold with two forms, the positions of 'ppTwoForm' its body
  -- keeps whole on every evaluation, so the by-value form drops none of
  -- them (OWN-7)
  , ppTwoForm :: Map.Map Int (Set.Set Int)
  -- ^ per native manifold, the positions of the parameters it keeps, or
  -- keeps a part of, on some evaluations but takes by reference: a member
  -- may give it a second form taking them by value, for a call that may
  -- move every one of them (OWN-7); of a closure, only bound parameters
  , ppLent :: Map.Map Int LentResult
  -- ^ the manifolds whose by-reference form returns a result lent from a
  -- parameter or owned (OWN-8)
  , ppLentLets :: Map.Map (Int, Int) (Int, Set.Set Int)
  -- ^ (top-level manifold, let index) of the lets bound to a call of such a
  -- manifold that read the result where it lies, each with the manifold
  -- called and the caller's locals the result may be lent from (OWN-8)
  , ppCondLets :: Map.Map (Int, Int) CondLet
  -- ^ (top-level manifold, let index) of the lets bound to a conditional
  -- that may choose a part of a live local, read where it lies (OWN-10)
  }

-- | A let bound to a conditional read where it lies (OWN-10): the locals an
-- arm may lend from, the let's type, the keys ('condKey') of the
-- conditionals whose value is the let's, the locals an arm may lend, and
-- the lets of a part an arm lends.
data CondLet = CondLet
  { clSources :: Set.Set Int
  , clType :: TypeF
  , clIfs :: Set.Set (Set.Set Int)
  , clLeaves :: Set.Set Int
  , clParts :: Set.Set Int
  }
  deriving (Eq, Show)

-- | The lets a conditional binds, nested manifolds aside: the conditionals
-- of an 'CondLet' are named by them, since each binds once in its manifold.
condKey :: NativeExpr -> Set.Set Int
condKey e = go (RN e)
  where
    go n = case n of
      RNM _ -> Set.empty
      RSM _ -> Set.empty
      _ -> maybe Set.empty (\(i, _, _) -> Set.singleton i) (rnodeLet n) <> foldMap go (rnodeChildren n)

-- | The conditional lets of top-level manifold @root@.
condLetsIn :: PoolPlan -> Int -> [CondLet]
condLetsIn pp root = [cl | ((r, _), cl) <- Map.toList (ppCondLets pp), r == root]

-- | How a manifold's by-reference form returns a lent result (OWN-8): the
-- positions and indices of the parameters it may lend from, the result's type,
-- the carried slots of the loop its body is, and the locals of that loop a
-- base leaf may return lent ('lentLeaves'). A body that is a conditional
-- instead has no slots, and the keys ('condKey') of the conditionals whose
-- value is the result, the locals an arm may return lent, and the lets of a
-- part an arm lends ('condTree').
data LentResult = LentResult
  { lrParams :: [(Int, Int)]
  , lrType :: TypeF
  , lrSlots :: [Int]
  , lrLeaves :: Set.Set Int
  , lrIfs :: Set.Set (Set.Set Int)
  , lrParts :: Set.Set Int
  }
  deriving (Eq, Show)

emptyPoolPlan :: PoolPlan
emptyPoolPlan = PoolPlan {ppTakes = Set.empty, ppReleases = Set.empty, ppByValue = Map.empty, ppTwoForm = Map.empty, ppKeptAlways = Map.empty, ppLent = Map.empty, ppLentLets = Map.empty, ppCondLets = Map.empty}

planPool :: OwnCaps -> [SerialManifold] -> PoolPlan
planPool caps sms =
  PoolPlan
    { ppTakes = takes
    , ppReleases = releases
    , ppByValue = byValue
    , ppTwoForm = twoForm
    , ppKeptAlways = keptAlways
    , ppLent = lent
    , ppLentLets = lentLets
    , ppCondLets = condLets
    }
  where
    -- A part of a result read in place is a reference into it, not a take;
    -- nor is a part of a local a result or a conditional's value may be lent
    -- from (OWN-8, OWN-10), which the let's reads read.
    takes = takes0 `Set.difference` (lentParts <> sourceParts)
    releases = releases0 `Set.difference` (lentParts <> sourceParts)
    sourceParts =
      Set.fromList
        [ (root, z)
        | sm@(SerialManifold root _ _ _ _) <- sms
        , scope <- [n | n <- universe (RSM sm), isManifold n]
        , let lets = [(i, rhs) | n <- within scope, Just (i, RN rhs, _) <- [rnodeLet n]]
              here = Set.fromList (map fst lets)
              bases = Map.fromList [(i, b) | (i, rhs) <- lets, Just b <- [baseVar rhs]]
              up seen i = case Map.lookup i bases of
                Just j | Set.notMember j seen -> up (Set.insert j seen) j
                _ -> seen
              chain i = up (Set.singleton i) i
              lentFrom = [(y, a) | ((r, y), cl) <- Map.toList condLets, r == root, a <- Set.toList (clSources cl)] <> [(y, a) | ((r, y), (_, as)) <- Map.toList lentLets, r == root, a <- Set.toList as]
              sources = Set.unions [chain a | (y, a) <- lentFrom, Set.member y here]
        , not (Set.null sources)
        , (z, _) <- lets
        , Set.member (root, z) takes0
        , not (Set.null (Set.intersection sources (chain z)))
        ]
    lentParts =
      Set.fromList
        [ (root, z)
        | sm@(SerialManifold root _ _ _ _) <- sms
        , let lets = [(i, rhs) | n <- universe (RSM sm), Just (i, RN rhs, _) <- [rnodeLet n]]
              bases = Map.fromList [(i, b) | (i, rhs) <- lets, Just b <- [baseVar rhs]]
              lentHere = Set.fromList [y | (r, y) <- Map.keys lentLets <> Map.keys condLets, r == root]
              reaches seen i =
                Set.member i lentHere
                  || (Set.notMember i seen && maybe False (reaches (Set.insert i seen)) (Map.lookup i bases))
        , (z, _) <- lets
        , Set.member (root, z) takes0
        , maybe False (reaches (Set.singleton z)) (Map.lookup z bases)
        ]
    -- OWN-8: a fully applied manifold, never a function value, whose body is
    -- a tail loop with one parameter it may keep (OWN-7) that some base leaf
    -- returns as it is or a part of, through the loop's unchanged slot.
    lent = Map.fromList (loopLent <> condLent)
    -- OWN-8: a body that is a conditional whose arms are parameters it may
    -- keep, parts of them, or values of their own.
    condLent =
      [ (m, LentResult ps rt [] (clLeaves cl) (clIfs cl) (clParts cl))
      | NativeManifold m _ form@(ManifoldFull _) body <- natives
      , not (Set.member m asValues)
      , not (Set.member m selfCalled)
      , Nothing <- [spineLoop body]
      , let value = unReturn body
            rt = typeFof value
            two = Map.findWithDefault Set.empty m twoCore
            own = Map.findWithDefault Set.empty m byValue
            params = [(j', i') | (j', Arg i' (Native _)) <- indexed (typeMofForm form), Set.member j' two, Set.notMember j' own]
      , not (ocCheap caps rt)
      , not (isFunT rt)
      , not (isOptionalT rt)
      , Just cl <- [condTree caps keepsAll (\v -> v `elem` map snd params) rt value]
      , not (Set.null (clIfs cl))
      , all (not . Set.null) (Set.toList (clIfs cl))
      , let ps = [p | p@(_, i') <- params, Set.member i' (clSources cl)]
      , length ps == Set.size (clSources cl)
      ]
    unReturn e = case e of
      ReturnN x -> unReturn x
      NativeLetN i r b -> NativeLetN i r (unReturn b)
      _ -> e
    loopLent =
        [ (m, LentResult [(j, i)] rt (map fst starts) leaves Set.empty Set.empty)
        | NativeManifold m _ form@(ManifoldFull _) body <- natives
        , not (Set.member m asValues)
        , not (Set.member m selfCalled)
        , Just (rt, starts, lb) <- [spineLoop body]
        , not (ocCheap caps rt)
        , not (isFunT rt)
        , let two = Map.findWithDefault Set.empty m twoCore
              own = Map.findWithDefault Set.empty m byValue
              cands =
                [ (j', i', ls)
                | (j', Arg i' (Native _)) <- indexed (typeMofForm form)
                , Set.member j' two
                , Set.notMember j' own
                , let ls = lentLeaves caps i' starts lb
                , not (Set.null ls)
                ]
        , [(j, i, leaves)] <- [cands]
        ]
    isManifold n = case n of
      RNM _ -> True
      RSM _ -> True
      _ -> False
    -- the nodes of a manifold's own body, nested manifolds aside
    within top = top : concatMap go (rnodeChildren top)
      where
        go n = case n of
          RNM _ -> []
          RSM _ -> []
          _ -> n : concatMap go (rnodeChildren n)
    selfCalled = Set.fromList [m | (m, ms) <- recCalls, Set.member m ms]
    recCalls = [(m, Set.fromList [c | RN (AppExeN _ (RecCallP c _) _) <- universe (RN body)]) | NativeManifold m _ _ body <- natives]
    -- OWN-8: a let bound to a call of such a manifold, which passes a named
    -- local where the result may be lent from and reads the result only in
    -- place.
    lentLets =
      Map.fromList
        [ ((root, y), a)
        | sm@(SerialManifold root _ _ _ _) <- sms
        , let ns = universe (RSM sm)
              bound = Map.fromListWith (+) [(i, 1 :: Int) | n <- ns, Just (i, _, _) <- [rnodeLet n]]
        , n <- ns
        , Just (y, RN rhs, body) <- [rnodeLet n]
        , Map.lookup y bound == Just 1
        , Just a@(_, vs) <- [lentArg rhs]
        , Set.notMember y vs
        , readsInPlace caps keepsAll y body
        ]
    keepsAll c = Map.findWithDefault Set.empty c byValue <> Map.findWithDefault Set.empty c twoCore
    -- OWN-10: a let bound once to a conditional whose every arm is a local,
    -- a part of one, or a fresh value, read in place.
    condLets =
      Map.fromList
        [ ((root, y), cl)
        | sm@(SerialManifold root _ _ _ _) <- sms
        , let ns = universe (RSM sm)
              bound = Map.fromListWith (+) [(i, 1 :: Int) | n <- ns, Just (i, _, _) <- [rnodeLet n]]
              once i = Map.findWithDefault 0 i bound <= 1
              taken = Set.fromList [i | (r, i) <- Set.toList (takes0 <> releases0), r == root]
        , n <- ns
        , Just (y, RN rhs@(IfN t _ _ _), body) <- [rnodeLet n]
        , Map.lookup y bound == Just 1
        , Map.notMember (root, y) lentLets
        , not (ocCheap caps t)
        , not (isFunT t)
        , not (isOptionalT t)
        , let inner = condKey rhs
        , all once (Set.toList inner)
        , Set.null (Set.intersection inner taken)
        , Just cl <- [condTree caps keepsAll (\v -> neededAfter v y (RSM sm)) t rhs]
        , Set.notMember y (clSources cl)
        , all once (Set.toList (clSources cl))
        , not (Set.null (clLeaves cl))
        , all (not . Set.null) (Set.toList (clIfs cl))
        , readsInPlace caps (keepsLent root y body) y body
        ]
    -- A call of an OWN-8 manifold bound by a let that reads its result in
    -- place takes the argument it may lend from by reference.
    keepsLent root y body c = case Map.lookup c lent of
      Just lr
        | let passes = length [() | RN e <- universe body, Just (c', vs) <- [lentArg e], c' == c, Set.member y vs]
              bound = Map.size (Map.filterWithKey (\(r, z) (c', vs) -> r == root && c' == c && Set.member y vs && rnodeBinds z body) lentLets)
        , passes == bound ->
            keepsAll c `Set.difference` Set.fromList (map fst (lrParams lr))
      _ -> keepsAll c
    isOptionalT OptionalF {} = True
    isOptionalT _ = False
    lentArg rhs = case rhs of
      ManN (NativeManifold m _ form@(ManifoldFull _) _)
        | Just lr <- Map.lookup m lent ->
            named m lr [Just i | (j, Arg i _) <- indexed (typeMofForm form), j `elem` map fst (lrParams lr)]
      AppExeN _ (RecCallP m Nothing) args
        | Just lr <- Map.lookup m lent ->
            named m lr [varIx v | (j, NativeArgExpr v) <- indexed args, j `elem` map fst (lrParams lr)]
      _ -> Nothing
    named m lr vs = case sequence vs of
      Just xs | length xs == length (lrParams lr) -> Just (m, Set.fromList xs)
      _ -> Nothing
    keptAlways =
      Map.filter (not . Set.null) $
        Map.fromList
          [ (m, Set.filter (\j -> any (\(k, Arg i _) -> k == j && Set.member i always) (indexed (typeMofForm form))) positions)
          | NativeManifold m _ form@(ManifoldFull _) body <- natives
          , Just positions <- [Map.lookup m twoForm]
          , let keeps c = Map.findWithDefault Set.empty c byValue <> Map.findWithDefault Set.empty c twoCore
                always = keptOnEveryPath keeps body
          ]
    (takes0, releases0) = consumableProjectionLets (ocTagAfterTake caps) (ocSharedTakes caps) (\m -> Map.findWithDefault Set.empty m byValue <> Map.findWithDefault Set.empty m twoCore) sms
    -- The second forms the lent, conditional and take analyses see; the
    -- forms a sourced call or a closure's bound parameter adds (OWN-7,
    -- CPP-1) are only emitted and selected by calls.
    twoCore = settleTwo False Set.empty Map.empty
    twoForm = if ocGivesToSources caps then unblocked Set.empty else twoCore
    settleTwo given blocked known =
      let next = twoFormGiven given blocked known
       in if next == known then known else settleTwo given blocked next
    -- A call selects a second form only when it may move every argument
    -- the form takes by value (OWN-7), so a position no call can give is
    -- dropped, and the forms are solved again without it.
    unblocked blocked =
      let two = settleTwo True blocked Map.empty
          blocked' = blocked <> Set.fromList [(m, j) | (m, js) <- Map.toList two, j <- Set.toList js, neverGiven two m j]
       in if blocked' == blocked then two else unblocked blocked'
    neverGiven two m j =
      let args = [a | (m', j', a) <- occurrences, m' == m, j' == j]
       in not (null args) && all (cannotGive two) args
    -- A local a call passes cannot be moved when it is a parameter of the
    -- calling manifold that it takes by reference in every form: a
    -- closure's captured value, or a parameter neither kept nor in a second
    -- form. A let or a top-level manifold's argument may be moved.
    cannotGive two (Just (NativeManifold c _ form _), i) =
      case [(k, isFull form || Set.member i free) | (k, Arg i' _) <- indexed (typeMofForm form), i' == i] of
        [(k, bound)] -> not bound || not (Set.member k (Map.findWithDefault Set.empty c byValue <> Map.findWithDefault Set.empty c two))
        _ -> False
      where
        free = Set.fromList [v | Arg v _ <- manifoldBound form]
    cannotGive _ (Nothing, _) = False
    -- Each call of a fully applied manifold with the manifold it is in and
    -- the local it passes at each position.
    occurrences = concat [callsIn Nothing (RSM sm) | sm <- sms]
    callsIn caller n = case n of
      RNM nm@(NativeManifold m _ form body) ->
        here caller (m, form) <> callsIn' (Just nm) (RN body)
      _ -> here' caller n <> concatMap (callsIn caller) (rnodeChildren n)
    callsIn' caller n = here' caller n <> concatMap (callsIn caller) (rnodeChildren n)
    here caller (m, ManifoldFull as) = [(m, j, (caller, i)) | (j, Arg i _) <- indexed as]
    here _ _ = []
    here' caller n = case n of
      RN (AppExeN _ (RecCallP m _) as) -> [(m, j, (caller, i)) | (j, NativeArgExpr v) <- indexed as, Just i <- [varIx v]]
      _ -> []
    twoFormGiven given blocked known =
      Map.filter (not . Set.null) . Map.fromListWith Set.intersection $
        [ (m, positions)
        | NativeManifold m _ form body <- natives
        , case form of
            ManifoldFull _ -> not (Set.member m asValues)
            _ -> given && Set.notMember m fullOccurs
        , let args = typeMofForm form
              free = Set.fromList [i | Arg i _ <- manifoldBound form]
              own = Map.findWithDefault Set.empty m byValue
              keeps c = Map.findWithDefault Set.empty c byValue <> Map.findWithDefault Set.empty c known
              kept = keptSomewhere keeps body <> (if given then givenToSources body else Set.empty)
              sources = letSources (RN body)
              positions =
                Set.fromList
                  [ j
                  | (j, Arg i (Native tf)) <- indexed args
                  , not (ocCheap caps tf)
                  , not (isFunT tf)
                  , Set.notMember j own
                  , Set.notMember (m, j) blocked
                  , isFull form || Set.member i free
                  , not (Set.null (Set.intersection kept (flowsFrom sources i)))
                  ]
        ]
    isFull ManifoldFull {} = True
    isFull _ = False
    fullOccurs = Set.fromList [m | NativeManifold m _ ManifoldFull {} _ <- natives]
    isFunT FunF {} = True
    isFunT _ = False
    byValue = Map.unionWith Set.union (settle Map.empty) foldParams
    -- A fold's step and combine that capture nothing, and occur nowhere
    -- else, are called by the fold alone, which hands over the accumulator
    -- (COPY-7): the step's first parameter and both of combine's.
    foldParams = Map.filterWithKey (\m _ -> Map.findWithDefault 0 m foldUses == Map.findWithDefault (0 :: Int) m manUses) $
      Map.fromListWith Set.intersection
        [ (m, Set.fromList [j | (j, Arg _ (Native tf)) <- indexed (typeMofForm form), j `elem` ps, not (ocCheap caps tf)])
        | (m, form, ps) <- foldFns
        ]
    foldFns =
      [ (m, form, ps)
      | RN (IntrinsicN _ intr _ args) <- nodes
      , (pos, ps) <- foldFnPositions intr
      , ManN (NativeManifold m _ form _) <- take 1 (drop pos args)
      , capturesNothing form
      ]
    foldUses = Map.fromListWith (+) [(m, 1) | (m, _, _) <- foldFns]
    manUses = Map.fromListWith (+) [(m, 1) | RN (ManN (NativeManifold m _ _ _)) <- nodes]
    nodes = concatMap (universe . RSM) sms
    universe n = n : concatMap universe (rnodeChildren n)
    -- A parameter passed on to a by-value parameter is kept too, so the
    -- positions grow from the callees up until nothing changes (OWN-7).
    settle known =
      let next = byValueGiven known
       in if next == known then known else settle next
    byValueGiven known =
      Map.filter (not . Set.null) . Map.fromListWith Set.intersection $
        [ (m, positions)
        | nm@(NativeManifold m _ form@(ManifoldFull _) _) <- natives
        , not (Set.member m asValues)
        , let args = typeMofForm form
              cands = Set.fromList [i | Arg i (Native tf) <- args, not (ocCheap caps tf)]
              kept = consumedParams (\c -> Map.findWithDefault Set.empty c known) (lendable caps) cands nm
              positions = Set.fromList [j | (j, Arg i _) <- indexed args, Set.member i kept]
        ]
    natives = concatMap (nativeManifoldsUnder . RSM) sms
    -- A manifold also used as a function value is called with what its
    -- caller holds, and one occurring more than once takes by value only
    -- what every occurrence does (OWN-7).
    asValues = Set.unions (map (valueIds . RSM) sms)
    valueIds n = here n <> Set.unions (map valueIds (rnodeChildren n))
      where
        here (RNM (NativeManifold m _ form _)) | notFull form = Set.singleton m
        here (RSM (SerialManifold m _ form _ _)) | notFull form = Set.singleton m
        here _ = Set.empty
    notFull ManifoldFull {} = False
    notFull _ = True

-- | The variables a native expression passes whole to a sourced call that
-- is not an operator, nested function values, suspensions and do blocks
-- aside (OWN-3): a sourced function may take such an argument (CPP-1).
givenToSources :: NativeExpr -> Set.Set Int
givenToSources body = go (RN body)
  where
    go n = case n of
      RN (DoBlockN _ _) -> Set.empty
      RNM _ -> Set.empty
      RSM _ -> Set.empty
      RN (AppExeN _ (SrcCallP _ src) args)
        | not (srcOperator src) ->
            Set.fromList [p | NativeArgExpr a <- args, Just p <- [varOf a]] <> foldMap go (rnodeChildren n)
      _ -> foldMap go (rnodeChildren n)
    varOf (LetVarN _ p _) = Just p
    varOf (BndVarN _ p _) = Just p
    varOf _ = Nothing

-- | The variables a native expression's reads keep anywhere in it: an
-- element of a constructor, list, tuple or record; a value it or a let's
-- right-hand side ends in, on either arm of a conditional; a loop's start
-- or next value; and an argument
-- a call takes by value (@keeps m@, the positions manifold @m@ takes or may
-- take by value). Nested function values and suspensions are their own
-- scopes (OWN-3).
keptSomewhere :: (Int -> Set.Set Int) -> NativeExpr -> Set.Set Int
keptSomewhere keeps body = keptUnder varOf varOf keeps (RN body)
  where
    varOf (LetVarN _ p _) = Just p
    varOf (BndVarN _ p _) = Just p
    varOf _ = Nothing

-- | 'keptSomewhere' under any node, given the variable a kept expression
-- names and the variable a let's right-hand side names.
keptUnder :: (NativeExpr -> Maybe Int) -> (NativeExpr -> Maybe Int) -> (Int -> Set.Set Int) -> RNode -> Set.Set Int
keptUnder varOf letVar keeps top = case top of
  RN body -> ends varOf body <> go top
  _ -> go top
  where
    go n = case n of
      RN (DoBlockN _ _) -> Set.empty
      RNM (NativeManifold m _ form@(ManifoldFull _) _) ->
        Set.fromList [p | (j, Arg p _) <- indexed (typeMofForm form), Set.member j (keeps m)]
      RNM _ -> Set.empty
      RSM _ -> Set.empty
      RN e -> here e <> foldMap go (rnodeChildren n)
      RLN (LoopContinue es) -> vars (map snd es) <> foldMap go (rnodeChildren n)
      RLS (LoopContinue es) -> vars (map snd es) <> foldMap go (rnodeChildren n)
      RLN (LoopBase e) -> ends varOf e <> foldMap go (rnodeChildren n)
      _ -> foldMap go (rnodeChildren n)
    here e = case e of
      VariantN _ _ _ xs -> vars xs
      ListN _ _ xs -> vars xs
      TupleN _ xs -> vars xs
      RecordN _ _ _ kvs -> vars (map snd kvs)
      AppExeN _ (RecCallP mid _) args -> Set.fromList [p | (j, NativeArgExpr a) <- indexed args, Just p <- [varOf a], Set.member j (keeps mid)]
      LoopN _ starts _ -> vars (map snd starts)
      NativeLetN _ rhs _ -> ends letVar rhs
      _ -> Set.empty
    ends v e = case e of
      NativeLetN _ _ b -> ends v b
      IfN _ _ a b -> ends v a <> ends v b
      ReturnN x -> ends v x
      CoerceN _ _ x -> ends v x
      _ -> maybe Set.empty Set.singleton (v e)
    vars xs = Set.fromList [p | Just p <- map varOf xs]

-- | The variables every evaluation of a body keeps whole, by the keeping
-- reads of 'keptSomewhere'; an arm of a conditional counts only what both
-- arms keep.
keptOnEveryPath :: (Int -> Set.Set Int) -> NativeExpr -> Set.Set Int
keptOnEveryPath keeps = go
  where
    go e = case e of
      NativeLetN _ rhs b -> go rhs <> go b
      IfN _ _ a b -> Set.intersection (go a) (go b)
      ReturnN x -> go x
      CoerceN _ _ x -> go x
      _ -> keptSomewhere keeps e `Set.intersection` unconditional e
    -- the variables a node keeps outside any conditional it holds
    unconditional e = case e of
      IfN {} -> Set.empty
      DoBlockN {} -> Set.empty
      _ -> keptSomewhere keeps e `Set.difference` underIf (RN e)
    underIf n = case n of
      RN e@(IfN {}) -> keptSomewhere keeps e
      RNM _ -> Set.empty
      RSM _ -> Set.empty
      _ -> foldMap underIf (rnodeChildren n)

-- | Each native let under a node, with the variables its value may be or
-- refer into: the variable a projection reads from, or a value its
-- right-hand side ends in.
letSources :: RNode -> [(Int, Set.Set Int)]
letSources n = maybe [] (\(i, rhs, _) -> [(i, sourcesOf rhs)]) (rnodeLet n) <> concatMap letSources (rnodeChildren n)
  where
    sourcesOf (RN e) = from e
    sourcesOf _ = Set.empty
    from e = case e of
      LetVarN _ j _ -> Set.singleton j
      BndVarN _ j _ -> Set.singleton j
      IntrinsicN _ IntrCtorField _ (x : _) -> from x
      IntrinsicN _ IntrListElem _ (x : _) -> from x
      PatN _ (PatGet _ [] (NativeArgExpr x)) -> from x
      NativeLetN _ _ b -> from b
      IfN _ _ a b -> from a <> from b
      CoerceN _ _ x -> from x
      _ -> Set.empty

-- | A variable and every let whose value may be it or refer into it.
flowsFrom :: [(Int, Set.Set Int)] -> Int -> Set.Set Int
flowsFrom sources = grow . Set.singleton
  where
    grow s =
      let s' = s <> Set.fromList [i | (i, from) <- sources, not (Set.null (Set.intersection from s))]
       in if Set.size s' == Set.size s then s else grow s'

-- | The loop a manifold body is, past the lets it binds first: the loop's
-- result type, its starts and its body.
spineLoop :: NativeExpr -> Maybe (TypeF, [(Int, NativeExpr)], LoopBody NativeExpr SerialExpr NativeExpr)
spineLoop e = case e of
  NativeLetN _ _ b -> spineLoop b
  ReturnN x -> spineLoop x
  LoopN t starts lb -> Just (t, starts, lb)
  _ -> Nothing

-- | The locals of a loop that hold parameter @p@ as the loop was lent it,
-- or a part of it (OWN-8), when some base leaf returns one of them, and
-- none otherwise: a slot that starts from @p@ and no continue changes, and
-- a let, bound once, to such a local or to a projection of one that has a
-- value of its own type in memory (OWN-6) and is not optional.
lentLeaves :: OwnCaps -> Int -> [(Int, NativeExpr)] -> LoopBody NativeExpr SerialExpr NativeExpr -> Set.Set Int
lentLeaves caps p starts lb = if hit then known else Set.empty
  where
    (known, hit) = go (Set.fromList slots) lb
    slots = [k | (k, s) <- starts, varIx s == Just p, all (maybe True (unchangedSlot k) . lookup k) (continues lb)]
    bound = Map.fromListWith (+) [(i, 1 :: Int) | i <- letsOf lb]
    go ks b = case b of
      LoopIf _ t f -> let (a, x) = go ks t; (c, z) = go ks f in (a <> c, x || z)
      LoopNLet y rhs r
        | Map.lookup y bound == Just 1, placeIn ks rhs -> go (Set.insert y ks) r
        | otherwise -> go ks r
      LoopSLet _ _ r -> go ks r
      LoopContinue _ -> (ks, False)
      LoopBase x -> (ks, maybe False (`Set.member` ks) (varIx x))
    placeIn ks e =
      lendable caps e && not (isOptional (typeFof e)) && case e of
        LetVarN _ v _ -> Set.member v ks
        BndVarN _ v _ -> Set.member v ks
        IntrinsicN _ IntrCtorField _ (s : _) -> base ks s
        IntrinsicN _ IntrListElem _ (s : _) -> base ks s
        PatN _ (PatGet sel [] (NativeArgExpr s)) ->
          not (selectorHasBracket sel) && length (ungroup sel) == 1 && base ks s
        _ -> False
    base ks s = maybe False (`Set.member` ks) (varIx s) || placeIn ks s
    isOptional OptionalF {} = True
    isOptional _ = False
    continues b = case b of
      LoopIf _ t f -> continues t <> continues f
      LoopNLet _ _ r -> continues r
      LoopSLet _ _ r -> continues r
      LoopBase _ -> []
      LoopContinue xs -> [xs]
    letsOf b = case b of
      LoopIf _ t f -> letsOf t <> letsOf f
      LoopNLet y _ r -> y : letsOf r
      LoopSLet _ _ r -> letsOf r
      _ -> []

-- | A conditional whose arms, past the lets each binds, are a conditional
-- of the same kind, a part of a local bound outside it (a projection that
-- has a value of its own type in memory, OWN-6, through lets bound to such
-- parts), or a value of their own; when nothing in it keeps a local an
-- arm may lend from or a part of one. An arm that
-- is the local itself lends it when @whole@ holds of it, and owns it
-- otherwise, as a move there copies nothing.
condTree :: OwnCaps -> (Int -> Set.Set Int) -> (Int -> Bool) -> TypeF -> NativeExpr -> Maybe CondLet
condTree caps keeps whole t top = do
  (ifs, leaves, parts, sources) <- arm Map.empty top
  -- an arm that is a local itself is not a keep of it, any other read is
  let kept = keptSomewhere keeps (dropWhole (Set.intersection leaves sources) top) `Set.difference` (leaves `Set.difference` sources)
  if not (Set.null sources) && Set.null (Set.intersection (sources <> parts) kept)
    then Just (CondLet sources t (Set.fromList (map condKey ifs)) leaves parts)
    else Nothing
  where
    dropWhole ws e = case e of
      NativeLetN i r@IfN {} b@(LetVarN _ j _) | i == j -> NativeLetN i (dropWhole ws r) b
      NativeLetN i r b -> NativeLetN i r (dropWhole ws b)
      IfN ty c a b -> IfN ty c (dropWhole ws a) (dropWhole ws b)
      _ | Just v <- varIx e, Set.member v ws -> NullN (typeFof e)
      _ -> e
    arm env e = case e of
      NativeLetN i r@IfN {} (LetVarN _ j _) | i == j -> arm env r
      NativeLetN i r b -> arm (Map.insert i r env) b
      IfN _ _ a b -> do
        (i1, l1, p1, s1) <- arm env a
        (i2, l2, p2, s2) <- arm env b
        Just (e : i1 <> i2, l1 <> l2, p1 <> p2, s1 <> s2)
      _ | Just v <- varIx e, Just r <- Map.lookup v env -> case part env r of
          Just (ls, src) -> Just ([], Set.singleton v, Set.fromList (v : ls), Set.singleton src)
          Nothing -> fresh
      _ | Just v <- varIx e, Map.notMember v env, whole v -> Just ([], Set.singleton v, Set.empty, Set.singleton v)
      _ -> fresh
    fresh = Just ([], Set.empty, Set.empty, Set.empty)
    part env e
      | lendable caps e, not (isOptional (typeFof e)) = case e of
          IntrinsicN _ IntrCtorField _ (s : _) -> base env s
          IntrinsicN _ IntrListElem _ (s : _) -> base env s
          PatN _ (PatGet sel [] (NativeArgExpr s))
            | not (selectorHasBracket sel), length (ungroup sel) == 1 -> base env s
          _ -> Nothing
      | otherwise = Nothing
    base env s = case varIx s of
      Just v -> case Map.lookup v env of
        Nothing -> Just ([], v)
        Just r -> (\(ls, src) -> (v : ls, src)) <$> part env r
      Nothing -> part env s
    isOptional OptionalF {} = True
    isOptional _ = False

-- | Whether local @x@ is named where it may be read after, or in the same
-- step as, let @y@ under a node: in the let's body, a later step, a part of
-- the same step, or a body that may run again. True when @y@ is not found.
neededAfter :: Int -> Int -> RNode -> Bool
neededAfter x y top = fromMaybe True (asum (map (go False) (rnodeChildren top)))
  where
    names = rnodeNames x
    go after n = case rnodeLet n of
      Just (i, rhs, body)
        | i == y -> Just (after || names body)
        | otherwise -> go (after || names body) rhs <|> go after body
      Nothing -> case n of
        RN (IfN _ c a b) -> branch after (RN c) [RN a, RN b]
        RLN (LoopIf c a b) -> branch after (RN c) [RLN a, RLN b]
        RLS (LoopIf c a b) -> branch after (RN c) [RLS a, RLS b]
        _
          | repeats n -> asum (map (go (after || names n)) (rnodeChildren n))
          | otherwise ->
              let kids = rnodeChildren n
               in asum [go (after || any names (others k kids)) c | (k, c) <- indexed kids]
    branch after c arms = go (after || any names arms) c <|> asum (map (go after) arms)
    others k kids = [c | (k', c) <- indexed kids, k' /= k]
    repeats n = case n of
      RN (LoopN {}) -> True
      RS (LoopS {}) -> True
      RN (DoBlockN {}) -> True
      RN (IntrinsicN _ IntrTry _ _) -> True
      RNM _ -> True
      RSM _ -> True
      _ -> False

varIx :: NativeExpr -> Maybe Int
varIx (LetVarN _ j _) = Just j
varIx (BndVarN _ j _) = Just j
varIx _ = Nothing

-- | Whether every read of local @y@ under a node reads it in place (OWN-8):
-- no read keeps it or a part of it ('keptUnder', @keeps@ as there),
-- no closure captures it, no let binds it or a part of it, and it is read
-- only by a call, a projection, a conditional's test, or an intrinsic that
-- only reads its argument.
readsInPlace :: OwnCaps -> (Int -> Set.Set Int) -> Int -> RNode -> Bool
readsInPlace caps keeps y body = Set.notMember y kept && all ok (universe body)
  where
    kept = keptUnder deepVar varIx keeps body
    universe n = n : concatMap universe (rnodeChildren n)
    names e = deepVar e == Just y
    ok n = case n of
      RNM (NativeManifold _ _ form _) -> isFull form || notCaptured form
      RSM (SerialManifold _ _ form _ _) -> notCaptured form
      RN (NativeLetN z rhs b) -> not (endsIn rhs) || partLet z rhs (RN b)
      RLN (LoopNLet z rhs b) -> not (endsIn rhs) || partLet z rhs (RLN b)
      RN (PatN _ (PatGet sel [] (NativeArgExpr s))) | names s -> not (selectorHasBracket sel) && length (ungroup sel) == 1
      RN (PatN _ app) -> not (any (\a -> case a of NativeArgExpr x -> names x; _ -> False) (toList app))
      RN (IntrinsicN _ intr _ xs) | any names xs -> intr `elem` [IntrCtorField, IntrListElem, IntrTagTest, IntrListLenTest, IntrShow, IntrHash, IntrEq, IntrLe, IntrSchema, IntrTypeof]
      RN (IfN _ _ a b) -> not (endsIn a || endsIn b)
      RN (AppExeN _ (PapplyP _) args) -> not (any names [x | NativeArgExpr x <- args])
      RN (CoerceN _ _ x) -> not (names x)
      RN (MapOptionalN _ _ _ x) -> not (names x)
      RN (NativeToWireN _ _ x) -> not (names x)
      RN (DoBlockN _ x) -> not (endsIn x)
      RN (EvalN _ x) -> not (endsIn x)
      _ -> True
    -- a scalar read out of it, or a part of it bound by reference that is
    -- itself read only in place
    partLet z rhs b =
      ocCheap caps (typeFof rhs)
        || (isPart rhs && readsInPlace caps keeps z b)
    isPart e = lendable caps e && case e of
      IntrinsicN _ IntrCtorField _ (s : _) -> names s
      IntrinsicN _ IntrListElem _ (s : _) -> names s
      PatN _ (PatGet sel [] (NativeArgExpr s)) -> not (selectorHasBracket sel) && length (ungroup sel) == 1 && names s
      _ -> False
    isFull ManifoldFull {} = True
    isFull _ = False
    notCaptured form = all (\(Arg j _) -> j /= y) (manifoldContext form)
    endsIn e = case e of
      NativeLetN _ _ b -> endsIn b
      IfN _ _ a b -> endsIn a || endsIn b
      CoerceN _ _ x -> endsIn x
      _ -> names e

-- | The variable at the base of a projection, or the variable itself.
deepVar :: NativeExpr -> Maybe Int
deepVar e = case e of
  LetVarN _ j _ -> Just j
  BndVarN _ j _ -> Just j
  IntrinsicN _ IntrCtorField _ (x : _) -> deepVar x
  IntrinsicN _ IntrListElem _ (x : _) -> deepVar x
  PatN _ (PatGet _ _ (NativeArgExpr x)) -> deepVar x
  CoerceN _ _ x -> deepVar x
  _ -> Nothing

-- | The value a right-hand side ends in, past the lets it binds for itself.
letValue :: NativeExpr -> NativeExpr
letValue (NativeLetN _ _ b) = letValue b
letValue e = e

-- | The parameter indices manifold @m@ takes by value.
byValueIndices :: PoolPlan -> Int -> ManifoldForm (Or TypeS TypeF) TypeF -> Set.Set Int
byValueIndices pp m form =
  Set.fromList [i | (j, Arg i _) <- indexed (typeMofForm form), Set.member j positions]
  where
    positions = Map.findWithDefault Set.empty m (ppByValue pp)

-- | How a native let binds its right-hand side: a copy, a reference to a
-- projection out of a live value, the projected payload taken (OWN-5), or
-- a call's or a conditional's value lent from one of the given locals or
-- owned (OWN-8, OWN-10).
data LetMode = LetCopy | LetBorrow | LetTake | LetLent (Set.Set Int)
  deriving (Eq, Show)

-- | The mode of let @i@ in top-level manifold @root@ with right-hand side
-- @rhs@ (OWN-5, OWN-6). A take is a payload projection the pool plan
-- takes; a borrow is any projection of a variable, through fields and list
-- elements.
letMode :: PoolPlan -> Int -> Int -> NativeExpr -> LetMode
letMode pp root i rhs
  | Just (_, as) <- Map.lookup (root, i) (ppLentLets pp) = LetLent as
  | Just cl <- Map.lookup (root, i) (ppCondLets pp) = LetLent (clSources cl)
  | IntrinsicN _ IntrCtorField _ (LetVarN {} : _) <- rhs, Set.member (root, i) (ppTakes pp) = LetTake
  | IntrinsicN _ IntrCtorField _ (BndVarN {} : _) <- rhs, Set.member (root, i) (ppTakes pp) = LetTake
  | PatN _ (PatGet _ [] (NativeArgExpr _)) <- rhs, Set.member (root, i) (ppTakes pp) = LetTake
  | IntrinsicN _ IntrListElem _ (_ : _) <- rhs, Set.member (root, i) (ppTakes pp) = LetTake
  | isPlace rhs = LetBorrow
  | otherwise = LetCopy
  where
    isPlace e = case e of
      IntrinsicN _ IntrListElem _ (s : _) -> borrowableRoot s || isPlace s
      IntrinsicN _ IntrCtorField _ (s : _) -> borrowableRoot s || isPlace s
      PatN _ (PatGet sel [] (NativeArgExpr s)) ->
        not (selectorHasBracket sel) && length (ungroup sel) == 1 && (borrowableRoot s || isPlace s)
      _ -> False

-- | What last-read marking and the move check need to know about a
-- manifold's lets, given how a member binds each one: the references, each
-- mapped to the variables it may refer into (OWN-2); the lets
-- that hold a value of their own; and the values that must never move. An
-- index bound more than once is never moved, and neither is anything one of
-- its bindings refers into.
data LetPlan = LetPlan
  { lpAliases :: Map.Map Int [Int]
  , lpOwned :: Set.Set Int
  , lpHeld :: Set.Set Int
  }

letPlan :: [(Int, NativeExpr, LetMode)] -> LetPlan
letPlan lets =
  LetPlan
    { lpAliases =
        Map.fromList
          ( [(i, [r]) | (i, rhs, LetBorrow) <- lets, not (Set.member i reused), Just r <- [baseVar rhs]]
              <> [(i, Set.toList rs) | (i, _, LetLent rs) <- lets, not (Set.member i reused)]
          )
    , lpOwned = Set.fromList [i | (i, _, mode) <- lets, owns mode, not (Set.member i reused)]
    , lpHeld = Set.fromList ([i | i <- Set.toList reused] <> [r | (i, rhs, LetBorrow) <- lets, Set.member i reused, Just r <- [baseVar rhs]])
    }
  where
    reused = Map.keysSet (Map.filter (> (1 :: Int)) (Map.fromListWith (+) [(i, 1) | (i, _, _) <- lets]))
    owns LetBorrow = False
    owns (LetLent _) = False
    owns _ = True

-- | The variable at the base of a projection a let may refer into.
baseVar :: NativeExpr -> Maybe Int
baseVar e = case e of
  NativeLetN _ _ b -> baseVar b
  LetVarN _ j _ -> Just j
  BndVarN _ j _ -> Just j
  IntrinsicN _ IntrCtorField _ (x : _) -> baseVar x
  IntrinsicN _ IntrListElem _ (x : _) -> baseVar x
  PatN _ (PatGet _ [] (NativeArgExpr x)) -> baseVar x
  _ -> Nothing

-- | Every native let under a manifold, with its right-hand side, nested
-- manifolds included.
nativeLets :: SerialManifold -> [(Int, NativeExpr)]
nativeLets sm = go (RSM sm)
  where
    go n = maybe [] (\(i, rhs, _) -> [(i, rhsExpr rhs)]) (rnodeLet n) <> concatMap go (rnodeChildren n)
    rhsExpr (RN e) = e
    rhsExpr _ = error "morloc bug: a native let whose right-hand side is not native"

-- | Whether a loop continue gives carried slot @i@ the value it already
-- holds, so the slot need not be reassigned.
unchangedSlot :: Int -> NativeExpr -> Bool
unchangedSlot i e = case e of
  LetVarN _ j _ -> i == j
  BndVarN _ j _ -> i == j
  _ -> False

-- | The carried slots a continue reassigns, with their new values and their
-- IR values: every slot, less those whose IR value is the slot itself.
changedSlots :: [(Int, NativeExpr, a)] -> [(Int, a, NativeExpr)]
changedSlots slots = [(i, v, o) | (i, o, v) <- slots, not (unchangedSlot i o)]

-- | The decisions within one manifold's body.
data ManifoldPlan = ManifoldPlan
  { mpBorrowed :: Set.Set Int
  -- ^ indices that hold a reference: borrowed parameters and the locals that
  -- borrow from them (OWN-6)
  , mpShared :: Set.Set Int
  -- ^ indices read at more than one point (OWN-2)
  , mpCarried :: Set.Set Int
  -- ^ the loop-carried indices of a native-loop manifold
  , mpOwnedParts :: Map.Map Int Int
  -- ^ the borrowed locals that refer into a value the manifold owns, each
  -- with that value ('ownedPartLets')
  }

planSerialManifold :: OwnCaps -> PoolPlan -> SerialManifold -> ManifoldPlan
planSerialManifold caps pp sm@(SerialManifold _ _ form _ e) =
  (borrowPlan caps pp (borrowedIndicesOfForm caps form) (RSM sm))
    { mpShared = sharedOf (runIdentity (foldSerialExprM varUseCountOps e)) `Set.union` carried `Set.union` lentArgsUnder pp (RSM sm)
    , mpCarried = carried
    }
  where
    carried = loopCarriedIdsSM sm

planNativeManifold :: OwnCaps -> PoolPlan -> NativeManifold -> ManifoldPlan
planNativeManifold caps pp nm@(NativeManifold m _ form e) =
  (borrowPlan caps pp borrowed (RNM nm))
    { mpShared = sharedOf (runIdentity (foldNativeExprM varUseCountOps e)) `Set.union` lentArgsUnder pp (RNM nm)
    }
  where
    borrowed = borrowedIndicesOfForm caps form `Set.difference` byValueIndices pp m form

-- | The locals a let under a node reads in place a result may be lent
-- from: each is read again wherever the let is (OWN-2, OWN-8).
lentArgsUnder :: PoolPlan -> RNode -> Set.Set Int
lentArgsUnder pp n = Set.unions [a | m <- universe n, Just (i, _, _) <- [rnodeLet m], Just a <- [Map.lookup i byLet]]
  where
    byLet = Map.fromList ([(i, as) | ((_, i), (_, as)) <- Map.toList (ppLentLets pp)] <> [(i, clSources cl) | ((_, i), cl) <- Map.toList (ppCondLets pp)])
    universe x = x : concatMap universe (rnodeChildren x)

-- | The borrows of a manifold given its borrowed parameters: the locals
-- that borrow from them (OWN-6), the lets that refer into a value the
-- manifold owns, and the locals that borrow from those.
borrowPlan :: OwnCaps -> PoolPlan -> Set.Set Int -> RNode -> ManifoldPlan
borrowPlan caps pp borrowed n =
  ManifoldPlan
    { mpBorrowed = refs
    , mpShared = Set.empty
    , mpCarried = Set.empty
    , mpOwnedParts = Map.filter (`Set.notMember` refs) owned
    }
  where
    -- A part lent from a value a take later empties is read before it.
    owned = Map.filterWithKey (\i j -> not (any (afterTake i j) takes)) (ownedPartLets (lendable caps) (not (ocTakesElems caps)) borrowed n)
    taken = Set.map snd (ppTakes pp)
    takes = [(j, body) | m <- universe n, Just (i, RN rhs, body) <- [rnodeLet m], Set.member i taken, Just j <- [takeSubject rhs]]
    afterTake i j (j', body) = j == j' && (usesIndex i body || i `elem` boundIn body)
    boundIn m = maybe [] (\(i, _, _) -> [i]) (rnodeLet m) <> concatMap boundIn (rnodeChildren m)
    takeSubject rhs = case rhs of
      IntrinsicN _ IntrCtorField _ (LetVarN _ j _ : _) -> Just j
      PatN _ (PatGet _ [] (NativeArgExpr x)) -> case x of
        LetVarN _ j _ -> Just j
        BndVarN _ j _ -> Just j
        _ -> Nothing
      _ -> Nothing
    universe m = m : concatMap universe (rnodeChildren m)
    lentLets = Set.fromList [i | m <- universe n, Just (i, _, _) <- [rnodeLet m], Set.member i lentIds]
    lentIds = Set.map snd (Map.keysSet (ppLentLets pp) <> Map.keysSet (ppCondLets pp))
    start = borrowed <> Map.keysSet owned <> lentLets <> condSourceRefs
    -- OWN-10: a part of a local a conditional's value may be lent from is
    -- read where it lies, as the local is read wherever that value is
    condSourceRefs =
      let own = within n
          here = Set.fromList [i | m <- own, Just (i, _, _) <- [rnodeLet m]]
          sources = Set.unions [chainUp (Set.singleton a) a | cl <- Map.elems (Map.filterWithKey (\(_, y) _ -> Set.member y here) (ppCondLets pp)), a <- Set.toList (clSources cl)]
       in Set.fromList
            [ i
            | not (Set.null sources)
            , m <- own
            , Just (i, RN rhs, _) <- [rnodeLet m]
            , Map.findWithDefault (0 :: Int) i ownCounts == 1
            , lendable caps rhs
            , isPart rhs
            , not (Set.null (Set.intersection sources (chainUp (Set.singleton i) i)))
            ]
    ownCounts = Map.fromListWith (+) [(i, 1) | m <- universe n, Just (i, _, _) <- [rnodeLet m]]
    chainUp seen i = case Map.lookup i bases of
      Just j | Set.notMember j seen -> chainUp (Set.insert j seen) j
      _ -> seen
    isPart e = case e of
      IntrinsicN _ IntrCtorField _ (s : _) -> varOrPart s
      IntrinsicN _ IntrListElem _ (s : _) -> varOrPart s
      PatN _ (PatGet sel [] (NativeArgExpr s)) -> not (selectorHasBracket sel) && length (ungroup sel) == 1 && varOrPart s
      _ -> False
    varOrPart s = case s of
      LetVarN {} -> True
      BndVarN {} -> True
      _ -> isPart s
    within top = top : concatMap go (rnodeChildren top)
      where
        go m = case m of
          RNM _ -> []
          RSM _ -> []
          _ -> m : concatMap go (rnodeChildren m)
    refs = settleRefs Set.empty
    settleRefs skip =
      let rs = start <> loopBorrowLocalsExcept (lendable caps) skip start n
          -- a take from a reference copies instead
          late = Set.filter (\d -> any (\t -> any (\j -> Set.notMember j rs && afterTake d j t) (chain d)) takes) (rs `Set.difference` start)
       in if Set.null late then rs else settleRefs (skip <> late)
    -- the values a local refers into, through the lets that refer into them
    chain d = go (Set.singleton d) (Map.lookup d bases)
      where
        go seen (Just j) | Set.notMember j seen = j : go (Set.insert j seen) (Map.lookup j bases)
        go _ _ = []
    bases = Map.fromList [(i, j) | m <- universe n, Just (i, RN rhs, _) <- [rnodeLet m], Just j <- [baseVar rhs]]

-- | Per-index variable-use counts over a manifold (OWN-2). Every read
-- counts, borrows included; the two arms of an 'IfN' count as their larger
-- arm, since one runs; a read in a closure body counts as two. A nested manifold counts as one use of each of its
-- context args, the call or capture that hands them over; its body reads are
-- its own scope's (OWN-3).
varUseCountOps :: FoldManifoldM Identity (Map.Map Int Int) (Map.Map Int Int) (Map.Map Int Int) (Map.Map Int Int) (Map.Map Int Int) (Map.Map Int Int)
varUseCountOps =
  FoldManifoldM
    { opSerialManifoldM = \(SerialManifold_ _ _ form _ _) -> return (ctxOf form)
    , opNativeManifoldM = \(NativeManifold_ _ _ form _) -> return (ctxOf form)
    , opSerialExprM = \node -> return (foldlSE add mempty node)
    , opNativeExprM = \node -> return $ case node of
        BndVarN_ _ i _ -> Map.singleton i 1
        LetVarN_ _ i _ -> Map.singleton i 1
        IfN_ _ c t f -> c `add` Map.unionWith max t f
        -- A suspension or @try body is a closure that may run more than
        -- once, so what it reads is never moved out of the manifold.
        DoBlockN_ _ inner -> Map.map (* 2) inner
        IntrinsicN_ _ IntrTry _ xs -> Map.map (* 2) (foldl add mempty xs)
        _ -> foldlNE add mempty node
    , opSerialArgM = \node -> return (foldlSA add mempty node)
    , opNativeArgM = \node -> return (foldlNA add mempty node)
    }
  where
    add = Map.unionWith (+)
    ctxOf form = Map.fromList [(i, 1) | Arg i _ <- manifoldContext form]

sharedOf :: Map.Map Int Int -> Set.Set Int
sharedOf = Map.keysSet . Map.filter (>= 2)

-- | The bound-variable indices that are borrowed parameters of a manifold:
-- its native parameters of a type that is not cheap to copy, and its
-- function parameters. Serial (dispatch) parameters are not.
borrowedIndicesOfForm :: (HasTypeM t) => OwnCaps -> ManifoldForm (Or TypeS TypeF) t -> Set.Set Int
borrowedIndicesOfForm caps form =
  Set.fromList [i | Arg i tm <- typeMofForm form, isBorrowed tm]
  where
    isBorrowed (Native tf) = not (ocCheap caps tf)
    isBorrowed (Function _ _) = True
    isBorrowed _ = False

-- | The loop-carried variable indices of a manifold body, or empty if the body
-- is not a native loop. 'addLoopWraps' makes the loop the whole body (possibly
-- under structural wrappers and the entry deserialize lets), so walk that spine.
-- The wrapper set must stay in sync with 'Serialize.loopCarriedTypes', which
-- walks the identical spine to resolve the carried slots' types.
loopCarriedIdsSM :: SerialManifold -> Set.Set Int
loopCarriedIdsSM (SerialManifold _ _ _ _ se) = go se
  where
    go (LoopS _ ids _) = Set.fromList ids
    go (ReturnS x) = go x
    go (SerialLetS _ _ x) = go x
    go (NativeLetS _ _ x) = go x
    go (CacheBodyS _ _ _ _ _ x) = go x
    go (DebugWrapS _ _ _ x) = go x
    go _ = Set.empty
