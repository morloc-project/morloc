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
import Control.Monad.Identity (Identity, runIdentity)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Morloc.CodeGenerator.Grammars.Translator.Imperative
  ( RNode (..)
  , rnodeChildren
  , rnodeLet
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
  , ppTwoForm :: Map.Map Int (Set.Set Int)
  -- ^ per fully applied native manifold, the positions of the parameters
  -- it keeps, or keeps a part of, on some evaluations but takes by
  -- reference: a member may give it a second form taking them by value,
  -- for a call that may move every one of them (OWN-7)
  }

emptyPoolPlan :: PoolPlan
emptyPoolPlan = PoolPlan {ppTakes = Set.empty, ppReleases = Set.empty, ppByValue = Map.empty, ppTwoForm = Map.empty}

planPool :: OwnCaps -> [SerialManifold] -> PoolPlan
planPool caps sms =
  PoolPlan
    { ppTakes = takes
    , ppReleases = releases
    , ppByValue = byValue
    , ppTwoForm = twoForm
    }
  where
    (takes, releases) = consumableProjectionLets (ocTagAfterTake caps) (ocSharedTakes caps) (\m -> Map.findWithDefault Set.empty m byValue <> Map.findWithDefault Set.empty m twoForm) sms
    twoForm = settleTwo Map.empty
    settleTwo known =
      let next = twoFormGiven known
       in if next == known then known else settleTwo next
    twoFormGiven known =
      Map.filter (not . Set.null) . Map.fromListWith Set.intersection $
        [ (m, positions)
        | NativeManifold m _ form@(ManifoldFull _) body <- natives
        , not (Set.member m asValues)
        , let args = typeMofForm form
              own = Map.findWithDefault Set.empty m byValue
              keeps c = Map.findWithDefault Set.empty c byValue <> Map.findWithDefault Set.empty c known
              kept = keptSomewhere keeps body
              sources = letSources (RN body)
              positions =
                Set.fromList
                  [ j
                  | (j, Arg i (Native tf)) <- indexed args
                  , not (ocCheap caps tf)
                  , not (isFunT tf)
                  , Set.notMember j own
                  , not (Set.null (Set.intersection kept (flowsFrom sources i)))
                  ]
        ]
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

-- | The variables a native expression's reads keep anywhere in it: an
-- element of a constructor, list, tuple or record; a value it or a let's
-- right-hand side ends in, on either arm of a conditional; a loop's start
-- or next value; and an argument
-- a call takes by value (@keeps m@, the positions manifold @m@ takes or may
-- take by value). Nested function values and suspensions are their own
-- scopes (OWN-3).
keptSomewhere :: (Int -> Set.Set Int) -> NativeExpr -> Set.Set Int
keptSomewhere keeps body = ends body <> go (RN body)
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
      RLN (LoopBase e) -> ends e <> foldMap go (rnodeChildren n)
      _ -> foldMap go (rnodeChildren n)
    here e = case e of
      VariantN _ _ _ xs -> vars xs
      ListN _ _ xs -> vars xs
      TupleN _ xs -> vars xs
      RecordN _ _ _ kvs -> vars (map snd kvs)
      AppExeN _ (RecCallP mid _) args -> Set.fromList [p | (j, NativeArgExpr a) <- indexed args, Just p <- [varOf a], Set.member j (keeps mid)]
      LoopN _ starts _ -> vars (map snd starts)
      NativeLetN _ rhs _ -> ends rhs
      _ -> Set.empty
    ends e = case e of
      NativeLetN _ _ b -> ends b
      IfN _ _ a b -> ends a <> ends b
      ReturnN x -> ends x
      CoerceN _ _ x -> ends x
      _ -> maybe Set.empty Set.singleton (varOf e)
    vars xs = Set.fromList [p | Just p <- map varOf xs]
    varOf (LetVarN _ p _) = Just p
    varOf (BndVarN _ p _) = Just p
    varOf _ = Nothing

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
-- projection out of a live value, or the projected payload taken (OWN-5).
data LetMode = LetCopy | LetBorrow | LetTake
  deriving (Eq, Show)

-- | The mode of let @i@ in top-level manifold @root@ with right-hand side
-- @rhs@ (OWN-5, OWN-6). A take is a payload projection the pool plan
-- takes; a borrow is any projection of a variable, through fields and list
-- elements.
letMode :: PoolPlan -> Int -> Int -> NativeExpr -> LetMode
letMode pp root i rhs
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
-- mapped to the variable at the base of its projection (OWN-2); the lets
-- that hold a value of their own; and the values that must never move. An
-- index bound more than once is never moved, and neither is anything one of
-- its bindings refers into.
data LetPlan = LetPlan
  { lpAliases :: Map.Map Int Int
  , lpOwned :: Set.Set Int
  , lpHeld :: Set.Set Int
  }

letPlan :: [(Int, NativeExpr, LetMode)] -> LetPlan
letPlan lets =
  LetPlan
    { lpAliases = Map.fromList [(i, r) | (i, rhs, LetBorrow) <- lets, not (Set.member i reused), Just r <- [baseVar rhs]]
    , lpOwned = Set.fromList [i | (i, _, mode) <- lets, mode /= LetBorrow, not (Set.member i reused)]
    , lpHeld = Set.fromList ([i | i <- Set.toList reused] <> [r | (i, rhs, LetBorrow) <- lets, Set.member i reused, Just r <- [baseVar rhs]])
    }
  where
    reused = Map.keysSet (Map.filter (> (1 :: Int)) (Map.fromListWith (+) [(i, 1) | (i, _, _) <- lets]))

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
    { mpShared = sharedOf (runIdentity (foldSerialExprM varUseCountOps e)) `Set.union` carried
    , mpCarried = carried
    }
  where
    carried = loopCarriedIdsSM sm

planNativeManifold :: OwnCaps -> PoolPlan -> NativeManifold -> ManifoldPlan
planNativeManifold caps pp nm@(NativeManifold m _ form e) =
  (borrowPlan caps pp borrowed (RNM nm))
    { mpShared = sharedOf (runIdentity (foldNativeExprM varUseCountOps e))
    }
  where
    borrowed = borrowedIndicesOfForm caps form `Set.difference` byValueIndices pp m form

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
    start = borrowed <> Map.keysSet owned
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
