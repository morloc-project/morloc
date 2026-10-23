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
  , loopBorrowLocals
  , nativeManifoldsUnder
  )
import Morloc.CodeGenerator.Namespace

-- | What a language's representation decides about ownership.
data OwnCaps = OwnCaps
  { ocCheap :: TypeF -> Bool
  -- ^ a type whose copy costs nothing
  , ocTagAfterTake :: Bool
  -- ^ whether a tag test may read a value after a field was taken from it
  , ocListKeeps :: Bool
  -- ^ whether a list literal's element is kept without a copy of its own
  }

-- | The decisions that span a pool's manifolds.
data PoolPlan = PoolPlan
  { ppTakes :: Set.Set (Int, Int)
  -- ^ (top-level manifold, let index) pairs whose payload projection takes
  -- the payload (OWN-5)
  , ppByValue :: Map.Map Int (Set.Set Int)
  -- ^ per fully applied native manifold, the parameter positions it takes by
  -- value (OWN-7)
  }

emptyPoolPlan :: PoolPlan
emptyPoolPlan = PoolPlan {ppTakes = Set.empty, ppByValue = Map.empty}

planPool :: OwnCaps -> [SerialManifold] -> PoolPlan
planPool caps sms =
  PoolPlan
    { ppTakes = consumableProjectionLets (ocTagAfterTake caps) sms
    , ppByValue = settle Map.empty
    }
  where
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
              kept = consumedParams (ocListKeeps caps) (\c -> Map.findWithDefault Set.empty c known) (not . ocCheap caps) cands nm
              positions = Set.fromList [j | (j, Arg i _) <- zip [0 ..] args, Set.member i kept]
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

-- | The parameter indices manifold @m@ takes by value.
byValueIndices :: PoolPlan -> Int -> ManifoldForm (Or TypeS TypeF) TypeF -> Set.Set Int
byValueIndices pp m form =
  Set.fromList [i | (j, Arg i _) <- zip [0 :: Int ..] (typeMofForm form), Set.member j positions]
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
  | isPlace rhs = LetBorrow
  | otherwise = LetCopy
  where
    isPlace e = case e of
      IntrinsicN _ IntrListElem _ (s : _) -> borrowableRoot s || isPlace s
      IntrinsicN _ IntrCtorField _ (s : _) -> borrowableRoot s || isPlace s
      AppExeN _ (PatCallP (PatternStruct sel)) [NativeArgExpr s] ->
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
    baseVar e = case e of
      LetVarN _ j _ -> Just j
      BndVarN _ j _ -> Just j
      IntrinsicN _ IntrCtorField _ (x : _) -> baseVar x
      IntrinsicN _ IntrListElem _ (x : _) -> baseVar x
      AppExeN _ (PatCallP (PatternStruct _)) [NativeArgExpr x] -> baseVar x
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

-- | The carried slots a continue reassigns, with their new values and, when
-- the IR continue is known, their IR values: every slot, less those whose IR
-- value is the slot itself. The carried slots, the values and the IR values
-- must match in number.
changedSlots :: [Int] -> Maybe [NativeExpr] -> [a] -> [(Int, a, Maybe NativeExpr)]
changedSlots ids origs vals
  | length ids /= length vals = error "morloc bug: a loop continue whose values do not match its carried slots"
  | otherwise = case origs of
      Just os
        | length os == length vals -> [(i, v, Just o) | (i, o, v) <- zip3 ids os vals, not (unchangedSlot i o)]
        | otherwise -> error "morloc bug: a loop continue whose lowered values do not match its IR values"
      Nothing -> [(i, v, Nothing) | (i, v) <- zip ids vals]

-- | The decisions within one manifold's body.
data ManifoldPlan = ManifoldPlan
  { mpBorrowed :: Set.Set Int
  -- ^ indices that hold a reference: borrowed parameters and the locals that
  -- borrow from them (OWN-6)
  , mpShared :: Set.Set Int
  -- ^ indices read at more than one point (OWN-2)
  , mpCarried :: Set.Set Int
  -- ^ the loop-carried indices of a native-loop manifold
  }

planSerialManifold :: OwnCaps -> SerialManifold -> ManifoldPlan
planSerialManifold caps sm@(SerialManifold _ _ form _ e) =
  ManifoldPlan
    { mpBorrowed = borrowed <> loopBorrowLocals (not . ocCheap caps) borrowed (RSM sm)
    , mpShared = sharedOf (runIdentity (foldSerialExprM varUseCountOps e)) `Set.union` carried
    , mpCarried = carried
    }
  where
    carried = loopCarriedIdsSM sm
    borrowed = borrowedIndicesOfForm caps form

planNativeManifold :: OwnCaps -> PoolPlan -> NativeManifold -> ManifoldPlan
planNativeManifold caps pp nm@(NativeManifold m _ form e) =
  ManifoldPlan
    { mpBorrowed = borrowed <> loopBorrowLocals (not . ocCheap caps) borrowed (RNM nm)
    , mpShared = sharedOf (runIdentity (foldNativeExprM varUseCountOps e))
    , mpCarried = Set.empty
    }
  where
    borrowed = borrowedIndicesOfForm caps form `Set.difference` byValueIndices pp m form

-- | Per-index variable-use counts over a manifold (OWN-2). Every read
-- counts, borrows included; the two arms of an 'IfN' count as their larger
-- arm, since one runs. A nested manifold counts as one use of each of its
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
