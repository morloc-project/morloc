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
  , planPool
  , ManifoldPlan (..)
  , planSerialManifold
  , planNativeManifold
  , LetMode (..)
  , letMode
  ) where

import Control.Monad.Identity (Identity, runIdentity)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Morloc.CodeGenerator.Grammars.Translator.Imperative
  ( RNode (RNM, RSM)
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
    , ppByValue =
        Map.fromList
          [ (m, positions)
          | nm@(NativeManifold m _ form@(ManifoldFull _) _) <- concatMap (nativeManifoldsUnder . RSM) sms
          , let args = typeMofForm form
                cands = Set.fromList [i | Arg i (Native tf) <- args, not (ocCheap caps tf)]
                kept = consumedParams (not . ocCheap caps) cands nm
                positions = Set.fromList [j | (j, Arg i _) <- zip [0 ..] args, Set.member i kept]
          , not (Set.null positions)
          ]
    }

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
  | isBorrowableProjection rhs && Set.member (root, i) (ppTakes pp) = LetTake
  | isPlace rhs = LetBorrow
  | otherwise = LetCopy
  where
    isPlace e = case e of
      IntrinsicN _ IntrListElem _ (s : _) -> borrowableRoot s || isPlace s
      IntrinsicN _ IntrCtorField _ (s : _) -> borrowableRoot s || isPlace s
      AppExeN _ (PatCallP (PatternStruct sel)) [NativeArgExpr s] ->
        not (selectorHasBracket sel) && length (ungroup sel) == 1 && (borrowableRoot s || isPlace s)
      _ -> False

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
