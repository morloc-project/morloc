{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.Typecheck.Internal
Description : Shared typechecking machinery (unification, substitution, context)
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Exports typechecking primitives shared between the frontend general
typechecker ('Morloc.Frontend.Typecheck') and any backend-specific
typecheckers: unification, context operations, substitution, fresh
variable generation, and type quantification\/unquantification.
-}
module Morloc.Typecheck.Internal
  ( (+>)
  , (++>)

    -- * accessing state
  , newvar
  , tvarname
  , newvarRich
  , evarname
  , qualify
  , unqualify

    -- * Typeclasses
  , Applicable (..)
  , GammaIndexLike (..)

    -- * manipulating context
  , access1
  , access2
  , solveExist
  , solveExistWith
  , lookupU
  , lookupE
  , cacheSolved
  , cut
  , substitute
  , rename
  , renameEType
  , cleanTypeName
  , prettyTypeU
  , isNatExpr
  , isStrExpr
  , isRecExpr
  , isListExpr
  , isSetExpr
  , prettyConstraint
  , occursCheck
  , toExistential
  , gammaContextList
  , gammaTrimAfter

    -- * selectors
  , selectorType
  , selectorGetter
  , selectorSetter

    -- * subtyping
  , subtype
  , isSubtypeOf2
  , recheckDeferred

    -- * primitive constraint discharge (Stage 9 of the tables refactor)
  , reduceConstraint
  , dischargeConstraints
  , normaliseSet

    -- * nat label helpers
  , collectNatVarNames
  , collectStrVarNames
  , collectRecVarNames
  , collectListVarNames

    -- * debugging
  , seeGamma
  -- debugging
  , enter
  , insetSay
  , leave
  , peak
  , peakGen
  , seeType
  ) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Morloc.BaseTypes as BT
import Morloc.Data.Doc
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import Morloc.Namespace.Expr
import Morloc.Namespace.Prim
import Morloc.Namespace.State
import Morloc.Namespace.Type
import qualified Morloc.Typecheck.NatSolver as NS
import qualified Morloc.Typecheck.StrSolver as SS
import qualified Morloc.Typecheck.RecSolver as RS
import qualified Morloc.Typecheck.ListSolver as LS
import qualified Morloc.Typecheck.SetSolver as SetS
import qualified Morloc.TypeEval as TE

qualify :: [TVar] -> TypeU -> TypeU
qualify vs t = foldr (\v -> ForallU v) t vs

unqualify :: TypeU -> ([TVar], TypeU)
unqualify (ForallU v (unqualify -> (vs, t))) = (v : vs, t)
unqualify t = ([], t)

toExistential :: Gamma -> TypeU -> (Gamma, TypeU)
toExistential g0 (unqualify -> (vs0, t0)) = f g0 vs0 t0
  where
    f g [] t = (g, t)
    f g (v : vs) t =
      let (g', newVar) = newvar ("cls_" <> unTVar v) g
       in f g' vs (substituteTVar v newVar t)

class Applicable a where
  apply :: Gamma -> a -> a

----------------------------------------------------------------------
-- Cross-kind function reduction (Stage 9)
----------------------------------------------------------------------

-- | Walk a Rec extension chain and collect (key, type) fields. Returns
-- Nothing if the chain doesn't terminate cleanly in RecEmptyU (free
-- row variable, unsolved Rec operator).
collectGroundRec :: TypeU -> Maybe [(Text, TypeU)]
collectGroundRec RecEmptyU = Just []
collectGroundRec (RecExtendU k t rest) = ((k, t):) <$> collectGroundRec rest
collectGroundRec _ = Nothing

-- | Reduce @Keys r@ when @r@ is a ground Rec. Returns a SetLitU of the
-- key names; otherwise leaves the form symbolic for solver-time deferral.
reduceKeys :: TypeU -> TypeU
reduceKeys r = case collectGroundRec r of
  Just fs -> SetLitU [StrLitU k | (k, _) <- fs]
  Nothing -> KeysU r

-- | Reduce @ListToSet l@ when @l@ is a ground list literal.
reduceListToSet :: TypeU -> TypeU
reduceListToSet (ListLitU es) = SetLitU (dedupSorted es)
  where
    dedupSorted = go . map normalise
    normalise t = t
    go [] = []
    go (x:xs) = x : go (filter (/= x) xs)
reduceListToSet l = ListToSetU l

-- | Reduce @Size c@ when @c@ is ground.
reduceSize :: TypeU -> TypeU
reduceSize (ListLitU es) = NatLitU (toInteger (length es))
reduceSize (SetLitU es) = NatLitU (toInteger (length es))
reduceSize SetEmptyU = NatLitU 0
reduceSize r@(RecExtendU _ _ _) = case collectGroundRec r of
  Just fs -> NatLitU (toInteger (length fs))
  Nothing -> SizeU r
reduceSize RecEmptyU = NatLitU 0
reduceSize c = SizeU c

-- | Reduce @ProjectField r f@ when both are ground.
reduceProjectField :: TypeU -> TypeU -> TypeU
reduceProjectField r (StrLitU name) = case collectGroundRec r of
  Just fs -> case lookup name fs of
    Just t -> t
    Nothing -> ProjectFieldU r (StrLitU name)
  Nothing -> ProjectFieldU r (StrLitU name)
reduceProjectField r f = ProjectFieldU r f

-- | Reduce @Singleton k v@ when the key is a Str literal: a one-field
-- Rec extension of the empty Rec. Defers when the key is still a
-- variable.
reduceRecSingleton :: TypeU -> TypeU -> TypeU
reduceRecSingleton (StrLitU k) v = RecExtendU k v RecEmptyU
reduceRecSingleton k v = RecSingletonU k v

-- | Extract a list of ground Str literals from a TypeU, if possible.
-- Returns Just on a fully-ground @ListLitU@ of @StrLitU@ values, Nothing
-- otherwise.
collectGroundStrList :: TypeU -> Maybe [Text]
collectGroundStrList (ListLitU es) = traverse asStrLit es
  where
    asStrLit (StrLitU s) = Just s
    asStrLit _ = Nothing
collectGroundStrList _ = Nothing

-- | Reduce @r # l@ to a Rec containing only the fields of @r@ whose
-- names appear in @l@. Both operands must be ground; otherwise leaves
-- the form symbolic. Order of result fields matches the original Rec.
reduceRecRestrict :: TypeU -> TypeU -> TypeU
reduceRecRestrict r l = case (collectGroundRec r, collectGroundStrList l) of
  (Just fs, Just keys) ->
    let kept = [(k, t) | (k, t) <- fs, k `elem` keys]
    in foldr (\(k, t) rest -> RecExtendU k t rest) RecEmptyU kept
  _ -> RecRestrictU r l

-- | Reduce @r - l@ to a Rec containing every field of @r@ whose name is
-- not in @l@. Drop-of-absent is benign (no constraint required).
reduceRecDiffList :: TypeU -> TypeU -> TypeU
reduceRecDiffList r l = case (collectGroundRec r, collectGroundStrList l) of
  (Just fs, Just keys) ->
    let kept = [(k, t) | (k, t) <- fs, k `notElem` keys]
    in foldr (\(k, t) rest -> RecExtendU k t rest) RecEmptyU kept
  _ -> RecDiffListU r l

-- | Reduce @r1 + r2@ when both are ground Recs to a single literal Rec.
-- Conflicting keys (same name on both sides with different types) are
-- preserved as-is in the symbolic form so the existing RecSolver
-- contradiction path produces a clear error. When one side is symbolic
-- (a row variable, etc.) the form stays @RecUnionU@.
reduceRecUnion :: TypeU -> TypeU -> TypeU
reduceRecUnion a b = case (collectGroundRec a, collectGroundRec b) of
  (Just fa, Just fb)
    | not anyOverlap ->
        foldr (\(k, t) rest -> RecExtendU k t rest) RecEmptyU (fa ++ fb)
    where
      keysA = map fst fa
      keysB = map fst fb
      anyOverlap = any (`elem` keysB) keysA
  _ -> RecUnionU a b

----------------------------------------------------------------------
-- Generic constraint discharge (Stage 9)
----------------------------------------------------------------------

-- | Reduce a primitive constraint to one of three outcomes:
--
-- - @Right Nothing@: discharged (constraint is satisfied).
-- - @Right (Just c)@: still defers; @c@ is the reduced form to keep
--   in the gamma queue. The reduced form is at least as
--   informative as the input.
-- - @Left msg@: contradicts; constraint cannot be satisfied.
--
-- Typeclass constraints ('Constraint cls ts') aren't handled here —
-- they pass through unchanged and are left for the typeclass-discharge
-- machinery elsewhere.
reduceConstraint :: Constraint -> Either Text (Maybe Constraint)
reduceConstraint c@(Constraint _ _) = Right (Just c)
reduceConstraint (CMember a s) = case (a, normaliseSet s) of
  (_, SetEmptyU) -> Left "Member: element required in empty set"
  (StrLitU x, SetLitU es)
    | StrLitU x `elem` es -> Right Nothing
    | all isStrLit es -> Left ("Member: '" <> x <> "' not in literal set")
    | otherwise -> Right (Just (CMember a (SetLitU es)))
  (_, s') -> Right (Just (CMember a s'))
  where
    isStrLit (StrLitU _) = True
    isStrLit _ = False
reduceConstraint (CSubset a b) = case (normaliseSet a, normaliseSet b) of
  (SetEmptyU, _) -> Right Nothing
  (SetLitU [], _) -> Right Nothing
  (SetLitU es, SetLitU bs)
    | all (`elem` bs) es -> Right Nothing
    | all isStrLit es && all isStrLit bs ->
        let missing = [e | e <- es, e `notElem` bs]
        in Left ("Subset: literal set missing " <> showStrLits missing)
    | otherwise -> Right (Just (CSubset (SetLitU es) (SetLitU bs)))
  (a', b') -> Right (Just (CSubset a' b'))
  where
    isStrLit (StrLitU _) = True
    isStrLit _ = False
    showStrLits xs = MT.intercalate ", " ["'" <> n <> "'" | StrLitU n <- xs]
reduceConstraint (CDisjoint a b) = case (normaliseSet a, normaliseSet b) of
  (SetEmptyU, _) -> Right Nothing
  (_, SetEmptyU) -> Right Nothing
  (SetLitU [], _) -> Right Nothing
  (_, SetLitU []) -> Right Nothing
  (SetLitU as, SetLitU bs)
    | null overlap -> Right Nothing
    | all isStrLit as && all isStrLit bs ->
        Left ("Disjoint: shared element(s) " <> showStrLits overlap)
    | otherwise -> Right (Just (CDisjoint (SetLitU as) (SetLitU bs)))
    where
      overlap = [x | x <- as, x `elem` bs]
  (a', b') -> Right (Just (CDisjoint a' b'))
  where
    isStrLit (StrLitU _) = True
    isStrLit _ = False
    showStrLits xs = MT.intercalate ", " ["'" <> n <> "'" | StrLitU n <- xs]

-- | Aggressive ground-form normalisation for set expressions only.
-- Non-set inputs return unchanged. Used by constraint reduction to
-- canonicalise both sides before structural checks.
normaliseSet :: TypeU -> TypeU
normaliseSet SetEmptyU = SetEmptyU
normaliseSet (SetLitU es) = SetLitU (dedupOrdered es)
  where
    dedupOrdered = go []
    go acc [] = reverse acc
    go acc (x:xs) = if x `elem` acc then go acc xs else go (x:acc) xs
normaliseSet (SetUnionU a b) = case (normaliseSet a, normaliseSet b) of
  (SetEmptyU, x) -> x
  (x, SetEmptyU) -> x
  (SetLitU as, SetLitU bs) ->
    let combined = as ++ [x | x <- bs, x `notElem` as]
    in SetLitU combined
  (a', b') -> SetUnionU a' b'
normaliseSet (SetInterU a b) = case (normaliseSet a, normaliseSet b) of
  (SetEmptyU, _) -> SetEmptyU
  (_, SetEmptyU) -> SetEmptyU
  (SetLitU as, SetLitU bs) -> SetLitU [x | x <- as, x `elem` bs]
  (a', b') -> SetInterU a' b'
normaliseSet (SetDiffU a b) = case (normaliseSet a, normaliseSet b) of
  (SetEmptyU, _) -> SetEmptyU
  (x, SetEmptyU) -> x
  (SetLitU as, SetLitU bs) -> SetLitU [x | x <- as, x `notElem` bs]
  (a', b') -> SetDiffU a' b'
normaliseSet t = t

-- | Walk gammaConstraints, discharging each obligation against the final
-- substitution. The algorithm has three outcomes per constraint:
--
--   1. @apply g c@ then @reduceConstraint@ returns 'Right Nothing'
--      (fully decided as true): drop it.
--   2. Returns 'Left msg' (contradiction): the whole pass fails with
--      the first contradiction.
--   3. Returns 'Right (Just c')' (still deferred — typically because a
--      row variable, list variable, etc., is not yet ground): the
--      constraint is checked against @gammaAssumedConstraints@. If
--      @c'@ is identical (after applying gamma) to one of the
--      assumptions, the obligation is "subsumed" by what the current
--      function's signature already declared, and we drop it.
--      Otherwise we either keep it for a later pass (if the algorithm
--      is being run mid-typecheck) or report it as an unsolved-
--      constraint error (if we are at end-of-typecheck).
--
-- Constraint propagation algorithm. A function body is allowed to use
-- any constraint that the function's signature has declared. When the
-- body invokes another function whose signature carries an obligation,
-- the obligation is renamed to fresh per-call names and queued. After
-- the body's gamma has solved everything that *can* be solved at this
-- function's level, a leftover obligation is one of two things:
--
--   * Equivalent (after apply) to a declared assumption — meaning the
--     caller has already promised this. We discharge it: the obligation
--     is the caller's promise.
--   * Not equivalent to anything — meaning the body needs more than the
--     signature declares. That is a real error: the user must add the
--     missing constraint to the function's @=>@ clause (or change the
--     body so the constraint is no longer needed).
--
-- This is essentially the same shape as Haskell's "missing class
-- constraint" diagnostic, restricted to morloc's primitive Member /
-- Subset / Disjoint forms. Equivalence is structural after gamma
-- application — two constraints are subsumed when their applied forms
-- are exactly @==@. We do not attempt logical implication (e.g.,
-- @CSubset a b /\ CSubset b c => CSubset a c@); subsumption is the
-- conservative-but-decidable approximation.
dischargeConstraints :: Gamma -> Either MDoc Gamma
dischargeConstraints g = case go (gammaConstraints g) [] of
  Left msg -> Left (pretty msg)
  Right kept -> Right (g { gammaConstraints = kept })
  where
    -- | An obligation is subsumed by an assumption when the two are
    -- structurally equal after applying the same gamma to both. The
    -- assumption already had gamma applied (via @apply g a@), so the
    -- obligation must be applied symmetrically before comparison.
    -- @Nothing@ in the assumption slot (top-level had no signature)
    -- means there are no assumptions to subsume against.
    assumed :: [Constraint]
    assumed = case gammaAssumedConstraints g of
      Just cs -> map (apply g) cs
      Nothing -> []

    -- | A constraint @c@ is subsumed iff @c == a@ for some assumption
    -- @a@. Direct equality is sufficient because both sides have
    -- already been ground out by gamma application.
    subsumed :: Constraint -> Bool
    subsumed c = c `elem` assumed

    go [] acc = Right (reverse acc)
    go (c:cs) acc = case reduceConstraint (apply g c) of
      Left msg -> Left msg
      Right Nothing -> go cs acc
      Right (Just c')
        | subsumed c' -> go cs acc
        | otherwise   -> go cs (c':acc)

----------------------------------------------------------------------

-- | Apply a context to a type (See Dunfield Figure 8).
instance Applicable TypeU where
  -- [G]a = a
  apply g (VarU v) =
    -- FIXME: very wrong - only works because of my renaming scheme
    case lookupU v g of
      (Just t') -> t'
      Nothing -> VarU v
  apply g (NatVarU v) = case Map.lookup v (gammaNatSubs g) of
    Just t -> t
    Nothing -> NatVarU v
  -- [G](A->B) = ([G]A -> [G]B)
  apply g (FunU ts t) = FunU (map (apply g) ts) (apply g t)
  apply g (AppU t ts) = AppU (apply g t) (map (apply g) ts)
  -- [G]ForallU a.a = forall a. [G]a
  apply g (ForallU v a) =
    -- FIXME: VERY WRONG
    case lookupU v g of
      (Just _) -> apply g a
      Nothing -> ForallU v (apply g a)
  -- [G[a=t]]a = [G[a=t]]t
  apply g (ExistU v (ts, tc) (rs, rc)) =
    case lookupU v g of
      -- FIXME: this seems problematic - do I keep the previous parameters or the new ones?
      (Just t') -> apply g t' -- reduce an existential; strictly smaller term
      Nothing -> ExistU v (map (apply g) ts, tc) (map (second (apply g)) rs, rc)
  apply g (NamU o n ps rs) = NamU o n ps [(k, apply g t) | (k, t) <- rs]
  apply g (EffectU effs t) = EffectU effs (apply g t)
  apply g (OptionalU t) = OptionalU (apply g t)
  apply _ t@(NatLitU _) = t
  apply g (NatAddU a b) = NatAddU (apply g a) (apply g b)
  apply g (NatMulU a b) = NatMulU (apply g a) (apply g b)
  apply g (NatSubU a b) = NatSubU (apply g a) (apply g b)
  apply g (NatDivU a b) = NatDivU (apply g a) (apply g b)
  apply _ t@NatVoidU = t
  apply g (StrVarU v) = case Map.lookup v (gammaStrSubs g) of
    Just t -> t
    Nothing -> StrVarU v
  apply _ t@(StrLitU _) = t
  apply g (StrConcatU a b) = StrConcatU (apply g a) (apply g b)
  apply _ t@StrVoidU = t
  apply g (RecVarU v) = case Map.lookup v (gammaRecSubs g) of
    Just t -> t
    Nothing -> RecVarU v
  apply _ t@RecEmptyU = t
  apply g (RecExtendU k a b) = RecExtendU k (apply g a) (apply g b)
  apply g (RecUnionU a b) = reduceRecUnion (apply g a) (apply g b)
  apply g (RecDiffU a ks) = RecDiffU (apply g a) ks
  apply g (RecIntersectU a b) = RecIntersectU (apply g a) (apply g b)
  apply g (RecRestrictU a b) = reduceRecRestrict (apply g a) (apply g b)
  apply g (RecDiffListU a b) = reduceRecDiffList (apply g a) (apply g b)
  apply _ t@RecVoidU = t
  -- List- and Set-kinded constructs: variables look up gammaListSubs /
  -- gammaSetSubs (parallel to RecVarU); operators recurse into operands.
  apply g (ListVarU v) = case Map.lookup v (gammaListSubs g) of
    Just t -> t
    Nothing -> ListVarU v
  apply g (ListLitU es) = ListLitU (map (apply g) es)
  apply g (ListAppU a b) = ListAppU (apply g a) (apply g b)
  apply _ t@ListVoidU = t
  apply g (SetVarU v) = case Map.lookup v (gammaSetSubs g) of
    Just t -> t
    Nothing -> SetVarU v
  apply _ t@SetEmptyU = t
  apply g (SetLitU es) = SetLitU (map (apply g) es)
  apply g (SetUnionU a b) = SetUnionU (apply g a) (apply g b)
  apply g (SetInterU a b) = SetInterU (apply g a) (apply g b)
  apply g (SetDiffU a b) = SetDiffU (apply g a) (apply g b)
  apply _ t@SetVoidU = t
  -- Cross-kind functions: recurse into operands first, then attempt
  -- ground-form reduction. When the operand is fully ground these
  -- reduce to a concrete value of the result kind; otherwise they
  -- stay symbolic and propagate.
  apply g (KeysU r) = reduceKeys (apply g r)
  apply g (ListToSetU l) = reduceListToSet (apply g l)
  apply g (SizeU c) = reduceSize (apply g c)
  apply g (ProjectFieldU r f) = reduceProjectField (apply g r) (apply g f)
  apply g (RecSingletonU k v) = reduceRecSingleton (apply g k) (apply g v)
  apply g (LabeledU n t) = LabeledU n (apply g t)

instance Applicable EType where
  apply g e =
    e
      { etype = apply g (etype e)
      , econs = Set.map (applyConstraint g) (econs e)
      }
    where
      applyConstraint g' (Constraint cls ts) = Constraint cls (map (apply g') ts)
      applyConstraint g' (CMember a s) = CMember (apply g' a) (apply g' s)
      applyConstraint g' (CSubset a b) = CSubset (apply g' a) (apply g' b)
      applyConstraint g' (CDisjoint a b) = CDisjoint (apply g' a) (apply g' b)

instance Applicable Constraint where
  apply g (Constraint cls ts) = Constraint cls (map (apply g) ts)
  apply g (CMember a s) = CMember (apply g a) (apply g s)
  apply g (CSubset a b) = CSubset (apply g a) (apply g b)
  apply g (CDisjoint a b) = CDisjoint (apply g a) (apply g b)

instance Applicable Gamma where
  apply g1 g2 =
    g2
      { gammaContext = IntMap.map f (gammaContext g2)
      , gammaSolved = Map.map (apply g1) (gammaSolved g2)
      , gammaNatSubs = Map.map (apply g1) (gammaNatSubs g2)
      , gammaStrSubs = Map.map (apply g1) (gammaStrSubs g2)
      , gammaRecSubs = Map.map (apply g1) (gammaRecSubs g2)
      , gammaListSubs = Map.map (apply g1) (gammaListSubs g2)
      , gammaSetSubs = Map.map (apply g1) (gammaSetSubs g2)
      , gammaConstraints = map (apply g1) (gammaConstraints g2)
      }
    where
      f :: GammaIndex -> GammaIndex
      f (AnnG v t) = AnnG v (apply g1 t)
      f (ExistG v (ps, pc) (rs, rc)) = ExistG v (map (apply g1) ps, pc) (map (second (apply g1)) rs, rc)
      f (SolvedG v t) = SolvedG v (apply g1 t)
      f x = x

class GammaIndexLike a where
  index :: a -> GammaIndex

instance GammaIndexLike GammaIndex where
  index = id

instance GammaIndexLike TypeU where
  index (ExistU t (ts, tc) (rs, rc)) = ExistG t (ts, tc) (rs, rc)
  index t = error $ "Can only index ExistT, found: " <> show t

instance GammaIndexLike TVar where
  index v = ExistG v ([], Open) ([], Open)

-- | Slot spacing between consecutive entries added by (+>).
-- Leaves room for solveExistWith to insert entries in between.
slotSpacing :: Int
slotSpacing = 256

-- | Prepend an entry to the context (newest position).
(+>) :: (GammaIndexLike a) => Gamma -> a -> Gamma
(+>) g x =
  let gi = index x
      s = gammaSlot g
  in g { gammaSlot = s + slotSpacing
       , gammaContext = IntMap.insert s gi (gammaContext g)
       , gammaExist = case gi of
           ExistG v _ _ -> Map.insert v s (gammaExist g)
           _ -> gammaExist g
       }

-- | Add multiple entries: last element of list becomes newest (highest slot).
(++>) :: (GammaIndexLike a) => Gamma -> [a] -> Gamma
(++>) g xs = foldl' (+>) g xs

isSubtypeOf2 :: Scope -> TypeU -> TypeU -> Bool
isSubtypeOf2 scope a b = case subtype scope a b (Gamma 0 0 IntMap.empty Map.empty Map.empty [] Map.empty Map.empty Map.empty Map.empty Map.empty [] Nothing Map.empty) of
  (Left _) -> False
  (Right _) -> True

subtypeEvaluated :: Scope -> TypeU -> TypeU -> Gamma -> Either MDoc Gamma
subtypeEvaluated scope t1 t2 g
  -- Reject sibling aliases before reduction. Without this, Array Int <: Deque Int
  -- would succeed transitively (Array Int -> List Int -> Deque Int) even though
  -- they are on different branches of the alias tree.
  | areSiblingAliases scope t1 t2 =
      Left $ "Cannot compare sibling types" <+> pretty t1 <+> "and" <+> pretty t2
  | otherwise = case (TE.reduceType scope t1, TE.reduceType scope t2) of
    (Just t1', _) -> subtype scope t1' t2 g
    (_, Just t2') -> subtype scope t1 t2' g
    (_, _)
      -- When both are bare type constructors that can't be reduced (e.g.,
      -- List vs Deque where Deque a = List a), check if one is an ancestor
      -- of the other by evaluating and comparing heads.
      | aliasEquivConstructors scope t1 t2 -> Right g
      | otherwise -> Left $ "Cannot compare types" <+> pretty t1 <+> "and" <+> pretty t2

-- | Check whether two applied types are sibling aliases -- both reduce to
-- the same ancestor but neither reduces to the other. For example,
-- Array Int and Deque Int are siblings (both reduce to List Int, but
-- Array does not reduce to Deque nor vice versa).
areSiblingAliases :: Scope -> TypeU -> TypeU -> Bool
areSiblingAliases scope (AppU (VarU v1) _) (AppU (VarU v2) _)
  | v1 == v2 = False
  | otherwise =
    let h1 = evalHead v1
        h2 = evalHead v2
    in case (h1, h2) of
         -- Both reduce to the same ancestor, but neither is the other's ancestor
         (Just hv1, Just hv2) -> hv1 == hv2 && hv1 /= v1 && hv2 /= v2
         _ -> False
  where
    evalHead v = case Map.lookup v scope of
      Just ((ps, _, _, _) : _)
        | all isGenericParam ps && not (null ps) ->
          let n = length ps
              freshVars = [VarU (TV (MT.show' i <> "__sib_cmp")) | i <- [0 .. n - 1]]
              app = AppU (VarU v) freshVars
          in case TE.evaluateType scope app of
               Right (AppU (VarU headV) _) -> Just headV
               _ -> Nothing
        | otherwise -> Nothing
      _ -> Nothing
    isGenericParam (Left _) = True
    isGenericParam _ = False
areSiblingAliases _ _ _ = False

-- | Check whether two unapplied type constructors are on the same path in
-- the alias hierarchy -- i.e., one reduces to the other. Applied aliases
-- (like Deque Int) are handled by reduceType above; this covers the bare
-- constructor case (Deque vs List) which arises when an existential is solved
-- to one name and then compared against an ancestor or descendant alias.
--
-- Only ancestor-descendant pairs match: List<->Deque and List<->Array succeed,
-- but Array<->Deque fails (siblings with a common ancestor but neither
-- reduces to the other).
aliasEquivConstructors :: Scope -> TypeU -> TypeU -> Bool
aliasEquivConstructors scope (VarU v1) (VarU v2) =
  reducesToHead v1 v2 || reducesToHead v2 v1
  where
    reducesToHead src target =
      case arityOf (Map.lookup src scope) of
        Just n | n > 0 ->
          let freshVars = [VarU (TV (MT.show' i <> "__alias_cmp")) | i <- [0 .. n - 1]]
              app = AppU (VarU src) freshVars
          in case TE.evaluateType scope app of
               Right (AppU (VarU headV) _) -> headV == target
               _ -> False
        -- base type with no alias: matches only itself
        _ -> False

    arityOf :: Maybe [([Either (TVar, Kind) TypeU], TypeU, ArgDoc, Bool)] -> Maybe Int
    arityOf Nothing = Nothing
    arityOf (Just []) = Nothing
    arityOf (Just ((ps, _, _, _) : _))
      | all isGenericParam ps = Just (length ps)
      | otherwise = Nothing

    isGenericParam (Left _) = True
    isGenericParam _ = False
aliasEquivConstructors _ _ _ = False

subtypeError :: TypeU -> TypeU -> MDoc -> Either MDoc a
subtypeError t1 t2 msg =
  Left $
    "Subtype error:" <+> msg
      <> "\n  "
      <> prettyTypeU t1 <+> "<:" <+> prettyTypeU t2

-- Nat expression helpers for SOP-based comparison
isNatExpr :: TypeU -> Bool
isNatExpr (NatVarU _) = True
isNatExpr (NatLitU _) = True
isNatExpr (NatAddU _ _) = True
isNatExpr (NatMulU _ _) = True
isNatExpr (NatSubU _ _) = True
isNatExpr (NatDivU _ _) = True
isNatExpr NatVoidU = True
isNatExpr _ = False

typeUToNatExpr :: TypeU -> Maybe NS.NatExpr
typeUToNatExpr (NatVarU v) = Just (NS.NatVar v)
typeUToNatExpr (NatLitU n) = Just (NS.NatLit n)
typeUToNatExpr (NatAddU a b) = NS.NatAdd <$> typeUToNatExpr a <*> typeUToNatExpr b
typeUToNatExpr (NatMulU a b) = NS.NatMul <$> typeUToNatExpr a <*> typeUToNatExpr b
typeUToNatExpr (NatSubU a b) = NS.NatSub <$> typeUToNatExpr a <*> typeUToNatExpr b
typeUToNatExpr (NatDivU a b) = NS.NatDiv <$> typeUToNatExpr a <*> typeUToNatExpr b
typeUToNatExpr (VarU v) = Just (NS.NatVar v)
typeUToNatExpr (ExistU v _ _) = Just (NS.NatVar v)
typeUToNatExpr _ = Nothing

natExprToTypeU :: NS.NatExpr -> TypeU
natExprToTypeU (NS.NatLit n) = NatLitU n
natExprToTypeU (NS.NatVar v) = NatVarU v
natExprToTypeU (NS.NatAdd a b) = NatAddU (natExprToTypeU a) (natExprToTypeU b)
natExprToTypeU (NS.NatMul a b) = NatMulU (natExprToTypeU a) (natExprToTypeU b)
natExprToTypeU (NS.NatSub a b) = NatSubU (natExprToTypeU a) (natExprToTypeU b)
natExprToTypeU (NS.NatDiv a b) = NatDivU (natExprToTypeU a) (natExprToTypeU b)

-- | True iff the TypeU is a Str-kinded expression (literal, variable, or
-- concat of those). Used to gate which deferred constraints are sent to the
-- StrSolver. See plans/tables/04-str-solver-scope.md.
isStrExpr :: TypeU -> Bool
isStrExpr (StrVarU _) = True
isStrExpr (StrLitU _) = True
isStrExpr (StrConcatU _ _) = True
isStrExpr StrVoidU = True
isStrExpr _ = False

typeUToStrExpr :: TypeU -> Maybe SS.StrExpr
typeUToStrExpr (StrVarU v) = Just (SS.StrVar v)
typeUToStrExpr (StrLitU s) = Just (SS.StrLit s)
typeUToStrExpr (StrConcatU a b) = SS.StrConcat <$> typeUToStrExpr a <*> typeUToStrExpr b
typeUToStrExpr _ = Nothing

strExprToTypeU :: SS.StrExpr -> TypeU
strExprToTypeU (SS.StrLit s) = StrLitU s
strExprToTypeU (SS.StrVar v) = StrVarU v
strExprToTypeU (SS.StrConcat a b) = StrConcatU (strExprToTypeU a) (strExprToTypeU b)

-- | Insert solved Str-variable assignments into the gamma's gammaStrSubs.
-- Mirrors applyNatSolutions but the StrSolver only ever produces ground
-- assignments (per memo 04), so there is no existential-vs-StrVar dispatch.
applyStrSolutions :: Map.Map TVar SS.StrExpr -> Gamma -> Gamma
applyStrSolutions subs g0 = foldl insertSub g0 (Map.toList subs)
  where
    insertSub g (v, se) = g { gammaStrSubs = Map.insert v (strExprToTypeU se) (gammaStrSubs g) }

-- | True iff the TypeU is a Rec-kinded expression (variable, empty,
-- extension, union, difference, intersection). Mirrors isNatExpr/isStrExpr.
isRecExpr :: TypeU -> Bool
isRecExpr (RecVarU _) = True
isRecExpr RecEmptyU = True
isRecExpr (RecExtendU _ _ _) = True
isRecExpr (RecUnionU _ _) = True
isRecExpr (RecDiffU _ _) = True
isRecExpr (RecIntersectU _ _) = True
isRecExpr (RecRestrictU _ _) = True
isRecExpr (RecDiffListU _ _) = True
isRecExpr (RecSingletonU _ _) = True
isRecExpr RecVoidU = True
isRecExpr _ = False

typeUToRecExpr :: TypeU -> Maybe RS.RecExpr
typeUToRecExpr (RecVarU v) = Just (RS.RecVar v)
typeUToRecExpr RecEmptyU = Just RS.RecEmpty
typeUToRecExpr (RecExtendU k t rest) = RS.RecExtend k t <$> typeUToRecExpr rest
typeUToRecExpr (RecUnionU a b) = RS.RecUnion <$> typeUToRecExpr a <*> typeUToRecExpr b
typeUToRecExpr (RecDiffU a ks) = (\a' -> RS.RecDiff a' ks) <$> typeUToRecExpr a
typeUToRecExpr (RecIntersectU a b) = RS.RecIntersect <$> typeUToRecExpr a <*> typeUToRecExpr b
typeUToRecExpr _ = Nothing

recExprToTypeU :: RS.RecExpr -> TypeU
recExprToTypeU (RS.RecVar v) = RecVarU v
recExprToTypeU RS.RecEmpty = RecEmptyU
recExprToTypeU (RS.RecExtend k t rest) = RecExtendU k t (recExprToTypeU rest)
recExprToTypeU (RS.RecUnion a b) = RecUnionU (recExprToTypeU a) (recExprToTypeU b)
recExprToTypeU (RS.RecDiff a ks) = RecDiffU (recExprToTypeU a) ks
recExprToTypeU (RS.RecIntersect a b) = RecIntersectU (recExprToTypeU a) (recExprToTypeU b)

-- | Insert solved Rec-tail variable assignments into the gamma's gammaRecSubs.
applyRecSolutions :: Map.Map TVar RS.RecExpr -> Gamma -> Gamma
applyRecSolutions subs g0 = foldl insertSub g0 (Map.toList subs)
  where
    insertSub g (v, re) = g { gammaRecSubs = Map.insert v (recExprToTypeU re) (gammaRecSubs g) }

-- | True iff the TypeU is a List-kinded expression. Mirrors isRecExpr.
isListExpr :: TypeU -> Bool
isListExpr (ListVarU _) = True
isListExpr (ListLitU _) = True
isListExpr (ListAppU _ _) = True
isListExpr ListVoidU = True
isListExpr _ = False

typeUToListExpr :: TypeU -> Maybe LS.ListExpr
typeUToListExpr (ListVarU v) = Just (LS.ListVar v)
typeUToListExpr (ListLitU es) = Just (LS.ListLit es)
typeUToListExpr (ListAppU a b) = LS.ListApp <$> typeUToListExpr a <*> typeUToListExpr b
typeUToListExpr _ = Nothing

listExprToTypeU :: LS.ListExpr -> TypeU
listExprToTypeU (LS.ListVar v) = ListVarU v
listExprToTypeU (LS.ListLit es) = ListLitU es
listExprToTypeU (LS.ListApp a b) = ListAppU (listExprToTypeU a) (listExprToTypeU b)

-- | Insert solved List-tail variable assignments into the gammaListSubs.
applyListSolutions :: Map.Map TVar LS.ListExpr -> Gamma -> Gamma
applyListSolutions subs g0 = foldl insertSub g0 (Map.toList subs)
  where
    insertSub g (v, le) = g { gammaListSubs = Map.insert v (listExprToTypeU le) (gammaListSubs g) }

-- | True iff the TypeU is a Set-kinded expression. Mirrors isRecExpr.
isSetExpr :: TypeU -> Bool
isSetExpr (SetVarU _) = True
isSetExpr SetEmptyU = True
isSetExpr (SetUnionU _ _) = True
isSetExpr (SetInterU _ _) = True
isSetExpr (SetDiffU _ _) = True
isSetExpr SetVoidU = True
isSetExpr _ = False

typeUToSetExpr :: TypeU -> Maybe SetS.SetExpr
typeUToSetExpr (SetVarU v) = Just (SetS.SetVar v)
typeUToSetExpr SetEmptyU = Just SetS.SetEmpty
typeUToSetExpr (SetUnionU a b) = SetS.SetUnion <$> typeUToSetExpr a <*> typeUToSetExpr b
typeUToSetExpr (SetInterU a b) = SetS.SetInter <$> typeUToSetExpr a <*> typeUToSetExpr b
typeUToSetExpr (SetDiffU a b) = SetS.SetDiff <$> typeUToSetExpr a <*> typeUToSetExpr b
typeUToSetExpr _ = Nothing

setExprToTypeU :: SetS.SetExpr -> TypeU
setExprToTypeU (SetS.SetVar v) = SetVarU v
setExprToTypeU SetS.SetEmpty = SetEmptyU
setExprToTypeU (SetS.SetLit []) = SetEmptyU
setExprToTypeU (SetS.SetLit (x:xs)) = SetUnionU (setExprToTypeU (SetS.SetLit xs)) (singletonU x)
  where
    singletonU = SetUnionU SetEmptyU
setExprToTypeU (SetS.SetUnion a b) = SetUnionU (setExprToTypeU a) (setExprToTypeU b)
setExprToTypeU (SetS.SetInter a b) = SetInterU (setExprToTypeU a) (setExprToTypeU b)
setExprToTypeU (SetS.SetDiff a b) = SetDiffU (setExprToTypeU a) (setExprToTypeU b)

-- | Insert solved Set-tail variable assignments into the gammaSetSubs.
applySetSolutions :: Map.Map TVar SetS.SetExpr -> Gamma -> Gamma
applySetSolutions subs g0 = foldl insertSub g0 (Map.toList subs)
  where
    insertSub g (v, se) = g { gammaSetSubs = Map.insert v (setExprToTypeU se) (gammaSetSubs g) }

applyNatSolutions :: Map.Map TVar NS.NatExpr -> Gamma -> Either MDoc Gamma
applyNatSolutions subs g0 = foldM applySub g0 (Map.toList subs)
  where
    applySub g (v, ne) =
      let t = natExprToTypeU ne
      -- Try solving as existential first (for existential nat vars),
      -- then store in gammaNatSubs (for NatVarU variables)
      in case solveExist v t g of
           Right (Just g') -> Right g'
           Right Nothing ->
             -- Not an existential — store as a NatVarU solution
             Right g { gammaNatSubs = Map.insert v t (gammaNatSubs g) }
           Left err -> Left err

-- | Re-check deferred Nat / Str constraints after all existentials are
-- solved. Applies the final gamma to each deferred pair, converts to the
-- appropriate kind-specific expression, and re-solves. Returns Left on
-- contradiction, Right with remaining still-deferred constraints (now
-- truly unsolvable).
recheckDeferred :: Gamma -> Either MDoc [(TypeU, TypeU)]
recheckDeferred g = foldM check [] (gammaDeferred g)
  where
    check acc (t1, t2) =
      let t1' = apply g t1
          t2' = apply g t2
      in case (typeUToNatExpr t1', typeUToNatExpr t2') of
           (Just ne1, Just ne2) ->
             case NS.solveNat ne1 ne2 of
               Right _ -> Right acc
               Left NS.Contradiction ->
                 Left $ "Nat constraint mismatch (deferred):"
                   <+> prettyTypeU t1' <+> "~" <+> prettyTypeU t2'
               Left (NS.Deferred _) -> Right ((t1', t2') : acc)
           _ -> case (typeUToStrExpr t1', typeUToStrExpr t2') of
             (Just se1, Just se2) ->
               case SS.solveStr se1 se2 of
                 Right _ -> Right acc
                 Left (SS.StrContradiction s1 s2) ->
                   Left $ "Str constraint mismatch (deferred):"
                     <+> dquotes (pretty s1) <+> "vs" <+> dquotes (pretty s2)
                 Left SS.StrDeferred -> Right ((t1', t2') : acc)
             _ -> case (typeUToRecExpr t1', typeUToRecExpr t2') of
               (Just re1, Just re2) ->
                 case RS.solveRec re1 re2 of
                   Right _ -> Right acc
                   Left (RS.RecContradiction msg) ->
                     Left $ "Rec constraint mismatch (deferred):" <+> pretty msg
                   Left (RS.RecMalformed msg) ->
                     Left $ "Rec malformed (deferred):" <+> pretty msg
                   Left RS.RecDeferred -> Right ((t1', t2') : acc)
               _ -> Right acc  -- not a kind we can solve, skip

-- | type 1 is more polymorphic than type 2 (Dunfield Figure 9)
subtype :: Scope -> TypeU -> TypeU -> Gamma -> Either MDoc Gamma
-- NatVarU: identical nat variables are equal; different ones fall to isNatExpr path
subtype _ (NatVarU v1) (NatVarU v2) g
  | v1 == v2 = return g
-- VarU vs VarT
subtype scope t1@(VarU a1) t2@(VarU a2) g
  -- If everything is the same, do nothing
  --
  -- ----------------------------------------- <:Var
  --  G[a] |- a_l <: a_l -| G[a]
  | a1 == a2 = return g
  | otherwise = subtypeEvaluated scope t1 t2 g
subtype scope a@ExistU {} b@ExistU {} g
  --
  -- ----------------------------------------- <:Exvar
  --  G[E.a] |- E.a <: E.a -| G[E.a]
  | a == b = return g
  -- ----------------------------------------- <:InstantiateL/<:InstantiateR
  --  G[E.a] |- Ea <: Ea -| G[E.a]
  | otherwise = instantiate scope a b g
-- formally, an `Ea notin FV(G)` check should be done here, but since the
-- types involved are all existentials, it will always pass, so I omit
-- it.

-- EffectU: covariant subtyping with effect row subsumption.
-- <E1> T1 <: <E2> T2 when E1 is a subset of E2 and T1 <: T2.
-- Fewer effects can be used where more effects are expected.
-- An effectful type cannot be narrowed to a type with fewer effects: this
-- is the narrowing check the user-facing rules call out. When either side
-- has an unsolved EffectVar we defer (effect inference does not yet solve
-- effect variables); for concrete sets the subset check is strict.
subtype scope t1@(EffectU e1 i1) t2@(EffectU e2 i2) g
  | effectSubsetOf e1 e2 = subtype scope i1 i2 g
  | effectSetHasVar e1 || effectSetHasVar e2 = subtype scope i1 i2 g
  | otherwise = subtypeError t1 t2 "effect set on left is not a subset of effect set on right"
-- Effectful type on the left, non-effectful on the right. An effect type
-- never satisfies a non-effect expected type and never instantiates a type
-- variable (no ExistU/ForallU escape): an effectful computation must be run
-- in a do-block and its pure result used instead. This is what makes the
-- effect coloring sound -- a thunk (a function) can never reach a pure or
-- polymorphic position, where the cross-language runtime cannot serialize it.
subtype _ t1@(EffectU _ _) t2 _ =
  subtypeError t1 t2 $
    "an effectful value cannot be used where a non-effectful type is"
      <+> "expected; run it in a do-block first (x <- e) and use the"
      <+> "bound value"
-- OptionalU: covariant subtyping
subtype scope (OptionalU t1) (OptionalU t2) g = subtype scope t1 t2 g
--  g1 |- B1 <: A1 -| g2
--  g2 |- [g2]A2 <: [g2]B2 -| g3
-- ----------------------------------------- <:-->
--  g1 |- A1 -> A2 <: B1 -> B2 -| g3
--
-- function subtypes are *contravariant* with respect to the input, that is,
-- the subtypes are reversed so we have b1<:a1 instead of a1<:b1.
--
-- Apply context between each argument subtype check so that solved
-- existentials propagate to later arguments. This is necessary when a
-- forall-bound variable appears in multiple argument positions (e.g.,
-- (==) :: c -> c -> Bool passed to fold).
subtype scope t1@(FunU as1 ret1) t2@(FunU as2 ret2) g0
  | length as1 /= length as2 = subtypeError t1 t2 "function arity mismatch"
  | null as1 = subtype scope ret1 ret2 g0
  | otherwise = do
      -- Process all arguments (contravariant: b <: a), applying context between each
      g1 <- foldlM (\g (b, a) -> subtype scope (apply g b) (apply g a) g) g0 (zip as2 as1)
      -- Apply context to return types, then subtype
      subtype scope (apply g1 ret1) (apply g1 ret2) g1

--  g1 |- A1 <: B1
-- ----------------------------------------- <:App
--  g1 |- A1 A2 <: B1 B2 -| g2
--  unparameterized types are the same as VarT, so subtype on that instead
subtype scope t1@(AppU v1@(ExistU _ _ _) vs1) t2@(AppU v2 vs2) g
  | length vs1 == length vs2 = zipSubtype t1 t2 scope (v1 : vs1) (v2 : vs2) g
  | otherwise = subtypeEvaluated scope t1 t2 g
subtype scope t1@(AppU v1 vs1) t2@(AppU v2@(ExistU _ _ _) vs2) g
  | length vs1 == length vs2 = zipSubtype t1 t2 scope (v1 : vs1) (v2 : vs2) g
  | otherwise = subtypeEvaluated scope t1 t2 g
subtype scope t1@(AppU v1 vs1) t2@(AppU v2 vs2) g
  | v1 == v2 && length vs1 == length vs2 = zipSubtype t1 t2 scope vs1 vs2 g
  -- Concrete-side type aliases for nat-parameterised types (e.g.
  -- `type Cpp => Vector (n :: Nat) a = "mlc::Tensor1<$1>" a`) drop Nat
  -- args from the C++ template -- the morloc-side has [n, a] but the
  -- concrete head only takes [a]. When comparing AppUs with the same
  -- head, retry after filtering Nat args from both sides.
  | v1 == v2
  , let vs1' = filter (not . isNatExpr) vs1
        vs2' = filter (not . isNatExpr) vs2
  , length vs1' == length vs2' && length vs1' /= length vs1
  = zipSubtype t1 t2 scope vs1' vs2' g
  | otherwise = subtypeEvaluated scope t1 t2 g
-- subtype unordered records
subtype scope (NamU _ v1 _ []) (NamU _ v2 _ []) g
  -- If one of the records is generic, allow promotion
  | v1 == BT.record || v2 == BT.record = return g
  -- Otherwise subtype the variable names
  | otherwise = subtype scope (VarU v1) (VarU v2) g
subtype _ t1@(NamU _ _ _ []) t2@(NamU _ _ _ _) _ =
  subtypeError t1 t2 "NamU - Unequal number of fields"
subtype _ t1@(NamU _ _ _ _) t2@(NamU _ _ _ []) _ =
  subtypeError t1 t2 "NamU - Unequal number of fields"
subtype scope t1@(NamU o1 v1 p1 ((k1, x1) : rs1)) t2@(NamU o2 v2 p2 es2) g0 =
  case filterApart (\(k2, _) -> k2 == k1) es2 of
    (Nothing, _) -> subtypeError t1 t2 "NamU - Unequal fields"
    (Just (_, x2), rs2) ->
      subtype scope x1 x2 g0
        >>= subtype scope (NamU o1 v1 p1 rs1) (NamU o2 v2 p2 rs2)
--  Ea not in FV(a)
--  g1[Ea] |- A <=: Ea -| g2
-- ----------------------------------------- <:InstantiateR
--  g1[Ea] |- A <: Ea -| g2
subtype scope a b@(ExistU _ ([], _) _) g = occursCheck b a "InstantiateR" >> instantiate scope a b g
--  Ea not in FV(a)
--  g1[Ea] |- Ea <=: A -| g2
-- ----------------------------------------- <:InstantiateL
--  g1[Ea] |- Ea <: A -| g2
subtype scope a@(ExistU _ ([], _) _) b g = occursCheck a b "InstantiateL" >> instantiate scope a b g
subtype scope a@(AppU _ _) b@(ExistU _ _ _) g = subtype scope b a g
subtype scope t1@(ExistU v1 (ps1, pc1) rs@([], _)) t2@(AppU _ ps2) g1
  -- if the existential is closed and the parameter length is not equal, die
  | pc1 == Closed && length ps1 /= length ps2 =
      subtypeError t1 t2 "InstantiateL - Expected equal number of type parameters"
  -- if the exsistential is open and it has fewer parameters, extend the
  -- parameter list and retry
  | pc1 == Open && length ps1 < length ps2 = do
      let (ps1', _) = extendList ps1 ps2
      subtype scope (ExistU v1 (ps1', pc1) rs) t2 g1
  -- existential built by selectorType (`_pattern_*`) has one slot per
  -- selector index, so this mismatch means an index getter overran the
  -- tuple it was applied to.
  | length ps1 > length ps2
  , MT.isPrefixOf "_pattern_" (unTVar v1) =
      subtypeError t1 t2 $ "tuple arity" <+> pretty (length ps2)
        <+> "required, index" <+> pretty (length ps1 - 1) <+> "given"
  | length ps1 > length ps2 =
      subtypeError t1 t2 "InstantiateL - too many parameters in left existential"
  -- otherwise, do the thing
  | otherwise = do
      g2 <- foldM (\g (p1, p2) -> subtype scope p1 p2 g) g1 (zip ps1 ps2)
      solveExist v1 t2 g2 >>= maybe (return g2) return

--  g1,>Ea,Ea |- [Ea/x]A <: B -| g2,>Ea,g3
-- ----------------------------------------- <:ForallL
--  g1 |- Forall x . A <: B -| g2
--
subtype scope (ForallU v a) b g0 = subtype scope (substitute v a) b (g0 +> v)
-- NOTE: I am deviating from the rules here by not cutting. It is not
-- necessary to do so since I rewrote all qualifiers to be globally unique.
-- Also, when I cut here I lose my only link to v, and that caused `map fst`
-- to not compile.

--  g1,a |- A <: B -| g2,a,g3
-- ----------------------------------------- <:ForallR
--  g1 |- A <: Forall a. B -| g2
subtype scope a (ForallU v b) g = subtype scope a b (g +> VarG v) >>= cut (VarG v)
-- NatVoidU is the erased-phantom-Nat sentinel; it is wildcard-compatible
-- with any Nat expression. This mirrors the PartialOrd instance and is
-- needed for subtyping at the codegen boundary, where unweaved TypeFs
-- carry NatVoidU on positions that were erased during weaving (e.g.
-- unresolved opaque output dims). Match before the generic Nat-expression
-- branch so we short-circuit without sending NatVoidU to typeUToNatExpr.
subtype _ NatVoidU t g | isNatExpr t = return g
subtype _ t NatVoidU g | isNatExpr t = return g
-- Nat expressions: compare via SOP normalization (handles commutativity,
-- associativity, and cross-form equality like 2+3 ~ 5)
subtype _ t1 t2 g
  | isNatExpr t1 && isNatExpr t2 =
      let t1' = apply g t1
          t2' = apply g t2
      in case (typeUToNatExpr t1', typeUToNatExpr t2') of
           (Just ne1, Just ne2) ->
             case NS.solveNat ne1 ne2 of
               Right subs
                 | Map.null subs -> return g
                 | otherwise -> applyNatSolutions subs g
               Left NS.Contradiction -> subtypeError t1 t2 "Nat constraint mismatch"
               Left (NS.Deferred _) -> return g { gammaDeferred = (t1', t2') : gammaDeferred g }
           _ -> subtypeError t1 t2 "Cannot compare Nat expressions"
-- StrVoidU is the erased phantom Str slot; it is compatible with any Str
-- expression. Mirrors the NatVoidU rule above.
subtype _ StrVoidU t g | isStrExpr t = return g
subtype _ t StrVoidU g | isStrExpr t = return g
-- Str expressions: compare via literal equality after normalization
-- (handles concat-of-literals folding). Variables defer per memo 04.
subtype _ t1 t2 g
  | isStrExpr t1 && isStrExpr t2 =
      let t1' = apply g t1
          t2' = apply g t2
      in case (typeUToStrExpr t1', typeUToStrExpr t2') of
           (Just se1, Just se2) ->
             case SS.solveStr se1 se2 of
               Right subs
                 | Map.null subs -> return g
                 | otherwise -> return (applyStrSolutions subs g)
               Left (SS.StrContradiction _ _) ->
                 subtypeError t1 t2 "Str constraint mismatch"
               Left SS.StrDeferred ->
                 return g { gammaDeferred = (t1', t2') : gammaDeferred g }
           _ -> subtypeError t1 t2 "Cannot compare Str expressions"
-- RecVoidU is the erased phantom Rec slot; compatible with any Rec
-- expression. Mirrors the NatVoidU and StrVoidU rules above.
subtype _ RecVoidU t g | isRecExpr t = return g
subtype _ t RecVoidU g | isRecExpr t = return g
-- Rec expressions: compare via the Rec solver, which canonicalizes
-- structural ops (extend, union, diff, intersect) and aligns ground
-- field maps. See plans/tables/10-rec-solver-decidability.md.
subtype _ t1 t2 g
  | isRecExpr t1 && isRecExpr t2 =
      let t1' = apply g t1
          t2' = apply g t2
      in case (typeUToRecExpr t1', typeUToRecExpr t2') of
           (Just re1, Just re2) ->
             case RS.solveRec re1 re2 of
               Right subs
                 | Map.null subs -> return g
                 | otherwise -> return (applyRecSolutions subs g)
               Left (RS.RecContradiction msg) ->
                 subtypeError t1 t2 ("Rec constraint mismatch: " <> pretty msg)
               Left (RS.RecMalformed msg) ->
                 subtypeError t1 t2 ("Rec malformed: " <> pretty msg)
               Left RS.RecDeferred ->
                 return g { gammaDeferred = (t1', t2') : gammaDeferred g }
           _ -> subtypeError t1 t2 "Cannot compare Rec expressions"
-- ListVoidU is the erased phantom List slot; compatible with any List
-- expression. Mirrors RecVoidU / StrVoidU.
subtype _ ListVoidU t g | isListExpr t = return g
subtype _ t ListVoidU g | isListExpr t = return g
-- List expressions: dispatch to ListSolver. Canonical form flattens
-- ListAppU into chunk sequences; equality strips matching prefixes
-- and suffixes, solving a single residual variable when possible.
subtype _ t1 t2 g
  | isListExpr t1 && isListExpr t2 =
      let t1' = apply g t1
          t2' = apply g t2
      in case (typeUToListExpr t1', typeUToListExpr t2') of
           (Just le1, Just le2) ->
             case LS.solveList le1 le2 of
               Right subs
                 | Map.null subs -> return g
                 | otherwise -> return (applyListSolutions subs g)
               Left (LS.ListContradiction msg) ->
                 subtypeError t1 t2 ("List constraint mismatch: " <> pretty msg)
               Left LS.ListDeferred ->
                 return g { gammaDeferred = (t1', t2') : gammaDeferred g }
           _ -> subtypeError t1 t2 "Cannot compare List expressions"
-- SetVoidU is the erased phantom Set slot; compatible with any Set
-- expression. Mirrors RecVoidU / ListVoidU.
subtype _ SetVoidU t g | isSetExpr t = return g
subtype _ t SetVoidU g | isSetExpr t = return g
-- Set expressions: dispatch to SetSolver.
subtype _ t1 t2 g
  | isSetExpr t1 && isSetExpr t2 =
      let t1' = apply g t1
          t2' = apply g t2
      in case (typeUToSetExpr t1', typeUToSetExpr t2') of
           (Just se1, Just se2) ->
             case SetS.solveSet se1 se2 of
               Right subs
                 | Map.null subs -> return g
                 | otherwise -> return (applySetSolutions subs g)
               Left (SetS.SetContradiction msg) ->
                 subtypeError t1 t2 ("Set constraint mismatch: " <> pretty msg)
               Left SetS.SetDeferred ->
                 return g { gammaDeferred = (t1', t2') : gammaDeferred g }
           _ -> subtypeError t1 t2 "Cannot compare Set expressions"
-- note that these need to be evaluated AFTER all the existentials
subtype scope t1@(VarU _) t2 g = subtypeEvaluated scope t1 t2 g
subtype scope t1 t2@(VarU _) g = subtypeEvaluated scope t1 t2 g
-- fall through
subtype _ a b _ = subtypeError a b "Type mismatch fall through"

zipSubtype :: TypeU -> TypeU -> Scope -> [TypeU] -> [TypeU] -> Gamma -> Either MDoc Gamma
zipSubtype _ _ _ [] [] g' = return g'
zipSubtype a b scope (t1' : ts1') (t2' : ts2') g' = do
  g'' <- subtype scope t1' t2' g'
  zipSubtype a b scope (map (apply g'') ts1') (map (apply g'') ts2') g''
zipSubtype a b _ _ _ _ = subtypeError a b "Parameter type mismatch"

-- | Dunfield Figure 10 -- type-level structural recursion
instantiate :: Scope -> TypeU -> TypeU -> Gamma -> Either MDoc Gamma
instantiate scope ta@(ExistU _ _ (_ : _, _)) tb@(NamU _ _ _ _) g1 = instantiate scope tb ta g1
instantiate scope ta@(ExistU _ _ (_ : _, _)) tb@(VarU _) g1 = instantiate scope tb ta g1
instantiate scope ta@(VarU _) tb@(ExistU _ _ (_ : _, _)) g1 = do
  case TE.reduceType scope ta of
    (Just ta') -> instantiate scope ta' tb g1
    Nothing -> subtypeError ta tb "Error in VarU versus NamU with existential keys"
instantiate scope ta@(NamU _ _ _ rs1) tb@(ExistU v _ (rs2@(_ : _), rc)) g1 = do
  let keyset1 = Set.fromList $ map fst rs1
      keyset2 = Set.fromList $ map fst rs2
  _ <- case rc of
    -- if the existential keys are closed, the the ta and tb keys must be identical
    Closed ->
      if keyset1 == keyset2
        then return ()
        else subtypeError ta tb "Error in NamU with conflicting closed keysets"
    -- if the existential keys are open, then all existential keys muts be in
    -- ta, but not vice versa
    Open ->
      if Set.isSubsetOf keyset2 keyset1
        then return ()
        else subtypeError ta tb "Error in NamU with conflicting open keysets"

  g2 <-
    foldM
      (\g' (t1, t2) -> subtype scope t1 t2 g')
      g1
      [(t1, t2) | (k1, t1) <- rs1, (k2, t2) <- rs2, k1 == k2]
  solveExist v ta g2 >>= maybe (subtypeError ta tb "Error in NamU with existential keys") return
-- ExistU vs EffectU: solve ?a = <effs> ?b, then ?b <: inner
instantiate scope (ExistU v ([], _) _) (EffectU effs inner) g1 = do
  let (g2, veb) = tvarname g1 "eff"
      eb = ExistU veb ([], Open) ([], Open)
  g3 <- solveExistWith v (EffectU effs eb) [index eb] g2 >>= maybe (return g2) return
  instantiate scope eb (apply g3 inner) g3
instantiate scope (EffectU effs inner) (ExistU v ([], _) _) g1 = do
  let (g2, veb) = tvarname g1 "eff"
      eb = ExistU veb ([], Open) ([], Open)
  g3 <- solveExistWith v (EffectU effs eb) [index eb] g2 >>= maybe (return g2) return
  instantiate scope (apply g3 inner) eb g3
-- ExistU vs OptionalU: solve ?a = ??b, then ?b <: inner
instantiate scope (ExistU v ([], _) _) (OptionalU inner) g1 = do
  let (g2, veb) = tvarname g1 "opt"
      eb = ExistU veb ([], Open) ([], Open)
  g3 <- solveExistWith v (OptionalU eb) [index eb] g2 >>= maybe (return g2) return
  instantiate scope eb (apply g3 inner) g3
instantiate scope (OptionalU inner) (ExistU v ([], _) _) g1 = do
  let (g2, veb) = tvarname g1 "opt"
      eb = ExistU veb ([], Open) ([], Open)
  g3 <- solveExistWith v (OptionalU eb) [index eb] g2 >>= maybe (return g2) return
  instantiate scope (apply g3 inner) eb g3
instantiate scope (ExistU v ([], _) _) (FunU as b) g1 = do
  let (g2, veas) = statefulMap (\g _ -> tvarname g "ta") g1 as
      (g3, veb) = tvarname g2 "to"
      eas = [ExistU v' ([], Open) ([], Open) | v' <- veas]
      eb = ExistU veb ([], Open) ([], Open)
  g4 <- solveExistWith v (FunU eas eb) (index eb : map index eas) g3 >>= maybe (return g3) return
  g5 <- foldlM (\g (e, t) -> instantiate scope e t g) g4 (zip eas as)
  instantiate scope eb (apply g5 b) g5

--  g1[Ea2,Ea1,Ea=Ea1->Ea2] |- Ea1 <=: A1 -| g2
--  g2 |- [g2]A2 <=: Ea2 -| g3
-- ----------------------------------------- InstRApp
--  g1[Ea] |- A1 -> A2 <=: Ea -| g3
instantiate scope (FunU as b) (ExistU v ([], _) _) g1 = do
  let (g2, veas) = statefulMap (\g _ -> tvarname g "ta") g1 as
      (g3, veb) = tvarname g2 "to"
      eas = [ExistU v' ([], Open) ([], Open) | v' <- veas]
      eb = ExistU veb ([], Open) ([], Open)
  g4 <- solveExistWith v (FunU eas eb) (index eb : map index eas) g3 >>= maybe (return g3) return
  g5 <- foldlM (\g (e, t) -> instantiate scope t e g) g4 (zip eas as)
  instantiate scope eb (apply g5 b) g5

-- This is terrible kludge, I am not close to having considered all the edge
-- cases. I need to completely rewrite my type system. Argh. I also need to get
-- rid of all default types. Defaults should be set explicitly in morloc code.
instantiate _ ta@(ExistU _ _ (_ : _, _)) (ExistU v ([], _) ([], _)) g1 =
  solveExist v ta g1 >>= maybe (return g1) return
instantiate _ (ExistU v ([], _) ([], _)) tb@(ExistU _ _ (_ : _, _)) g1 =
  solveExist v tb g1 >>= maybe (return g1) return

--
-- ----------------------------------------- InstLAllR
--
instantiate scope ta@(ExistU _ _ _) (ForallU v2 t2) g1 =
  instantiate scope ta t2 (g1 +> VarG v2)
    >>= cut (VarG v2)
-- InstLReach or instRReach -- each rule eliminates an existential
-- Replace the rightmost with leftmost (G[a][b] --> L,a,M,b=a,R)
-- WARNING: be careful here, since the implementation adds to the front and the
-- formal syntax adds to the back. Don't change anything in the function unless
-- you really know what you are doing and have tests to confirm it.
instantiate scope ta@(ExistU v1 (ps1, pc1) (rs1, rc1)) tb@(ExistU v2 (ps2, pc2) (rs2, rc2)) g1 = do
  -- check and expand open parameters
  (ps1', _) <- case (pc1, pc2, compare (length ps1) (length ps2)) of
    (_, _, EQ) -> Right (ps1, ps2)
    (Closed, Closed, _) -> subtypeError ta tb "Unequal parameter length for closed existentials"
    (Closed, Open, GT) -> Right $ extendList ps1 ps2
    (Closed, Open, LT) -> subtypeError ta tb "Left closed existential parameter list is less than right"
    (Open, Closed, LT) -> Right $ extendList ps1 ps2
    (Open, Closed, GT) -> subtypeError ta tb "Right closed existential parameter list is less than left"
    (Open, Open, _) -> Right $ extendList ps1 ps2

  let keyset1 = Set.fromList (map fst rs1)
  let keyset2 = Set.fromList (map fst rs2)

  -- check and expand open records
  (g2, rs1', _) <- case (rc1, rc2, Set.isSubsetOf keyset1 keyset2, Set.isSubsetOf keyset2 keyset1) of
    (Closed, Closed, False, _) -> subtypeError ta tb "Right closed existential contains keys missing in left closed existential"
    (Closed, Closed, _, False) -> subtypeError ta tb "Right closed existential contains keys missing in left closed existential"
    (Closed, Open, a, False) ->
      subtypeError ta tb $
        "Right existential contains keys missing in left closed existential " <> pretty a
    (Open, Closed, False, b) ->
      subtypeError ta tb $
        "Left existential contains keys missing in right closed existential " <> pretty b
    _ -> extendRec scope g1 rs1 rs2

  g3 <- foldM (\g (t1, t2) -> subtype scope t1 t2 g) g2 (zip ps1 ps2)
  g4 <-
    foldM
      (\g' (t1, t2) -> subtype scope t1 t2 g')
      g3
      [(t1, t2) | (k1, t1) <- rs1, (k2, t2) <- rs2, k1 == k2]

  -- define new types to insert.
  -- Use rs1' for both (rs1' and rs2' contain the same key set after
  -- extendRec; rs1' preserves the left side's original key order).
  -- This matters for record literals (rc1 = Closed): the literal's
  -- field order is the on-disk layout, and downstream serialization
  -- builds the schema from this list. Without this, a sub-record
  -- pattern application would solve the literal's existential to a
  -- record whose key order is pattern-driven (rs2 ++ literal-extras),
  -- and the materialized value would write fields in the wrong
  -- positions.
  let taExpanded = ExistU v1 (ps1', pc1) (rs1', rc1)
  let tbExpanded = ExistU v2 (ps1', pc1) (rs1', rc1)

  -- Check gammaSolved first: if either is already solved, skip access2
  case (Map.lookup v1 (gammaSolved g4), Map.lookup v2 (gammaSolved g4)) of
    (Just t1, Just t2) -> subtype scope t1 t2 g4
    (Just t1, _)       -> subtype scope t1 tb g4
    (_, Just t2)       -> subtype scope ta t2 g4
    _ -> case access2 v1 v2 g4 of
      -- InstLReach: v1 is newer than v2, solve v1 = tbExpanded
      Just _ -> solveExist v1 tbExpanded g4 >>= maybe (return g4) return
      Nothing -> case access2 v2 v1 g4 of
        -- InstRReach: v2 is newer than v1, solve v2 = taExpanded
        Just _ -> solveExist v2 taExpanded g4 >>= maybe (return g4) return
        Nothing -> return g4

--  g1[Ea],>Eb,Eb |- [Eb/x]B <=: Ea -| g2,>Eb,g3
-- ----------------------------------------- InstRAllL
--  g1[Ea] |- Forall x. B <=: Ea -| g2
instantiate scope (ForallU x b) tb@(ExistU _ ([], _) _) g1 =
  instantiate
    scope
    (substitute x b) -- [Eb/x]B
    tb -- Ea
    (g1 +> MarkG x +> ExistG x ([], Open) ([], Open)) -- g1[Ea],>Eb,Eb
    >>= cut (MarkG x)
--  g1 |- t
-- ----------------------------------------- InstRSolve
--  g1,Ea,g2 |- t <=: Ea -| g1,Ea=t,g2
instantiate scope ta (ExistU v ([], _) ([], _)) g1 =
  case lookupU v g1 of
    Just t  -> subtype scope ta t g1 >>= specializeExist scope v t ta
    Nothing -> solveExist v ta g1 >>= maybe (return g1) return

--  g1 |- t
-- ----------------------------------------- instLSolve
--  g1,Ea,g2 |- Ea <=: t -| g1,Ea=t,g2
instantiate scope (ExistU v ([], _) ([], _)) tb g1 =
  case lookupU v g1 of
    Just t  -> subtype scope t tb g1 >>= specializeExist scope v t tb
    Nothing -> solveExist v tb g1 >>= maybe (return g1) return

instantiate _ ta tb _ = subtypeError ta tb "Unexpected types"

-- | After a subtype check succeeds between a solved existential's current
-- value and a new type, check if the new type is more specialized (a
-- descendant in the alias hierarchy). If so, update the solution.
-- E.g., if ?a = List Int and we check against Deque Int, update to Deque Int
-- since Deque is a specialization of List.
specializeExist :: Scope -> TVar -> TypeU -> TypeU -> Gamma -> Either MDoc Gamma
specializeExist scope v currentType newType g
  | isMoreSpecialized scope newType currentType = Right $ cacheSolved v newType g
  | otherwise = Right g

-- | Check if t1 is a more specialized (descendant) alias of t2.
-- t1 is more specialized if it has an alias definition that evaluates to
-- t2's head constructor, while t2 does not evaluate to t1's head.
isMoreSpecialized :: Scope -> TypeU -> TypeU -> Bool
isMoreSpecialized scope (AppU (VarU v1) _) (AppU (VarU v2) _) =
  v1 /= v2 && reducesToHead scope v1 v2
  where
    reducesToHead scope' src target =
      case Map.lookup src scope' of
        Just ((ps, _, _, _) : _)
          | all isGenericParam ps && not (null ps) ->
            let n = length ps
                freshVars = [VarU (TV (MT.show' i <> "__spec_cmp")) | i <- [0 .. n - 1]]
                app = AppU (VarU src) freshVars
            in case TE.evaluateType scope' app of
                 Right (AppU (VarU headV) _) -> headV == target
                 _ -> False
        _ -> False
    isGenericParam (Left _) = True
    isGenericParam _ = False
isMoreSpecialized _ _ _ = False

solve :: TVar -> TypeU -> Either MDoc GammaIndex
solve v t
  | occursIn v t =
      Left $ "Infinite recursion, cannot substitute" <+> pretty v <+> "into type" <+> pretty t
  | otherwise = Right (SolvedG v t)
  where
    occursIn :: TVar -> TypeU -> Bool
    occursIn v' (VarU v'') = v' == v''
    occursIn _ (NatVarU _) = False
    occursIn v' (ExistU v'' (ps, _) (rs, _)) = v' == v'' || any (occursIn v') ps || any (occursIn v' . snd) rs
    occursIn v' (ForallU _ t') = occursIn v' t'
    occursIn v' (FunU ts t') = any (occursIn v') ts || occursIn v' t'
    occursIn v' (AppU t' ts) = occursIn v' t' || any (occursIn v') ts
    occursIn v' (NamU _ _ ps rs) = any (occursIn v') ps || any (occursIn v' . snd) rs
    occursIn v' (EffectU _ t') = occursIn v' t'
    occursIn v' (OptionalU t') = occursIn v' t'
    occursIn _ (NatLitU _) = False
    occursIn v' (NatAddU a b) = occursIn v' a || occursIn v' b
    occursIn v' (NatMulU a b) = occursIn v' a || occursIn v' b
    occursIn v' (NatSubU a b) = occursIn v' a || occursIn v' b
    occursIn v' (NatDivU a b) = occursIn v' a || occursIn v' b
    occursIn _ NatVoidU = False
    occursIn _ (StrVarU _) = False
    occursIn _ (StrLitU _) = False
    occursIn v' (StrConcatU a b) = occursIn v' a || occursIn v' b
    occursIn _ StrVoidU = False
    occursIn _ (RecVarU _) = False
    occursIn _ RecEmptyU = False
    occursIn v' (RecExtendU _ a b) = occursIn v' a || occursIn v' b
    occursIn v' (RecUnionU a b) = occursIn v' a || occursIn v' b
    occursIn v' (RecDiffU a _) = occursIn v' a
    occursIn v' (RecIntersectU a b) = occursIn v' a || occursIn v' b
    occursIn v' (RecRestrictU a b) = occursIn v' a || occursIn v' b
    occursIn v' (RecDiffListU a b) = occursIn v' a || occursIn v' b
    occursIn _ RecVoidU = False
    occursIn _ (ListVarU _) = False
    occursIn v' (ListLitU es) = any (occursIn v') es
    occursIn v' (ListAppU a b) = occursIn v' a || occursIn v' b
    occursIn _ ListVoidU = False
    occursIn _ (SetVarU _) = False
    occursIn _ SetEmptyU = False
    occursIn v' (SetLitU es) = any (occursIn v') es
    occursIn v' (SetUnionU a b) = occursIn v' a || occursIn v' b
    occursIn v' (SetInterU a b) = occursIn v' a || occursIn v' b
    occursIn v' (SetDiffU a b) = occursIn v' a || occursIn v' b
    occursIn _ SetVoidU = False
    occursIn v' (KeysU r) = occursIn v' r
    occursIn v' (ListToSetU l) = occursIn v' l
    occursIn v' (SizeU c) = occursIn v' c
    occursIn v' (ProjectFieldU r f) = occursIn v' r || occursIn v' f
    occursIn v' (RecSingletonU k vt) = occursIn v' k || occursIn v' vt
    occursIn v' (LabeledU _ t') = occursIn v' t'

-- | Record a solved variable in the gamma map cache
cacheSolved :: TVar -> TypeU -> Gamma -> Gamma
cacheSolved v t g = g {gammaSolved = Map.insert v t (gammaSolved g)}

occursCheck :: TypeU -> TypeU -> Text -> Either MDoc ()
occursCheck t1 t2 place =
  if Set.member t1 (free t2)
    then subtypeError t1 t2 $ "Occurs check at" <+> pretty place
    else Right ()

{- | substitute all appearances of a given variable with an existential
[t/v]A
-}
substitute :: TVar -> TypeU -> TypeU
substitute v = substituteTVar v (ExistU v ([], Open) ([], Open))

-- | Find an unsolved ExistG by TVar. O(log N) via gammaExist index.
-- Returns the slot and entry if found.
access1 :: TVar -> Gamma -> Maybe (Int, GammaIndex)
access1 v g = do
  slot <- Map.lookup v (gammaExist g)
  entry <- IntMap.lookup slot (gammaContext g)
  return (slot, entry)

-- | Check if ExistG v1 has a HIGHER slot (= newer) than ExistG v2.
-- Used for InstLReach/InstRReach ordering.
access2 :: TVar -> TVar -> Gamma -> Maybe (Int, Int)
access2 v1 v2 g = do
  s1 <- Map.lookup v1 (gammaExist g)
  s2 <- Map.lookup v2 (gammaExist g)
  if s1 > s2 then Just (s1, s2) else Nothing

-- | Solve an ExistG: replace it with SolvedG in place. O(log N).
-- Returns Right Nothing if the ExistG is not found (already solved).
-- Returns Left on solve error (e.g., occurs check). Returns Right (Just g) on success.
solveExist :: TVar -> TypeU -> Gamma -> Either MDoc (Maybe Gamma)
solveExist v t g = case Map.lookup v (gammaExist g) of
  Nothing -> Right Nothing
  Just slot -> do
    solved <- solve v t
    return . Just $ cacheSolved v t $ g
      { gammaContext = IntMap.insert slot solved (gammaContext g)
      , gammaExist = Map.delete v (gammaExist g)
      }

-- | Solve an ExistG and insert additional entries between the solved
-- position and older entries. O(log N + K) where K = length extras.
-- The first element of extras gets the highest sub-slot (= newest).
-- Returns Right Nothing if ExistG not found. Left on solve error.
solveExistWith :: TVar -> TypeU -> [GammaIndex] -> Gamma -> Either MDoc (Maybe Gamma)
solveExistWith v t extras g = case Map.lookup v (gammaExist g) of
  Nothing -> Right Nothing
  Just slot -> do
    solved <- solve v t
    let g1 = cacheSolved v t $ g
          { gammaContext = IntMap.insert slot solved (gammaContext g)
          , gammaExist = Map.delete v (gammaExist g)
          }
        -- Insert extras at slots below the solved entry
        insertExtra (g', subSlot) x =
          ( g' { gammaContext = IntMap.insert subSlot x (gammaContext g')
               , gammaExist = case x of
                   ExistG ev _ _ -> Map.insert ev subSlot (gammaExist g')
                   _ -> gammaExist g'
               }
          , subSlot - 1
          )
    return . Just $ fst $ foldl' insertExtra (g1, slot - 1) extras

-- | Look up a solved existential type variable. O(log N) via gammaSolved.
lookupU :: TVar -> Gamma -> Maybe TypeU
lookupU v g = Map.lookup v (gammaSolved g)

-- | Look up an annotation type variable. O(N) scan (AnnG entries are rare).
lookupE :: EVar -> Gamma -> Maybe TypeU
lookupE v g = foldr step Nothing (IntMap.toDescList (gammaContext g))
  where
    step (_, AnnG v' t) _ | v == v' = Just t
    step _ acc = acc

-- | Remove context entries newer than (and including) a marker. O(N) in
-- removed entries for cleanup; O(log N) for the IntMap split itself.
cut :: GammaIndex -> Gamma -> Either MDoc Gamma
cut marker g = do
  -- Find the marker's slot by scanning (markers are infrequent)
  markerSlot <- case [s | (s, gi) <- IntMap.toDescList (gammaContext g), gi == marker] of
    (s : _) -> Right s
    [] -> Left $ "Empty cut" <+> pretty marker
  -- Everything with slot < markerSlot is kept (older entries)
  let kept = fst (IntMap.split markerSlot (gammaContext g))
      -- Collect removed entries for cleanup
      removedSlots = IntMap.filterWithKey (\s _ -> s >= markerSlot) (gammaContext g)
      removedSolvedKeys = [v | (_, SolvedG v _) <- IntMap.toList removedSlots]
      removedExistKeys = [v | (_, ExistG v _ _) <- IntMap.toList removedSlots]
      solvedMap' = foldl' (flip Map.delete) (gammaSolved g) removedSolvedKeys
      existMap' = foldl' (flip Map.delete) (gammaExist g) removedExistKeys
  return $ g
    { gammaContext = kept
    , gammaExist = existMap'
    , gammaSolved = solvedMap'
    }

-- | Convert context to a list (newest first) for iteration/debugging.
gammaContextList :: Gamma -> [GammaIndex]
gammaContextList g = map snd (IntMap.toDescList (gammaContext g))

-- | Trim context back to the state it had at a given slot counter value.
-- Removes all entries with slot >= the given threshold.
gammaTrimAfter :: Int -> Gamma -> Gamma
gammaTrimAfter slotThreshold g =
  let kept = fst (IntMap.split slotThreshold (gammaContext g))
      removedSlots = IntMap.filterWithKey (\s _ -> s >= slotThreshold) (gammaContext g)
      removedSolvedKeys = [v | (_, SolvedG v _) <- IntMap.toList removedSlots]
      removedExistKeys = [v | (_, ExistG v _ _) <- IntMap.toList removedSlots]
      solvedMap' = foldl' (flip Map.delete) (gammaSolved g) removedSolvedKeys
      existMap' = foldl' (flip Map.delete) (gammaExist g) removedExistKeys
  in g
    { gammaContext = kept
    , gammaSlot = slotThreshold
    , gammaExist = existMap'
    , gammaSolved = solvedMap'
    }

selectorType :: Gamma -> Selector -> MorlocMonad (Gamma, TypeU)
selectorType g0 SelectorEnd = do
  let (g1, s) = newvar "_pattern_" g0
  return (g1, s)
selectorType g0 (SelectorIdx x xs) = do
  -- highest index in this pattern, matching tuple must be at least this long
  let maxIndex = maximum (map fst (x : xs))

  -- combine groups, e.g.: .(.1.(0,1), .1.2, .2) --> .(.1.(0,1,2), .2)
  xs' <- mapM (secondM weaveSelectors) (groupSort (x : xs))

  (g1, ts) <- statefulMapM (makeIndexType xs') g0 (take (maxIndex + 1) [0 ..])

  return $ newvarRich (ts, Open) ([], Closed) "_pattern_" g1
  where
    makeIndexType :: [(Int, Selector)] -> Gamma -> Int -> MorlocMonad (Gamma, TypeU)
    makeIndexType xs' g i = case lookup i xs' of
      (Just s) -> selectorType g s
      Nothing -> selectorType g SelectorEnd
selectorType g0 (SelectorKey x xs) = do
  xs' <- mapM (secondM weaveSelectors) (groupSort (x : xs))
  (g1, ss) <- statefulMapM selectorType g0 (map snd xs')
  return $ newvarRich ([], Closed) (zip (map (Key . fst) xs') ss, Open) "_pattern_" g1

weaveSelectors :: [Selector] -> MorlocMonad Selector
weaveSelectors [] = return SelectorEnd
weaveSelectors (s0 : ss0) = foldrM weavePair s0 ss0
  where
    weavePair :: Selector -> Selector -> MorlocMonad Selector
    weavePair SelectorEnd s = return s
    weavePair s SelectorEnd = return s
    weavePair (SelectorIdx s1 ss1) (SelectorIdx s2 ss2) = do
      xs <- mapM (secondM weaveSelectors) (groupSort ((s1 : ss1) <> (s2 : ss2)))
      return $ SelectorIdx (head xs) (tail xs)
    weavePair (SelectorKey s1 ss1) (SelectorKey s2 ss2) = do
      xs <- mapM (secondM weaveSelectors) (groupSort ((s1 : ss1) <> (s2 : ss2)))
      return $ SelectorKey (head xs) (tail xs)
    weavePair x@(SelectorKey _ _) y@(SelectorIdx _ _) = weavePair y x
    weavePair (SelectorIdx _ _) (SelectorKey _ _) = MM.throwSystemError $ "Bad pattern, cannot merge index and keyword patterns"

selectorGetter :: TypeU -> Selector -> [TypeU]
selectorGetter t SelectorEnd = [t]
selectorGetter (ExistU _ _ (ks, _)) (SelectorKey x xs) =
  concat [maybe [] (\t -> selectorGetter t s) (lookup (Key k) ks) | (k, s) <- (x : xs)]
selectorGetter (ExistU _ (ts, _) _) (SelectorIdx x xs) =
  concat [selectorGetter (ts !! i) s | (i, s) <- (x : xs)]
selectorGetter _ _ = error "Unreachable"

-- | map over a type using a selector and update the type using set values
selectorSetter ::
  [TypeU] -> -- types to which the selected fields are set
  Selector -> -- current selector pattern
  TypeU -> -- current type that is being updated
  TypeU -- modified return type
selectorSetter setTypes0 s0 t0 = fst (f t0 setTypes0 s0)
  where
    f ::
      TypeU ->
      [TypeU] ->
      Selector ->
      (TypeU, [TypeU]) -- the modified type and the list of remaining setters
    f _ (t : ts) SelectorEnd = (t, ts)
    -- Walk selectors left-to-right so the i-th selector consumes the i-th
    -- value from setTypes1; foldr would walk right-to-left and swap them.
    f (ExistU v (ts, tc) (ks, kc)) setTypes1 (SelectorKey s ss) =
      let (ks', setTypes2) = foldl' (flip subKey) (ks, setTypes1) (s : ss)
       in (ExistU v (ts, tc) (ks', kc), setTypes2)
    f (NamU o v ps ks) setTypes1 (SelectorKey s ss) =
      let (ks', setTypes2) = foldl' (flip subKey) (ks, setTypes1) (s : ss)
       in (NamU o v ps ks', setTypes2)
    -- handle non-existential records
    --  * note that this may well change the field type of the record, this should
    --    raise an error later if such changes are not allowed
    f (ExistU v (ts, tc) (ks, kc)) setTypes1 (SelectorIdx s ss) =
      let (ts', setTypes2) = foldl subIdx (ts, setTypes1) (s : ss)
       in (ExistU v (ts', tc) (ks, kc), setTypes2)
    -- handle non-existential tuples
    f (AppU t ts) setTypes1 (SelectorIdx s ss)
      -- if this is a tuple, fine, proceed
      | (VarU (BT.tuple (length ts))) == t =
          let (ts', setTypes2) = foldl subIdx (ts, setTypes1) (s : ss)
           in (AppU t ts', setTypes2)
      -- otherwise die
      | otherwise = error "Unreachable case"
    -- and die some more
    f _ _ _ = error "Unreachable pattern case"

    subKey :: (Text, Selector) -> ([(Key, TypeU)], [TypeU]) -> ([(Key, TypeU)], [TypeU])
    subKey (k, s) (ks, setTypesN) = case lookup (Key k) ks of
      Nothing -> error "Malformed pattern"
      (Just priorType) -> (ks', setTypesN')
        where
          (newType, setTypesN') = f priorType setTypesN s
          ks' = [if k' == k then (Key k, newType) else x | x@(Key k', _) <- ks]

    subIdx :: ([TypeU], [TypeU]) -> (Int, Selector) -> ([TypeU], [TypeU])
    subIdx (ts, setTypesN) (i, s)
      | i < length ts =
          let (newType, setTypesN') = f (ts !! i) setTypesN s
           in (take i ts <> [newType] <> drop (i + 1) ts, setTypesN')
      | otherwise = error $ "Bad pattern, index " <> show i <> " is greather than tuple length"

extendList :: [a] -> [a] -> ([a], [a])
extendList [] ys = (ys, ys)
extendList xs [] = (xs, xs)
extendList (x : xs) (y : ys) =
  let (xs', ys') = extendList xs ys
   in (x : xs', y : ys')

extendRec ::
  (Ord k) =>
  Scope ->
  Gamma ->
  [(k, TypeU)] ->
  [(k, TypeU)] ->
  Either MDoc (Gamma, [(k, TypeU)], [(k, TypeU)])
extendRec scope g0 xs ys = do
  g1 <-
    foldlM
      ( \g (k, x) ->
          maybe
            (return g)
            (\y -> subtype scope x y g)
            (lookup k ys)
      )
      g0
      xs
  return $
    ( g1
    , xs <> [y | y@(k, _) <- ys, Set.notMember k setX]
    , ys <> [x | x@(k, _) <- xs, Set.notMember k setY]
    )
  where
    setX = Set.fromList (map fst xs)
    setY = Set.fromList (map fst ys)

newvar :: Text -> Gamma -> (Gamma, TypeU)
newvar = newvarRich ([], Open) ([], Open)

newvarRich ::
  -- | type parameters
  ([TypeU], OpenOrClosed) ->
  -- | key-value pairs
  ([(Key, TypeU)], OpenOrClosed) ->
  -- | prefix, just for readability
  Text ->
  Gamma ->
  (Gamma, TypeU)
newvarRich ps rs prefix g =
  let (g', v) = tvarname g prefix
   in (g' +> ExistG v ps rs, ExistU v ps rs)

-- | standardize quantifier names, for example, replace `a -> b` with `v0 -> v1`.
rename :: Gamma -> TypeU -> (Gamma, TypeU)
rename g0 t0 =
  let (g1, t1, _) = renameWithMap g0 t0 in (g1, t1)

-- | Variant of 'rename' that also returns a rewrite function that can
-- be applied to associated TypeU values (for example, the constraints
-- in a function signature) so they pick up the same fresh-name
-- substitutions. Used by 'renameEType' to keep a signature's
-- constraint clauses in sync with the renamed type.
renameWithMap :: Gamma -> TypeU -> (Gamma, TypeU, TypeU -> TypeU)
renameWithMap g0 (ForallU v@(TV s) t0) =
  let (g1, v') = tvarname g0 (s <> "@q")
      (g2, t1, rw) = renameWithMap g1 t0
      t2 = substituteTVar v (VarU v') t1
      rw' = substituteTVar v (VarU v') . rw
   in (g2, ForallU v' t2, rw')
-- After stripping ForallU, rename NatVarU / StrVarU / RecVarU variables to
-- fresh names. All three kinds are implicitly forall-quantified and need
-- per-instantiation freshening so multiple uses of a polymorphic function
-- don't accidentally share a kind-variable name across call sites.
renameWithMap g0 t0 =
  let nvs = nub (collectNatVarNames t0)
      svs = nub (collectStrVarNames t0)
      rvs = nub (collectRecVarNames t0)
      lvs = nub (collectListVarNames t0)
   in if null nvs && null svs && null rvs && null lvs
      then (g0, t0, id)
      else
        let (g1, nvs') = statefulMap (\g (TV s) -> tvarname g (s <> "@n")) g0 nvs
            (g2, svs') = statefulMap (\g (TV s) -> tvarname g (s <> "@s")) g1 svs
            (g3, rvs') = statefulMap (\g (TV s) -> tvarname g (s <> "@r")) g2 rvs
            (g4, lvs') = statefulMap (\g (TV s) -> tvarname g (s <> "@l")) g3 lvs
            natMap = Map.fromList (zip nvs nvs')
            strMap = Map.fromList (zip svs svs')
            recMap = Map.fromList (zip rvs rvs')
            listMap = Map.fromList (zip lvs lvs')
            rw = renameKindedVars natMap strMap recMap listMap
         in (g4, rw t0, rw)

-- | Rename a signature's TypeU together with its primitive constraints.
-- Both share the same fresh-name substitutions so a constraint like
-- @CDisjoint (KeysU r1) (KeysU r2)@ tracks the same renamed @r1@ and
-- @r2@ that appear in the renamed type.
renameEType :: Gamma -> EType -> (Gamma, EType)
renameEType g0 et =
  let (g1, t', rw) = renameWithMap g0 (etype et)
      cs' = Set.map (renameConstraint rw) (econs et)
   in (g1, et { etype = t', econs = cs' })
  where
    renameConstraint rw (Constraint cls ts) = Constraint cls (map rw ts)
    renameConstraint rw (CMember a s) = CMember (rw a) (rw s)
    renameConstraint rw (CSubset a b) = CSubset (rw a) (rw b)
    renameConstraint rw (CDisjoint a b) = CDisjoint (rw a) (rw b)

-- | Rename kind-variable occurrences according to per-kind name maps.
-- Mirrors the old renameNatVars but additionally renames StrVarU and
-- RecVarU. Each kind has its own map so name spaces stay disjoint.
renameKindedVars ::
  Map.Map TVar TVar ->  -- Nat-var renames
  Map.Map TVar TVar ->  -- Str-var renames
  Map.Map TVar TVar ->  -- Rec-var renames
  Map.Map TVar TVar ->  -- List-var renames
  TypeU -> TypeU
renameKindedVars natM strM recM listM = go
  where
    renN v = Map.findWithDefault v v natM
    renS v = Map.findWithDefault v v strM
    renR v = Map.findWithDefault v v recM
    renL v = Map.findWithDefault v v listM
    go (NatVarU v) = NatVarU (renN v)
    go (StrVarU v) = StrVarU (renS v)
    go (RecVarU v) = RecVarU (renR v)
    go (VarU v) = VarU v
    go (ExistU v (ts, tc) (rs, rc)) = ExistU v (map go ts, tc) ([(k, go t) | (k, t) <- rs], rc)
    go (ForallU v t) = ForallU v (go t)
    go (FunU ts t) = FunU (map go ts) (go t)
    go (AppU t ts) = AppU (go t) (map go ts)
    go (NamU n o ps rs) = NamU n o (map go ps) [(k, go t) | (k, t) <- rs]
    go (EffectU effs t) = EffectU effs (go t)
    go (OptionalU t) = OptionalU (go t)
    go t@(NatLitU _) = t
    go (NatAddU a b) = NatAddU (go a) (go b)
    go (NatMulU a b) = NatMulU (go a) (go b)
    go (NatSubU a b) = NatSubU (go a) (go b)
    go (NatDivU a b) = NatDivU (go a) (go b)
    go t@NatVoidU = t
    go t@(StrLitU _) = t
    go (StrConcatU a b) = StrConcatU (go a) (go b)
    go t@StrVoidU = t
    go t@RecEmptyU = t
    go (RecExtendU k a b) = RecExtendU k (go a) (go b)
    go (RecUnionU a b) = RecUnionU (go a) (go b)
    go (RecDiffU a ks) = RecDiffU (go a) ks
    go (RecIntersectU a b) = RecIntersectU (go a) (go b)
    go (RecRestrictU a b) = RecRestrictU (go a) (go b)
    go (RecDiffListU a b) = RecDiffListU (go a) (go b)
    go t@RecVoidU = t
    go (ListVarU v) = ListVarU (renL v)
    go (ListLitU es) = ListLitU (map go es)
    go (ListAppU a b) = ListAppU (go a) (go b)
    go t@ListVoidU = t
    go t@(SetVarU _) = t
    go t@SetEmptyU = t
    go (SetLitU es) = SetLitU (map go es)
    go (SetUnionU a b) = SetUnionU (go a) (go b)
    go (SetInterU a b) = SetInterU (go a) (go b)
    go (SetDiffU a b) = SetDiffU (go a) (go b)
    go t@SetVoidU = t
    go (KeysU r) = KeysU (go r)
    go (ListToSetU l) = ListToSetU (go l)
    go (SizeU c) = SizeU (go c)
    go (ProjectFieldU r f) = ProjectFieldU (go r) (go f)
    go (RecSingletonU k v) = RecSingletonU (go k) (go v)
    go (LabeledU n t) = LabeledU n (go t)

{- | Rename all generic type variables (ForallU-bound and ExistU) to clean
letters from a lazy pool: a, b, c, ..., z, a1, b1, ..., z1, a2, ...
Avoids names already used by concrete types in the expression.
-}
cleanTypeName :: TypeU -> TypeU
cleanTypeName t0 =
  let (vs, body) = unqualify t0
      evs = collectExistVars body
      nvs = collectNatVarNames body
      svs = collectStrVarNames body
      rvs = collectRecVarNames body
      allGeneric = nub (vs ++ evs ++ nvs ++ svs ++ rvs)
      fixed = collectFixedNames (Set.fromList allGeneric) body
      pool = filter (\(TV n) -> Set.notMember n fixed) letterPool
      renameMap = Map.fromList (zip allGeneric pool)
      renamedBody = applyVarRenaming renameMap body
      renamedVs = map (\v -> Map.findWithDefault v v renameMap) vs
   in simplifyNats $ qualify renamedVs renamedBody

-- | Simplify nat arithmetic in types (e.g., 34 + (4 + 5) -> 43)
simplifyNats :: TypeU -> TypeU
simplifyNats = go
  where
    go (NatAddU a b) = trySimplify (NatAddU (go a) (go b))
    go (NatMulU a b) = trySimplify (NatMulU (go a) (go b))
    go (NatSubU a b) = trySimplify (NatSubU (go a) (go b))
    go (NatDivU a b) = trySimplify (NatDivU (go a) (go b))
    go (AppU f ts) = AppU (go f) (map go ts)
    go (FunU ts r) = FunU (map go ts) (go r)
    go (ForallU v t) = ForallU v (go t)
    go (NamU o v ps es) = NamU o v (map go ps) [(k, go t) | (k, t) <- es]
    go (ExistU v (ps, pc) (rs, rc)) = ExistU v (map go ps, pc) ([(k, go t) | (k, t) <- rs], rc)
    go (EffectU e t) = EffectU e (go t)
    go (OptionalU t) = OptionalU (go t)
    go t@(NatVarU _) = t
    go (LabeledU n t) = LabeledU n (go t)
    go t = t

    trySimplify nat = case typeUToNatExpr nat of
      Just ne -> natExprToTypeU (NS.sopToNatExpr (NS.normalize ne))
      Nothing -> nat

letterPool :: [TVar]
letterPool =
  [ TV (MT.singleton c <> suffix)
  | suffix <- "" : map MT.show' [1 :: Int ..]
  , c <- ['a' .. 'z']
  ]

collectExistVars :: TypeU -> [TVar]
collectExistVars = go
  where
    go (VarU _) = []
    go (NatVarU _) = []
    go (ExistU v (ts, _) (rs, _)) = v : concatMap go ts ++ concatMap (go . snd) rs
    go (ForallU _ t) = go t
    go (FunU ts t) = concatMap go (t : ts)
    go (AppU t ts) = concatMap go (t : ts)
    go (NamU _ _ ps rs) = concatMap go ps ++ concatMap (go . snd) rs
    go (EffectU _ t) = go t
    go (OptionalU t) = go t
    go (NatLitU _) = []
    go (NatAddU a b) = go a ++ go b
    go (NatMulU a b) = go a ++ go b
    go (NatSubU a b) = go a ++ go b
    go (NatDivU a b) = go a ++ go b
    go NatVoidU = []
    go (StrVarU _) = []
    go (StrLitU _) = []
    go (StrConcatU a b) = go a ++ go b
    go StrVoidU = []
    go (RecVarU _) = []
    go RecEmptyU = []
    go (RecExtendU _ a b) = go a ++ go b
    go (RecUnionU a b) = go a ++ go b
    go (RecDiffU a _) = go a
    go (RecIntersectU a b) = go a ++ go b
    go (RecRestrictU a b) = go a ++ go b
    go (RecDiffListU a b) = go a ++ go b
    go RecVoidU = []
    go (ListVarU _) = []
    go (ListLitU es) = concatMap go es
    go (ListAppU a b) = go a ++ go b
    go ListVoidU = []
    go (SetVarU _) = []
    go SetEmptyU = []
    go (SetLitU es) = concatMap go es
    go (SetUnionU a b) = go a ++ go b
    go (SetInterU a b) = go a ++ go b
    go (SetDiffU a b) = go a ++ go b
    go SetVoidU = []
    go (KeysU r) = go r
    go (ListToSetU l) = go l
    go (SizeU c) = go c
    go (ProjectFieldU r f) = go r ++ go f
    go (RecSingletonU k v) = go k ++ go v
    go (LabeledU _ t) = go t

-- | Collect NatVarU variable names from a type (for renaming)
collectNatVarNames :: TypeU -> [TVar]
collectNatVarNames = go
  where
    go (NatVarU v) = [v]
    go (VarU _) = []
    go (ExistU _ (ts, _) (rs, _)) = concatMap go ts ++ concatMap (go . snd) rs
    go (ForallU _ t) = go t
    go (FunU ts t) = concatMap go (t : ts)
    go (AppU t ts) = concatMap go (t : ts)
    go (NamU _ _ ps rs) = concatMap go ps ++ concatMap (go . snd) rs
    go (EffectU _ t) = go t
    go (OptionalU t) = go t
    go (NatLitU _) = []
    go (NatAddU a b) = go a ++ go b
    go (NatMulU a b) = go a ++ go b
    go (NatSubU a b) = go a ++ go b
    go (NatDivU a b) = go a ++ go b
    go NatVoidU = []
    go (StrVarU _) = []
    go (StrLitU _) = []
    go (StrConcatU a b) = go a ++ go b
    go StrVoidU = []
    go (RecVarU _) = []
    go RecEmptyU = []
    go (RecExtendU _ a b) = go a ++ go b
    go (RecUnionU a b) = go a ++ go b
    go (RecDiffU a _) = go a
    go (RecIntersectU a b) = go a ++ go b
    go (RecRestrictU a b) = go a ++ go b
    go (RecDiffListU a b) = go a ++ go b
    go RecVoidU = []
    go (ListVarU _) = []
    go (ListLitU es) = concatMap go es
    go (ListAppU a b) = go a ++ go b
    go ListVoidU = []
    go (SetVarU _) = []
    go SetEmptyU = []
    go (SetLitU es) = concatMap go es
    go (SetUnionU a b) = go a ++ go b
    go (SetInterU a b) = go a ++ go b
    go (SetDiffU a b) = go a ++ go b
    go SetVoidU = []
    go (KeysU r) = go r
    go (ListToSetU l) = go l
    go (SizeU c) = go c
    go (ProjectFieldU r f) = go r ++ go f
    go (RecSingletonU k v) = go k ++ go v
    go (LabeledU _ t) = go t

-- | Collect StrVarU variable names from a type. Mirrors collectNatVarNames.
-- Used by the resolveStrLabels bridge that turns runtime Str-literal args
-- into type-level Str solutions when the function uses f:Str labels.
collectStrVarNames :: TypeU -> [TVar]
collectStrVarNames = go
  where
    go (StrVarU v) = [v]
    go (VarU _) = []
    go (NatVarU _) = []
    go (ExistU _ (ts, _) (rs, _)) = concatMap go ts ++ concatMap (go . snd) rs
    go (ForallU _ t) = go t
    go (FunU ts t) = concatMap go (t : ts)
    go (AppU t ts) = concatMap go (t : ts)
    go (NamU _ _ ps rs) = concatMap go ps ++ concatMap (go . snd) rs
    go (EffectU _ t) = go t
    go (OptionalU t) = go t
    go (NatLitU _) = []
    go (NatAddU a b) = go a ++ go b
    go (NatMulU a b) = go a ++ go b
    go (NatSubU a b) = go a ++ go b
    go (NatDivU a b) = go a ++ go b
    go NatVoidU = []
    go (StrLitU _) = []
    go (StrConcatU a b) = go a ++ go b
    go StrVoidU = []
    go (RecVarU _) = []
    go RecEmptyU = []
    go (RecExtendU _ a b) = go a ++ go b
    go (RecUnionU a b) = go a ++ go b
    go (RecDiffU a _) = go a
    go (RecIntersectU a b) = go a ++ go b
    go (RecRestrictU a b) = go a ++ go b
    go (RecDiffListU a b) = go a ++ go b
    go RecVoidU = []
    go (ListVarU _) = []
    go (ListLitU es) = concatMap go es
    go (ListAppU a b) = go a ++ go b
    go ListVoidU = []
    go (SetVarU _) = []
    go SetEmptyU = []
    go (SetLitU es) = concatMap go es
    go (SetUnionU a b) = go a ++ go b
    go (SetInterU a b) = go a ++ go b
    go (SetDiffU a b) = go a ++ go b
    go SetVoidU = []
    go (KeysU r) = go r
    go (ListToSetU l) = go l
    go (SizeU c) = go c
    go (ProjectFieldU r f) = go r ++ go f
    go (RecSingletonU k v) = go k ++ go v
    go (LabeledU _ t) = go t

-- | Collect RecVarU variable names from a type. Mirrors collectNatVarNames.
collectRecVarNames :: TypeU -> [TVar]
collectRecVarNames = go
  where
    go (RecVarU v) = [v]
    go (VarU _) = []
    go (NatVarU _) = []
    go (StrVarU _) = []
    go (ExistU _ (ts, _) (rs, _)) = concatMap go ts ++ concatMap (go . snd) rs
    go (ForallU _ t) = go t
    go (FunU ts t) = concatMap go (t : ts)
    go (AppU t ts) = concatMap go (t : ts)
    go (NamU _ _ ps rs) = concatMap go ps ++ concatMap (go . snd) rs
    go (EffectU _ t) = go t
    go (OptionalU t) = go t
    go (NatLitU _) = []
    go (NatAddU a b) = go a ++ go b
    go (NatMulU a b) = go a ++ go b
    go (NatSubU a b) = go a ++ go b
    go (NatDivU a b) = go a ++ go b
    go NatVoidU = []
    go (StrLitU _) = []
    go (StrConcatU a b) = go a ++ go b
    go StrVoidU = []
    go RecEmptyU = []
    go (RecExtendU _ a b) = go a ++ go b
    go (RecUnionU a b) = go a ++ go b
    go (RecDiffU a _) = go a
    go (RecIntersectU a b) = go a ++ go b
    go (RecRestrictU a b) = go a ++ go b
    go (RecDiffListU a b) = go a ++ go b
    go RecVoidU = []
    go (ListVarU _) = []
    go (ListLitU es) = concatMap go es
    go (ListAppU a b) = go a ++ go b
    go ListVoidU = []
    go (SetVarU _) = []
    go SetEmptyU = []
    go (SetLitU es) = concatMap go es
    go (SetUnionU a b) = go a ++ go b
    go (SetInterU a b) = go a ++ go b
    go (SetDiffU a b) = go a ++ go b
    go SetVoidU = []
    go (KeysU r) = go r
    go (ListToSetU l) = go l
    go (SizeU c) = go c
    go (ProjectFieldU r f) = go r ++ go f
    go (RecSingletonU k v) = go k ++ go v
    go (LabeledU _ t) = go t

-- | Collect ListVarU variable names from a type. Mirrors collectStrVarNames.
-- Used by the resolveListLabels bridge that turns runtime [Str]-literal args
-- into type-level List solutions when the function uses l:[Str] labels.
collectListVarNames :: TypeU -> [TVar]
collectListVarNames = go
  where
    go (ListVarU v) = [v]
    go (VarU _) = []
    go (NatVarU _) = []
    go (StrVarU _) = []
    go (RecVarU _) = []
    go (SetVarU _) = []
    go (ExistU _ (ts, _) (rs, _)) = concatMap go ts ++ concatMap (go . snd) rs
    go (ForallU _ t) = go t
    go (FunU ts t) = concatMap go (t : ts)
    go (AppU t ts) = concatMap go (t : ts)
    go (NamU _ _ ps rs) = concatMap go ps ++ concatMap (go . snd) rs
    go (EffectU _ t) = go t
    go (OptionalU t) = go t
    go (NatLitU _) = []
    go (NatAddU a b) = go a ++ go b
    go (NatMulU a b) = go a ++ go b
    go (NatSubU a b) = go a ++ go b
    go (NatDivU a b) = go a ++ go b
    go NatVoidU = []
    go (StrLitU _) = []
    go (StrConcatU a b) = go a ++ go b
    go StrVoidU = []
    go RecEmptyU = []
    go (RecExtendU _ a b) = go a ++ go b
    go (RecUnionU a b) = go a ++ go b
    go (RecDiffU a _) = go a
    go (RecIntersectU a b) = go a ++ go b
    go (RecRestrictU a b) = go a ++ go b
    go (RecDiffListU a b) = go a ++ go b
    go RecVoidU = []
    go (ListLitU es) = concatMap go es
    go (ListAppU a b) = go a ++ go b
    go ListVoidU = []
    go SetEmptyU = []
    go (SetLitU es) = concatMap go es
    go (SetUnionU a b) = go a ++ go b
    go (SetInterU a b) = go a ++ go b
    go (SetDiffU a b) = go a ++ go b
    go SetVoidU = []
    go (KeysU r) = go r
    go (ListToSetU l) = go l
    go (SizeU c) = go c
    go (ProjectFieldU r f) = go r ++ go f
    go (RecSingletonU k v) = go k ++ go v
    go (LabeledU _ t) = go t

collectFixedNames :: Set.Set TVar -> TypeU -> Set.Set Text
collectFixedNames generics = go
  where
    go (VarU v)
      | Set.member v generics = Set.empty
      | otherwise = Set.singleton (unTVar v)
    go (NatVarU _) = Set.empty
    go (ExistU _ (ts, _) (rs, _)) = Set.unions (map go ts ++ map (go . snd) rs)
    go (ForallU _ t) = go t
    go (FunU ts t) = Set.unions $ map go (t : ts)
    go (AppU t ts) = Set.unions $ map go (t : ts)
    go (NamU _ (TV n) ps rs) =
      Set.insert n $ Set.unions (map go ps ++ map (go . snd) rs)
    go (EffectU _ t) = go t
    go (OptionalU t) = go t
    go (NatLitU _) = Set.empty
    go (NatAddU a b) = Set.union (go a) (go b)
    go (NatMulU a b) = Set.union (go a) (go b)
    go (NatSubU a b) = Set.union (go a) (go b)
    go (NatDivU a b) = Set.union (go a) (go b)
    go NatVoidU = Set.empty
    go (StrVarU _) = Set.empty
    go (StrLitU _) = Set.empty
    go (StrConcatU a b) = Set.union (go a) (go b)
    go StrVoidU = Set.empty
    go (RecVarU _) = Set.empty
    go RecEmptyU = Set.empty
    go (RecExtendU _ a b) = Set.union (go a) (go b)
    go (RecUnionU a b) = Set.union (go a) (go b)
    go (RecDiffU a _) = go a
    go (RecIntersectU a b) = Set.union (go a) (go b)
    go (RecRestrictU a b) = Set.union (go a) (go b)
    go (RecDiffListU a b) = Set.union (go a) (go b)
    go RecVoidU = Set.empty
    go (ListVarU _) = Set.empty
    go (ListLitU es) = Set.unions (map go es)
    go (ListAppU a b) = Set.union (go a) (go b)
    go ListVoidU = Set.empty
    go (SetVarU _) = Set.empty
    go SetEmptyU = Set.empty
    go (SetLitU es) = Set.unions (map go es)
    go (SetUnionU a b) = Set.union (go a) (go b)
    go (SetInterU a b) = Set.union (go a) (go b)
    go (SetDiffU a b) = Set.union (go a) (go b)
    go SetVoidU = Set.empty
    go (KeysU r) = go r
    go (ListToSetU l) = go l
    go (SizeU c) = go c
    go (ProjectFieldU r f) = Set.union (go r) (go f)
    go (RecSingletonU k v) = Set.union (go k) (go v)
    go (LabeledU _ t) = go t

applyVarRenaming :: Map.Map TVar TVar -> TypeU -> TypeU
applyVarRenaming m = go
  where
    ren v = Map.findWithDefault v v m
    go (VarU v) = VarU (ren v)
    go (NatVarU v) = NatVarU (ren v)
    go (ExistU v (ts, tc) (rs, rc)) =
      ExistU (ren v) (map go ts, tc) ([(k, go t) | (k, t) <- rs], rc)
    go (ForallU v t) = ForallU (ren v) (go t)
    go (FunU ts t) = FunU (map go ts) (go t)
    go (AppU t ts) = AppU (go t) (map go ts)
    go (NamU n o ps rs) = NamU n o (map go ps) [(k, go t) | (k, t) <- rs]
    go (EffectU effs t) = EffectU effs (go t)
    go (OptionalU t) = OptionalU (go t)
    go t@(NatLitU _) = t
    go (NatAddU a b) = NatAddU (go a) (go b)
    go (NatMulU a b) = NatMulU (go a) (go b)
    go (NatSubU a b) = NatSubU (go a) (go b)
    go (NatDivU a b) = NatDivU (go a) (go b)
    go t@NatVoidU = t
    -- StrVarU and RecVarU must be renamed by the same map as VarU so
    -- references to a kinded type-level variable stay in sync after
    -- renaming. Treating them as inert was a bug that caused
    -- Holder (r + {x=Int}) signatures to lose the connection between
    -- the input r and the output r after cleanTypeName.
    go (StrVarU v) = StrVarU (ren v)
    go t@(StrLitU _) = t
    go (StrConcatU a b) = StrConcatU (go a) (go b)
    go t@StrVoidU = t
    go (RecVarU v) = RecVarU (ren v)
    go t@RecEmptyU = t
    go (RecExtendU k a b) = RecExtendU k (go a) (go b)
    go (RecUnionU a b) = RecUnionU (go a) (go b)
    go (RecDiffU a ks) = RecDiffU (go a) ks
    go (RecIntersectU a b) = RecIntersectU (go a) (go b)
    go (RecRestrictU a b) = RecRestrictU (go a) (go b)
    go (RecDiffListU a b) = RecDiffListU (go a) (go b)
    go t@RecVoidU = t
    go (ListVarU v) = ListVarU (ren v)
    go (ListLitU es) = ListLitU (map go es)
    go (ListAppU a b) = ListAppU (go a) (go b)
    go t@ListVoidU = t
    go (SetVarU v) = SetVarU (ren v)
    go t@SetEmptyU = t
    go (SetLitU es) = SetLitU (map go es)
    go (SetUnionU a b) = SetUnionU (go a) (go b)
    go (SetInterU a b) = SetInterU (go a) (go b)
    go (SetDiffU a b) = SetDiffU (go a) (go b)
    go t@SetVoidU = t
    go (KeysU r) = KeysU (go r)
    go (ListToSetU l) = ListToSetU (go l)
    go (SizeU c) = SizeU (go c)
    go (ProjectFieldU r f) = ProjectFieldU (go r) (go f)
    go (RecSingletonU k v) = RecSingletonU (go k) (go v)
    go (LabeledU n t) = LabeledU n (go t)

-- | User-facing constraint display for diagnostics. Strips internal
-- rename suffixes (e.g. @r\@r0@ -> @r@) by routing through
-- 'prettyTypeU' on each TypeU sub-term, and adds parentheses around
-- compound arguments so the rendered form can be copy-pasted directly
-- into a function signature's @=>@ clause. Mirrors the structure of
-- the 'Pretty' instance in 'Morloc.Namespace.Type' but produces a
-- copy-paste-ready surface.
prettyConstraint :: Constraint -> MDoc
prettyConstraint (Constraint cls ts) = pretty cls <+> hsep (map argDoc ts)
prettyConstraint (CMember a s) = "Member" <+> argDoc a <+> argDoc s
prettyConstraint (CSubset a b) = "Subset" <+> argDoc a <+> argDoc b
prettyConstraint (CDisjoint a b) = "Disjoint" <+> argDoc a <+> argDoc b

-- | Render a constraint argument: atomic types (single variables or
-- literals) appear bare; compound types (applications, operators)
-- get parenthesised so the output reads as one would write the
-- constraint in a signature.
argDoc :: TypeU -> MDoc
argDoc t = if isAtomicType t then prettyTypeU t else parens (prettyTypeU t)

isAtomicType :: TypeU -> Bool
isAtomicType (VarU _) = True
isAtomicType (NatVarU _) = True
isAtomicType (StrVarU _) = True
isAtomicType (RecVarU _) = True
isAtomicType (ListVarU _) = True
isAtomicType (SetVarU _) = True
isAtomicType (NatLitU _) = True
isAtomicType (StrLitU _) = True
isAtomicType (ListLitU _) = True
isAtomicType (SetLitU _) = True
isAtomicType RecEmptyU = True
isAtomicType (RecExtendU _ _ _) = True  -- {f=a, ...} is bracket-delimited
isAtomicType _ = False

-- | User-facing type display: clean variable names, no forall, no angle brackets.
prettyTypeU :: TypeU -> MDoc
prettyTypeU = renderClean . cleanTypeName
  where
    renderClean t0 =
      let (_, body) = unqualify t0
      in f True body

    tv (TV v) = pretty (MT.takeWhile (/= '@') v)

    f _ (VarU v) = tv v
    f _ (NatVarU v) = tv v
    f _ (ExistU v ([], _) ([], _)) = tv v
    f _ (AppU (VarU (TV "List")) [t]) = "[" <> f True t <> "]"
    f _ (AppU (VarU (TV "Tuple2")) ts) = encloseSep "(" ")" ", " (map (f True) ts)
    f _ (AppU (VarU (TV "Tuple3")) ts) = encloseSep "(" ")" ", " (map (f True) ts)
    f _ (AppU (VarU (TV "Tuple4")) ts) = encloseSep "(" ")" ", " (map (f True) ts)
    f _ (AppU (VarU (TV "Tuple5")) ts) = encloseSep "(" ")" ", " (map (f True) ts)
    f _ (AppU (VarU (TV "Tuple6")) ts) = encloseSep "(" ")" ", " (map (f True) ts)
    f _ (AppU (VarU (TV "Tuple7")) ts) = encloseSep "(" ")" ", " (map (f True) ts)
    f _ (AppU (VarU (TV "Tuple8")) ts) = encloseSep "(" ")" ", " (map (f True) ts)
    f _ (EffectU effs t) =
      let labels = resolveEffectSet effs
       in if Set.null labels
            then "{" <> f True t <> "}"
            else "<" <> hcat (punctuate "," (map pretty (Set.toList labels))) <> ">" <+> f False t
    f _ (OptionalU t) = "?" <> f False t
    f _ (NatLitU n) = pretty n
    f _ (NatAddU a b) = "(" <> f True a <+> "+" <+> f True b <> ")"
    f _ (NatMulU a b) = "(" <> f True a <+> "*" <+> f True b <> ")"
    f _ (NatSubU a b) = "(" <> f True a <+> "-" <+> f True b <> ")"
    f _ (NatDivU a b) = "(" <> f True a <+> "/" <+> f True b <> ")"
    f _ NatVoidU = "_"
    f _ (StrVarU v) = tv v
    f _ (StrLitU s) = dquotes (pretty s)
    f _ (StrConcatU a b) = "(" <> f True a <+> "+" <+> f True b <> ")"
    f _ StrVoidU = "_"
    f _ (RecVarU v) = tv v
    f _ RecEmptyU = "{}"
    f _ rec@(RecExtendU _ _ _)
      | (fields, RecEmptyU) <- collectExtends rec =
          braces (hcat (punctuate ", " [pretty k <> "=" <> f False t | (k, t) <- fields]))
      | (fields, tl) <- collectExtends rec =
          "(" <> f True tl <+> hsep ["+" <+> pretty k <> "=" <> f False t | (k, t) <- fields] <> ")"
    f _ (RecUnionU a b) = "(" <> f True a <+> "+" <+> f True b <> ")"
    f _ (RecDiffU a ks) = "(" <> f True a <+> "-" <+> braces (hcat (punctuate "," (map pretty ks))) <> ")"
    f _ (RecIntersectU a b) = "(" <> f True a <+> "&" <+> f True b <> ")"
    f _ (RecRestrictU a b) = "(" <> f True a <+> "#" <+> f True b <> ")"
    f _ (RecDiffListU a b) = "(" <> f True a <+> "-" <+> f True b <> ")"
    f _ RecVoidU = "_"
    f _ (ListVarU v) = tv v
    f _ (ListLitU es) = "[" <> hcat (punctuate ", " (map (f True) es)) <> "]"
    f _ (ListAppU a b) = "(" <> f True a <+> "+" <+> f True b <> ")"
    f _ ListVoidU = "_"
    f _ (SetVarU v) = tv v
    f _ SetEmptyU = "{}"
    f _ (SetLitU es) = "{" <> hcat (punctuate ", " (map (f True) es)) <> "}"
    f _ (SetUnionU a b) = "(" <> f True a <+> "+" <+> f True b <> ")"
    f _ (SetInterU a b) = "(" <> f True a <+> "&" <+> f True b <> ")"
    f _ (SetDiffU a b) = "(" <> f True a <+> "-" <+> f True b <> ")"
    f _ SetVoidU = "_"
    f _ (KeysU r) = "Keys" <+> f False r
    f _ (ListToSetU l) = "ListToSet" <+> f False l
    f _ (SizeU c) = "Size" <+> f False c
    f _ (ProjectFieldU r fld) = f False r <> "." <> f False fld
    f _ (RecSingletonU k v) = "Singleton" <+> f False k <+> f False v
    f _ (LabeledU (TV n) t) = pretty n <> ":" <> f False t
    f False t = parens (f True t)
    f _ (ExistU v (ts, _) (rs, _)) =
      tv v
        <+> list (map (f False) ts)
        <+> list [tupled [pretty k, f True t] | (k, t) <- rs]
    f _ (FunU [] t) = "() -> " <> f False t
    f _ (FunU ts t) = hsep $ punctuate " ->" (map (f False) (ts <> [t]))
    f _ (ForallU _ t) = f True t
    f _ (AppU t ts) = hsep $ map (f False) (t : ts)
    f _ (NamU _ n ps _) =
      let params = if null ps
                   then mempty
                   else space <> hsep (map (f False) ps)
      in pretty n <> params

tvarname :: Gamma -> Text -> (Gamma, TVar)
tvarname g prefix =
  let i = gammaCounter g
   in (g {gammaCounter = i + 1}, TV (prefix <> MT.pack (show i)))

evarname :: Gamma -> Text -> (Gamma, EVar)
evarname g prefix =
  let i = gammaCounter g
   in (g {gammaCounter = i + 1}, EV (prefix <> "@@" <> MT.pack (show i)))

-- debugging -------------------

enter :: MDoc -> MorlocMonad ()
enter d = do
  depth <- MM.incDepth
  insetSay $ "--" <> pretty depth <> "-->" <+> d

insetSay :: MDoc -> MorlocMonad ()
insetSay d = do
  MM.sayVVV $ " :" <+> d

seeType :: TypeU -> MorlocMonad ()
seeType t = insetSay $ pretty t

leave :: MDoc -> MorlocMonad ()
leave d = do
  depth <- MM.decDepth
  insetSay $ "<--" <> pretty (depth + 1) <> "--" <+> d

seeGamma :: Gamma -> MorlocMonad ()
seeGamma g = MM.sayVVV $ nest 4 $ "Gamma:" <> line <> vsep (map pretty (gammaContextList g))

peak :: (Foldable f) => ExprS g f c -> MorlocMonad ()
peak = insetSay . pretty

peakGen :: (Foldable f) => AnnoS g f c -> MorlocMonad ()
peakGen = insetSay . pretty
