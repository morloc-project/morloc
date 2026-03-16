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
  , cleanTypeName
  , prettyTypeU
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
import qualified Morloc.TypeEval as TE

qualify :: [TVar] -> TypeU -> TypeU
qualify vs t = foldr ForallU t vs

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

-- | Apply a context to a type (See Dunfield Figure 8).
instance Applicable TypeU where
  -- [G]a = a
  apply g (VarU v) =
    -- FIXME: very wrong - only works because of my renaming scheme
    case lookupU v g of
      (Just t') -> t'
      Nothing -> VarU v
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

instance Applicable EType where
  apply g e =
    e
      { etype = apply g (etype e)
      , econs = Set.map (applyConstraint g) (econs e)
      }
    where
      applyConstraint g' (Constraint cls ts) = Constraint cls (map (apply g') ts)

instance Applicable Gamma where
  apply g1 g2 =
    g2
      { gammaContext = IntMap.map f (gammaContext g2)
      , gammaSolved = Map.map (apply g1) (gammaSolved g2)
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
isSubtypeOf2 scope a b = case subtype scope a b (Gamma 0 0 IntMap.empty Map.empty Map.empty []) of
  (Left _) -> False
  (Right _) -> True

subtypeEvaluated :: Scope -> TypeU -> TypeU -> Gamma -> Either MDoc Gamma
subtypeEvaluated scope t1 t2 g =
  case (TE.reduceType scope t1, TE.reduceType scope t2) of
    (Just t1', _) -> subtype scope t1' t2 g
    (_, Just t2') -> subtype scope t1 t2' g
    (_, _) -> Left $ "Cannot compare types" <+> pretty t1 <+> "and" <+> pretty t2

subtypeError :: TypeU -> TypeU -> MDoc -> Either MDoc a
subtypeError t1 t2 msg =
  Left $
    "Subtype error:" <+> msg
      <> "\n  "
      <> prettyTypeU t1 <+> "<:" <+> prettyTypeU t2

-- Nat expression helpers for SOP-based comparison
isNatExpr :: TypeU -> Bool
isNatExpr (NatLitU _) = True
isNatExpr (NatAddU _ _) = True
isNatExpr (NatMulU _ _) = True
isNatExpr (NatSubU _ _) = True
isNatExpr (NatDivU _ _) = True
isNatExpr _ = False

typeUToNatExpr :: TypeU -> Maybe NS.NatExpr
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
natExprToTypeU (NS.NatVar v) = VarU v
natExprToTypeU (NS.NatAdd a b) = NatAddU (natExprToTypeU a) (natExprToTypeU b)
natExprToTypeU (NS.NatMul a b) = NatMulU (natExprToTypeU a) (natExprToTypeU b)
natExprToTypeU (NS.NatSub a b) = NatSubU (natExprToTypeU a) (natExprToTypeU b)
natExprToTypeU (NS.NatDiv a b) = NatDivU (natExprToTypeU a) (natExprToTypeU b)

applyNatSolutions :: Map.Map TVar NS.NatExpr -> Gamma -> Either MDoc Gamma
applyNatSolutions subs g0 = foldM applySub g0 (Map.toList subs)
  where
    applySub g (v, ne) =
      let t = natExprToTypeU ne
      in case solveExist v t g of
           Right (Just g') -> Right g'
           Right Nothing -> Right g
           Left err -> Left err

-- | type 1 is more polymorphic than type 2 (Dunfield Figure 9)
subtype :: Scope -> TypeU -> TypeU -> Gamma -> Either MDoc Gamma
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
subtype scope (EffectU e1 t1) (EffectU e2 t2) g
  | effectSubsetOf e1 e2 = subtype scope t1 t2 g
  | otherwise = subtype scope t1 t2 g -- permissive for now: EffectVar not yet solved
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
  (ps1', ps2') <- case (pc1, pc2, compare (length ps1) (length ps2)) of
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
  (g2, rs1', rs2') <- case (rc1, rc2, Set.isSubsetOf keyset1 keyset2, Set.isSubsetOf keyset2 keyset1) of
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

  -- define new types to insert
  let taExpanded = ExistU v1 (ps1', pc1) (rs1', rc1)
  let tbExpanded = ExistU v2 (ps2', pc1) (rs2', rc1)

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
    Just t  -> subtype scope ta t g1
    Nothing -> solveExist v ta g1 >>= maybe (return g1) return

--  g1 |- t
-- ----------------------------------------- instLSolve
--  g1,Ea,g2 |- Ea <=: t -| g1,Ea=t,g2
instantiate scope (ExistU v ([], _) ([], _)) tb g1 =
  case lookupU v g1 of
    Just t  -> subtype scope t tb g1
    Nothing -> solveExist v tb g1 >>= maybe (return g1) return

instantiate _ ta tb _ = subtypeError ta tb "Unexpected types"

solve :: TVar -> TypeU -> Either MDoc GammaIndex
solve v t
  | occursIn v t =
      Left $ "Infinite recursion, cannot substitute" <+> pretty v <+> "into type" <+> pretty t
  | otherwise = Right (SolvedG v t)
  where
    occursIn :: TVar -> TypeU -> Bool
    occursIn v' (VarU v'') = v' == v''
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
    f (ExistU v (ts, tc) (ks, kc)) setTypes1 (SelectorKey s ss) =
      let (ks', setTypes2) = foldr subKey (ks, setTypes1) (s : ss)
       in (ExistU v (ts, tc) (ks', kc), setTypes2)
    f (NamU o v ps ks) setTypes1 (SelectorKey s ss) =
      let (ks', setTypes2) = foldr subKey (ks, setTypes1) (s : ss)
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
rename g0 (ForallU v@(TV s) t0) =
  let (g1, v') = tvarname g0 (s <> "___q")
      (g2, t1) = rename g1 t0
      t2 = substituteTVar v (VarU v') t1
   in (g2, ForallU v' t2)
-- Unless I add N-rank types, foralls can only be on top, so no need to recurse.
rename g t = (g, t)

{- | Rename all generic type variables (ForallU-bound and ExistU) to clean
letters from a lazy pool: a, b, c, ..., z, a1, b1, ..., z1, a2, ...
Avoids names already used by concrete types in the expression.
-}
cleanTypeName :: TypeU -> TypeU
cleanTypeName t0 =
  let (vs, body) = unqualify t0
      evs = collectExistVars body
      allGeneric = nub (vs ++ evs)
      fixed = collectFixedNames (Set.fromList allGeneric) body
      pool = filter (\(TV n) -> Set.notMember n fixed) letterPool
      renameMap = Map.fromList (zip allGeneric pool)
      renamedBody = applyVarRenaming renameMap body
      renamedVs = map (\v -> Map.findWithDefault v v renameMap) vs
   in simplifyNats $ foldr ForallU renamedBody renamedVs

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

collectFixedNames :: Set.Set TVar -> TypeU -> Set.Set Text
collectFixedNames generics = go
  where
    go (VarU v)
      | Set.member v generics = Set.empty
      | otherwise = Set.singleton (unTVar v)
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

applyVarRenaming :: Map.Map TVar TVar -> TypeU -> TypeU
applyVarRenaming m = go
  where
    ren v = Map.findWithDefault v v m
    go (VarU v) = VarU (ren v)
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

prettyTypeU :: TypeU -> MDoc
prettyTypeU = pretty . cleanTypeName

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
