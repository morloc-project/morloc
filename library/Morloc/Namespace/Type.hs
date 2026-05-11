{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.Namespace.Type
Description : Type system types and partial order logic
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Core type representations. 'Type' is the ground type (no quantifiers) used
after type erasure. 'TypeU' is the full type with existentials ('ExistU') and
universals ('ForallU'), used during typechecking and in type signatures.
'EType' extends 'TypeU' with typeclass constraints and documentation.

A partial order is defined on 'TypeU' via "Data.PartialOrd" where @t1 <= t2@
means t1 is at least as specific as t2 (t1 is a subtype of t2).
-}
module Morloc.Namespace.Type
  ( -- * Types
    NamType (..)
  , Type (..)
    -- Pattern synonyms for migrated kind constructors are bundled with
    -- 'TypeU' so importers using @TypeU (..)@ pick them up as if they
    -- were real constructors.
  , TypeU
      ( ..
      , NatLitU
      , NatAddU
      , NatSubU
      , NatMulU
      , NatDivU
      , StrLitU
      , StrConcatU
      , ListLitU
      , ListAppU
      , SetEmptyU
      , SetLitU
      , SetUnionU
      , SetInterU
      , SetDiffU
      , RecEmptyU
      , RecExtendU
      , RecUnionU
      , RecIntersectU
      , RecRestrictU
      , RecDiffU
      , RecDiffListU
      , KeysU
      , ListToSetU
      , SizeU
      , ProjectFieldU
      , RecSingletonU
      )
  , OpenOrClosed (..)
  , OpTag (..)
  , TyLit (..)
  , extractKey
  , collectExtends
  , type2typeu
  , EType (..)
  , unresolvedType2type

    -- * Smart constructors for kind operators
  , opArity
  , mkOp
  , mkNatAdd
  , mkNatSub
  , mkNatMul
  , mkNatDiv
  , mkStrConcat
  , mkRecUnion
  , mkRecIntersect
  , mkRecRestrict
  , mkRecDiffList
  , mkRecSingleton
  , mkListApp
  , mkSetUnion
  , mkSetInter
  , mkSetDiff
  , mkKeys
  , mkListToSet
  , mkSize
  , mkProjectField

    -- * Effect types
  , EffectLabel
  , EffectSet (..)
  , resolveEffectSet
  , emptyEffectSet
  , ioEffectSet
  , effectSubsetOf
  , effectSetHasVar

    -- * Docstring related types
  , CliOpt (..)
  , ArgDoc (..)
  , ArgDocVars (..)
  , ExprTypeE (..)

    -- * Scope
  , Scope

    -- * Type extensions
  , Constraint (..)

    -- * Predicates
  , containsUnk

    -- * Typeclasses
  , Typelike (..)

    -- * kludge
  , newVariable

    -- * Partial order logic
  , isSubtypeOf
  , equivalent
  , mostGeneral
  , mostSpecific
  , mostSpecificSubtypes
  , substituteFirst
  , findFirst
  ) where

import qualified Data.List as DL
import Data.Map.Strict (Map)
import qualified Data.PartialOrd as P
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as DT
import Morloc.Data.Doc
import Morloc.Namespace.Prim

---- Effect types

-- | A named effect label (e.g., "IO", "Random", "Error")
type EffectLabel = Text

-- | An effect set for use during typechecking. May contain concrete labels,
-- unsolved effect variables, or unions of effect sets.
data EffectSet
  = EffectSet (Set.Set EffectLabel)
  | EffectVar TVar
  | EffectUnion EffectSet EffectSet
  deriving (Show, Ord, Eq)

-- | Resolve an effect set to concrete labels. Unsolved variables resolve to empty.
resolveEffectSet :: EffectSet -> Set.Set EffectLabel
resolveEffectSet (EffectSet labels) = labels
resolveEffectSet (EffectVar _) = Set.empty
resolveEffectSet (EffectUnion a b) = Set.union (resolveEffectSet a) (resolveEffectSet b)

-- | An empty effect set (no effects)
emptyEffectSet :: EffectSet
emptyEffectSet = EffectSet Set.empty

-- | An IO effect set
ioEffectSet :: EffectSet
ioEffectSet = EffectSet (Set.singleton "IO")

-- | Check if one effect set is a subset of another (resolved labels).
-- Unsolved EffectVar resolves to empty, so EffectVar is a subset of everything.
effectSubsetOf :: EffectSet -> EffectSet -> Bool
effectSubsetOf e1 e2 = Set.isSubsetOf (resolveEffectSet e1) (resolveEffectSet e2)

-- | Does an effect set mention any unsolved EffectVar? Subtyping rules
-- that would otherwise reject a non-subset relation may need to defer
-- judgement until the variable is solved.
effectSetHasVar :: EffectSet -> Bool
effectSetHasVar (EffectVar _) = True
effectSetHasVar (EffectSet _) = False
effectSetHasVar (EffectUnion a b) = effectSetHasVar a || effectSetHasVar b

---- Type definitions

{- | Scope maps each type name to its definitions: the type parameters, the
body type, documentation, and whether the definition is terminal (won't be
expanded further during type resolution).
-}
type Scope =
  Map
    TVar
    [ ( [Either (TVar, Kind) TypeU] -- type parameters (generic for left, specific for right)
      , TypeU
      , ArgDoc
      , Bool -- True if this is a "terminal" type (won't be reduced further)
      )
    ]

-- | Flavors of named (keyed) types
data NamType
  = -- | Structural record with named fields
    NamRecord
  | -- | Nominal object type
    NamObject
  | -- | Tabular type (columns as fields)
    NamTable
  deriving (Show, Ord, Eq)

{- | Ground type with no quantifiers. Produced after type erasure and used in
code generation where all type variables have been resolved.
-}
data Type
  = UnkT TVar
  | VarT TVar
  | FunT [Type] Type
  | AppT Type [Type]
  | NamT NamType TVar [Type] [(Key, Type)]
  | EffectT (Set.Set EffectLabel) Type
  | OptionalT Type
  | NatLitT Integer
  | NatAddT Type Type
  | NatMulT Type Type
  | NatSubT Type Type
  | NatDivT Type Type
  | NatVoidT  -- ^ Erased phantom Nat slot. Distinct from NatLitT 0
              -- (which is a real empty-dim measurement). See note on
              -- NatVoidU in TypeU below.
  | StrLitT Text -- ^ Type-level Str literal at the ground level
  | StrConcatT Type Type -- ^ Type-level Str concatenation at the ground level
  | StrVoidT -- ^ Erased phantom Str slot. Mirrors NatVoidT.
  deriving (Show, Ord, Eq)

data OpenOrClosed = Open | Closed
  deriving (Show, Ord, Eq)

-- | Tags for kind-arithmetic operators carried by 'OpU' on 'TypeU'.
-- Arity is fixed per tag (see 'opArity'); smart constructors enforce it
-- at construction. Introduced in the kind-system cleanup; coexists with
-- the per-kind 'NatAddU'/'StrConcatU'/etc. constructors until migration
-- finishes.
data OpTag
  = OpNatAdd | OpNatSub | OpNatMul | OpNatDiv     -- arity 2
  | OpStrConcat                                    -- arity 2
  | OpRecExtend                                    -- arity 3: [LitU (LStr k), value, rest]
  | OpRecUnion | OpRecIntersect | OpRecRestrict
  | OpRecDiffList | OpRecSingleton                 -- arity 2
  | OpListApp                                      -- arity 2
  | OpSetUnion | OpSetInter | OpSetDiff            -- arity 2
  | OpKeys | OpListToSet | OpSize                  -- arity 1
  | OpProjectField                                 -- arity 2
  deriving (Show, Ord, Eq)

-- | Kind-tagged type-level literal values carried by 'LitU' on 'TypeU'.
-- Subsumes 'NatLitU' / 'StrLitU' / 'RecEmptyU' + 'RecExtendU' chain /
-- 'ListLitU' / 'SetEmptyU' + 'SetLitU' under one constructor. Named with
-- the 'Ty' prefix to disambiguate from term-level 'Morloc.Namespace.Expr.Lit'.
data TyLit
  = LNat Integer              -- ^ Nat literal: 0, 1, 2, ...
  | LStr Text                 -- ^ Str literal: "x", "y", ...
  | LRec [(Text, TypeU)]      -- ^ Rec literal: flat list of (field, type)
  | LList [TypeU]             -- ^ List literal: element types in order
  | LSet [TypeU]              -- ^ Set literal: canonical-form element types
  deriving (Show, Ord, Eq)

{- | Full type with quantifiers. 'ExistU' represents existential variables
(solved during unification), 'ForallU' represents universally quantified
variables. This is the primary type representation during typechecking.
-}
data TypeU
  = VarU TVar
  | NatVarU TVar -- ^ Nat-kinded variable, never quantified by ForallU
  | ExistU
      TVar
      ([TypeU], OpenOrClosed)
      ([(Key, TypeU)], OpenOrClosed)
  | ForallU TVar TypeU
  | FunU [TypeU] TypeU
  | AppU TypeU [TypeU]
  | NamU NamType TVar [TypeU] [(Key, TypeU)]
  | EffectU EffectSet TypeU
  | OptionalU TypeU
  -- Nat-kind operators and literal are now pattern synonyms over 'OpU' /
  -- 'LitU' (see below).
  | NatVoidU  -- ^ Erased phantom Nat slot. Substituted in for missing
              -- Nat-kinded args (kind realignment in expandHeadOnly), or
              -- when a NatVar / NatAdd / unresolved variable is reduced
              -- to ground form during weaving. Distinct from `NatLitU 0`
              -- (a real empty-dim measurement) so downstream consumers
              -- (pretty printer, dim reification in Serial.hs, equality)
              -- can tell phantoms apart from real measurements.
  | StrVarU TVar -- ^ Str-kinded variable, never quantified by ForallU.
                 -- Mirrors NatVarU. See plans/tables/04-str-solver-scope.md.
  -- Str-kind literal and concat are now pattern synonyms over 'LitU' /
  -- 'OpU' (see below).
  | StrVoidU -- ^ Erased phantom Str slot. Mirrors NatVoidU.
  -- Rec-kinded constructs (Stage 3 of the tables refactor). See
  -- plans/tables/10-rec-solver-decidability.md.
  | RecVarU TVar -- ^ Rec-kinded row variable, never quantified by ForallU.
  -- All Rec operators (empty, extend, union, intersect, restrict,
  -- diff-list, diff-by-Text-list) are now pattern synonyms over 'LitU' /
  -- 'OpU' (see below).
  | RecVoidU -- ^ Erased phantom Rec slot. Mirrors NatVoidU / StrVoidU.
  -- List-kinded constructs (Stage 8 of the tables refactor). Lists are
  -- ordered, position-preserving sequences; equality is element-wise.
  -- Element kind is fixed at the carrier (e.g. KindList KindStr); the
  -- TypeU level does not track it.
  | ListVarU TVar -- ^ List-kinded variable, never quantified by ForallU.
                  -- Mirrors NatVarU / StrVarU / RecVarU.
  -- List-kind literal and append are now pattern synonyms over 'LitU' /
  -- 'OpU' (see below).
  | ListVoidU -- ^ Erased phantom List slot. Mirrors NatVoidU / RecVoidU.
  -- Set-kinded constructs (Stage 8 of the tables refactor). Sets are
  -- order/duplicate-insensitive; canonical form is sorted dedup.
  | SetVarU TVar -- ^ Set-kinded variable, never quantified by ForallU.
  -- Set-kind empty/literal and union/inter/diff are now pattern synonyms
  -- over 'LitU' / 'OpU' (see below).
  | SetVoidU -- ^ Erased phantom Set slot. Distinct from empty set literal.
  -- Cross-kind functions (KeysU, ListToSetU, SizeU, ProjectFieldU,
  -- RecSingletonU) are now pattern synonyms over 'OpU' (see below).
  -- Rec/List operators 'RecRestrictU' and 'RecDiffListU' are also pattern
  -- synonyms.
  -- Unified carriers (kind-system cleanup, Stage 1). 'OpU' subsumes every
  -- kind-arithmetic operator above (NatAdd/NatSub/.../KeysU/.../etc); 'LitU'
  -- subsumes every kind literal (NatLit/StrLit/RecEmpty+RecExtend/ListLit/
  -- SetLit). Both coexist with the per-kind constructors until migration
  -- finishes; arity for 'OpU' is fixed by 'opArity' and enforced by smart
  -- constructors ('mkNatAdd', ...).
  | OpU OpTag [TypeU]
  | LitU TyLit
  | LabeledU TVar TypeU -- ^ Transient: m:Int -> LabeledU (TV "m") Int, stripped in desugar
  deriving (Show, Ord, Eq)

-- Pattern synonyms for the migrated Nat-kind constructors. These are
-- drop-in replacements for the removed 'NatLitU' / 'NatAddU' / ...
-- constructors: every existing call site (in this module or any other)
-- continues to work in both pattern and expression position.
pattern NatLitU :: Integer -> TypeU
pattern NatLitU n = LitU (LNat n)

pattern NatAddU :: TypeU -> TypeU -> TypeU
pattern NatAddU a b = OpU OpNatAdd [a, b]

pattern NatSubU :: TypeU -> TypeU -> TypeU
pattern NatSubU a b = OpU OpNatSub [a, b]

pattern NatMulU :: TypeU -> TypeU -> TypeU
pattern NatMulU a b = OpU OpNatMul [a, b]

pattern NatDivU :: TypeU -> TypeU -> TypeU
pattern NatDivU a b = OpU OpNatDiv [a, b]

pattern StrLitU :: Text -> TypeU
pattern StrLitU s = LitU (LStr s)

pattern StrConcatU :: TypeU -> TypeU -> TypeU
pattern StrConcatU a b = OpU OpStrConcat [a, b]

pattern ListLitU :: [TypeU] -> TypeU
pattern ListLitU es = LitU (LList es)

pattern ListAppU :: TypeU -> TypeU -> TypeU
pattern ListAppU a b = OpU OpListApp [a, b]

pattern SetEmptyU :: TypeU
pattern SetEmptyU = LitU (LSet [])

pattern SetLitU :: [TypeU] -> TypeU
pattern SetLitU es = LitU (LSet es)

pattern SetUnionU :: TypeU -> TypeU -> TypeU
pattern SetUnionU a b = OpU OpSetUnion [a, b]

pattern SetInterU :: TypeU -> TypeU -> TypeU
pattern SetInterU a b = OpU OpSetInter [a, b]

pattern SetDiffU :: TypeU -> TypeU -> TypeU
pattern SetDiffU a b = OpU OpSetDiff [a, b]

pattern RecEmptyU :: TypeU
pattern RecEmptyU = LitU (LRec [])

-- Single-field Rec extension. The label is carried as a 'LitU (LStr k)'
-- in the first operand position so 'OpU OpRecExtend' has uniform shape
-- (no per-op data in 'OpTag'). The pattern synonym is bidirectional and
-- only matches when the label slot holds a literal Str.
pattern RecExtendU :: Text -> TypeU -> TypeU -> TypeU
pattern RecExtendU k v rest = OpU OpRecExtend [LitU (LStr k), v, rest]

pattern RecUnionU :: TypeU -> TypeU -> TypeU
pattern RecUnionU a b = OpU OpRecUnion [a, b]

pattern RecIntersectU :: TypeU -> TypeU -> TypeU
pattern RecIntersectU a b = OpU OpRecIntersect [a, b]

pattern RecRestrictU :: TypeU -> TypeU -> TypeU
pattern RecRestrictU a b = OpU OpRecRestrict [a, b]

pattern RecDiffListU :: TypeU -> TypeU -> TypeU
pattern RecDiffListU a b = OpU OpRecDiffList [a, b]

-- | Drop a fixed set of named fields from a record. The 'Text' name list
-- is encoded as a 'LitU (LList ...)' of 'LitU (LStr _)' so it folds into
-- the existing 'OpRecDiffList' op. The pattern only matches when every
-- list element is a literal Str.
pattern RecDiffU :: TypeU -> [Text] -> TypeU
pattern RecDiffU r ks <- OpU OpRecDiffList [r, (recDiffKeys -> Just ks)]
  where RecDiffU r ks = OpU OpRecDiffList [r, LitU (LList (map (LitU . LStr) ks))]

recDiffKeys :: TypeU -> Maybe [Text]
recDiffKeys (LitU (LList items)) = traverse extractStr items
  where
    extractStr (LitU (LStr s)) = Just s
    extractStr _ = Nothing
recDiffKeys _ = Nothing

pattern KeysU :: TypeU -> TypeU
pattern KeysU r = OpU OpKeys [r]

pattern ListToSetU :: TypeU -> TypeU
pattern ListToSetU l = OpU OpListToSet [l]

pattern SizeU :: TypeU -> TypeU
pattern SizeU c = OpU OpSize [c]

pattern ProjectFieldU :: TypeU -> TypeU -> TypeU
pattern ProjectFieldU r f = OpU OpProjectField [r, f]

pattern RecSingletonU :: TypeU -> TypeU -> TypeU
pattern RecSingletonU k v = OpU OpRecSingleton [k, v]

-- | The pattern synonyms above plus the kind-specific carriers
-- exhaustively cover 'TypeU'. GHC's exhaustiveness checker cannot infer
-- this on its own (especially because 'RecDiffU' uses a view pattern and
-- 'RecExtendU' requires a specific shape in slot 0), so we assert it
-- explicitly.
{-# COMPLETE
    VarU, NatVarU, ExistU, ForallU, FunU, AppU, NamU, EffectU, OptionalU,
    NatVoidU, StrVarU, StrVoidU, RecVarU, RecVoidU,
    ListVarU, ListVoidU, SetVarU, SetVoidU, LabeledU,
    NatLitU, NatAddU, NatSubU, NatMulU, NatDivU,
    StrLitU, StrConcatU,
    ListLitU, ListAppU,
    SetEmptyU, SetLitU, SetUnionU, SetInterU, SetDiffU,
    RecEmptyU, RecExtendU, RecUnionU, RecIntersectU,
    RecRestrictU, RecDiffListU, RecDiffU, RecSingletonU,
    KeysU, ListToSetU, SizeU, ProjectFieldU
  #-}

{- | Extended Type that may represent a language specific type as well as sets
of properties and constrains.
-}
data EType
  = EType
  { etype :: TypeU
  , econs :: Set.Set Constraint
  , edocs :: ArgDoc
  , enatLabels :: Map TVar Int -- ^ Nat var name -> argument position index (from m:Int syntax)
  }
  deriving (Show, Eq, Ord)

-- | Type-level constraints attached to a signature via @=>@.
--
-- - 'Constraint' is the existing typeclass form, e.g. @(Eq a) =>@.
-- - 'CMember' / 'CSubset' / 'CDisjoint' are Stage-9 generic primitives
--   over the new kinds. They are written with the same @=>@ syntax but
--   carry a fixed positional shape rather than a free class name.
data Constraint
  = Constraint ClassName [TypeU]
  | CMember TypeU TypeU       -- ^ Member a s :: a is an element of set s
  | CSubset TypeU TypeU       -- ^ Subset s1 s2 :: every element of s1 is in s2
  | CDisjoint TypeU TypeU     -- ^ Disjoint s1 s2 :: s1 and s2 share no elements
  deriving (Show, Eq, Ord)

-- a CLI option that takes an argument
data CliOpt
  = CliOptShort Char
  | CliOptLong Text
  | CliOptBoth Char Text
  deriving (Show, Ord, Eq)

data ArgDocVars = ArgDocVars
  { docLines :: [Text]
  , docName :: Maybe Text
  , docLiteral :: Maybe Bool
  , docUnroll :: Maybe Bool
  , docDefault :: Maybe Text
  , docMetavar :: Maybe Text
  , docArg :: Maybe CliOpt
  , docTrue :: Maybe CliOpt
  , docFalse :: Maybe CliOpt
  , docReturn :: Maybe Text
  }
  deriving (Show, Ord, Eq)

data ArgDoc
  = ArgDocRec ArgDocVars [(Key, ArgDocVars)]
  | ArgDocSig
      ArgDocVars
      [ArgDocVars]
      ArgDocVars
  | ArgDocAlias ArgDocVars
  deriving (Show, Ord, Eq)

-- Wraps all information stored in a type definition
data ExprTypeE = ExprTypeE
  { exprTypeConcreteForm :: Maybe (Lang, Bool)
  , exprTypeName :: TVar
  , exprTypeParams :: [Either (TVar, Kind) TypeU]
  , exprTypeType :: TypeU
  , exprTypeDoc :: ArgDoc
  }
  deriving (Show, Ord, Eq)

---- Typeclasses

class Typelike a where
  typeOf :: a -> Type

  free :: a -> Set.Set a

  substituteTVar :: TVar -> a -> a -> a

  nargs :: a -> Int
  nargs (typeOf -> FunT ts _) = length ts
  nargs _ = 0

  normalizeType :: a -> a

---- Typeclass instances

instance Defaultable ArgDocVars where
  defaultValue =
    ArgDocVars
      { docLines = []
      , docName = Nothing
      , docLiteral = Nothing
      , docUnroll = Nothing
      , docDefault = Nothing
      , docMetavar = Nothing
      , docArg = Nothing
      , docTrue = Nothing
      , docFalse = Nothing
      , docReturn = Nothing
      }

instance Typelike Type where
  typeOf = id

  substituteTVar v0 r0 t0 = sub t0
    where
      sub t@(UnkT _) = t
      sub t@(VarT v)
        | v0 == v = r0
        | otherwise = t
      sub (FunT ts t) = FunT (map sub ts) (sub t)
      sub (AppT v ts) = AppT (sub v) (map sub ts)
      sub (NamT r n ps es) = NamT r n ps [(k, sub t) | (k, t) <- es]
      sub (EffectT effs t) = EffectT effs (sub t)
      sub (OptionalT t) = OptionalT (sub t)
      sub t@(NatLitT _) = t
      sub (NatAddT a b) = NatAddT (sub a) (sub b)
      sub (NatMulT a b) = NatMulT (sub a) (sub b)
      sub (NatSubT a b) = NatSubT (sub a) (sub b)
      sub (NatDivT a b) = NatDivT (sub a) (sub b)
      sub t@NatVoidT = t
      sub t@(StrLitT _) = t
      sub (StrConcatT a b) = StrConcatT (sub a) (sub b)
      sub t@StrVoidT = t

  free (UnkT _) = Set.empty
  free v@(VarT _) = Set.singleton v
  free (FunT ts t) = Set.unions (map free (t : ts))
  free (AppT t ts) = Set.unions (map free (t : ts))
  free (NamT _ _ _ es) = Set.unions (map (free . snd) es)
  free (EffectT _ t) = free t
  free (OptionalT t) = free t
  free (NatLitT _) = Set.empty
  free (NatAddT a b) = Set.union (free a) (free b)
  free (NatMulT a b) = Set.union (free a) (free b)
  free (NatSubT a b) = Set.union (free a) (free b)
  free (NatDivT a b) = Set.union (free a) (free b)
  free NatVoidT = Set.empty
  free (StrLitT _) = Set.empty
  free (StrConcatT a b) = Set.union (free a) (free b)
  free StrVoidT = Set.empty

  normalizeType (FunT ts1 (FunT ts2 ft)) = normalizeType $ FunT (ts1 <> ts2) ft
  normalizeType (AppT t ts) = AppT (normalizeType t) (map normalizeType ts)
  normalizeType (NamT n v ds ks) = NamT n v (map normalizeType ds) (zip (map fst ks) (map (normalizeType . snd) ks))
  normalizeType (EffectT effs t) = EffectT effs (normalizeType t)
  normalizeType (OptionalT t) = OptionalT (normalizeType t)
  normalizeType (NatAddT a b) = NatAddT (normalizeType a) (normalizeType b)
  normalizeType (NatMulT a b) = NatMulT (normalizeType a) (normalizeType b)
  normalizeType (NatSubT a b) = NatSubT (normalizeType a) (normalizeType b)
  normalizeType (NatDivT a b) = NatDivT (normalizeType a) (normalizeType b)
  normalizeType (StrConcatT a b) = StrConcatT (normalizeType a) (normalizeType b)
  normalizeType t = t

instance Typelike TypeU where
  typeOf (VarU v) = VarT v
  typeOf (NatVarU _) = NatVoidT
  typeOf (ExistU _ (ps, _) (rs@(_ : _), _)) = NamT NamRecord (TV "Record") (map typeOf ps) (map (second typeOf) rs)
  typeOf (ExistU v _ _) = typeOf (ForallU v (VarU v))
  typeOf (ForallU v t) = substituteTVar v (UnkT v) (typeOf t)
  typeOf (FunU ts t) = FunT (map typeOf ts) (typeOf t)
  typeOf (AppU t ts) = AppT (typeOf t) (map typeOf ts)
  typeOf (NamU n o ps rs) = NamT n o (map typeOf ps) (zip (map fst rs) (map (typeOf . snd) rs))
  typeOf (EffectU effs t) = EffectT (resolveEffectSet effs) (typeOf t)
  typeOf (OptionalU t) = OptionalT (typeOf t)
  typeOf NatVoidU = NatVoidT
  typeOf (StrVarU _) = StrVoidT  -- free Str var erases to StrVoidT at ground level
  typeOf StrVoidU = StrVoidT
  -- Rec-kinded constructs collapse to NatVoidT when polymorphic (free row
  -- variable somewhere in the chain). A fully ground RecExtend chain
  -- terminating in RecEmptyU lowers to a NamT NamRecord so the runtime
  -- layer can read the column schema (used for Arrow Tables in
  -- nexus IO). See plans/tables/10-rec-solver-decidability.md.
  typeOf (RecVarU _) = NatVoidT
  typeOf r@(RecExtendU _ _ _) = case groundRecFields r of
    Just fs -> NamT NamRecord (TV "Rec") [] [(Key k, typeOf t) | (k, t) <- fs]
    Nothing -> NatVoidT
  typeOf RecVoidU = NatVoidT
  -- List- and Set-kinded constructs are entirely phantom at the ground
  -- level; they contribute no runtime type information. They erase to
  -- NatVoidT for the same reason Rec polymorphic forms do: the codegen
  -- layer doesn't have a List/Set ground type to map to.
  typeOf (ListVarU _) = NatVoidT
  typeOf ListVoidU = NatVoidT
  typeOf (SetVarU _) = NatVoidT
  typeOf SetVoidU = NatVoidT
  -- Cross-kind functions are now pattern synonyms over 'OpU'; their
  -- typeOf is dispatched via the unified 'OpU _ _' fallback below
  -- (erase to NatVoidT). The Nat / Str cases reduce to ground forms
  -- through the kind-specific OpU clauses.
  -- Unified carriers: behave identically to the per-kind constructors.
  -- Stage-1 coexistence; once per-kind constructors are removed these
  -- become the only path.
  typeOf (OpU OpNatAdd [a, b]) = NatAddT (typeOf a) (typeOf b)
  typeOf (OpU OpNatSub [a, b]) = NatSubT (typeOf a) (typeOf b)
  typeOf (OpU OpNatMul [a, b]) = NatMulT (typeOf a) (typeOf b)
  typeOf (OpU OpNatDiv [a, b]) = NatDivT (typeOf a) (typeOf b)
  typeOf (OpU OpStrConcat [a, b]) = StrConcatT (typeOf a) (typeOf b)
  typeOf (OpU _ _) = NatVoidT  -- Rec/List/Set/cross-kind ops erase to phantom
  typeOf (LitU (LNat n)) = NatLitT n
  typeOf (LitU (LStr s)) = StrLitT s
  typeOf (LitU (LRec fs)) = NamT NamRecord (TV "Rec") [] [(Key k, typeOf t) | (k, t) <- fs]
  typeOf (LitU (LList _)) = NatVoidT
  typeOf (LitU (LSet _)) = NatVoidT
  typeOf (LabeledU _ t) = typeOf t

  free v@(VarU _) = Set.singleton v
  free (NatVarU _) = Set.empty
  free v@(ExistU _ ([], _) (rs, _)) = Set.unions $ Set.singleton v : map (free . snd) rs
  free (ExistU v (ts, _) _) = Set.unions $ Set.singleton (AppU (VarU v) ts) : map free ts
  free (ForallU v t) = Set.delete (VarU v) (free t)
  free (FunU ts t) = Set.unions $ map free (t : ts)
  free (AppU t ts) = Set.unions $ map free (t : ts)
  free (NamU _ _ ps rs) = Set.unions $ map free (map snd rs <> ps)
  free (EffectU _ t) = free t
  free (OptionalU t) = free t
  free NatVoidU = Set.empty
  -- Str-kinded constructs are implicitly forall-quantified (like NatVarU);
  -- they contribute no free type variables. See plans/tables/04-str-solver-scope.md.
  free (StrVarU _) = Set.empty
  free StrVoidU = Set.empty
  free (RecVarU _) = Set.empty
  free RecVoidU = Set.empty
  -- List- and Set-kinded vars are implicitly forall-quantified (like Nat,
  -- Str, Rec vars); they contribute no free Type-kinded variables.
  free (ListVarU _) = Set.empty
  free ListVoidU = Set.empty
  free (SetVarU _) = Set.empty
  free SetVoidU = Set.empty
  -- Cross-kind functions are pattern synonyms over 'OpU'; their
  -- recursion is handled by the unified 'OpU _ args' clause below.
  -- Unified carriers: 'OpU' is uniform structural recursion across all
  -- operators (no per-op logic needed). Literals are inert except for
  -- recursive payloads in LRec/LList/LSet.
  free (OpU _ args) = Set.unions (map free args)
  free (LitU (LNat _)) = Set.empty
  free (LitU (LStr _)) = Set.empty
  free (LitU (LRec fs)) = Set.unions (map (free . snd) fs)
  free (LitU (LList es)) = Set.unions (map free es)
  free (LitU (LSet es)) = Set.unions (map free es)
  free (LabeledU _ t) = free t

  substituteTVar v (ForallU q r) t =
    if Set.member (VarU q) (free t)
      then
        let q' = newVariable r t
            r' = substituteTVar q (VarU q') r
         in ForallU q' (substituteTVar v r' t)
      else
        ForallU q (substituteTVar v r t)
  substituteTVar _ _ t@(NatVarU _) = t
  substituteTVar v0 r0 t0 = sub t0
    where
      sub t@(VarU v)
        | v0 == v = r0
        | otherwise = t
      sub t@(NatVarU _) = t
      sub (ExistU v (map sub -> ps, pc) (map (second sub) -> rs, rc)) = ExistU v (ps, pc) (rs, rc)
      sub (ForallU v t)
        | v0 == v = ForallU v t
        | otherwise = ForallU v (sub t)
      sub (FunU ts t) = FunU (map sub ts) (sub t)
      sub (AppU t ts) = AppU (sub t) (map sub ts)
      sub (NamU r n ps rs) = NamU r n (map sub ps) [(k, sub t) | (k, t) <- rs]
      sub (EffectU effs t) = EffectU effs (sub t)
      sub (OptionalU t) = OptionalU (sub t)
      sub t@NatVoidU = t
      -- Str-kinded constructs: substitution does not touch them (parallel to
      -- NatVarU). Concat recurses inside via the OpU clause below.
      sub t@(StrVarU _) = t
      sub t@StrVoidU = t
      -- Rec-kinded constructs: same pattern. Variables and the empty record
      -- are inert; operators recurse into their operands so type-level vars
      -- inside Rec field-types still see substitutions.
      sub t@(RecVarU _) = t
      sub t@RecVoidU = t
      -- List- and Set-kinded constructs: variables and empty/void are
      -- inert; operators recurse into their operands.
      sub t@(ListVarU _) = t
      sub t@ListVoidU = t
      sub t@(SetVarU _) = t
      sub t@SetVoidU = t
      -- Cross-kind functions are pattern synonyms over 'OpU'; their
      -- substitution is handled by the unified 'OpU op args' clause below.
      -- Unified carriers: uniform structural recursion. Lit payloads recurse
      -- through their type-carrying sub-parts.
      sub (OpU op args) = OpU op (map sub args)
      sub (LitU (LNat n)) = LitU (LNat n)
      sub (LitU (LStr s)) = LitU (LStr s)
      sub (LitU (LRec fs)) = LitU (LRec [(k, sub t) | (k, t) <- fs])
      sub (LitU (LList es)) = LitU (LList (map sub es))
      sub (LitU (LSet es)) = LitU (LSet (map sub es))
      sub (LabeledU n t) = LabeledU n (sub t)

  normalizeType (FunU ts1 (FunU ts2 ft)) = normalizeType $ FunU (ts1 <> ts2) ft
  normalizeType (AppU t ts) = AppU (normalizeType t) (map normalizeType ts)
  normalizeType (NamU n v ds ks) = NamU n v (map normalizeType ds) (zip (map fst ks) (map (normalizeType . snd) ks))
  normalizeType (ForallU v t) = ForallU v (normalizeType t)
  normalizeType (ExistU v (map normalizeType -> ps, pc) (map (second normalizeType) -> rs, rc)) = ExistU v (ps, pc) (rs, rc)
  normalizeType (EffectU effs t) = EffectU effs (normalizeType t)
  normalizeType (OptionalU t) = OptionalU (normalizeType t)
  normalizeType t@(NatVarU _) = t
  -- Unified carriers: recurse into payload so nested forms are normalized.
  normalizeType (OpU op args) = OpU op (map normalizeType args)
  normalizeType (LitU (LRec fs)) = LitU (LRec [(k, normalizeType t) | (k, t) <- fs])
  normalizeType (LitU (LList es)) = LitU (LList (map normalizeType es))
  normalizeType (LitU (LSet es)) = LitU (LSet (map normalizeType es))
  normalizeType t@(LitU _) = t
  normalizeType (LabeledU n t) = LabeledU n (normalizeType t)
  normalizeType t = t

----- Partial order logic

instance P.PartialOrd TypeU where
  (<=) (VarU v1) (VarU v2) = v1 == v2
  (<=) (NatVarU v1) (NatVarU v2) = v1 == v2
  (<=) (ExistU v1 (ts1, _) (rs1, _)) (ExistU v2 (ts2, _) (rs2, _)) =
    v1 == v2
      && length ts1 == length ts2
      && and (zipWith (P.<=) ts1 ts2)
      && and [maybe False (t1 P.<=) (lookup k rs2) | (k, t1) <- rs1]
  (<=) (ForallU v t1) t2
    | (P.==) (ForallU v t1) t2 = True
    | otherwise = (P.<=) (substituteFirst v t1 t2) t2
  (<=) (FunU (t11 : rs1) t12) (FunU (t21 : rs2) t22) = t11 P.<= t21 && FunU rs1 t12 P.<= FunU rs2 t22
  (<=) (FunU [] t12) (FunU [] t22) = t12 P.<= t22
  (<=) (AppU t1 (t11 : rs1)) (AppU t2 (t21 : rs2)) = t11 P.<= t21 && AppU t1 rs1 P.<= AppU t2 rs2
  (<=) (AppU t1 []) (AppU t2 []) = t1 P.<= t2
  (<=) (NamU o1 n1 ps1 ((k1, e1) : rs1)) (NamU o2 n2 ps2 es2) =
    case DL.partition ((== k1) . fst) es2 of
      ([(_, e2)], rs2) -> e1 P.<= e2 && NamU o1 n1 ps1 rs1 P.<= NamU o2 n2 ps2 rs2
      _ -> False
  (<=) (NamU o1 n1 ps1 []) (NamU o2 n2 ps2 []) =
    o1 == o2 && n1 == n2 && length ps1 == length ps2
  (<=) (EffectU e1 t1) (EffectU e2 t2) = e1 == e2 && t1 P.<= t2
  (<=) (OptionalU t1) (OptionalU t2) = t1 P.<= t2
  -- NatVoid is wildcard-compatible with any Nat: erased phantoms compare
  -- as equal to any real (or other erased) Nat slot. Nat-op and Nat-lit
  -- wildcards are now handled below via the unified-carrier rules.
  (<=) NatVoidU (NatVarU _) = True
  (<=) (NatVarU _) NatVoidU = True
  (<=) NatVoidU NatVoidU = True
  -- Unified-carrier subtyping: structural for matching tags; NatVoid
  -- wildcards extend to OpU Nat-ops and LitU LNat so the new forms are
  -- interchangeable with NatAddU/NatLitU/etc at the subtype level.
  (<=) (OpU op1 args1) (OpU op2 args2) =
    op1 == op2
      && length args1 == length args2
      && and (zipWith (P.<=) args1 args2)
  (<=) (LitU l1) (LitU l2) = l1 == l2
  (<=) NatVoidU (OpU op _) = isNatOpTag op
  (<=) (OpU op _) NatVoidU = isNatOpTag op
  (<=) NatVoidU (LitU (LNat _)) = True
  (<=) (LitU (LNat _)) NatVoidU = True
  (<=) (LabeledU _ t1) t2 = t1 P.<= t2
  (<=) t1 (LabeledU _ t2) = t1 P.<= t2
  (<=) _ _ = False

  (==) (ForallU v1 t1) (ForallU v2 t2) =
    if Set.member (VarU v1) (free t2)
      then
        let v = newVariable t1 t2
         in (P.==) (substituteTVar v1 (VarU v) t1) (substituteTVar v2 (VarU v) t2)
      else (P.==) t1 (substituteTVar v2 (VarU v1) t2)
  (==) a b = a == b

substituteFirst :: TVar -> TypeU -> TypeU -> TypeU
substituteFirst v t1 t2 = case findFirst v t1 t2 of
  (Just t) -> substituteTVar v t t1
  Nothing -> t1

findFirst :: TVar -> TypeU -> TypeU -> Maybe TypeU
findFirst v = f
  where
    f (VarU v') t2
      | v == v' = Just t2
      | otherwise = Nothing
    f (NatVarU _) _ = Nothing
    f (ForallU v1 t1) (ForallU v2 t2)
      | v == v1 = Nothing
      | otherwise = f t1 (substituteTVar v2 (VarU v1) t2)
    f (ForallU v1 t1) t2
      | v == v1 = Nothing
      | otherwise = f (substituteTVar v1 (VarU v1) t1) t2
    f (FunU ts1 t1) (FunU ts2 t2) =
      foldl firstOf Nothing (zipWith f (ts1 <> [t1]) (ts2 <> [t2]))
    f (AppU t1 ts1) (AppU t2 ts2) =
      foldl firstOf Nothing (zipWith f (t1 : ts1) (t2 : ts2))
    f (NamU o1 n1 ps1 ((k1, e1) : rs1)) (NamU o2 n2 ps2 es2) =
      case DL.partition ((== k1) . fst) es2 of
        ([(_, e2)], rs2) -> firstOf (f e1 e2) (f (NamU o1 n1 ps1 rs1) (NamU o2 n2 ps2 rs2))
        _ -> Nothing
    f (EffectU _ t1) (EffectU _ t2) = f t1 t2
    f (OptionalU t1) (OptionalU t2) = f t1 t2
    -- Unified-carrier matching: same op + same arity recurses pairwise.
    -- LRec recurses on field values when keys agree in order.
    f (OpU op1 args1) (OpU op2 args2)
      | op1 == op2 && length args1 == length args2 =
          foldl firstOf Nothing (zipWith f args1 args2)
      | otherwise = Nothing
    f (LitU (LRec fs1)) (LitU (LRec fs2))
      | map fst fs1 == map fst fs2 =
          foldl firstOf Nothing (zipWith f (map snd fs1) (map snd fs2))
      | otherwise = Nothing
    f (LitU (LList es1)) (LitU (LList es2))
      | length es1 == length es2 =
          foldl firstOf Nothing (zipWith f es1 es2)
      | otherwise = Nothing
    f (LitU (LSet es1)) (LitU (LSet es2))
      | length es1 == length es2 =
          foldl firstOf Nothing (zipWith f es1 es2)
      | otherwise = Nothing
    f (LabeledU _ t1) t2 = f t1 t2
    f t1 (LabeledU _ t2) = f t1 t2
    f _ _ = Nothing

    firstOf :: Maybe a -> Maybe a -> Maybe a
    firstOf (Just x) _ = Just x
    firstOf _ (Just x) = Just x
    firstOf _ _ = Nothing

-- | is t1 a generalization of t2?
isSubtypeOf :: TypeU -> TypeU -> Bool
isSubtypeOf t1 t2 = case P.compare t1 t2 of
  (Just x) -> x <= EQ
  _ -> False

equivalent :: TypeU -> TypeU -> Bool
equivalent t1 t2 = isSubtypeOf t1 t2 && isSubtypeOf t2 t1

-- | find the most specific subtypes
mostSpecificSubtypes :: TypeU -> [TypeU] -> [TypeU]
mostSpecificSubtypes t ts = mostSpecific $ filter (`isSubtypeOf` t) ts

-- | find all types that are not greater than any other type
mostGeneral :: [TypeU] -> [TypeU]
mostGeneral = P.minima

-- | find all types that are not less than any other type
mostSpecific :: [TypeU] -> [TypeU]
mostSpecific = P.maxima

---- Utility functions

-- | Walk a Rec extension chain, collecting (key, value) fields. Returns
-- Nothing if the chain does not terminate cleanly in RecEmptyU (e.g. ends
-- in a free RecVarU or contains an unsolved Rec operator). Used by the
-- TypeU -> Type lowering to preserve the column schema of ground Tables.
groundRecFields :: TypeU -> Maybe [(Text, TypeU)]
groundRecFields RecEmptyU = Just []
groundRecFields (RecExtendU k t rest) = ((k, t):) <$> groundRecFields rest
groundRecFields _ = Nothing

extractKey :: TypeU -> TVar
extractKey (VarU v) = v
extractKey (NatVarU v) = v
extractKey (ForallU _ t) = extractKey t
extractKey (AppU t _) = extractKey t
extractKey (NamU _ v _ _) = v
extractKey (ExistU v _ _) = v
extractKey (EffectU _ t) = extractKey t
extractKey (OptionalU t) = extractKey t
extractKey NatVoidU = TV "Nat"
extractKey (StrVarU _) = TV "Str"
extractKey StrVoidU = TV "Str"
extractKey (RecVarU _) = TV "Rec"
extractKey RecVoidU = TV "Rec"
extractKey (ListVarU _) = TV "List"
extractKey ListVoidU = TV "List"
extractKey (SetVarU _) = TV "Set"
extractKey SetVoidU = TV "Set"
extractKey (OpU op _) = opKeyTag op
extractKey (LitU (LNat _)) = TV "Nat"
extractKey (LitU (LStr _)) = TV "Str"
extractKey (LitU (LRec _)) = TV "Rec"
extractKey (LitU (LList _)) = TV "List"
extractKey (LitU (LSet _)) = TV "Set"
extractKey (LabeledU _ t) = extractKey t
extractKey t = error $ "Cannot currently handle functional type imports: " <> show t

-- | Result-kind tag for each operator, used by 'extractKey' for
-- instance-resolution lookup. Mirrors the per-kind constructor mapping
-- (NatAddU -> "Nat", KeysU -> "Set", etc).
opKeyTag :: OpTag -> TVar
opKeyTag OpNatAdd = TV "Nat"
opKeyTag OpNatSub = TV "Nat"
opKeyTag OpNatMul = TV "Nat"
opKeyTag OpNatDiv = TV "Nat"
opKeyTag OpStrConcat = TV "Str"
opKeyTag OpRecExtend = TV "Rec"
opKeyTag OpRecUnion = TV "Rec"
opKeyTag OpRecIntersect = TV "Rec"
opKeyTag OpRecRestrict = TV "Rec"
opKeyTag OpRecDiffList = TV "Rec"
opKeyTag OpRecSingleton = TV "Rec"
opKeyTag OpListApp = TV "List"
opKeyTag OpSetUnion = TV "Set"
opKeyTag OpSetInter = TV "Set"
opKeyTag OpSetDiff = TV "Set"
opKeyTag OpKeys = TV "Set"
opKeyTag OpListToSet = TV "Set"
opKeyTag OpSize = TV "Nat"
opKeyTag OpProjectField = TV "Type"

-- | Expected arity of a kind operator. The smart constructors below
-- enforce this at construction time so downstream code can rely on
-- well-formed 'OpU' nodes.
opArity :: OpTag -> Int
opArity OpKeys = 1
opArity OpListToSet = 1
opArity OpSize = 1
opArity OpRecExtend = 3
opArity _ = 2

-- | Whether an operator produces a Nat-kinded result. Used by the
-- partial-order 'NatVoidU' wildcard rules so erased phantoms remain
-- compatible with the unified-carrier Nat-op forms.
isNatOpTag :: OpTag -> Bool
isNatOpTag OpNatAdd = True
isNatOpTag OpNatSub = True
isNatOpTag OpNatMul = True
isNatOpTag OpNatDiv = True
isNatOpTag OpSize = True
isNatOpTag _ = False

-- | Generic smart constructor; validates arity. Production callers should
-- prefer the named helpers ('mkNatAdd', ...) for arity-correct-by-shape.
mkOp :: OpTag -> [TypeU] -> TypeU
mkOp op args
  | length args == opArity op = OpU op args
  | otherwise =
      error $ "mkOp: arity mismatch for " <> show op
           <> "; expected " <> show (opArity op)
           <> " got " <> show (length args)

-- Named smart constructors. Arity is encoded in the function signature.
mkNatAdd, mkNatSub, mkNatMul, mkNatDiv :: TypeU -> TypeU -> TypeU
mkNatAdd a b = OpU OpNatAdd [a, b]
mkNatSub a b = OpU OpNatSub [a, b]
mkNatMul a b = OpU OpNatMul [a, b]
mkNatDiv a b = OpU OpNatDiv [a, b]

mkStrConcat :: TypeU -> TypeU -> TypeU
mkStrConcat a b = OpU OpStrConcat [a, b]

mkRecUnion, mkRecIntersect, mkRecRestrict, mkRecDiffList, mkRecSingleton
  :: TypeU -> TypeU -> TypeU
mkRecUnion a b = OpU OpRecUnion [a, b]
mkRecIntersect a b = OpU OpRecIntersect [a, b]
mkRecRestrict a b = OpU OpRecRestrict [a, b]
mkRecDiffList a b = OpU OpRecDiffList [a, b]
mkRecSingleton k v = OpU OpRecSingleton [k, v]

mkListApp :: TypeU -> TypeU -> TypeU
mkListApp a b = OpU OpListApp [a, b]

mkSetUnion, mkSetInter, mkSetDiff :: TypeU -> TypeU -> TypeU
mkSetUnion a b = OpU OpSetUnion [a, b]
mkSetInter a b = OpU OpSetInter [a, b]
mkSetDiff a b = OpU OpSetDiff [a, b]

mkKeys, mkListToSet, mkSize :: TypeU -> TypeU
mkKeys r = OpU OpKeys [r]
mkListToSet l = OpU OpListToSet [l]
mkSize c = OpU OpSize [c]

mkProjectField :: TypeU -> TypeU -> TypeU
mkProjectField r f = OpU OpProjectField [r, f]

type2typeu :: Type -> TypeU
type2typeu (VarT v) = VarU v
type2typeu (UnkT v) = ForallU v (VarU v)
type2typeu (FunT ts t) = FunU (map type2typeu ts) (type2typeu t)
type2typeu (AppT v ts) = AppU (type2typeu v) (map type2typeu ts)
type2typeu (NamT o n ps rs) = NamU o n (map type2typeu ps) [(k, type2typeu x) | (k, x) <- rs]
type2typeu (EffectT effs t) = EffectU (EffectSet effs) (type2typeu t)
type2typeu (OptionalT t) = OptionalU (type2typeu t)
type2typeu (NatLitT n) = NatLitU n
type2typeu (NatAddT a b) = NatAddU (type2typeu a) (type2typeu b)
type2typeu (NatMulT a b) = NatMulU (type2typeu a) (type2typeu b)
type2typeu (NatSubT a b) = NatSubU (type2typeu a) (type2typeu b)
type2typeu (NatDivT a b) = NatDivU (type2typeu a) (type2typeu b)
type2typeu NatVoidT = NatVoidU
type2typeu (StrLitT s) = StrLitU s
type2typeu (StrConcatT a b) = StrConcatU (type2typeu a) (type2typeu b)
type2typeu StrVoidT = StrVoidU

unresolvedType2type :: TypeU -> Type
unresolvedType2type (VarU v) = VarT v
unresolvedType2type (NatVarU _) = NatVoidT
unresolvedType2type ExistU {} = error "Cannot cast existential type to Type"
unresolvedType2type (ForallU _ _) = error "Cannot cast universal type as Type"
unresolvedType2type (FunU ts t) = FunT (map unresolvedType2type ts) (unresolvedType2type t)
unresolvedType2type (AppU v ts) = AppT (unresolvedType2type v) (map unresolvedType2type ts)
unresolvedType2type (NamU t n ps rs) = NamT t n (map unresolvedType2type ps) [(k, unresolvedType2type e) | (k, e) <- rs]
unresolvedType2type (EffectU effs t) = EffectT (resolveEffectSet effs) (unresolvedType2type t)
unresolvedType2type (OptionalU t) = OptionalT (unresolvedType2type t)
unresolvedType2type NatVoidU = NatVoidT
unresolvedType2type (StrVarU _) = StrVoidT
unresolvedType2type StrVoidU = StrVoidT
-- Rec-kinded constructs: erase to NatVoidT at the ground type level. The
-- Type ADT does not represent Rec separately. See Stage 3 design memos.
unresolvedType2type (RecVarU _) = NatVoidT
unresolvedType2type RecVoidU = NatVoidT
-- List- and Set-kinded constructs erase to NatVoidT at the ground type
-- level. The Type ADT does not represent List or Set separately.
unresolvedType2type (ListVarU _) = NatVoidT
unresolvedType2type ListVoidU = NatVoidT
unresolvedType2type (SetVarU _) = NatVoidT
unresolvedType2type SetVoidU = NatVoidT
-- Cross-kind functions are pattern synonyms over 'OpU'; their lowering
-- is handled by the unified 'OpU _ _' clause below (erase to NatVoidT).
-- Unified carriers: mirror typeOf above. Only Nat-op forms have ground
-- Type-level counterparts; everything else erases to NatVoidT.
unresolvedType2type (OpU OpNatAdd [a, b]) = NatAddT (unresolvedType2type a) (unresolvedType2type b)
unresolvedType2type (OpU OpNatSub [a, b]) = NatSubT (unresolvedType2type a) (unresolvedType2type b)
unresolvedType2type (OpU OpNatMul [a, b]) = NatMulT (unresolvedType2type a) (unresolvedType2type b)
unresolvedType2type (OpU OpNatDiv [a, b]) = NatDivT (unresolvedType2type a) (unresolvedType2type b)
unresolvedType2type (OpU OpStrConcat [a, b]) = StrConcatT (unresolvedType2type a) (unresolvedType2type b)
unresolvedType2type (OpU _ _) = NatVoidT
unresolvedType2type (LitU (LNat n)) = NatLitT n
unresolvedType2type (LitU (LStr s)) = StrLitT s
unresolvedType2type (LitU (LRec _)) = NatVoidT
unresolvedType2type (LitU (LList _)) = NatVoidT
unresolvedType2type (LitU (LSet _)) = NatVoidT
unresolvedType2type (LabeledU _ t) = unresolvedType2type t

-- | get a fresh variable name that is not used in t1 or t2
newVariable :: TypeU -> TypeU -> TVar
newVariable t1 t2 = findNew variables (Set.union (allVars t1) (allVars t2))
  where
    variables = [1 ..] >>= flip replicateM ['a' .. 'z']

    findNew :: [String] -> Set.Set TypeU -> TVar
    findNew [] _ = error "No variable in the infinite list was OK with you? Sheesh, picky."
    findNew (x : xs) ts
      | Set.member (VarU v) ts = findNew xs ts
      | otherwise = v
      where
        v = TV $ DT.pack x

    allVars :: TypeU -> Set.Set TypeU
    allVars (ForallU v t) = Set.union (Set.singleton (VarU v)) (allVars t)
    allVars (NatVarU v) = Set.singleton (NatVarU v)
    allVars (EffectU _ t) = allVars t
    allVars (OptionalU t) = allVars t
    -- Unified carriers: recurse into payload via allVars so nested
    -- bound-quantified vars are still collected.
    allVars (OpU _ args) = Set.unions (map allVars args)
    allVars (LitU (LRec fs)) = Set.unions (map (allVars . snd) fs)
    allVars (LitU (LList es)) = Set.unions (map allVars es)
    allVars (LitU (LSet es)) = Set.unions (map allVars es)
    allVars (LabeledU _ t) = allVars t
    allVars t = free t

{- | Check whether a ground type contains any unknown (unresolved) type variables.
These arise from erasing 'ForallU' during 'typeOf', indicating a polymorphic type.
-}
containsUnk :: Type -> Bool
containsUnk (UnkT _) = True
containsUnk (VarT _) = False
containsUnk (FunT ts t) = any containsUnk ts || containsUnk t
containsUnk (AppT t ts) = containsUnk t || any containsUnk ts
containsUnk (NamT _ _ ps rs) = any containsUnk ps || any (containsUnk . snd) rs
containsUnk (EffectT _ t) = containsUnk t
containsUnk (OptionalT t) = containsUnk t
containsUnk (NatLitT _) = False
containsUnk (NatAddT a b) = containsUnk a || containsUnk b
containsUnk (NatMulT a b) = containsUnk a || containsUnk b
containsUnk (NatSubT a b) = containsUnk a || containsUnk b
containsUnk (NatDivT a b) = containsUnk a || containsUnk b
containsUnk NatVoidT = False
containsUnk (StrLitT _) = False
containsUnk (StrConcatT a b) = containsUnk a || containsUnk b
containsUnk StrVoidT = False

----- Pretty instances -------------------------------------------------------

-- | Records, objects, and tables render identically in type signatures —
-- just the type name optionally followed by parameters. The record/object/
-- table distinction is surfaced separately in the CLI help's type
-- definition block, where each named type is listed with its tag and
-- fields. Using a trivial instance here keeps signatures uncluttered.
instance Pretty NamType where
  pretty _ = mempty

instance Pretty Type where
  pretty t0 = f True t0
    where
      f _ (UnkT v) = pretty v
      f _ (VarT v) = pretty v
      f _ (AppT (VarT (TV "List")) [t]) = "[" <> f True t <> "]"
      f _ (AppT (VarT (TV "Tuple2")) ts) = encloseSep "(" ")" ", " (map (f True) ts)
      f _ (AppT (VarT (TV "Tuple3")) ts) = encloseSep "(" ")" ", " (map (f True) ts)
      f _ (AppT (VarT (TV "Tuple4")) ts) = encloseSep "(" ")" ", " (map (f True) ts)
      f _ (AppT (VarT (TV "Tuple5")) ts) = encloseSep "(" ")" ", " (map (f True) ts)
      f _ (AppT (VarT (TV "Tuple6")) ts) = encloseSep "(" ")" ", " (map (f True) ts)
      f _ (AppT (VarT (TV "Tuple7")) ts) = encloseSep "(" ")" ", " (map (f True) ts)
      f _ (AppT (VarT (TV "Tuple8")) ts) = encloseSep "(" ")" ", " (map (f True) ts)
      f _ (EffectT effs t)
        | Set.null effs = "{" <> f True t <> "}"
        | otherwise = "<" <> hcat (punctuate "," (map pretty (Set.toList effs))) <> ">" <+> f False t
      f _ (OptionalT t) = "?" <> f False t
      f _ (NatLitT n) = pretty n
      f _ (NatAddT a b) = "(" <> f True a <+> "+" <+> f True b <> ")"
      f _ (NatMulT a b) = "(" <> f True a <+> "*" <+> f True b <> ")"
      f _ (NatSubT a b) = "(" <> f True a <+> "-" <+> f True b <> ")"
      f _ (NatDivT a b) = "(" <> f True a <+> "/" <+> f True b <> ")"
      f _ NatVoidT = "_"
      f _ (StrLitT s) = dquotes (pretty s)
      f _ (StrConcatT a b) = "(" <> f True a <+> "+" <+> f True b <> ")"
      f _ StrVoidT = "_"
      f False t = parens (f True t)
      f _ (FunT [] t) = "() -> " <> f False t
      f _ (FunT ts t) = hsep $ punctuate " -> " (map (f False) (ts <> [t]))
      f _ (AppT t ts) = hsep (map (f False) (t : ts))
      -- Named types (records / objects / tables) render as "name [p1 ...]",
      -- Haskell-style. No tag and no inline field block; the record/table
      -- distinction and the field list are surfaced in the CLI help's
      -- type definition section and in typeclass-aware contexts.
      f _ (NamT _ n ps _) =
        let params = if null ps
                     then mempty
                     else space <> hsep (map (f False) ps)
        in pretty n <> params

-- | Walk a chain of nested 'RecExtendU' nodes and return its leaf fields
-- in source order plus the eventual tail expression (typically
-- 'RecEmptyU' for ground records, 'RecVarU' for row-polymorphic ones).
-- Used by the pretty printer to render iterated extensions as @{k=v,...}@.
collectExtends :: TypeU -> ([(Text, TypeU)], TypeU)
collectExtends = go []
  where
    go acc (RecExtendU k t rest) = go ((k, t) : acc) rest
    go acc t = (reverse acc, t)

instance Pretty TypeU where
  pretty t0 = f True t0
    where
      f _ (VarU v) = pretty v
      f _ (NatVarU v) = pretty v
      f _ (ExistU v ([], _) ([], _)) = angles $ pretty v
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
      f _ NatVoidU = "_"
      f _ (StrVarU v) = pretty v
      f _ StrVoidU = "_"
      f _ (RecVarU v) = pretty v
      f _ rec@(RecExtendU _ _ _)
        -- Render iterated extensions ending in RecEmptyU as `{k1=t1, k2=t2}`.
        -- A non-empty tail (RecVarU or another operator) renders as
        -- `(tail + k1=t1 + k2=t2)` to keep the tail visible.
        | (fields, RecEmptyU) <- collectExtends rec =
            braces (hcat (punctuate ", " [pretty k <> "=" <> f False t | (k, t) <- fields]))
        | (fields, tl) <- collectExtends rec =
            "(" <> f True tl <+> hsep ["+" <+> pretty k <> "=" <> f False t | (k, t) <- fields] <> ")"
      f _ (RecDiffU a ks) = "(" <> f True a <+> "-" <+> braces (hcat (punctuate "," (map pretty ks))) <> ")"
      f _ RecVoidU = "_"
      -- List- and Set-kinded constructs.
      f _ (ListVarU v) = pretty v
      f _ ListVoidU = "_"
      f _ (SetVarU v) = pretty v
      f _ SetVoidU = "_"
      -- Cross-kind functions are pattern synonyms over 'OpU'; rendering
      -- is dispatched by the unified 'OpU OpKeys / OpListToSet / OpSize /
      -- OpProjectField / OpRecSingleton' clauses below.
      -- Unified carriers: render identically to their per-kind counterparts.
      f _ (OpU OpNatAdd [a, b]) = "(" <> f True a <+> "+" <+> f True b <> ")"
      f _ (OpU OpNatSub [a, b]) = "(" <> f True a <+> "-" <+> f True b <> ")"
      f _ (OpU OpNatMul [a, b]) = "(" <> f True a <+> "*" <+> f True b <> ")"
      f _ (OpU OpNatDiv [a, b]) = "(" <> f True a <+> "/" <+> f True b <> ")"
      f _ (OpU OpStrConcat [a, b]) = "(" <> f True a <+> "+" <+> f True b <> ")"
      f _ (OpU OpRecUnion [a, b]) = "(" <> f True a <+> "+" <+> f True b <> ")"
      f _ (OpU OpRecIntersect [a, b]) = "(" <> f True a <+> "&" <+> f True b <> ")"
      f _ (OpU OpRecRestrict [a, b]) = "(" <> f True a <+> "#" <+> f True b <> ")"
      f _ (OpU OpRecDiffList [a, b]) = "(" <> f True a <+> "-" <+> f True b <> ")"
      f _ (OpU OpRecSingleton [k, v]) = "Singleton" <+> f False k <+> f False v
      f _ (OpU OpListApp [a, b]) = "(" <> f True a <+> "+" <+> f True b <> ")"
      f _ (OpU OpSetUnion [a, b]) = "(" <> f True a <+> "+" <+> f True b <> ")"
      f _ (OpU OpSetInter [a, b]) = "(" <> f True a <+> "&" <+> f True b <> ")"
      f _ (OpU OpSetDiff [a, b]) = "(" <> f True a <+> "-" <+> f True b <> ")"
      f _ (OpU OpKeys [r]) = "Keys" <+> f False r
      f _ (OpU OpListToSet [l]) = "ListToSet" <+> f False l
      f _ (OpU OpSize [c]) = "Size" <+> f False c
      f _ (OpU OpProjectField [r, fld]) = f False r <> "." <> f False fld
      f _ (OpU op args) = pretty (DT.pack (show op)) <> tupled (map (f True) args)  -- malformed; debug fallback
      f _ (LitU (LNat n)) = pretty n
      f _ (LitU (LStr s)) = dquotes (pretty s)
      f _ (LitU (LRec [])) = "{}"
      f _ (LitU (LRec fs)) = braces (hcat (punctuate ", " [pretty k <> "=" <> f False t | (k, t) <- fs]))
      f _ (LitU (LList es)) = "[" <> hcat (punctuate ", " (map (f True) es)) <> "]"
      f _ (LitU (LSet es)) = "{" <> hcat (punctuate ", " (map (f True) es)) <> "}"
      f _ (LabeledU (TV n) t) = pretty n <> ":" <> f False t
      f False t = parens (f True t)
      f _ (ExistU v (ts, _) (rs, _)) =
        angles $
          pretty v
            <+> list (map (f False) ts)
            <+> list (map ((\(x, y) -> tupled [x, y]) . bimap pretty (f True)) rs)
      f _ (FunU [] t) = "() -> " <> f False t
      f _ (FunU ts t) = hsep $ punctuate " ->" (map (f False) (ts <> [t]))
      f _ (ForallU v t) = "forall" <+> pretty v <+> "." <+> f True t
      f _ (AppU t ts) = hsep $ map (f False) (t : ts)
      -- See the NamT case in 'Pretty Type' above for the rendering rules.
      f _ (NamU _ n ps _) =
        let params = if null ps
                     then mempty
                     else space <> hsep (map (f False) ps)
        in pretty n <> params

instance Pretty EType where
  pretty (EType t (Set.toList -> cs) _ _) = case cs of
    [] -> pretty t
    [c] -> pretty c <+> "=>" <+> pretty t
    _ -> tupled (map pretty cs) <+> "=>" <+> pretty t

instance Pretty Constraint where
  pretty (Constraint cls ts) = pretty cls <+> hsep (map pretty ts)
  pretty (CMember a s) = "Member" <+> pretty a <+> pretty s
  pretty (CSubset a b) = "Subset" <+> pretty a <+> pretty b
  pretty (CDisjoint a b) = "Disjoint" <+> pretty a <+> pretty b
