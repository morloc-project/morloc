{-# LANGUAGE DeriveTraversable #-}
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
means t1 is at least as general as t2 (t1 instantiates to t2), so
@forall a . a <= Int@.
-}
module Morloc.Namespace.Type
  ( -- * Types
    NamType (..)
  , Type (..)
  , TArg (..)
  , tyArgs
  , KTerm (.., NatVoidT, StrVoidT)
  , ArgT
  , argOf
  , ktermU
  , argU
    -- Pattern synonyms bundled so that @TypeU (..)@ imports them.
  , TypeU
      ( ..
      , NatVarU
      , IntVarU
      , StrVarU
      , RecVarU
      , NatVoidU
      , StrVoidU
      , RecVoidU
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
  , isTupleName
  , tupleArity
  , type2typeu
  , splitFunAt
  , EType (..)

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
  , effectSetParts
  , effectSetVars
  , isEmptyEffectSet
  , normalizeEffectSet
  , unionEffectSet
  , mkEffectU
  , mkEffectT
  , prettyEffectSet

    -- * Docstring related types
  , CliOpt (..)
  , SourceAtom (..)
  , FormAtom (..)
  , PathPerm (..)
  , Check (..)
  , ArgSource (..)
  , WithSpec (..)
  , ParseSpec (..)
  , parseEntryName
  , parseEntryPrefix
  , parseEntryTargets
  , FoldSpec (..)
  , mangleTerminalName
  , mangleReplayName
  , replayEntryPrefix
  , CompanionRole (..)
  , companionName
  , ActionKind (..)
  , ReplayPlan (..)
  , replayPlanKind
  , anonRecordVar
  , opKind
  , kindOfU
  , fillsNat
  , partitionKindArgs
  , isInternalTerminalName
  , ArgDoc (..)
  , ArgDocVars (..)
  , inheritArgDocVars
  , ExprTypeE (..)

    -- * Scope
  , Scope
  , TypedefKind (..)

    -- * Type extensions
  , Constraint (..)

    -- * Predicates
  , containsUnk

    -- * Typeclasses
  , Typelike (..)
  , uncurryU
  , alignFun

    -- * kludge
  , newVariable

    -- * Partial order logic
  , isSubtypeOf
  , alphaEq
  , unifyU
  , applySubstU
  , wireFormsAgree
  , equivalent
  , mostGeneral
  , mostSpecific
  , generalizes
  , mostSpecificSubtypes
  , dataCtorsOfName
  , dataDeclOfName
  , paramPairs
  , instantiateParams
  , dataBodyCtors
  ) where

import qualified Data.List as DL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.PartialOrd as P
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Char as C
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

emptyEffectSet :: EffectSet
emptyEffectSet = EffectSet Set.empty

ioEffectSet :: EffectSet
ioEffectSet = EffectSet (Set.singleton "IO")

-- | Subset on resolved labels; an unsolved EffectVar is a subset of everything.
effectSubsetOf :: EffectSet -> EffectSet -> Bool
effectSubsetOf e1 e2 = Set.isSubsetOf (resolveEffectSet e1) (resolveEffectSet e2)

-- | Does an effect set mention any unsolved EffectVar? Subtyping rules
-- that would otherwise reject a non-subset relation may need to defer
-- judgement until the variable is solved.
effectSetHasVar :: EffectSet -> Bool
effectSetHasVar (EffectVar _) = True
effectSetHasVar (EffectSet _) = False
effectSetHasVar (EffectUnion a b) = effectSetHasVar a || effectSetHasVar b

-- | Decompose an effect set into its concrete labels and its set of
-- effect variables (flattening unions). Labels are concrete effects;
-- variables are unsolved row tails. The two are separate namespaces.
effectSetParts :: EffectSet -> (Set.Set EffectLabel, Set.Set TVar)
effectSetParts = go
  where
    go (EffectSet ls) = (ls, Set.empty)
    go (EffectVar v) = (Set.empty, Set.singleton v)
    go (EffectUnion a b) =
      let (la, va) = go a
          (lb, vb) = go b
       in (Set.union la lb, Set.union va vb)

-- | The effect variables mentioned by an effect set (for renaming and
-- occurs checks). Effect variables live in their own implicitly
-- universally-quantified namespace, like NatVarU / StrVarU.
effectSetVars :: EffectSet -> [TVar]
effectSetVars = Set.toList . snd . effectSetParts

-- | An effect set is provably empty when it has no concrete labels and
-- no (unsolved) effect variables. The empty set is the monoid identity:
-- @<> A == A@.
isEmptyEffectSet :: EffectSet -> Bool
isEmptyEffectSet es =
  let (ls, vs) = effectSetParts es
   in Set.null ls && Set.null vs

-- | Canonical form: concrete labels merged into a single 'EffectSet',
-- the effect variables appended as a deterministic union chain. Keeps
-- the derived Eq/Ord stable so equal rows compare equal regardless of
-- how they were built.
normalizeEffectSet :: EffectSet -> EffectSet
normalizeEffectSet es =
  let (ls, vs) = effectSetParts es
   in case (Set.null ls, Set.toList vs) of
        (True, []) -> EffectSet Set.empty
        (False, []) -> EffectSet ls
        (True, [v]) -> EffectVar v
        (_, vlist) -> foldl EffectUnion (EffectSet ls) (map EffectVar vlist)

unionEffectSet :: EffectSet -> EffectSet -> EffectSet
unionEffectSet a b = normalizeEffectSet (EffectUnion a b)


---- Type definitions

{- | How a type name was introduced.

* 'TypedefAlias' -- transparent @type Foo = Bar@: same identity as Bar,
  same instances, no per-language overrides. Reduces freely.

* 'TypedefNewtype' -- nominal @newtype Foo = Bar@: own per-language
  overrides, own instances, inherits only Bar's wire format.
  Opaque to reduction.

* 'TypedefPrimitive' -- @type Bool@, @type List a@, etc.: forward
  declaration with no RHS; has its own per-language overrides.
  Opaque to reduction.

* 'TypedefEnum' -- a `data` declaration: nominal like a newtype, with a
  closed constructor set and a wire form of its own.
-}
data TypedefKind
  = TypedefAlias
  | TypedefNewtype
  | TypedefPrimitive
  | TypedefEnum
  deriving (Show, Eq, Ord)

{- | Scope maps each type name to its definitions: the type parameters, the
body type, documentation, whether the definition is terminal (won't be
expanded further during type resolution), and whether it is a `type` alias
or a `newtype`.
-}
type Scope =
  Map
    TVar
    [ ( [Either (TVar, Kind) TypeU] -- type parameters (generic for left, specific for right)
      , TypeU
      , ArgDoc
      , Bool -- True if this is a "terminal" type (won't be reduced further)
      , TypedefKind -- Alias (transparent) or Newtype (nominal, wire-equivalent)
      )
    ]

-- | The constructors of a `data` type, in declaration order, or Nothing if
-- the name is not a `data` type. A constructor's position is its wire tag.
--
-- The scope body encodes the constructors as type-level literals (see
-- 'dataBodyCtors'); reduction stops at the nominal boundary, so the body
-- is carried, never expanded.
--
-- Keyed by name, so it cannot see through an alias; use 'TypeEval.dataHead'.
dataCtorsOfName :: Scope -> TVar -> Maybe [(Text, [TypeU])]
dataCtorsOfName scope v = snd <$> dataDeclOfName scope v

-- | A `data` declaration's type parameters and constructors, read from one
-- declaration.
dataDeclOfName :: Scope -> TVar -> Maybe ([TVar], [(Text, [TypeU])])
dataDeclOfName scope v = do
  entries <- Map.lookup v scope
  listToMaybe
    [ ([p | Left (p, _) <- ps], cs)
    | (ps, body, _, _, TypedefEnum) <- entries
    , Just cs <- [dataBodyCtors body]
    ]

-- | A declaration's variable parameters paired with an application's
-- arguments. With an argument per parameter, each variable takes the
-- argument in its own position and a specialized parameter takes none;
-- with fewer, the variables take the arguments in order.
paramPairs :: [Either (TVar, Kind) TypeU] -> [a] -> [(TVar, a)]
paramPairs ps ts
  | length ps == length ts = [(v, t) | (Left (v, _), t) <- zip ps ts]
  | otherwise = zip [v | Left (v, _) <- ps] ts

-- | Set each parameter to its argument at once: an argument that names a
-- parameter is not substituted again, and a binder shadowing a parameter
-- is respected.
instantiateParams :: Typelike a => [(TVar, a)] -> a -> a
instantiateParams pairs t =
  let held = [TV (T.pack ("#" <> show i)) | i <- [0 .. length pairs - 1 :: Int]]
      named = foldl (\acc ((p, _), h) -> substituteTVar p (tvar h) acc) t (zip pairs held)
   in foldl (\acc ((_, a), h) -> substituteTVar h a acc) named (zip pairs held)

-- | Decode a `data` declaration's body: a list of constructors, each a list
-- of its name followed by its argument types.
dataBodyCtors :: TypeU -> Maybe [(Text, [TypeU])]
dataBodyCtors (LitU (LList xs)) = mapM oneCtor xs
  where
    oneCtor (LitU (LList (LitU (LStr n) : args))) = Just (n, args)
    oneCtor _ = Nothing
dataBodyCtors _ = Nothing

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
  | AppT Type [ArgT]
  | NamT NamType TVar [ArgT] [(Key, Type)]
  | EffectT (Set.Set EffectLabel) Type
  | OptionalT Type
  deriving (Show, Ord, Eq)

-- | One argument of an applied type: a type, or an argument of another kind
-- (KIND-2), which holds no value but which a native form's macro skips and a
-- dimension check reads.
data TArg k t = TyA t | KiA k
  deriving (Show, Ord, Eq, Functor, Foldable, Traversable)

-- | The type arguments, which the @$N@ of a native form counts.
tyArgs :: [TArg k t] -> [t]
tyArgs as = [t | TyA t <- as]

-- | An argument of a non-Type kind after erasure.
data KTerm
  = -- | A variable inside a kind expression that nothing determined.
    KUnkT TVar
  | -- | A variable inside a kind expression.
    KVarT TVar
  | NatLitT Integer
  | NatAddT KTerm KTerm
  | NatMulT KTerm KTerm
  | NatSubT KTerm KTerm
  | NatDivT KTerm KTerm
  | -- | An erased placeholder of its kind, distinct from a literal of that
    -- kind (@NatLitT 0@).
    VoidT Kind
  | StrLitT Text
  | StrConcatT KTerm KTerm
  deriving (Show, Ord, Eq)

pattern NatVoidT :: KTerm
pattern NatVoidT = VoidT KindNat

pattern StrVoidT :: KTerm
pattern StrVoidT = VoidT KindStr

type ArgT = TArg KTerm Type

data OpenOrClosed = Open | Closed
  deriving (Show, Ord, Eq)

-- | Kind operators carried by 'OpU'. Arity is fixed per tag ('opArity').
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

-- | Type-level literals carried by 'LitU'.
data TyLit
  = LNat Integer
  | LStr Text
  | LRec [(Text, TypeU)]
  | LList [TypeU]
  | LSet [TypeU]              -- ^ canonical form: sorted, deduplicated
  deriving (Show, Ord, Eq)

{- | Full type with quantifiers. 'ExistU' represents existential variables
(solved during unification), 'ForallU' represents universally quantified
variables. This is the primary type representation during typechecking.
-}
data TypeU
  = VarU TVar
  -- | Kind-tagged variable ('NatVarU', 'StrVarU', 'RecVarU'). 'VarU' is
  -- Type-kinded; List and Set variables are separate constructors because
  -- their element kind is not tracked.
  | KVarU (TVar, Kind)
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
  -- | Kind-tagged erased phantom slot ('NatVoidU', 'StrVoidU', 'RecVoidU'),
  -- filling a missing kind argument. Distinct from a literal such as
  -- @NatLitU 0@ so that phantoms are not mistaken for real measurements.
  | VoidU Kind
  | ListVarU TVar -- ^ List-kinded variable, never quantified by ForallU.
  | ListVoidU -- ^ Erased phantom List slot.
  | SetVarU TVar -- ^ Set-kinded variable, never quantified by ForallU.
  | SetVoidU -- ^ Erased phantom Set slot.
  -- | Every kind operator; the per-operator forms below are pattern synonyms.
  -- Build with the smart constructors ('mkNatAdd', ...), which enforce
  -- 'opArity'.
  | OpU OpTag [TypeU]
  | LitU TyLit
  | LabeledU TVar TypeU -- ^ m@Int: a signature argument's label, or a newtype header's (DIM-9)
  deriving (Show, Ord, Eq)

pattern NatVarU :: TVar -> TypeU
pattern NatVarU v = KVarU (v, KindNat)

pattern IntVarU :: TVar -> TypeU
pattern IntVarU v = KVarU (v, KindInt)

pattern StrVarU :: TVar -> TypeU
pattern StrVarU v = KVarU (v, KindStr)

pattern RecVarU :: TVar -> TypeU
pattern RecVarU v = KVarU (v, KindRec)

pattern NatVoidU :: TypeU
pattern NatVoidU = VoidU KindNat

pattern StrVoidU :: TypeU
pattern StrVoidU = VoidU KindStr

pattern RecVoidU :: TypeU
pattern RecVoidU = VoidU KindRec

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

-- Matches only when the label slot holds a literal Str.
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

-- | Drop a fixed set of named fields from a record. Matches only when every
-- listed name is a literal Str.
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

{-# COMPLETE
    VarU, NatVarU, IntVarU, ExistU, ForallU, FunU, AppU, NamU, EffectU, OptionalU,
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

{-# COMPLETE
    VarU, KVarU, ExistU, ForallU, FunU, AppU, NamU, EffectU, OptionalU,
    VoidU, ListVarU, ListVoidU, SetVarU, SetVoidU, LabeledU,
    OpU, LitU
  #-}

{- | Extended Type that may represent a language specific type as well as sets
of properties and constrains.
-}
data EType
  = EType
  { etype :: TypeU
  , econs :: Set.Set Constraint
  , edocs :: ArgDoc
  , enatLabels :: Map TVar Int -- ^ Nat var name -> argument position index (from m@Int syntax)
  }
  deriving (Show, Eq, Ord)

-- | Constraints attached to a signature via @=>@: a typeclass constraint,
-- or one of the fixed-shape set primitives below.
data Constraint
  = Constraint ClassName [TypeU]
  | CMember TypeU TypeU       -- ^ Member a s :: a is an element of set s
  | CSubset TypeU TypeU       -- ^ Subset s1 s2 :: every element of s1 is in s2
  | CDisjoint TypeU TypeU     -- ^ Disjoint s1 s2 :: s1 and s2 share no elements
  deriving (Show, Eq, Ord)

data CliOpt
  = CliOptShort Char
  | CliOptLong Text
  | CliOptBoth Char Text
  deriving (Show, Ord, Eq)

-- | Where an argument's bytes come from (`@source`). Without it, scalars and
-- `Str` are read inline and other types are auto-classified as inline JSON
-- or a file.
data SourceAtom = SourceInline | SourceFile
  deriving (Show, Ord, Eq)

-- | How to interpret an argument's bytes (`@form`). Without it, the bytes are
-- classified as a structured format and never read as raw bytes.
-- `FormBytes` sniffs for a packet and falls back to raw bytes;
-- `FormBytesOnly` never sniffs; `FormPacket` accepts only a packet;
-- `FormTable` reads a headed table (CLI-13).
data FormAtom = FormPacket | FormBytes | FormBytesOnly | FormList | FormTable
  deriving (Show, Ord, Eq)

-- | Path-permission subset, e.g. "r", "w", "c", "rw", "rwc".
newtype PathPerm = PathPerm Text
  deriving (Show, Ord, Eq)

-- | A value invariant declared by `@check.<kind>`.
data Check
  = CheckPath PathPerm
  deriving (Show, Ord, Eq)

-- | The source of one handler argument in a formatter directive
-- (`@with`/`@render`). The formatted payload is bound separately: it is
-- placed where 'ArgValue' appears in 'wsArgs', or appended last when
-- 'wsArgs' contains no 'ArgValue'.
data ArgSource
  = ArgPos Int   -- ^ `$N`, a 1-based positional reference to a parent argument
  | ArgOffset    -- ^ `@offset`, the per-batch stream element offset (@tell)
  | ArgValue     -- ^ `@value`, the formatted payload
  deriving (Show, Ord, Eq)

-- | The three terms a folding formatter declares; a formatter names all
-- three or does not fold.
--
-- They must form a monoid: several accumulators may each start from
-- 'fsInit' and be merged with 'fsCombine', so unless 'fsInit' is an
-- identity and 'fsCombine' associative the answer depends on how the
-- stream was split. Neither law is checked.
data FoldSpec = FoldSpec
  { fsStep    :: EVar  -- ^ @b -> [a] -> b@, folds one batch into the accumulator
  , fsInit    :: EVar  -- ^ @b@, the identity: starts every accumulator, and
                       --   is the answer for an empty stream
  , fsCombine :: EVar  -- ^ @b -> b -> b@, merges accumulators into the answer
  }
  deriving (Show, Ord, Eq)

-- | One `@with`/`@render` formatter declaration on a signature preamble: a
-- command flag that dispatches to a synthesized entry applying 'wsTerm' to
-- the command's output.
data WithSpec = WithSpec
  { wsShort   :: Maybe Char
  , wsLong    :: Text
  , wsTerm    :: EVar
  , wsRender  :: Bool         -- ^ framing: False = `@with`   (typed output, rendered by -f)
                              --            True  = `@render` (final bytes, emitted verbatim)
  , wsStream  :: Bool         -- ^ `@stream`: True = per-batch streaming
                              --   False = whole-list gather (materialize, apply once)
  , wsDefault :: Bool         -- ^ `@default`: fires when no formatter flag and no `-f`
  , wsArgs    :: [ArgSource]  -- ^ handler argument sources, in order; the payload
                              --   goes at 'ArgValue' or is appended last if absent
  , wsFold    :: Maybe FoldSpec
                              -- ^ `@fold`/`@init`/`@combine`: the stream is folded
                              --   into one accumulator rather than gathered into a
                              --   list, and the handler receives the accumulator
  }
  deriving (Show, Ord, Eq)

-- | One `@parse` declaration on a command argument. At the command line,
-- a value prefixed with `<name>:`, or ending in one of 'psExts', is a path
-- to a file in that format, and 'psHandler' reads it into the argument's
-- value.
data ParseSpec = ParseSpec
  { psName    :: Text
  , psHandler :: EVar
  , psExts    :: [Text]  -- ^ lowercase, each starting with `.`
  }
  deriving (Show, Ord, Eq)

-- | The entry-point name of an export's `@with`/`@render` action.
--
-- The `mlcp_` prefix is a legal identifier in every pool language and never
-- contains `__`, which C++ reserves (lex.name).
mangleTerminalName :: EVar -> Text -> EVar
mangleTerminalName = mangleWithPrefix "mlcp_"

-- | The name of the replay entry of a terminal action: the handler applied
-- to the parent's staged output rather than to a fresh run of the parent.
mangleReplayName :: EVar -> Text -> EVar
mangleReplayName = mangleWithPrefix replayEntryPrefix

-- | A command synthesized for a parent command, by what it does for it.
data CompanionRole
  = RoleAction Text
  -- ^ the action with this long flag, on a fresh run of the parent
  | RoleReplay Text
  -- ^ the action with this long flag, on the parent's saved output
  | RoleParse (Maybe Text)
  -- ^ the `@parse` entry of the parent, or of its action with this long flag
  deriving (Show, Ord, Eq)

-- | The name of a parent's companion. Every companion name is made here.
companionName :: EVar -> CompanionRole -> EVar
companionName p (RoleAction long) = mangleTerminalName p long
companionName p (RoleReplay long) = mangleReplayName p long
companionName p (RoleParse Nothing) = parseEntryName p
companionName p (RoleParse (Just long)) = parseEntryName (mangleTerminalName p long)

-- | The compiler-owned prefix of every replay entry.
replayEntryPrefix :: Text
replayEntryPrefix = "mlcr_"

-- | How a terminal action's handler consumes its command's output.
data ActionKind
  = KindValue   -- ^ the value a non-streaming command returns
  | KindGather  -- ^ everything a streaming command streams, at once
  | KindStream  -- ^ each batch, as the command streams it (@stream)
  | KindFold    -- ^ each batch, into one accumulator (@fold)
  deriving (Show, Ord, Eq)

-- | The replay entry of one terminal action (see 'mangleReplayName'): the
-- kind it was synthesized for, or why the action has none and runs only on
-- a fresh run of its command.
data ReplayPlan
  = Replayed ActionKind
  | NotReplayed ActionKind Text
  deriving (Show, Ord, Eq)

replayPlanKind :: ReplayPlan -> ActionKind
replayPlanKind (Replayed k) = k
replayPlanKind (NotReplayed k _) = k

mangleWithPrefix :: Text -> EVar -> Text -> EVar
mangleWithPrefix prefix (EV parent) long =
  EV (DT.concat [prefix, parent, "_", DT.map hyphenToUnder long])
  where
    hyphenToUnder '-' = '_'
    hyphenToUnder c = c

-- | The name of every anonymous closed record. It is not a legal type name,
-- so no declared record can share it.
anonRecordVar :: TVar
anonRecordVar = TV "@REC"

-- | The kind of the value of a type-level operator.
opKind :: OpTag -> Kind
opKind op = case op of
  OpNatAdd -> KindNat
  OpNatSub -> KindNat
  OpNatMul -> KindNat
  OpNatDiv -> KindNat
  OpSize -> KindNat
  OpStrConcat -> KindStr
  OpRecExtend -> KindRec
  OpRecUnion -> KindRec
  OpRecIntersect -> KindRec
  OpRecRestrict -> KindRec
  OpRecDiffList -> KindRec
  OpRecSingleton -> KindRec
  OpListApp -> KindList KindStr
  OpSetUnion -> KindSet KindStr
  OpSetInter -> KindSet KindStr
  OpSetDiff -> KindSet KindStr
  OpKeys -> KindSet KindStr
  OpListToSet -> KindSet KindStr
  OpProjectField -> KindType

-- | A kind that may fill a Nat position: Nat, or Int, which no parameter
-- declares and which fills only Nat positions (KIND-1).
fillsNat :: Kind -> Bool
fillsNat k = k == KindNat || k == KindInt

-- | The kind of a type argument whose form fixes it: a literal, an
-- operator, a placeholder or a kinded variable. Nothing for a Type-kinded
-- form and for a ground row, which is a Rec argument or a record type
-- depending on the parameter it fills (see
-- 'Morloc.CodeGenerator.ValueArgs').
kindOfU :: TypeU -> Maybe Kind
kindOfU t = case t of
  KVarU (_, k) -> Just k
  VoidU k -> Just k
  ListVarU _ -> Just (KindList KindStr)
  ListVoidU -> Just (KindList KindStr)
  SetVarU _ -> Just (KindSet KindStr)
  SetVoidU -> Just (KindSet KindStr)
  LitU (LNat _) -> Just KindNat
  LitU (LStr _) -> Just KindStr
  LitU (LList _) -> Just (KindList KindStr)
  LitU (LSet _) -> Just (KindSet KindStr)
  LitU (LRec _) -> Nothing
  r@(RecExtendU _ _ _) -> maybe (Just KindRec) (const Nothing) (groundRecFields r)
  OpU op _ -> case opKind op of
    KindType -> Nothing
    k -> Just k
  LabeledU _ x -> kindOfU x
  _ -> Nothing


-- | The arguments whose kind is not fixed by their form, and the number of
-- the others.
partitionKindArgs :: (a -> Maybe Kind) -> [a] -> ([a], Int)
partitionKindArgs kindOf ts =
  let types = [x | x <- ts, isNothing (kindOf x)]
   in (types, length ts - length types)

-- | Recognizes the compiler-internal name prefix produced by
-- 'mangleTerminalName'. The nexus filters these out of the top-level
-- command menu but keeps them dispatchable via each parent command's
-- terminal flags.
isInternalTerminalName :: Text -> Bool
isInternalTerminalName t =
  DT.isPrefixOf "mlcp_" t || DT.isPrefixOf parseEntryPrefix t || DT.isPrefixOf replayEntryPrefix t

-- | The compiler-owned prefix of every name synthesized for `@parse`.
parseEntryPrefix :: Text
parseEntryPrefix = "mlcq_"

-- | The name of the entry synthesized for a target command whose arguments
-- declare `@parse` formats. The target is the command itself or one of its
-- 'mangleTerminalName' commands.
parseEntryName :: EVar -> EVar
parseEntryName (EV t) = EV (parseEntryPrefix <> t)

-- | The targets of a command's `@parse` entries, given its preamble and
-- argument docstrings: the command and each of its `@with`/`@render`
-- commands, or none when no argument declares a format.
parseEntryTargets :: EVar -> ArgDocVars -> [ArgDocVars] -> [EVar]
parseEntryTargets name cmdDoc argDocs
  | any (not . null . docParse) argDocs =
      name : [ mangleTerminalName name (wsLong w) | w <- docWith cmdDoc ]
  | otherwise = []

data ArgDocVars = ArgDocVars
  { docLines :: [Text]
  , docName :: Maybe Text
  , docLiteral :: Maybe Bool
  , docMany :: Maybe Bool
  , docStdin :: Maybe Bool
  , docUnroll :: Maybe Bool
  , docDefault :: Maybe Text
  , docMetavar :: Maybe Text
  , docArg :: Maybe CliOpt
  , docTrue :: Maybe CliOpt
  , docFalse :: Maybe CliOpt
  , docReturn :: Maybe Text
  , docSource :: Maybe SourceAtom
  , docForm :: Maybe FormAtom
  , docChecks :: [Check]
  , docListSource :: Maybe SourceAtom
  , docListForm :: Maybe FormAtom
  , docListChecks :: [Check]
  , docWith :: [WithSpec]
  , docParse :: [ParseSpec]
  , docMime :: Maybe Text
  , docEpilogues :: [[Text]]
    -- ^ `@epilogue` blocks: verbatim lines printed at the foot of the
    -- command's help, after its argument and return blocks
  }
  deriving (Show, Ord, Eq)

-- | Docstring variables with each one unset in the first taken from the
-- second (DOC-5: the nearest docstring wins).
inheritArgDocVars :: ArgDocVars -> ArgDocVars -> ArgDocVars
inheritArgDocVars r1 r2 =
  ArgDocVars
    { docLines = if (length (docLines r1) > 0) then docLines r1 else docLines r2
    , docName = docName r1 <|> docName r2
    , docLiteral = docLiteral r1 <|> docLiteral r2
    , docMany = docMany r1 <|> docMany r2
    , docStdin = docStdin r1 <|> docStdin r2
    , docUnroll = docUnroll r1 <|> docUnroll r2
    , docDefault = docDefault r1 <|> docDefault r2
    , docMetavar = docMetavar r1 <|> docMetavar r2
    , docArg = docArg r1 <|> docArg r2
    , docTrue = docTrue r1 <|> docTrue r2
    , docFalse = docFalse r1 <|> docFalse r2
    , docReturn = docReturn r1 <|> docReturn r2
    , docSource = docSource r1 <|> docSource r2
    , docForm = docForm r1 <|> docForm r2
    , docChecks = if null (docChecks r1) then docChecks r2 else docChecks r1
    , docListSource = docListSource r1 <|> docListSource r2
    , docListForm = docListForm r1 <|> docListForm r2
    , docListChecks = if null (docListChecks r1) then docListChecks r2 else docListChecks r1
    , docWith = if null (docWith r1) then docWith r2 else docWith r1
    , docParse = docParse r1
    , docMime = docMime r1 <|> docMime r2
    , docEpilogues = if null (docEpilogues r1) then docEpilogues r2 else docEpilogues r1
    }

data ArgDoc
  = ArgDocRec ArgDocVars [(Key, ArgDocVars)]
  | ArgDocSig
      ArgDocVars
      [ArgDocVars]
      ArgDocVars
  | ArgDocAlias ArgDocVars
  | ArgDocData ArgDocVars [(Text, ArgDocVars)]
  -- ^ A `data` declaration: the type's own docstring and one per
  -- constructor, in declaration order.
  deriving (Show, Ord, Eq)

data ExprTypeE = ExprTypeE
  { exprTypeConcreteForm :: Maybe (Lang, Bool)
  , exprTypeName :: TVar
  , exprTypeParams :: [Either (TVar, Kind) TypeU]
  , exprTypeType :: TypeU
  , exprTypeDoc :: ArgDoc
  , exprTypeKind :: TypedefKind
  }
  deriving (Show, Ord, Eq)

---- Typeclasses

class Typelike a where
  typeOf :: a -> Type

  free :: a -> Set.Set a

  substituteTVar :: TVar -> a -> a -> a

  -- | A type that is just this variable.
  tvar :: TVar -> a

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
      , docMany = Nothing
      , docStdin = Nothing
      , docUnroll = Nothing
      , docDefault = Nothing
      , docMetavar = Nothing
      , docArg = Nothing
      , docTrue = Nothing
      , docFalse = Nothing
      , docReturn = Nothing
      , docSource = Nothing
      , docForm = Nothing
      , docChecks = []
      , docListSource = Nothing
      , docListForm = Nothing
      , docListChecks = []
      , docWith = []
      , docParse = []
      , docMime = Nothing
      , docEpilogues = []
      }

instance Typelike Type where
  tvar = VarT

  typeOf = id

  substituteTVar v0 r0 t0 = sub t0
    where
      sub t@(UnkT _) = t
      sub t@(VarT v)
        | v0 == v = r0
        | otherwise = t
      sub (FunT ts t) = FunT (map sub ts) (sub t)
      sub (AppT v ts) = AppT (sub v) (map (fmap sub) ts)
      sub (NamT r n ps es) = NamT r n ps [(k, sub t) | (k, t) <- es]
      sub (EffectT effs t) = EffectT effs (sub t)
      sub (OptionalT t) = OptionalT (sub t)

  free (UnkT _) = Set.empty
  free v@(VarT _) = Set.singleton v
  free (FunT ts t) = Set.unions (map free (t : ts))
  free (AppT t ts) = Set.unions (map free (t : tyArgs ts))
  free (NamT _ _ _ es) = Set.unions (map (free . snd) es)
  free (EffectT _ t) = free t
  free (OptionalT t) = free t

  normalizeType (FunT ts1@(_ : _) (FunT ts2@(_ : _) ft)) = normalizeType $ FunT (ts1 <> ts2) ft
  normalizeType (AppT (AppT h innerArgs) outerArgs) =
    normalizeType (AppT h (innerArgs ++ outerArgs))
  normalizeType (AppT t ts) = AppT (normalizeType t) (map (fmap normalizeType) ts)
  normalizeType (NamT n v ds ks) = NamT n v (map (fmap normalizeType) ds) (zip (map fst ks) (map (normalizeType . snd) ks))
  normalizeType (EffectT effs t) = EffectT effs (normalizeType t)
  normalizeType (OptionalT t) = OptionalT (normalizeType t)
  normalizeType t = t

-- | Two function types compared parameter by parameter (APP-1): the
-- parameters both have, paired, and the two results, the longer side's
-- surplus parameters belonging to its result. Nothing when exactly one side
-- is a suspension, which takes no parameters and is not a grouping.
alignFun :: ([TypeU], TypeU) -> ([TypeU], TypeU) -> Maybe ([(TypeU, TypeU)], (TypeU, TypeU))
alignFun (as1, r1) (as2, r2)
  | null as1 /= null as2 = Nothing
  | otherwise = Just (zip pre1 pre2, (result rest1 r1, result rest2 r2))
  where
    n = min (length as1) (length as2)
    (pre1, rest1) = splitAt n as1
    (pre2, rest2) = splitAt n as2
    result [] r = r
    result rest r = FunU rest r

-- | A type as an applied argument: a type, or a term of another kind.
argOf :: TypeU -> ArgT
argOf u = case u of
  VarU v -> TyA (VarT v)
  ExistU _ (ps, _) (rs@(_ : _), _) -> TyA (NamT NamRecord (TV "Record") (map argOf ps) (map (second typeOf) rs))
  ExistU v _ _ -> argOf (ForallU v (VarU v))
  ForallU v t -> case argOf t of
    TyA x -> TyA (kindTerms (undetermined v) (substituteTVar v (UnkT v) x))
    KiA k -> KiA (undetermined v k)
  -- `a -> (b -> c)` is `a -> b -> c`; grouping is an implementation's choice
  -- (a source's @rsize@). An effect or an empty argument list is a
  -- suspension, not a grouping.
  FunU ts t -> TyA $ case typeOf t of
    FunT us r | not (null ts), not (null us) -> FunT (map typeOf ts ++ us) r
    t' -> FunT (map typeOf ts) t'
  AppU t ts -> TyA (AppT (typeOf t) (map argOf ts))
  NamU n o ps rs -> TyA (NamT n o (map argOf ps) (zip (map fst rs) (map (typeOf . snd) rs)))
  EffectU effs t -> TyA (mkEffectT (resolveEffectSet effs) (typeOf t))
  OptionalU t -> TyA (OptionalT (typeOf t))
  KVarU (_, KindInt) -> KiA NatVoidT
  KVarU (_, k) -> KiA (VoidT k)
  VoidU k -> KiA (VoidT k)
  -- A ground Rec chain lowers to a record so its column schema survives
  -- (Arrow tables); a row-polymorphic one erases.
  RecExtendU {} -> case groundRecFields u of
    Just fs -> TyA (NamT NamRecord anonRecordVar [] [(Key k, typeOf t) | (k, t) <- fs])
    Nothing -> KiA (VoidT KindRec)
  ListVarU _ -> KiA (VoidT (KindList KindStr))
  ListVoidU -> KiA (VoidT (KindList KindStr))
  SetVarU _ -> KiA (VoidT (KindSet KindStr))
  SetVoidU -> KiA (VoidT (KindSet KindStr))
  OpU OpNatAdd [a, b] -> KiA (NatAddT (operand NatVoidT a) (operand NatVoidT b))
  OpU OpNatSub [a, b] -> KiA (NatSubT (operand NatVoidT a) (operand NatVoidT b))
  OpU OpNatMul [a, b] -> KiA (NatMulT (operand NatVoidT a) (operand NatVoidT b))
  OpU OpNatDiv [a, b] -> KiA (NatDivT (operand NatVoidT a) (operand NatVoidT b))
  OpU OpStrConcat [a, b] -> KiA (StrConcatT (operand StrVoidT a) (operand StrVoidT b))
  -- A Type-kinded operator left unreduced (a projection from a row
  -- variable) is a type nothing determined.
  OpU op _ -> case opKind op of
    KindType -> TyA (UnkT (TV (render (pretty u))))
    k -> KiA (VoidT k)
  LitU (LNat n) -> KiA (NatLitT n)
  LitU (LStr x) -> KiA (StrLitT x)
  LitU (LRec fs) -> TyA (NamT NamRecord anonRecordVar [] [(Key k, typeOf t) | (k, t) <- fs])
  LitU (LList _) -> KiA (VoidT (KindList KindStr))
  LitU (LSet _) -> KiA (VoidT (KindSet KindStr))
  LabeledU _ t -> argOf t
  where
    -- a quantified variable inside a kind term is one nothing determined
    undetermined v x = case x of
      KVarT w | w == v -> KUnkT w
      NatAddT a b -> NatAddT (undetermined v a) (undetermined v b)
      NatMulT a b -> NatMulT (undetermined v a) (undetermined v b)
      NatSubT a b -> NatSubT (undetermined v a) (undetermined v b)
      NatDivT a b -> NatDivT (undetermined v a) (undetermined v b)
      StrConcatT a b -> StrConcatT (undetermined v a) (undetermined v b)
      _ -> x
    -- an operand of a kind expression; one that is no kind term is erased
    operand void x = case argOf x of
      KiA k -> k
      TyA (UnkT v) -> KUnkT v
      TyA (VarT v) -> KVarT v
      TyA _ -> void

-- | A function type's arguments and result, spelled flat by the rule
-- 'typeOf' applies: @a -> (b -> c)@ is @a -> b -> c@, while an effect or an
-- empty argument list is a suspension rather than a grouping.
-- | Apply a rewrite to every kind term inside a type.
kindTerms :: (KTerm -> KTerm) -> Type -> Type
kindTerms f t = case t of
  FunT ts r -> FunT (map (kindTerms f) ts) (kindTerms f r)
  AppT h ts -> AppT (kindTerms f h) (map arg ts)
  NamT o v ps rs -> NamT o v (map arg ps) [(k, kindTerms f x) | (k, x) <- rs]
  EffectT e x -> EffectT e (kindTerms f x)
  OptionalT x -> OptionalT (kindTerms f x)
  _ -> t
  where
    arg (TyA x) = TyA (kindTerms f x)
    arg (KiA k) = KiA (f k)

uncurryU :: TypeU -> ([TypeU], TypeU)
uncurryU (ForallU _ t) = uncurryU t
uncurryU t@(FunU [] _) = ([], t)
uncurryU (FunU ts r) = case uncurryU r of
  (us@(_ : _), r') -> (ts ++ us, r')
  _ -> (ts, r)
uncurryU t = ([], t)

instance Typelike TypeU where
  tvar = VarU

  -- KIND-2: every type a term or a field has is Type-kinded, so a kind term
  -- reaches here only as an applied argument, which 'argOf' tags.
  typeOf u = case argOf u of
    TyA t -> t
    KiA _ -> UnkT (TV "_")

  free v@(VarU _) = Set.singleton v
  free (NatVarU _) = Set.empty
  free (IntVarU _) = Set.empty
  free v@(ExistU _ ([], _) (rs, _)) = Set.unions $ Set.singleton v : map (free . snd) rs
  free (ExistU v (ts, _) _) = Set.unions $ Set.singleton (AppU (VarU v) ts) : map free ts
  free (ForallU v t) = Set.delete (VarU v) (free t)
  free (FunU ts t) = Set.unions $ map free (t : ts)
  free (AppU t ts) = Set.unions $ map free (t : ts)
  free (NamU _ _ ps rs) = Set.unions $ map free (map snd rs <> ps)
  free (EffectU _ t) = free t
  free (OptionalU t) = free t
  free NatVoidU = Set.empty
  -- Kind-tagged variables are implicitly quantified and are not free
  -- Type-kinded variables.
  free (StrVarU _) = Set.empty
  free StrVoidU = Set.empty
  free (RecVarU _) = Set.empty
  free RecVoidU = Set.empty
  free (ListVarU _) = Set.empty
  free ListVoidU = Set.empty
  free (SetVarU _) = Set.empty
  free SetVoidU = Set.empty
  free (KVarU _) = Set.empty
  free (VoidU _) = Set.empty
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
  substituteTVar v0 r0 t0 = sub t0
    where
      sub t@(VarU v)
        | v0 == v = r0
        | otherwise = t
      -- Kind-tagged variables are substituted by name; the caller must pass
      -- a replacement of matching kind.
      sub t@(NatVarU v)
        | v0 == v = r0
        | otherwise = t
      sub t@(IntVarU v)
        | v0 == v = r0
        | otherwise = t
      sub t@(StrVarU v)
        | v0 == v = r0
        | otherwise = t
      sub t@(RecVarU v)
        | v0 == v = r0
        | otherwise = t
      sub t@(ListVarU v)
        | v0 == v = r0
        | otherwise = t
      sub t@(SetVarU v)
        | v0 == v = r0
        | otherwise = t
      sub (ExistU v (map sub -> ps, pc) (map (second sub) -> rs, rc)) = ExistU v (ps, pc) (rs, rc)
      sub (ForallU v t)
        | v0 == v = ForallU v t
        | otherwise = ForallU v (sub t)
      sub (FunU ts t) = FunU (map sub ts) (sub t)
      sub (AppU t ts) = AppU (sub t) (map sub ts)
      sub (NamU r n ps rs) = NamU r n (map sub ps) [(k, sub t) | (k, t) <- rs]
      sub (EffectU effs t) = mkEffectU effs (sub t)
      sub (OptionalU t) = OptionalU (sub t)
      sub t@NatVoidU = t
      sub t@StrVoidU = t
      sub t@RecVoidU = t
      sub t@ListVoidU = t
      sub t@SetVoidU = t
      sub t@(KVarU (v, _))
        | v0 == v = r0
        | otherwise = t
      sub t@(VoidU _) = t
      sub (OpU op args) = OpU op (map sub args)
      sub (LitU (LNat n)) = LitU (LNat n)
      sub (LitU (LStr s)) = LitU (LStr s)
      sub (LitU (LRec fs)) = LitU (LRec [(k, sub t) | (k, t) <- fs])
      sub (LitU (LList es)) = LitU (LList (map sub es))
      sub (LitU (LSet es)) = LitU (LSet (map sub es))
      sub (LabeledU n t) = LabeledU n (sub t)

  -- APP-1; an empty argument list is a suspension, not a grouping.
  normalizeType (FunU ts1@(_ : _) (FunU ts2@(_ : _) ft)) = normalizeType $ FunU (ts1 <> ts2) ft
  -- Substituting a partially applied class type into a method's @f a@
  -- yields @AppU (AppU H [n]) [a]@; downstream code matches only the flat
  -- @AppU H [n, a]@.
  normalizeType (AppU (AppU h innerArgs) outerArgs) =
    normalizeType (AppU h (innerArgs ++ outerArgs))
  normalizeType (AppU t ts) = AppU (normalizeType t) (map normalizeType ts)
  normalizeType (NamU n v ds ks) = NamU n v (map normalizeType ds) (zip (map fst ks) (map (normalizeType . snd) ks))
  normalizeType (ForallU v t) = ForallU v (normalizeType t)
  normalizeType (ExistU v (map normalizeType -> ps, pc) (map (second normalizeType) -> rs, rc)) = ExistU v (ps, pc) (rs, rc)
  normalizeType (EffectU effs t) = mkEffectU effs (normalizeType t)
  normalizeType (OptionalU t) = OptionalU (normalizeType t)
  normalizeType t@(NatVarU _) = t
  normalizeType t@(IntVarU _) = t
  normalizeType (OpU op args) = OpU op (map normalizeType args)
  normalizeType (LitU (LRec fs)) = LitU (LRec [(k, normalizeType t) | (k, t) <- fs])
  normalizeType (LitU (LList es)) = LitU (LList (map normalizeType es))
  normalizeType (LitU (LSet es)) = LitU (LSet (map normalizeType es))
  normalizeType t@(LitU _) = t
  normalizeType (LabeledU n t) = LabeledU n (normalizeType t)
  normalizeType t = t

----- Partial order logic

instance P.PartialOrd TypeU where
  (<=) = generalizes False

  -- Alpha-equivalence: binding the same variables in a different order
  -- gives an equal type.
  (==) = alphaEq

-- | Whether the first type instantiates to the second. Its forall-bound
-- variables may each take one value, and so may its free kind variables
-- when @kinds@ is set (they are implicitly quantified in an instance head);
-- every variable of the second type is fixed. An erased Nat placeholder
-- matches any Nat either way.
generalizes :: Bool -> TypeU -> TypeU -> Bool
generalizes kinds t1 t2 = isJust (match flex0 t1 t2 Map.empty)
  where
    flex0 = Set.fromList [v | kinds, (v, form) <- varForms t1, isKindForm form]
    isKindForm VarPlain = False
    isKindForm _ = True

    match :: Set.Set TVar -> TypeU -> TypeU -> Map TVar TypeU -> Maybe (Map TVar TypeU)
    match flex a b s = case (a, b) of
      (LabeledU _ x, _) -> match flex x b s
      (_, LabeledU _ y) -> match flex a y s
      (ForallU v x, _) -> match (Set.insert v flex) x b s
      _ | Just (v, ok) <- flexible flex a -> if ok b then bind v b s else Nothing
      (_, ForallU w y) ->
        let w' = rigidName flex w
            form = maybe VarPlain id (lookup w (varForms y))
         in match flex a (substituteTVar w (rebuildVar form w') y) s
      (VarU v, VarU w) -> guard (v == w) s
      (KVarU v, KVarU w) -> guard (v == w) s
      (ListVarU v, ListVarU w) -> guard (v == w) s
      (SetVarU v, SetVarU w) -> guard (v == w) s
      (ExistU v (ts1, _) (rs1, _), ExistU w (ts2, _) (rs2, _))
        | v == w, length ts1 == length ts2 ->
            pairs flex (zip ts1 ts2) s
              >>= \s' -> pairs flex [(x, y) | (k, x) <- rs1, Just y <- [lookup k rs2]] s'
              >>= \s'' -> guard (all (\(k, _) -> isJust (lookup k rs2)) rs1) s''
      (FunU ts1 r1, FunU ts2 r2) -> do
        (ps, rs) <- alignFun (ts1, r1) (ts2, r2)
        pairs flex (rs : ps) s
      (AppU h1 as1, AppU h2 as2) | length as1 == length as2 -> pairs flex (zip (h1 : as1) (h2 : as2)) s
      (NamU o1 n1 ps1 rs1, NamU o2 n2 ps2 rs2)
        | o1 == o2, n1 == n2, length ps1 == length ps2, DL.sort (map fst rs1) == DL.sort (map fst rs2) ->
            pairs flex (zip ps1 ps2 <> [(x, y) | (k, x) <- rs1, Just y <- [lookup k rs2]]) s
      (EffectU e1 x, EffectU e2 y) | e1 == e2 -> match flex x y s
      (OptionalU x, OptionalU y) -> match flex x y s
      (OpU op1 as1, OpU op2 as2) | op1 == op2, length as1 == length as2 -> pairs flex (zip as1 as2) s
      (LitU (LRec fs1), LitU (LRec fs2)) | map fst fs1 == map fst fs2 -> pairs flex (zip (map snd fs1) (map snd fs2)) s
      (LitU (LList es1), LitU (LList es2)) | length es1 == length es2 -> pairs flex (zip es1 es2) s
      (LitU (LSet es1), LitU (LSet es2)) | length es1 == length es2 -> pairs flex (zip es1 es2) s
      (LitU l1, LitU l2) -> guard (l1 == l2) s
      (NatVoidU, y) | natLike y -> Just s
      (x, NatVoidU) | natLike x -> Just s
      (VoidU k1, VoidU k2) -> guard (k1 == k2) s
      (ListVoidU, ListVoidU) -> Just s
      (SetVoidU, SetVoidU) -> Just s
      _ -> Nothing

    -- A bound variable of the second type, renamed apart from the first's.
    rigidName flex (TV w) = head [v | k <- [0 :: Int ..], let v = TV (w <> "~" <> DT.pack (show k)), not (Set.member v flex)]

    pairs flex xys s = foldM (\s' (x, y) -> match flex x y s') s xys

    bind v b s = case Map.lookup v s of
      Just b0
        | alphaEq b0 b -> Just s
        | b0 == NatVoidU, natLike b -> Just (Map.insert v b s)
        | b == NatVoidU, natLike b0 -> Just s
        | otherwise -> Nothing
      Nothing -> Just (Map.insert v b s)

    guard True s = Just s
    guard False _ = Nothing

    -- A flexible variable, and what it may stand for.
    flexible flex t = case t of
      VarU v | Set.member v flex -> Just (v, \b -> isNothing (kindOfU b))
      KVarU (v, k) | Set.member v flex -> Just (v, \b -> kindOfU b == Just k || k == KindNat && maybe False fillsNat (kindOfU b) || k == KindRec && isRow b)
      ListVarU v | Set.member v flex -> Just (v, \b -> kindOfU b == Just (KindList KindStr))
      SetVarU v | Set.member v flex -> Just (v, \b -> kindOfU b == Just (KindSet KindStr))
      _ -> Nothing

    isRow b = case b of
      LitU (LRec _) -> True
      RecExtendU {} -> True
      _ -> False

    natLike x = case x of
      NatVarU _ -> True
      IntVarU _ -> True
      NatVoidU -> True
      LitU (LNat _) -> True
      OpU op _ -> isNatOpTag op
      _ -> False

-- | The carrier a type variable occurs in. A variable must be replaced by a
-- value of its own kind, so a renaming has to rebuild the carrier it found.
data VarForm = VarPlain | VarKinded Kind | VarListed | VarSetted

rebuildVar :: VarForm -> TVar -> TypeU
rebuildVar VarPlain v = VarU v
rebuildVar (VarKinded k) v = KVarU (v, k)
rebuildVar VarListed v = ListVarU v
rebuildVar VarSetted v = SetVarU v

-- | Every type variable in a type, with the carrier it occurs in.
varForms :: TypeU -> [(TVar, VarForm)]
varForms = go
  where
    go (VarU v) = [(v, VarPlain)]
    go (KVarU (v, k)) = [(v, VarKinded k)]
    go (ListVarU v) = [(v, VarListed)]
    go (SetVarU v) = [(v, VarSetted)]
    go ListVoidU = []
    go SetVoidU = []
    go (VoidU _) = []
    go (LitU _) = []
    go (OpU _ ts) = concatMap go ts
    go (FunU ts t) = concatMap go (t : ts)
    go (AppU t ts) = concatMap go (t : ts)
    go (EffectU _ t) = go t
    go (OptionalU t) = go t
    go (LabeledU _ t) = go t
    go (NamU _ v ps rs) = (v, VarPlain) : concatMap go (ps <> map snd rs)
    go (ExistU v (ts, _) (rs, _)) = (v, VarPlain) : concatMap go (ts <> map snd rs)
    go (ForallU v t) = (v, VarPlain) : go t

-- | Rename the given variables to names that occur in neither type, preserving
-- each one's carrier. Two types that happen to use the same variable name do
-- not thereby constrain each other, so they must be separated before unifying.
renameApartU :: [TVar] -> [TVar] -> TypeU -> TypeU -> ([TVar], TypeU, TypeU)
renameApartU avoid vs t1 t2 =
  let forms = varForms t1 <> varForms t2
      used = map fst forms <> avoid
      (vs', renames) = foldl step ([], []) vs
      step (accVs, accR) v =
        let v' = fresh (used <> map snd accR) v
         in (accVs <> [v'], accR <> [(v, v')])
      apply t = foldl (\acc (v, v') -> substituteTVar v (rebuildVar (formOf forms v) v') acc) t renames
   in (vs', apply t1, apply t2)
  where
    formOf forms v = maybe VarPlain id (lookup v forms)
    fresh taken v =
      let candidates = [TV (unTVar v <> DT.replicate k "'") | k <- [1 ..]]
       in head [c | c <- candidates, c `notElem` taken]

-- | Most general unifier of two types under a set of flexible variables.
--
-- A flexible variable may take any value; anything else matches only itself.
-- The substitution is kept idempotent, so applying it is a fold of
-- 'substituteTVar'.
unifyU :: [TVar] -> TypeU -> TypeU -> Maybe [(TVar, TypeU)]
unifyU flex = go []
  where
    go sub x y = case (resolve sub x, resolve sub y) of
      (a, b) | Just v <- flexVar a -> bind sub v b
             | Just v <- flexVar b -> bind sub v a
      (VarU v1, VarU v2) | v1 == v2 -> Just sub
      (KVarU (v1, k1), KVarU (v2, k2)) | v1 == v2, k1 == k2 -> Just sub
      (ListVarU v1, ListVarU v2) | v1 == v2 -> Just sub
      (SetVarU v1, SetVarU v2) | v1 == v2 -> Just sub
      (ListVoidU, ListVoidU) -> Just sub
      (SetVoidU, SetVoidU) -> Just sub
      (VoidU k1, VoidU k2) | k1 == k2 -> Just sub
      (LitU l1, LitU l2) | l1 == l2 -> Just sub
      (OpU o1 as1, OpU o2 as2) | o1 == o2 -> goAll sub as1 as2
      (FunU as1 r1, FunU as2 r2) -> goAll sub (r1 : as1) (r2 : as2)
      (AppU f1 as1, AppU f2 as2) -> goAll sub (f1 : as1) (f2 : as2)
      (EffectU e1 u1, EffectU e2 u2) | e1 == e2 -> go sub u1 u2
      (OptionalU u1, OptionalU u2) -> go sub u1 u2
      (LabeledU l1 u1, LabeledU l2 u2) | l1 == l2 -> go sub u1 u2
      (NamU o1 n1 ps1 rs1, NamU o2 n2 ps2 rs2)
        | o1 == o2, n1 == n2, map fst rs1 == map fst rs2 ->
            goAll sub (ps1 <> map snd rs1) (ps2 <> map snd rs2)
      (ExistU v1 (ts1, o1) (rs1, c1), ExistU v2 (ts2, o2) (rs2, c2))
        | v1 == v2, o1 == o2, c1 == c2, map fst rs1 == map fst rs2 ->
            goAll sub (ts1 <> map snd rs1) (ts2 <> map snd rs2)
      (ForallU v1 u1, ForallU v2 u2) | v1 == v2 -> go sub u1 u2
      _ -> Nothing

    goAll sub xs ys
      | length xs /= length ys = Nothing
      | otherwise = foldl step (Just sub) (zip xs ys)
      where
        step acc (u, v) = acc >>= \s -> go s u v

    flexVar t = case t of
      (VarU v) | v `elem` flex -> Just v
      (KVarU (v, _)) | v `elem` flex -> Just v
      (ListVarU v) | v `elem` flex -> Just v
      (SetVarU v) | v `elem` flex -> Just v
      _ -> Nothing

    resolve sub t = case flexVar t of
      (Just v) -> maybe t (resolve sub) (lookup v sub)
      Nothing -> t

    -- Composed eagerly so the substitution stays idempotent. Binding a
    -- variable to a term containing it would make an infinite type.
    bind sub v t
      | Just v' <- flexVar t, v' == v = Just sub
      | v `elem` map fst (varForms t) = Nothing
      | otherwise = Just ((v, t) : [(u, substituteTVar v t s) | (u, s) <- sub])

applySubstU :: [(TVar, TypeU)] -> TypeU -> TypeU
applySubstU sub t = foldl (\acc (v, r) -> substituteTVar v r acc) t sub

-- | Do two @(head, wire)@ pairs agree wherever their heads meet?
--
-- Heads with no common instance describe disjoint sets of use sites and say
-- nothing about each other. Where they do meet, the form a value of that type
-- serializes to must be the same under both.
wireFormsAgree :: ([TVar], TypeU, TypeU) -> ([TVar], TypeU, TypeU) -> Bool
wireFormsAgree (vs1, h1, w1) (vs2, h2, w2) =
  let avoid = vs1 <> map fst (varForms h1) <> map fst (varForms w1)
      (vs2', h2', w2') = renameApartU avoid vs2 h2 w2
   in case unifyU (vs1 <> vs2') h1 h2' of
        Nothing -> True
        (Just sub) -> alphaEq (applySubstU sub w1) (applySubstU sub w2')

-- | Equality up to a consistent renaming of bound variables. Adjacent
-- universals commute, so the bijection is built from the structure of the
-- two types, not from the order of their quantifiers.
alphaEq :: TypeU -> TypeU -> Bool
alphaEq a b =
  let (vs1, t1) = peelForalls a
      (vs2, t2) = peelForalls b
   in length vs1 == length vs2
        && case walk (Set.fromList vs1, Set.fromList vs2) [] t1 t2 of
             (Just _) -> True
             Nothing -> False
  where
    peelForalls :: TypeU -> ([TVar], TypeU)
    peelForalls (ForallU v t) = let (vs, t') = peelForalls t in (v : vs, t')
    peelForalls t = ([], t)

    -- The bijection so far, one entry per matched pair of bound variables.
    walk ::
      (Set.Set TVar, Set.Set TVar) ->
      [(TVar, TVar)] ->
      TypeU ->
      TypeU ->
      Maybe [(TVar, TVar)]
    walk flex bij x y = case (x, y) of
      (VarU v1, VarU v2) -> bind flex bij v1 v2
      (KVarU (v1, k1), KVarU (v2, k2)) | k1 == k2 -> bind flex bij v1 v2
      (ListVarU v1, ListVarU v2) -> bind flex bij v1 v2
      (SetVarU v1, SetVarU v2) -> bind flex bij v1 v2
      (ListVoidU, ListVoidU) -> Just bij
      (SetVoidU, SetVoidU) -> Just bij
      (VoidU k1, VoidU k2) | k1 == k2 -> Just bij
      (LitU l1, LitU l2) | l1 == l2 -> Just bij
      (OpU o1 as1, OpU o2 as2) | o1 == o2 -> walkAll flex bij as1 as2
      (FunU as1 r1, FunU as2 r2) -> do
        (ps, (q1, q2)) <- alignFun (as1, r1) (as2, r2)
        walkAll flex bij (q1 : map fst ps) (q2 : map snd ps)
      (AppU f1 as1, AppU f2 as2) -> walkAll flex bij (f1 : as1) (f2 : as2)
      (EffectU e1 u1, EffectU e2 u2) | e1 == e2 -> walk flex bij u1 u2
      (OptionalU u1, OptionalU u2) -> walk flex bij u1 u2
      (LabeledU l1 u1, LabeledU l2 u2) | l1 == l2 -> walk flex bij u1 u2
      (NamU o1 n1 ps1 rs1, NamU o2 n2 ps2 rs2)
        | o1 == o2
        , n1 == n2
        , map fst rs1 == map fst rs2 ->
            walkAll flex bij (ps1 <> map snd rs1) (ps2 <> map snd rs2)
      (ExistU v1 (ts1, o1) (rs1, c1), ExistU v2 (ts2, o2) (rs2, c2))
        | v1 == v2
        , o1 == o2
        , c1 == c2
        , map fst rs1 == map fst rs2 ->
            walkAll flex bij (ts1 <> map snd rs1) (ts2 <> map snd rs2)
      -- A nested quantifier opens a scope on both sides at once. Its variables
      -- join the bijection like any other, so nested binders commute too.
      (ForallU _ _, ForallU _ _) ->
        let (ivs1, b1) = peelForalls x
            (ivs2, b2) = peelForalls y
            (f1, f2) = flex
         in if length ivs1 == length ivs2
              then walk (foldr Set.insert f1 ivs1, foldr Set.insert f2 ivs2) bij b1 b2
              else Nothing
      _ -> Nothing

    walkAll ::
      (Set.Set TVar, Set.Set TVar) ->
      [(TVar, TVar)] ->
      [TypeU] ->
      [TypeU] ->
      Maybe [(TVar, TVar)]
    walkAll flex bij xs ys
      | length xs /= length ys = Nothing
      | otherwise = go bij (zip xs ys)
      where
        go acc [] = Just acc
        go acc ((u, v) : rest) = walk flex acc u v >>= \acc' -> go acc' rest

    -- Two bound variables match when neither is already matched to something
    -- else. A variable bound by neither side matches only itself.
    bind ::
      (Set.Set TVar, Set.Set TVar) ->
      [(TVar, TVar)] ->
      TVar ->
      TVar ->
      Maybe [(TVar, TVar)]
    bind (f1, f2) bij v1 v2
      | Set.member v1 f1 && Set.member v2 f2 =
          case (lookup v1 bij, lookup v2 [(r, l) | (l, r) <- bij]) of
            (Nothing, Nothing) -> Just ((v1, v2) : bij)
            (Just v2', Just v1') | v2' == v2 && v1' == v1 -> Just bij
            _ -> Nothing
      | not (Set.member v1 f1) && not (Set.member v2 f2) && v1 == v2 = Just bij
      | otherwise = Nothing

isSubtypeOf :: TypeU -> TypeU -> Bool
isSubtypeOf t1 t2 = case P.compare t1 t2 of
  (Just x) -> x <= EQ
  _ -> False

equivalent :: TypeU -> TypeU -> Bool
equivalent t1 t2 = isSubtypeOf t1 t2 && isSubtypeOf t2 t1

-- | find the most specific subtypes
mostSpecificSubtypes :: TypeU -> [TypeU] -> [TypeU]
mostSpecificSubtypes t ts = mostSpecific $ filter (\x -> generalizes True x t) ts

-- | The instance heads no other head is strictly more general than. A kind
-- variable in a head is implicitly quantified, so it is flexible here.
mostGeneral :: [TypeU] -> [TypeU]
mostGeneral ts = [t | t <- ts, not (any (`strictlyMoreGeneral` t) ts)]

-- | The instance heads no other head is strictly more specific than (CLS-16).
mostSpecific :: [TypeU] -> [TypeU]
mostSpecific ts = [t | t <- ts, not (any (strictlyMoreGeneral t) ts)]

strictlyMoreGeneral :: TypeU -> TypeU -> Bool
strictlyMoreGeneral a b = generalizes True a b && not (generalizes True b a)

---- Utility functions

-- | The fields of a Rec extension chain that ends in RecEmptyU, else Nothing.
groundRecFields :: TypeU -> Maybe [(Text, TypeU)]
groundRecFields RecEmptyU = Just []
groundRecFields (RecExtendU k t rest) = ((k, t):) <$> groundRecFields rest
groundRecFields _ = Nothing

extractKey :: TypeU -> TVar
extractKey (VarU v) = v
extractKey (NatVarU v) = v
extractKey (IntVarU v) = v
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
extractKey (KVarU (v, _)) = v
extractKey (VoidU KindNat) = TV "Nat"
extractKey (VoidU KindStr) = TV "Str"
extractKey (VoidU KindRec) = TV "Rec"
extractKey (VoidU (KindList _)) = TV "List"
extractKey (VoidU (KindSet _)) = TV "Set"
extractKey (VoidU KindType) = TV "Type"
extractKey (OpU op _) = opKeyTag op
extractKey (LitU (LNat _)) = TV "Nat"
extractKey (LitU (LStr _)) = TV "Str"
extractKey (LitU (LRec _)) = anonRecordVar
extractKey (LitU (LList _)) = TV "List"
extractKey (LitU (LSet _)) = TV "Set"
extractKey (LabeledU _ t) = extractKey t
extractKey t = error $ "Cannot currently handle functional type imports: " <> show t

-- | The result-kind key of an operator, for 'extractKey'.
opKeyTag :: OpTag -> TVar
opKeyTag OpNatAdd = TV "Nat"
opKeyTag OpNatSub = TV "Nat"
opKeyTag OpNatMul = TV "Nat"
opKeyTag OpNatDiv = TV "Nat"
opKeyTag OpStrConcat = TV "Str"
opKeyTag OpRecExtend = anonRecordVar
opKeyTag OpRecUnion = anonRecordVar
opKeyTag OpRecIntersect = anonRecordVar
opKeyTag OpRecRestrict = anonRecordVar
opKeyTag OpRecDiffList = anonRecordVar
opKeyTag OpRecSingleton = anonRecordVar
opKeyTag OpListApp = TV "List"
opKeyTag OpSetUnion = TV "Set"
opKeyTag OpSetInter = TV "Set"
opKeyTag OpSetDiff = TV "Set"
opKeyTag OpKeys = TV "Set"
opKeyTag OpListToSet = TV "Set"
opKeyTag OpSize = TV "Nat"
opKeyTag OpProjectField = TV "Type"

opArity :: OpTag -> Int
opArity OpKeys = 1
opArity OpListToSet = 1
opArity OpSize = 1
opArity OpRecExtend = 3
opArity _ = 2

isNatOpTag :: OpTag -> Bool
isNatOpTag OpNatAdd = True
isNatOpTag OpNatSub = True
isNatOpTag OpNatMul = True
isNatOpTag OpNatDiv = True
isNatOpTag OpSize = True
isNatOpTag _ = False

-- | Build an 'OpU', checking its arity.
mkOp :: OpTag -> [TypeU] -> TypeU
mkOp op args
  | length args == opArity op = OpU op args
  | otherwise =
      error $ "mkOp: arity mismatch for " <> show op
           <> "; expected " <> show (opArity op)
           <> " got " <> show (length args)

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

-- | A suspension type @<E> T@ with its row normalized. The empty row is a
-- row: @<> T@ is a thunk yielding a @T@, not a @T@. A row over a
-- suspension is a second layer, never merged with the first.
mkEffectU :: EffectSet -> TypeU -> TypeU
mkEffectU es t = EffectU (normalizeEffectSet es) t

mkEffectT :: Set.Set EffectLabel -> Type -> Type
mkEffectT = EffectT

type2typeu :: Type -> TypeU
type2typeu (VarT v) = VarU v
type2typeu (UnkT v) = ForallU v (VarU v)
type2typeu (FunT ts t) = FunU (map type2typeu ts) (type2typeu t)
type2typeu (AppT (AppT h innerArgs) outerArgs) =
  -- Flattened, as in 'normalizeType'.
  type2typeu (AppT h (innerArgs ++ outerArgs))
type2typeu (AppT v ts) = AppU (type2typeu v) (map argU ts)
type2typeu (NamT o n ps rs) = NamU o n (map argU ps) [(k, type2typeu x) | (k, x) <- rs]
type2typeu (EffectT effs t) = mkEffectU (EffectSet effs) (type2typeu t)
type2typeu (OptionalT t) = OptionalU (type2typeu t)

argU :: ArgT -> TypeU
argU (TyA t) = type2typeu t
argU (KiA k) = ktermU k

-- | A kind term as a general type.
ktermU :: KTerm -> TypeU
ktermU k = case k of
  KUnkT v -> ForallU v (VarU v)
  KVarT v -> VarU v
  NatLitT n -> NatLitU n
  NatAddT a b -> NatAddU (ktermU a) (ktermU b)
  NatMulT a b -> NatMulU (ktermU a) (ktermU b)
  NatSubT a b -> NatSubU (ktermU a) (ktermU b)
  NatDivT a b -> NatDivU (ktermU a) (ktermU b)
  VoidT (KindList _) -> ListVoidU
  VoidT (KindSet _) -> SetVoidU
  VoidT x -> VoidU x
  StrLitT x -> StrLitU x
  StrConcatT a b -> StrConcatU (ktermU a) (ktermU b)

-- | The first @n@ inputs of a function type and the type left once they are
-- consumed. Types are flattened (@a -> (b -> c)@ is @a -> b -> c@), so a
-- function returning a function lists the returned function's inputs after
-- its own.
splitFunAt :: Int -> Type -> ([Type], Type)
splitFunAt n (FunT ins out)
  | length ins > n = (take n ins, FunT (drop n ins) out)
  | otherwise = (ins, out)
splitFunAt _ t = ([], t)

-- | A variable name used in neither type.
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
    allVars (IntVarU v) = Set.singleton (IntVarU v)
    allVars (EffectU _ t) = allVars t
    allVars (OptionalU t) = allVars t
    -- Recurse with allVars, not free, to collect nested bound variables.
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
containsUnk (AppT t ts) = containsUnk t || any argUnk ts
containsUnk (NamT _ _ ps rs) = any argUnk ps || any (containsUnk . snd) rs
containsUnk (EffectT _ t) = containsUnk t
containsUnk (OptionalT t) = containsUnk t

argUnk :: ArgT -> Bool
argUnk (TyA t) = containsUnk t
argUnk (KiA k) = ktermUnk k
  where
    ktermUnk x = case x of
      KUnkT _ -> True
      NatAddT a b -> ktermUnk a || ktermUnk b
      NatMulT a b -> ktermUnk a || ktermUnk b
      NatSubT a b -> ktermUnk a || ktermUnk b
      NatDivT a b -> ktermUnk a || ktermUnk b
      StrConcatT a b -> ktermUnk a || ktermUnk b
      _ -> False

----- Pretty instances -------------------------------------------------------

-- | Empty: records, objects and tables render the same in signatures.
instance Pretty NamType where
  pretty _ = mempty

-- | The arity of a tuple constructor name: exactly @"Tuple" <> show k@.
tupleArity :: Text -> Maybe Int
tupleArity n = case DT.stripPrefix "Tuple" n of
  Just ds
    | not (DT.null ds) && DT.all C.isDigit ds && (DT.length ds == 1 || DT.head ds /= '0') ->
        Just (DT.foldl' (\acc c -> acc * 10 + C.digitToInt c) 0 ds)
  _ -> Nothing

isTupleName :: Text -> Bool
isTupleName = isJust . tupleArity

instance (Pretty k, Pretty t) => Pretty (TArg k t) where
  pretty (TyA t) = pretty t
  pretty (KiA k) = pretty k

instance Pretty KTerm where
  pretty k = case k of
    KUnkT v -> pretty v
    KVarT v -> pretty v
    NatLitT n -> pretty n
    NatAddT x y -> "(" <> pretty x <+> "+" <+> pretty y <> ")"
    NatMulT x y -> "(" <> pretty x <+> "*" <+> pretty y <> ")"
    NatSubT x y -> "(" <> pretty x <+> "-" <+> pretty y <> ")"
    NatDivT x y -> "(" <> pretty x <+> "/" <+> pretty y <> ")"
    VoidT _ -> "_"
    StrLitT x -> dquotes (pretty x)
    StrConcatT x y -> "(" <> pretty x <+> "+" <+> pretty y <> ")"

instance Pretty Type where
  pretty t0 = f True t0
    where
      f _ (UnkT v) = pretty v
      f _ (VarT v) = pretty v
      f _ (AppT (VarT (TV "List")) [TyA t]) = "[" <> f True t <> "]"
      -- A tuple is one line however wide it is: its rendering reaches help
      -- text and manifest fields, where a soft line break would be a
      -- newline inside a type.
      f _ (AppT (VarT (TV n)) ts) | isTupleName n = "(" <> hcat (punctuate ", " (map (a True) ts)) <> ")"
      -- A row over a suspension is a second layer and is parenthesized so
      -- it cannot be read as a row union.
      f True (EffectT effs t) =
        "<" <> hcat (punctuate "," (map pretty (Set.toList effs))) <> ">" <+> layer t
      f _ (OptionalT t) = "?" <> f False t
      -- A named type without parameters is a single token and never needs
      -- parentheses, whatever position it sits in.
      f _ (NamT _ n [] _) = pretty n
      f False t = parens (f True t)
      -- An arrow associates to the right, so only a function in argument
      -- position needs parentheses; an application or a suspension there
      -- reads unambiguously.
      f _ (FunT [] t) = "() -> " <> f True t
      f _ (FunT ts t) = hsep $ punctuate " ->" (map arrowArg ts <> [f True t])
      f _ (AppT t ts) = hsep (f False t : map (a False) ts)
      -- A named type renders as its name and parameters, without fields.
      f _ (NamT _ n ps _) =
        let params = if null ps
                     then mempty
                     else space <> hsep (map (a False) ps)
        in pretty n <> params
      a top (TyA t) = f top t
      a _ (KiA k) = pretty k
      layer t@(EffectT _ _) = parens (f True t)
      layer t = f False t
      arrowArg t@(FunT _ _) = parens (f True t)
      arrowArg t = f True t

-- | The fields of a chain of 'RecExtendU' nodes, in source order, and its tail.
collectExtends :: TypeU -> ([(Text, TypeU)], TypeU)
collectExtends = go []
  where
    go acc (RecExtendU k t rest) = go ((k, t) : acc) rest
    go acc t = (reverse acc, t)

-- | Render an effect row as @\<L1,...,Ln, e\>@: sorted concrete labels, then
-- effect variables. The empty row renders as @\<\>@.
prettyEffectSet :: EffectSet -> Doc ann
prettyEffectSet effs =
  let (ls, vs) = effectSetParts effs
      parts = map pretty (Set.toList ls) ++ map pretty (Set.toList vs)
   in "<" <> hcat (punctuate "," parts) <> ">"

instance Pretty TypeU where
  pretty t0 = f True t0
    where
      f _ (VarU v) = pretty v
      f _ (NatVarU v) = pretty v
      f _ (IntVarU v) = pretty v
      f _ (ExistU v ([], _) ([], _)) = "*" <> pretty v
      f _ (AppU (VarU (TV "List")) [t]) = "[" <> f True t <> "]"
      f _ (AppU (VarU (TV n)) ts) | isTupleName n = "(" <> hcat (punctuate ", " (map (f True) ts)) <> ")"
      f True (EffectU effs t) = prettyEffectSet effs <+> layerU t
      f _ (OptionalU t) = "?" <> f False t
      f _ NatVoidU = "_"
      f _ (StrVarU v) = pretty v
      f _ StrVoidU = "_"
      f _ (RecVarU v) = pretty v
      f _ rec@(RecExtendU _ _ _)
        -- `{k1=t1, k2=t2}`, or `(tail + k1=t1 + k2=t2)` when the tail is not empty.
        | (fields, RecEmptyU) <- collectExtends rec =
            braces (hcat (punctuate ", " [pretty k <> "=" <> f False t | (k, t) <- fields]))
        | (fields, tl) <- collectExtends rec =
            "(" <> f True tl <+> hsep ["+" <+> pretty k <> "=" <> f False t | (k, t) <- fields] <> ")"
      f _ (RecDiffU a ks) = "(" <> f True a <+> "-" <+> braces (hcat (punctuate "," (map pretty ks))) <> ")"
      f _ RecVoidU = "_"
      f _ (ListVarU v) = pretty v
      f _ ListVoidU = "_"
      f _ (SetVarU v) = pretty v
      f _ SetVoidU = "_"
      f _ (KVarU (v, _)) = pretty v
      f _ (VoidU _) = "_"
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
      f _ (LabeledU (TV n) t) = pretty n <> "@" <> f False t
      f _ (NamU _ n [] _) = pretty n
      f False t = parens (f True t)
      f _ (ExistU v (ts, _) (rs, _)) =
        "*"
          <> pretty v
            <+> list (map (f False) ts)
            <+> list (map ((\(x, y) -> tupled [x, y]) . bimap pretty (f True)) rs)
      f _ (FunU [] t) = "() -> " <> f True t
      f _ (FunU ts t) = hsep $ punctuate " ->" (map arrowArgU ts <> [f True t])
      f _ (ForallU v t) = "forall" <+> pretty v <+> "." <+> f True t
      f _ (AppU t ts) = hsep $ map (f False) (t : ts)
      f _ (NamU _ n ps _) =
        let params = if null ps
                     then mempty
                     else space <> hsep (map (f False) ps)
        in pretty n <> params
      layerU t@(EffectU _ _) = parens (f True t)
      layerU t = f False t
      arrowArgU t@(FunU _ _) = parens (f True t)
      arrowArgU t = f True t

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
