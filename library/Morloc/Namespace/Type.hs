{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.Namespace.Type
Description : Type system types and partial order logic
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc.Namespace.Type
  ( -- * Types
    NamType (..)
  , Type (..)
  , TypeU (..)
  , OpenOrClosed (..)
  , extractKey
  , type2typeu
  , EType (..)
  , unresolvedType2type

    -- * Docstring related types
  , CliOpt (..)
  , ArgDoc (..)
  , ArgDocVars (..)
  , ExprTypeE (..)

    -- * Scope
  , Scope

    -- * Type extensions
  , Constraint (..)

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
import Data.Text (Text)
import qualified Data.Text as DT
import qualified Data.Set as Set
import Morloc.Data.Doc
import Morloc.Namespace.Prim

---- Type definitions

-- | Stores a list of types that are present in the scope of each type variable
type Scope =
  Map
    TVar
    [ ( [Either TVar TypeU] -- type parameters (generic for left, specific for right)
      , TypeU
      , ArgDoc
      , Bool -- True if this is a "terminal" type (won't be reduced further)
      )
    ]

data NamType
  = NamRecord
  | NamObject
  | NamTable
  deriving (Show, Ord, Eq)

-- | A basic type
data Type
  = UnkT TVar
  | VarT TVar
  | FunT [Type] Type
  | AppT Type [Type]
  | NamT NamType TVar [Type] [(Key, Type)]
  | ThunkT Type
  deriving (Show, Ord, Eq)

data OpenOrClosed = Open | Closed
  deriving (Show, Ord, Eq)

-- | A type with existentials and universals
data TypeU
  = VarU TVar
  | ExistU
      TVar
      ([TypeU], OpenOrClosed)
      ([(Key, TypeU)], OpenOrClosed)
  | ForallU TVar TypeU
  | FunU [TypeU] TypeU
  | AppU TypeU [TypeU]
  | NamU NamType TVar [TypeU] [(Key, TypeU)]
  | ThunkU TypeU
  deriving (Show, Ord, Eq)

{- | Extended Type that may represent a language specific type as well as sets
of properties and constrains.
-}
data EType
  = EType
  { etype :: TypeU
  , econs :: Set.Set Constraint
  , edocs :: ArgDoc
  }
  deriving (Show, Eq, Ord)

data Constraint = Constraint ClassName [TypeU]
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
  , exprTypeParams :: [Either TVar TypeU]
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
      sub (ThunkT t) = ThunkT (sub t)

  free (UnkT _) = Set.empty
  free v@(VarT _) = Set.singleton v
  free (FunT ts t) = Set.unions (map free (t : ts))
  free (AppT t ts) = Set.unions (map free (t : ts))
  free (NamT _ _ _ es) = Set.unions (map (free . snd) es)
  free (ThunkT t) = free t

  normalizeType (FunT ts1 (FunT ts2 ft)) = normalizeType $ FunT (ts1 <> ts2) ft
  normalizeType (AppT t ts) = AppT (normalizeType t) (map normalizeType ts)
  normalizeType (NamT n v ds ks) = NamT n v (map normalizeType ds) (zip (map fst ks) (map (normalizeType . snd) ks))
  normalizeType (ThunkT t) = ThunkT (normalizeType t)
  normalizeType t = t

instance Typelike TypeU where
  typeOf (VarU v) = VarT v
  typeOf (ExistU _ (ps, _) (rs@(_ : _), _)) = NamT NamRecord (TV "Record") (map typeOf ps) (map (second typeOf) rs)
  typeOf (ExistU v _ _) = typeOf (ForallU v (VarU v))
  typeOf (ForallU v t) = substituteTVar v (UnkT v) (typeOf t)
  typeOf (FunU ts t) = FunT (map typeOf ts) (typeOf t)
  typeOf (AppU t ts) = AppT (typeOf t) (map typeOf ts)
  typeOf (NamU n o ps rs) = NamT n o (map typeOf ps) (zip (map fst rs) (map (typeOf . snd) rs))
  typeOf (ThunkU t) = ThunkT (typeOf t)

  free v@(VarU _) = Set.singleton v
  free v@(ExistU _ ([], _) (rs, _)) = Set.unions $ Set.singleton v : map (free . snd) rs
  free (ExistU v (ts, _) _) = Set.unions $ Set.singleton (AppU (VarU v) ts) : map free ts
  free (ForallU v t) = Set.delete (VarU v) (free t)
  free (FunU ts t) = Set.unions $ map free (t : ts)
  free (AppU t ts) = Set.unions $ map free (t : ts)
  free (NamU _ _ ps rs) = Set.unions $ map free (map snd rs <> ps)
  free (ThunkU t) = free t

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
      sub (ExistU v (map sub -> ps, pc) (map (second sub) -> rs, rc)) = ExistU v (ps, pc) (rs, rc)
      sub (ForallU v t)
        | v0 == v = ForallU v t
        | otherwise = ForallU v (sub t)
      sub (FunU ts t) = FunU (map sub ts) (sub t)
      sub (AppU t ts) = AppU (sub t) (map sub ts)
      sub (NamU r n ps rs) = NamU r n (map sub ps) [(k, sub t) | (k, t) <- rs]
      sub (ThunkU t) = ThunkU (sub t)

  normalizeType (FunU ts1 (FunU ts2 ft)) = normalizeType $ FunU (ts1 <> ts2) ft
  normalizeType (AppU t ts) = AppU (normalizeType t) (map normalizeType ts)
  normalizeType (NamU n v ds ks) = NamU n v (map normalizeType ds) (zip (map fst ks) (map (normalizeType . snd) ks))
  normalizeType (ForallU v t) = ForallU v (normalizeType t)
  normalizeType (ExistU v (map normalizeType -> ps, pc) (map (second normalizeType) -> rs, rc)) = ExistU v (ps, pc) (rs, rc)
  normalizeType (ThunkU t) = ThunkU (normalizeType t)
  normalizeType t = t

----- Partial order logic

instance P.PartialOrd TypeU where
  (<=) (VarU v1) (VarU v2) = v1 == v2
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
  (<=) (ThunkU t1) (ThunkU t2) = t1 P.<= t2
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
    f (ThunkU t1) (ThunkU t2) = f t1 t2
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

extractKey :: TypeU -> TVar
extractKey (VarU v) = v
extractKey (ForallU _ t) = extractKey t
extractKey (AppU t _) = extractKey t
extractKey (NamU _ v _ _) = v
extractKey (ExistU v _ _) = v
extractKey (ThunkU t) = extractKey t
extractKey t = error $ "Cannot currently handle functional type imports: " <> show t

type2typeu :: Type -> TypeU
type2typeu (VarT v) = VarU v
type2typeu (UnkT v) = ForallU v (VarU v)
type2typeu (FunT ts t) = FunU (map type2typeu ts) (type2typeu t)
type2typeu (AppT v ts) = AppU (type2typeu v) (map type2typeu ts)
type2typeu (NamT o n ps rs) = NamU o n (map type2typeu ps) [(k, type2typeu x) | (k, x) <- rs]
type2typeu (ThunkT t) = ThunkU (type2typeu t)

unresolvedType2type :: TypeU -> Type
unresolvedType2type (VarU v) = VarT v
unresolvedType2type ExistU {} = error "Cannot cast existential type to Type"
unresolvedType2type (ForallU _ _) = error "Cannot cast universal type as Type"
unresolvedType2type (FunU ts t) = FunT (map unresolvedType2type ts) (unresolvedType2type t)
unresolvedType2type (AppU v ts) = AppT (unresolvedType2type v) (map unresolvedType2type ts)
unresolvedType2type (NamU t n ps rs) = NamT t n (map unresolvedType2type ps) [(k, unresolvedType2type e) | (k, e) <- rs]
unresolvedType2type (ThunkU t) = ThunkT (unresolvedType2type t)

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
    allVars (ThunkU t) = allVars t
    allVars t = free t

----- Pretty instances -------------------------------------------------------

instance Pretty NamType where
  pretty = viaShow

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
      f _ (ThunkT t) = "{" <> f True t <> "}"
      f False t = parens (f True t)
      f _ (FunT [] t) = "() -> " <> f False t
      f _ (FunT ts t) = hsep $ punctuate " -> " (map (f False) (ts <> [t]))
      f _ (AppT t ts) = hsep (map (f False) (t : ts))
      f _ (NamT o n ps rs) =
        block
          4
          (viaShow o <+> pretty n <> encloseSep "<" ">" "," (map pretty ps))
          (vsep [pretty k <+> "::" <+> pretty x | (k, x) <- rs])

instance Pretty TypeU where
  pretty t0 = f True t0
    where
      f _ (VarU v) = pretty v
      f _ (ExistU v ([], _) ([], _)) = angles $ pretty v
      f _ (AppU (VarU (TV "List")) [t]) = "[" <> f True t <> "]"
      f _ (AppU (VarU (TV "Tuple2")) ts) = encloseSep "(" ")" ", " (map (f True) ts)
      f _ (AppU (VarU (TV "Tuple3")) ts) = encloseSep "(" ")" ", " (map (f True) ts)
      f _ (AppU (VarU (TV "Tuple4")) ts) = encloseSep "(" ")" ", " (map (f True) ts)
      f _ (AppU (VarU (TV "Tuple5")) ts) = encloseSep "(" ")" ", " (map (f True) ts)
      f _ (AppU (VarU (TV "Tuple6")) ts) = encloseSep "(" ")" ", " (map (f True) ts)
      f _ (AppU (VarU (TV "Tuple7")) ts) = encloseSep "(" ")" ", " (map (f True) ts)
      f _ (AppU (VarU (TV "Tuple8")) ts) = encloseSep "(" ")" ", " (map (f True) ts)
      f _ (ThunkU t) = "{" <> f True t <> "}"
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
      f _ (NamU o n ps rs) =
        block
          4
          (viaShow o <+> pretty n <> encloseSep "<" ">" "," (map pretty ps))
          (vsep [pretty k <+> "::" <+> f True x | (k, x) <- rs])

instance Pretty EType where
  pretty (EType t (Set.toList -> cs) _) = case cs of
    [] -> pretty t
    [c] -> pretty c <+> "=>" <+> pretty t
    _ -> tupled (map pretty cs) <+> "=>" <+> pretty t

instance Pretty Constraint where
  pretty (Constraint cls ts) = pretty cls <+> hsep (map pretty ts)
