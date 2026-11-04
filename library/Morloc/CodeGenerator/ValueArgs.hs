{- |
Module      : Morloc.CodeGenerator.ValueArgs
Description : The declared kind of each argument of an applied type
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

A type's non-`Type` parameters are erased (KIND-2): a dimension, a label or
a table's row holds no value of its own. A ground row has the shape of an
anonymous record, so whether it is a row or a record type is read from the
parameter it fills.
-}
module Morloc.CodeGenerator.ValueArgs
  ( declaredKinds
  , argKinds
  , valueArgsT
  , appValueArgsT
  ) where

import qualified Data.Map as Map
import qualified Morloc.BaseTypes as BT
import Morloc.CodeGenerator.Namespace

-- | The kind of each declared parameter of a general type name applied to
-- @n@ arguments; Nothing when no definition of the name takes @n@
-- parameters. A specific parameter is `Type`-kinded.
declaredKinds :: Scope -> TVar -> Int -> Maybe [Kind]
declaredKinds gscope v n = case [ps | (ps, _, _, _, _) <- Map.findWithDefault [] v gscope, length ps == n] of
  (ps : _) -> Just (map paramKind ps)
  [] -> Nothing
  where
    paramKind (Left (_, k)) = k
    paramKind (Right _) = KindType

-- | For each argument of @v@ applied to @n@ arguments: its declared kind,
-- Nothing where no definition takes @n@ parameters, and whether a ground row
-- there fills a Rec parameter rather than standing for a record type. A
-- table's row is its row whatever the declaration says.
argKinds :: Scope -> TVar -> Int -> [(Maybe Kind, Bool)]
argKinds gscope v n = case declaredKinds gscope v n of
  Just ks -> [(Just k, BT.isTableVar v || k /= KindType) | k <- ks]
  Nothing -> replicate n (Nothing, BT.isTableVar v || takesRow)
  where
    takesRow = or [k == KindRec | (ps, _, _, _, _) <- Map.findWithDefault [] v gscope, Left (_, k) <- ps]

-- | The arguments of @v@ that are runtime values: its type arguments, less a
-- ground row where 'argKinds' places a row.
valueArgsT :: Scope -> TVar -> [ArgT] -> [Type]
valueArgsT gscope v as =
  [t | ((_, rowHere), TyA t) <- zip (argKinds gscope v (length as)) as, not (rowHere && isRow t)]
  where
    isRow (NamT NamRecord r [] _) = r == anonRecordVar
    isRow _ = False

-- | The value arguments of an application, by its head; an application
-- whose head is not a type name keeps every argument.
appValueArgsT :: Scope -> Type -> [ArgT] -> [Type]
appValueArgsT gscope (VarT v) ts = valueArgsT gscope v ts
appValueArgsT _ _ ts = tyArgs ts
