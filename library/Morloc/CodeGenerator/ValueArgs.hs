{- |
Module      : Morloc.CodeGenerator.ValueArgs
Description : Which arguments of an applied type are runtime values
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

A type's non-`Type` parameters are erased (KIND-2): a dimension, a label or
a table's row holds no value of its own. A ground row lowers to the shape of
an anonymous record, so code that collects the values a pool marshals asks
here rather than deciding by shape.
-}
module Morloc.CodeGenerator.ValueArgs
  ( declaredKinds
  , valueArgsF
  , valueArgsT
  , appValueArgs
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

-- | The arguments of @v@ that are runtime values. An argument is dropped
-- only when its declared parameter is not `Type`-kinded AND it has the
-- shape of a kind-level argument, so a wire shape that happens to line up
-- with the declaration never loses a value. An argument list that matches
-- no definition's arity drops Nat- and Str-shaped arguments, and a row
-- when the name declares a Rec-kinded parameter. A row under a table head
-- is dropped whatever the declaration says.
valueArgsF :: Scope -> TVar -> [TypeF] -> [TypeF]
valueArgsF gscope v = filterArgs gscope v isKindTypeF isRow
  where
    isRow (NamF NamRecord (FV r _) [] _) = r == anonRecordVar
    isRow _ = False

isRowT :: Type -> Bool
isRowT (NamT NamRecord r [] _) = r == anonRecordVar
isRowT _ = False

-- | 'valueArgsF' for 'Type'.
valueArgsT :: Scope -> TVar -> [Type] -> [Type]
valueArgsT gscope v = filterArgs gscope v isKind isRowT
  where
    isKind a = isKindTypeT a || isStr a
    isStr (StrLitT _) = True
    isStr StrVoidT = True
    isStr (StrConcatT _ _) = True
    isStr _ = False

-- | The value arguments of an application, by its head; an application
-- whose head is not a type name keeps every argument.
appValueArgs :: Scope -> TypeF -> [TypeF] -> [TypeF]
appValueArgs gscope (VarF (FV v _)) ts = valueArgsF gscope v ts
appValueArgs _ _ ts = ts

-- | 'appValueArgs' for 'Type'.
appValueArgsT :: Scope -> Type -> [Type] -> [Type]
appValueArgsT gscope (VarT v) ts = valueArgsT gscope v ts
appValueArgsT _ _ ts = ts

filterArgs :: Scope -> TVar -> (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterArgs gscope v isKind isRow args = case declaredKinds gscope v (length args) of
  Just ks -> [a | (a, k) <- zip args ks, keep k a]
  Nothing -> [a | a <- args, not (isKind a || (takesRow && isRow a))]
  where
    keep k a
      | tableRow a = False
      | k == KindType = True
      | otherwise = not (isKind a || isRow a)
    tableRow a = BT.isTableVar v && isRow a
    takesRow =
      BT.isTableVar v
        || or [k == KindRec | (ps, _, _, _, _) <- Map.findWithDefault [] v gscope, Left (_, k) <- ps]
