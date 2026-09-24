{- |
Module      : Morloc.CodeGenerator.Value
Description : Which expressions are values, for strict beta-reduction
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Morloc evaluates an argument once, at the application
(@spec/types/effects.md@, law 5). Beta-reduction may therefore substitute an
argument into the body only when evaluating it does nothing: when it is
already a value. Any other argument is bound once by a @let@ at the
application, whatever the number of references to its parameter, zero
included.
-}
module Morloc.CodeGenerator.Value
  ( isValue
  , isValueWith
  , etaParts
  , isSuspension
  ) where

import Data.Foldable (toList)
import qualified Data.Text as T
import Morloc.Namespace.Expr
import Morloc.Namespace.Prim
import Morloc.Namespace.Type

-- | A value: evaluating it computes nothing (see the cases of 'isValueWith').
isValue :: Foldable f => AnnoS (Indexed Type) f c -> Bool
isValue = isValueWith (const Nothing)

-- | 'isValue', given the first stage point of each recursive function a
-- 'CallS' may name (the number of arguments after which it does work
-- before the function it returns); without one, a recursive call takes
-- every input of its type.
isValueWith :: Foldable f => (EVar -> Maybe Int) -> AnnoS (Indexed Type) f c -> Bool
isValueWith stage a@(AnnoS (Idx _ t) _ e) = case e of
  -- a partial application the typechecker wrote as a lambda is what the
  -- application is
  LamS _ _ | Just (f, pre) <- etaParts a -> isValueWith stage (AnnoS (Idx 0 t) (annC a) (AppS f pre))
  LamS _ _ -> True
  UniS -> True
  NullS -> True
  RealS _ _ -> True
  IntS _ _ -> True
  LogS _ -> True
  StrS _ -> True
  BndS _ -> True
  LetBndS _ -> True
  CallS _ -> True
  ExeS _ -> True
  DoBlockS _ -> True
  VarS _ alts -> let xs = toList alts in not (null xs) && all (isValueWith stage) xs
  LstS xs -> all (isValueWith stage) xs
  TupS xs -> all (isValueWith stage) xs
  NamS rs -> all (isValueWith stage . snd) rs
  ConS _ _ _ xs -> all (isValueWith stage) xs
  CoerceS _ x -> isValueWith stage x
  -- applying a function whose result is a suspension builds the suspension
  -- and runs nothing (spec/types/effects.md, law 5)
  AppS f xs -> isValueWith stage f && all (isValueWith stage) xs && (length xs < arity stage f || isSuspension t)
  _ -> False

annC :: AnnoS g f c -> c
annC (AnnoS _ c _) = c

-- | A partial application @f pre@ the typechecker eta-expanded into
-- @\vs -> f pre vs@ (its own parameters, named @..@@..@), as its head and
-- the arguments it applies.
etaParts :: Foldable f => AnnoS g f c -> Maybe (AnnoS g f c, [AnnoS g f c])
etaParts (AnnoS _ _ (LamS vs (AnnoS _ _ (AppS f xs))))
  | not (null vs)
  , all isEtaVar vs
  , (pre, post) <- splitAt (length xs - length vs) xs
  , not (null pre)
  , map bndName post == map Just vs
  , all (\v -> v `notElem` concatMap boundNames (f : pre)) vs =
      Just (f, pre)
  where
    isEtaVar (EV v) = T.isInfixOf (T.pack "@@") v
    bndName (AnnoS _ _ (BndS v)) = Just v
    bndName _ = Nothing
etaParts _ = Nothing

-- | The variables referenced in a tree.
boundNames :: Foldable f => AnnoS g f c -> [EVar]
boundNames (AnnoS _ _ (BndS v)) = [v]
boundNames (AnnoS _ _ e) = case e of
  VarS _ _ -> []
  _ -> concat (foldExprSList boundNames e)
  where
    foldExprSList k x = foldExprS (\y -> [k y]) x

isSuspension :: Type -> Bool
isSuspension (EffectT _ _) = True
isSuspension _ = False

-- | The number of arguments a function takes before it runs. A lambda (or a
-- term implemented by one) takes its parameters; a sourced function and a
-- recursive call take every input of their type. A function held in a
-- variable, or computed, has an unknown arity: it may run as soon as it is
-- applied to one argument, so it counts as zero.
arity :: Foldable f => (EVar -> Maybe Int) -> AnnoS (Indexed Type) f c -> Int
arity stage (AnnoS (Idx _ t) _ e) = case e of
  LamS vs _ -> length vs
  VarS _ alts -> case map (arity stage) (toList alts) of
    [] -> typeArity
    ns -> minimum ns
  ExeS _ -> typeArity
  CallS v -> fromMaybe typeArity (stage v)
  _ -> 0
  where
    typeArity = case t of
      FunT ts _ -> length ts
      _ -> 0
