{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.CodeGenerator.Suspension
Description : Lower every suspension to a closure of no arguments
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

A suspension @<E> T@ is a thunk of a computation: a value that, when run,
may perform the effects in @E@ and yields a @T@. In the pools it is a
closure of no arguments, and it shares every code path that a closure of
one or more arguments already has: captured variables are copied into the
closure when it is built, an aggregate may hold it, and it crosses a pool
boundary as a reference back to the manifold that builds it.

After 'Morloc.CodeGenerator.EffectBoundary' has placed every suspend and
force, this pass rewrites each 'PolyDoBlock' into a 'PolyManifold' whose
form is a 'ManifoldPart' with the block's free variables as context
arguments and no bound arguments. The manifold's body is the block's
body, so calling the closure runs the block. A 'PolyEval' stays as the
force: applying the closure to no arguments.
-}
module Morloc.CodeGenerator.Suspension
  ( lowerSuspensions
  ) where

import Morloc.CodeGenerator.EffectBoundary (polyOuterType)
import Morloc.CodeGenerator.Express (polyFreeVars)
import Morloc.CodeGenerator.Namespace
import qualified Data.Set as Set
import qualified Morloc.Monad as MM

lowerSuspensions :: PolyHead -> MorlocMonad PolyHead
lowerSuspensions (PolyHead lang midx args body) =
  PolyHead lang midx args <$> walk lang body

-- | Threads the pool the expression is generated into, which is the pool
-- a new closure manifold belongs to.
walk :: Lang -> PolyExpr -> MorlocMonad PolyExpr
walk lang (PolyDoBlock (Idx cidx _) e) = walk lang e >>= suspend lang cidx
-- Forcing an intrinsic declared with a suspension result is the eager
-- intrinsic call itself: the runtime functions behind @save, @load, @try
-- and the rest run when called.
walk lang (PolyEval _ (PolyIntrinsic (Idx i (EffectT _ t)) intr xs)) =
  PolyIntrinsic (Idx i t) intr <$> mapM (walk lang) xs
walk lang (PolyEval t e) = PolyEval t <$> walk lang e
-- An intrinsic declared with a suspension result, held rather than forced,
-- is the body of a suspension: a closure that makes the eager call.
walk lang (PolyIntrinsic (Idx i (EffectT _ t)) intr xs) = do
  xs' <- mapM (walk lang) xs
  suspend lang i (PolyIntrinsic (Idx i t) intr xs')
walk _ (PolyManifold l m form k e) = PolyManifold l m form k <$> walk l e
walk _ (PolyRemoteInterface l t is rf e) = PolyRemoteInterface l t is rf <$> walk l e
walk lang (PolyLet i e1 e2) = PolyLet i <$> walk lang e1 <*> walk lang e2
walk lang (PolyReturn e) = PolyReturn <$> walk lang e
walk lang (PolyApp f xs) = PolyApp <$> walk lang f <*> mapM (walk lang) xs
walk lang (PolyCacheBody l m as e) = PolyCacheBody l m as <$> walk lang e
walk lang (PolyDebugWrap m as e) = PolyDebugWrap m as <$> walk lang e
walk lang (PolyList v ts xs) = PolyList v ts <$> mapM (walk lang) xs
walk lang (PolyTuple v xs) = PolyTuple v <$> mapM (\(t, x) -> (,) t <$> walk lang x) xs
walk lang (PolyRecord o v ps rs) =
  PolyRecord o v ps <$> mapM (\(k, (t, x)) -> (,) k . (,) t <$> walk lang x) rs
walk lang (PolyVariant t n i xs) = PolyVariant t n i <$> mapM (walk lang) xs
-- A coercion applied to a suspension acts on the value it yields, so the
-- result is a suspension that runs the original and coerces its result.
walk lang (PolyCoerce c (Idx i (EffectT _ t)) e) = do
  e' <- walk lang e
  let inner = case polyOuterType e' of
        Just (EffectT _ x) -> x
        _ -> t
  suspend lang i (PolyCoerce c (Idx i t) (PolyEval (Idx i inner) e'))
walk lang (PolyCoerce c t e) = PolyCoerce c t <$> walk lang e
walk lang (PolyIf c a b) = PolyIf <$> walk lang c <*> walk lang a <*> walk lang b
walk lang (PolyLoop t ids e) = PolyLoop t ids <$> walk lang e
walk lang (PolyLoopContinue es) = PolyLoopContinue <$> mapM (walk lang) es
walk lang (PolyIntrinsic t intr xs) = PolyIntrinsic t intr <$> mapM (walk lang) xs
walk _ leaf = return leaf

-- | The closure of no arguments whose body is the given expression, built
-- in the given pool. Its context arguments are the expression's free
-- variables, copied into the closure when it is built.
suspend :: Lang -> Int -> PolyExpr -> MorlocMonad PolyExpr
suspend lang cidx e = do
  m <- MM.freshManifoldIndex cidx
  let ctx = [Arg i None | i <- Set.toList (polyFreeVars e)]
  return $ PolyManifold lang m (ManifoldPart ctx []) Transparent (returned e)

-- | A manifold body returns its tail. The return is pushed through lets to
-- the tail; a tail that already returns is left alone.
returned :: PolyExpr -> PolyExpr
returned (PolyLet i e1 e2) = PolyLet i e1 (returned e2)
returned r@(PolyReturn _) = r
returned x = PolyReturn x
