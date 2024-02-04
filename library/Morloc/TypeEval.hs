{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.TypeEval
Description : Functions for evaluating type expressions
Copyright   : (c) Zebulun Arendsee, 2024
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.TypeEval (
  evaluateType,
  transformType,
  evaluateStep,
  pairEval
) where

import Morloc.Namespace
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.Data.Map as Map
import qualified Data.Set as Set

-- Evaluate an expression with both the concrete and general scopes.
--
-- First try to resolve an expression with the concrete scope
-- If this fails, resolve one step with the general scope.
pairEval
  :: Scope -- concrete scope
  -> Scope -- general scope
  -> TypeU
  -> Either MorlocError TypeU
pairEval cscope gscope
  -- transform the concrete type until an unresolvable node is reached
  = generalTransformType Set.empty id resolveGen cscope
  where
    -- resolve by attempting to evaluate one step as in the general scope
    resolveGen f bnd t
      = case generalTransformType bnd (\_ _ -> return) resolveFail gscope t of
        (Right t') -> if t' /= t
          -- if general resolution succeedds, continue evaluation with the concrete scope
          then f bnd t'
          -- if it fails, return to the concrete scope to handle failure without
          -- general resolution option
          else generalTransformType Set.empty id resolveFail cscope t
        -- if no resolution is possible, propagate the error
        e -> e

evaluateStep :: Scope -> TypeU -> Maybe TypeU
evaluateStep scope t0 =
  case generalTransformType Set.empty (\_ _ -> return) resolveFail scope t0 of
    (Left _) -> Nothing
    (Right t) -> Just t

-- evaluate a type until terminal functions called, fail if termini are not reached
transformType :: Scope -> TypeU -> Either MorlocError TypeU
transformType = generalTransformType Set.empty id resolveFail

-- evaluate a type as far as possible given the type functions in scope
evaluateType :: Scope -> TypeU -> Either MorlocError TypeU
evaluateType = generalTransformType Set.empty id resolveIgnore

resolveIgnore
  :: (Set.Set TVar -> TypeU -> Either MorlocError TypeU)
  -> Set.Set TVar
  -> TypeU
  -> Either MorlocError TypeU
resolveIgnore f bnd (AppU (VarU v) ts) = AppU (VarU v) <$> mapM (f bnd) ts
resolveIgnore _ _ t@(VarU _) = return t
resolveIgnore _ _ _ = error "Reached unexpected branch"

resolveFail
  :: (Set.Set TVar -> TypeU -> Either MorlocError TypeU)
  -> Set.Set TVar
  -> TypeU
  -> Either MorlocError TypeU
resolveFail _ _ (AppU (VarU v) _) = MM.throwError $ UndefinedType v
resolveFail _ _ (VarU v) = MM.throwError $ UndefinedType v
resolveFail _ _ _ = error "Reached unexpected branch"

generalTransformType
  :: Set.Set TVar
  -> ((Set.Set TVar -> TypeU -> Either MorlocError TypeU) -> Set.Set TVar -> TypeU -> Either MorlocError TypeU)
  -> ((Set.Set TVar -> TypeU -> Either MorlocError TypeU) -> Set.Set TVar -> TypeU -> Either MorlocError TypeU)
  -> Scope -> TypeU -> Either MorlocError TypeU
generalTransformType bnd0 recurse' resolve' h = f bnd0
  where

  recurse = recurse' f
  resolve = resolve' recurse

  f :: Set.Set TVar -> TypeU -> Either MorlocError TypeU
  f bnd (ExistU v ps rs) = do
    ps' <- mapM (recurse bnd) ps
    rs' <- mapM (\(k, v') -> (,) k <$> recurse bnd v') rs
    return $ ExistU v ps' rs'
  f bnd (FunU ts t) = FunU <$> mapM (recurse bnd) ts <*> recurse bnd t
  f bnd (NamU o n ps rs) = do
    (n', o') <- case Map.lookup n h of
        -- If the record type itself is aliased, substitute the name and record form
        (Just [(_, NamU o'' n'' _ _, _)]) -> return (n'', o'')
        -- Otherwise, keep the record name and form and recurse only into children
        _ -> return (n, o)
    ts' <- mapM (recurse bnd . snd) rs
    ps' <- mapM (recurse bnd) ps
    return $ NamU o' n' ps' (zip (map fst rs) ts')

  -- type Cpp (A a b) = "map<$1,$2>" a b
  -- foo Cpp :: A D [B] -> X
  -- -----------------------------------
  -- foo :: "map<$1,$2>" D [B] -> X
  --
  -- type Foo a = (a, A)
  -- f :: Foo Int -> B
  -- -----------------
  -- f :: (Int, A) -> B
  f bnd t0@(AppU (VarU v) ts)
    | Set.member v bnd = AppU (VarU v) <$> mapM (recurse bnd) ts
    | otherwise =
        case Map.lookup v h of
          (Just (t':ts')) -> do
            (vs, newType, isTerminal) <- foldlM (mergeAliases v (length ts)) t' ts' |>> renameTypedefs bnd
            case (length ts == length vs, isTerminal) of
              -- non-equal number of type parameters
              (False, _) -> MM.throwError $ BadTypeAliasParameters v (length vs) (length ts)
              -- reached a terminal type (e.g. `"vector<$1,$2>" a b`)
              (_, True ) -> terminate bnd $ foldr parsub newType (zip vs ts)
              -- substitute the head term and re-evaluate
              (_, False) -> recurse bnd $ foldr parsub newType (zip vs ts)
          _ -> resolve bnd t0

  -- Can only apply VarU?
  f _ (AppU _ _) = undefined

  -- type Foo = A
  -- f :: Foo -> B
  -- -----------------
  -- f :: A -> B
  f bnd t0@(VarU v)
    | Set.member v bnd = return t0
    | otherwise =
     case Map.lookup v h of
      (Just []) -> return t0
      (Just ts1@(t1:_)) -> do
        -- new parameters may be added on the right that are not on the left
        (_, t2, isTerminal) <- foldlM (mergeAliases v 0) t1 ts1
        if isTerminal
          then terminate bnd t2
          else recurse bnd t2
      Nothing -> resolve bnd t0

  f bnd (ForallU v t) = ForallU v <$> recurse (Set.insert v bnd) t

  terminate :: Set.Set TVar -> TypeU -> Either MorlocError TypeU
  terminate bnd (ExistU v ts rs) = ExistU v <$> mapM (recurse bnd) ts <*> mapM (secondM (recurse bnd)) rs
  terminate bnd (FunU ts t) = FunU <$> mapM (recurse bnd) ts <*> recurse bnd t
  terminate bnd (ForallU v t) = ForallU v <$> recurse (Set.insert v bnd) t
  terminate bnd (AppU t ts) = AppU t <$> mapM (recurse bnd) ts
  terminate bnd (NamU o v ts rs) = NamU o v <$> mapM (recurse bnd) ts <*> mapM (secondM (recurse bnd)) rs
  terminate _   (VarU v) = return (VarU v)

  renameTypedefs :: Set.Set TVar -> ([TVar], TypeU, Bool) -> ([TVar], TypeU, Bool)
  renameTypedefs _ ([], t, isTerminal) = ([], t, isTerminal)
  renameTypedefs bnd (v@(TV x) : vs, t, isTerminal)
    | Set.member v bnd =
        let (vs', t', isTerminal') = renameTypedefs bnd (vs, t, isTerminal)
            v' = head [x' | x' <- [TV (MT.show' i <> x) | i <- [(0 :: Int) ..]], not (Set.member x' bnd), x' `notElem` vs']
            t'' = substituteTVar v (VarU v') t'
        in (v':vs', t'', isTerminal')
    | otherwise =
        let (vs', t', isTerminal') = renameTypedefs bnd (vs, t, isTerminal)
        in (v:vs', t', isTerminal')

  -- When a type alias is imported from two places, this function reconciles them, if possible
  mergeAliases
    :: TVar
    -> Int
    -> ([TVar], TypeU, Bool)
    -> ([TVar], TypeU, Bool)
    -> Either MorlocError ([TVar], TypeU, Bool)
  mergeAliases v i t@(ts1, t1, isTerminal1) (ts2, t2, isTerminal2)
    | i /= length ts1 = MM.throwError $ BadTypeAliasParameters v i (length ts1)
    |    isSubtypeOf t1' t2'
      && isSubtypeOf t2' t1'
      && length ts1 == length ts2
      && isTerminal1 == isTerminal2 = return t
    | otherwise = MM.throwError (ConflictingTypeAliases t1 t2)
    where
      t1' = foldl (flip ForallU) t1 ts1
      t2' = foldl (flip ForallU) t2 ts2

-- Replace a type variable with an expression. For example:
-- parsub ("a", "Int") -> "Map a b" -> "Map Int b"
parsub :: (TVar, TypeU) -> TypeU -> TypeU
parsub (v, t2) t1@(VarU v0)
  | v0 == v = t2 -- substitute
  | otherwise = t1 -- keep the original
parsub _ ExistU{} = error "What the bloody hell is an existential doing down here?"
parsub pair (ForallU v t1) = ForallU v (parsub pair t1)
parsub pair (FunU ts t) = FunU (map (parsub pair) ts) (parsub pair t)
parsub pair (AppU t ts) = AppU (parsub pair t) (map (parsub pair) ts)
parsub pair (NamU o n ps rs) = NamU o n (map (parsub pair) ps) [(k', parsub pair t) | (k', t) <- rs]
