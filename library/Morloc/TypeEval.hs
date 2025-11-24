{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.TypeEval
Description : Functions for evaluating type expressions
Copyright   : (c) Zebulun Arendsee, 2016-2025
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.TypeEval (
  evaluateType,
  transformType,
  evaluateStep,
  pairEval,
  reduceType
) where

import Morloc.Namespace
import Morloc.Data.Doc
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.Data.Map as Map
import qualified Data.Set as Set

-- Evaluate a type expression with both the concrete and general scopes
--
-- This function does not know the concrete language, the parent sets that.
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
          -- if general resolution succeeds, continue evaluation with the concrete scope
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

-- | evaluate a type exactly one step, return nothing if no evaluation is possible
reduceType :: Scope -> TypeU -> Maybe TypeU
reduceType scope t0 =
    case evaluateStep scope t0 of
        (Just t1) -> if t1 == t0 then Nothing else Just t1
        Nothing -> Nothing

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
  -> Scope -- may be general or concrete scope
  -> TypeU
  -> Either MorlocError TypeU
generalTransformType bnd0 recurse' resolve' scope = f bnd0
  where

  recurse = recurse' f
  resolve = resolve' recurse

  f :: Set.Set TVar -> TypeU -> Either MorlocError TypeU
  f bnd (ExistU v (ps, pc) (rs, rc)) = do
    ps' <- mapM (recurse bnd) ps
    rs' <- mapM (\(k, v') -> (,) k <$> recurse bnd v') rs
    return $ ExistU v (ps', pc) (rs', rc)
  f bnd (FunU ts t) = FunU <$> mapM (recurse bnd) ts <*> recurse bnd t
  f bnd (NamU o n ps rs) = do
    (n', o') <- case Map.lookup n scope of
        -- If the record type itself is aliased, substitute the name and record form
        (Just [(_, NamU o'' n'' _ _, _, _)]) -> return (n'', o'')
        -- Otherwise, keep the record name and form and recurse only into children
        _ -> return (n, o)
    ts' <- mapM (recurse bnd . snd) rs
    ps' <- mapM (recurse bnd) ps
    return $ NamU o' n' ps' (zip (map fst rs) ts')

  f bnd t0@(AppU (VarU v) ts)
    -- Handle generic case:
    --   type Cpp => A a b = "map<$1,$2>" a b
    --   foo Cpp :: A D [B] -> X
    --   -----------------------------------
    --   foo :: "map<$1,$2>" D [B] -> X
    --
    --   type Foo a = (a, A)
    --   f :: Foo Int -> B
    --   -----------------
    --   f :: (Int, A) -> B
    | Set.member v bnd = AppU (VarU v) <$> mapM (recurse bnd) ts
    -- Handle specialization, e.g.
    --   type Py => List Int64 = "np.ndarray" "int64"
    | otherwise =
        case Map.lookup v scope of
          (Just ts') -> do
            mergedAliases <- foldlM (mergeAliases ts) Nothing (map Just ts') |>> fmap (renameTypedefs bnd)
            case mergedAliases of
                (Just (vs, newType, _, isTerminal)) -> case isTerminal of
                   True -> terminate bnd $ foldr parsub newType (zip vs ts)
                   -- substitute the head term and re-evaluate
                   False -> recurse bnd $ foldr parsub newType (zip vs ts)
                Nothing -> MM.throwError . OtherError . render
                    $ "No matching alias found for" <+> viaShow t0
                    <+> "\n  scope" <+> viaShow scope
                    <+> "\n  ts':" <+> viaShow ts'
          _ -> resolve bnd t0

  -- t may be existential
  f bnd (AppU t ts) = AppU <$> recurse bnd t <*> mapM (recurse bnd) ts

  -- type Foo = A
  -- f :: Foo -> B
  -- -----------------
  -- f :: A -> B
  f bnd t0@(VarU v)
    | Set.member v bnd = return t0
    | otherwise = case Map.lookup v scope of
      (Just []) -> return t0
      (Just ts1) -> do
        -- new parameters may be added on the right that are not on the left
        mergedAliases <- foldlM (mergeAliases []) Nothing (map Just ts1)
        case mergedAliases of
            (Just (_, t2, _, isTerminal)) ->
                if isTerminal
                  then terminate bnd t2
                  else recurse bnd t2
            Nothing -> MM.throwError . OtherError . render
              $ "No matching alias found for" <+> viaShow t0
              <> "\n  scope:" <+> viaShow scope
              <> "\n  v:" <+> pretty v
              <> "\n  ts1:" <+> list (map viaShow ts1)
      Nothing -> resolve bnd t0

  f bnd (ForallU v t) = ForallU v <$> recurse (Set.insert v bnd) t

  terminate :: Set.Set TVar -> TypeU -> Either MorlocError TypeU
  terminate bnd (ExistU v (ts, tc) (rs, rc)) = do
    ts' <- mapM (recurse bnd) ts
    rs' <- mapM (secondM (recurse bnd)) rs
    return $ ExistU v (ts', tc) (rs', rc)
  terminate bnd (FunU ts t) = FunU <$> mapM (recurse bnd) ts <*> recurse bnd t
  terminate bnd (ForallU v t) = ForallU v <$> recurse (Set.insert v bnd) t
  terminate bnd (AppU t ts) = AppU t <$> mapM (recurse bnd) ts
  terminate bnd (NamU o v ts rs) = NamU o v <$> mapM (recurse bnd) ts <*> mapM (secondM (recurse bnd)) rs
  terminate _   (VarU v) = return (VarU v)

  renameTypedefs :: Set.Set TVar -> ([Either TVar TypeU], TypeU, CmdArg, Bool) -> ([TVar], TypeU, CmdArg, Bool)
  renameTypedefs _ ([], t, d, isTerminal) = ([], t, d, isTerminal)
  renameTypedefs bnd (Left v@(TV x) : vs, t, d, isTerminal)
    | Set.member v bnd =
        let (vs', t', d', isTerminal') = renameTypedefs bnd (vs, t, d, isTerminal)
            v' = head [x' | x' <- [TV (MT.show' i <> x) | i <- [(0 :: Int) ..]], not (Set.member x' bnd), x' `notElem` vs']
            t'' = substituteTVar v (VarU v') t'
        in (v':vs', t'', d', isTerminal')
    | otherwise =
        let (vs', t', d', isTerminal') = renameTypedefs bnd (vs, t, d, isTerminal)
        in (v:vs', t', d', isTerminal')
  renameTypedefs bnd (Right _ : vs, t, d, isTerminal)
    = renameTypedefs bnd (vs, t, d, isTerminal)

  -- When a type alias is imported from two places, this function reconciles them, if possible
  mergeAliases
    :: [TypeU]
    -> Maybe ([Either TVar TypeU], TypeU, CmdArg, Bool)
    -> Maybe ([Either TVar TypeU], TypeU, CmdArg, Bool)
    -> Either MorlocError (Maybe ([Either TVar TypeU], TypeU, CmdArg, Bool))
  mergeAliases _ Nothing Nothing = Right Nothing
  mergeAliases tsMain Nothing (Just b)
    | checkAlias tsMain b = Right (Just b)
    | otherwise = Right Nothing
  mergeAliases tsMain (Just a) Nothing
    | checkAlias tsMain a = Right (Just a)
    | otherwise = Right Nothing
  -- TODO: should the docstring args be considered here?
  mergeAliases tsMain (Just a@(ts1, t1, _, isTerminal1)) (Just b@(ts2, t2, _, isTerminal2))
    -- if both are invalid, return nothing
    | not aIsValid && not bIsValid = Right Nothing
    -- if one is valid and the other isn't, return the valid one
    | aIsValid && not bIsValid = Right (Just a)
    | not aIsValid && bIsValid = Right (Just b)
    -- if they are both valid AND they are identical AND there is no specialization, return the first
    |
      -- the return types are the same
         isSubtypeOf t1 t2
      && isSubtypeOf t2 t1
      -- there is no specialization
      && nonspecialized
      -- the return type is concrete, not an alias for something else
      && isTerminal1 == isTerminal2 = return (Just a)
    -- handle specialization
    | not nonspecialized = return $ selectSpecialization a b
    | otherwise = MM.throwError (ConflictingTypeAliases t1 t2)
    where
      aIsValid = checkAlias tsMain a
      bIsValid = checkAlias tsMain b
      -- True if all parameters in both aliases are generic
      nonspecialized = all
        (\(x, y) -> either (\ _ -> either (const True) (const False) y) (const False) x)
        (zip ts1 ts2)

  selectSpecialization
    :: ([Either TVar TypeU], TypeU, CmdArg, Bool)
    -> ([Either TVar TypeU], TypeU, CmdArg, Bool)
    -> Maybe ([Either TVar TypeU], TypeU, CmdArg, Bool)
  selectSpecialization a@(aps0, _, _, _) b@(bps0, _, _, _) = g aps0 bps0 where 
      g [] _ = Just a
      g _ [] = Just b
      g ((Right _):_) ((Left _):_) = Just a
      g ((Left _):_) ((Right _):_) = Just b
      g ((Left _):aps) ((Left _):bps) = g aps bps
      g ((Right ta):aps) ((Right tb):bps)
        | isSubtypeOf ta tb && isSubtypeOf tb ta = g aps bps
        | isSubtypeOf ta tb && not (isSubtypeOf tb ta) = Just b
        | not (isSubtypeOf ta tb) && isSubtypeOf tb ta = Just a
        | otherwise = Nothing

  checkAlias
    :: [TypeU]
    -> ([Either TVar TypeU], TypeU, CmdArg, Bool)
    -> Bool
  checkAlias ts1 (ts2, _, _, _) =
    length ts1 == length ts2 &&
    all (\(x, y) -> either (const True) (\ytype -> isSubtypeOf ytype x) y) (zip ts1 ts2)

-- Replace a type variable with an expression. For example:
-- parsub ("a", "Int") -> "Map a b" -> "Map Int b"
parsub :: (TVar, TypeU) -> TypeU -> TypeU
parsub (v, t2) t1@(VarU v0)
  | v0 == v = t2 -- substitute
  | otherwise = t1 -- keep the original
parsub pair (ExistU t (ts, tc) (rs, rc)) = ExistU t (map (parsub pair) ts, tc) (zip (map fst rs) (map (parsub pair . snd) rs), rc)
parsub pair (ForallU v t1) = ForallU v (parsub pair t1)
parsub pair (FunU ts t) = FunU (map (parsub pair) ts) (parsub pair t)
parsub pair (AppU t ts) = AppU (parsub pair t) (map (parsub pair) ts)
parsub pair (NamU o n ps rs) = NamU o n (map (parsub pair) ps) [(k', parsub pair t) | (k', t) <- rs]
