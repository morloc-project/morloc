{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Morloc.CodeGenerator.Grammars.Common
Description : A common set of utility functions for language templates
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.CodeGenerator.Grammars.Common
  ( typeOfTypeM
  , invertSerialManifold
  , PoolDocs(..)
  , mergePoolDocs
  ) where

import Morloc.Data.Doc
import Morloc.CodeGenerator.Namespace
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.CodeGenerator.Serial as MCS

import qualified Data.Set as Set

-- Stores pieces of code made while building a pool
data PoolDocs = PoolDocs
  { poolCompleteManifolds :: [MDoc]
    -- ^ completely generated manifolds
  , poolExpr :: MDoc
    -- ^ the inplace expression
  , poolPriorLines :: [MDoc]
    -- ^ lines to precede the returned expression
  , poolPriorExprs :: [MDoc]
    -- ^ expressions that should precede this manifold, may include helper
    -- functions or imports
  }

-- | Merge a series of pools, keeping prior lines, expression and manifolds, but
-- merging bodies with a function. For example, merge all elements in a list and
-- process the poolExpr variales into list syntax in the given language.
mergePoolDocs :: ([MDoc] -> MDoc) -> [PoolDocs] -> PoolDocs
mergePoolDocs f ms = PoolDocs
    { poolCompleteManifolds = concatMap poolCompleteManifolds ms
    , poolExpr = f (map poolExpr ms)
    , poolPriorLines = concatMap poolPriorLines ms
    , poolPriorExprs = concatMap poolPriorExprs ms
    }


-- see page 112 of my super-secret notes ...
-- example:
-- > f [g x, 42] (h 1 [1,2])
-- converts to:
-- > let a0 = g x
-- > in let a1 = [a0, 42]
-- >    in let a2 = [1,2]
-- >       in let a3 = h 1 a2
-- >          in f a1 a3
-- expression inversion will not alter expression type
invertSerialManifold :: SerialManifold -> MorlocMonad SerialManifold
invertSerialManifold  = return

-- invertExprM :: ExprM f -> MorlocMonad (ExprM f)
-- invertExprM e0 = do
--   MM.setCounter (maxLetIndex e0)
--   invertExprM' e0
--   where
--
--   invertExprM' :: ExprM f -> MorlocMonad (ExprM f)
--   invertExprM' (ManifoldM m l t form e) = do
--     -- There may already be let expressions in this manifold.
--     -- If so, we want to start our index, which is used for assigning unique
--     -- names, to the max let index + 1.
--     e' <- invertExprM' e
--     MM.sayVVV "invertExprM' ManifoldM"
--     return $ ManifoldM m l t form e'
--   invertExprM' (SerializeM t p e) = do
--     e' <- invertExprM' e
--     v <- MM.getCounter
--     MM.sayVVV $ "invertExprM' SerializeM" <+> pretty v
--     return $ dependsOn (LetM v (Serial t) (SerializeM t p (terminalOf e')) (LetVarM (Serial t) v)) e'
--   invertExprM' (DeserializeM t p e) = do
--     e' <- invertExprM' e
--     v <- MM.getCounter
--     MM.sayVVV $ "invertExprM' DeserializeM" <+> pretty v
--     return $ dependsOn (LetM v (Native t) (DeserializeM t p (terminalOf e')) (LetVarM (Native t) v)) e'
--   invertExprM' (ReturnM t e) = do
--     e' <- invertExprM' e
--     MM.sayVVV "invertExprM' ReturnM"
--     return $ dependsOn (ReturnM t (terminalOf e')) e'
--   invertExprM' (LetM v t e1 e2) = do
--     e1' <- invertExprM' e1
--     e2' <- invertExprM' e2
--     MM.sayVVV "invertExprM' LetM"
--     return $ LetM v t e1' e2'
--   invertExprM' (CisAppM t f es) = do
--     f' <- invertExprM' f
--     es' <- mapM invertExprM' es
--     v <- MM.getCounter
--     let appM' = LetM v t (CisAppM t (terminalOf f') (map terminalOf es')) (LetVarM t v)
--     MM.sayVVV $ "invertExprM' CisAppM" <+> pretty v
--     return $ foldl dependsOn appM' (f':es')
--   invertExprM' (AccM t e k) = do
--     e' <- invertExprM' e
--     MM.sayVVV "invertExprM' AccM"
--     return $ dependsOn (AccM t (terminalOf e') k) e'
--   invertExprM' (ListM t es) = do
--     es' <- mapM invertExprM' es
--     v <- MM.getCounter
--     let e = LetM v (Native t) (ListM t (map terminalOf es')) (LetVarM (Native t) v)
--         e' = foldl dependsOn e es'
--     MM.sayVVV $ "invertExprM' ListM" <+> pretty v
--     return e'
--   invertExprM' (TupleM t es) = do
--     es' <- mapM invertExprM' es
--     v <- MM.getCounter
--     let e = LetM v (Native t) (TupleM t (map terminalOf es')) (LetVarM (Native t) v)
--         e' = foldl dependsOn e es'
--     MM.sayVVV $ "invertExprM' TupleM" <+> pretty v
--     return e'
--   invertExprM' (RecordM t entries) = do
--     es' <- mapM (invertExprM' . snd) entries
--     v <- MM.getCounter
--     let entries' = zip (map fst entries) (map terminalOf es')
--         e = LetM v (Native t) (RecordM t entries') (LetVarM (Native t) v)
--         e' = foldl dependsOn e es'
--     MM.sayVVV $ "invertExprM' RecordM" <+> pretty v
--     return e'
--   invertExprM' e = do
--     MM.sayVVV "invertExprM' e"
--     return e
--
-- maxLetIndex :: ExprM f -> Int
-- maxLetIndex (LetM i _ e1 e2) = foldl max (i + 1) [maxLetIndex e1, maxLetIndex e2]
-- maxLetIndex (CisAppM _ e es) = foldl max (maxLetIndex e) (map maxLetIndex es)
-- maxLetIndex (AccM _ e _) = maxLetIndex e
-- maxLetIndex (LetVarM _ i) = i + 1
-- maxLetIndex (ListM _ es) = foldl max 0 $ map maxLetIndex es
-- maxLetIndex (TupleM _ es) = foldl max 0 $ map maxLetIndex es
-- maxLetIndex (RecordM _ rs) = foldl max 0 $ map (maxLetIndex . snd) rs
-- maxLetIndex (SerializeM _ _ e) = maxLetIndex e
-- maxLetIndex (DeserializeM _ _ e) = maxLetIndex e
-- maxLetIndex (ReturnM _ e) = maxLetIndex e
-- maxLetIndex _ = 0
--
-- -- transfer all let-dependencies from y to x
-- --
-- -- Technically, I should check for variable reuse in the let-chain and
-- -- resolve conflicts be substituting in fresh variable names. However, for
-- -- now, I will trust that my name generator created names that are unique
-- -- within the manifold.
-- dependsOn :: ExprM f -> ExprM f -> ExprM f
-- dependsOn x (LetM t v e y) = LetM t v e (dependsOn x y)
-- dependsOn x _ = x
--
-- -- get the rightmost expression in a let-tree
-- terminalOf :: ExprM f -> ExprM f
-- terminalOf (LetM _ _ _ e) = terminalOf e
-- terminalOf e = e

typeOfTypeM :: TypeM -> Maybe TypeF
typeOfTypeM Passthrough = Nothing
typeOfTypeM (Serial c) = Just c
typeOfTypeM (Native c) = Just c
typeOfTypeM (Function ins out) = FunF <$> mapM typeOfTypeM ins <*> typeOfTypeM out
