{-|
Module      : Morloc.CodeGenerator.Generate
Description : Short description
Copyright   : (c) Zebulun Arendsee, 2019
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.CodeGenerator.Generate
( 
  generate 
) where

import qualified Data.Map as Map
import qualified Data.Text as MT
import Morloc.CodeGenerator.Grammars.Common (Grammar)
import Morloc.Monad as MM
import Data.Scientific (Scientific)
import Control.Monad ((>=>))

import Morloc.Namespace

data SAnno a = Annotation (SExpr a) a deriving (Show, Ord, Eq)

data SExpr a
  = UniS
  | VarS EVar
  | ListS [SAnno a]
  | TupleS [SAnno a]
  | LamS [EVar] (SAnno a)
  | AppS (SAnno a) [SAnno a]
  | NumS Scientific
  | LogS Bool
  | StrS MT.Text
  | RecS [(EVar, SAnno a)]
  deriving (Show, Ord, Eq)

data SerialMap = SerialMap {
    packers :: Map.Map Type (Name, Path)
  , unpackers :: Map.Map Type (Name, Path)
}

generate :: [Module] -> MorlocMonad (Script, [Script])
generate ms = do
  smap <- findSerializers ms
  ast <- connect ms
  generateScripts smap ast

generateScripts
  :: SerialMap
  -> [SAnno (EType, Maybe EType, Int)]
  -> MorlocMonad (Script, [Script])
generateScripts smap es
  = (,)
  <$> makeNexus [t | (Annotation _ t) <- es]
  <*> (mapM (codify smap) es >>= segregate >>= mapM selectGrammar >>= mapM makePool)

-- | EType alone is sufficient to create the nexus. The nexus needs to know 1)
-- the type of each command it calls, 2) the language of each type (to
-- determine the pool), and the ID of each function (since calls are by
-- manifold ID).
makeNexus :: [(EType, Maybe EType, Int)] -> MorlocMonad Script
makeNexus = undefined

makePool :: (Grammar, [SAnno (EType, Int, MDoc)]) -> MorlocMonad Script
makePool = undefined

findSerializers :: [Module] -> MorlocMonad SerialMap
findSerializers ms = undefined

-- | Create one tree for each nexus command.
connect :: [Module] -> MorlocMonad [SAnno (EType, Maybe EType, Int)]
connect ms = mapM (collect ms >=> realize >=> enumerate) (findRoots ms)

collect :: [Module] -> (Expr, Module) -> MorlocMonad (SAnno [EType])
collect ms (AnnE UniE ts, m) = sanno UniS m ts
collect ms (AnnE (VarE v) ts, m) = sanno (VarS v) m ms ts 
collect ms (AnnE (ListE es) ts, m) = do
  es' <- mapM (collect ms) [(e,m) | e <- es]
  sanno (ListS es') m ms ts
collect ms (AnnE (TupleE es) ts, m) = do
  es' <- mapM (collect ms) [(e,m) | e <- es]
  sanno (TupleS es') m ms ts
collect ms (AnnE (RecE es) ts, m) = do
  es' <- mapM (\x -> collect ms (x, m)) (map snd es)
  sanno (RecS (zip (map fst es) es')) m ms ts
collect ms (AnnE (LamE v e) ts, m) = do
  e' <- collect ms (e, m)
  case e' of
    (Annotation (LamS vs e'') t) -> return $ Annotation (LamS (v:vs) e'') t
    e''@(Annotation _ t) -> return $ Annotation (LamS [v] e'') t
collect ms (AnnE (AppE e1 e2) ts, m) = do
  e1' <- collect ms (e1, m)
  e2' <- collect ms (e2, m)
  case e1' of
    (Annotation (AppS f es) t) -> return $ Annotation (AppS f (e2':es)) t
    f@(Annotation _ t) -> return $ Annotation (AppS f [e2']) t
collect ms (AnnE (LogE e) ts, m) = sanno (LogS e) m ms ts
collect ms (AnnE (NumE e) ts, m) = sanno (NumS e) m ms ts
collect ms (AnnE (StrE e) ts, m) = sanno (StrS e) m ms ts
collect _ _ = MM.throwError . OtherError $ "Unexpected type in collect"

sanno :: SExpr [EType] -> Module -> [Module] -> [Type] -> MorlocMonad (SAnno [EType])
sanno e m ts = return $ Annotation e (toEType m ms ts)

toEType :: Module -> [Module] -> [Type] -> [EType]
toEType = undefined

findSignature :: TVar -> Module -> [Module] -> TypeSet
findSignature = undefined

-- | Select a single concrete language for each sub-expression. Store the
-- concrete type and the general type (if available).
realize :: SAnno [EType] -> MorlocMonad (SAnno (EType, Maybe EType))
realize = undefined

enumerate :: SAnno (EType, Maybe EType) -> MorlocMonad (SAnno (EType, Maybe EType, Int))
enumerate = undefined

codify
  :: SerialMap
  -> SAnno (EType, Maybe EType, Int)
  -> MorlocMonad (SAnno (EType, Int, MDoc))
codify = undefined

selectGrammar :: (Lang, a) -> MorlocMonad (Grammar, a)
selectGrammar = undefined

segregate :: [SAnno (EType, Int, MDoc)] -> MorlocMonad [(Lang, [SAnno (EType, Int, MDoc)])]
segregate = undefined

findRoots :: [Module] -> [(Expr, Module)]
findRoots [] = []
findRoots ms = case filter isRoot ms of
  [] -> []
  ms' -> catMaybes . concat $ map (\m -> map (findExpr m) (moduleExports m)) ms'
  where
    -- is this module a "root" module?
    -- a root module is a module that is not imported from any other module
    isRoot :: Module -> Bool
    isRoot m = elem (moduleName m)
                    (concat $ map (map importModuleName) (map moduleImports ms))

    findExpr :: Module -> EVar -> Maybe (Expr, Module)
    findExpr m v = case [(e,m) | (Declaration v e) <- moduleBody m, elem v (moduleExports m)] of
      (x:_) -> Just x
      _ -> case [findExpr m' v | m' <- ms, elem (moduleName m') (map importModuleName (moduleImports m))] of
        (x:_) -> x
        _ -> Nothing
