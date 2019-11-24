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

import Morloc.Namespace
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import Morloc.CodeGenerator.Grammars.Common (Grammar)
import Data.Scientific (Scientific)
import Control.Monad ((>=>))
import qualified Data.Map as Map
import qualified Data.Set as Set

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

data Meta = Meta {
    metaGeneralType :: Maybe Type
  , metaProperties :: Set.Set Property
  , metaConstraints :: Set.Set Constraint
  , metaSource :: Maybe Source
  , metaModule :: MVar
  , metaId :: Int
  -- -- there should be morloc source info here, for great debugging
  -- metaMorlocSource :: Path
  -- metaMorlocSourceLine :: Int
  -- metaMorlocSourceColumn :: Int
}

generate :: [Module] -> MorlocMonad (Script, [Script])
generate ms = do
  MM.startCounter -- initialize state counter to 0, used to index manifolds
  smap <- findSerializers ms
  ast <- connect ms
  generateScripts smap ast

generateScripts
  :: SerialMap
  -> [SAnno (Type, Meta)]
  -> MorlocMonad (Script, [Script])
generateScripts smap es
  = (,)
  <$> makeNexus [t | (Annotation _ t) <- es]
  <*> (mapM (codify smap) es >>= segregate >>= mapM selectGrammar >>= mapM makePool)

-- | Type alone is sufficient to create the nexus. The nexus needs to know 1)
-- the type of each command it calls, 2) the language of each type (to
-- determine the pool), and the ID of each function (since calls are by
-- manifold ID).
makeNexus :: [(Type, Meta)] -> MorlocMonad Script
makeNexus = undefined

makePool :: (Grammar, [SAnno (Type, Meta, MDoc)]) -> MorlocMonad Script
makePool = undefined

findSerializers :: [Module] -> MorlocMonad SerialMap
findSerializers ms = undefined

-- | Create one tree for each nexus command.
connect :: [Module] -> MorlocMonad [SAnno (Type, Meta)]
connect ms = do
  let modmap = Map.fromList [(moduleName m, m) | m <- ms] 
  mapM (collect modmap >=> realize) (findRoots modmap)

collect :: Map.Map MVar Module -> (Expr, EVar, MVar) -> MorlocMonad (SAnno [(Type, Meta)])
collect ms (e, ev, mv) = root where
  root :: MorlocMonad (SAnno [(Type, Meta)])
  root = undefined

  -- TODO: I need to couple metadata to the manifolds function calls. Each call
  -- must have a path to source code and may optionally have properties and
  -- constraints. I probably need to replace MVar with some specialized object
  -- for storing metadata. This is an important hook for future features (such
  -- as documentation, caching options, runtime data, etc).
  collect' :: (Expr, MVar) -> MorlocMonad (SAnno [(Type, Meta)])
  collect' (AnnE UniE ts, m) = simpleCollect UniS ts m
  -- TODO: I need to store v somewhere, if I have v and the module name, I can
  -- get everything else that I might need when I need it.
  collect' (AnnE (VarE v) ts, m) = evaluateVariable m v ts
  collect' (AnnE (ListE es) ts, m) = do
    es' <- mapM collect' [(e,m) | e <- es]
    simpleCollect (ListS es') ts m
  collect' (AnnE (TupleE es) ts, m) = do
    es' <- mapM collect' [(e,m) | e <- es]
    simpleCollect (TupleS es') ts m
  collect' (AnnE (RecE es) ts, m) = do
    es' <- mapM (\x -> collect' (x, m)) (map snd es)
    simpleCollect (RecS (zip (map fst es) es')) ts m
  collect' (AnnE (LamE v e) ts, m) = do
    e' <- collect' (e, m)
    case e' of
      (Annotation (LamS vs e'') t) -> return $ Annotation (LamS (v:vs) e'') t
      e''@(Annotation _ t) -> return $ Annotation (LamS [v] e'') t
  collect' (AnnE (AppE e1 e2) ts, m) = do
    e1' <- collect' (e1, m)
    e2' <- collect' (e2, m)
    case e1' of
      (Annotation (AppS f es) t) -> return $ Annotation (AppS f (e2':es)) t
      f@(Annotation _ t) -> return $ Annotation (AppS f [e2']) t
  collect' (AnnE (LogE e) ts, m) = simpleCollect (LogS e) ts m
  collect' (AnnE (NumE e) ts, m) = simpleCollect (NumS e) ts m
  collect' (AnnE (StrE e) ts, m) = simpleCollect (StrS e) ts m
  collect _ _ = MM.throwError . OtherError $ "Unexpected type in collect"

  getGeneralType :: [Type] -> MorlocMonad (Maybe Type)
  getGeneralType ts = case [t | t <- ts, langOf' t == MorlocLang] of 
      [] -> return Nothing
      [x] -> return $ Just x
      xs -> MM.throwError . OtherError $ "Expected 0 or 1 general types, found " <> MT.show' (length xs)

  simpleCollect :: SExpr [(Type, Meta)] -> [Type] -> MVar -> MorlocMonad (SAnno [(Type, Meta)])
  simpleCollect x ts v = do
    i <- MM.getCounter
    generalType <- getGeneralType ts
    let meta = Meta { metaGeneralType = generalType
                    , metaProperties = Set.empty
                    , metaConstraints = Set.empty
                    , metaSource = Nothing
                    , metaModule = v
                    , metaId = i
                    }
    return $ Annotation x [(t, meta) | t <- ts] 

  makeVarMeta :: EVar -> MVar -> [Type] -> MorlocMonad Meta
  makeVarMeta evar mvar ts = do
    i <- MM.getCounter
    generalType <- getGeneralType ts
    let typeset = lookupTypeSet evar mvar ms 
    return $ Meta { metaGeneralType = generalType
                  , metaProperties = Set.empty
                  , metaConstraints = Set.empty
                  , metaSource = Nothing
                  , metaModule = mvar
                  , metaId = i
                  }

  -- | Evaluate a variable. If it was imported, lookup of the module it came from.
  evaluateVariable :: MVar -> EVar -> [Type] -> MorlocMonad (SAnno ([(Type, Meta)]))
  evaluateVariable e m = undefined

-- | Find the first source for a term sourced from a given language relative to a given module 
lookupSource :: EVar -> MVar -> Lang -> Map.Map MVar Module -> Maybe Source
lookupSource evar mvar lang ms =
  case Map.lookup mvar ms |>> moduleSourceMap >>= Map.lookup (evar, lang) of
    (Just src) -> Just src
    Nothing -> Map.lookup mvar ms
            |>> moduleImportMap
            |>> Map.elems
            |>> mapMaybe (\mvar' -> lookupSource evar mvar' lang ms)
            >>= listToMaybe

-- | Find the first typeset defined for a term relative to a given module
lookupTypeSet :: EVar -> MVar -> Map.Map MVar Module -> Maybe TypeSet
lookupTypeSet evar mvar ms =
  case Map.lookup mvar ms |>> moduleTypeMap >>= Map.lookup evar of
    (Just tm) -> Just tm
    Nothing ->  Map.lookup mvar ms
            |>> moduleImportMap
            |>> Map.elems
            |>> mapMaybe (\mvar' -> lookupTypeSet evar mvar' ms)
            >>= listToMaybe

-- | Select a single concrete language for each sub-expression. Store the
-- concrete type and the general type (if available).
realize :: SAnno [(Type, Meta)] -> MorlocMonad (SAnno (Type, Meta))
realize = undefined

codify
  :: SerialMap
  -> SAnno (Type, Meta)
  -> MorlocMonad (SAnno (Type, Meta, MDoc))
codify = undefined

selectGrammar :: (Lang, a) -> MorlocMonad (Grammar, a)
selectGrammar = undefined

segregate :: [SAnno (Type, Meta, MDoc)] -> MorlocMonad [(Lang, [SAnno (Type, Meta, MDoc)])]
segregate = undefined

findRoots :: Map.Map MVar Module -> [(Expr, EVar, MVar)]
findRoots ms
  = catMaybes
  . Set.toList
  . mapSum
  . Map.map (\m -> Set.map (findExpr m) (moduleExports m))
  . Map.filter isRoot
  $ ms where
    -- is this module a "root" module?
    -- a root module is a module that is not imported from any other module
    isRoot :: Module -> Bool
    isRoot m = Set.member (moduleName m) allImports

    -- set of all modules that are imported
    allImports = mapSumWith (valset . moduleImportMap) ms

    findExpr :: Module -> EVar -> Maybe (Expr, EVar, MVar)
    findExpr m v
      | Set.member v (moduleExports m) = case Map.lookup v (moduleDeclarationMap m) of
          (Just e) -> Just (e, v, moduleName m)
          Nothing -> case Map.elems $ Map.filterWithKey (\v' _ -> v' == v) (moduleImportMap m) of
            mvs -> case [findExpr m' v | m' <- mapMaybe (flip Map.lookup $ ms) mvs] of
              (x:_) -> x
              _ -> Nothing
      | otherwise = Nothing
