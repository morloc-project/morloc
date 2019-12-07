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
import Morloc.Data.Doc
import Morloc.TypeChecker.PartialOrder
import Morloc.Pretty (prettyType)
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import Morloc.CodeGenerator.Grammars.Common
import qualified Morloc.CodeGenerator.Nexus as Nexus
import qualified Morloc.System as MS
import Data.Scientific (Scientific)
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Morloc.CodeGenerator.Grammars.Template.C as GrammarC
import qualified Morloc.CodeGenerator.Grammars.Template.Cpp as GrammarCpp
import qualified Morloc.CodeGenerator.Grammars.Template.R as GrammarR
import qualified Morloc.CodeGenerator.Grammars.Template.Python3 as GrammarPython3

-- -- | Information sufficient to build a manifold function in a pool
-- type Manifold =
--   ( Type -- The chosen concrete type (language-specific) type of the manifold
--   , Meta -- General (NOT language-specific) metadata for the manifold
--   , MDoc -- The code for the manifold, generated in `codify`
--   )
--
-- -- | The interface to a pool is a set of functions (manifolds) that can be
-- -- called by index from the nexus. These manifolds in turn may call other
-- -- manifolds, creating a call tree. These are strictly tree data structures, so
-- -- no manifolds are shared between trees rooted on different manifolds. CallSet
-- -- specifies a single "tree". The manifolds are stored as a list here, since
-- -- they have already been encoded (in codify) and thus do not need to access
-- -- their neighbors. The list is ordered, though, such that manifolds appear in
-- -- the reverse order they are called. This ensures manifolds are defined before
-- -- being called, which is matters in many languages.
-- type CallSet =
--   ( Int -- The integer index of the first manifold in the set
--   , [Manifold] -- All manifolds that will be called
--   )

data Role
  = Foreign
  | Domestic
  deriving(Show, Ord, Eq)

-- | Translate typechecker-created modules into compilable code
generate :: [Module] -> MorlocMonad (Script, [Script])
generate ms = do
  -- initialize state counter to 0, used to index manifolds
  MM.startCounter

  -- modmap :: Map.Map MVar Module
  let modmap = Map.fromList [(moduleName m, m) | m <- ms] 

  -- recursively find all serializers imported from any module
  smap <- findSerializers ms

  -- translate modules into bitrees
  ast <- roots modmap >>= mapM (collect modmap) >>= mapM realize

  -- build nexus
  -- -----------
  -- Each (Type, Int, Name) tuple passed to Nexus.generate maps to nexus subcommand.
  -- Each nexus subcommand calls one function from one of the language-specific pool.
  -- The call passes the pool an index for the function (manifold) that will be called.
  nexus <- Nexus.generate [(t, poolId m x, metaName m) | (SAnno x (t, m)) <- ast]

  -- for each language, collect all functions into one "pool"
  pools <- mapM segment ast |>> concat >>= pool >>= mapM (encode smap modmap)

  -- return the nexus script and each pool script
  return (nexus, pools)
  where
    -- map from nexus id to pool id
    -- these differ when a declared variable is exported
    poolId :: Meta -> SExpr (Type, Meta) -> Int
    poolId _ (LamS _ (SAnno _ (_, meta))) = metaId meta
    poolId meta _ = metaId meta

-- | Find the expressions that are exposed to the user
roots :: Map.Map MVar Module -> MorlocMonad [(Expr, EVar, MVar)]
roots ms
  = return
  . catMaybes
  . Set.toList
  . mapSum
  . Map.map (\m -> Set.map (findExpr ms m) (moduleExports m))
  . Map.filter isRoot
  $ ms where
    -- is this module a "root" module?
    -- a root module is a module that is not imported from any other module
    isRoot :: Module -> Bool
    isRoot m = not $ Set.member (moduleName m) allImports

    -- set of all modules that are imported
    allImports = mapSumWith (valset . moduleImportMap) ms

segment
  :: SAnno (Type, Meta)
  -> MorlocMonad [SAnno (Type, Meta, Role)]
segment = undefined

-- Sort manifolds into pools. Within pools, group manifolds into call sets.
pool
  :: [SAnno (Type, Meta, Role)]
  -> MorlocMonad [(Lang, [SAnno (Type, Meta, Role)])]
pool = undefined

encode
  :: SerialMap
  -> Map.Map MVar Module
  -> (Lang, [SAnno (Type, Meta, Role)])
  -> MorlocMonad Script
encode = undefined

-- find all serialization functions in a list of modules
findSerializers :: [Module] -> MorlocMonad SerialMap
findSerializers ms = return $ SerialMap
  { packers = Map.unions (map (findSerialFun Pack) ms)
  , unpackers = Map.unions (map (findSerialFun Unpack) ms)
  } where

  findSerialFun :: Property -> Module -> Map.Map Type (Name, Path)
  findSerialFun p m
    = Map.fromList
    . map (\(t, x) -> (getType p t, x))
    . mapSum
    . Map.mapWithKey (\v t -> map (g m) (f p v t))
    $ moduleTypeMap m

  f :: Property -> EVar -> TypeSet -> [(Type, EVar)]
  f p v (TypeSet (Just gentype) ts) =
    if Set.member p (eprop gentype)
      then [(etype t, v) | t <- ts]
      else [(etype t, v) | t <- ts, Set.member p (eprop t)]
  f p v (TypeSet Nothing ts) = [(etype t, v) | t <- ts, Set.member p (eprop t)]

  g :: Module -> (Type, EVar) -> (Type, (Name, Path))
  g m (t, v) = case Map.lookup (v, langOf' t) (moduleSourceMap m) of
    (Just (Source (EV name) _ (Just path) _)) -> (t, (name, path))
    _ -> error "something evil this way comes"

  -- Get the type that is being serialized or deserialized
  getType :: Property -> Type -> Type
  getType Pack   (FunT t _) = t
  getType Unpack (FunT _ t) = t
  getType p (Forall v t) = Forall v (getType p t)
  getType _ t = t


collect :: Map.Map MVar Module -> (Expr, EVar, MVar) -> MorlocMonad (SAnno [(Type, Meta)])
collect ms (e@(AnnE _ ts), ev, mv) = do
  (SAnno sexpr _) <- collect' Set.empty (e, mv)
  SAnno sexpr <$> makeVarMeta ev mv ts
  where
    collect' :: Set.Set EVar -> (Expr, MVar) -> MorlocMonad (SAnno [(Type, Meta)])
    collect' _ (AnnE UniE ts, m) = simpleCollect UniS Nothing ts m
    collect' args (AnnE (VarE v) ts, m) = evaluateVariable args m v ts
    collect' args (AnnE (ListE es) ts, m) = do
      es' <- mapM (collect' args) [(e,m) | e <- es]
      simpleCollect (ListS es') Nothing ts m
    collect' args (AnnE (TupleE es) ts, m) = do
      es' <- mapM (collect' args) [(e,m) | e <- es]
      simpleCollect (TupleS es') Nothing ts m
    collect' args (AnnE (RecE es) ts, m) = do
      es' <- mapM (\x -> (collect' args) (x, m)) (map snd es)
      simpleCollect (RecS (zip (map fst es) es')) Nothing ts m
    collect' args (AnnE e1@(LamE _ _) ts, m) = do
      let (vs, e) = unrollLambda e1
      e' <- collect' (Set.union args (Set.fromList vs)) (e, m)
      simpleCollect (LamS vs e') Nothing ts m
    collect' args (AnnE (AppE e1 e2) ts, m) = do
      e1' <- collect' args (e1, m)
      e2' <- collect' args (e2, m)
      case e1' of
        (SAnno (AppS f es) t) -> return $ SAnno (AppS f (es ++ [e2'])) t
        f@(SAnno _ t) -> return $ SAnno (AppS f [e2']) t
    collect' _ (AnnE (LogE e) ts, m) = simpleCollect (LogS e) Nothing ts m
    collect' _ (AnnE (NumE e) ts, m) = simpleCollect (NumS e) Nothing ts m
    collect' _ (AnnE (StrE e) ts, m) = simpleCollect (StrS e) Nothing ts m
    collect _ _ _ = MM.throwError . OtherError $ "Unexpected type in collect"

    lambdaArgs :: Expr -> [EVar]
    lambdaArgs (LamE v e2) = v : lambdaArgs e2
    lambdaArgs _ = []

    unrollLambda :: Expr -> ([EVar], Expr)
    unrollLambda (LamE v e2) = case unrollLambda e2 of
      (vs, e) -> (v:vs, e)
    unrollLambda e = ([], e)

    -- | Evaluate a variable. If it was imported, lookup of the module it came from.
    evaluateVariable
      :: Set.Set EVar
      -> MVar
      -> EVar
      -> [Type]
      -> MorlocMonad (SAnno ([(Type, Meta)]))
    evaluateVariable args mvar evar@(EV name) ts = do
      let m = fromJust $ Map.lookup mvar ms
      if Set.member evar args
        then
          -- variable is bound under a lambda, so we leave it as a variable
          simpleCollect (VarS evar) (Just name) ts mvar
        else
          -- Term is defined outside, so we replace it with the exterior
          -- definition This leads to massive code duplication, but the code is
          -- not identical. Each instance of the term may be in a different
          -- language or have different settings (e.g. different memoization
          -- handling).
          case findExpr ms m evar of
            -- if the expression is defined somewhere, unroll it
            (Just (expr', evar', mvar')) ->
              if elem evar' (map fst (Map.keys (moduleSourceMap m)))
                then simpleCollect (VarS evar') (Just name) ts mvar'
                else do
                  (SAnno sexpr _) <- collect' args (expr', mvar')
                  SAnno sexpr <$> makeVarMeta evar' mvar' ts
            Nothing -> simpleCollect (VarS evar) (Just name) ts mvar

    simpleCollect
      :: SExpr [(Type, Meta)]
      -> Maybe Name
      -> [Type]
      -> MVar
      -> MorlocMonad (SAnno [(Type, Meta)])
    simpleCollect x name ts v = do
      i <- MM.getCounter
      generalType <- getGeneralType ts
      let meta = Meta { metaGeneralType = generalType
                      , metaName = name
                      , metaProperties = Set.empty
                      , metaConstraints = Set.empty
                      , metaSources = Set.empty
                      , metaModule = v
                      , metaId = i
                      , metaArgs = [] -- cannot be set until after realization
                      }
      return $ SAnno x [(t, meta) | t <- ts] 
collect _ _ = MM.throwError . OtherError $ "This is probably a bug" 

makeVarMeta :: EVar -> MVar -> [Type] -> MorlocMonad [(Type, Meta)]
makeVarMeta evar@(EV name) mvar ts = do
  i <- MM.getCounter
  generalType <- getGeneralType ts
  let meta = Meta { metaGeneralType = generalType
                  , metaName = Just name 
                  , metaProperties = Set.empty
                  , metaConstraints = Set.empty
                  , metaSources = Set.empty
                  , metaModule = mvar
                  , metaId = i
                  , metaArgs = [] -- cannot be set until after realization
                  }
  return $ zip [t | t <- ts, langOf' t /= MorlocLang] (repeat meta)

-- | Select a single concrete language for each sub-expression. Store the
-- concrete type and the general type (if available).
realize :: SAnno [(Type, Meta)] -> MorlocMonad (SAnno (Type, Meta))
realize (SAnno _ []) = MM.throwError . OtherError $ "No type found"
realize x = stepAM selectRealization x where
  -- | Keep the first concrete realization in the list
  -- FIXME: this is an EXTREMELY bad approach
  selectRealization :: [(Type, Meta)] -> MorlocMonad (Type, Meta)
  selectRealization xs =
    case filter (\(t, _) -> isJust (langOf t)) xs of
      [] -> MM.throwError . OtherError $ "No concrete realization found"
      (x:_) -> return x

  stepAM :: Monad m => (a -> m b) -> SAnno a -> m (SAnno b) 
  stepAM f (SAnno x a) = SAnno <$> stepBM f x <*> f a

  stepBM :: Monad m => (a -> m b) -> SExpr a -> m (SExpr b)
  stepBM _ UniS = return $ UniS
  stepBM f (VarS x) = return $ VarS x
  stepBM f (ListS xs) = ListS <$> mapM (stepAM f) xs
  stepBM f (TupleS xs) = TupleS <$> mapM (stepAM f) xs
  stepBM f (LamS vs x) = LamS vs <$> stepAM f x
  stepBM f (AppS x xs) = AppS <$> stepAM f x <*> mapM (stepAM f) xs
  stepBM _ (NumS x) = return $ NumS x
  stepBM _ (LogS x) = return $ LogS x
  stepBM _ (StrS x) = return $ StrS x
  stepBM f (RecS entries) = RecS <$> mapM (\(v, x) -> (,) v <$> stepAM f x) entries


-------- Utility and lookup functions ----------------------------------------

getGeneralType :: [Type] -> MorlocMonad (Maybe Type)
getGeneralType ts = case [t | t <- ts, langOf' t == MorlocLang] of 
    [] -> return Nothing
    [x] -> return $ Just x
    xs -> MM.throwError . OtherError $
      "Expected 0 or 1 general types, found " <> MT.show' (length xs)

-- | Find exported expressions. These may be declared or sourced in the current
-- module or they may be imported from a different module. If they are
-- imported, ascend through modules to the original declaration, returning the
-- module where they are defined.
findExpr :: Map.Map MVar Module -> Module -> EVar -> Maybe (Expr, EVar, MVar)
findExpr ms m v
  | Set.member v (moduleExports m)
      =   evarDeclared
      <|> evarSourced
      <|> evarImported
  | otherwise = Nothing
  where
    evarDeclared :: Maybe (Expr, EVar, MVar) 
    evarDeclared =   Map.lookup v (moduleDeclarationMap m)
                 |>> (\e -> (e, v, moduleName m))

    evarSourced :: Maybe (Expr, EVar, MVar)
    evarSourced = listToMaybe
                     . map (\((v', _), _) -> (typeEVar v', v', moduleName m))
                     . Map.toList
                     . Map.filterWithKey (\(v',_) _ -> v' == v)
                     $ moduleSourceMap m

    typeEVar :: EVar -> Expr  
    typeEVar v' = case Map.lookup v' (moduleTypeMap m) of
      (Just (TypeSet t ts)) -> AnnE (VarE v') (map etype (maybe ts (\t' -> t':ts) t))

    evarImported :: Maybe (Expr, EVar, MVar)
    evarImported =
      case
        catMaybes [findExpr ms m' v | m' <- mapMaybe (flip Map.lookup $ ms) (listMVars m)]
      of
        [] -> Nothing   
        (x:_) -> Just x

    listMVars :: Module -> [MVar]
    listMVars m = Map.elems $ Map.filterWithKey (\v' _ -> v' == v) (moduleImportMap m) 
