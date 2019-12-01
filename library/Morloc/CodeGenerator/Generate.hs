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
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import Morloc.CodeGenerator.Grammars.Common
import qualified Morloc.CodeGenerator.Nexus as Nexus
import qualified Morloc.System as MS
import Data.Scientific (Scientific)
import Control.Monad ((>=>))
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Morloc.CodeGenerator.Grammars.Template.C as GrammarC
import qualified Morloc.CodeGenerator.Grammars.Template.Cpp as GrammarCpp
import qualified Morloc.CodeGenerator.Grammars.Template.R as GrammarR
import qualified Morloc.CodeGenerator.Grammars.Template.Python3 as GrammarPython3

say :: MDoc -> MorlocMonad ()
say d = liftIO . putDoc $ " : " <> d <> "\n"

generate :: [Module] -> MorlocMonad (Script, [Script])
generate ms = do
  MM.startCounter -- initialize state counter to 0, used to index manifolds
  smap <- findSerializers ms
  ast <- connect ms
  say $ "length of ast: " <> pretty (length ast)
  generateScripts smap ast

generateScripts
  :: SerialMap
  -> [SAnno (Type, Meta)]
  -> MorlocMonad (Script, [Script])
generateScripts smap es
  = (,)
  <$>  Nexus.generate [(t, metaId m, metaName m) | (t, m) <- map poolCall es]
  <*> (mapM (codify smap) es |>> foldPools >>= mapM addGrammar >>= mapM (makePool smap))
  where
    addGrammar :: (Lang, a) -> MorlocMonad (Grammar, a)
    addGrammar (lang, x) = do 
      say "adding grammar" 
      grammar <- selectGrammar lang
      return (grammar, x)

    -- reach into the top exported lambda and find the pool application
    poolCall :: SAnno (Type, Meta) -> (Type, Meta)
    poolCall (SAnno (LamS _ (SAnno _ m)) _) = m
    poolCall _ = error "Currently only functional exports are supported"

makePool :: SerialMap -> (Grammar, [(Int, [(Type, Meta, MDoc)])]) -> MorlocMonad Script
makePool h (g, xs) = do
  state <- MM.get
  code <- poolCode g h xs
  return $ Script 
    { scriptBase = "pool"
    , scriptLang = gLang g
    , scriptCode = render code
    , scriptCompilerFlags =
        filter (/= "") . map packageGccFlags $ statePackageMeta state
    , scriptInclude = map MS.takeDirectory (poolIncludes xs)
    }

-- | Find all unique included directory paths
poolIncludes :: [(Int, [(Type, Meta, MDoc)])] -> [Path]
poolIncludes xs
  = nub
  . catMaybes
  $ (map snd xs >>= map (\(_,x,_) -> x))
    |>> (\m -> metaSource m >>= srcPath)

poolCode :: Grammar -> SerialMap -> [(Int, [(Type, Meta, MDoc)])] -> MorlocMonad MDoc
poolCode g h xs = do
  xs' <- getTopPoolCalls h xs
  let mans = concat [[d | (_,_,d) <- ys] | (i, ys) <- xs]
      sigs = concat [[(m,t) | (t,m,_) <- ys] | (i, ys) <- xs]
      dispatchManifold =
        (gSwitch g)
          (\(_,m,_) -> pretty (metaId m))
          (\(t,m,p) -> mainCall g t m p)
          xs'
  gMain g $ PoolMain
    { pmSources = map pretty (poolIncludes xs)
    , pmSignatures = map (makeSignature g) sigs
    , pmPoolManifolds = mans
    , pmDispatchManifold = dispatchManifold
    }

getTopPoolCalls
  :: SerialMap
  -> [(Int, [(Type, Meta, MDoc)])]
  -> MorlocMonad [(Type, Meta, Name)]
getTopPoolCalls h xs = mapM f xs where
  f :: (Int, [(Type, Meta, MDoc)]) -> MorlocMonad (Type, Meta, Name)
  f (i, ys) =
    case listToMaybe [(i, t, m) | (t, m, _) <- ys, metaId m == i] of
      (Just (i,t,m)) -> do
        packer <- selectFunction t Pack h
        return (t,m,packer)
      Nothing -> MM.throwError . OtherError $ "Manifold ID not in pool"

-- call a manifold and deserialize the return value
mainCall :: Grammar -> Type -> Meta -> Name -> MDoc
mainCall g t m packer =
  (gCall g) (pretty packer) $
  [(gCall g) ("m" <> pretty (metaId m)) (take (length (metaArgs m)) (gCmdArgs g))]

makeSignature :: Grammar -> (Meta, Type) -> MDoc
makeSignature g (m, t) = (gSignature g) $ GeneralFunction 
  { gfComments = ""
  , gfReturnType = Just . gShowType g $ t
  , gfName = fromJust . fmap pretty $ metaName m
  , gfArgs = map (prepArg g) (metaArgs m)
  , gfBody = ""
  }

prepArg :: Grammar -> Argument -> (Maybe MDoc, MDoc)
prepArg g r = case (gShowType g . argType $ r, argName r) of  
  (t, n) -> (Just t, pretty n)

selectGrammar :: Lang -> MorlocMonad Grammar
selectGrammar CLang       = return GrammarC.grammar
selectGrammar CppLang     = return GrammarCpp.grammar
selectGrammar RLang       = return GrammarR.grammar
selectGrammar Python3Lang = return GrammarPython3.grammar
selectGrammar MorlocLang = MM.throwError . OtherError $
  "No grammar exists for MorlocLang, this may be a compiler bug"
selectGrammar lang = MM.throwError . OtherError $
  "No grammar found for " <> MT.show' lang


-- | This is a beast of a return type. Here is what it means:
-- [ (Lang,  -- the one pool language
--   [ (Int  -- the id for toplevel manifold, this manifold may or may
--           -- not be exported to the manifold, the important bit is
--           -- that the manifold be a foreign call
--   , [(Type, Meta, MDoc)]
--   )])]
foldPools :: [SAnno (Type, Meta, MDoc)] -> [(Lang, [(Int, [(Type, Meta, MDoc)])])]
foldPools xs
  = groupSort -- [(Lang, [(Int, [(Type, Meta, MDoc)])])]
  . map (\((l,i),xs) -> (l, (i, xs))) -- [(Lang, (Int, [(Type, Meta, MDoc)])]
  . Map.toList -- [((Lang, Int), [(Type, Meta, MDoc)])]
  . Map.unionsWith (++)
  $ map (\x -> foldPool' (getKey x) x) xs
  where
    foldPool'
      :: (Lang, Int)
      -> SAnno (Type, Meta, MDoc)
      -> Map.Map (Lang, Int) [(Type, Meta, MDoc)]
    foldPool' _ (SAnno UniS _) = Map.empty
    foldPool' _ (SAnno (VarS _) _) = Map.empty
    foldPool' k (SAnno (ListS  xs) _) = Map.unionsWith (++) (map (foldPool' k) xs)
    foldPool' k (SAnno (TupleS xs) _) = Map.unionsWith (++) (map (foldPool' k) xs)
    foldPool' k (SAnno (LamS _ x)  _) = foldPool' k x
    foldPool' k (SAnno (AppS x xs) m) =
      Map.unionsWith
        (++)
        (Map.singleton (rekey k x) [m] : map (\x -> foldPool' (rekey k x) x) xs)
    foldPool' _ (SAnno (NumS _) _) = Map.empty
    foldPool' _ (SAnno (LogS _) _) = Map.empty
    foldPool' _ (SAnno (StrS _) _) = Map.empty
    foldPool' k (SAnno (RecS xs) _) = Map.unionsWith (++) (map (foldPool' k . snd) xs)

    rekey :: (Lang, Int) -> SAnno (Type, Meta, MDoc) -> (Lang, Int)
    rekey k@(lang, _) (SAnno _ (t, m, _))
      | lang /= lang' = (lang', metaId m)
      | otherwise = k
      where
        lang' = langOf' t

    getKey ::  SAnno (Type, Meta, MDoc) -> (Lang, Int)
    getKey (SAnno _ (t, m, _)) = (langOf' t, metaId m)

findSerializers :: [Module] -> MorlocMonad SerialMap
findSerializers ms = return $ SerialMap
  { packers = Map.unions (map (findSerialFun Pack) ms)
  , unpackers = Map.unions (map (findSerialFun Unpack) ms)
  } where

  findSerialFun :: Property -> Module -> Map.Map Type (Name, Path)
  findSerialFun p m
    = Map.fromList
    . map (getType p)
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

  getType :: Property -> (Type, a) -> (Type, a)
  getType Pack   (FunT t _, x) = (t, x)
  getType Unpack (FunT _ t, x) = (t, x)
  getType _ t = t

-- | Create one tree for each nexus command.
connect :: [Module] -> MorlocMonad [SAnno (Type, Meta)]
connect ms = do
  say $ "loading" <+> pretty (length ms) <+> "modules"
  let modmap = Map.fromList [(moduleName m, m) | m <- ms] 
      roots = findRoots modmap
  say $ "found" <+> pretty (length roots) <+> "root modules"
  mapM (collect modmap >=> realize) roots 

collect :: Map.Map MVar Module -> (Expr, EVar, MVar) -> MorlocMonad (SAnno [(Type, Meta)])
collect ms (e@(AnnE _ ts), ev, mv) = root where
  root = do
    say $ "collect"
    (SAnno sexpr _) <- collect' Set.empty (e, mv)
    SAnno sexpr <$> makeVarMeta ev mv ts

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
      (SAnno (AppS f es) t) -> return $ SAnno (AppS f (e2':es)) t
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

  getGeneralType :: [Type] -> MorlocMonad (Maybe Type)
  getGeneralType ts = case [t | t <- ts, langOf' t == MorlocLang] of 
      [] -> return Nothing
      [x] -> return $ Just x
      xs -> MM.throwError . OtherError $ "Expected 0 or 1 general types, found " <> MT.show' (length xs)

  -- | Evaluate a variable. If it was imported, lookup of the module it came from.
  evaluateVariable :: Set.Set EVar -> MVar -> EVar -> [Type] -> MorlocMonad (SAnno ([(Type, Meta)]))
  evaluateVariable args mvar evar@(EV name) ts = do
    say $ viaShow args
    say $ viaShow evar
    say $ viaShow mvar
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
        case Map.lookup mvar ms >>= (\m -> findExpr ms m evar) of
          -- if the expression is defined somewhere, unroll it
          (Just (expr', evar', mvar')) -> do
            (SAnno sexpr _) <- collect' args (expr', mvar')
            SAnno sexpr <$> makeVarMeta evar' mvar' ts
          Nothing -> simpleCollect (VarS evar) (Just name) ts mvar

  simpleCollect :: SExpr [(Type, Meta)] -> Maybe Name -> [Type] -> MVar -> MorlocMonad (SAnno [(Type, Meta)])
  simpleCollect x name ts v = do
    i <- MM.getCounter
    generalType <- getGeneralType ts
    let meta = Meta { metaGeneralType = generalType
                    , metaName = name
                    , metaProperties = Set.empty
                    , metaConstraints = Set.empty
                    , metaSource = Nothing
                    , metaModule = v
                    , metaId = i
                    , metaArgs = [] -- cannot be set until after realization
                    }
    return $ SAnno x [(t, meta) | t <- ts] 

  makeVarMeta :: EVar -> MVar -> [Type] -> MorlocMonad [(Type, Meta)]
  makeVarMeta evar@(EV name) mvar ts = do
    i <- MM.getCounter
    generalType <- getGeneralType ts
    let typeset = lookupTypeSet evar mvar ms 
        meta = Meta { metaGeneralType = generalType
                    , metaName = Just name 
                    , metaProperties = Set.empty
                    , metaConstraints = Set.empty
                    , metaSource = Nothing
                    , metaModule = mvar
                    , metaId = i
                    , metaArgs = [] -- cannot be set until after realization
                    }
    return $ zip [t | t <- ts, langOf' t /= MorlocLang] (repeat meta)

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
realize (SAnno _ []) = MM.throwError . OtherError $ "No type found"
realize x = do
  say $ "realize"
  stepAM (head . filter (\(t, _) -> isJust (langOf t))) x

stepAM :: Monad m => (a -> b) -> SAnno a -> m (SAnno b) 
stepAM f (SAnno x a) = SAnno <$> stepBM f x <*> pure (f a)

stepBM :: Monad m => (a -> b) -> SExpr a -> m (SExpr b)
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

codify
  :: SerialMap
  -> SAnno (Type, Meta)
  -> MorlocMonad (SAnno (Type, Meta, MDoc))
codify hashmap (SAnno (LamS vs e2) (t@(FunT _ _), meta)) = do
  args <- zipWithM makeNexusArg vs (typeArgs t)
  codify' hashmap args e2
  where
    -- these are arguments provided by the user from the nexus
    -- they are the original inputs to the entire morloc program
    makeNexusArg :: EVar -> Type -> MorlocMonad Argument
    makeNexusArg (EV n) t = do
      packer <- selectFunction t Pack hashmap
      unpacker <- selectFunction t Unpack hashmap
      return $ Argument
        { argName = n
        , argType = t
        , argPacker = packer
        , argUnpacker = unpacker
        , argIsPacked = True
        }
codify _ _ = MM.throwError . OtherError $
  "Top-level nexus entries must be non-polymorphic functions"

codify'
  :: SerialMap -- h - stores pack and unpack maps
  -> [Argument] -- r - lambda-bound arguments
  -> SAnno (Type, Meta)
  -> MorlocMonad (SAnno (Type, Meta, MDoc))
codify' h r (SAnno (AppS e funargs) (t, m)) = do
  grammar <- selectGrammar (langOf' t)
  let m' = m {metaArgs = r}
  e2 <- codify' h r e 
  args <- mapM (codify' h r) funargs
  mandoc <- makeManifold grammar r m' args e2
  return $ SAnno (AppS e2 args) (t, m', mandoc)
codify' _ _ (SAnno UniS (t,m)) = do
  grammar <- selectGrammar (langOf' t)
  return $ SAnno UniS (t, m, gNull grammar)
codify' _ _ (SAnno (VarS (EV v)) (t,m)) = return $ SAnno (VarS (EV v)) (t, m, pretty v)
codify' h r (SAnno (ListS xs) (t,m)) = do
  elements <- mapM (codify' h r) xs
  grammar <- selectGrammar (langOf' t)
  let mdoc = (gList grammar) (map getDoc elements)
  return $ SAnno (ListS elements) (t,m,mdoc)
codify' h r (SAnno (TupleS xs) (t,m)) = do
  elements <- mapM (codify' h r) xs
  grammar <- selectGrammar (langOf' t)
  let mdoc = (gTuple grammar) (map getDoc elements)
  return $ SAnno (TupleS elements) (t,m,mdoc)
codify' h r (SAnno (LamS vs e) (t,m)) = do
  newargs <- updateArguments h r (zipWith (\e t -> (e,t,False)) vs (typeArgs t))
  body <- codify' h newargs e
  let mdoc = "lambda" ---------------- FIXME incomplete ---------------
      m' = m { metaArgs = newargs }
  return $ SAnno (LamS vs body) (t, m', mdoc)
codify' h r (SAnno (RecS entries) (t, m)) = do
  newvals <- mapM (codify' h r) (map snd entries)
  grammar <- selectGrammar (langOf' t)
  let newEntries = zip (map fst entries) newvals
      mdoc = gRecord grammar
           $ map (\(EV k, v) -> (pretty k, getDoc v)) newEntries
  return $ SAnno (RecS newEntries) (t, m, mdoc)
codify' _ _ (SAnno (NumS x) (t,m)) = return $ SAnno (NumS x) (t, m, viaShow x) -- Scientific
codify' _ _ (SAnno (LogS x) (t,m)) = do
  grammar <- selectGrammar (langOf' t)
  return $ SAnno (LogS x) (t, m, (gBool grammar) x)
codify' _ _ (SAnno (StrS x) (t,m)) = return $ SAnno (StrS x) (t, m, pretty x) -- Text

makeManifold
  :: Grammar
  -> [Argument]
  -> Meta
  -> [SAnno (Type, Meta, MDoc)]
  -> SAnno (Type, Meta, MDoc)
  -> MorlocMonad MDoc
makeManifold g args meta inputs (SAnno _ (otype, _, _)) = do
  name <- case metaName meta of
    (Just n) -> return n
    Nothing -> MM.throwError . OtherError $ "No name found for manifold"
  inputs' <- mapM (prepInput g args) inputs
  return . gFunction g $ GeneralFunction
    { gfComments = "comment" <> line
    , gfReturnType = Just ((gShowType g) otype)
    , gfName = "m" <> pretty (metaId meta)
    , gfArgs = [(Just ((gShowType g) (argType a)), pretty (argName a)) | a <- args]
    , gfBody =
        vsep (catMaybes (map snd inputs')) <> line <>
        (gReturn g $ (gCall g) (pretty name) (map fst inputs'))
    }

prepInput :: Grammar -> [Argument] -> SAnno (Type, Meta, MDoc) -> MorlocMonad (MDoc, Maybe MDoc) 
prepInput g rs (SAnno _ (t, m, d)) = do
  let name = metaName m
      arg = listToMaybe [r | r <- rs, Just (argName r) == metaName m]
      nth = findIndex (\r -> Just (argName r) == metaName m) rs
  case (name, arg, nth, fmap argIsPacked arg) of
    (Just n, Just r, Just i, Just True) ->
       return ( "x" <> pretty i, Just . gAssign g $ GeneralAssignment
          { gaType = Just . gShowType g . argType $ r
          , gaName = "x" <> pretty i
          , gaValue = (gCall g) (pretty (argUnpacker r)) [pretty name]
          , gaArg = Nothing
          }
       )
    (Just n, _, _, Just False) -> return (pretty name, Nothing)
    _ -> MM.throwError . OtherError $ "Need to add handling for this"

getDoc :: SAnno (Type, Meta, MDoc) -> MDoc 
getDoc (SAnno _ (_, _, x)) = x

updateArguments :: SerialMap -> [Argument] -> [(EVar, Type, Bool)] -> MorlocMonad [Argument]
updateArguments _ args [] = return args
updateArguments hashmap args xs = do
  newargs <- mapM makeArg xs
  let oldargs = [x | x <- args, not (elem (argName x) (map argName newargs))]
  return $ newargs ++ oldargs
  where
    makeArg :: (EVar, Type, Bool) -> MorlocMonad Argument
    makeArg (EV n, t, packed) = do
      packer <- selectFunction t Pack hashmap
      unpacker <- selectFunction t Unpack hashmap
      return $ Argument
        { argName = n
        , argType = t
        , argPacker = packer
        , argUnpacker = unpacker
        , argIsPacked = packed
        }

typeArgs :: Type -> [Type]
typeArgs (FunT t1 t2) = t1 : typeArgs t2
typeArgs t = [t]

exprArgs :: SExpr a -> [Name]
exprArgs (LamS vs _) = [name | (EV name) <- vs]
exprArgs _ = []

selectFunction :: Type -> Property -> SerialMap -> MorlocMonad Name
selectFunction t p h = case mostSpecificSubtypes t (Map.keys hmap) of
  [] -> MM.throwError . OtherError $ "No packer found"
  (x:_) -> case Map.lookup x hmap of
    (Just (name, _)) -> return name
    Nothing -> MM.throwError . OtherError $ "I swear it used to be there"
  where
    hmap = if p == Pack then packers h else unpackers h

findRoots :: Map.Map MVar Module -> [(Expr, EVar, MVar)]
findRoots ms
  = catMaybes
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

findExpr :: Map.Map MVar Module -> Module -> EVar -> Maybe (Expr, EVar, MVar)
findExpr ms m v
  | Set.member v (moduleExports m) = case Map.lookup v (moduleDeclarationMap m) of
      (Just e) -> Just (e, v, moduleName m)
      Nothing -> case Map.elems $ Map.filterWithKey (\v' _ -> v' == v) (moduleImportMap m) of
        mvs -> case [findExpr ms m' v | m' <- mapMaybe (flip Map.lookup $ ms) mvs] of
          (x:_) -> x
          _ -> Nothing
  | otherwise = Nothing
