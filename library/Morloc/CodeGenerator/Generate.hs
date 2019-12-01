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
import Control.Monad ((>=>))
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Morloc.CodeGenerator.Grammars.Template.C as GrammarC
import qualified Morloc.CodeGenerator.Grammars.Template.Cpp as GrammarCpp
import qualified Morloc.CodeGenerator.Grammars.Template.R as GrammarR
import qualified Morloc.CodeGenerator.Grammars.Template.Python3 as GrammarPython3

say :: MDoc -> MorlocMonad ()
say d = liftIO . putDoc $ " : " <> d <> "\n"

-- | Translate modules from the typechecker into executable code
generate :: [Module] -> MorlocMonad (Script, [Script])
generate ms = do
  -- initialize state counter to 0, used to index manifolds
  MM.startCounter
  -- recursively find all serializers imported from any module
  smap <- findSerializers ms
  -- translate modules into bitrees
  -- connect :: [Module] -> [SAnno (Type, Meta)]
  ast <- connect ms 
  -- modmap :: Map.Map MVar Module
  let modmap = Map.fromList [(moduleName m, m) | m <- ms] 
  generateScripts smap modmap ast

generateScripts
  :: SerialMap
  -> Map.Map MVar Module
  -> [SAnno (Type, Meta)]
  -> MorlocMonad (Script, [Script])
generateScripts smap ms es = do
  nexus <- Nexus.generate [(t, poolId m x, metaName m) | (SAnno x (t, m)) <- es]
  pools <-
    -- translate expressions to code
        mapM (codify smap ms) es  
    -- pool code by language
    |>> foldPools 
    -- collate manifold code into single pool code
    >>= mapM (makePool smap)
  return (nexus, pools)
  where
    -- map from nexus id to pool id
    poolId :: Meta -> SExpr (Type, Meta) -> Int
    poolId _ (LamS _ (SAnno _ (_, meta))) = metaId meta
    poolId meta _ = metaId meta

makePool :: SerialMap -> (Lang, [(Int, [(Type, Meta, MDoc)])]) -> MorlocMonad Script
makePool h (lang, xs) = do
  g <- selectGrammar lang
  state <- MM.get
  code <- poolCode g h xs
  return $ Script 
    { scriptBase = "pool"
    , scriptLang = lang
    , scriptCode = render code
    , scriptCompilerFlags =
        filter (/= "") . map packageGccFlags $ statePackageMeta state
    , scriptInclude = map MS.takeDirectory (poolIncludes h xs)
    }

poolCode :: Grammar -> SerialMap -> [(Int, [(Type, Meta, MDoc)])] -> MorlocMonad MDoc
poolCode g h xs = do
  xs' <- getTopPoolCalls h xs
  let mans = concat [[d | (_,_,d) <- ys] | (i, ys) <- xs]
      sigs = concat [[(m,t) | (t,m,_) <- ys] | (i, ys) <- xs]
  importPaths <- mapM (gPrepImport g) (poolIncludes h xs)
  gMain g $ PoolMain
    { pmSources = map ((gImport g) "") importPaths
    , pmSignatures = map (makeSignature g) sigs
    , pmPoolManifolds = mans
    , pmDispatchManifold = makeDispatcher g xs'
    }

-- | Find all unique included directory paths (include serialization functions)
poolIncludes :: SerialMap -> [(Int, [(Type, Meta, MDoc)])] -> [Path]
poolIncludes h xs
  = let mets = map snd xs >>= map (\(_,x,_) -> x) in
      (nub . concat)
        [ catMaybes . Set.toList . Set.map srcPath . Set.unions . map metaSources $ mets
        , map argPackerPath   . concat . map metaArgs $ mets
        , map argUnpackerPath . concat . map metaArgs $ mets
        ]

getTopPoolCalls
  :: SerialMap
  -> [(Int, [(Type, Meta, MDoc)])]
  -> MorlocMonad [(Type, Meta, Name)]
getTopPoolCalls h xs = mapM f xs where
  f :: (Int, [(Type, Meta, MDoc)]) -> MorlocMonad (Type, Meta, Name)
  f (i, ys) =
    case listToMaybe [(i, t, m) | (t, m, _) <- ys, metaId m == i] of
      (Just (i,t,m)) -> do
        (packer, path) <- selectFunction t Pack h
        return (t,m,packer)
      Nothing -> MM.throwError . OtherError $ "Manifold ID not in pool"

-- Make a function for generating the code to dispatch from the pool main
-- function to manifold functions. The two arguments of the returned function
-- (MDoc->MDoc->MDoc) are 1) the manifold id and 2) the variable name where the
-- results are stored.
makeDispatcher
  :: Grammar
  -> [( Type
      , Meta
      , Name -- the name of the function that packs the return data
      )]
  -> (  MDoc -- manifold integer id
     -> MDoc -- variable name that stores the result
     -> MDoc
     )
makeDispatcher g xs =
  (gSwitch g) (\(_,m,_) -> pretty (metaId m))
              (\(t,m,p) -> mainCall g t m p)
              xs
  where
    mainCall :: Grammar -> Type -> Meta -> Name -> MDoc
    mainCall g t m packer = (gCall g)
      (pretty packer)
      [(gCall g)
          (makeManifoldName m)
          (take (length (metaArgs m))
          (gCmdArgs g))]

makeSignature :: Grammar -> (Meta, Type) -> MDoc
makeSignature g (m, t) = (gSignature g) $ GeneralFunction 
  { gfComments = ""
  , gfReturnType = Just . gShowType g . last . typeArgs $ t
  , gfName = makeManifoldName m
  , gfArgs = map (prepArg g) (metaArgs m)
  , gfBody = ""
  }

prepArg :: Grammar -> Argument -> (Maybe MDoc, MDoc)
prepArg g r = (Just . gShowType g $ t, pretty n) where
  t = if argIsPacked r
        then gSerialType g
        else argType r
  n = argName r

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
    foldPool' k (SAnno (AppS x xs) (t,m,d)) =
      let srcs = Set.unions (map collectSources (x:xs)) in
        Map.unionsWith
          (++)
          ( Map.singleton (rekey k x) [(t, m {metaSources = srcs}, d)]
          : map (\x -> foldPool' (rekey k x) x) xs)
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

collectSources :: SAnno (Type, Meta, MDoc) -> Set.Set Source
collectSources (SAnno (VarS _) (_, m, _)) = metaSources m
collectSources (SAnno (ListS xs) _) = Set.unions (map collectSources xs)
collectSources (SAnno (TupleS xs) _) = Set.unions (map collectSources xs)
collectSources (SAnno (RecS xs) _) = Set.unions (map collectSources (map snd xs))
collectSources (SAnno (AppS _ _) _) = Set.empty
collectSources (SAnno (LamS _ x) _) = collectSources x
collectSources _ = Set.empty

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
                    , metaSources = Set.empty
                    , metaModule = v
                    , metaId = i
                    , metaArgs = [] -- cannot be set until after realization
                    }
    return $ SAnno x [(t, meta) | t <- ts] 

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

-- | Find the first source for a term sourced from a given language relative to a given module 
lookupSource :: EVar -> MVar -> Lang -> Map.Map MVar Module -> Set.Set Source
lookupSource evar mvar lang ms =
  case Map.lookup mvar ms |>> moduleSourceMap >>= Map.lookup (evar, lang) of
    (Just src) -> Set.singleton src
    Nothing -> case Map.lookup mvar ms of
      (Just mod) -> Set.unions
                 . map (\mvar' -> lookupSource evar mvar' lang ms)
                 . Map.elems
                 . moduleImportMap
                 $ mod
      Nothing -> Set.empty

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
  -> Map.Map MVar Module 
  -> SAnno (Type, Meta)
  -> MorlocMonad (SAnno (Type, Meta, MDoc))
codify hashmap hs (SAnno (LamS vs e2) (t@(FunT _ _), meta)) = do
  args <- zipWithM makeNexusArg vs (typeArgs t)
  codify' hashmap hs args e2
  where
    -- these are arguments provided by the user from the nexus
    -- they are the original inputs to the entire morloc program
    makeNexusArg :: EVar -> Type -> MorlocMonad Argument
    makeNexusArg (EV n) t = do
      (packer, packerpath) <- selectFunction t Pack hashmap
      (unpacker, unpackerpath) <- selectFunction t Unpack hashmap
      return $ Argument
        { argName = n
        , argType = t
        , argPacker = packer
        , argPackerPath = packerpath
        , argUnpacker = unpacker
        , argUnpackerPath = unpackerpath
        , argIsPacked = True
        }
codify _ _ _ = MM.throwError . OtherError $
  "Top-level nexus entries must be non-polymorphic functions"

codify'
  :: SerialMap -- h - stores pack and unpack maps
  -> Map.Map MVar Module
  -> [Argument] -- r - lambda-bound arguments
  -> SAnno (Type, Meta)
  -> MorlocMonad (SAnno (Type, Meta, MDoc))
codify' h ms r (SAnno (AppS e funargs) (t, m)) = do
  grammar <- selectGrammar (langOf' t)
  e2 <- codify' h ms r e 
  let m' = m {metaArgs = r}
  args <- mapM (codify' h ms r) funargs
  mandoc <- makeManifold grammar r m' args e2
  return $ SAnno (AppS e2 args) (t, m', mandoc)
codify' _ _ _ (SAnno UniS (t,m)) = do
  grammar <- selectGrammar (langOf' t)
  return $ SAnno UniS (t, m, gNull grammar)
codify' _ hs _ (SAnno (VarS evar@(EV v)) (t,m)) = do
  let src = lookupSource evar (metaModule m) (langOf' t) hs 
      m' = m {metaSources = src}
  return $ SAnno (VarS evar) (t, m', pretty v)
codify' h ms r (SAnno (ListS xs) (t,m)) = do
  elements <- mapM (codify' h ms r) xs
  grammar <- selectGrammar (langOf' t)
  let mdoc = (gList grammar) (map getDoc elements)
  return $ SAnno (ListS elements) (t,m,mdoc)
codify' h ms r (SAnno (TupleS xs) (t,m)) = do
  elements <- mapM (codify' h ms r) xs
  grammar <- selectGrammar (langOf' t)
  let mdoc = (gTuple grammar) (map getDoc elements)
  return $ SAnno (TupleS elements) (t,m,mdoc)
codify' h ms r (SAnno (LamS vs e) (t,m)) = do
  newargs <- updateArguments h r (zipWith (\e t -> (e,t,False)) vs (typeArgs t))
  body <- codify' h ms newargs e
  let mdoc = "lambda" ---------------- FIXME incomplete ---------------
      m' = m { metaArgs = newargs }
  return $ SAnno (LamS vs body) (t, m', mdoc)
codify' h ms r (SAnno (RecS entries) (t, m)) = do
  newvals <- mapM (codify' h ms r) (map snd entries)
  grammar <- selectGrammar (langOf' t)
  let newEntries = zip (map fst entries) newvals
      mdoc = gRecord grammar
           $ map (\(EV k, v) -> (pretty k, getDoc v)) newEntries
  return $ SAnno (RecS newEntries) (t, m, mdoc)
codify' _ _ _ (SAnno (NumS x) (t,m)) = return $ SAnno (NumS x) (t, m, viaShow x) -- Scientific
codify' _ _ _ (SAnno (LogS x) (t,m)) = do
  grammar <- selectGrammar (langOf' t)
  return $ SAnno (LogS x) (t, m, (gBool grammar) x)
codify' _ _ _ (SAnno (StrS x) (t,m)) = return $ SAnno (StrS x) (t, m, pretty x) -- Text

makeManifold
  :: Grammar
  -> [Argument]
  -> Meta
  -> [SAnno (Type, Meta, MDoc)]
  -> SAnno (Type, Meta, MDoc)
  -> MorlocMonad MDoc
makeManifold g args meta inputs (SAnno _ (t, _, _)) = do
  let otype = last (typeArgs t)
  name <- case metaName meta of
    (Just n) -> return n
    Nothing -> MM.throwError . OtherError $ "No name found for manifold"
  inputs' <- zipWithM (prepInput g args) [0..] inputs
  return . gFunction g $ GeneralFunction
    { gfComments = prettyType t <> line
    , gfReturnType = Just ((gShowType g) otype)
    , gfName = makeManifoldName meta
    , gfArgs = map (prepArg g) args
    , gfBody =
        vsep (catMaybes (map snd inputs')) <> line <>
        (gReturn g $ (gCall g) (pretty name) (map fst inputs'))
    }

makeManifoldName :: Meta -> MDoc
makeManifoldName m = pretty $ "m" <> MT.show' (metaId m)

makeArgumentName :: Int -> MDoc
makeArgumentName i = "x" <> pretty i

-- Handle preparation of arguments passed to a manifold. Return the name of the
-- variable that will be used and an block of code that defines the variable
-- (if necessary).
prepInput
  :: Grammar
  -> [Argument] -- all arguments of the manifold
  -> Int
  -> SAnno (Type, Meta, MDoc) -- an input to the wrapped function
  -> MorlocMonad (MDoc, Maybe MDoc)
prepInput g rs i (SAnno (AppS x xs) (t, m, d)) = do
  let varname = makeArgumentName i
      mid = makeManifoldName m
      ass = (gAssign g) $ GeneralAssignment
        { gaType = Just . gShowType g . last . typeArgs $ t
        , gaName = varname
        , gaValue = (gCall g) mid (map (pretty . argName) rs)
        , gaArg = Nothing
        }
  return (varname, Just ass)
prepInput g rs i (SAnno _ (t, m, d)) = do
  let name = metaName m
      varname = makeArgumentName i
      arg = listToMaybe [r | r <- rs, Just (argName r) == metaName m]
  case (name, arg, fmap argIsPacked arg) of
    (Just n, Just r, Just True) ->
       return (varname, Just . gAssign g $ GeneralAssignment
          { gaType = Just . gShowType g . argType $ r
          , gaName = varname
          , gaValue = (gCall g) (pretty (argUnpacker r)) [pretty name]
          , gaArg = Nothing
          }
       )
    -- the argument is used in the wrapped function, but is not serialized
    (Just n, _, Just False) -> return (pretty name, Nothing)
    -- the argument is not used in the wrapped function
    (Just n, _, Nothing) -> return (pretty name, Nothing)
    x -> MM.throwError . OtherError $ MT.show' x

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
      (packer, packerpath) <- selectFunction t Pack hashmap
      (unpacker, unpackerpath) <- selectFunction t Unpack hashmap
      return $ Argument
        { argName = n
        , argType = t
        , argPacker = packer
        , argPackerPath = packerpath
        , argUnpacker = unpacker
        , argUnpackerPath = unpackerpath
        , argIsPacked = packed
        }

typeArgs :: Type -> [Type]
typeArgs (FunT t1 t2) = t1 : typeArgs t2
typeArgs t = [t]

exprArgs :: SExpr a -> [Name]
exprArgs (LamS vs _) = [name | (EV name) <- vs]
exprArgs _ = []

selectFunction :: Type -> Property -> SerialMap -> MorlocMonad (Name, Path)
selectFunction t p h = case mostSpecificSubtypes t (Map.keys hmap) of
  [] -> MM.throwError . OtherError $ "No packer found"
  (x:_) -> case Map.lookup x hmap of
    (Just x) -> return x
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
