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
import qualified Morloc.Config as MC
import qualified Morloc.Data.Text as MT
import qualified Morloc.Language as Lang
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
  ast <- roots modmap >>= mapM (collect modmap) >>= mapM (realize smap)

  -- build nexus
  -- -----------
  -- Each (Type, Int, Name) tuple passed to Nexus.generate maps to nexus subcommand.
  -- Each nexus subcommand calls one function from one of the language-specific pool.
  -- The call passes the pool an index for the function (manifold) that will be called.
  nexus <- Nexus.generate [(t, poolId m x, metaName m) | (SAnno x (t, m)) <- ast]

  -- for each language, collect all functions into one "pool"
  pools <- mapM segment ast |>> concat >>= pool >>= mapM encode

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

-- | Select a single concrete language for each sub-expression.
-- Store the concrete type and the general type (if available).
-- Select serialization functions.
realize :: SerialMap -> SAnno [(Type, Meta)] -> MorlocMonad (SAnno (Type, Meta))
realize h x = realize' Nothing [] x where

  realize'
    :: Maybe Lang
    -> [Argument]
    -> SAnno [(Type, Meta)]
    -> MorlocMonad (SAnno (Type, Meta))

  -- new arguments may be bound under lambdas
  realize' lang rs (SAnno (LamS vs x) ms) = do
    (t, m, lang') <- selectRealization lang ms
    let isPacked = isNothing lang
    args <- zipWithM (makeArg isPacked) vs (init $ typeArgs t)
         >>= updateArguments m rs
    x' <- realize' lang' args x
    (packer, packerpath) <- selectFunction (last $ typeArgs t) Pack h
    let m' = m { metaArgs = args
               , metaPacker = packer
               , metaPackerPath = packerpath
               }
    return $ SAnno (LamS vs x') (t, m')
    where
      makeArg :: Bool -> EVar -> Type -> MorlocMonad Argument
      makeArg packed (EV n) t = do
        (packer, packerpath) <- selectFunction t Pack h
        (unpacker, unpackerpath) <- selectFunction t Unpack h
        return $ Argument
          { argName = n
          , argType = t
          , argPacker = packer
          , argPackerPath = packerpath
          , argUnpacker = unpacker
          , argUnpackerPath = unpackerpath
          , argIsPacked = packed
          }

      updateArguments :: Meta -> [Argument] -> [Argument] -> MorlocMonad [Argument]
      updateArguments m oldargs newargs = case metaName m of
        -- the lambda is named, which means we are jumping into a new scope
        -- where the old arguments are not defined
        (Just _) -> return newargs
        -- we are in an anonymous lambda, where arguments from the outside
        -- scope are still defined
        Nothing -> do
          let oldargs' = [x | x <- oldargs, not (elem (argName x) (map argName newargs))]
          return $ newargs ++ oldargs'

      -- | choose a packer or unpacker for a given type
      selectFunction :: Type -> Property -> SerialMap -> MorlocMonad (Name, Path)
      selectFunction t p h = case mostSpecificSubtypes t (Map.keys hmap) of
        [] -> MM.throwError . OtherError $ "No packer found"
        (x:_) -> case Map.lookup x hmap of
          (Just x) -> return x
          Nothing -> MM.throwError . OtherError $ "I swear it used to be there"
        where
          hmap = if p == Pack then packers h else unpackers h

  realize' lang rs (SAnno (AppS e funargs) ms) = do
    (t, m, lang') <- selectRealization lang ms
    e' <- realize' lang' rs e
    funargs' <- mapM (realize' lang' rs) funargs
    return (SAnno (AppS e' funargs') (t, m {metaArgs = rs}))

  realize' lang rs (SAnno (ListS xs) ms) = do
    (t, m, lang') <- selectRealization lang ms
    xs' <- mapM (realize' lang' rs) xs
    return $ SAnno (ListS xs') (t, m)

  realize' lang rs (SAnno (TupleS xs) ms) = do
    (t, m, lang') <- selectRealization lang ms
    xs' <- mapM (realize' lang' rs) xs
    return $ SAnno (TupleS xs') (t, m)

  realize' lang rs (SAnno (RecS entries) ms) = do
    (t, m, lang') <- selectRealization lang ms
    xs' <- mapM (realize' lang' rs) (map snd entries)
    let entries' = zip (map fst entries) xs'
    return $ SAnno (RecS entries') (t, m)

  realize' lang _ (SAnno (NumS x) ms) = do
    (t, m, lang') <- selectRealization lang ms
    return (SAnno (NumS x) (t, m))

  realize' lang _ (SAnno (LogS x) ms) = do
    (t, m, lang') <- selectRealization lang ms
    return (SAnno (LogS x) (t, m))

  realize' lang _ (SAnno (StrS x) ms) = do
    (t, m, lang') <- selectRealization lang ms
    return (SAnno (StrS x) (t, m))

  realize' lang _ (SAnno (VarS x) ms) = do
    (t, m, _) <- selectRealization lang ms
    return (SAnno (VarS x) (t, m))

  -- | If the parent has a defined language, then try to find a realization
  -- that matches, otherwise choose the first realization in the list.
  selectRealization :: Maybe Lang -> [(Type, Meta)] -> MorlocMonad (Type, Meta, Maybe Lang)
  selectRealization Nothing xs =
    case filter (\(t, _) -> isJust (langOf t)) xs of
      [] -> MM.throwError . OtherError $ "No concrete realization found"
      ((t, m):_) -> return (t, m, langOf t)
  selectRealization (Just lang) xs =
    case filter (\(t, _) -> lang == langOf' t) xs of
     ((t, m):_) -> return (t, m, langOf t)
     [] -> selectRealization Nothing xs

segment
  :: SAnno (Type, Meta)
  -> MorlocMonad [SAnno (Type, Meta)]
segment s = (\(r,rs)-> r:rs) <$> segment' Nothing Nothing s where 
  segment'
    :: Maybe Meta -- data from the parent
    -> Maybe Type -- child type in parent language
    -> SAnno (Type, Meta)
    -> MorlocMonad (SAnno (Type, Meta), [SAnno (Type, Meta)])
  segment' (Just m0) (Just t0) s@(SAnno (AppS x@(SAnno _ (t2, _)) xs) (t1, m1))
    | langOf' t0 == langOf' t1 = do
        (x', rs') <- segment' (Just m1) Nothing x
        xs' <- zipWithM (\t x -> segment' (Just m1) (Just t) x) (typeArgs t2) xs
        let (args', rss') = unzip xs'
        return (SAnno (AppS x' args') (t1, m1), rs' ++ concat rss')
    | otherwise = do
        -- generate the tree for the other language
        (x', rs') <- segment' (Just m1) (Just t1) s
        let t1' = makeFun (map argType (metaArgs m0) ++ [t0])
            v = EV . fromJust $ metaName m1
        return (SAnno (ForeignS (metaId m1) (langOf' t1)) (t1', m1), x':rs')
  segment' (Just m0) _ (SAnno (LamS vs x) (t1, m1)) = do
    (x', xs') <- segment' (Just m1) (Just . last $ typeArgs t1) x 
    return (SAnno (LamS vs x') (t1, m1), xs')
  segment' (Just m0) (Just (ArrT _ [tExp])) (SAnno (ListS xs) (t1, m1)) = do
    xs' <- mapM (segment' (Just m1) (Just tExp)) xs
    let (elements, rss') = unzip xs'
    return (SAnno (ListS elements) (t1, m1), concat rss')
  segment' (Just m0) (Just (ArrT _ ts0)) (SAnno (TupleS xs) (t1, m1)) = do
    xs' <- zipWithM (\t x -> segment' (Just m1) (Just t) x) ts0 xs
    let (elements, rss') = unzip xs'
    return (SAnno (TupleS elements) (t1, m1), concat rss')
  segment' (Just m0) (Just (RecT ts0)) (SAnno (RecS entries) (t1, m1)) = do
    xs' <- zipWithM
             (\t x -> segment' (Just m1) (Just t) x)
             (map snd ts0)
             (map snd entries)
    let (es', rss') = unzip xs'
        entries' = zip (map fst entries) es'
    return (SAnno (RecS entries') (t1, m1), concat rss')
  segment' _ _ s = return (s, [])

  makeFun :: [Type] -> Type
  makeFun [] = error "No types given"
  makeFun [t] = t
  makeFun (t:ts) = FunT t (makeFun ts)

-- Sort manifolds into pools. Within pools, group manifolds into call sets.
pool
  :: [SAnno (Type, Meta)]
  -> MorlocMonad [(Lang, [SAnno (Type, Meta)])]
pool = return . groupSort . map (\s -> (langOf' (sannoType s), s))

encode :: (Lang, [SAnno (Type, Meta)]) -> MorlocMonad Script
encode (lang, xs) = do
  g <- selectGrammar lang
  state <- MM.get
  code <- mapM (codify g) xs >>= makePoolCode g
  return $ Script
    { scriptBase = "pool"
    , scriptLang = lang
    , scriptCode = render code
    , scriptCompilerFlags =
        filter (/= "") . map packageGccFlags $ statePackageMeta state
    , scriptInclude =
        map MS.takeDirectory [s | Source _ _ (Just s) _ <- findSources xs]
    }

makePoolCode :: Grammar -> [SAnno (Type, Meta, MDoc)] -> MorlocMonad MDoc
makePoolCode g xs = do
  srcs <- encodeSources g xs
  let manifolds = conmap gatherManifolds xs
      signatures = conmap (encodeSignatures g) xs
  gMain g $ PoolMain
    { pmSources = srcs
    , pmSignatures = signatures
    , pmPoolManifolds = manifolds
    , pmDispatchManifold = makeDispatchBuilder g xs
    }

gatherManifolds :: SAnno (Type, Meta, MDoc) -> [MDoc]
gatherManifolds = catMaybes . unpackSAnno f where
  f :: SExpr (Type, Meta, MDoc) -> (Type, Meta, MDoc) -> Maybe MDoc
  f (AppS _ _) (_, _, x) = Just x
  f (ForeignS _ _) (_, _, x) = Just x
  f _ _ = Nothing

encodeSources :: Grammar -> [SAnno (Type, Meta, MDoc)] -> MorlocMonad [MDoc]
encodeSources g xs = fmap catMaybes $ mapM encodeSource (findSources' xs) where
  encodeSource :: Source -> MorlocMonad (Maybe MDoc)
  encodeSource src = case srcPath src of
    (Just path) -> (Just . (gImport g) "") <$> (gPrepImport g) path
    Nothing -> return $ Nothing

findSources :: [SAnno (Type, Meta)] -> [Source]
findSources = Set.toList . Set.unions . conmap (unpackSAnno f)
  where
    f :: SExpr (Type, Meta) -> (Type, Meta) -> Set.Set Source
    f _ (_, m) = metaSources m
    f _ _ = Set.empty

findSources' :: [SAnno (Type, Meta, MDoc)] -> [Source]
findSources' = Set.toList . Set.unions . conmap (unpackSAnno f)
  where
    f :: SExpr (Type, Meta, MDoc) -> (Type, Meta, MDoc) -> Set.Set Source
    f _ (_, m, _) = metaSources m
    f _ _ = Set.empty

encodeSignatures :: Grammar -> SAnno (Type, Meta, MDoc) -> [MDoc]
encodeSignatures = undefined
-- encodeSignatures g = map (makeSignature g) . unpackSAnno f where
--   f :: SExpr (Type, Meta, MDoc) -> (Type, Meta, MDoc) -> (Meta, MDoc)
--   f (AppS _ _) (_, m, x) = Just (m, x)
--   f (ForeignS _ _) (_, m, x) = Just (m, x)
--   f _ _ = Nothing
--
--   -- | Create a signature/prototype. Not all languages need this. C and C++ need
--   -- prototype definitions, but R and Python do not. Languages that do not
--   -- require signatures may simply write the type information as comments at the
--   -- beginning of the source file.
--   makeSignature :: Grammar -> (Meta, Type) -> MDoc
--   makeSignature g (m, t) = (gSignature g) $ GeneralFunction
--     { gfComments = ""
--     , gfReturnType = Just . gShowType g . last . typeArgs $ t
--     , gfName = makeManifoldName m
--     , gfArgs = map (prepArg g) (metaArgs m)
--     , gfBody = ""
--     }

makeDispatchBuilder :: Grammar -> [SAnno (Type, Meta, MDoc)] -> (MDoc -> MDoc -> MDoc)
makeDispatchBuilder = undefined
-- makeDispatchBuilder g xs = makeDispatcher g (mapM (unpackSAnno f) xs) where
--   f :: SExpr (Type, Meta, MDoc) -> (Type, Meta, MDoc) -> (Meta, MDoc, Name)
--   f (AppS _ _) (_, m, x) = Just (m, x, metaPacker m)
--   f _ _ = Nothing

-- | Make a function for generating the code to dispatch from the pool main
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
makeDispatcher = undefined
-- makeDispatcher g xs =
--   (gSwitch g) (\(_,m,_) -> pretty (metaId m))
--               (\(t,m,p) -> mainCall g t m p)
--               xs
--   where
--     mainCall :: Grammar -> Type -> Meta -> Name -> MDoc
--     mainCall g t m packer = (gCall g)
--       (pretty packer)
--       [(gCall g)
--           (makeManifoldName m)
--           (take (length (metaArgs m))
--           (gCmdArgs g))]

codify
  :: Grammar
  -> SAnno (Type, Meta)
  -> MorlocMonad (SAnno (Type, Meta, MDoc))
codify = undefined
--
-- -- | UniS
-- codify g (SAnno UniS (t, m)) = return $ SAnno UniS (t, m, gNull g)
--
-- -- | VarS EVar
-- codify _ (SAnno (VarS v) (t, m)) = return $ SAnno (VarS v) (t, m, pretty v)
--
-- -- | ListS [SAnno a]
-- codify g (SAnno (ListS xs) (t, m)) = do
--   xs' <- mapM codify xs
--   let mdoc = (gList g) (map getdoc xs')
--   return $ SAnno (ListS xs') (t, m, mdoc)
--
-- -- | TupleS [SAnno a]
-- codify g (SAnno (TupleS xs) (t, m)) = do
--   xs' <- mapM codify xs
--   let mdoc = (gTuple g) (map getdoc xs')
--   return $ SAnno (TupleS xs') (t, m, mdoc)
--
-- -- | RecS [(EVar, SAnno a)]
-- codify g (SAnno (RecS entries) (t, m)) = do
--   xs' <- mapM codify (map snd xs)
--   let mdoc = (gRecord g) (zip (map getdoc xs') (map (pretty . fst) xs))
--   return $ SAnno (RecS (zip xs' (map fst xs))) (t, m, mdoc)
--
-- -- | LamS [EVar] (SAnno a)
-- codify g (SAnno (LamS vs x) (t, m)) = codify g x
--
-- -- | NumS Scientific
-- codify _ (SAnno (NumS x) (t, m)) = (SAnno (NumS x) (t, m, pretty x))
--
-- -- | LogS Bool
-- codify _ (SAnno (LogS x) (t, m)) = (SAnno (NumS x) (t, m, (gBool g) x))
--
-- -- | StrS MT.Text
-- codify _ (SAnno (StrS x) (t, m)) = (SAnno (NumS x) (t, m, (gQuote g) x))
--
-- -- | ForeignS Int Lang
-- codify g (SAnno (ForeignS i lang) (t, m)) = do
--   config <- MM.ask
--   mdoc <- case MC.getPoolCallBuilder config lang (gQuote g) of
--     Nothing -> MM.throwError . UnknownError $ "ah for fuck sake!!!"
--     (Just poolBuilder) -> do
--       let exe = Lang.makeExecutableName lang "pool"
--       return $ ForeignCallDoc
--         { fcdForeignPool = Lang.makeSourceName lang "pool"
--         , fcdForeignExe = exe
--         , fcdMid = pretty i
--         , fcdArgs = map prepArg (metaArgs m)
--         , fcdCall = poolBuilder exe (pretty i)
--         , fcdFile = Lang.makeSourceName (langOf' t) "pool"
--         }
--   return (t, m, mdoc)
--   where
--     prepArg :: Argument -> MDoc
--     prepArg arg =
--       if isPacked arg
--         then argName
--         else (gCall g) (argUnpacker arg) [argName]
--
-- -- | AppS (SAnno a) [SAnno a]
-- codify g (SAnno (AppS e args) (t, m)) = do
--   e2 <- codify e
--   args' <- mapM codify args
--   mandoc <- makeManifoldDoc args' e2
--   return $ SAnno (AppS e2 args') (t, m', mandoc)
--   where
--     makeManifoldDoc
--       :: [SAnno (Type, Meta, MDoc)] -- inputs
--       -> SAnno (Type, Meta, MDoc)
--       -> MorlocMonad MDoc
--     makeManifoldDoc inputs (SAnno _ (t, _, _)) = do
--       let otype = last (typeArgs t)
--           margs = metaArgs meta
--       name <- case metaName meta of
--         (Just n) -> return n
--         Nothing -> MM.throwError . OtherError $ "No name found for manifold"
--       inputs' <- zipWithM (prepInput margs) [0..] inputs
--       return . gFunction g $ GeneralFunction
--         { gfComments = prettyType t <> line
--         , gfReturnType = Just ((gShowType g) otype)
--         , gfName = makeManifoldName m
--         , gfArgs = map prepArg margs
--         , gfBody =
--             vsep (catMaybes (map snd inputs')) <> line <>
--             (gReturn g $ (gCall g) (pretty name) (map fst inputs'))
--         }
--
--     -- Handle preparation of arguments passed to a manifold. Return the name of the
--     -- variable that will be used and an block of code that defines the variable
--     -- (if necessary).
--     prepInput
--       :: [Argument] -- all arguments of the manifold
--       -> Int
--       -> SAnno (Type, Meta, MDoc) -- an input to the wrapped function
--       -> MorlocMonad (MDoc, Maybe MDoc)
--     prepInput rs i (SAnno (AppS x xs) (t, m, d)) = do
--       let varname = makeArgumentName i
--           mid = makeManifoldName m
--           ass = (gAssign g) $ GeneralAssignment
--             { gaType = Just . gShowType g . last . typeArgs $ t
--             , gaName = varname
--             , gaValue = (gCall g) mid (map (pretty . argName) rs)
--             , gaArg = Nothing
--             }
--       return (varname, Just ass)
--     prepInput rs i (SAnno _ (t, m, d)) = do
--       let name = metaName m
--           varname = makeArgumentName i
--           arg = listToMaybe [r | r <- rs, Just (argName r) == metaName m]
--       case (name, arg, fmap argIsPacked arg) of
--         (Just n, Just r, Just True) ->
--            return (varname, Just . gAssign g $ GeneralAssignment
--               { gaType = Just . gShowType g . argType $ r
--               , gaName = varname
--               , gaValue = (gCall g) (pretty (argUnpacker r)) [pretty name]
--               , gaArg = Nothing
--               }
--            )
--         -- the argument is used in the wrapped function, but is not serialized
--         (Just n, _, Just False) -> return (pretty name, Nothing)
--         -- the argument is not used in the wrapped function
--         (Just n, _, Nothing) -> return (pretty name, Nothing)
--         x -> MM.throwError . OtherError $ MT.show' x
--
--     -- | Serialize the result of a call if a serialization function is defined.
--     -- Presumably, if no serialization function is given, then the argument is
--     -- either a native construct or will be passed on in serialized form.
--     prepArg :: Argument -> (Maybe MDoc, MDoc)
--     prepArg r = (Just . gShowType g $ t, pretty n) where
--       t = if argIsPacked r
--             then gSerialType g
--             else argType r
--       n = argName r

-------- Utility and lookup functions ----------------------------------------

getdoc :: SAnno (Type, Meta, MDoc) -> MDoc 
getdoc (SAnno _ (_, _, d)) = d

unrollLambda :: Expr -> ([EVar], Expr)
unrollLambda (LamE v e2) = case unrollLambda e2 of
  (vs, e) -> (v:vs, e)
unrollLambda e = ([], e)

getGeneralType :: [Type] -> MorlocMonad (Maybe Type)
getGeneralType ts = case [t | t <- ts, langOf' t == MorlocLang] of 
    [] -> return Nothing
    [x] -> return $ Just x
    xs -> MM.throwError . OtherError $
      "Expected 0 or 1 general types, found " <> MT.show' (length xs)

typeArgs :: Type -> [Type]
typeArgs (FunT t1 t2) = t1 : typeArgs t2
typeArgs t = [t]

exprArgs :: SExpr a -> [Name]
exprArgs (LamS vs _) = [name | (EV name) <- vs]
exprArgs _ = []

sannoType :: SAnno (Type, Meta) -> Type
sannoType (SAnno _ (t, _)) = t

-- | Map a language to a grammar
selectGrammar :: Lang -> MorlocMonad Grammar
selectGrammar CLang       = return GrammarC.grammar
selectGrammar CppLang     = return GrammarCpp.grammar
selectGrammar RLang       = return GrammarR.grammar
selectGrammar Python3Lang = return GrammarPython3.grammar
selectGrammar MorlocLang = MM.throwError . OtherError $
  "No grammar exists for MorlocLang, this may be a compiler bug"
selectGrammar lang = MM.throwError . OtherError $
  "No grammar found for " <> MT.show' lang

unpackSAnno :: (SExpr a -> a -> b) -> SAnno a -> [b] 
unpackSAnno f (SAnno e@(ListS xs) m) = f e m : conmap (unpackSAnno f) xs
unpackSAnno f (SAnno e@(TupleS xs) m) = f e m : conmap (unpackSAnno f) xs
unpackSAnno f (SAnno e@(RecS entries) m) = f e m : conmap (unpackSAnno f) (map snd entries)
unpackSAnno f (SAnno e@(LamS _ x) m) = f e m : unpackSAnno f x
unpackSAnno f (SAnno e@(AppS x xs) m) = f e m : conmap (unpackSAnno f) (x:xs)
unpackSAnno f (SAnno e m) = [f e m]

conmap :: (a -> [b]) -> [a] -> [b]
conmap f = concat . map f

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
