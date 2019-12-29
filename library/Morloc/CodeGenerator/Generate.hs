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

-- | XXXXXXXXXXXXXX-DOUBLE CHECK Store all necessary information about a particular implementation of a
-- term.  A term may either be declared or sourced. If declared, the left and
-- right hand sides of the declaration are stored. If sourced, the Source
-- object is stored. In either case, the module where the term is defined is
-- also stored.
--
-- XXXXXXXXXXXXXX-DOUBLE CHECK Each element of the list consists of an EVar that is the term exported from
-- the main module. This term may be a named composition in the main module, a
-- sourced function/value from language-specific code, or an imported term from
-- another module. A term may be defined in multiple modules or sourced from
-- multiple implementations. Thus each term exported from main is associated
-- with a list of possible implementations/realizations.
data TermOrigin = Declared Module EVar Expr | Sourced Module Source

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
  nexus <- Nexus.generate [ (metaType c, poolId m x, metaName m)
                          | SAnno (One (x, c)) m <- ast
                          ]

  -- for each language, collect all functions into one "pool"
  pools <- mapM segment ast |>> concat >>= pool >>= mapM encode

  -- return the nexus script and each pool script
  return (nexus, pools)
  where
    -- map from nexus id to pool id
    -- these differ when a declared variable is exported
    poolId :: GMeta -> SExpr GMeta One CMeta -> Int
    poolId _ (LamS _ (SAnno _ meta)) = metaId meta
    poolId meta _ = metaId meta

-- | Find the expressions that are exposed to the user.
roots :: Map.Map MVar Module -> MorlocMonad [(EVar, [TermOrigin])]
roots ms = do
  case roots of
    [m] ->
      let vs = Set.toList (moduleExports m) in
        return $ zip vs (map (findTerm ms m) vs) 
    [] -> MM.throwError . OtherError $ "Circular dependencies between modules"
    _ -> MM.throwError . OtherError $ "Multiple root modules"
  where
    isRoot m = not $ Set.member (moduleName m) allImports
    allImports = mapSumWith (valset . moduleImportMap) ms
    roots = filter isRoot (Map.elems ms)

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


-- | Translate a term to a tree
collect
  :: Map.Map MVar Module
  -> (EVar, [TermOrigin])
  -> MorlocMonad (SAnno GMeta Many [CMeta])
collect ms (evar', xs@(x:_)) = do
  gmeta <- makeGMeta (Just evar') (getTermModule x)
  trees <- mapM collectTerm xs
  return $ SAnno (Many trees) gmeta
  where

    -- Notice that `args` is NOT an intput to collectTerm. Morloc uses lexical
    -- scoping, and the input to collectTerm is the origin of a term, so the
    -- definition of the term is outside the scope of the parent expression.
    collectTerm
      :: TermOrigin
      -> MorlocMonad (SExpr GMeta Many [CMeta], [CMeta])
    collectTerm (Declared m _ (AnnE x ts)) = do
      xs <- collectExpr Set.empty m ts x
      case xs of
        [x] -> return x
        _ -> MM.throwError . OtherError $ "Expected exactly one topology for a declared term"
    collectTerm term@(Sourced m src) = do
      ts <- getTermTypes term 
      let cmetas = map (makeCMeta (Just src) m) ts
      return (VarS (srcAlias src), cmetas)

    collectAnno
      :: Set.Set EVar
      -> Expr
      -> Module
      -> MorlocMonad (SAnno GMeta Many [CMeta])
    collectAnno args (AnnE e ts) m = do
      gmeta <- makeGMeta Nothing m
      trees <- collectExpr args m ts e
      return $ SAnno (Many trees) gmeta
    collectAnno _ _ _ = error "impossible bug - unannotated expression" 

    collectExpr
      :: Set.Set EVar
      -> Module
      -> [Type]
      -> Expr
      -> MorlocMonad [(SExpr GMeta Many [CMeta], [CMeta])]
    collectExpr args m ts (UniE) = simpleCollect UniS m ts
    collectExpr args m ts (NumE x) = simpleCollect (NumS x) m ts
    collectExpr args m ts (LogE x) = simpleCollect (LogS x) m ts
    collectExpr args m ts (StrE x) = simpleCollect (StrS x) m ts
    collectExpr args m ts (VarE v)
      | Set.member v args = return [(VarS v, cmetas)]
      | otherwise = mapM collectTerm (findTerm ms m v) -- FIXME: WHENCE ts?
      where
        cmetas = map (makeCMeta Nothing m) ts
    collectExpr args m ts (ListE es) = undefined
    collectExpr args m ts (TupleE es) = undefined
    collectExpr args m ts (LamE v e) = undefined
    collectExpr args m ts (AppE e1 e2) = undefined
    collectExpr args m ts (RecE entries) = undefined
    collectExpr _ _ _ _ = MM.throwError . OtherError $ "Unexpected expression in collectExpr"

    simpleCollect
      :: (SExpr GMeta Many [CMeta])
      -> Module
      -> [Type]
      -> MorlocMonad [(SExpr GMeta Many [CMeta], [CMeta])]
    simpleCollect sexpr m ts = return [(sexpr, map (makeCMeta Nothing m) ts)]

    -- | Find info common across realizations of a given term in a given module
    makeGMeta :: Maybe EVar -> Module -> MorlocMonad GMeta
    makeGMeta evar m = do
      i <- MM.getCounter
      case evar >>= (flip Map.lookup) (moduleTypeMap m) of
        (Just (TypeSet (Just e) _)) -> return $ GMeta
          { metaId = i
          , metaName = fmap (\(EV x) -> x) evar
          , metaGeneralType = Just (etype e)
          , metaProperties = eprop e
          , metaConstraints = econs e
          }
        Nothing -> MM.throwError . OtherError $ "Could not build GMeta"
    makeGMeta _ _ = MM.throwError . OtherError $ "Cannot make GMeta from nothing"

    makeCMeta :: Maybe Source -> Module -> Type -> CMeta
    makeCMeta src m t = CMeta {
          metaLang = langOf' t
        , metaType = t
        , metaSource = src
        , metaModule = moduleName m
      }

-- | Select a single concrete language for each sub-expression.
-- Store the concrete type and the general type (if available).
-- Select serialization functions.
realize
  :: SerialMap
  -> SAnno GMeta Many [CMeta]
  -> MorlocMonad (SAnno GMeta One CMeta)
realize = undefined
-- realize h x = realize' Nothing [] x where
--
--   realize'
--     :: Maybe Lang
--     -> [Argument]
--     -> SAnno [(Type, Meta)]
--     -> MorlocMonad (SAnno (Type, Meta))
--
--   -- new arguments may be bound under lambdas
--   realize' lang rs (SAnno (LamS vs x) ms) = do
--     (t, m, lang') <- selectRealization lang ms
--     let isPacked = isNothing lang
--     args <- zipWithM (makeArg isPacked) vs (init $ typeArgs t)
--          >>= updateArguments m rs
--     x' <- realize' lang' args x
--     (packer, packerpath) <- selectFunction (last $ typeArgs t) Pack h
--     let m' = m { metaArgs = args
--                , metaPacker = Just packer
--                , metaPackerPath = Just packerpath
--                }
--     return $ SAnno (LamS vs x') (t, m')
--     where
--       makeArg :: Bool -> EVar -> Type -> MorlocMonad Argument
--       makeArg packed (EV n) t = do
--         (packer, packerpath) <- selectFunction t Pack h
--         (unpacker, unpackerpath) <- selectFunction t Unpack h
--         return $ Argument
--           { argName = n
--           , argType = t
--           , argPacker = packer
--           , argPackerPath = packerpath
--           , argUnpacker = unpacker
--           , argUnpackerPath = unpackerpath
--           , argIsPacked = packed
--           }
--
--       updateArguments :: Meta -> [Argument] -> [Argument] -> MorlocMonad [Argument]
--       updateArguments m oldargs newargs = case metaName m of
--         -- the lambda is named, which means we are jumping into a new scope
--         -- where the old arguments are not defined
--         (Just _) -> return newargs
--         -- we are in an anonymous lambda, where arguments from the outside
--         -- scope are still defined
--         Nothing -> do
--           let oldargs' = [x | x <- oldargs, not (elem (argName x) (map argName newargs))]
--           return $ newargs ++ oldargs'
--
--   realize' lang rs (SAnno (AppS e funargs) ms) = do
--     (t, m, lang') <- selectRealization lang ms
--     e' <- realize' lang' rs e
--     funargs' <- mapM (realize' lang' rs) funargs
--     (packer, packerpath) <- selectFunction (last $ typeArgs t) Pack h
--     let m' = m { metaArgs = rs
--                , metaPacker = Just packer
--                , metaPackerPath = Just packerpath
--                }
--     return (SAnno (AppS e' funargs') (t, m'))
--
--   realize' lang rs (SAnno (ListS xs) ms) = do
--     (t, m, lang') <- selectRealization lang ms
--     xs' <- mapM (realize' lang' rs) xs
--     return $ SAnno (ListS xs') (t, m)
--
--   realize' lang rs (SAnno (TupleS xs) ms) = do
--     (t, m, lang') <- selectRealization lang ms
--     xs' <- mapM (realize' lang' rs) xs
--     return $ SAnno (TupleS xs') (t, m)
--
--   realize' lang rs (SAnno (RecS entries) ms) = do
--     (t, m, lang') <- selectRealization lang ms
--     xs' <- mapM (realize' lang' rs) (map snd entries)
--     let entries' = zip (map fst entries) xs'
--     return $ SAnno (RecS entries') (t, m)
--
--   realize' lang _ (SAnno (NumS x) ms) = do
--     (t, m, lang') <- selectRealization lang ms
--     return (SAnno (NumS x) (t, m))
--
--   realize' lang _ (SAnno (LogS x) ms) = do
--     (t, m, lang') <- selectRealization lang ms
--     return (SAnno (LogS x) (t, m))
--
--   realize' lang _ (SAnno (StrS x) ms) = do
--     (t, m, lang') <- selectRealization lang ms
--     return (SAnno (StrS x) (t, m))
--
--   realize' lang _ (SAnno (VarS x) ms) = do
--     (t, m, _) <- selectRealization lang ms
--     return (SAnno (VarS x) (t, m))
--
--   -- | If the parent has a defined language, then try to find a realization
--   -- that matches, otherwise choose the first realization in the list.
--   selectRealization :: Maybe Lang -> [(Type, Meta)] -> MorlocMonad (Type, Meta, Maybe Lang)
--   selectRealization Nothing xs =
--     case filter (\(t, _) -> isJust (langOf t)) xs of
--       [] -> MM.throwError . OtherError $ "No concrete realization found"
--       ((t, m):_) -> return (t, m, langOf t)
--   selectRealization (Just lang) xs =
--     case filter (\(t, _) -> lang == langOf' t) xs of
--      ((t, m):_) -> return (t, m, langOf t)
--      [] -> selectRealization Nothing xs


-- | choose a packer or unpacker for a given type
selectFunction :: Type -> Property -> SerialMap -> MorlocMonad (Name, Path)
selectFunction = undefined
-- selectFunction t p h = case mostSpecificSubtypes t (Map.keys hmap) of
--   [] -> MM.throwError . OtherError $ "No packer found"
--   (x:_) -> case Map.lookup x hmap of
--     (Just x) -> return x
--     Nothing -> MM.throwError . OtherError $ "I swear it used to be there"
--   where
--     hmap = if p == Pack then packers h else unpackers h


segment
  :: SAnno GMeta One CMeta
  -> MorlocMonad [SAnno GMeta One CMeta]
segment = undefined
-- segment s = (\(r,rs)-> r:rs) <$> segment' Nothing Nothing s where
--   segment'
--     :: Maybe Meta -- data from the parent
--     -> Maybe Type -- child type in parent language
--     -> SAnno (Type, Meta)
--     -> MorlocMonad (SAnno (Type, Meta), [SAnno (Type, Meta)])
--   segment' (Just m0) (Just t0) s@(SAnno (AppS x@(SAnno _ (t2, _)) xs) (t1, m1))
--     | langOf' t0 == langOf' t1 = do
--         (x', rs') <- segment' (Just m1) Nothing x
--         xs' <- zipWithM (\t x -> segment' (Just m1) (Just t) x) (typeArgs t2) xs
--         let (args', rss') = unzip xs'
--         return (SAnno (AppS x' args') (t1, m1), rs' ++ concat rss')
--     | otherwise = do
--         -- generate the tree for the other language
--         (x', rs') <- segment' (Just m1) (Just t1) s
--         let t1' = makeFun (map argType (metaArgs m0) ++ [t0])
--         v <- case metaName m1 of
--           (Just v') -> return v'
--           Nothing -> MM.throwError . OtherError $ "Expected a named function"
--         return (SAnno (ForeignS (metaId m1) (langOf' t1)) (t1', m1), x':rs')
--   segment' (Just m0) _ (SAnno (LamS vs x) (t1, m1)) = do
--     (x', xs') <- segment' (Just m1) (Just . last $ typeArgs t1) x
--     return (SAnno (LamS vs x') (t1, m1), xs')
--   segment' (Just m0) (Just (ArrT _ [tExp])) (SAnno (ListS xs) (t1, m1)) = do
--     xs' <- mapM (segment' (Just m1) (Just tExp)) xs
--     let (elements, rss') = unzip xs'
--     return (SAnno (ListS elements) (t1, m1), concat rss')
--   segment' (Just m0) (Just (ArrT _ ts0)) (SAnno (TupleS xs) (t1, m1)) = do
--     xs' <- zipWithM (\t x -> segment' (Just m1) (Just t) x) ts0 xs
--     let (elements, rss') = unzip xs'
--     return (SAnno (TupleS elements) (t1, m1), concat rss')
--   segment' (Just m0) (Just (RecT ts0)) (SAnno (RecS entries) (t1, m1)) = do
--     xs' <- zipWithM
--              (\t x -> segment' (Just m1) (Just t) x)
--              (map snd ts0)
--              (map snd entries)
--     let (es', rss') = unzip xs'
--         entries' = zip (map fst entries) es'
--     return (SAnno (RecS entries') (t1, m1), concat rss')
--   segment' _ _ s = return (s, [])
--
--   makeFun :: [Type] -> Type
--   makeFun [] = error "No types given"
--   makeFun [t] = t
--   makeFun (t:ts) = FunT t (makeFun ts)


-- Sort manifolds into pools. Within pools, group manifolds into call sets.
pool
  :: [SAnno GMeta One CMeta]
  -> MorlocMonad [(Lang, [SAnno GMeta One CMeta])]
pool = undefined
-- pool = return . groupSort . map (\s -> (langOf' (sannoType s), s))


encode :: (Lang, [SAnno GMeta One CMeta]) -> MorlocMonad Script
encode = undefined
-- encode (lang, xs) = do
--   g <- selectGrammar lang
--   state <- MM.get
--   code <- mapM (codify g) xs >>= makePoolCode g
--   return $ Script
--     { scriptBase = "pool"
--     , scriptLang = lang
--     , scriptCode = render code
--     , scriptCompilerFlags =
--         filter (/= "") . map packageGccFlags $ statePackageMeta state
--     , scriptInclude =
--         map MS.takeDirectory [s | Source _ _ (Just s) _ <- findSources xs]
--     }


makePoolCode :: Grammar -> [SAnno (GMeta, MDoc) One CMeta] -> MorlocMonad MDoc
makePoolCode = undefined
-- makePoolCode g xs = do
--   srcs <- encodeSources g xs
--   let manifolds = conmap gatherManifolds xs
--       signatures = conmap (encodeSignatures g) xs
--   gMain g $ PoolMain
--     { pmSources = srcs
--     , pmSignatures = signatures
--     , pmPoolManifolds = manifolds
--     , pmDispatchManifold = makeDispatchBuilder g xs
--     }


gatherManifolds :: SAnno (GMeta, MDoc) One CMeta -> [MDoc]
gatherManifolds = undefined
-- gatherManifolds = catMaybes . unpackSAnno f where
--   f :: SExpr (Type, Meta, MDoc) -> (Type, Meta, MDoc) -> Maybe MDoc
--   f (AppS _ _) (_, _, d) = Just d
--   f (ForeignS _ _) (_, _, x) = Just x
--   f _ _ = Nothing


encodeSources :: Grammar -> [SAnno (GMeta, MDoc) One CMeta] -> MorlocMonad [MDoc]
encodeSources = undefined
-- encodeSources g xs = do
--   let srcs = findSources' xs
--   say $ "sources:"
--   say $ viaShow srcs
--   fmap catMaybes $ mapM encodeSource srcs
--   where
--     encodeSource :: Source -> MorlocMonad (Maybe MDoc)
--     encodeSource src = case srcPath src of
--       (Just path) -> (Just . (gImport g) "") <$> (gPrepImport g) path
--       Nothing -> return $ Nothing


findSources :: [SAnno GMeta One CMeta] -> [Source]
findSources = undefined
-- findSources = Set.toList . Set.unions . conmap (unpackSAnno f)
--   where
--     f :: SExpr (Type, Meta) -> (Type, Meta) -> Set.Set Source
--     f _ (_, m) = metaSources m
--     f _ _ = Set.empty


findSources' :: [SAnno (GMeta, MDoc) One CMeta] -> [Source]
findSources' = undefined
-- findSources' = Set.toList . Set.unions . conmap (unpackSAnno f)
--   where
--     f :: SExpr (Type, Meta, MDoc) -> (Type, Meta, MDoc) -> Set.Set Source
--     f _ (_, m, _) = metaSources m


-- | Create a signature/prototype. Not all languages need this. C and C++ need
-- prototype definitions, but R and Python do not. Languages that do not
-- require signatures may simply write the type information as comments at the
-- beginning of the source file.
encodeSignatures :: Grammar -> SAnno (GMeta, MDoc) One CMeta -> [MDoc]
encodeSignatures = undefined
-- encodeSignatures g = map (makeSignature g) . catMaybes . unpackSAnno f where
--   f :: SExpr (Type, Meta, MDoc) -> (Type, Meta, MDoc) -> Maybe (Meta, Type)
--   f (AppS _ _) (t, m, _) = Just (m, t)
--   f (ForeignS _ _) (t, m, _) = Just (m, t)
--   f _ _ = Nothing
--
--   makeSignature :: Grammar -> (Meta, Type) -> MDoc
--   makeSignature g (m, t) = (gSignature g) $ GeneralFunction
--     { gfComments = ""
--     , gfReturnType = Just . gShowType g . last . typeArgs $ t
--     , gfName = makeManifoldName m
--     , gfArgs = map (makeArg g) (metaArgs m)
--     , gfBody = ""
--     }
--
--   makeArg :: Grammar -> Argument -> (Maybe MDoc, MDoc)
--   makeArg g arg =
--     if argIsPacked arg
--       then (Just ((gShowType g) (gSerialType g)), pretty (argName arg))
--       else (Nothing, pretty (argName arg))


-- | Make a function for generating the code to dispatch from the pool main
-- function to manifold functions. The two arguments of the returned function
-- (MDoc->MDoc->MDoc) are 1) the manifold id and 2) the variable name where the
-- results are stored.
makeDispatchBuilder :: Grammar -> [SAnno (GMeta, MDoc) One CMeta] -> (MDoc -> MDoc -> MDoc)
makeDispatchBuilder = undefined
-- makeDispatchBuilder g xs =
--   (gSwitch g)
--     (\(_,m,_) -> pretty (metaId m))
--     (\(t,m,p) -> mainCall g t m p)
--     switchList
--   where
--     switchList = [(t, m, fromJust (metaPacker m)) | (SAnno (AppS _ _) (t, m, _)) <- xs]
--
--     mainCall :: Grammar -> Type -> Meta -> Name -> MDoc
--     mainCall g t m packer = (gCall g)
--       (pretty packer)
--       [(gCall g)
--           (makeManifoldName m)
--           (take (length (metaArgs m))
--           (gCmdArgs g))]

codify
  :: Grammar
  -> SAnno GMeta One CMeta
  -> MorlocMonad (SAnno (GMeta, MDoc) One CMeta) 
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
--   xs' <- mapM (codify g) xs
--   let mdoc = (gList g) (map getdoc xs')
--   return $ SAnno (ListS xs') (t, m, mdoc)
--
-- -- | TupleS [SAnno a]
-- codify g (SAnno (TupleS xs) (t, m)) = do
--   xs' <- mapM (codify g) xs
--   let mdoc = (gTuple g) (map getdoc xs')
--   return $ SAnno (TupleS xs') (t, m, mdoc)
--
-- -- | RecS [(EVar, SAnno a)]
-- codify g (SAnno (RecS entries) (t, m)) = do
--   xs <- mapM (codify g) (map snd entries)
--   let mdoc = (gRecord g) (zip (map (pretty . fst) entries) (map getdoc xs))
--   return $ SAnno (RecS (zip (map fst entries) xs)) (t, m, mdoc)
--
-- -- | LamS [EVar] (SAnno a)
-- codify g (SAnno (LamS vs x) (t, m)) = codify g x
--
-- -- | NumS Scientific
-- codify _ (SAnno (NumS x) (t, m)) = return $ SAnno (NumS x) (t, m, viaShow x)
--
-- -- | LogS Bool
-- codify g (SAnno (LogS x) (t, m)) = return $ SAnno (LogS x) (t, m, (gBool g) x)
--
-- -- | StrS MT.Text
-- codify g (SAnno (StrS x) (t, m)) = return $ (SAnno (StrS x) (t, m, (gQuote g) (pretty x)))
--
-- -- | ForeignS Int Lang
-- codify g (SAnno (ForeignS i lang) (t, m)) = do
--   config <- MM.ask
--   mdoc <- case MC.getPoolCallBuilder config lang (gQuote g) of
--     Nothing -> MM.throwError . OtherError $ "ah for fuck sake!!!"
--     (Just poolBuilder) -> do
--       let exe = pretty $ Lang.makeExecutableName lang "pool"
--       return . gForeignCall g $ ForeignCallDoc
--         { fcdForeignPool = pretty $ Lang.makeSourceName lang "pool"
--         , fcdForeignExe = exe
--         , fcdMid = pretty i
--         , fcdArgs = map cliArg (metaArgs m)
--         , fcdCall = poolBuilder exe (pretty i)
--         , fcdFile = pretty $ Lang.makeSourceName (langOf' t) "pool"
--         }
--   return $ SAnno (ForeignS i lang) (t, m, mdoc)
--   where
--     cliArg :: Argument -> MDoc
--     cliArg r =
--       if argIsPacked r
--         then pretty (argName r)
--         else (gCall g) (pretty $ argUnpacker r) [pretty $ argName r]
--
-- -- | AppS (SAnno a) [SAnno a]
-- codify g (SAnno (AppS e args) (t, m)) = do
--   e2 <- codify g e
--   args' <- mapM (codify g) args
--   mandoc <- makeManifoldDoc args' e2
--   return $ SAnno (AppS e2 args') (t, m, mandoc)
--   where
--
--     makeManifoldDoc
--       :: [SAnno (Type, Meta, MDoc)] -- inputs
--       -> SAnno (Type, Meta, MDoc)
--       -> MorlocMonad MDoc
--     makeManifoldDoc inputs (SAnno _ (t, m, _)) = do
--       let otype = last (typeArgs t)
--           margs = metaArgs m
--       name <- case metaName m of
--         (Just n) -> return n
--         Nothing -> MM.throwError . OtherError $ "No name found for manifold"
--       inputs' <- zipWithM (prepInput margs) [0..] inputs
--       return . gFunction g $ GeneralFunction
--         { gfComments = prettyType t <> line
--         , gfReturnType = Just ((gShowType g) otype)
--         , gfName = makeManifoldName m
--         , gfArgs = map (prepArg g) margs
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
--     prepArg :: Grammar -> Argument -> (Maybe MDoc, MDoc)
--     prepArg g r = (Just . gShowType g $ t, pretty n) where
--       t = if argIsPacked r
--             then gSerialType g
--             else argType r
--       n = argName r



-------- Utility and lookup functions ----------------------------------------

getdoc :: SAnno (GMeta, MDoc) One CMeta -> MDoc 
getdoc (SAnno _ (_, d)) = d

unrollLambda :: Expr -> ([EVar], Expr)
unrollLambda (LamE v e2) = case unrollLambda e2 of
  (vs, e) -> (v:vs, e)
unrollLambda e = ([], e)

getGeneralType :: [Type] -> MorlocMonad (Maybe Type)
getGeneralType = undefined
-- getGeneralType ts = case [t | t <- ts, langOf' t == MorlocLang] of
--     [] -> return Nothing
--     [x] -> return $ Just x
--     xs -> MM.throwError . OtherError $
--       "Expected 0 or 1 general types, found " <> MT.show' (length xs)

typeArgs :: Type -> [Type]
typeArgs (FunT t1 t2) = t1 : typeArgs t2
typeArgs t = [t]

exprArgs :: SExpr g f c -> [Name]
exprArgs (LamS vs _) = [name | (EV name) <- vs]
exprArgs _ = []

makeManifoldName :: GMeta -> MDoc
makeManifoldName m = pretty $ "m" <> MT.show' (metaId m)

makeArgumentName :: Int -> MDoc
makeArgumentName i = "x" <> pretty i

sannoType :: SAnno g One CMeta -> Type
sannoType (SAnno (One (_, c)) _) = metaType c

getTermModule :: TermOrigin -> Module
getTermModule (Sourced m _) = m
getTermModule (Declared m _ _) = m

getTermEVar :: TermOrigin -> EVar
getTermEVar (Sourced _ src) = srcAlias src
getTermEVar (Declared _ v _) = v

getTermTypeSet :: TermOrigin -> MorlocMonad TypeSet
getTermTypeSet t =
  case Map.lookup (getTermEVar t) (moduleTypeMap (getTermModule t)) of
    (Just ts) -> return ts
    Nothing -> MM.throwError . OtherError $ "Expected the term to have a typeset"

getTermTypes :: TermOrigin -> MorlocMonad [Type]
getTermTypes t = do
  (TypeSet _ es) <- getTermTypeSet t
  return $ map etype es

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

unpackSAnnoG :: (SExpr g One c -> g -> g') -> SExpr g One c -> [g'] 
unpackSAnnoG = undefined
-- unpackSAnnoG f (SAnno e@(ListS xs) m) = f e m : conmap (unpackSAnnoG f) xs
-- unpackSAnnoG f (SAnno e@(TupleS xs) m) = f e m : conmap (unpackSAnnoG f) xs
-- unpackSAnnoG f (SAnno e@(RecS entries) m) = f e m : conmap (unpackSAnnoG f) (map snd entries)
-- unpackSAnnoG f (SAnno e@(LamS _ x) m) = f e m : unpackSAnnoG f x
-- unpackSAnnoG f (SAnno e@(AppS x xs) m) = f e m : conmap (unpackSAnnoG f) (x:xs)
-- unpackSAnnoG f (SAnno e m) = [f e m]

unpackSAnnoC :: (SExpr g One c -> c -> c') -> SExpr g One c -> [c'] 
unpackSAnnoC = undefined
-- unpackSAnnoC f (SAnno e@(ListS xs) m) = f e m : conmap (unpackSAnnoC f) xs
-- unpackSAnnoC f (SAnno e@(TupleS xs) m) = f e m : conmap (unpackSAnnoC f) xs
-- unpackSAnnoC f (SAnno e@(RecS entries) m) = f e m : conmap (unpackSAnnoC f) (map snd entries)
-- unpackSAnnoC f (SAnno e@(LamS _ x) m) = f e m : unpackSAnnoC f x
-- unpackSAnnoC f (SAnno e@(AppS x xs) m) = f e m : conmap (unpackSAnnoC f) (x:xs)
-- unpackSAnnoC f (SAnno e m) = [f e m]

conmap :: (a -> [b]) -> [a] -> [b]
conmap f = concat . map f
  
say :: MDoc -> MorlocMonad ()
say d = liftIO . putDoc $ " : " <> d <> "\n"

{- | Find exported expressions.

Terms may be declared or sourced in the current module or they may be imported
from a different module. If they are imported, ascend through modules to the
original declaration, returning the module where they are defined.

For each input term (EVar) a list is returned. Each element in the list
describes a specific implementation of the term. These implementations may have
different topologies and languages. A given language may have more than one
implementation. However, all implementations share the same general type.

Each element in the return list is a tuple of two values. The module where the
term is exported and the source/declaration information needed to uniquely
specify it (within an Either monad). If the term is sourced, then a (Left
Source) data constructor holds the required source information. If the term is
declared, a (EVar, Expr) tuple stores the left and right sides of a declaration
(the same information that is stored in the Declaration data constructor of
Expr).
-}
findTerm
  :: Map.Map MVar Module
  -> Module -- ^ a module where EVar is used
  -> EVar -- ^ the variable name in the top level module
  -> [TermOrigin]
findTerm ms m v
  | Set.member v (moduleExports m)
      = evarDeclared
      ++ evarSourced
      ++ evarImported
  | otherwise = []
  where
    evarDeclared :: [TermOrigin]
    evarDeclared = case Map.lookup v (moduleDeclarationMap m) of
      -- If a term is defined as being equal to another term, find this other term.
      (Just (Declaration v' (VarE v''))) -> if v' /= v''
        then findTerm ms m v''
        else error "found term of type `x = x`, the typechecker should have died on this ..."
      (Just (Declaration v' e)) -> [Declared m v' e]
      Nothing -> []

    evarSourced :: [TermOrigin]
    evarSourced = map (\(_, src) -> Sourced m src)
                . Map.toList
                . Map.filterWithKey (\(v',_) _ -> v' == v)
                $ moduleSourceMap m

    evarImported :: [TermOrigin]
    evarImported =
      concat [findTerm ms m' v | m' <- mapMaybe (flip Map.lookup $ ms) (listMVars m)]

    typeEVar :: EVar -> Expr
    typeEVar v' = case Map.lookup v' (moduleTypeMap m) of
      (Just (TypeSet t ts)) -> AnnE (VarE v') (map etype (maybe ts (\t' -> t':ts) t))

    listMVars :: Module -> [MVar]
    listMVars m = Map.elems $ Map.filterWithKey (\v' _ -> v' == v) (moduleImportMap m)
