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
import Morloc.Pretty (prettyType, prettyExpr)
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

-- | Store all necessary information about a particular implementation of a
-- term.  A term may either be declared or sourced. If declared, the left and
-- right hand sides of the declaration are stored. If sourced, the Source
-- object is stored. In either case, the module where the term is defined is
-- also stored.
data TermOrigin = Declared Module EVar Expr | Sourced Module Source
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
  ast
    -- find each term that is exported to the nexus
    <- roots modmap   -- [(EVar, [TermOrigin])]
    -- turn each term into an ambiguous call tree
    >>= mapM (collect modmap)   -- [SAnno GMeta Many [CMeta]]
    -- select a single instance at each node in the tree
    -- also resolve the manifold arguments that are in scope for each expression
    >>= mapM (realize smap)  -- [SAnno GMeta One (CMeta, IMeta)]

  -- build nexus
  -- -----------
  -- Each nexus subcommand calls one function from one one pool.
  -- The call passes the pool an index for the function (manifold) that will be called.
  nexus <- Nexus.generate [ (metaType c, poolId m x, metaName m)
                          | SAnno (One (x, (c, _))) m <- ast
                          ]

  -- for each language, collect all functions into one "pool"
  pools
    -- Separate the call trees into mono-lingual segments terminated in
    -- primitives or foreign calls.
    <- mapM segment ast |>> concat
    -- Gather segments into pools, currently tihs entails gathering all
    -- segments from a given language into one pool. Later it may be more
    -- nuanced.
    >>= pool
    -- Generate the code for each pool
    >>= mapM encode

  -- return the nexus script and each pool script
  return (nexus, pools)
  where
    -- map from nexus id to pool id
    -- these differ when a declared variable is exported
    poolId :: GMeta -> SExpr GMeta One (CMeta, IMeta) -> Int
    poolId _ (LamS _ (SAnno _ meta)) = metaId meta
    poolId meta _ = metaId meta

-- | Find the expressions that are exposed to the user.
-- Each element of the returned list consists of an EVar that is the term
-- exported from the main module. This term may be a named composition in the
-- main module, a sourced function/value from language-specific code, or an
-- imported term from another module. A term may be defined in multiple modules
-- or sourced from multiple implementations. Thus each term exported from main
-- is associated with a list of possible implementations/realizations.
roots :: Map.Map MVar Module -> MorlocMonad [(EVar, [TermOrigin])]
roots ms = do
  case roots of
    [m] ->
      let vs = Set.toList (moduleExports m) in
        return $ zip vs (map (findTerm False ms m) vs)
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
    . Map.mapWithKey (\v t -> conmap (g m) (f p v t))
    $ moduleTypeMap m

  f :: Property -> EVar -> TypeSet -> [(Type, EVar)]
  f p v (TypeSet (Just gentype) ts) =
    if Set.member p (eprop gentype)
      then [(etype t, v) | t <- ts]
      else [(etype t, v) | t <- ts, Set.member p (eprop t)]
  f p v (TypeSet Nothing ts) = [(etype t, v) | t <- ts, Set.member p (eprop t)]

  g :: Module -> (Type, EVar) -> [(Type, (Name, Path))]
  g m (t, v) = case Map.lookup (v, langOf' t) (moduleSourceMap m) of
    (Just (Source (EV name) _ (Just path) _)) -> [(t, (name, path))]
    _ -> []

  -- Get the type that is being serialized or deserialized
  getType :: Property -> Type -> Type
  getType Pack   (FunT t _) = t
  getType Unpack (FunT _ t) = t
  getType p (Forall v t) = Forall v (getType p t)
  getType _ t = t


-- | Build the call tree for a single nexus command. The result is ambiguous,
-- with 1 or more possible tree topologies, each with one or more possible for
-- each function.
collect
  :: Map.Map MVar Module
  -> (EVar, [TermOrigin])
  -> MorlocMonad (SAnno GMeta Many [CMeta])
collect ms (EV v, []) = MM.throwError . OtherError $
  "No origin found for variable '" <> v <> "'"
collect ms (evar', xs@(x:_)) = do
  -- say $ hsep ["for", pretty evar', "found", pretty (length xs), "definitions"]
  gmeta <- makeGMeta (Just evar') (getTermModule x) Nothing
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
      -- remove general types
      let ts' = filter (isJust . langOf) ts
      xs <- collectExpr Set.empty m ts' x
      case xs of
        [x] -> return x
        _ -> MM.throwError . OtherError $
          "Expected exactly one topology for a declared term"
    collectTerm (Declared _ _ _) = MM.throwError . OtherError $
      "Invalid expression in CollectTerm Declared, expected AnnE"
    collectTerm term@(Sourced m src) = do
      ts <- getTermTypes term
      -- remove general types
      let ts' = filter (isJust . langOf) ts
      cmetas <- mapM (makeCMeta (Just src) m) ts'
      return (VarS (srcAlias src), cmetas)
      where
        getTermTypes :: TermOrigin -> MorlocMonad [Type]
        getTermTypes t = do
          (TypeSet _ es) <- getTermTypeSet t
          return $ map etype es

    collectAnno
      :: Set.Set EVar
      -> Module
      -> Expr
      -> MorlocMonad (SAnno GMeta Many [CMeta])
    collectAnno args m (AnnE e ts) = do
      gtype <- getGeneralType ts
      gmeta <- makeGMeta (getExprName e) m gtype
      -- remove general types
      let ts' = filter (isJust . langOf) ts
      trees <- collectExpr args m ts' e
      return $ SAnno (Many trees) gmeta
    collectAnno _ _ _ = error "impossible bug - unannotated expression"

    getExprName :: Expr -> Maybe EVar
    getExprName (VarE v) = Just v
    getExprName _ = Nothing

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
      | Set.member v args = do
          cmetas <- mapM (makeCMeta Nothing m) ts
          return [(VarS v, cmetas)]
      | otherwise = do
          cmetas <- mapM (makeCMeta Nothing m) ts
          let terms = findTerm True ms m v
          xs <- mapM collectTerm terms
          let chosen = map (chooseTypes cmetas) xs
          return chosen
      where
        -- FIXME: The typesystem should handle this. It should unroll every
        -- type as far as it can be unrolled, and infer specialized types all
        -- the way down. Multiple declarations of every term within a given
        -- language should be allowed. The function below will only work in
        -- special cases where there is 1) a single instance of the term in
        -- each language and 2) types beneath the term (if this is a
        -- composition) do not depend on the type on top.
        chooseTypes
          :: [CMeta]
          -> (SExpr GMeta Many [CMeta], [CMeta])
          -> (SExpr GMeta Many [CMeta], [CMeta])
        chooseTypes cmetas (x, cmetas') =
          (x, [ cmeta { metaSource = metaSource cmeta' }
              | cmeta <- cmetas
              , cmeta' <- cmetas'
              , metaLang cmeta == metaLang cmeta'])
    collectExpr args m ts (ListE es) = do
      es' <- mapM (collectAnno args m) es
      simpleCollect (ListS es') m ts
    collectExpr args m ts (TupleE es) = do
      es' <- mapM (collectAnno args m) es
      simpleCollect (TupleS es') m ts
    collectExpr args m ts (RecE entries) = do
      es' <- mapM (collectAnno args m) (map snd entries)
      let entries' = zip (map fst entries) es'
      simpleCollect (RecS entries') m ts
    collectExpr args m ts e@(LamE v _) =
      case unrollLambda e of
        (args', e') -> do
          e'' <- collectAnno (Set.union args (Set.fromList args')) m e'
          simpleCollect (LamS args' e'') m ts
    -- AppS (SAnno g f c) [SAnno g f c]
    collectExpr args m ts (AppE e1 e2) = do
      -- The topology of e1' may vary. It could be a direct binary function. Or
      -- it could be a partially applied function. So it is necessary to map
      -- over the Many.
      e1'@(SAnno (Many fs) g) <- collectAnno args m e1
      e2' <- collectAnno args m e2
      let trees = map (\(f, c) -> (app f c g e2', c)) fs
      return trees
    collectExpr _ _ _ _ = MM.throwError . OtherError $ "Unexpected expression in collectExpr"

    app
      :: SExpr GMeta Many [CMeta]
      -> [CMeta]
      -> GMeta
      -> SAnno GMeta Many [CMeta]
      -> SExpr GMeta Many [CMeta]
    app (AppS f es) _ _ e2 = AppS f (es ++ [e2])
    app f cmetas g e2 = AppS (SAnno (Many [(f, cmetas)]) g) [e2]

    simpleCollect
      :: (SExpr GMeta Many [CMeta])
      -> Module
      -> [Type]
      -> MorlocMonad [(SExpr GMeta Many [CMeta], [CMeta])]
    simpleCollect sexpr m ts = do
      cmeta <- mapM (makeCMeta Nothing m) ts
      return [(sexpr, cmeta)]

    -- | Find info common across realizations of a given term in a given module
    makeGMeta :: Maybe EVar -> Module -> Maybe Type -> MorlocMonad GMeta
    makeGMeta evar m gtype = do
      i <- MM.getCounter
      let name = fmap (\(EV x) -> x) evar
      case evar >>= (flip Map.lookup) (moduleTypeMap m) of
        (Just (TypeSet (Just e) _)) -> do
          return $ GMeta
            { metaId = i
            , metaName = name
            , metaGeneralType = maybe (Just (etype e)) Just gtype
            , metaProperties = eprop e
            , metaConstraints = econs e
            }
        _ -> do
          return $ GMeta
            { metaId = i
            , metaName = name
            , metaGeneralType = gtype
            , metaProperties = Set.empty
            , metaConstraints = Set.empty
            }

    makeCMeta :: Maybe Source -> Module -> Type -> MorlocMonad CMeta
    makeCMeta src m t
      | langOf t == Nothing = MM.throwError . OtherError $
        "In collect, cannot make CMeta from general type: " <> render (prettyType t) 
      | otherwise = return $ CMeta
          { metaLang = langOf' t
          , metaType = t
          , metaSource = src
          , metaModule = moduleName m
          }

-- | Select a single concrete language for each sub-expression.  Store the
-- concrete type and the general type (if available).  Select serialization
-- functions (the serial map should not be needed after this step in the
-- workflow).
realize
  :: SerialMap
  -> SAnno GMeta Many [CMeta]
  -> MorlocMonad (SAnno GMeta One (CMeta, IMeta))
realize h x = realizeAnno True Nothing [] x where

  realizeAnno
    :: Bool -- is this a top-level expression
    -> Maybe Lang
    -> [Argument]
    -> SAnno GMeta Many [CMeta]
    -> MorlocMonad (SAnno GMeta One (CMeta, IMeta))
  realizeAnno isTop lang args (SAnno (Many xs@((x, ys@(cmeta:_)):_)) gmeta) = do
    -- say $ hsep
    --   [ "realizeAnno: for"
    --   , viaShow (metaName gmeta)
    --   , "found"
    --   , pretty (length xs)
    --   , "topologies and"
    --   , pretty (length ys)
    --   , "cmetas with types:"
    --   ] <> line
    --   <> indent 4 (tupled (map (prettyType . metaType) ys)) <> line
    x' <- realizeExpr lang args gmeta cmeta x
    args' <- handleArgsForSourced isTop cmeta x' args
    imeta <- makeIMeta isTop x' cmeta args'
    -- say $ pretty (metaId gmeta) <+> descSExpr x <> ":"
    --     <+> maybe "_ ::" (\n -> pretty n <+> "::") (metaName gmeta)
    --     <+> maybe "<untyped>" prettyType (metaGeneralType gmeta)
    return $ SAnno (One (x', (cmeta, imeta))) gmeta
  realizeAnno _ lang _ (SAnno (Many []) gmeta) = MM.throwError . OtherError $
    "No valid cases found for: " <> render (viaShow gmeta) <> " in " <> render (viaShow lang)
  realizeAnno _ lang _ (SAnno (Many ((x, []):_)) gmeta) = MM.throwError . OtherError $
    "No valid cmeta found for: " <> render (viaShow gmeta) <> " in " <> render (viaShow lang)

  -- prepare arguments for top-level sourced expressions
  handleArgsForSourced
    :: Bool
    -> CMeta
    -> SExpr GMeta One (CMeta, IMeta)
    -> [Argument]
    -> MorlocMonad [Argument]
  handleArgsForSourced True c (VarS x) _ = do
    let ftype = metaType c -- full function type
        otype = last (typeArgs ftype) -- output type
        itypes = init (typeArgs ftype) -- input types
        vars = variableStream (length itypes)
    zipWithM (makeArg True h) vars itypes
    where
      -- infinite stream: ['a', 'b', ..., 'z', 'aa', 'ab', ..., 'aaa', ...]
      variables = [1 ..] >>= flip replicateM ['a' .. 'z']
      -- get the nth variable
      variableStream :: Int -> [EVar]
      variableStream i = map (EV . MT.pack) $ take i variables
  handleArgsForSourced _ _ _ args = return args

  realizeExpr
    :: Maybe Lang -- ^ the parent language (not the language in CMeta)
    -> [Argument]
    -> GMeta -> CMeta
    -> SExpr GMeta Many [CMeta]
    -> MorlocMonad (SExpr GMeta One (CMeta, IMeta))
  realizeExpr _ _ _ _ (UniS) = return UniS
  realizeExpr _ _ _ _ (NumS x) = return $ NumS x
  realizeExpr _ _ _ _ (LogS x) = return $ LogS x
  realizeExpr _ _ _ _ (StrS x) = return $ StrS x
  realizeExpr _ _ _ _ (VarS x) = return $ VarS x
  realizeExpr lang args _ _ (ListS xs) = do
    xs' <- mapM (realizeAnno False lang args) xs
    return $ ListS xs'
  realizeExpr lang args _ _ (TupleS xs) = do
    xs' <- mapM (realizeAnno False lang args) xs
    return $ TupleS xs'
  realizeExpr lang args _ _ (RecS entries) = do
    vals <- mapM (realizeAnno False lang args) (map snd entries)
    return $ RecS (zip (map fst entries) vals)

  -- if the lambda is anonymous, the arguments from context are preserved
  realizeExpr lang args gmeta cmeta (LamS vs e) = do
    let isPacked = isNothing lang
        inputTypes = (init . typeArgs . metaType $ cmeta)
    args' <- zipWithM (makeArg isPacked h) vs inputTypes
      >>= updateArguments gmeta args
    e' <- realizeAnno False lang args' e
    return $ LamS vs e'

  realizeExpr lang args _ cmeta (AppS f xs) = do
    let lang' = Just $ metaLang cmeta
    f' <- realizeAnno False lang' args f
    xs' <- mapM (realizeAnno False lang' args) xs
    return (AppS f' xs')
  realizeExpr _ _ _ _ (ForeignS _ _) = error "This is unexpected"

  makeIMeta
    :: Bool
    -> (SExpr GMeta One (CMeta, IMeta))
    -> CMeta -> [Argument] -> MorlocMonad IMeta
  makeIMeta isTop sexpr cmeta args = do
    case (selectFunction t Pack h) of
      (Just (packer, packerpath)) ->
        return $ IMeta
          { metaArgs = args
          , metaPacker = Just packer
          , metaPackerPath = Just packerpath
          }
      _ ->
        return $ IMeta
          { metaArgs = args
          , metaPacker = Nothing
          , metaPackerPath = Nothing
          }
    where
      t = case (isTop, sexpr) of
        (_, AppS _ _) -> last $ typeArgs (metaType cmeta)
        (True, VarS _) -> case metaType cmeta of
          t'@(FunT _ _) -> last $ typeArgs t'
          t' -> t'
        _ -> metaType cmeta

  updateArguments :: GMeta -> [Argument] -> [Argument] -> MorlocMonad [Argument]
  updateArguments m oldargs newargs = case metaName m of
    -- the lambda is named, which means we are jumping into a new scope
    -- where the old arguments are not defined
    (Just _) -> return newargs
    -- we are in an anonymous lambda, where arguments from the outside
    -- scope are still defined
    Nothing -> do
      let oldargs' = [x | x <- oldargs, not (elem (argName x) (map argName newargs))]
      return $ newargs ++ oldargs'

makeArg :: Bool -> SerialMap -> EVar -> Type -> MorlocMonad Argument
makeArg packed h (EV n) t
  | langOf t == Nothing = MM.throwError . OtherError $
    "Cannot make an argument from the general type: " <> render (prettyType t)
  | otherwise = do
    case (selectFunction t Pack h, selectFunction t Unpack h) of
      (Just (packer, packerpath), Just (unpacker, unpackerpath)) ->
        return $ Argument
          { argName = n
          , argType = t
          , argPacker = packer
          , argPackerPath = packerpath
          , argUnpacker = unpacker
          , argUnpackerPath = unpackerpath
          , argIsPacked = packed
          }
      _ -> MM.throwError . OtherError $ "Expected a packer and unpacker for argument"


-- | choose a packer or unpacker for a given type
selectFunction :: Type -> Property -> SerialMap -> Maybe (Name, Path)
selectFunction t p h = case mostSpecificSubtypes t (Map.keys hmap) of
  [] -> Nothing
  (x:_) -> Map.lookup x hmap
  where
    hmap = if p == Pack then packers h else unpackers h


segment
  :: SAnno GMeta One (CMeta, IMeta)
  -> MorlocMonad [SAnno GMeta One (CMeta, IMeta)]
segment s = (\(r,rs) -> r:rs) <$> segment' Nothing Nothing s where
  segment'
    :: Maybe (GMeta, CMeta, IMeta) -- data from the parent
    -> Maybe Type -- child type in parent language
    -> SAnno GMeta One (CMeta, IMeta)
    -> MorlocMonad
         ( SAnno GMeta One (CMeta, IMeta)
         , [SAnno GMeta One (CMeta, IMeta)])
  segment' (Just (g0, c0, i0)) (Just t0) s@(SAnno (One (AppS f xs, (c1, i1))) g1)
    | langOf' t0 == metaLang c1 = do
        (x', rs') <- segment' (Just (g1, c1, i1)) Nothing f
        xs' <- zipWithM (\t x -> segment' (Just (g1, c1, i1)) (Just t) x) (typeArgs (sannoType f)) xs
        let (args', rss') = unzip xs'
        return (SAnno (One (AppS x' args', (c1, i1))) g1, rs' ++ concat rss')
    | otherwise = do
        -- generate the tree for the other language
        (x', rs') <- segment' (Just (g1, c1, i1)) (Just $ sannoType f) s
        let t1' = makeFun (map argType (metaArgs i0) ++ [t0])
        c1' <- case langOf t1' of
          Nothing -> MM.throwError . OtherError $
            "Cannot make CMeta from general type"
          (Just t) -> return $ CMeta
            { metaType = t1'
            , metaLang = langOf' t1'
            }
        v <- case metaName g1 of
          (Just v') -> return v'
          Nothing -> MM.throwError . OtherError $ "Expected a named function"
        return (SAnno (One (ForeignS (metaId g1) (metaLang c1), (c1', i1))) g1, x':rs')
  segment' (Just (g0, c0, i0)) _ (SAnno (One (LamS vs x, (c1, i1))) g1) = do
    let t1 = Just . last $ typeArgs (metaType c1)
    (x', xs') <- segment' (Just (g1, c1, i1)) t1 x
    return (SAnno (One (LamS vs x', (c1, i1))) g1, xs')
  segment' (Just (g0, c0, i0)) (Just (ArrT _ [tExp])) (SAnno (One (ListS xs, (c1, i1))) g1) = do
    xs' <- mapM (segment' (Just (g1, c1, i1)) (Just tExp)) xs
    let (elements, rss') = unzip xs'
    return (SAnno (One (ListS elements, (c1, i1))) g1, concat rss')
  segment' (Just (g0, c0, i0)) (Just (ArrT _ ts0)) (SAnno (One (TupleS xs, (c1, i1))) g1) = do
    xs' <- zipWithM (\t x -> segment' (Just (g1, c1, i1)) (Just t) x) ts0 xs
    let (elements, rss') = unzip xs'
    return (SAnno (One (TupleS elements, (c1, i1))) g1, concat rss')
  segment' (Just (g0, c0, i0)) (Just (RecT ts0)) (SAnno (One (RecS entries, (c1, i1))) g1) = do
    xs' <- zipWithM
             (\t x -> segment' (Just (g1, c1, i1)) (Just t) x)
             (map snd ts0)
             (map snd entries)
    let (es', rss') = unzip xs'
        entries' = zip (map fst entries) es'
    return (SAnno (One (RecS entries', (c1, i1))) g1, concat rss')
  segment' _ _ s = return (s, [])

makeFun :: [Type] -> Type
makeFun [] = error "No types given"
makeFun [t] = t
makeFun (t:ts) = FunT t (makeFun ts)


-- Sort manifolds into pools. Within pools, group manifolds into call sets.
pool
  :: [SAnno GMeta One (CMeta, IMeta)]
  -> MorlocMonad [(Lang, [SAnno GMeta One (CMeta, IMeta)])]
pool = return . groupSort . map (\s -> (langOf' (sannoType s), s))


encode :: (Lang, [SAnno GMeta One (CMeta, IMeta)]) -> MorlocMonad Script
encode (lang, xs) = do
  let srcs = filter (\src -> srcLang src == lang) (findSources xs)
  g <- selectGrammar lang
  state <- MM.get
  code <- mapM (codify g) xs >>= makePoolCode g srcs
  return $ Script
    { scriptBase = "pool"
    , scriptLang = lang
    , scriptCode = render code
    , scriptCompilerFlags =
        filter (/= "") . map packageGccFlags $ statePackageMeta state
    , scriptInclude =
        unique . map MS.takeDirectory $ [s | Source _ _ (Just s) _ <- srcs]
    }


makePoolCode :: Grammar -> [Source] -> [SAnno GMeta One (CMeta, IMeta, MDoc)] -> MorlocMonad MDoc
makePoolCode g srcs xs = do
  srcstrs <- encodeSources g srcs 
  let manifolds = conmap gatherManifolds xs
      signatures = conmap (encodeSignatures g) xs
  gMain g $ PoolMain
    { pmSources = srcstrs
    , pmSignatures = signatures
    , pmPoolManifolds = manifolds
    , pmDispatchManifold = makeDispatchBuilder g xs
    }


gatherManifolds :: SAnno g One (a, b, MDoc) -> [MDoc]
gatherManifolds (SAnno (One (VarS _, (_, i, d))) _) = [d]
gatherManifolds x = catMaybes . unpackSAnno f $ x
  where
    f :: SExpr g One (a, b, MDoc) -> g -> (a, b, MDoc) -> Maybe MDoc
    f (AppS _ _) _ (_, _, d) = Just d
    f (ForeignS _ _) _ (_, _, d) = Just d
    f _ _ _ = Nothing


encodeSources :: Grammar -> [Source] -> MorlocMonad [MDoc]
encodeSources g srcs =
  mapM encodeSource (unique . mapMaybe srcPath $ srcs)
  where
    encodeSource :: Path -> MorlocMonad MDoc
    encodeSource path = gImport g <$> pure "" <*> (gPrepImport g) path


findSources :: [SAnno g One (CMeta, IMeta)] -> [Source]
findSources = unique . concat . concat . map (unpackSAnno f)
  where
    f :: SExpr g One (CMeta, IMeta) -> g -> (CMeta, IMeta) -> [Source]
    f _ _ (c, i)
      =  maybeToList (metaSource c)
      ++ maybeToList (makeSrc <$> metaPacker i
                              <*> pure (langOf' . metaType $ c)
                              <*> metaPackerPath i)
      ++ conmap toSource (metaArgs i)

    toSource :: Argument -> [Source]
    toSource r =
      [ makeSrc (argPacker   r) (langOf' (argType r)) (argPackerPath   r)
      , makeSrc (argUnpacker r) (langOf' (argType r)) (argUnpackerPath r)
      ]

    makeSrc :: Name -> Lang -> Path -> Source
    makeSrc n l p = Source
      { srcName = EV n
      , srcLang = l
      , srcPath = Just p
      , srcAlias = EV n
      }

-- | Create a signature/prototype. Not all languages need this. C and C++ need
-- prototype definitions, but R and Python do not. Languages that do not
-- require signatures may simply write the type information as comments at the
-- beginning of the source file.
encodeSignatures :: Grammar -> SAnno GMeta One (CMeta, IMeta, MDoc) -> [MDoc]
encodeSignatures g x =
  (map (makeSignature g) . catMaybes . unpackSAnno f $ x) ++
  maybeToList (getSourced g x)
  where
    f :: SExpr g One (c, i, d)
      -> g
      -> (c, i, d)
      -> Maybe (g, c, i)
    f (AppS _ _) g (c, i, _) = Just (g, c, i)
    f (ForeignS _ _) g (c, i, _) = Just (g, c, i)
    f _ _ _ = Nothing

    -- make the signature for an exported function that is directly sourced
    -- such cases will always be at the top level, hence no recursion is needed
    getSourced :: Grammar -> SAnno GMeta One (CMeta, IMeta, MDoc) -> Maybe MDoc
    getSourced g (SAnno (One ((VarS v), (c, i, _))) m) = Just (makeSignature g (m, c, i))
    getSourced _ _ = Nothing

    makeSignature :: Grammar -> (GMeta, CMeta, IMeta) -> MDoc
    makeSignature g (m, c, i) = (gSignature g) $ GeneralFunction
      { gfComments = maybeToList $
          fmap (\t -> pretty (metaName m) <+> "::" <+> prettyType t) (metaGeneralType m)
      , gfReturnType = Just . gShowType g . last . typeArgs $ metaType c
      , gfName = makeManifoldName m
      , gfArgs = map (makeArg' g) (metaArgs i)
      , gfBody = ""
      }

    makeArg' :: Grammar -> Argument -> (Maybe MDoc, MDoc)
    makeArg' g arg =
      if argIsPacked arg
        then (Just ((gShowType g) (gSerialType g)), pretty (argName arg))
        else (Just ((gShowType g) (argType arg)), pretty (argName arg))


-- | Make a function for generating the code to dispatch from the pool main
-- function to manifold functions. The two arguments of the returned function
-- (MDoc->MDoc->MDoc) are 1) the manifold id and 2) the variable name where the
-- results are stored.
makeDispatchBuilder :: Grammar -> [SAnno GMeta One (CMeta, IMeta, MDoc)] -> (MDoc -> MDoc -> MDoc)
makeDispatchBuilder g xs =
  (gSwitch g)
    (\(m, _, _) -> pretty (metaId m))
    (mainCall g)
    (mapMaybe getPoolCall xs)
  where
    getPoolCall :: SAnno GMeta One (CMeta, IMeta, MDoc) -> Maybe (GMeta, CMeta, IMeta)
    getPoolCall (SAnno (One (AppS _ _, (c, i, _))) m) = Just (m, c, i)
    getPoolCall (SAnno (One (VarS _,   (c, i, _))) m)
      | (length (metaArgs i)) > 0 = Just (m, c, i)
      | otherwise = Nothing
    getPoolCall _ = Nothing
 
    mainCall :: Grammar -> (GMeta, CMeta, IMeta) -> MDoc
    mainCall g (m, c, i) = case metaPacker i of
      Nothing -> error $
        "Could not find packer for " <> show m
      (Just packer) ->
        (gCall g)
          (pretty packer)
          [(gCall g)
              (makeManifoldName m)
              (take (length (metaArgs i))
              (gCmdArgs g))]

codify
  :: Grammar
  -> SAnno GMeta One (CMeta, IMeta)
  -> MorlocMonad (SAnno GMeta One (CMeta, IMeta, MDoc))
codify = codify' True

-- | UniS
codify' _ g (SAnno (One (UniS, (c, i))) m) = return $ SAnno (One (UniS, (c, i, gNull g))) m

-- | ListS [SAnno a]
codify' _ g (SAnno (One (ListS xs, (c, i))) m) = do
  xs' <- mapM (codify' False g) xs
  let mdoc = (gList g) (map getdoc xs')
  return $ SAnno (One (ListS xs', (c, i, mdoc))) m

-- | TupleS [SAnno a]
codify' _ g (SAnno (One (TupleS xs, (c, i))) m) = do
  xs' <- mapM (codify' False g) xs
  let mdoc = (gTuple g) (map getdoc xs')
  return $ SAnno (One (TupleS xs', (c, i, mdoc))) m

-- | RecS [(EVar, SAnno a)]
codify' _ g (SAnno (One (RecS entries, (c, i))) m) = do
  xs <- mapM (codify' False g) (map snd entries)
  let mdoc = (gRecord g) (zip (map (pretty . fst) entries) (map getdoc xs))
      sexpr = RecS (zip (map fst entries) xs)
  return $ SAnno (One (sexpr, (c, i, mdoc))) m

-- | LamS [EVar] (SAnno a)
codify' _ g (SAnno (One (LamS vs x, (c, i))) m) = codify' False g x

-- | NumS Scientific
codify' _ _ (SAnno (One (NumS x, (c, i))) m) = return $ SAnno (One (NumS x, (c, i, viaShow x))) m

-- | LogS Bool
codify' _ g (SAnno (One (LogS x, (c, i))) m) = return $ SAnno (One (LogS x, (c, i, (gBool g) x))) m

-- | StrS MT.Text
codify' _ g (SAnno (One (StrS x, (c, i))) m) = return $ SAnno (One (StrS x, (c, i, (gQuote g) (pretty x)))) m

-- | ForeignS Int Lang
codify' _ g (SAnno (One (ForeignS mid lang, (c, i))) m) = do
  let t = metaType c
  config <- MM.ask
  mdoc <- case MC.getPoolCallBuilder config lang (gQuote g) of
    Nothing -> MM.throwError . OtherError $ "ah for fuck sake!!!"
    (Just poolBuilder) -> do
      let exe = pretty $ Lang.makeExecutableName lang "pool"
      return . gForeignCall g $ ForeignCallDoc
        { fcdForeignPool = pretty $ Lang.makeSourceName lang "pool"
        , fcdForeignExe = exe
        , fcdMid = pretty mid
        , fcdArgs = map cliArg (metaArgs i)
        , fcdCall = poolBuilder exe (pretty mid)
        , fcdFile = pretty $ Lang.makeSourceName (langOf' t) "pool"
        }
  return $ SAnno (One (ForeignS mid lang, (c, i, mdoc))) m
  where
    cliArg :: Argument -> MDoc
    cliArg r =
      if argIsPacked r
        then pretty (argName r)
        else (gCall g) (pretty $ argUnpacker r) [pretty $ argName r]

-- | VarS EVar
codify' False _ (SAnno (One (VarS v, (c, i))) m) = do
  let name = maybe v srcName (metaSource c)
  return $ SAnno (One (VarS v, (c, i, pretty name))) m
codify' True _ (SAnno (One (VarS v, (c, i))) m) = do
  let args = metaArgs i
      ftype = metaType c -- full function type
      otype = last (typeArgs ftype) -- output type
      itypes = init (typeArgs ftype) -- input types
  g <- selectGrammar (langOf' ftype)
  inputs' <- mapM (simplePrepInput g) (zip [0..] args)
  let srcName = maybe v (\(Source x _ _ _) -> x) (metaSource c)
  let mdoc = gFunction g $ GeneralFunction {
      gfComments = comments
    , gfReturnType = Just ((gShowType g) otype)
    , gfName = makeManifoldName m
    , gfArgs = map (prepArg g) args
    , gfBody =
        vsep (map snd inputs') <> line <>
        (gReturn g $ (gCall g) (pretty srcName) (map fst inputs'))
    }
  return $ SAnno (One (VarS v, (c, i, mdoc))) m
  where
    comments = catMaybes
      [ Just "From A"
      , Just (pretty v)
      , fmap (signature (metaName m) Nothing) (metaGeneralType m)
      , Just $ signature (metaName m) (Just (metaLang c)) (metaType c)
      ]

    simplePrepInput
      :: Grammar
      -> (Int, Argument)
      -> MorlocMonad ( MDoc -- variable name
                     , MDoc -- assignment code
                     )
    simplePrepInput g (i, r) = do
      let varname = makeArgumentName i
      return (varname, gAssign g $ GeneralAssignment
         { gaType = Just . gShowType g . argType $ r
         , gaName = varname
         , gaValue = (gCall g) (pretty (argUnpacker r)) [pretty (argName r)]
         , gaArg = Nothing
         })

-- | AppS (SAnno a) [SAnno a]
codify' _ g (SAnno (One (AppS e args, (c, i))) m) = do
  let t = metaType c
  e2 <- codify' False g e
  args' <- mapM (codify' False g) args
  mandoc <- makeManifoldDoc g args' e2 (c, i, m)
  return $ SAnno (One (AppS e2 args', (c, i, mandoc))) m


makeManifoldDoc
  :: Grammar
  -> [SAnno GMeta One (CMeta, IMeta, MDoc)] -- inputs
  -> SAnno GMeta One (CMeta, IMeta, MDoc) -- the function
  -> (CMeta, IMeta, GMeta) -- the AppS meta data
  -> MorlocMonad MDoc
makeManifoldDoc g inputs (SAnno (One (x, (c', i', _))) m') (c, i, m) = do
  let ftype = metaType c'
      otype = last (typeArgs ftype)
      margs = metaArgs i

  -- say $ pretty (metaId m) <+> pretty (metaId m') <+> descSExpr x
  --     -- <+> maybe "_ ::" (\n -> pretty n <+> "::") (metaName gmeta)
  --     -- <+> maybe "<untyped>" prettyType (metaGeneralType gmeta)

  innerCall <- case x of
    (AppS _ _) -> return $ makeManifoldName m'
    _ -> case (metaName m', metaSource c') of
      (_, Just (Source (EV x) _ _ _)) -> return (pretty x)
      (Just n, _) -> return (pretty n)
      _ -> MM.throwError . OtherError $ "No name found for manifold"

  inputs' <- zipWithM (prepInput g margs) [0..] inputs
  return . gFunction g $ GeneralFunction
    { gfComments = catMaybes $
        [ Just "From B"
        , Just (descSExpr x)
        , fmap (signature (metaName m') Nothing) (metaGeneralType m')
        , Just $ signature (metaName m') (Just (metaLang c)) (metaType c)
        ]
    , gfReturnType = Just ((gShowType g) otype)
    , gfName = makeManifoldName m
    , gfArgs = map (prepArg g) margs
    , gfBody =
        vsep (catMaybes (map snd inputs')) <> line <>
        (gReturn g $ (gCall g) innerCall (map fst inputs'))
    }

-- Handle preparation of arguments passed to a manifold. Return the name of the
-- variable that will be used and an block of code that defines the variable
-- (if necessary).
prepInput
  :: Grammar
  -> [Argument] -- all arguments of the manifold
  -> Int
  -> SAnno GMeta One (CMeta, IMeta, MDoc) -- an input to the wrapped function
  -> MorlocMonad (MDoc, Maybe MDoc)
prepInput g _ _ (SAnno (One (UniS, _)) _) = return (gNull g, Nothing) 
prepInput g _ _ (SAnno (One (NumS x, _)) _) = return ((gReal g) x, Nothing)
prepInput g _ _ (SAnno (One (LogS x, _)) _) = return ((gBool g) x, Nothing)
prepInput g _ _ (SAnno (One (StrS x, _)) _) = return ((gQuote g . pretty) x, Nothing)
prepInput g _ _ (SAnno (One (ListS _, (_, _, d))) _) = return (d, Nothing)
prepInput g _ _ (SAnno (One (TupleS _, (_, _, d))) _) = return (d, Nothing)
prepInput g _ _ (SAnno (One (RecS _, (_, _, d))) _) = return (d, Nothing)
prepInput g _ _ (SAnno (One (LamS _ _, _)) _) = MM.throwError . OtherError $
  "Function passing not implemented yet ..."
prepInput g _ _ (SAnno (One (ForeignS _ _, (_, _, d))) _) = return (d, Nothing)
prepInput g _ mid (SAnno (One (AppS x xs, (c, i, d))) m) = do
  let varname = makeArgumentName mid
      t = metaType c
      mname = makeManifoldName m
      ass = (gAssign g) $ GeneralAssignment
        { gaType = Just . gShowType g . last . typeArgs $ t
        , gaName = varname
        , gaValue = (gCall g) mname (map (pretty . argName) (metaArgs i))
        , gaArg = Nothing
        }
  return (varname, Just ass)
-- handle sourced files, which should be used as unaliased variables
prepInput _ _ _ (SAnno (One (VarS _, (CMeta _ _ (Just v) _, _, _))) _) = return (pretty (srcName v), Nothing)
prepInput g rs mid (SAnno (One (_, (c, i, d))) m) = do
  let name = metaName m
      varname = makeArgumentName mid
      t = metaType c
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
    x -> MM.throwError . OtherError $ "import prep error: " <> MT.show' x

-- | Serialize the result of a call if a serialization function is defined.
-- Presumably, if no serialization function is given, then the argument is
-- either a native construct or will be passed on in serialized form.
prepArg :: Grammar -> Argument -> (Maybe MDoc, MDoc)
prepArg g r = (Just . gShowType g $ t, pretty n) where
  t = if argIsPacked r
        then gSerialType g
        else argType r
  n = argName r



-------- Utility and lookup functions ----------------------------------------

say :: Doc ann -> MorlocMonad ()
say d = liftIO . putDoc $ " : " <> d <> "\n"

unrollLambda :: Expr -> ([EVar], Expr)
unrollLambda (LamE v e2) = case unrollLambda e2 of
  (vs, e) -> (v:vs, e)
unrollLambda (AnnE (LamE v e2) _) = case unrollLambda e2 of
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
typeArgs t@(Forall _ _) = error . MT.unpack . render $
  "qualified types should have been eliminated long ago in (" <> prettyType t <> ")"
typeArgs t = [t]

exprArgs :: SExpr g f c -> [Name]
exprArgs (LamS vs _) = [name | (EV name) <- vs]
exprArgs _ = []

makeManifoldName :: GMeta -> MDoc
makeManifoldName m = pretty $ "m" <> MT.show' (metaId m)

makeArgumentName :: Int -> MDoc
makeArgumentName i = "x" <> pretty i

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

unpackSAnno :: (SExpr g One c -> g -> c -> a) -> SAnno g One c -> [a]
unpackSAnno f (SAnno (One (e@(ListS xs), c)) g) = f e g c : conmap (unpackSAnno f) xs
unpackSAnno f (SAnno (One (e@(TupleS xs), c)) g) = f e g c : conmap (unpackSAnno f) xs
unpackSAnno f (SAnno (One (e@(RecS entries), c)) g) = f e g c : conmap (unpackSAnno f) (map snd entries)
unpackSAnno f (SAnno (One (e@(LamS _ x), c)) g) = f e g c : unpackSAnno f x
unpackSAnno f (SAnno (One (e@(AppS x xs), c)) g) = f e g c : conmap (unpackSAnno f) (x:xs)
unpackSAnno f (SAnno (One (e, c)) g) = [f e g c]

sannoWith :: (g -> c -> a) -> SAnno g One c -> a
sannoWith f (SAnno (One (_, c)) g) = f g c

sannoWithG :: (g -> a) -> SAnno g One c -> a
sannoWithG f (SAnno (One _) g) = f g

sannoWithC :: (c -> a) -> SAnno g One c -> a
sannoWithC f (SAnno (One (_, c)) _) = f c

sannoType :: SAnno g One (CMeta, a) -> Type
sannoType = sannoWithC (metaType . fst)

getdoc :: SAnno g One (a, b, MDoc) -> MDoc
getdoc = sannoWithC third

descSExpr :: SExpr g f c -> MDoc
descSExpr (UniS) = "UniS"
descSExpr (VarS v) = "VarS" <+> pretty v
descSExpr (ListS _) = "ListS"
descSExpr (TupleS _) = "TupleS"
descSExpr (LamS vs _) = "LamS" <+> hsep (map pretty vs)
descSExpr (AppS _ _) = "AppS"
descSExpr (NumS _) = "NumS"
descSExpr (LogS _) = "LogS"
descSExpr (StrS _) = "StrS"
descSExpr (RecS _) = "RecS"
descSExpr (ForeignS i lang) = "ForeignS" <+> pretty i <+> viaShow lang

signature :: Maybe Name -> Maybe Lang -> Type -> MDoc
signature n l t
  = maybe "_" pretty n <+> maybe "" (\l' -> " " <> viaShow l') l <+> "::" <+> prettyType t

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
  :: Bool -- ^ should non-exported terms be included?
  -> Map.Map MVar Module
  -> Module -- ^ a module where EVar is used
  -> EVar -- ^ the variable name in the top level module
  -> [TermOrigin]
findTerm includeInternal ms m v
  | includeInternal || Set.member v (moduleExports m)
      = evarDeclared
      ++ evarSourced
      ++ evarImported
  | otherwise = []
  where
    evarDeclared :: [TermOrigin]
    evarDeclared = case Map.lookup v (moduleDeclarationMap m) of
      -- If a term is defined as being equal to another term, find this other term.
      (Just (VarE v')) -> if v /= v'
        then findTerm False ms m v'
        else error "found term of type `x = x`, the typechecker should have died on this ..."
      (Just e) -> [Declared m v e]
      _ -> []

    evarSourced :: [TermOrigin]
    evarSourced = map (\(_, src) -> Sourced m src)
                . Map.toList
                . Map.filterWithKey (\(v',_) _ -> v' == v)
                $ moduleSourceMap m

    evarImported :: [TermOrigin]
    evarImported =
      concat [findTerm False ms m' v | m' <- mapMaybe (flip Map.lookup $ ms) (listMVars m)]

    typeEVar :: EVar -> Expr
    typeEVar v'@(EV name) = case Map.lookup v' (moduleTypeMap m) of
      (Just (TypeSet t ts)) -> AnnE (VarE v') (map etype (maybe ts (\t' -> t':ts) t))
      Nothing -> error $ "Variable '" <> MT.unpack name <> "' is not defined"

    listMVars :: Module -> [MVar]
    listMVars m = Map.elems $ Map.filterWithKey (\v' _ -> v' == v) (moduleImportMap m)
