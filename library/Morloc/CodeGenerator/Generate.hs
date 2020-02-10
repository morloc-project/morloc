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

  -- translate modules into bitrees
  ast
    -- find each term that is exported to the nexus
    <- roots modmap   -- [(EVar, [TermOrigin])]
    -- turn each term into an ambiguous call tree
    >>= mapM (collect modmap)   -- [SAnno GMeta Many [CMeta]]
    -- select a single instance at each node in the tree
    >>= mapM realize  -- [SAnno GMeta One CMeta]

  -- print abstract syntax trees to the console as debugging message
  say $ line <> indent 2 (vsep (map writeAST ast))

  -- build nexus
  -- -----------
  -- Each nexus subcommand calls one function from one one pool.
  -- The call passes the pool an index for the function (manifold) that will be called.
  nexus <- Nexus.generate
    [ (metaType c, poolId m x, metaName m)
    | SAnno (One (x, c)) m <- ast
    ]

  -- recursively find all serializers imported from any module
  smap <- findSerializers ms

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
    >>= mapM (encode smap)

  -- return the nexus script and each pool script
  return (nexus, pools)
  where
    -- map from nexus id to pool id
    -- these differ when a declared variable is exported
    poolId :: GMeta -> SExpr GMeta One CMeta -> Int
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
  xs <- case roots of
    [m] ->
      let vs = Set.toList (moduleExports m) in
        return $ zip vs (map (findTerm False ms m) vs)
    [] -> MM.throwError . OtherError $ "Circular dependencies between modules"
    _ -> MM.throwError . OtherError $ "Multiple root modules"
  return xs
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

  findSerialFun :: Property -> Module -> Map.Map ConcreteType (Name, Path)
  findSerialFun p m
    = Map.fromList
    . map (\(t, x) -> (getType p t, x))
    . mapSum
    . Map.mapWithKey (\v t -> conmap (g m) (f p v t))
    $ moduleTypeMap m

  -- extract serialization functions from a typeset if appropriate
  f :: Property -> EVar -> TypeSet -> [(ConcreteType, EVar)]
  f p v (TypeSet (Just gentype) ts) =
    if Set.member p (eprop gentype)
      then [(ConcreteType (etype t), v) | t <- ts]
      else [(ConcreteType (etype t), v) | t <- ts, Set.member p (eprop t)]
  f p v (TypeSet Nothing ts) =
    [ (ConcreteType (etype t), v)
    | t <- ts
    , Set.member p (eprop t)]

  -- find the source for the serialization functions
  g :: Module -> (ConcreteType, EVar) -> [(ConcreteType, (Name, Path))]
  g m (t, v) = case Map.lookup (v, langOf' t) (moduleSourceMap m) of
    (Just (Source name _ (Just path) _)) -> [(t, (name, path))]
    _ -> []

  -- get the type that is being serialized or deserialized
  getType :: Property -> ConcreteType -> ConcreteType
  getType p (ConcreteType t) = ConcreteType $ getType' p t where
    getType' :: Property -> Type -> Type
    getType' Pack   (FunT t' _) = t'
    getType' Unpack (FunT _ t') = t'
    getType' p (Forall v t') = Forall v (getType' p t')
    getType' _ t' = t'


-- | Build the call tree for a single nexus command. The result is ambiguous,
-- with 1 or more possible tree topologies, each with one or more possible for
-- each function.
collect
  :: Map.Map MVar Module
  -> (EVar, [TermOrigin])
  -> MorlocMonad (SAnno GMeta Many [CMeta])
collect ms (v, []) = MM.throwError . OtherError $
  "No origin found for variable '" <> unEVar v <> "'"
collect ms (evar', xs@(x:_)) = do
  -- Just look at one x, since any should emit the same GMeta (if not, then
  -- something is broken upstream of GMeta is not general enough)
  gmeta <- makeGMeta (Just evar') (getTermModule x) Nothing
  trees <- mapM collectTerm xs
  rewrite $ SAnno (Many trees) gmeta
  where

    -- Notice that `args` is NOT an input to collectTerm. Morloc uses lexical
    -- scoping, and the input to collectTerm is the origin of a term, so the
    -- definition of the term is outside the scope of the parent expression.
    collectTerm
      :: TermOrigin
      -> MorlocMonad (SExpr GMeta Many [CMeta], [CMeta])
    collectTerm (Declared m _ (AnnE x ts)) = do
      xs <- collectExpr Set.empty m (getConcreteTypes ts) x
      case xs of
        [x] -> return x
        _ -> MM.throwError . OtherError $
          "Expected exactly one topology for a declared term"
    collectTerm (Declared _ _ _) = MM.throwError . OtherError $
      "Invalid expression in CollectTerm Declared, expected AnnE"
    collectTerm term@(Sourced m src) = do
      ts <- getTermTypes term
      cmetas <- mapM (makeCMeta m) (getConcreteTypes ts)
      return (CallS src, cmetas)
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
      trees <- collectExpr args m (getConcreteTypes ts) e
      return $ SAnno (Many trees) gmeta
    collectAnno _ _ _ = error "impossible bug - unannotated expression"

    getExprName :: Expr -> Maybe EVar
    getExprName (VarE v) = Just v
    getExprName _ = Nothing

    collectExpr
      :: Set.Set EVar
      -> Module
      -> [ConcreteType]
      -> Expr
      -> MorlocMonad [(SExpr GMeta Many [CMeta], [CMeta])]
    collectExpr args m ts (UniE) = simpleCollect UniS m ts
    collectExpr args m ts (NumE x) = simpleCollect (NumS x) m ts
    collectExpr args m ts (LogE x) = simpleCollect (LogS x) m ts
    collectExpr args m ts (StrE x) = simpleCollect (StrS x) m ts
    collectExpr args m ts (VarE v)
      | Set.member v args = do
          cmetas <- mapM (makeCMeta m) ts
          return [(VarS v, cmetas)]
      | otherwise = do
          cmetas <- mapM (makeCMeta m) ts
          let terms = findTerm True ms m v
          xs <- mapM collectTerm terms
          let chosen = map (chooseTypes cmetas) xs
          return chosen
      where
        -- FIXME: The typesystem should handle this. It should unroll every
        -- type as far as it can be unrolled, and infer specialized types all
        -- the way down. Multiple declarations of every term within a given
        -- language should be allowed. The function below will only work in
        -- special cases where there is A) a single instance of the term in
        -- each language and B) types beneath the term (if this is a
        -- composition) do not depend on the type on top.
        chooseTypes
          :: [CMeta]
          -> (SExpr GMeta Many [CMeta], [CMeta])
          -> (SExpr GMeta Many [CMeta], [CMeta])
        chooseTypes cmetas (x, cmetas') =
          (x, [ cmeta
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
      e1'@(SAnno (Many fs) g1) <- collectAnno args m e1
      e2' <- collectAnno args m e2
      mapM (app g1 e2') fs

    collectExpr _ _ _ _ = MM.throwError . OtherError $ "Unexpected expression in collectExpr"
    app
      :: GMeta
      -> SAnno GMeta Many [CMeta]
      -> (SExpr GMeta Many [CMeta], [CMeta])
      -> MorlocMonad (SExpr GMeta Many [CMeta], [CMeta])
    app _ e2 ((AppS f es), cmetas) = do
      cmetas' <- mapM partialApplyCMeta cmetas
      return (AppS f (es ++ [e2]), cmetas')
    app g e2 (f, cmetas) = do
      cmetas' <- mapM partialApplyCMeta cmetas
      return (AppS (SAnno (Many [(f, cmetas)]) g) [e2], cmetas')

    partialApplyCMeta :: CMeta -> MorlocMonad CMeta
    partialApplyCMeta c = do
      t <- partialApply (unConcreteType $ metaType c)
      return $ c {metaType = ConcreteType t}

    simpleCollect
      :: (SExpr GMeta Many [CMeta])
      -> Module
      -> [ConcreteType]
      -> MorlocMonad [(SExpr GMeta Many [CMeta], [CMeta])]
    simpleCollect sexpr m ts = do
      cmeta <- mapM (makeCMeta m) ts
      return [(sexpr, cmeta)]

    -- | Find info common across realizations of a given term in a given module
    makeGMeta :: Maybe EVar -> Module -> Maybe GeneralType -> MorlocMonad GMeta
    makeGMeta name m gtype = do
      i <- MM.getCounter
      case name >>= (flip Map.lookup) (moduleTypeMap m) of
        (Just (TypeSet (Just e) _)) -> do
          return $ GMeta
            { metaId = i
            , metaName = name
            , metaGeneralType = maybe (Just . GeneralType $ etype e) Just gtype
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

    makeCMeta :: Module -> ConcreteType -> MorlocMonad CMeta
    makeCMeta m t =
      return $ CMeta
        { metaLang = langOf' t
        , metaType = t
        , metaModule = moduleName m
        }

rewrite
  :: SAnno GMeta Many [CMeta]
  -> MorlocMonad (SAnno GMeta Many [CMeta])
rewrite (SAnno (Many es0) g0) = do
  es0' <- fmap concat $ mapM rewriteL0 es0 
  return $ SAnno (Many es0') g0
  where
    rewriteL0
      :: (SExpr GMeta Many [CMeta], [CMeta])
      -> MorlocMonad [(SExpr GMeta Many [CMeta], [CMeta])]
    rewriteL0 (AppS (SAnno (Many es1) g1) args, c1) = do
      args' <- mapM rewrite args
      -- originally es1 consists of a list of CallS and LamS constructos
      --  - CallS are irreducible source functions
      --  - LamS are Morloc abstractions that can be reduced
      -- separate LamS expressions from all others
      let (es1LamS, es1CallS) = partitionEithers (map sepLamS es1)
      -- rewrite the LamS expressions, each expression will yields 1 or more
      es1LamS' <- fmap concat $ mapM (rewriteL1 args') es1LamS
      return $ (AppS (SAnno (Many es1CallS) g1) args', c1) : es1LamS'
      where
        sepLamS
          :: (SExpr g Many c, c)
          -> Either ([EVar], SAnno g Many c)
                    (SExpr g Many c, c)
        sepLamS (x@(LamS vs body), _) = Left (vs, body)
        sepLamS x = Right x
    rewriteL0 (ListS xs, c) = do
      xs' <- mapM rewrite xs
      return [(ListS xs', c)]
    rewriteL0 (TupleS xs, c) = do
      xs' <- mapM rewrite xs
      return [(TupleS xs', c)]
    rewriteL0 (RecS entries, c) = do
      xs' <- mapM rewrite (map snd entries)
      return [(RecS $ zip (map fst entries) xs', c)]
    rewriteL0 (LamS vs x, c) = do
      x' <- rewrite x
      return [(LamS vs x', c)]
    -- VarS UniS NumS LogS StrS CallS ForeignS
    rewriteL0 x = return [x]

    rewriteL1
      :: [SAnno g Many c]
      -> ([EVar], SAnno g Many c) -- lambda variables and body
      -> MorlocMonad [(SExpr g Many c, c)]
    rewriteL1 args (vs, SAnno (Many es2) g2)
      | length vs == length args =
          fmap concat $ mapM (substituteExprs (zip vs args)) es2
      | length vs > length args = error
          "Partial function application not yet implemented (coming soon)"
      | length vs < length args = MM.throwError . OtherError $
          "Type error: too many arguments applied to lambda"

    substituteExprs
      :: [(EVar, SAnno g Many c)]
      -> (SExpr g Many c, c) -- body
      -> MorlocMonad [(SExpr g Many c, c)] -- substituted bodies
    substituteExprs [] x = return [x]
    substituteExprs ((v, r):rs) x = do
      xs' <- substituteExpr v r x
      fmap concat $ mapM (substituteExprs rs) xs'

    substituteExpr
      :: EVar
      -> SAnno g Many c -- replacement
      -> (SExpr g Many c, c) -- expression
      -> MorlocMonad [(SExpr g Many c, c)]
    substituteExpr v (SAnno (Many xs) _) x@(VarS v', _)
      | v == v' = return xs
      | otherwise = return [x]
    substituteExpr v r (ListS xs, c) = do
      xs' <- mapM (substituteAnno v r) xs
      return [(ListS xs, c)]
    substituteExpr v r (TupleS xs, c) = do
      xs' <- mapM (substituteAnno v r) xs
      return [(TupleS xs, c)]
    substituteExpr v r (RecS entries, c) = do
      xs' <- mapM (substituteAnno v r) (map snd entries)
      return [(RecS (zip (map fst entries) xs'), c)]
    substituteExpr v r (LamS vs x, c) = do
      x' <- substituteAnno v r x
      return [(LamS vs x', c)]
    substituteExpr v r (AppS f xs, c) = do
      f' <- substituteAnno v r f
      xs' <- mapM (substituteAnno v r) xs
      return [(AppS f' xs', c)]
    -- VarS UniS NumS LogS StrS CallS ForeignS
    substituteExpr _ _ x = return [x]

    substituteAnno
      :: EVar -- variable to replace
      -> SAnno g Many c -- replacement branch set
      -> SAnno g Many c -- search branch
      -> MorlocMonad (SAnno g Many c)
    substituteAnno v r (SAnno (Many xs) g) = do
      xs' <- fmap concat $ mapM (substituteExpr v r) xs
      return $ SAnno (Many xs') g



-- | Select a single concrete language for each sub-expression.  Store the
-- concrete type and the general type (if available).  Select serialization
-- functions (the serial map should not be needed after this step in the
-- workflow).
realize
  :: SAnno GMeta Many [CMeta]
  -> MorlocMonad (SAnno GMeta One CMeta)
realize x = do
  realizationMay <- realizeAnno 0 Nothing x
  case realizationMay of
    (Just (_, realization)) -> do
      return realization 
    Nothing -> MM.throwError . OtherError $ "No valid realization found"
  where
    realizeAnno
      :: Int
      -> Maybe Lang
      -> SAnno GMeta Many [CMeta]
      -> MorlocMonad (Maybe (Int, SAnno GMeta One CMeta))
    realizeAnno depth langMay (SAnno (Many xs) m) = do
      asts <- mapM (\(x, cs) -> mapM (realizeExpr (depth+1) langMay x) cs) xs |>> concat
      case maximumOnMay (\(s,_,_) -> s) (catMaybes asts) of
        Just (i, x, c) -> do
          return $ Just (i, SAnno (One (x, c)) m)
        Nothing -> do
          return Nothing

    indent' :: Int -> MDoc
    indent' i = pretty (take i (repeat '-')) <> " "

    realizeExpr
      :: Int
      -> Maybe Lang
      -> SExpr GMeta Many [CMeta]
      -> CMeta
      -> MorlocMonad (Maybe (Int, SExpr GMeta One CMeta, CMeta))
    realizeExpr depth lang x c = realizeExpr' depth (maybe (metaLang c) id lang) x c

    realizeExpr'
      :: Int
      -> Lang
      -> SExpr GMeta Many [CMeta]
      -> CMeta
      -> MorlocMonad (Maybe (Int, SExpr GMeta One CMeta, CMeta))
    realizeExpr' _ lang (UniS) c
      | lang == metaLang c = return $ Just (0, UniS, c)
      | otherwise = return Nothing
    realizeExpr' _ lang (NumS x) c
      | lang == metaLang c = return $ Just (0, NumS x, c)
      | otherwise = return Nothing
    realizeExpr' _ lang (LogS x) c
      | lang == metaLang c = return $ Just (0, LogS x, c)
      | otherwise = return Nothing
    realizeExpr' _ lang (StrS x) c
      | lang == metaLang c = return $ Just (0, StrS x, c)
      | otherwise = return Nothing
    realizeExpr' _ lang (CallS src) c
      | lang == metaLang c = return $ Just (0, CallS src, c)
      | otherwise = return Nothing
    realizeExpr' _ lang (VarS x) c
      | lang == metaLang c = return $ Just (0, VarS x, c)
      | otherwise = return Nothing
    realizeExpr' depth lang (ListS xs) c
      | lang == metaLang c = do
        xsMay <- mapM (realizeAnno depth (Just lang)) xs
        case (fmap unzip . sequence) xsMay of
          (Just (scores, xs')) -> return $ Just (sum scores, ListS xs', c)
          Nothing -> return Nothing
      | otherwise = return Nothing
    realizeExpr' depth lang (TupleS xs) c
      | lang == metaLang c = do
        xsMay <- mapM (realizeAnno depth (Just lang)) xs
        case (fmap unzip . sequence) xsMay of
          (Just (scores, xs')) -> return $ Just (sum scores, TupleS xs', c)
          Nothing -> return Nothing
      | otherwise = return Nothing
    realizeExpr' depth lang (RecS entries) c
      | lang == metaLang c = do
          xsMay <- mapM (realizeAnno depth (Just lang)) (map snd entries)
          case (fmap unzip . sequence) xsMay of
            (Just (scores, vals)) -> return $ Just (sum scores, RecS (zip (map fst entries) vals), c)
            Nothing -> return Nothing
      | otherwise = return Nothing
    realizeExpr' depth _ (LamS vs x) c = do
      xMay <- realizeAnno depth (Just $ metaLang c) x
      case xMay of
        (Just (score, x')) -> return $ Just (score, LamS vs x', c)
        Nothing -> return Nothing
    realizeExpr' depth lang (AppS f xs) c = do
      fMay <- realizeAnno depth (Just $ metaLang c) f
      xsMay <- mapM (realizeAnno depth (Just $ metaLang c)) xs
      case (fMay, (fmap unzip . sequence) xsMay, Lang.pairwiseCost lang (metaLang c)) of
        (Just (fscore, f'), Just (scores, xs'), Just interopCost) ->
          return $ Just (fscore + sum scores + interopCost, AppS f' xs', c) 
        _ -> return Nothing
    realizeExpr' _ _ (ForeignS _ _) _ = MM.throwError . OtherError $
      "ForeignS should not yet appear in an SExpr"

writeAST
  :: SAnno GMeta One CMeta -> MDoc
writeAST s = hang 2 . vsep $ ["AST:", describe s] where
  describe
    :: SAnno GMeta One CMeta
    -> MDoc
  describe (SAnno (One (x@(ListS xs), _)) _) = descSExpr x
  describe (SAnno (One (x@(TupleS xs), _)) _) = descSExpr x
  describe (SAnno (One (x@(RecS xs), _)) _) = descSExpr x
  describe (SAnno (One (x@(AppS f xs), c)) g) =
    hang 2 . vsep $
      [ descSExpr x
      , describe f
      ] ++ map describe xs
  describe (SAnno (One (f@(LamS _ x), c)) g) = do 
    hang 2 . vsep $
      [ name c g <+> descSExpr f
      , describe x
      ] 
  describe (SAnno (One (x, _)) _) = descSExpr x

  name :: CMeta -> GMeta -> MDoc
  name (viaShow . metaLang -> lang) g =
    maybe
      ("_" <+> lang <+> "::")
      (\x -> pretty x <+> lang <+> "::")
      (metaName g)


-- | In the returned list, every top-level element is a LamS that is called
-- from either the nexus or another pool.
segment
  :: SAnno GMeta One CMeta
  -> MorlocMonad [SAnno GMeta One CMeta]
segment x@(SAnno (One (_, c)) _) = do
  (x', xs) <- segment' c x
  return (x' : xs)
  where
    segment' 
      :: CMeta
      -> SAnno GMeta One CMeta
      -> MorlocMonad (SAnno GMeta One CMeta, [SAnno GMeta One CMeta])
    segment' _ x@(SAnno (One (UniS, _)) _) = return (x, [])
    segment' _ x@(SAnno (One (NumS _, _)) _) = return (x, [])
    segment' _ x@(SAnno (One (LogS _, _)) _) = return (x, [])
    segment' _ x@(SAnno (One (StrS _, _)) _) = return (x, [])
    segment' _ x@(SAnno (One (VarS _, _)) _) = return (x, [])
    segment' _ x@(SAnno (One (CallS _, _)) _) = return (x, [])
    segment' c0 (SAnno (One (ListS xs, c1)) m) = do
      (xs', rs) <- mapM (segment' c0) xs |>> unzip
      return (SAnno (One (ListS xs', c1)) m, concat rs)
    segment' c0 (SAnno (One (TupleS xs, c1)) m) = do
      (xs', rs) <- mapM (segment' c0) xs |>> unzip
      return (SAnno (One (TupleS xs', c1)) m, concat rs)
    segment' c0 (SAnno (One (RecS xs, c1)) m) = do
      (vals, rs) <- mapM (segment' c0) (map snd xs) |>> unzip
      return (SAnno (One (RecS (zip (map fst xs) vals), c1)) m, concat rs)
    segment' c0 (SAnno (One (LamS vs x, c1)) m)
      | metaLang c0 == metaLang c1 = do
          (x', rs) <- segment' c1 x
          return (SAnno (One (LamS vs x', c1)) m, rs)
      | otherwise = do
          (x', rs) <- segment' c1 x
          return ( SAnno (One (ForeignS (metaId m) (metaLang c1), c1)) m
                 , SAnno (One (LamS vs x', c1)) m : rs
                 )
    segment' c0 (SAnno (One (AppS x xs, c1)) m) = do
      (x', xrs) <- segment' c0 x
      (xs', xsrss) <- mapM (segment' c0) xs |>> unzip
      return (SAnno (One (AppS x' xs', c1)) m, xrs ++ concat xsrss)


-- Sort manifolds into pools. Within pools, group manifolds into call sets.
pool
  :: [SAnno GMeta One CMeta]
  -> MorlocMonad [(Lang, [SAnno GMeta One CMeta])]
pool = return . groupSort . map (\s -> (langOf' (sannoType s), s))


encode :: SerialMap -> (Lang, [SAnno GMeta One CMeta]) -> MorlocMonad Script
encode h (lang, xs) = do
  state <- MM.get

  -- get the grammar rules for lang
  g <- selectGrammar lang

  -- find sources in lang that need to be included/imported
  let srcs = findSources h xs
  srcdocs <- mapM (encodeSource g) srcs

  -- determine where serialization occurs, add argument list to tree
  serializedTree <- mapM (serialize h) xs

  -- translate each node in the AST to code
  codeTree <- mapM (codify h g) serializedTree
  let manifolds = conmap gatherManifolds codeTree
      signatures = conmap (encodeSignatures g) serializedTree

  -- generate final pool code
  code <- gMain g $ PoolMain
    { pmSources = srcdocs
    , pmSignatures = signatures
    , pmPoolManifolds = manifolds
    , pmDispatchManifold = makeDispatchBuilder h g serializedTree
    }

  return $ Script
    { scriptBase = "pool"
    , scriptLang = lang
    , scriptCode = Code . render $ code
    , scriptCompilerFlags =
        filter (/= "") . map packageGccFlags $ statePackageMeta state
    , scriptInclude = unique $ map MS.takeDirectory srcs
    }

-- | Add arguments that are required for each term. Unneeded arguments are
-- removed at each step.
serialize
  :: SerialMap
  -> SAnno GMeta One CMeta
  -> MorlocMonad (SAnno GMeta One (CMeta, [Argument]))
serialize h (SAnno (One (LamS vs x, c)) m) = do
  args0 <- zipWithM (makeArgument h) vs (typeArgs (metaType c))
  x' <- serialize' h args0 x
  return $ SAnno (One (LamS vs x', (c, args0))) m
serialize h (SAnno (One (CallS src, c)) m) = do
  let ts = init $ typeArgs (metaType c)
      vs = map EVar ([1 ..] >>= flip replicateM ['a' .. 'z'] |>> MT.pack)
  args0 <- zipWithM (makeArgument h) vs ts
  return $ SAnno (One (CallS src, (c, args0))) m
serialize h x = serialize' h [] x

-- TODO: the arguments coupled to every term should be the arguments USED
-- (not inherited) by the term. I need to ensure the argument threading
-- leads to correct passing of packed/unpacked arguments. AppS should
-- "know" that it needs to pack functions that are passed to a foreign
-- call, for instance.
serialize'
  :: SerialMap
  -> [Argument] -- arguments in parental scope (child needn't retain them)
  -> SAnno GMeta One CMeta
  -> MorlocMonad (SAnno GMeta One (CMeta, [Argument]))
-- primitives, no arguments are required for a primitive, so empty lists
serialize' _ _ (SAnno (One (UniS, c)) m) = return $ SAnno (One (UniS, (c, []))) m
serialize' _ _ (SAnno (One (NumS x, c)) m) = return $ SAnno (One (NumS x, (c, []))) m
serialize' _ _ (SAnno (One (LogS x, c)) m) = return $ SAnno (One (LogS x, (c, []))) m
serialize' _ _ (SAnno (One (StrS x, c)) m) = return $ SAnno (One (StrS x, (c, []))) m
-- VarS EVar
serialize' _ args (SAnno (One (VarS v, c)) m) = do
  let args' = filter (\r -> argName r == v) args
  return $ SAnno (One (VarS v, (c, args'))) m
-- CallS Source
serialize' _ args (SAnno (One (CallS src, c)) m) = do
  return $ SAnno (One (CallS src, (c, []))) m
-- containers
serialize' h args (SAnno (One (ListS xs, c)) m) = do
  xs' <- mapM (serialize' h args) xs
  let args' = unique . concat . map sannoSnd $ xs'
  return $ SAnno (One (ListS xs', (c, args'))) m
serialize' h args (SAnno (One (TupleS xs, c)) m) = do
  xs' <- mapM (serialize' h args) xs
  let args' = unique . concat . map sannoSnd $ xs'
  return $ SAnno (One (TupleS xs', (c, args'))) m
serialize' h args (SAnno (One (RecS entries, c)) m) = do
  vs' <- mapM (serialize' h args) (map snd entries)
  let args' = unique . concat . map sannoSnd $ vs'
  return $ SAnno (One (RecS (zip (map fst entries) vs'), (c, args'))) m
serialize' h _ (SAnno (One (LamS vs x, c)) m) = do
  args' <- zipWithM (makeArgument h) vs (typeArgs (metaType c)) 
  x' <- serialize' h args' x
  return $ SAnno (One (LamS vs x', (c, []))) m 
serialize' h args (SAnno (One (AppS x xs, c)) m) = do
  x' <- serialize' h args x
  xs' <- mapM (serialize' h args) xs
  let args' = sannoSnd x' ++ (unique . concat . map sannoSnd) xs'
  return $ SAnno (One (AppS x' xs', (c, args'))) m
serialize' _ args (SAnno (One (ForeignS i lang, c)) m) = do
  let args' = map pack args
  return $ SAnno (One (ForeignS i lang, (c, args'))) m


makeArgument h v t = case selectFunction t Unpack h of
  Nothing -> MM.throwError . OtherError . render $
    "No packer found for type" <+> parens (prettyType t)
  (Just (name, _)) -> return $ PackedArgument v t (Just name)

gatherManifolds :: SAnno g One MDoc -> [MDoc]
gatherManifolds (SAnno (One (CallS _, d)) _) = [d]
gatherManifolds x = catMaybes . unpackSAnno f $ x
  where
    f :: SExpr g One MDoc -> g -> MDoc -> Maybe MDoc
    f (AppS _ _) _ d = Just d
    f (ForeignS _ _) _ d = Just d
    f _ _ _ = Nothing


encodeSource :: Grammar -> Path -> MorlocMonad MDoc
encodeSource g path = gImport g <$> pure "" <*> (gPrepImport g) path


findSources :: SerialMap -> [SAnno g One CMeta] -> [Path]
findSources h xs = unique . concat . concat . map (unpackSAnno f) $ xs
  where
    f :: SExpr g One CMeta -> g -> CMeta -> [Path]
    f (CallS src) _ c = catMaybes
      [ srcPath src
      , fmap snd $ selectFunction (metaType c) Pack h
      , fmap snd $ selectFunction (metaType c) Unpack h
      ]
    f _ _ c = catMaybes
      [ fmap snd $ selectFunction (metaType c) Pack h
      , fmap snd $ selectFunction (metaType c) Unpack h
      ]


-- | Create a signature/prototype. Not all languages need this. C and C++ need
-- prototype definitions, but R and Python do not. Languages that do not
-- require signatures may simply write the type information as comments at the
-- beginning of the source file.
encodeSignatures :: Grammar -> SAnno GMeta One (CMeta, [Argument]) -> [MDoc]
encodeSignatures g (SAnno (One (CallS _, (c, args))) m) = [makeSignature g (Just m, m, c, args)]
encodeSignatures g (SAnno (One (LamS _ x, _)) _) =
  map (makeSignature g) (catMaybes $ unpackSAnno f x) ++ maybeToList (getSourced g x)
  where
    f :: SExpr GMeta One (CMeta, [Argument])
      -> GMeta
      -> (CMeta, [Argument])
      -> Maybe (Maybe GMeta, GMeta, CMeta, [Argument])
    f (AppS (SAnno _ gFun) _) m (c, args) = Just (Just gFun, m, c, args)
    f (ForeignS _ _) m (c, args) = Just (Nothing, m, c, args)
    f _ _ _ = Nothing

    -- make the signature for an exported function that is directly sourced
    -- such cases will always be at the top level, hence no recursion is needed
    getSourced :: Grammar -> SAnno GMeta One (CMeta, [Argument]) -> Maybe MDoc
    getSourced g (SAnno (One ((VarS v), (c, args))) m) = Just (makeSignature g (Just m, m, c, args))
    getSourced _ _ = Nothing
encodeSignatures _ _ = error "Expected LamS or VarS at all top-level segments"

makeSignature :: Grammar -> (Maybe GMeta, GMeta, CMeta, [Argument]) -> MDoc
makeSignature g (gFun, m, c, args) = (gSignature g) $ GeneralFunction
  { gfComments = maybeToList $
      fmap (\(GeneralType t) -> maybe "_" pretty (gFun >>= metaName) <+> "::" <+> prettyType t)
           (gFun >>= metaGeneralType)
  , gfReturnType = Just . gShowType g . last . typeArgs $ metaType c
  , gfName = makeManifoldName m
  , gfArgs = map (makeArg' g) args
  , gfBody = ""
  }

makeArg' :: Grammar -> Argument -> (Maybe MDoc, MDoc)
makeArg' g (PackedArgument v t _) = (Just ((gShowType g) (gSerialType g)), pretty v)
makeArg' g (UnpackedArgument v t _) = (Just ((gShowType g) t), pretty v)


varExists :: SAnno GMeta One CMeta -> EVar -> Bool
varExists x v = varExists' x where
  varExists' (SAnno (One (VarS v', _)) _) = v == v'
  varExists' (SAnno (One (ListS xs, _)) _) = any varExists' xs
  varExists' (SAnno (One (TupleS xs, _)) _) = any varExists' xs
  varExists' (SAnno (One (RecS xs, _)) _) = any (varExists' . snd) xs
  varExists' (SAnno (One (AppS x xs, _)) _) = varExists' x || any varExists' xs
  varExists' (SAnno (One (LamS vs x, _)) _)
    | elem v vs = False
    | otherwise = varExists' x
  varExists' _ = False

-- | Make a function for generating the code to dispatch from the pool main
-- function to manifold functions. The two arguments of the returned function
-- (MDoc->MDoc->MDoc) are 1) the manifold id and 2) the variable name where the
-- results are stored.
makeDispatchBuilder
  :: SerialMap
  -> Grammar
  -> [SAnno GMeta One (CMeta, [Argument])]
  -> (MDoc -> MDoc -> MDoc)
makeDispatchBuilder h g xs =
  (gSwitch g)
    (\(_, m, _) -> pretty (metaId m))
    (mainCall g)
    (mapMaybe getPoolCall xs)
  where
    getPoolCall
      :: SAnno GMeta One (CMeta, [Argument])
      -> Maybe ([EVar], GMeta, ConcreteType)
    getPoolCall (SAnno (One (LamS vs (SAnno (One (AppS _ _, (c, _))) m), _)) _)
      = Just (vs, m, metaType c)
    getPoolCall (SAnno (One (LamS vs (SAnno (One (CallS _  , (c, _))) m), _)) _)
      | (length vs) > 0 = Just (vs, m, metaType c)
      | otherwise = Nothing
    getPoolCall (SAnno (One (CallS _, (c, args))) m)
      | (length args) > 0 = Just (map argName args, m, last . typeArgs $ metaType c)
      | otherwise = Nothing
    getPoolCall _ = Nothing

    -- Note, the CMeta is for the type of the full application, that is, the return type.
    -- It is NOT for the function that is called.
    mainCall :: Grammar -> ([EVar], GMeta, ConcreteType) -> MDoc
    mainCall g (vs, m, t) = case selectFunction t Pack h of
      Nothing -> error $
        "Could not find packer for " <> show m
      (Just (packer, _)) ->
        (gCall g)
          (pretty packer)
          [(gCall g)
              (makeManifoldName m)
              (take (length vs)
              (gCmdArgs g))]

codify
  :: SerialMap
  -> Grammar
  -> SAnno GMeta One (CMeta, [Argument])
  -> MorlocMonad (SAnno GMeta One MDoc)
codify h g x = codify' True h g x |>> mapC third

codify'
  :: Bool
  -> SerialMap
  -> Grammar
  -> SAnno GMeta One (CMeta, [Argument])
  -> MorlocMonad (SAnno GMeta One (CMeta, [Argument], MDoc))
-- primitives
codify' _ _ g (SAnno (One (UniS, (c, rs))) m) = return $ SAnno (One (UniS, (c, rs, gNull g))) m
codify' _ _ _ (SAnno (One (NumS x, (c, rs))) m) = return $ SAnno (One (NumS x, (c, rs, viaShow x))) m
codify' _ _ g (SAnno (One (LogS x, (c, rs))) m) = return $ SAnno (One (LogS x, (c, rs, (gBool g) x))) m
codify' _ _ g (SAnno (One (StrS x, (c, rs))) m) = return $ SAnno (One (StrS x, (c, rs, (gQuote g) (pretty x)))) m

-- | ListS [SAnno a]
codify' _ h g (SAnno (One (ListS xs, (c, args))) m) = do
  xs' <- mapM (codify' False h g) xs
  let mdoc = (gList g) (map (sannoWithC third) xs')
  return $ SAnno (One (ListS xs', (c, args, mdoc))) m

-- | TupleS [SAnno a]
codify' _ h g (SAnno (One (TupleS xs, (c, args))) m) = do
  xs' <- mapM (codify' False h g) xs
  let mdoc = (gTuple g) (map (sannoWithC third) xs')
  return $ SAnno (One (TupleS xs', (c, args, mdoc))) m

-- | RecS [(EVar, SAnno a)]
codify' _ h g (SAnno (One (RecS entries, (c, args))) m) = do
  xs <- mapM (codify' False h g) (map snd entries)
  let mdoc = (gRecord g) (zip (map (pretty . fst) entries) (map (sannoWithC third) xs))
      sexpr = RecS (zip (map fst entries) xs)
  return $ SAnno (One (sexpr, (c, args, mdoc))) m

-- | LamS [EVar] (SAnno a)
codify' isTop h g (SAnno (One (LamS vs x, (c, args))) m) = do
  x' <- codify' isTop h g x
  let mdoc = undefined -- FIXME: what does a function look like, anyway?
  return (SAnno (One (LamS vs x', (c, args, mdoc))) m)

-- | ForeignS Int Lang
codify' _ h g (SAnno (One (ForeignS mid lang, (c, args))) m) = do
  let t = metaType c
  config <- MM.ask
  args' <- mapM cliArg args
  mdoc <- case MC.getPoolCallBuilder config lang (gQuote g) of
    Nothing -> MM.throwError . OtherError $ "ah for fuck sake!!!"
    (Just poolBuilder) -> do
      let exe = pretty $ Lang.makeExecutableName lang "pool"
      return . gForeignCall g $ ForeignCallDoc
        { fcdForeignPool = pretty $ Lang.makeSourceName lang "pool"
        , fcdForeignExe = exe
        , fcdMid = pretty mid
        , fcdArgs = args'
        , fcdCall = poolBuilder exe (pretty mid)
        , fcdFile = pretty $ Lang.makeSourceName (langOf' t) "pool"
        }
  return $ SAnno (One (ForeignS mid lang, (c, args, mdoc))) m
  where
    cliArg :: Argument -> MorlocMonad MDoc
    cliArg (PackedArgument v _ _) = return $ pretty v
    cliArg (UnpackedArgument v _ (Just unpacker)) = return $ (gCall g) (pretty unpacker) [pretty v]
    cliArg (UnpackedArgument v _ Nothing) = MM.throwError . OtherError $
      "In cliArg, no unpacker found"

-- | VarS EVar
codify' _ _ _ (SAnno (One (VarS v, (c, args))) m) = do
  let name = unEVar v
  return $ SAnno (One (VarS v, (c, args, pretty name))) m

codify' False _ _ (SAnno (One (CallS src, (c, args))) m) = do
  let name = srcName src
  return $ SAnno (One (CallS src, (c, args, pretty name))) m

codify' True _ g (SAnno (One (CallS src, (c, args))) m) = do
  let ftype = metaType c -- full function type
      otype = last (typeArgs ftype) -- output type
      itypes = init (typeArgs ftype) -- input types
  inputs' <- mapM (simplePrepInput g) (zip [0..] args)
  args' <- mapM (prepArg g) args
  let name = unName (srcName src)
  let mdoc = gFunction g $ GeneralFunction {
      gfComments = comments
    , gfReturnType = Just ((gShowType g) otype)
    , gfName = makeManifoldName m
    , gfArgs = args'
    , gfBody =
        vsep (map snd inputs') <> line <>
        (gReturn g $ (gCall g) (pretty name) (map fst inputs'))
    }
  return $ SAnno (One (CallS src, (c, args, mdoc))) m
  where
    comments = catMaybes
      [ Just "From A"
      , Just (pretty (srcName src))
      , fmap (generalSignature (metaName m)) (metaGeneralType m)
      , Just $ concreteSignature (metaName m) (metaType c)
      ]

    simplePrepInput
      :: Grammar
      -> (Int, Argument)
      -> MorlocMonad ( MDoc -- variable name
                     , MDoc -- assignment code
                     )
    simplePrepInput g (i, PackedArgument v t (Just unpackerName)) = do
      let varname = makeArgumentName i
      return (varname, gAssign g $ GeneralAssignment
         { gaType = Just . gShowType g $ t
         , gaName = varname
         , gaValue = (gCall g) (pretty unpackerName) [pretty v]
         , gaArg = Nothing
         })
    simplePrepInput _ (_, UnpackedArgument _ _ _) = MM.throwError . OtherError $
      "Don't panic"
    simplePrepInput _ (_, PackedArgument _ _ Nothing) = MM.throwError . OtherError $
      "Oh shit, panic panic PANIC!!!"

-- | AppS (SAnno a) [SAnno a]
codify' _ h g (SAnno (One (AppS f xs, (c, args))) m) = do
  let t = metaType c
  f' <- codify' False h g f
  xs' <- mapM (codify' False h g) xs 
  mandoc <- makeManifoldDoc g xs' f' (c, args, m)
  return $ SAnno (One (AppS f' xs', (c, args, mandoc))) m


makeManifoldDoc
  :: Grammar
  -> [SAnno GMeta One (CMeta, [Argument], MDoc)] -- inputs
  -> SAnno GMeta One (CMeta, [Argument], MDoc) -- the function
  -> (CMeta, [Argument], GMeta) -- the AppS meta data
  -> MorlocMonad MDoc
makeManifoldDoc g inputs (SAnno (One (x, (c', _, _))) m') (c, margs, m) = do
  let ftype = metaType c'
      otype = metaType c

  -- TODO: this is the problem part
  innerCall <- case x of
    (AppS _ _) -> return $ makeManifoldName m'
    (CallS src) -> return (pretty $ srcName src)
    (LamS vs x) -> return (pretty $ metaName m')
    _ -> MM.throwError . OtherError $ "Unexpected innerCall: " <> render (descSExpr x)

  inputs' <- zipWithM (prepInput g margs) [0..] inputs
  args' <- mapM (prepArg g) margs
  return . gFunction g $ GeneralFunction
    { gfComments = catMaybes $
        [ Just "From B"
        , Just (descSExpr x)
        , fmap (generalSignature (metaName m')) (metaGeneralType m')
        , Just $ concreteSignature (metaName m') (metaType c')
        ]
    , gfReturnType = Just ((gShowType g) otype)
    , gfName = makeManifoldName m
    , gfArgs = args'
    , gfBody =
        vsep (catMaybes (map snd inputs')) <> line <>
        (gReturn g $ (gCall g) innerCall (map fst inputs'))
    }


-- Handle preparation of arguments passed to a manifold. Return the name of the
-- variable that will be used and an block of code that defines the variable
-- (if necessary).
prepInput
  :: Grammar
  -> [Argument] -- arguments in the parent (the input arguments may differ)
  -> Int
  -> SAnno GMeta One (CMeta, [Argument], MDoc) -- an input to the wrapped function
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
prepInput g _ mid (SAnno (One (AppS x xs, (c, args, d))) m) = do
  let varname = makeArgumentName mid
      t = metaType c
      mname = makeManifoldName m
      ass = (gAssign g) $ GeneralAssignment
        { gaType = Just . gShowType g . last . typeArgs $ t
        , gaName = varname
        , gaValue = (gCall g) mname (map (pretty . argName) args)
        , gaArg = Nothing
        }
  return (varname, Just ass)
-- handle sourced files, which should be used as unaliased variables
prepInput _ _ _ (SAnno (One (CallS src, _)) _) = return (pretty (srcName src), Nothing)
prepInput g rs mid (SAnno (One (_, (c, _, d))) m) = do
  let name = metaName m
      varname = makeArgumentName mid
      t = metaType c
      arg = listToMaybe [r | r <- rs, Just (argName r) == metaName m]
  case (name, arg) of
    (Just n, Just (PackedArgument _ t (Just unpacker))) ->
       return (varname, Just . gAssign g $ GeneralAssignment
          { gaType = Just . gShowType g $ t
          , gaName = varname
          , gaValue = (gCall g) (pretty unpacker) [pretty name]
          , gaArg = Nothing
          }
       )
    (_, Just (PackedArgument _ _ Nothing)) -> MM.throwError . OtherError $
      "No unpacker found"
    -- the argument is used in the wrapped function, but is not serialized
    (Just n, _) -> return (pretty name, Nothing)
    (Nothing, _) -> MM.throwError . OtherError $ "import prep error"

-- | Serialize the result of a call if a serialization function is defined.
-- Presumably, if no serialization function is given, then the argument is
-- either a native construct or will be passed on in serialized form.
prepArg :: Grammar -> Argument -> MorlocMonad (Maybe MDoc, MDoc)
prepArg g (PackedArgument v t _) = return (Just . gShowType g $ gSerialType g, pretty v)
prepArg g (UnpackedArgument v t _) = return (Just . gShowType g $ t, pretty v)
prepArg _ _ = MM.throwError . OtherError $ "Something is stinky in the packers"



-------- Utility and lookup functions ----------------------------------------

say :: Doc ann -> MorlocMonad ()
say d = liftIO . putDoc $ " : " <> d <> "\n"

-- | choose a packer or unpacker for a given type
selectFunction :: ConcreteType -> Property -> SerialMap -> Maybe (Name, Path)
selectFunction t p h =
  case mostSpecificSubtypes (typeOf t) (map typeOf (Map.keys hmap)) of
    [] -> Nothing
    (x:_) -> Map.lookup (ConcreteType x) hmap
    where
      hmap = if p == Pack then packers h else unpackers h

unrollLambda :: Expr -> ([EVar], Expr)
unrollLambda (LamE v e2) = case unrollLambda e2 of
  (vs, e) -> (v:vs, e)
unrollLambda (AnnE (LamE v e2) _) = case unrollLambda e2 of
  (vs, e) -> (v:vs, e)
unrollLambda e = ([], e)

getGeneralType :: [Type] -> MorlocMonad (Maybe GeneralType)
getGeneralType ts = case [GeneralType t | t <- ts, langOf' t == MorlocLang] of
  [] -> return Nothing
  [x] -> return $ Just x
  xs -> MM.throwError . OtherError $
    "Expected 0 or 1 general types, found " <> MT.show' (length xs)

getConcreteTypes :: [Type] -> [ConcreteType]
getConcreteTypes ts = [ConcreteType t | t <- ts, isJust (langOf t)]

typeArgs :: ConcreteType -> [ConcreteType]
typeArgs (ConcreteType t) = map concreteType (typeArgs' t)
  where
    typeArgs' (FunT t1 t2) = t1 : typeArgs' t2
    typeArgs' t@(Forall _ _) = error . MT.unpack . render $
      "unexpected qualified type, this is probably a the generator, not the typechecker or Morloc code (" <> prettyType t <> ")"
    typeArgs' t = [t]

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
unpackSAnno f (SAnno (One (e@(ListS xs),     c)) g) = f e g c : conmap (unpackSAnno f) xs
unpackSAnno f (SAnno (One (e@(TupleS xs),    c)) g) = f e g c : conmap (unpackSAnno f) xs
unpackSAnno f (SAnno (One (e@(RecS entries), c)) g) = f e g c : conmap (unpackSAnno f) (map snd entries)
unpackSAnno f (SAnno (One (e@(LamS _ x),     c)) g) = f e g c : unpackSAnno f x
unpackSAnno f (SAnno (One (e@(AppS x xs),    c)) g) = f e g c : conmap (unpackSAnno f) (x:xs)
unpackSAnno f (SAnno (One (e, c)) g)                = [f e g c]

sannoWithC :: (c -> a) -> SAnno g One c -> a
sannoWithC f (SAnno (One (_, c)) _) = f c

mapC :: (c -> a) -> SAnno g One c -> SAnno g One a
mapC f (SAnno (One (ListS xs, c)) g) =
  SAnno (One (ListS (map (mapC f) xs), f c)) g
mapC f (SAnno (One (TupleS xs, c)) g) =
  SAnno (One (TupleS (map (mapC f) xs), f c)) g
mapC f (SAnno (One (RecS entries, c)) g) =
  SAnno (One (RecS (map (\(k, v) -> (k, mapC f v)) entries), f c)) g
mapC f (SAnno (One (LamS vs x, c)) g) =
  SAnno (One (LamS vs (mapC f x), f c)) g
mapC f (SAnno (One (AppS x xs, c)) g) =
  SAnno (One (AppS (mapC f x) (map (mapC f) xs), f c)) g
mapC f (SAnno (One (VarS x, c)) g) = SAnno (One (VarS x, f c)) g
mapC f (SAnno (One (CallS src, c)) g) = SAnno (One (CallS src, f c)) g
mapC f (SAnno (One (UniS, c)) g) = SAnno (One (UniS, f c)) g
mapC f (SAnno (One (NumS x, c)) g) = SAnno (One (NumS x, f c)) g
mapC f (SAnno (One (LogS x, c)) g) = SAnno (One (LogS x, f c)) g
mapC f (SAnno (One (StrS x, c)) g) = SAnno (One (StrS x, f c)) g
mapC f (SAnno (One (ForeignS i lang, c)) g) = SAnno (One (ForeignS i lang, f c)) g

sannoType :: SAnno g One CMeta -> ConcreteType
sannoType = sannoWithC (metaType . id)

descSExpr :: SExpr g f c -> MDoc
descSExpr (UniS) = "UniS"
descSExpr (VarS v) = "VarS" <+> pretty v
descSExpr (CallS src)
  =   "CallS"
  <+> pretty (srcAlias src)
  <+> parens (pretty (srcAlias src) <> "@" <> viaShow (srcLang src))
descSExpr (ListS _) = "ListS"
descSExpr (TupleS _) = "TupleS"
descSExpr (LamS vs _) = "LamS" <+> hsep (map pretty vs)
descSExpr (AppS _ _) = "AppS"
descSExpr (NumS _) = "NumS"
descSExpr (LogS _) = "LogS"
descSExpr (StrS _) = "StrS"
descSExpr (RecS _) = "RecS"
descSExpr (ForeignS i lang) = "ForeignS" <+> pretty i <+> viaShow lang

concreteSignature :: Maybe EVar -> ConcreteType -> MDoc
concreteSignature n (ConcreteType t)
  = maybe "_" pretty n <+> viaShow (langOf' t) <+> "::" <+> prettyType t

generalSignature :: Maybe EVar -> GeneralType -> MDoc
generalSignature n (GeneralType t)
  = maybe "_" pretty n <+> "::" <+> prettyType t

partialApply :: Type -> MorlocMonad Type
partialApply (FunT _ t) = return t
partialApply (Forall v t) = do
  t' <- partialApply t
  return $ Forall v t'
partialApply _ = MM.throwError . OtherError $
  "Cannot partially apply a non-function type"

partialApplyN :: Int -> Type -> MorlocMonad Type
partialApplyN i t
  | i < 0 = MM.throwError . OtherError $
    "Do you really want to apply a negative number of arguments?"
  | i == 0 = return t
  | i > 0 = do
    appliedType <- partialApply t
    partialApplyN (i-1) appliedType

pack :: Argument -> Argument
pack (UnpackedArgument v t n) = PackedArgument v t n
pack x = x

unpack :: Argument -> Argument
unpack (PackedArgument v t n) = UnpackedArgument v t n
unpack x = x

sannoSnd :: SAnno g One (a, b) -> b
sannoSnd (SAnno (One (_, (_, x))) _) = x

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
    typeEVar name = case Map.lookup name (moduleTypeMap m) of
      (Just (TypeSet t ts)) -> AnnE (VarE name) (map etype (maybe ts (\t' -> t':ts) t))
      Nothing -> error $ "Variable '" <> MT.unpack (unEVar name) <> "' is not defined"

    listMVars :: Module -> [MVar]
    listMVars m = Map.elems $ Map.filterWithKey (\v' _ -> v' == v) (moduleImportMap m)
