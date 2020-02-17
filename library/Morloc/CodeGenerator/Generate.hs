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
    >>= mapM (collect modmap)   -- [SAnno GMeta Many [CType]]
    -- select a single instance at each node in the tree
    >>= mapM realize  -- [SAnno GMeta One CType]
    -- rewrite partials
    >>= mapM rewritePartials

  -- print abstract syntax trees to the console as debugging message
  say $ line <> indent 2 (vsep (map (writeAST id Nothing) ast))

  -- build nexus
  -- -----------
  -- Each nexus subcommand calls one function from one one pool.
  -- The call passes the pool an index for the function (manifold) that will be called.
  nexus <- Nexus.generate
    [ (t, poolId m x, metaName m)
    | SAnno (One (x, t)) m <- ast
    ]

  -- recursively find all serializers imported from any module
  smap <- findSerializers ms

  -- for each language, collect all functions into one "pool"
  pools
    <- mapM (parameterize smap) ast
    -- Separate the call trees into mono-lingual segments terminated in
    -- primitives or foreign calls.
    >>= mapM (segment smap) |>> concat
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
    poolId :: GMeta -> SExpr GMeta One CType -> Int
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

  findSerialFun :: Property -> Module -> Map.Map CType (Name, Path)
  findSerialFun p m
    = Map.fromList
    . map (\(t, x) -> (getType p t, x))
    . mapSum
    . Map.mapWithKey (\v t -> conmap (g m) (f p v t))
    $ moduleTypeMap m

  -- extract serialization functions from a typeset if appropriate
  f :: Property -> EVar -> TypeSet -> [(CType, EVar)]
  f p v (TypeSet (Just gentype) ts) =
    if Set.member p (eprop gentype)
      then [(CType (etype t), v) | t <- ts]
      else [(CType (etype t), v) | t <- ts, Set.member p (eprop t)]
  f p v (TypeSet Nothing ts) =
    [ (CType (etype t), v)
    | t <- ts
    , Set.member p (eprop t)]

  -- find the source for the serialization functions
  g :: Module -> (CType, EVar) -> [(CType, (Name, Path))]
  g m (t, v) = case Map.lookup (v, langOf' t) (moduleSourceMap m) of
    (Just (Source name _ (Just path) _)) -> [(t, (name, path))]
    _ -> []

  -- get the type that is being serialized or deserialized
  getType :: Property -> CType -> CType
  getType p (CType t) = CType $ getType' p t where
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
  -> MorlocMonad (SAnno GMeta Many [CType])
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
      -> MorlocMonad (SExpr GMeta Many [CType], [CType])
    collectTerm (Declared m _ (AnnE x ts)) = do
      xs <- collectExpr Set.empty m (getCTypes ts) x
      case xs of
        [x] -> return x
        _ -> MM.throwError . OtherError $
          "Expected exactly one topology for a declared term"
    collectTerm (Declared _ _ _) = MM.throwError . OtherError $
      "Invalid expression in CollectTerm Declared, expected AnnE"
    collectTerm term@(Sourced m src) = do
      ts <- getTermTypes term |>> getCTypes
      return (CallS src, ts)
      where
        getTermTypes :: TermOrigin -> MorlocMonad [Type]
        getTermTypes t = do
          (TypeSet _ es) <- getTermTypeSet t
          return $ map etype es

    collectAnno
      :: Set.Set EVar
      -> Module
      -> Expr
      -> MorlocMonad (SAnno GMeta Many [CType])
    collectAnno args m (AnnE e ts) = do
      gtype <- getGType ts
      gmeta <- makeGMeta (getExprName e) m gtype
      trees <- collectExpr args m (getCTypes ts) e
      return $ SAnno (Many trees) gmeta
    collectAnno _ _ _ = error "impossible bug - unannotated expression"

    getExprName :: Expr -> Maybe EVar
    getExprName (VarE v) = Just v
    getExprName _ = Nothing

    collectExpr
      :: Set.Set EVar
      -> Module
      -> [CType]
      -> Expr
      -> MorlocMonad [(SExpr GMeta Many [CType], [CType])]
    collectExpr args m ts (UniE) = return [(UniS, ts)]
    collectExpr args m ts (NumE x) = return [(NumS x, ts)]
    collectExpr args m ts (LogE x) = return [(LogS x, ts)]
    collectExpr args m ts (StrE x) = return [(StrS x, ts)]
    collectExpr args m ts (VarE v)
      | Set.member v args = return [(VarS v, ts)]
      | otherwise = do
          let terms = findTerm True ms m v
          xs <- mapM collectTerm terms
          let chosen = map (chooseTypes ts) xs
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
          :: [CType]
          -> (SExpr GMeta Many [CType], [CType])
          -> (SExpr GMeta Many [CType], [CType])
        chooseTypes ts (x, ts') =
          (x, [ t
              | t <- ts
              , t' <- ts'
              , langOf' t == langOf' t'])
    collectExpr args m ts (ListE es) = do
      es' <- mapM (collectAnno args m) es
      return [(ListS es', ts)]
    collectExpr args m ts (TupleE es) = do
      es' <- mapM (collectAnno args m) es
      return [(TupleS es', ts)]
    collectExpr args m ts (RecE entries) = do
      es' <- mapM (collectAnno args m) (map snd entries)
      let entries' = zip (map fst entries) es'
      return [(RecS entries', ts)]
    collectExpr args m ts e@(LamE v _) =
      case unrollLambda e of
        (args', e') -> do
          e'' <- collectAnno (Set.union args (Set.fromList args')) m e'
          return [(LamS args' e'', ts)]
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
      -> SAnno GMeta Many [CType]
      -> (SExpr GMeta Many [CType], [CType])
      -> MorlocMonad (SExpr GMeta Many [CType], [CType])
    app _ e2 ((AppS f es), ts) = do
      ts' <- mapM partialApplyConcrete ts
      return (AppS f (es ++ [e2]), ts')
    app g e2 (f, ts) = do
      ts' <- mapM partialApplyConcrete ts
      return (AppS (SAnno (Many [(f, ts)]) g) [e2], ts')

    partialApplyConcrete :: CType -> MorlocMonad CType
    partialApplyConcrete t =
      fmap CType $ partialApply (unCType t)

-- | Find info common across realizations of a given term in a given module
makeGMeta :: Maybe EVar -> Module -> Maybe GType -> MorlocMonad GMeta
makeGMeta name m gtype = do
  i <- MM.getCounter
  case name >>= (flip Map.lookup) (moduleTypeMap m) of
    (Just (TypeSet (Just e) _)) -> do
      return $ GMeta
        { metaId = i
        , metaName = name
        , metaGType = maybe (Just . GType $ etype e) Just gtype
        , metaProperties = eprop e
        , metaConstraints = econs e
        }
    _ -> do
      return $ GMeta
        { metaId = i
        , metaName = name
        , metaGType = gtype
        , metaProperties = Set.empty
        , metaConstraints = Set.empty
        }

rewrite
  :: SAnno GMeta Many [CType]
  -> MorlocMonad (SAnno GMeta Many [CType])
rewrite (SAnno (Many es0) g0) = do
  es0' <- fmap concat $ mapM rewriteL0 es0 
  return $ SAnno (Many es0') g0
  where
    rewriteL0
      :: (SExpr GMeta Many [CType], [CType])
      -> MorlocMonad [(SExpr GMeta Many [CType], [CType])]
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
    -- UniS NumS LogS StrS CallS ForeignS
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
  :: SAnno GMeta Many [CType]
  -> MorlocMonad (SAnno GMeta One CType)
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
      -> SAnno GMeta Many [CType]
      -> MorlocMonad (Maybe (Int, SAnno GMeta One CType))
    realizeAnno depth langMay (SAnno (Many xs) m) = do
      asts <- mapM (\(x, cs) -> mapM (realizeExpr (depth+1) langMay x) cs) xs |>> concat
      case minimumOnMay (\(s,_,_) -> s) (catMaybes asts) of
        Just (i, x, c) -> do
          return $ Just (i, SAnno (One (x, c)) m)
        Nothing -> do
          return Nothing

    indent' :: Int -> MDoc
    indent' i = pretty (take i (repeat '-')) <> " "

    realizeExpr
      :: Int
      -> Maybe Lang
      -> SExpr GMeta Many [CType]
      -> CType
      -> MorlocMonad (Maybe (Int, SExpr GMeta One CType, CType))
    realizeExpr depth lang x c = realizeExpr' depth (maybe (langOf' c) id lang) x c

    realizeExpr'
      :: Int
      -> Lang
      -> SExpr GMeta Many [CType]
      -> CType
      -> MorlocMonad (Maybe (Int, SExpr GMeta One CType, CType))
    realizeExpr' _ lang (UniS) c
      | lang == langOf' c = return $ Just (0, UniS, c)
      | otherwise = return Nothing
    realizeExpr' _ lang (NumS x) c
      | lang == langOf' c = return $ Just (0, NumS x, c)
      | otherwise = return Nothing
    realizeExpr' _ lang (LogS x) c
      | lang == langOf' c = return $ Just (0, LogS x, c)
      | otherwise = return Nothing
    realizeExpr' _ lang (StrS x) c
      | lang == langOf' c = return $ Just (0, StrS x, c)
      | otherwise = return Nothing
    realizeExpr' _ lang (CallS src) c
      | lang == langOf' c = return $ Just (0, CallS src, c)
      | otherwise = return Nothing
    realizeExpr' _ lang (VarS x) c
      | lang == langOf' c = return $ Just (0, VarS x, c)
      | otherwise = return Nothing
    realizeExpr' depth lang (ListS xs) c
      | lang == langOf' c = do
        xsMay <- mapM (realizeAnno depth (Just lang)) xs
        case (fmap unzip . sequence) xsMay of
          (Just (scores, xs')) -> return $ Just (sum scores, ListS xs', c)
          Nothing -> return Nothing
      | otherwise = return Nothing
    realizeExpr' depth lang (TupleS xs) c
      | lang == langOf' c = do
        xsMay <- mapM (realizeAnno depth (Just lang)) xs
        case (fmap unzip . sequence) xsMay of
          (Just (scores, xs')) -> return $ Just (sum scores, TupleS xs', c)
          Nothing -> return Nothing
      | otherwise = return Nothing
    realizeExpr' depth lang (RecS entries) c
      | lang == langOf' c = do
          xsMay <- mapM (realizeAnno depth (Just lang)) (map snd entries)
          case (fmap unzip . sequence) xsMay of
            (Just (scores, vals)) -> return $ Just (sum scores, RecS (zip (map fst entries) vals), c)
            Nothing -> return Nothing
      | otherwise = return Nothing
    realizeExpr' depth _ (LamS vs x) c = do
      xMay <- realizeAnno depth (Just $ langOf' c) x
      case xMay of
        (Just (score, x')) -> return $ Just (score, LamS vs x', c)
        Nothing -> return Nothing
    realizeExpr' depth lang (AppS f xs) c = do
      fMay <- realizeAnno depth (Just $ langOf' c) f
      xsMay <- mapM (realizeAnno depth (Just $ langOf' c)) xs
      case (fMay, (fmap unzip . sequence) xsMay, Lang.pairwiseCost lang (langOf' c)) of
        (Just (fscore, f'), Just (scores, xs'), Just interopCost) ->
          return $ Just (fscore + sum scores + interopCost, AppS f' xs', c) 
        _ -> return Nothing
    realizeExpr' _ _ (ForeignS _ _ _) _ = MM.throwError . OtherError $
      "ForeignS should not yet appear in an SExpr"

rewritePartials
  :: SAnno GMeta One CType
  -> MorlocMonad (SAnno GMeta One CType)
rewritePartials (SAnno (One (AppS f xs, ftype@(CType (FunT _ _)))) m) = do
  let gTypeArgs = maybe (repeat Nothing) typeArgsG (metaGType m)
  f' <- rewritePartials f
  xs' <- mapM rewritePartials xs
  lamGType <- makeGType $ [metaGType g | (SAnno _ g) <- xs'] ++ gTypeArgs
  let vs = map EVar . take (nargs ftype) $ freshVarsAZ [] -- TODO: exclude existing arguments
      ys = zipWith3 makeVar vs (typeArgsC ftype) gTypeArgs
  -- unsafe, but should not fail for well-typed input
      appType = fromJust . last . typeArgsC $ ftype
      appMeta = m {metaGType = metaGType m >>= (last . typeArgsG)}
      lamCType = untypeArgs $ map (Just . sannoWithC id) xs' ++ typeArgsC ftype
      lamMeta = m {metaGType = Just lamGType}
  return $ SAnno (One (LamS vs (SAnno (One (AppS f' (xs' ++ ys), appType)) appMeta), lamCType)) lamMeta
  where
    makeGType :: [Maybe GType] -> MorlocMonad GType
    makeGType ts = fmap GType . makeType . map unGType $ (map fromJust ts)

    makeVar :: EVar -> Maybe CType -> Maybe GType -> SAnno GMeta One CType
    makeVar _ Nothing _ = error "Yeah, so this can happen"
    makeVar v (Just c) g = SAnno (One (VarS v, c))
      ( m { metaGType = g
          , metaName = Nothing
          , metaProperties = Set.empty
          , metaConstraints = Set.empty
          }
      )

    makeType :: [Type] -> MorlocMonad Type
    makeType [] = MM.throwError . OtherError $ "empty type"
    makeType [t] = return t
    makeType (t:ts) = FunT <$> pure t <*> makeType ts

-- apply the pattern above down the AST
rewritePartials (SAnno (One (AppS f xs, t)) m) = do
  xs' <- mapM rewritePartials xs
  f' <- rewritePartials f
  return $ SAnno (One (AppS f' xs', t)) m
rewritePartials (SAnno (One (LamS vs x, t)) m) = do
  x' <- rewritePartials x
  return $ SAnno (One (LamS vs x', t)) m
rewritePartials (SAnno (One (ListS xs, t)) m) = do
  xs' <- mapM rewritePartials xs
  return $ SAnno (One (ListS xs', t)) m
rewritePartials (SAnno (One (TupleS xs, t)) m) = do
  xs' <- mapM rewritePartials xs
  return $ SAnno (One (TupleS xs', t)) m
rewritePartials (SAnno (One (RecS entries, t)) m) = do
  let keys = map fst entries
  vals <- mapM rewritePartials (map snd entries)
  return $ SAnno (One (RecS (zip keys vals), t)) m
rewritePartials x = return x

writeAST
  :: (a -> CType) -> Maybe (a -> MDoc) -> SAnno GMeta One a -> MDoc
writeAST getType extra s = hang 2 . vsep $ ["AST:", describe s]
  where
    addExtra x = case extra of
      (Just f) -> " " <> f x
      Nothing -> ""

    describe (SAnno (One (x@(ListS xs), _)) _) = descSExpr x
    describe (SAnno (One (x@(TupleS xs), _)) _) = descSExpr x
    describe (SAnno (One (x@(RecS xs), _)) _) = descSExpr x
    describe (SAnno (One (x@(AppS f xs), c)) g) =
      hang 2 . vsep $
        [ descSExpr x <+> parens (prettyType (getType c)) <> addExtra c
        , describe f
        ] ++ map describe xs
    describe (SAnno (One (f@(LamS _ x), c)) g) = do 
      hang 2 . vsep $
        [ name (getType c) g
            <+> descSExpr f
            <+> parens (prettyType (getType c))
            <> addExtra c
        , describe x
        ] 
    describe (SAnno (One (x, c)) _) =
          descSExpr x
      <+> parens (prettyType (getType c))
      <>  addExtra c

    name :: CType -> GMeta -> MDoc
    name (viaShow . langOf' -> lang) g =
      maybe
        ("_" <+> lang <+> "::")
        (\x -> pretty x <+> lang <+> "::")
        (metaName g)


-- | This function handles the mechanics of segmentation, not the choice of
-- languages or the let-optimizations that ultimately determine which
segment
  :: SerialMap
  -> SAnno GMeta One (CType, [Argument])
  -> MorlocMonad [SAnno GMeta One (CType, [Argument])]
segment h x@(SAnno (One (_, c)) _) = do
  say $ " ---- entering segment"
  (x', xs) <- segment' (fst c) x
  say $ line <> indent 2 (vsep (map writeAST' (x' : xs)))
  return (x' : xs)
  where
    writeAST' = writeAST fst (Just (list . map prettyArgument . snd))

    segment' 
      :: CType
      -> SAnno GMeta One (CType, [Argument])
      -> MorlocMonad
         ( SAnno GMeta One (CType, [Argument])
         , [SAnno GMeta One (CType, [Argument])])
    segment' _ x@(SAnno (One (UniS  , _)) _) = return (x, [])
    segment' _ x@(SAnno (One (NumS _, _)) _) = return (x, [])
    segment' _ x@(SAnno (One (LogS _, _)) _) = return (x, [])
    segment' _ x@(SAnno (One (StrS _, _)) _) = return (x, [])
    segment' _ x@(SAnno (One (VarS _, _)) _) = return (x, [])
    segment' _ (SAnno (One (ListS xs, c)) m) = do
      t <- listType (fst c)
      (xs', rs) <- mapM (segment' t) xs |>> unzip
      return (SAnno (One (ListS xs', c)) m, concat rs)
      where
        listType :: CType -> MorlocMonad CType
        listType (CType (ArrT (TV _ "List") [t])) = return (CType t)
        listType _ = MM.throwError . OtherError $ "Expected List type"
    segment' _ (SAnno (One (TupleS xs, c)) m) = do
      ts <- tupleTypes (fst c)
      (xs', rs) <- zipWithM segment' ts xs |>> unzip
      return (SAnno (One (TupleS xs', c)) m, concat rs)
      where
        tupleTypes :: CType -> MorlocMonad [CType]
        tupleTypes (CType (ArrT (TV _ "Tuple") ts)) = return (map CType ts)
        tupleTypes _ = MM.throwError . OtherError $ "Expected Tuple type"
    segment' _ (SAnno (One (RecS xs, c)) m) = do
      ts <- recTypes (fst c)
      (vals, rs) <- zipWithM segment' ts (map snd xs) |>> unzip
      return (SAnno (One (RecS (zip (map fst xs) vals), c)) m, concat rs)
      where
        recTypes :: CType -> MorlocMonad [CType]
        recTypes (CType (RecT entries)) = return (map (CType . snd) entries)
        recTypes _ = MM.throwError . OtherError $ "Expected Tuple type"
    segment' t0 (SAnno (One (LamS vs x, c1)) m)
      | langOf' t0 == langOf' (fst c1) = do
          (x', rs) <- segment' (fromJust . last . typeArgsC . fst $ c1) x
          return (SAnno (One (LamS vs x', c1)) m, rs)
      | otherwise = MM.throwError . OtherError $
        "Foreign lambda's are not currently supported (coming soon)"
    segment' c0 (SAnno (One (AppS x@(SAnno (One (CallS src, c2)) m2) xs, c1)) m) = do
      (xs', xsrss) <- mapM (segment' (fst c1)) xs |>> unzip
      case langOf' c0 == langOf' (fst c2) of
        True -> return (SAnno (One (AppS x xs', c1)) m, concat xsrss)
        False -> do

          let lamArgs = typeArgsC (fst c2)
              lamType = fst c2
          -- argument names shared between segments
              vs' = map argName (snd c1)

          foreignArgs <- zipWithM (makeArgument h) vs' lamArgs

          -- FIXME: soooooo ugly ...
          let (SAnno (One (AppS x' xs'', c1'@(_, lamRs'))) _) = mapC (reparameterize foreignArgs) (SAnno (One (AppS x xs', c1)) m)

          -- foreign function argument type
          let foreignCMeta = (c0, snd c1)
              appCMeta = (fst c1, foreignArgs)
          -- let meta for each produced expression
          -- all three should have the same metaId
              foreignMeta = m
              lamMeta = m2 {metaId = metaId m} -- FIXME - need to adjust the general type
              appMeta = m2 {metaId = metaId m} -- FIXME - need to adjust the general type
          -- final three
          -- the terminal manifold in L1 in this segment, same type as the AppS
              foreignCall = SAnno (One (ForeignS (metaId m) (langOf' (fst c2)) vs', foreignCMeta)) foreignMeta
              foreignApp = SAnno (One (AppS x' xs'', c1')) appMeta
              foreignLam = SAnno (One (LamS vs' foreignApp, (lamType, lamRs'))) lamMeta
          return (foreignCall , foreignLam : concat xsrss)
    segment' _ x@(SAnno (One (CallS _, _)) _) = return (x, [])

-- Now that the AST is segmented by language, we can resolve passed-through
-- arguments where possible.
reparameterize
  :: [Argument]
  -> (CType, [Argument])
  -> (CType, [Argument])
reparameterize args0 (t, args1) = (t, map f args1)
  where
    f :: Argument -> Argument
    f r@(PackedArgument _ _ _) = r
    f r@(UnpackedArgument _ _ _) = r
    f r@(PassThroughArgument v) =
      case [r' | r' <- args0, argName r' == v] of
        (r':_) -> r'
        _ -> r


-- Sort manifolds into pools. Within pools, group manifolds into call sets.
pool
  :: [SAnno GMeta One (CType, [Argument])]
  -> MorlocMonad [(Lang, [SAnno GMeta One (CType, [Argument])])]
pool = return . groupSort . map (\s@(SAnno (One (_, (t, _))) _) -> (langOf' t, s))


encode
  :: SerialMap
  -> (Lang, [SAnno GMeta One (CType, [Argument])])
  -> MorlocMonad Script
encode h (lang, xs) = do
  state <- MM.get

  -- get the grammar rules for lang
  g <- selectGrammar lang

  -- find sources in lang that need to be included/imported
  let srcs = findSources h xs
  srcdocs <- mapM (encodeSource g) srcs

  -- translate each node in the AST to code
  codeTree <- mapM (codify h g) xs
  let manifolds = conmap gatherManifolds codeTree
      signatures = conmap (encodeSignatures g) xs

  -- generate final pool code
  code <- gMain g $ PoolMain
    { pmSources = srcdocs
    , pmSignatures = signatures
    , pmPoolManifolds = manifolds
    , pmDispatchManifold = makeDispatchBuilder h g xs
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
parameterize
  :: SerialMap
  -> SAnno GMeta One CType
  -> MorlocMonad (SAnno GMeta One (CType, [Argument]))
parameterize h (SAnno (One (LamS vs x, t)) m) = do
  args0 <- zipWithM (makeArgument h) vs (typeArgsC t)
  x' <- parameterize' h args0 x
  return $ SAnno (One (LamS vs x', (t, args0))) m
parameterize h (SAnno (One (CallS src, t)) m) = do
  ts <- case sequence (typeArgsC t) of
    (Just ts') -> return (init ts')
    Nothing -> MM.throwError . OtherError . render $
      "Unexpected type in parameterize CallS:" <+> prettyType t
  let vs = map EVar (freshVarsAZ [])
  args0 <- zipWithM (makeArgument h) vs (map Just ts)
  return $ SAnno (One (CallS src, (t, args0))) m
parameterize h x = parameterize' h [] x

-- TODO: the arguments coupled to every term should be the arguments USED
-- (not inherited) by the term. I need to ensure the argument threading
-- leads to correct passing of packed/unpacked arguments. AppS should
-- "know" that it needs to pack functions that are passed to a foreign
-- call, for instance.
parameterize'
  :: SerialMap
  -> [Argument] -- arguments in parental scope (child needn't retain them)
  -> SAnno GMeta One CType
  -> MorlocMonad (SAnno GMeta One (CType, [Argument]))
-- primitives, no arguments are required for a primitive, so empty lists
parameterize' _ _ (SAnno (One (UniS, c)) m) = return $ SAnno (One (UniS, (c, []))) m
parameterize' _ _ (SAnno (One (NumS x, c)) m) = return $ SAnno (One (NumS x, (c, []))) m
parameterize' _ _ (SAnno (One (LogS x, c)) m) = return $ SAnno (One (LogS x, (c, []))) m
parameterize' _ _ (SAnno (One (StrS x, c)) m) = return $ SAnno (One (StrS x, (c, []))) m
-- VarS EVar
parameterize' _ args (SAnno (One (VarS v, c)) m) = do
  let args' = filter (\r -> argName r == v) args
  return $ SAnno (One (VarS v, (c, args'))) m
-- CallS Source
parameterize' _ args (SAnno (One (CallS src, c)) m) = do
  return $ SAnno (One (CallS src, (c, []))) m
-- containers
parameterize' h args (SAnno (One (ListS xs, c)) m) = do
  xs' <- mapM (parameterize' h args) xs
  let args' = unique . concat . map sannoSnd $ xs'
  return $ SAnno (One (ListS xs', (c, args'))) m
parameterize' h args (SAnno (One (TupleS xs, c)) m) = do
  xs' <- mapM (parameterize' h args) xs
  let args' = unique . concat . map sannoSnd $ xs'
  return $ SAnno (One (TupleS xs', (c, args'))) m
parameterize' h args (SAnno (One (RecS entries, c)) m) = do
  vs' <- mapM (parameterize' h args) (map snd entries)
  let args' = unique . concat . map sannoSnd $ vs'
  return $ SAnno (One (RecS (zip (map fst entries) vs'), (c, args'))) m
parameterize' h _ (SAnno (One (LamS vs x, c)) m) = do
  args0 <- zipWithM (makeArgument h) vs (typeArgsC c)
  x' <- parameterize' h args0 x
  return $ SAnno (One (LamS vs x', (c, []))) m 
parameterize' h args (SAnno (One (AppS x xs, c)) m) = do
  x' <- parameterize' h args x
  xs' <- mapM (parameterize' h args) xs
  let args' = sannoSnd x' ++ (unique . concat . map sannoSnd) xs'
  return $ SAnno (One (AppS x' xs', (c, args'))) m
parameterize' _ args (SAnno (One (ForeignS i lang vs, c)) m) =
  case sequence $ map listToMaybe [[r | r <- args, argName r == v] | v <- vs] of
    Nothing -> MM.throwError . OtherError $ "Bad argument sent to ForeignS"
    Just args' -> return $ SAnno (One (ForeignS i lang vs, (c, args'))) m

makeArgument :: SerialMap -> EVar -> Maybe CType -> MorlocMonad Argument 
makeArgument h v (Just t) = case selectFunction t Unpack h of
  Nothing -> MM.throwError . OtherError . render $
    "No packer found for type" <+> parens (prettyType t)
  (Just (name, _)) -> return $ PackedArgument v t (Just name)
makeArgument _ v Nothing = return $ PassThroughArgument v

gatherManifolds :: SAnno g One MDoc -> [MDoc]
gatherManifolds (SAnno (One (CallS _, d)) _) = [d]
gatherManifolds x = catMaybes . unpackSAnno f $ x
  where
    f :: SExpr g One MDoc -> g -> MDoc -> Maybe MDoc
    f (AppS _ _) _ d = Just d
    f _ _ _ = Nothing


encodeSource :: Grammar -> Path -> MorlocMonad MDoc
encodeSource g path = gImport g <$> pure "" <*> (gPrepImport g) path


findSources :: SerialMap -> [SAnno g One (CType, [Argument])] -> [Path]
findSources h xs = unique . concat . concat . map (unpackSAnno f) $ xs
  where
    f :: SExpr g One (CType, [Argument]) -> g -> (CType, [Argument]) -> [Path]
    f (CallS src) _ (c, _) = catMaybes
      [ srcPath src
      , fmap snd $ selectFunction c Pack h
      , fmap snd $ selectFunction c Unpack h
      ]
    f _ _ (c, _) = catMaybes
      [ fmap snd $ selectFunction c Pack h
      , fmap snd $ selectFunction c Unpack h
      ]


-- | Create a signature/prototype. Not all languages need this. C and C++ need
-- prototype definitions, but R and Python do not. Languages that do not
-- require signatures may simply write the type information as comments at the
-- beginning of the source file.
encodeSignatures :: Grammar -> SAnno GMeta One (CType, [Argument]) -> [MDoc]
encodeSignatures g (SAnno (One (CallS _, (c, args))) m) = [makeSignature g (Just m, m, c, args)]
encodeSignatures g (SAnno (One (LamS _ x, _)) _) =
  map (makeSignature g) (catMaybes $ unpackSAnno f x) ++ maybeToList (getSourced g x)
  where
    f :: SExpr GMeta One (CType, [Argument])
      -> GMeta
      -> (CType, [Argument])
      -> Maybe (Maybe GMeta, GMeta, CType, [Argument])
    f (AppS (SAnno _ gFun) _) m (c, args) = Just (Just gFun, m, c, args)
    f (ForeignS _ _ _) m (c, args) = Just (Nothing, m, c, args)
    f _ _ _ = Nothing

    -- make the signature for an exported function that is directly sourced
    -- such cases will always be at the top level, hence no recursion is needed
    getSourced :: Grammar -> SAnno GMeta One (CType, [Argument]) -> Maybe MDoc
    getSourced g (SAnno (One ((VarS v), (c, args))) m) = Just (makeSignature g (Just m, m, c, args))
    getSourced _ _ = Nothing
encodeSignatures _ _ = error "Expected LamS or VarS at all top-level segments"

makeSignature :: Grammar -> (Maybe GMeta, GMeta, CType, [Argument]) -> MDoc
makeSignature g (gFun, m, c, args) = (gSignature g) $ GeneralFunction
  { gfComments = maybeToList $
      fmap (\(GType t) -> maybe "_" pretty (gFun >>= metaName) <+> "::" <+> prettyType t)
           (gFun >>= metaGType)
  , gfReturnType = Just . gShowType g . last . fromJust . sequence . typeArgsC $ c
  , gfName = makeManifoldName m
  , gfArgs = map (makeArg' g) args
  , gfBody = ""
  }

makeArg' :: Grammar -> Argument -> (Maybe MDoc, MDoc)
makeArg' g (PackedArgument v _ _)  = (Just ((gShowType g) (gSerialType g)), pretty v)
makeArg' g (PassThroughArgument v) = (Just ((gShowType g) (gSerialType g)), pretty v)
makeArg' g (UnpackedArgument v t _) = (Just ((gShowType g) t), pretty v)


varExists :: SAnno GMeta One CType -> EVar -> Bool
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
  -> [SAnno GMeta One (CType, [Argument])]
  -> (MDoc -> MDoc -> MDoc)
makeDispatchBuilder h g xs =
  (gSwitch g)
    (\(_, m, _) -> pretty (metaId m))
    (mainCall g)
    (mapMaybe getPoolCall xs)
  where
    getPoolCall
      :: SAnno GMeta One (CType, [Argument])
      -> Maybe ([EVar], GMeta, CType)
    getPoolCall (SAnno (One (LamS vs (SAnno (One (AppS _ _, (c, _))) m), _)) _)
      = Just (vs, m, c)
    getPoolCall (SAnno (One (LamS vs (SAnno (One (CallS _  , (c, _))) m), _)) _)
      | (length vs) > 0 = Just (vs, m, c)
      | otherwise = Nothing
    getPoolCall (SAnno (One (CallS _, (c, args))) m)
      | (length args) > 0 = Just (map argName args, m, last . fromJust . sequence . typeArgsC $ c)
      | otherwise = Nothing
    getPoolCall _ = Nothing

    -- Note, the CType is for the type of the full application, that is, the return type.
    -- It is NOT for the function that is called.
    mainCall :: Grammar -> ([EVar], GMeta, CType) -> MDoc
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
  -> SAnno GMeta One (CType, [Argument])
  -> MorlocMonad (SAnno GMeta One MDoc)
codify h g x = codify' True h g x |>> mapC third

codify'
  :: Bool
  -> SerialMap
  -> Grammar
  -> SAnno GMeta One (CType, [Argument])
  -> MorlocMonad (SAnno GMeta One (CType, [Argument], MDoc))
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
codify' _ h g (SAnno (One (ForeignS mid lang vs, (t, args))) m) = do
  config <- MM.ask
  args' <- mapM cliArg args
  packer <- case selectFunction t Unpack h of
    (Just (name, _)) -> return name
    _ -> MM.throwError . OtherError . render $
      "Could not find unpacker for foreign call of type:" <+> prettyType t
  mdoc <- case MC.getPoolCallBuilder config lang (gQuote g) of
    Nothing -> MM.throwError . OtherError $ "ah for fuck sake!!!"
    (Just poolBuilder) -> do
      let exe = pretty $ Lang.makeExecutableName lang "pool"
      return $ (gCall g) (pretty packer)
        [ gForeignCall g $ ForeignCallDoc
            { fcdForeignPool = pretty $ Lang.makeSourceName lang "pool"
            , fcdForeignExe = exe
            , fcdMid = pretty mid
            , fcdArgs = args'
            , fcdCall = poolBuilder exe (pretty mid)
            , fcdFile = pretty $ Lang.makeSourceName (langOf' t) "pool"
            }
        ]
  return $ SAnno (One (ForeignS mid lang vs, (t, args, mdoc))) m
  where
    cliArg :: Argument -> MorlocMonad MDoc
    cliArg (PackedArgument v _ _) = return $ pretty v
    cliArg (PassThroughArgument v) = return $ pretty v
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

codify' True _ g (SAnno (One (CallS src, (ftype, args))) m) = do
  (otype, itypes) <- case sequence (typeArgsC ftype) of
    (Just ts) -> return (last ts, init ts)
    Nothing -> MM.throwError . OtherError . render $
      "Unexpected type type codify':" <+> prettyType ftype
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
  return $ SAnno (One (CallS src, (ftype, args, mdoc))) m
  where
    comments = catMaybes
      [ Just "From A"
      , Just (pretty (srcName src))
      , fmap (generalSignature (metaName m)) (metaGType m)
      , Just $ concreteSignature (metaName m) ftype
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
    simplePrepInput _ (_, PassThroughArgument _) = MM.throwError . OtherError $
      "This might be a problem, or not, or whatever"

-- | AppS (SAnno a) [SAnno a]
codify' _ h g (SAnno (One (AppS f xs, (c, args))) m) = do
  f' <- codify' False h g f
  xs' <- mapM (codify' False h g) xs 
  mandoc <- makeManifoldDoc g xs' f' (c, args, m)
  return $ SAnno (One (AppS f' xs', (c, args, mandoc))) m


makeManifoldDoc
  :: Grammar
  -> [SAnno GMeta One (CType, [Argument], MDoc)] -- inputs
  -> SAnno GMeta One (CType, [Argument], MDoc) -- the function
  -> (CType, [Argument], GMeta) -- the AppS meta data
  -> MorlocMonad MDoc
makeManifoldDoc g inputs (SAnno (One (x, (ftype, _, _))) m') (otype, margs, m) = do
  innerCall <- case x of
    (AppS _ _) -> return $ makeManifoldName m'
    (CallS src) -> return (pretty $ srcName src)
    _ -> MM.throwError . OtherError $ "Unexpected innerCall: " <> render (descSExpr x)

  inputs' <- zipWithM (prepInput g margs) [0..] inputs
  args' <- mapM (prepArg g) margs

  return . gFunction g $ GeneralFunction
    { gfComments = catMaybes $
        [ Just "From B"
        , Just (descSExpr x)
        , fmap (generalSignature (metaName m')) (metaGType m')
        , Just $ concreteSignature (metaName m') ftype
        ]
    , gfReturnType = Just ((gShowType g) otype)
    , gfName = makeManifoldName m
    , gfArgs = args'
    , gfBody =
        vsep (catMaybes (map snd inputs')) <> line <>
        (gReturn g $ (gCall g) innerCall (map fst inputs'))
    }


-- Handle preparation of arguments passed to a manifold. Return the name of the
-- variable that will be used and a block of code that defines the variable
-- (if necessary).
prepInput
  :: Grammar
  -> [Argument] -- arguments in the parent (the input arguments may differ)
  -> Int
  -> SAnno GMeta One (CType, [Argument], MDoc) -- an input to the wrapped function
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
prepInput g _ _ (SAnno (One (ForeignS _ _ _, (_, _, d))) _) = return (d, Nothing)
prepInput g _ mid (SAnno (One (AppS x xs, (t, args, d))) m) = do
  gaType' <- case sequence (typeArgsC t) of
    (Just ts) -> return . Just . gShowType g . last $ ts
    Nothing -> MM.throwError . OtherError . render $
      "Unexpected type in prepInput:" <+> prettyType t
  let varname = makeArgumentName mid
      mname = makeManifoldName m
      ass = (gAssign g) $ GeneralAssignment
        { gaType = gaType'
        , gaName = varname
        , gaValue = (gCall g) mname (map (pretty . argName) args)
        , gaArg = Nothing
        }
  return (varname, Just ass)
-- handle sourced files, which should be used as unaliased variables
prepInput _ _ _ (SAnno (One (CallS src, _)) _) = return (pretty (srcName src), Nothing)
prepInput g rs mid (SAnno (One (VarS v, (t, _, d))) m) = do
  let name = pretty (unEVar v)
      varname = makeArgumentName mid
      arg = listToMaybe [r | r <- rs, argName r == v]
  case arg of
    (Just (PackedArgument _ t (Just unpacker))) ->
       return (varname, Just . gAssign g $ GeneralAssignment
          { gaType = Just . gShowType g $ t
          , gaName = varname
          , gaValue = (gCall g) (pretty unpacker) [name]
          , gaArg = Nothing
          }
       )
    (Just (PassThroughArgument v)) -> MM.throwError . OtherError . render $
      "Compiler bug: a PassThroughArgument cannot be an input"
    (Just (PackedArgument _ _ Nothing)) -> MM.throwError . OtherError $
      "No unpacker found"
    -- the argument is used in the wrapped function, but is not serialized
    Nothing -> return (name, Nothing)

-- | Serialize the result of a call if a serialization function is defined.
-- Presumably, if no serialization function is given, then the argument is
-- either a native construct or will be passed on in serialized form.
prepArg :: Grammar -> Argument -> MorlocMonad (Maybe MDoc, MDoc)
prepArg g (PackedArgument v t _) = return (Just . gShowType g $ gSerialType g, pretty v)
prepArg g (PassThroughArgument v) = return (Just . gShowType g $ gSerialType g, pretty v)
prepArg g (UnpackedArgument v t _) = return (Just . gShowType g $ t, pretty v)

-------- Utility and lookup functions ----------------------------------------

say :: Doc ann -> MorlocMonad ()
say d = liftIO . putDoc $ " : " <> d <> "\n"

-- | choose a packer or unpacker for a given type
selectFunction :: CType -> Property -> SerialMap -> Maybe (Name, Path)
selectFunction t p h =
  case mostSpecificSubtypes (typeOf t) (map typeOf (Map.keys hmap)) of
    [] -> Nothing
    -- FIXME: I should not have to filter here, the mostSpecificSubtypes
    -- function should NEVER return a type from a different language. However,
    -- it currently does. This needs to be fixed.
    xs -> case filter (\x -> langOf' x == langOf' t) xs of
      [] -> Nothing
      (x:_) -> Map.lookup (CType x) hmap
    where
      hmap = if p == Pack then packers h else unpackers h

unrollLambda :: Expr -> ([EVar], Expr)
unrollLambda (LamE v e2) = case unrollLambda e2 of
  (vs, e) -> (v:vs, e)
unrollLambda (AnnE (LamE v e2) _) = case unrollLambda e2 of
  (vs, e) -> (v:vs, e)
unrollLambda e = ([], e)

getGType :: [Type] -> MorlocMonad (Maybe GType)
getGType ts = case [GType t | t <- ts, langOf' t == MorlocLang] of
  [] -> return Nothing
  [x] -> return $ Just x
  xs -> MM.throwError . OtherError $
    "Expected 0 or 1 general types, found " <> MT.show' (length xs)

getCTypes :: [Type] -> [CType]
getCTypes ts = [CType t | t <- ts, isJust (langOf t)]

-- resolves a function into a list of types, for example:
-- ((Num->String)->[Num]->[String]) would resolve to the list
-- [(Num->String),[Num],[String]]. Any unsolved variables (i.e., that are still
-- universally qualified) will be stored as Nothing. Such variables will always
-- be passthrough arguments that have unknown type in the current language, but
-- will be passed on to one where they are defined.
typeArgsC :: CType -> [Maybe CType]
typeArgsC t = map (fmap ctype) (typeArgs [] (unCType t))

typeArgsG :: GType -> [Maybe GType]
typeArgsG t = map (fmap GType) (typeArgs [] (unGType t))

typeArgs :: [TVar] -> Type -> [Maybe Type]
typeArgs unsolved (FunT t1@(VarT v) t2)
  | elem v unsolved = Nothing : typeArgs unsolved t2
  | otherwise = Just t1 : typeArgs unsolved t2
typeArgs unsolved (FunT t1 t2) = Just t1 : typeArgs unsolved t2
typeArgs unsolved (Forall v t) = typeArgs (v:unsolved) t
typeArgs unsolved t@(VarT v)
  | elem v unsolved = [Nothing]
  | otherwise = [Just t]
typeArgs unsolved t = [Just t]

untypeArgs :: [Maybe CType] -> CType
untypeArgs xs = ctype . untypeArgs' . map (fmap unCType) $ xs
  where
    exclude = [v | (Just (CType (VarT (TV _ v)))) <- xs]
    vs = freshVarsAZ exclude

    lang = langOf . head . catMaybes $ xs

    untypeArgs' :: [Maybe Type] -> Type 
    untypeArgs' xs = case untypeArgs'' vs xs of
      (vs', _, t) -> generalize vs' t 

    generalize :: [MT.Text] -> Type -> Type
    generalize [] t = t
    generalize (v':vs') t = Forall (TV lang v') (generalize vs' t)

    untypeArgs'' _ [] = error "empty type"
    untypeArgs'' vs [Just t] = ([], vs, t)
    untypeArgs'' (v':vs') [Nothing] = ([v'], vs', VarT (TV lang v'))
    untypeArgs'' vs1 (t1:ts) = case untypeArgs'' vs1 [t1] of
      (v1', vs2, t1') -> case untypeArgs'' vs2 ts of
        (v2', vs3, t2') -> (v1' ++ v2', vs3, FunT t1' t2')

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
mapC f (SAnno (One (ForeignS i lang vs, c)) g) = SAnno (One (ForeignS i lang vs, f c)) g

descSExpr :: SExpr g f c -> MDoc
descSExpr (UniS) = "UniS"
descSExpr (VarS v) = "VarS" <+> pretty v
descSExpr (CallS src)
  =   "CallS"
  <+> pretty (srcAlias src) <+> "<" <> viaShow (srcLang src) <> ">"
descSExpr (ListS _) = "ListS"
descSExpr (TupleS _) = "TupleS"
descSExpr (LamS vs _) = "LamS" <+> hsep (map pretty vs)
descSExpr (AppS _ _) = "AppS"
descSExpr (NumS _) = "NumS"
descSExpr (LogS _) = "LogS"
descSExpr (StrS _) = "StrS"
descSExpr (RecS _) = "RecS"
descSExpr (ForeignS i lang vs) =
  parens (hsep ("ForeignS" : map pretty vs)) <+> pretty i <+> viaShow lang

concreteSignature :: Maybe EVar -> CType -> MDoc
concreteSignature n (CType t)
  = maybe "_" pretty n <+> viaShow (langOf' t) <+> "::" <+> prettyType t

generalSignature :: Maybe EVar -> GType -> MDoc
generalSignature n (GType t)
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

-- generate infinite list of fresh variables of form ['a','b',...,'z','aa','ab',...,'zz',...]
freshVarsAZ
  :: [MT.Text] -- variables to exclude
  -> [MT.Text]
freshVarsAZ exclude =
  filter
    (\x -> not (elem x exclude))
    ([1 ..] >>= flip replicateM ['a' .. 'z'] |>> MT.pack)

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
