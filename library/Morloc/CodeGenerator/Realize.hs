{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.CodeGenerator.Realize
Description : Select concrete implementations for each polymorphic call site
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

When a term has multiple candidate implementations (different languages,
different source files), this pass selects the best one at each call site
based on language affinity and minimizing cross-language transitions. The
result is a fully-realized tree where every node has exactly one
implementation.
-}
module Morloc.CodeGenerator.Realize
  ( realityCheck
  , removeVarS
  ) where

import Morloc.CodeGenerator.Serial (containsEffectT, containsFunT)
import Morloc.CodeGenerator.Value (etaParts, isValueWith)
import Morloc.Frontend.Namespace (newIndex)
import Morloc.Frontend.Rename (displayName)
import Morloc.CodeGenerator.Namespace
import qualified Morloc.CodeGenerator.SystemConfig as MCS
import Morloc.Data.Doc
import Morloc.Data.Map (Map)
import qualified Morloc.Data.Map as Map
import Data.IORef (modifyIORef, newIORef, readIORef)
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.TypeEval as TE
import qualified Morloc.LangRegistry as LR
import qualified Morloc.CodeGenerator.Infer as Infer

-- | One line per named call of a realized tree, its arguments and body
-- indented beneath it, with its language. @*@ marks a node in another
-- language than its parent: a crossing. An unnamed node is listed only
-- when it crosses, by its kind.
placementLines :: AnnoS (Indexed Type) One (Indexed Lang) -> [MT.Text]
placementLines = go 0 Nothing
  where
    go :: Int -> Maybe Lang -> AnnoS (Indexed Type) One (Indexed Lang) -> [MT.Text]
    go depth parent (AnnoS _ (Idx _ lang) e) = case e of
      AppS f xs | Just v <- callName f -> entry (name v) : below (definition f <> xs)
      VarS v (One body) -> entry (name v) : below (definition' body)
      ExeS (SrcCall src) -> [entry (name (srcAlias src))]
      CallS v -> [entry ("call " <> name v)]
      LetBndS v | moved -> [entry (name v)]
      BndS v | moved -> [entry (name v)]
      _ | moved -> entry ("(" <> kind e <> ")") : below (children e)
      _ -> concatMap (go depth (Just lang)) (children e)
      where
        moved = maybe False (/= lang) parent
        mark = if moved then " *" else ""
        entry x = MT.replicate depth "  " <> x <> " [" <> render (pretty lang) <> "]" <> mark
        below = concatMap (go (depth + 1) (Just lang))
        name = unEVar . displayName

    children = foldExprS (: [])

    kind :: ExprS g f c -> MT.Text
    kind e = case e of
      AppS {} -> "application"
      LamS {} -> "lambda"
      LstS {} -> "list"
      TupS {} -> "tuple"
      NamS {} -> "record"
      ConS _ n _ _ -> n
      LetS {} -> "let"
      IfS {} -> "if"
      DoBlockS {} -> "do"
      EvalS {} -> "eval"
      CoerceS {} -> "cast"
      IntrinsicS {} -> "intrinsic"
      LabelS {} -> "label"
      _ -> "value"

    callName :: AnnoS g One c -> Maybe EVar
    callName (AnnoS _ _ (VarS v _)) = Just v
    callName (AnnoS _ _ (ExeS (SrcCall src))) = Just (srcAlias src)
    callName (AnnoS _ _ (CallS v)) = Just (EV ("call " <> unEVar (displayName v)))
    callName _ = Nothing

    definition (AnnoS _ _ (VarS _ (One body))) = definition' body
    definition _ = []

    definition' (AnnoS _ _ (ExeS _)) = []
    definition' body = [body]

realityCheck ::
  -- | one AST forest for each command exported from main
  [AnnoS (Indexed Type) Many Int] ->
  MorlocMonad
    ( [AnnoS (Indexed Type) One ()]
    , [AnnoS (Indexed Type) One (Indexed Lang)]
    )
realityCheck es = do
  names <- MM.gets stateSpecNames
  let isSpec (AnnoS _ _ (VarS v _)) = Set.member v names
      isSpec _ = False
      (specs, roots) = List.partition isSpec es
  -- shared specializations first, each scored once, in the order their uses
  -- need them (a specialization's own shared references come before it)
  (tables, finishers) <- foldM scoreSpec (Map.empty, Map.empty) specs
  -- select a single instance at each node of each exported tree, and
  -- separate unrealized (general) trees from realized ones
  (gASTs0, rASTs00) <-
    mapM (\e -> realize tables e >>= \(_, finish) -> finish Nothing) roots
      |>> partitionEithers
  rASTs0 <- realizeDemanded finishers rASTs00
  let takesFunction = Set.fromList [v | AnnoS (Idx _ (FunT ts _)) _ (VarS v _) <- specs, any containsFunT ts]
  gASTs1 <- nexusDemanded finishers takesFunction gASTs0

  -- Extract non-exported recursive helpers into their own rASTs.
  -- This must happen before removeVarS so we can find the VarS wrappers.
  rASTs1 <- extractRecursiveHelpers rASTs0
  wanted <- MM.gets statePlacement
  case wanted of
    Just _ -> do
      let placement = concatMap placementLines rASTs1
      sum (map MT.length placement) `seq` MM.modify (\st -> st {statePlacement = Just placement})
    Nothing -> return ()

  -- Now dissolve remaining (non-recursive) VarS wrappers
  let gASTs = map removeVarS gASTs1
      rASTs = map (headInBody . removeVarS) rASTs1

  -- check and configure the system
  -- in the future, the results of this step may be used to winnow the build
  MCS.configure rASTs

  return (gASTs, rASTs)

type Scored = AnnoS (Indexed Type) Many (Indexed [(Lang, Score)])

-- | Finish realizing a scored tree, with the language of the context it is
-- used in (or none, for a root).
type Finisher =
  Maybe Lang ->
  MorlocMonad (Either (AnnoS (Indexed Type) One ()) (AnnoS (Indexed Type) One (Indexed Lang)))

-- | Score a shared specialization once: its table serves every use.
scoreSpec ::
  (Map EVar [(Lang, Score)], Map EVar Finisher) ->
  AnnoS (Indexed Type) Many Int ->
  MorlocMonad (Map EVar [(Lang, Score)], Map EVar Finisher)
scoreSpec (tables, finishers) spec@(AnnoS _ _ (VarS v _)) = do
  (AnnoS _ (Idx _ scores) _, finish) <- realize tables spec
  return (Map.insert v scores tables, Map.insert v finish finishers)
scoreSpec acc _ = return acc

-- | Realize each shared specialization once per language its uses demand,
-- and point each use at its copy. A use's language is the one its call was
-- given; the copy is named by the language its root lands in, so uses that
-- land alike share one copy. Copies are realized as their own uses are
-- found, until none is new (the specializations form no cycle).
realizeDemanded ::
  Map EVar Finisher ->
  [AnnoS (Indexed Type) One (Indexed Lang)] ->
  MorlocMonad [AnnoS (Indexed Type) One (Indexed Lang)]
realizeDemanded finishers trees0
  | Map.null finishers = return trees0
  | otherwise = do
      (named, copies) <- loop Map.empty Map.empty [] (concatMap demands trees0)
      return (map (renameUses named) (trees0 <> copies))
  where
    demands t = [(v, l) | AnnoS _ (Idx _ l) (CallS v) <- annoNodes t, Map.member v finishers]
    loop named _ copies [] = return (named, copies)
    loop named landed copies (d@(v, l) : ds)
      | Map.member d named = loop named landed copies ds
      | otherwise = case Map.lookup v finishers of
          Nothing -> loop named landed copies ds
          Just finish -> finish (Just l) >>= \r -> case r of
            Left _ -> MM.throwCompilerBug $ "a shared specialization has no language:" <+> pretty v
            Right tree@(AnnoS _ (Idx _ l') _) -> case Map.lookup (v, l') landed of
              Just name -> loop (Map.insert d name named) landed copies ds
              Nothing -> do
                let name = EV (unEVar v <> "@" <> langName l')
                    firstCopy = not (any ((== v) . fst) (Map.keys landed))
                copy@(AnnoS (Idx gi _) _ _) <- renameRoot name <$> if firstCopy then return tree else reindexOne tree
                MM.modify (\st -> st {stateRecursionTargets = Map.insert name gi (stateRecursionTargets st)})
                loop (Map.insert d name named) (Map.insert (v, l') name landed) (copies <> [copy]) (ds <> demands copy)
    renameUses named = go
      where
        go (AnnoS g c@(Idx _ l) (CallS v))
          | Just name <- Map.lookup (v, l) named = AnnoS g c (CallS name)
        go (AnnoS g c e) = AnnoS g c (mapExprS go e)

-- | The calls of shared specializations in trees the nexus evaluates. The
-- nexus calls a first-order one by name: its copy with no language joins
-- the trees as a named function ('stateNamedGasts'). The nexus holds no
-- function values, so one that takes a function is copied into each call
-- instead, as a term is expanded in place.
nexusDemanded ::
  Map EVar Finisher ->
  Set.Set EVar ->
  [AnnoS (Indexed Type) One ()] ->
  MorlocMonad [AnnoS (Indexed Type) One ()]
nexusDemanded finishers takesFunction trees0
  | Map.null finishers = return trees0
  | otherwise = do
      namedRef <- MM.liftIO (newIORef Map.empty)
      copiesRef <- MM.liftIO (newIORef [])
      finishedRef <- MM.liftIO (newIORef Map.empty)
      -- each specialization is finished once; every use takes a fresh copy
      let pure' v = do
            finished <- MM.liftIO (readIORef finishedRef)
            tree <- case (Map.lookup v finished, Map.lookup v finishers) of
              (Just g, _) -> return g
              (_, Nothing) -> MM.throwCompilerBug $ "no shared specialization" <+> pretty v
              (_, Just finish) -> finish Nothing >>= \r -> case r of
                Left g -> MM.liftIO (modifyIORef finishedRef (Map.insert v g)) >> return g
                Right _ -> MM.throwCompilerBug $ "a shared specialization called by the nexus has a language:" <+> pretty v
            reindexWith return tree
          rewrite (AnnoS g c e) = case e of
            CallS v
              | Map.member v finishers ->
                  if Set.member v takesFunction
                    then pure' v >>= rewrite . removeVarS
                    else do
                      done <- MM.liftIO (readIORef namedRef)
                      case Map.lookup v done of
                        Just name -> return (AnnoS g c (CallS name))
                        Nothing -> do
                          let name = EV (unEVar v <> "@nexus")
                          MM.liftIO (modifyIORef namedRef (Map.insert v name))
                          -- a copy of its own: a pool may hold another
                          copy@(AnnoS (Idx ri _) _ _) <- renameRoot name <$> (pure' v >>= rewrite)
                          MM.modify (\st -> st {stateNamedGasts = Map.insert ri name (stateNamedGasts st)})
                          MM.liftIO (modifyIORef copiesRef (<> [copy]))
                          return (AnnoS g c (CallS name))
            _ -> AnnoS g c <$> mapExprSM rewrite e
      trees <- mapM rewrite trees0
      copies <- MM.liftIO (readIORef copiesRef)
      return (trees <> copies)

-- | The realize objective (COST-1): estimated time, then the number of
-- crossings between pools, then the number of language changes within a
-- pool, compared lexicographically.
data Score = Score
  { scoreTime :: !Double
  , scoreCrossings :: !Int
  , scoreSwitches :: !Int
  }
  deriving (Eq, Ord, Show)

instance Semigroup Score where
  Score a b c <> Score x y z = Score (a + x) (b + y) (c + z)

instance Monoid Score where
  mempty = Score 0 0 0

-- | A score repeated @k@ times, saturating below the "no realization"
-- sentinel so that no placement's cost reaches it.
scaleScore :: Int -> Score -> Score
scaleScore k (Score t c w)
  | t >= 1e300 = Score t c w
  | otherwise = Score (min 1e290 (fromIntegral k * t)) (sat c) (sat w)
  where
    sat n = fromInteger (min (10 ^ (15 :: Int)) (toInteger k * toInteger n))

-- | How many times a recursive definition runs its body per call, when
-- nothing says otherwise (COST-14).
defaultRecursionCount :: Int
defaultRecursionCount = 1000

-- State for the realize scoring algorithm
data RState = RState
  { rLangs :: [Lang]
  -- | The arguments of the application whose head is being scored, each
  -- with its type and table, for the head's lambda to bind.
  , rApplied :: [(Type, [(Lang, Score)])]
  -- | A lambda parameter bound to a function value: the table of that value.
  , rBndVars :: Map EVar [(Lang, Score)]
  -- | A let-bound variable: the table of a reference to it when its value
  -- is or holds a function, 'Nothing' for data, whose references are free.
  , rLetVars :: Map EVar (Maybe [(Lang, Score)])
  }

emptyRState =
  RState
    { rLangs = []
    , rApplied = []
    , rBndVars = Map.empty
    , rLetVars = Map.empty
    }


-- | Beta-reduce literal-lambda-head applications, @(\\v -> body) arg@, on the
-- ambiguous @Many@ tree BEFORE scoring. Only applications whose head is a literal
-- 'LamS' are reduced (never a @VarS (Many ..)@ head, which may mix a reducible
-- lambda and an irreducible source call and so must wait for the Many->One
-- collapse, cf. 'Morloc.CodeGenerator.LambdaEval'). Reduction is restricted to
-- parameters used AT MOST ONCE, making substitution a pure move (no cloning, no
-- reindexing): this cannot bloat and needs no fresh indices. Multiply-used
-- redexes are left for the post-collapse 'applyLambdas'.
--
-- A point-free composition chain @id . id . ..@ desugars to nested
-- singly-used literal-lambda redexes that collapse here to a single
-- @\\x -> x@, so the scorer sees the chain's calls rather than its
-- plumbing.
normalizePop1 ::
  (EVar -> Maybe Int) ->
  AnnoS (Indexed Type) Many Int ->
  MorlocMonad (AnnoS (Indexed Type) Many Int)
-- empty lambda, no args left: unwrap
normalizePop1 sp (AnnoS g1 _ (AppS (AnnoS _ _ (LamS [] (AnnoS _ c2 e))) [])) =
  normalizePop1 sp (AnnoS g1 c2 e)
-- empty lambda with remaining args (over-application): unwrap the spent layer
normalizePop1 sp (AnnoS g1 c1 (AppS (AnnoS _ _ (LamS [] body)) es@(_ : _))) =
  normalizePop1 sp (AnnoS g1 c1 (AppS body es))
-- beta-reduce one parameter. A value used at most once is substituted (a pure
-- move). An argument that is not a value is evaluated once at the application,
-- so a data argument is bound by a let; a function- or suspension-typed one is
-- left for 'applyLambdas'.
normalizePop1 sp
  ( AnnoS
      i1@(Idx i1n i1t)
      tb1
      ( AppS
          (AnnoS (Idx i2 (FunT (tv : tas) tb2)) c (LamS (v : vs) e2))
          (e1 : es)
        )
    )
    | valueArg && countRefsMany v e2 <= 1 =
        normalizePop1 sp $ residual (substFreeBnd v e1 e2)
    | not valueArg && not (isFunT tv) && not (isEffT tv) = do
        e1' <- normalizePop1 sp e1
        letIx <- newIndex i1n
        body <- normalizePop1 sp (residual (rebindFreeBnd v e2))
        return (AnnoS (Idx letIx i1t) tb1 (LetS v e1' body))
  where
    valueArg = isValueWith sp e1
    residual body =
      AnnoS i1 tb1 $ AppS (AnnoS (Idx i2 (FunT tas tb2)) c (LamS vs body)) es
    isFunT (FunT _ _) = True
    isFunT _ = False
    -- a suspension's own arguments are bound by 'applyLambdas' before the
    -- suspension is shared ('hoistThunkArgs')
    isEffT (EffectT _ _) = True
    isEffT _ = False
-- application to no args: unwrap
normalizePop1 sp (AnnoS g1 _ (AppS (AnnoS _ c2 e) [])) =
  normalizePop1 sp (AnnoS g1 c2 e)
-- every other node (incl. multiply-used or Many-headed applications): recurse
normalizePop1 sp (AnnoS g c e) = AnnoS g c <$> mapExprSM (normalizePop1 sp) e

-- | Count free @BndS@/@LetBndS@ references to @v@, stopping at any binder that
-- shadows @v@. Mirrors 'Morloc.CodeGenerator.LambdaEval.countRefs' for the @Many@
-- tree so counting and 'substFreeBnd' agree on exactly which sites they touch.
countRefsMany :: EVar -> AnnoS (Indexed Type) Many Int -> Int
countRefsMany v = go
  where
    go (AnnoS _ _ (BndS v')) | v == v' = 1
    go (AnnoS _ _ (LetBndS v')) | v == v' = 1
    go (AnnoS _ _ (LamS vs _)) | v `elem` vs = 0
    go (AnnoS _ _ (LetS v' e1 _)) | v == v' = go e1
    go (AnnoS _ _ e) = getSum (foldExprS (Sum . go) e)

-- | Replace every free reference to @v@ with @r@, stopping at shadowing binders.
-- Callers guarantee at most one free occurrence, so this is a pure move (the
-- reference is replaced in place; nothing is cloned or reindexed).
substFreeBnd ::
  EVar ->
  AnnoS (Indexed Type) Many Int ->
  AnnoS (Indexed Type) Many Int ->
  AnnoS (Indexed Type) Many Int
substFreeBnd v r = go
  where
    go (AnnoS _ _ (BndS v')) | v == v' = r
    go (AnnoS _ _ (LetBndS v')) | v == v' = r
    go e0@(AnnoS _ _ (LamS vs _)) | v `elem` vs = e0
    -- a non-recursive let shadows v in its body, not in its right-hand side
    go (AnnoS g c (LetS v' e1 e2)) | v == v' = AnnoS g c (LetS v' (go e1) e2)
    go (AnnoS g c e) = AnnoS g c (mapExprS go e)

-- | Rebind every free lambda-bound reference to @v@ as a let-bound one, for a
-- parameter turned into a @let@.
rebindFreeBnd :: EVar -> AnnoS (Indexed Type) Many Int -> AnnoS (Indexed Type) Many Int
rebindFreeBnd v = go
  where
    go (AnnoS g c (BndS v')) | v == v' = AnnoS g c (LetBndS v)
    go e0@(AnnoS _ _ (LamS vs _)) | v `elem` vs = e0
    go (AnnoS g c (LetS v' e1 e2)) | v == v' = AnnoS g c (LetS v' (go e1) e2)
    go (AnnoS g c e) = AnnoS g c (mapExprS go e)

{- | Choose a single concrete implementation. In the future, this component
may be one of the more complex components of the morloc compiler. It will
probably need to be implemented using an optimizing SMT solver. It will
also need benchmarking data from all the implementations and possibly
statistical info describing inputs.
-}
realize ::
  Map EVar [(Lang, Score)] ->
  AnnoS (Indexed Type) Many Int ->
  MorlocMonad (Scored, Finisher)
realize tables s0 = do
  registry <- MM.gets stateLangRegistry
  -- A term that calls nothing sourced has no language of its own and is
  -- evaluated by the nexus. That evaluator has no name to call a function
  -- by -- it knows only the variables a lambda binds -- so a term that
  -- calls itself cannot run there and must be given a pool. The languages
  -- offered are those the program declares concrete types for, which are
  -- the pools it can build; one of them is chosen by the ordinary scoring,
  -- so the choice follows whatever else the term touches.
  --
  -- A replay entry that drives a saved stream frame by frame (@replay) needs
  -- a pool for the same reason: only a pool can call its sink per frame. Its
  -- command streams from a sourced producer, so the program has a pool.
  specNames <- MM.gets stateSpecNames
  let AnnoS (Idx i0 _) _ _ = s0
      selfCalling = anyCallS (not . (`Set.member` specNames)) s0
      replays = anyIntrinsicS IntrReplay s0
  langs <-
    if selfCalling || replays
      then do
        scopes <- MM.gets stateConcreteTypedefs
        case unique (map (LR.poolOf registry) (Map.keys scopes)) of
          []
            | selfCalling ->
                MM.throwSourcedError i0 $
                  "this function calls itself, and a function that calls itself needs a"
                    <+> "language to run in: the nexus evaluates an expression but cannot"
                    <+> "call a function by name. Import a language module (`import"
                    <+> "root-py`, `root-cpp`, ...) to give the program a pool."
            | otherwise ->
                MM.throwCompilerBugAt i0 "a replay entry in a program with no pool language"
          ls -> return ls
      else return []
  realizeWithRegistry registry tables langs s0

-- | Score a tree, returning the scored tree and how to finish it: a shared
-- specialization is scored once and finished once per language it is used in.
-- @tables@ holds the score of each shared specialization a call may name.
realizeWithRegistry ::
  LangRegistry ->
  Map EVar [(Lang, Score)] ->
  [Lang] ->
  AnnoS (Indexed Type) Many Int ->
  MorlocMonad (Scored, Finisher)
realizeWithRegistry registry tables seedLangs s0 = do
  -- Normalize language-invariant (literal-lambda-head) redexes before scoring.
  let sp = stagePoints s0
  s0' <- normalizePop1 (`Map.lookup` sp) s0
  scored <- scoreAnnoS emptyRState {rLangs = seedLangs} s0'
  return (scored, finish scored)
  where
    finish scored ctx = do
      e@(AnnoS g li x) <- collapseAnnoS [] ctx scored
      case (li, ctx) of
        -- language-free code used from a pool runs in that pool, as it
        -- would inlined there
        (Idx i Nothing, Just l) -> propagateDown (AnnoS g (Idx i (Just l)) x) |>> Right
        (Idx _ Nothing, Nothing) -> makeGAST e |>> Left
        _ -> propagateDown e |>> Right

    pairwiseCost :: Lang -> Lang -> Int
    pairwiseCost l1 l2
      | l1 == l2 = case Map.lookup (langName l2) (lrSameLangCosts registry) of
          Nothing -> lrDefaultSameCost registry
          (Just score) -> score
      | otherwise = case Map.lookup (langName l1, langName l2) (lrOptimizedPairs registry) of
          Nothing -> case Map.lookup (langName l2) (lrCrossLangCosts registry) of
            Nothing -> lrDefaultCrossCost registry
            (Just score) -> score
          (Just score) -> score

    languageCost :: Lang -> Int
    languageCost lang = pairwiseCost lang lang

    -- \| Depth first pass calculating scores for each language. Alternates with
    -- scoresSExpr.
    scoreAnnoS ::
      RState ->
      AnnoS (Indexed Type) Many Int ->
      MorlocMonad (AnnoS (Indexed Type) Many (Indexed [(Lang, Score)]))
    scoreAnnoS rstat0 (AnnoS gi@(Idx gidx t) ci e) = do
      -- The arguments of an application reach only the lambdas its head
      -- binds them with ('boundArgs' follows the same chain).
      let rstat = if passesApplied e then rstat0 else rstat0 {rApplied = []}
      (e', Idx ci' table) <- scoreExpr rstat (e, ci)
      -- An operation that belongs to no language runs where its context
      -- puts it, but only in a language that can hold what it computes.
      table' <- maybe (return table) (\ts -> holding gidx ts table) (heldTypes e t)
      return $ AnnoS gi (Idx ci' table') e'

    -- The languages of @table@ that can hold values of types @ts@ (codegen
    -- finds each a concrete type there). When none of them can, the
    -- program's other pool languages that can are offered instead, each at
    -- the table's cost plus the crossing to it. A table with no language
    -- (a tree the nexus evaluates) is left alone, and so is one where no
    -- pool language can hold the value: its error is reported where the
    -- value is written out.
    holding :: Int -> [Type] -> [(Lang, Score)] -> MorlocMonad [(Lang, Score)]
    holding _ _ [] = return []
    holding i ts table = do
      able <- ableLangs i ts (map fst table)
      return $ case (able, [x | x@(l, _) <- table, l `elem` able]) of
        ([], _) -> table
        (_, []) -> [(l, reachFrom l table) | l <- able]
        (_, kept) -> kept

    -- The language a language-free computation placed in context @l@ runs
    -- in: @l@ if it can hold the values the computation works on, else the
    -- pool language that can, cheapest to reach from @l@; @l@ if none can.
    holdingLang :: Int -> [Type] -> Lang -> MorlocMonad Lang
    holdingLang i ts l = do
      able <- ableLangs i ts [l]
      return $ case able of
        _ | l `elem` able -> l
        [] -> l
        _ -> snd (minimum [(boundary l l', l') | l' <- able])

    -- Of these languages and the program's pool languages, those in which
    -- codegen finds a concrete type for each of @ts@.
    ableLangs :: Int -> [Type] -> [Lang] -> MorlocMonad [Lang]
    ableLangs i ts langs = do
      scopes <- MM.gets stateConcreteTypedefs
      let candidates = unique (langs <> map (LR.poolOf registry) (Map.keys scopes))
      filterM (\l -> and <$> mapM (Infer.canHoldType True l i) ts) candidates

    -- The types a language-free computation works on, for one that is.
    heldTypes :: ExprS (Indexed Type) f c -> Type -> Maybe [Type]
    heldTypes e t = case e of
      ExeS (PatCall _) -> Just (componentTypes t)
      AppS (AnnoS (Idx _ ft) _ (ExeS (PatCall _))) _ -> Just (componentTypes ft)
      -- a call of a function value runs wherever it is placed
      AppS (AnnoS (Idx _ ft) _ (BndS _)) _ -> Just (componentTypes ft)
      AppS (AnnoS (Idx _ ft) _ (LetBndS _)) _ -> Just (componentTypes ft)
      LstS _ -> Just [t]
      TupS _ -> Just [t]
      NamS _ -> Just [t]
      _ -> Nothing

    -- what an operation of this type computes on: its inputs and result
    componentTypes :: Type -> [Type]
    componentTypes (FunT ins out) = ins <> [out]
    componentTypes ty = [ty]

    -- \| Alternates with scoresAnnoS, finds the best score for each language at
    -- application nodes.
    scoreExpr ::
      RState ->
      (ExprS (Indexed Type) Many Int, Int) ->
      MorlocMonad (ExprS (Indexed Type) Many (Indexed [(Lang, Score)]), Indexed [(Lang, Score)])
    scoreExpr rstat (LstS xs, i) = do
      (xs', best) <- scoreComponents rstat xs
      return (LstS xs', orAmbient i rstat best)
    scoreExpr rstat (TupS xs, i) = do
      (xs', best) <- scoreComponents rstat xs
      return (TupS xs', orAmbient i rstat best)
    scoreExpr rstat (NamS rs, i) = do
      (xs, best) <- scoreComponents rstat (map snd rs)
      return (NamS (zip (map fst rs) xs), orAmbient i rstat best)
    scoreExpr rstat (LamS vs x, i) = do
      x' <- scoreAnnoS (updateRState vs rstat) x
      return (LamS vs x', Idx i (scoresOf x'))
    -- Each argument is scored once, before the head, so a lambda the head
    -- binds it to refers to its table rather than scoring it again at every
    -- use. A function argument the head's lambda binds is priced where the
    -- body calls it (COST-4); every other argument is priced here. A lambda
    -- handed to a function is built in that function's language, so the
    -- arguments are scored in the head's languages when they are known (its
    -- sources, or the function value a parameter or let holds): a call
    -- there on a parameter the host supplies stays with the host.
    scoreExpr rstat (AppS f xs, i) = do
      let argLangs = case f of
            AnnoS _ _ (BndS v) | Just table <- Map.lookup v (rBndVars rstat) -> map fst table
            AnnoS _ _ (LetBndS v) | Just (Just table) <- Map.lookup v (rLetVars rstat) -> map fst table
            _ -> sourceLangs f
      xs' <- mapM (scoreAnnoS rstat {rLangs = argLangs, rApplied = []}) xs
      let args = [(t, scoresOf x') | x'@(AnnoS (Idx _ t) _ _) <- xs']
          bound = boundArgs f
          pairss = [a | (k, a@(t, _)) <- zip [0 ..] args, not (k < bound && isCallableType t)]
      -- A head has no ambient language: it is placed by its own
      -- implementations. One with none (a call on a function parameter
      -- bound to no table, a recursive call) lives where the expression
      -- holding it lives.
      f' <- scoreAnnoS rstat {rLangs = [], rApplied = args} f
      let headTable = case scoresOf f' of
            [] -> [(l, mempty) | l <- rLangs rstat]
            table -> table
      return (AppS f' xs', Idx i (scoreApp headTable pairss))
    -- non-recursive expressions
    scoreExpr rstat (UniS, i) = return (UniS, zipLang i rstat)
    scoreExpr rstat (NullS, i) = return (NullS, zipLang i rstat)
    scoreExpr rstat (VarS v (Many xs), i) = do
      (xs', best) <- scoreAlternatives v rstat xs
      return (VarS v (Many xs'), Idx i best)
    -- A parameter bound to no function value is data the caller passes
    -- in, read wherever it is used (COST-3).
    scoreExpr rstat (BndS v, i) = do
      case Map.lookup v (rBndVars rstat) of
        Just scores -> return (BndS v, Idx i scores)
        Nothing -> return (BndS v, Idx i [])
    scoreExpr _ (ExeS x@(SrcCall src), i) = return (ExeS x, Idx i [(srcLang src, Score (callCost src) 0 0)])
    scoreExpr rstat (ExeS x@(PatCall _), i) = return (ExeS x, zipLang i rstat)
    scoreExpr rstat (RealS si x, i) = return (RealS si x, zipLang i rstat)
    scoreExpr rstat (IntS si x, i) = return (IntS si x, zipLang i rstat)
    scoreExpr rstat (LogS x, i) = return (LogS x, zipLang i rstat)
    scoreExpr rstat (StrS x, i) = return (StrS x, zipLang i rstat)
    scoreExpr rstat (ConS tv n j xs, i) = do
      (xs', best) <- scoreComponents rstat xs
      return (ConS tv n j xs', orAmbient i rstat best)
    scoreExpr rstat (LetS v e1 e2, i) = do
      e1' <- scoreAnnoS rstat {rApplied = []} e1
      -- A data value is read where its let runs, or carried into a subtree
      -- in another pool as one of the free variables that subtree's single
      -- boundary passes (COST-3), so a reference to it adds nothing of its
      -- own. A function value is priced by where its definition can run:
      -- its cheapest home relative to the cheapest place overall, plus
      -- reaching it; the RHS's own cost is counted once, at the LetS below.
      let rhs = scoresOf e1'
          refTable
            | null rhs = []
            | otherwise =
                let base = minimum (map snd rhs)
                    langs = unique (map fst rhs <> rLangs rstat)
                    above (Score t c w) (Score bt bc bw) = Score (t - bt) (max 0 (c - bc)) (max 0 (w - bw))
                 in [ (l, minimum [above sc base <> boundary h l | (h, sc) <- rhs])
                    | l <- langs
                    ]
          ref = if holdsCallable (annoType e1') then Just refTable else Nothing
          rstat' = rstat { rLetVars = Map.insert v ref (rLetVars rstat) }
      e2' <- scoreAnnoS rstat' e2
      -- Score the chain like an application: the binding's RHS and the body
      -- both contribute, with cross-language penalties applied per-lang.
      let best = scoreApp [] [ (annoType e1', minPairs (scoresOf e1'))
                             , (annoType e2', minPairs (scoresOf e2'))
                             ]
      return (LetS v e1' e2', Idx i best)
    scoreExpr rstat (LetBndS v, i) =
      case Map.lookup v (rLetVars rstat) of
        Just Nothing -> return (LetBndS v, Idx i [])
        Just (Just scs@(_ : _)) -> return (LetBndS v, Idx i scs)
        _ -> return (LetBndS v, zipLang i rstat)
    scoreExpr rstat (CallS v, i) = case Map.lookup v tables of
      -- a shared specialization costs what it costs in each language
      Just table -> return (CallS v, Idx i table)
      Nothing -> return (CallS v, zipLang i rstat)
    scoreExpr rstat (IfS c t e, i) = do
      c' <- scoreAnnoS rstat c
      t' <- scoreAnnoS rstat t
      e' <- scoreAnnoS rstat e
      -- Combine the condition and both branches like an application (each
      -- sub-result must be produced in the chosen language, with cross-language
      -- penalties), NOT a per-language min across all three concatenated: a bare
      -- 'minPairs' lets a language-agnostic branch (e.g. a base case that just
      -- returns a parameter, score 0 in every language) mask the real cost of a
      -- heavy branch (e.g. a cross-pool recursive loop body), collapsing the
      -- manifold's language choice to an arbitrary tiebreak. Mirrors 'LetS'.
      let best = scoreApp [] [ (annoType c', minPairs (scoresOf c'))
                             , (annoType t', minPairs (scoresOf t'))
                             , (annoType e', minPairs (scoresOf e'))
                             ]
      return (IfS c' t' e', Idx i best)
    scoreExpr rstat (DoBlockS x, i) = do
      x' <- scoreAnnoS rstat x
      return (DoBlockS x', Idx i (scoresOf x'))
    scoreExpr rstat (EvalS x, i) = do
      x' <- scoreAnnoS rstat x
      return (EvalS x', Idx i (scoresOf x'))
    scoreExpr rstat (CoerceS c@(CoerceNewtype dir target) x, i) = do
      x' <- scoreAnnoS rstat x
      let childScores = scoresOf x'
      (candidates, allowed) <- castLangs (castHead dir target (annoType x')) (rLangs rstat <> map fst childScores)
      let best = case allowed of
            [] -> [(l, noScore) | l <- candidates]
            _ -> scoreApp [(l, mempty) | l <- allowed] [(annoType x', childScores)]
      return (CoerceS c x', Idx i best)
    scoreExpr rstat (CoerceS c x, i) = do
      x' <- scoreAnnoS rstat x
      return (CoerceS c x', Idx i (scoresOf x'))
    scoreExpr rstat (LabelS k x, i) = do
      x' <- scoreAnnoS rstat x
      return (LabelS k x', Idx i (scoresOf x'))
    scoreExpr rstat (IntrinsicS intr xs, i) = do
      xs' <- mapM (scoreAnnoS rstat) xs
      let Idx _ langScores = zipLang i rstat
          best = case xs' of
            [] -> langScores
            _ -> scoreApp [] [(annoType x', minPairs (scoresOf x')) | x' <- xs']
      return (IntrinsicS intr xs', Idx i best)

    -- calculate the score for an application based on the score of the function
    -- and the scores of the arguments
    scoreApp ::
      [ ( Lang -- the language of the ith calling function implementation
        , Score -- the score of the ith implementation
        )
      ] ->
      [ ( Type -- the type of the kth argument, priced by 'crossingPayloadCost'
        , [ ( Lang -- the language of the jth implementation of the kth argument
            , Score -- the score of the jth implementation of the kth argument
            )
          ]
        )
      ] ->
      [(Lang, Score)]
    -- if nothing is known, nothing is returned
    scoreApp [] (concatMap snd -> []) = []
    -- if none of the arguments are language-specific, the scores are based only
    -- on the functions
    scoreApp scores (concatMap snd -> []) = scores
    -- if the function is not language-specific, calculate the cost of calling
    -- all arguments from each possible language context
    scoreApp [] pairss =
      let score = [(lang, mempty) | lang <- unique $ map fst (concatMap snd pairss)]
       in scoreApp score pairss
    -- if arguments and function have implementations, calculate cost relative to
    -- each function implementation
    scoreApp scores pairss =
      -- An argument with no candidates carries no language information, so
      -- it costs nothing and constrains nothing. It is NOT unrealizable: the
      -- clause above says the same of a head with no candidates.
      [ (l1, s1 <> mconcat [reachWith (crossScore t) l1 pairs | (t, pairs) <- pairss, not (null pairs)])
      | (l1, s1) <- scores
      ]

    updateRState :: [EVar] -> RState -> RState
    updateRState [] rstat = rstat
    updateRState vs rstat@(RState _ [] bound _) =
      rstat {rBndVars = foldr Map.delete bound vs}
    updateRState (v : vs) rstat@(RState _ ((t, table) : ps) bound _) =
      updateRState vs $
        rstat
          { rApplied = ps
          , rBndVars = if isCallableType t then Map.insert v table bound else Map.delete v bound
          }

    -- How many leading arguments of an application its head's lambda binds:
    -- the fewest any implementation binds, a sourced one binding none.
    boundArgs :: AnnoS (Indexed Type) Many c -> Int
    boundArgs (AnnoS _ _ e) = case e of
      LamS vs body -> length vs + boundArgs body
      VarS _ (Many ys@(_ : _)) -> minimum (map boundArgs ys)
      LetS _ _ body -> boundArgs body
      LabelS _ x -> boundArgs x
      CoerceS _ x -> boundArgs x
      _ -> 0

    -- The nodes through which an application's arguments reach the lambdas
    -- of its head.
    passesApplied :: ExprS g f c -> Bool
    passesApplied e = case e of
      LamS {} -> True
      VarS {} -> True
      LetS {} -> True
      LabelS {} -> True
      CoerceS {} -> True
      _ -> False

    -- A table, or the ambient languages for a node that has none.
    orAmbient :: Int -> RState -> [(Lang, Score)] -> Indexed [(Lang, Score)]
    orAmbient i rstat [] = zipLang i rstat
    orAmbient i _ table = Idx i table

    -- The languages a head is sourced in.
    sourceLangs :: AnnoS (Indexed Type) Many c -> [Lang]
    sourceLangs (AnnoS _ _ e) = case e of
      ExeS (SrcCall src) -> [srcLang src]
      VarS _ (Many ys) -> unique (concatMap sourceLangs ys)
      LabelS _ x -> sourceLangs x
      CoerceS _ x -> sourceLangs x
      _ -> []

    zipLang :: Int -> RState -> Indexed [(Lang, Score)]
    zipLang i (rLangs -> langs) = Idx i (zip langs (repeat mempty))

    scoresOf :: AnnoS a Many (Indexed [(Lang, Score)]) -> [(Lang, Score)]
    scoresOf (AnnoS _ (Idx _ xs) _) = minPairs xs

    -- The implementations of term @v@: one is chosen, so each language costs
    -- its cheapest implementation reached from there. A recursive
    -- implementation runs its body many times per call (COST-14).
    scoreAlternatives ::
      EVar ->
      RState ->
      [AnnoS (Indexed Type) Many Int] ->
      MorlocMonad ([AnnoS (Indexed Type) Many (Indexed [(Lang, Score)])], [(Lang, Score)])
    scoreAlternatives v rstat xs0 = do
      xs1 <- mapM (fmap repeated . scoreAnnoS rstat) xs0
      let pairs = minPairs (concat [xs' | (AnnoS _ (Idx _ xs') _) <- xs1])
      return (xs1, [(l1, reachFrom l1 pairs) | l1 <- unique (map fst pairs)])
      where
        repeated x@(AnnoS g (Idx j ss) e)
          | containsCallS v x = AnnoS g (Idx j [(l, scaleScore defaultRecursionCount sc) | (l, sc) <- ss]) e
          | otherwise = x

    -- The components of a list, tuple or record: all of them are built, so
    -- each is priced like an argument (COST-11).
    scoreComponents ::
      RState ->
      [AnnoS (Indexed Type) Many Int] ->
      MorlocMonad ([AnnoS (Indexed Type) Many (Indexed [(Lang, Score)])], [(Lang, Score)])
    scoreComponents rstat xs0 = do
      xs1 <- mapM (scoreAnnoS rstat) xs0
      let pairss = [(annoType x, scoresOf x) | x <- xs1]
          langs' = unique (concatMap (map fst . snd) pairss)
      return (xs1, scoreApp [(l, mempty) | l <- langs'] pairss)

    -- The cheapest way to have a value from @table@ in language @l@.
    reachFrom :: Lang -> [(Lang, Score)] -> Score
    reachFrom = reachWith boundary

    reachWith :: (Lang -> Lang -> Score) -> Lang -> [(Lang, Score)] -> Score
    reachWith edge l table = minimum [sc <> edge l l2 | (l2, sc) <- table]

    -- The enclosing recursive heads, outermost first, each with the
    -- language it was placed in. A node with a back-edge to one of them
    -- beneath it is placed in that head's language (see 'pickLanguage').
    collapseAnnoS ::
      [(EVar, Lang)] ->
      Maybe Lang ->
      AnnoS (Indexed Type) Many (Indexed [(Lang, Score)]) ->
      MorlocMonad (AnnoS (Indexed Type) One (Indexed (Maybe Lang)))
    collapseAnnoS = collapseNode True

    -- The head of an application is not a value: a lambda there is applied
    -- in place, so a named one keeps its own choice.
    collapseHead ::
      [(EVar, Lang)] ->
      Maybe Lang ->
      AnnoS (Indexed Type) Many (Indexed [(Lang, Score)]) ->
      MorlocMonad (AnnoS (Indexed Type) One (Indexed (Maybe Lang)))
    collapseHead = collapseNode False

    collapseNode ::
      Bool ->
      [(EVar, Lang)] ->
      Maybe Lang ->
      AnnoS (Indexed Type) Many (Indexed [(Lang, Score)]) ->
      MorlocMonad (AnnoS (Indexed Type) One (Indexed (Maybe Lang)))
    collapseNode isValue heads l1 (AnnoS gi@(Idx gidx gt) ci0 e0) = do
      l1' <- case (l1, heldTypes e0 gt) of
        (Just l, Just ts) -> Just <$> holdingLang gidx ts l
        _ -> return l1
      let AnnoS _ ci e = case l1' of
            Just l | isValue -> pinNamedLambda l (AnnoS gi ci0 e0)
            _ -> AnnoS gi ci0 e0
      (e', ci'') <- collapseExpr heads gt l1' (e, ci)
      -- moved away from its context, a language-free computation keeps the
      -- language it was moved to rather than inheriting its context's again
      let ci' = case ci'' of
            Idx i Nothing | l1' /= l1 -> Idx i l1'
            _ -> ci''
      -- Explainability: at high verbosity, record each node's language decision
      -- (its index, the parent/incoming language, and the language chosen). The
      -- exact tree-DP makes this a faithful, per-node account of "why this pool".
      let Idx idx mlang = ci'
      MM.sayVVV $
        "realize: node" <+> pretty idx
          <+> ("parent=" <> maybe "general" pretty l1)
          <+> ("chose=" <> maybe "general" pretty mlang)
          <+> ("table=" <> tableDoc ci0)
      return (AnnoS gi ci' e')

    tableDoc :: Indexed [(Lang, Score)] -> MDoc
    tableDoc (Idx _ ss) =
      list [pretty l <> ":" <> viaShow (scoreTime sc) <> "/" <> pretty (scoreCrossings sc) <> "/" <> pretty (scoreSwitches sc) | (l, sc) <- ss]

    -- The newtype a cast converts: the argument's type when unwrapping, the
    -- result's when wrapping.
    castHead :: CastDir -> TypeU -> Type -> TVar
    castHead Unwrap _ argT = extractKey (type2typeu argT)
    castHead Wrap target _ = extractKey target

    -- These languages with the program's pool languages, and those of them
    -- where a cast of newtype @n@ changes no value: @n@ declares no form
    -- there (NEWT-7).
    castLangs :: TVar -> [Lang] -> MorlocMonad ([Lang], [Lang])
    castLangs n langs = do
      scopes <- MM.gets stateConcreteTypedefs
      let candidates = unique (langs <> map (LR.poolOf registry) (Map.keys scopes))
      allowed <- filterM (\l -> not . Map.member n <$> MM.getConcreteScope l) candidates
      return (candidates, allowed)

    -- Sentinel "no realization" score, above any cost a placement can reach
    -- ('scaleScore' saturates below it).
    noScore :: Score
    noScore = Score 1e300 0 0

    -- | The general type an annotated expression carries.
    annoType :: AnnoS (Indexed Type) f c -> Type
    annoType (AnnoS (Idx _ t) _ _) = t

    -- | What it costs to reach a child placed in @l2@ from a parent in @l1@
    -- (COST-3): one crossing into @l2@'s pool, or nothing within one pool,
    -- where a change of language only counts as a switch.
    boundary :: Lang -> Lang -> Score
    boundary l1 l2
      | l1 == l2 = mempty
      | LR.coLocated registry l1 l2 = Score 0 0 1
      | otherwise = Score (fromIntegral (pairwiseCost l1 l2)) 1 0

    -- | What it costs to move a value of this type across a pool boundary,
    -- added to the cost of the language pair itself. Zero within a pool,
    -- where nothing is moved.
    crossScore :: Type -> Lang -> Lang -> Score
    crossScore t l1 l2
      | LR.coLocated registry l1 l2 = boundary l1 l2
      | otherwise = boundary l1 l2 <> Score (crossingPayloadCost t) 0 0

    -- | THE EXTENSION POINT for pricing a crossing by WHAT is moved rather
    -- than only by where it is going.
    --
    -- It answers zero for every type, which is exactly the historical
    -- behaviour: a crossing costs what the language pair costs, whether it
    -- carries a byte or a gigabyte. Every alternative is a research question,
    -- so the choice is named and given one home rather than being spread
    -- through the scorer.
    --
    -- Three types are mispriced by the flat answer, in rough order of how
    -- badly:
    --
    --   * A FUNCTION value is not moved at all. It is replaced by a proxy
    --     that calls home on every application, so its real cost is
    --     (number of applications) x (round trip) -- unbounded, and not
    --     knowable from the type. Anything finite here understates it.
    --   * A variable-width aggregate costs its serialized size, which is a
    --     property of the value rather than of the type. A type-level answer
    --     can only be a prior on size.
    --   * A fixed-width primitive is very nearly free, and is the one case
    --     the flat answer overcharges relative to the others.
    --
    -- The smallest useful refinement is a constant: zero for primitives and
    -- fixed-width containers of them, one for everything else. Richer answers
    -- (size priors from a type, amortizing a function value over an estimated
    -- call count, letting a backend declare its own table) all fit here
    -- without touching the scorer.
    --
    -- Any refinement must remain a pure function of the type: this runs before
    -- values exist. The type given is the general type at the crossing point,
    -- already monomorphized, so a cost may depend on type parameters.
    crossingPayloadCost :: Type -> Double
    crossingPayloadCost _ = 0

    cost ::
      Maybe Lang -> -- parent language (if given)
      Lang -> -- child lang (should always be given if we are working from scored pairs)
      Score -> -- score
      Score
    cost (Just l1) l2 score = score <> boundary l1 l2
    cost _ _ score = score

    -- FIXME: in the future, this function should be replaced by an estimate of
    -- the function runtime, for now I will just base it off languages.
    callCost :: Source -> Double
    callCost src = fromIntegral (languageCost (srcLang src))

    collapseExpr ::
      [(EVar, Lang)] ->
      Type ->
      Maybe Lang -> -- the language of the parent expression (if Nothing, then this is a GAST)
      (ExprS (Indexed Type) Many (Indexed [(Lang, Score)]), Indexed [(Lang, Score)]) ->
      MorlocMonad (ExprS (Indexed Type) One (Indexed (Maybe Lang)), Indexed (Maybe Lang))

    collapseExpr _ _ _ (VarS v (Many []), Idx i _) =
      MM.throwSourcedError i $ "No implementation found for" <+> squotes (pretty (displayName v))
    -- Select one implementation for the given term
    collapseExpr heads gt l1 (VarS v (Many xs), Idx i _) = do
      let minXs = minsBy (\(AnnoS _ (Idx _ ss) _) -> minimumMay [cost l1 l2 s | (l2, s) <- ss]) xs
      (x, lang) <- case minXs of
        [] -> MM.throwSourcedError i $ "No implementation found for" <+> squotes (pretty (displayName v))
        [x] -> handleOne x
        choices ->
          resolveInstances gt choices >>= \case
            (y : _) -> handleOne y
            [] -> MM.throwSourcedError i $ "No implementation found for" <+> squotes (pretty (displayName v))
      return (VarS v (One x), Idx i lang)
      where
        handleOne ::
          AnnoS (Indexed Type) Many (Indexed [(Lang, Score)]) ->
          MorlocMonad (AnnoS (Indexed Type) One (Indexed (Maybe Lang)), Maybe Lang)
        handleOne x@(AnnoS _ (Idx _ ss) e) = do
          newLang <-
            if isFunctionalData e
              then return l1
              else pickLanguage heads l1 [x] ss
          -- A term whose expansion refers back to itself is a recursive
          -- head: everything beneath it down to the back-edge follows its
          -- language.
          let heads' = case newLang of
                Just l | containsCallS v x -> heads ++ [(v, l)]
                _ -> heads
          x' <- collapseAnnoS heads' newLang x
          return (x', newLang)

        -- The implementations whose type matches the use best: the most
        -- specific instance, at the nearest link of the use's alias chain.
        resolveInstances ::
          Type ->
          [AnnoS (Indexed Type) Many (Indexed [(Lang, Score)])] ->
          MorlocMonad [AnnoS (Indexed Type) Many (Indexed [(Lang, Score)])]
        resolveInstances gt' xs' = do
          gscope <- MM.getGeneralScope
          -- gt' followed by each step of its alias chain toward the root
          -- type (C -> B -> A -> Str). A candidate matches at the first
          -- link its head is compatible with, so an instance on a nearer
          -- alias is preferred over one on a farther alias, and a sibling
          -- alias never matches at all.
          let chain = gt' : List.unfoldr (fmap (\t -> (t, t)) . reduceStep gscope) gt'
              matched =
                [ (lvl, type2typeu t, x)
                | x@(AnnoS (Idx _ t) _ _) <- xs'
                , Just lvl <- [List.findIndex (`compatibleType` t) chain]
                ]
              -- Instance heads are ordered by subsumption (UnkT lowers to
              -- a per-leaf forall), so a well-formed set of matches has a
              -- unique greatest element: the most specific instance. Heads
              -- that tie on specificity (distinct aliases of one type) are
              -- separated by chain position; incomparable maxima mean the
              -- heads overlap without either specializing the other.
              maxima = mostSpecific [h | (_, h, _) <- matched]
              best = [(lvl, h, x) | (lvl, h, x) <- matched, any (equivalent h) maxima]
              nearest = [(h, x) | (lvl, h, x) <- best, lvl == minimum [l | (l, _, _) <- best]]
          case nearest of
            [] ->
              MM.throwSourcedError i $
                "No matching implementation found for" <+> squotes (pretty (displayName v))
                  <+> "at type" <+> pretty gt'
            -- one head, possibly sourced several times (duplicate imports)
            ((h0, _) : rest) | all (equivalent h0 . fst) rest -> return (map snd nearest)
            -- The use site does not fix the type, so the overlap cannot be
            -- judged here. A generic export is dropped before code
            -- generation, so the pick below is never observed there.
            _ | containsUnk gt' -> return (map snd nearest)
            _ ->
              MM.throwSourcedError i $
                "Ambiguous instances for" <+> squotes (pretty (displayName v))
                  <+> "at type" <+> pretty gt' <> ":"
                  <> "\n" <> indent 2 (vsep (map pretty maxima))
                  <> "\nEach matches, and none is more specific than the others."
                  <> "\nDeclare an instance for their common specialization to"
                  <+> "disambiguate, or remove one of them."

        -- One alias-reduction step: the top-level constructor first, then
        -- (when aliases sit inside a compound type, e.g. Celsius -> Celsius
        -- with type Celsius = Int) the leaves.
        reduceStep :: Scope -> Type -> Maybe Type
        reduceStep gscope t =
          typeOf <$> (TE.reduceType gscope (type2typeu t) <|> TE.reduceTypeLeaves gscope (type2typeu t))

        -- Structural type equality with UnkT as wildcard.
        -- UnkT arises from unsolved generics (e.g., mempty :: List (UnkT a)).
        -- This allows List Int to match List (UnkT a) without matching
        -- Deque (UnkT a) or accepting sibling aliases in function types.
        compatibleType :: Type -> Type -> Bool
        compatibleType (UnkT _) _ = True
        compatibleType _ (UnkT _) = True
        compatibleType (VarT v1) (VarT v2) = v1 == v2
        compatibleType (FunT as1 r1) (FunT as2 r2) =
          length as1 == length as2
            && all (uncurry compatibleType) (zip as1 as2)
            && compatibleType r1 r2
        compatibleType (AppT h1 ps1) (AppT h2 ps2) =
          compatibleType h1 h2
            && length ps1 == length ps2
            && all (uncurry compatibleType) (zip ps1 ps2)
        compatibleType (NamT o1 n1 ps1 rs1) (NamT o2 n2 ps2 rs2) =
          o1 == o2 && n1 == n2
            && length ps1 == length ps2
            && all (uncurry compatibleType) (zip ps1 ps2)
            && length rs1 == length rs2
            && all (\((k1,v1),(k2,v2)) -> k1 == k2 && compatibleType v1 v2) (zip rs1 rs2)
        compatibleType (OptionalT t1) (OptionalT t2) = compatibleType t1 t2
        compatibleType (EffectT e1 t1) (EffectT e2 t2) = e1 == e2 && compatibleType t1 t2
        compatibleType (NatLitT n1) (NatLitT n2) = n1 == n2
        -- NatVoidT is wildcard-compatible with any Nat-shaped value
        compatibleType NatVoidT (NatLitT _) = True
        compatibleType (NatLitT _) NatVoidT = True
        compatibleType NatVoidT NatVoidT = True
        compatibleType t1 t2 = t1 == t2

    ----- NOTE: Some cases are inseperable, the code above does not
    ----- account for this, which may allow incorrect code to be
    ----- generated.
    -- xs' ->  MM.throwSystemError
    --   $ "no rule to separate the following sourced functions of type" <+> parens (pretty gt)":\n"
    --   <> indent 2 (vsep [ "*" <+> pretty t <+> ":" <+> pretty y | y@(AnnoS (Idx _ t) _ _, _)  <- xs'])

    -- Propagate downwards
    -- A lambda is a value built where it appears, so it lives in its
    -- parent's language; only its body chooses, and a call the body makes
    -- elsewhere is an ordinary crossing there. The scores of a lambda are
    -- its body's, so the parent's choice already accounts for the body.
    collapseExpr heads _ l1 (LamS vs x, Idx i ss) = do
      lang <- case l1 of
        Just _ -> return l1
        Nothing -> pickLanguage heads l1 [x] ss
      x' <- collapseAnnoS heads lang x
      return (LamS vs x', Idx i lang)
    collapseExpr heads _ l1 (AppS f xs, Idx i ss) = do
      -- A sourced function runs where it is sourced; only a call whose
      -- head is defined in morloc can follow a recursive head's language.
      lang <- if isSourcedMany f then chooseLanguage l1 ss else pickLanguage heads l1 (f : xs) ss
      f' <- collapseHead heads lang f
      xs' <- mapM (if isSourceHead f' then collapseCarried heads lang else collapseAnnoS heads lang) xs
      return (AppS f' xs', Idx i lang)
    -- Propagate data
    collapseExpr heads _ l1 (e@(LstS xs), Idx i ss) = do
      lang <- if isFunctionalData e then return l1 else pickLanguage heads l1 xs ss
      xs' <- mapM (collapseElement heads e lang) xs
      return (LstS xs', Idx i lang)
    collapseExpr heads _ l1 (e@(TupS xs), Idx i ss) = do
      lang <- if isFunctionalData e then return l1 else pickLanguage heads l1 xs ss
      xs' <- mapM (collapseElement heads e lang) xs
      return (TupS xs', Idx i lang)
    collapseExpr heads _ l1 (e@(NamS rs), Idx i ss) = do
      lang <- if isFunctionalData e then return l1 else pickLanguage heads l1 (map snd rs) ss
      xs' <- mapM (collapseElement heads e lang . snd) rs
      return (NamS (zip (map fst rs) xs'), Idx i lang)
    -- collapse leaf expressions
    collapseExpr _ _ _ (ExeS x@(SrcCall src), Idx i _) = return (ExeS x, Idx i (Just (srcLang src)))
    collapseExpr _ _ lang (ExeS x@(PatCall _), Idx i _) = return (ExeS x, Idx i lang)
    collapseExpr _ _ lang (BndS v, Idx i _) = return (BndS v, Idx i lang)
    collapseExpr _ _ lang (UniS, Idx i _) = return (UniS, Idx i lang)
    collapseExpr _ _ lang (NullS, Idx i _) = return (NullS, Idx i lang)
    collapseExpr _ _ lang (RealS si x, Idx i _) = return (RealS si x, Idx i lang)
    collapseExpr _ _ lang (IntS si x, Idx i _) = return (IntS si x, Idx i lang)
    collapseExpr _ _ lang (LogS x, Idx i _) = return (LogS x, Idx i lang)
    collapseExpr _ _ lang (StrS x, Idx i _) = return (StrS x, Idx i lang)
    collapseExpr heads _ lang (ConS tv n j xs, Idx i _) = do
      xs' <- mapM (collapseAnnoS heads lang) xs
      return (ConS tv n j xs', Idx i lang)
    collapseExpr heads _ l1 (LetS v e1 e2, Idx i ss) = do
      lang <- pickLanguage heads l1 [e1, e2] ss
      e1' <- collapseAnnoS heads lang e1
      e2' <- collapseAnnoS heads lang e2
      return (LetS v e1' e2', Idx i lang)
    collapseExpr _ _ lang (LetBndS v, Idx i _) = return (LetBndS v, Idx i lang)
    collapseExpr _ _ lang (CallS v, Idx i _) = return (CallS v, Idx i lang)
    collapseExpr heads _ l1 (IfS c t e, Idx i ss) = do
      lang <- pickLanguage heads l1 [c, t, e] ss
      c' <- collapseAnnoS heads lang c
      t' <- collapseAnnoS heads lang t
      e' <- collapseAnnoS heads lang e
      return (IfS c' t' e', Idx i lang)
    collapseExpr heads _ l1 (DoBlockS x, Idx i ss) = do
      lang <- pickLanguage heads l1 [x] ss
      x' <- collapseAnnoS heads lang x
      return (DoBlockS x', Idx i lang)
    collapseExpr heads _ l1 (EvalS x, Idx i ss) = do
      lang <- pickLanguage heads l1 [x] ss
      x' <- collapseAnnoS heads lang x
      return (EvalS x', Idx i lang)
    -- Every path that gives a cast its language comes through here: the
    -- choice (including a recursion head's) must be one where the newtype
    -- has no form of its own (NEWT-7), else the cheapest one that is.
    collapseExpr heads _ l1 (CoerceS c@(CoerceNewtype dir target) x, Idx i ss) = do
      picked <- pickLanguage heads l1 [x] ss
      let n = castHead dir target (annoType x)
          -- a cast given no language of its own runs in its parent's
          effective = maybe l1 Just picked
      (_, allowed) <- castLangs n (maybe [] pure effective <> map fst ss)
      lang <- case effective of
        Just l
          | l `notElem` allowed ->
              case [(l2, s) | (l2, s) <- ss, l2 `elem` allowed] of
                [] ->
                  MM.throwSourcedError i $
                    "Cannot convert between" <+> squotes (pretty n)
                      <+> "and its representation in" <+> pretty l
                      <> ":" <+> pretty n <+> "has a" <+> pretty l
                      <+> "form of its own, and no language without one is available."
                usable -> chooseLanguage l1 usable
        _ -> return picked
      x' <- collapseAnnoS heads lang x
      return (CoerceS c x', Idx i lang)
    collapseExpr heads _ l1 (CoerceS c x, Idx i ss) = do
      lang <- pickLanguage heads l1 [x] ss
      x' <- collapseAnnoS heads lang x
      return (CoerceS c x', Idx i lang)
    collapseExpr heads _ l1 (LabelS k x, Idx i ss) = do
      lang <- pickLanguage heads l1 [x] ss
      x' <- collapseAnnoS heads lang x
      return (LabelS k x', Idx i lang)
    collapseExpr heads _ l1 (IntrinsicS intr xs, Idx i ss) = do
      lang <- pickLanguage heads l1 xs ss
      xs' <- mapM (collapseAnnoS heads lang) xs
      return (IntrinsicS intr xs', Idx i lang)

    -- An element of a container. A container holding function values sits
    -- in its parent's language, and so does each function value it holds:
    -- a closure is homed with the container that carries it and reaches
    -- any other pool from inside its body. Its body is collapsed as usual,
    -- so a call it makes elsewhere becomes an ordinary crossing there.
    -- A function value handed straight to a sourced function is carried the
    -- same way: the host's pool holds the callable it is given.
    collapseElement ::
      [(EVar, Lang)] ->
      ExprS (Indexed Type) Many (Indexed [(Lang, Score)]) ->
      Maybe Lang ->
      AnnoS (Indexed Type) Many (Indexed [(Lang, Score)]) ->
      MorlocMonad (AnnoS (Indexed Type) One (Indexed (Maybe Lang)))
    collapseElement heads container lang x
      | isFunctionalData container = collapseCarried heads lang x
      | otherwise = collapseAnnoS heads lang x

    collapseCarried ::
      [(EVar, Lang)] ->
      Maybe Lang ->
      AnnoS (Indexed Type) Many (Indexed [(Lang, Score)]) ->
      MorlocMonad (AnnoS (Indexed Type) One (Indexed (Maybe Lang)))
    collapseCarried heads lang x@(AnnoS (Idx _ t) _ e)
      | isCallableType t, not (isNewtypeCast e) = do
          AnnoS g (Idx i _) e' <- collapseAnnoS heads lang x
          return (AnnoS g (Idx i lang) e')
      | otherwise = collapseAnnoS heads lang x

    -- A named function used as a value is a lambda reached through its
    -- name, so like an inline lambda it is built in its parent's language;
    -- only its body chooses. Its scores are cut to that language, priced as
    -- its body's cheapest placement reached from there.
    pinNamedLambda ::
      Lang ->
      AnnoS (Indexed Type) Many (Indexed [(Lang, Score)]) ->
      AnnoS (Indexed Type) Many (Indexed [(Lang, Score)])
    pinNamedLambda lang x@(AnnoS g (Idx i ss) e) = case e of
      LamS _ _ -> AnnoS g (Idx i (pinScores ss)) e
      VarS v (Many alts)
        | any isNamedLambda alts ->
            AnnoS g (Idx i (pinScores ss)) (VarS v (Many (map (pinNamedLambda lang) alts)))
      _ -> x
      where
        pinScores ss0 = case minimumMay [cost (Just lang) l2 s2 | (l2, s2) <- ss0] of
          Nothing -> ss0
          Just s -> [(lang, s)]

    isNamedLambda :: AnnoS g Many c -> Bool
    isNamedLambda (AnnoS _ _ (LamS _ _)) = True
    isNamedLambda (AnnoS _ _ (VarS _ (Many alts))) = any isNamedLambda alts
    isNamedLambda _ = False

    -- The head of an application, before collapse, is a sourced function
    -- however it is named: every alternative of the term is a source call.
    isSourcedMany :: AnnoS (Indexed Type) Many c -> Bool
    isSourcedMany (AnnoS _ _ (ExeS (SrcCall _))) = True
    isSourcedMany (AnnoS _ _ (VarS _ (Many xs))) = not (null xs) && all isSourcedMany xs
    isSourcedMany _ = False

    -- The head of an application is a sourced function, reached directly or
    -- through the term that names it.
    isSourceHead :: AnnoS (Indexed Type) One (Indexed (Maybe Lang)) -> Bool
    isSourceHead (AnnoS _ _ (ExeS (SrcCall _))) = True
    isSourceHead (AnnoS _ _ (VarS _ (One x))) = isSourceHead x
    isSourceHead _ = False

    -- A cast keeps the language chosen for it (NEWT-7); it is not carried
    -- into its container's.
    isNewtypeCast :: ExprS g f c -> Bool
    isNewtypeCast (CoerceS (CoerceNewtype _ _) _) = True
    isNewtypeCast _ = False


    chooseLanguage :: Maybe Lang -> [(Lang, Score)] -> MorlocMonad (Maybe Lang)
    chooseLanguage l1 ss =
      case minBy (\(l2, s2) -> (s2, l2)) [(l2, cost l1 l2 s2) | (l2, s2) <- ss] of
        Nothing -> return Nothing
        (Just (l3, _)) -> return (Just l3)

    -- The language of a node, given the recursive heads enclosing it. A
    -- node with a back-edge to one of them beneath it takes that head's
    -- language: a recursion whose cycle crosses pools cannot lower to a
    -- loop, and every level of it then nests a foreign call that parks a
    -- worker in each pool, so its depth is bounded by workers rather than
    -- memory. Kept in one pool, the cycle lowers to a loop whose foreign
    -- primitives are a bounded number of calls per step. The outermost
    -- head wins, so nested cycles agree; a candidate hosted in the head's
    -- pool serves as well as the head's own language. A scorer cannot
    -- decide this, since a back-edge scores nothing in every language.
    pickLanguage ::
      [(EVar, Lang)] ->
      Maybe Lang ->
      [AnnoS (Indexed Type) Many (Indexed [(Lang, Score)])] ->
      [(Lang, Score)] ->
      MorlocMonad (Maybe Lang)
    pickLanguage heads l1 xs ss =
      case [l | (w, l) <- heads, any (containsCallS w) xs] of
        (l : _) -> return (Just (hostedWith l))
        [] -> chooseLanguage l1 ss
      where
        hostedWith l = case minBy (\(l2, s) -> (s, l2)) [(l2, s) | (l2, s) <- ss, LR.coLocated registry l l2] of
          Just (l2, _) -> l2
          Nothing -> l

    minBy :: (Ord b) => (a -> b) -> [a] -> Maybe a
    minBy _ [] = Nothing
    minBy _ [x] = Just x
    minBy f (x1 : rs) = case minBy f rs of
      Nothing -> Just x1
      (Just x2) -> if f x1 <= f x2 then Just x1 else Just x2

    minsBy :: (Ord b) => (a -> b) -> [a] -> [a]
    minsBy _ [] = []
    minsBy f (x : xs) = snd $ minsBy' (f x, [x]) xs
      where
        minsBy' (best, grp) [] = (best, grp)
        minsBy' (best, grp) (y : ys) = minsBy' (newSet (f y)) ys
          where
            newSet newScore
              | newScore == best = (best, y : grp)
              | newScore < best = (newScore, [y])
              | otherwise = (best, grp)

    -- find the lowest cost function for each key
    -- the groupSort function will never yield an empty value for vs, so `minimum` is safe
    minPairs :: (Ord a, Ord b) => [(a, b)] -> [(a, b)]
    minPairs = map (second minimum) . groupSort

    propagateDown ::
      AnnoS (Indexed Type) One (Indexed (Maybe Lang)) ->
      MorlocMonad (AnnoS (Indexed Type) One (Indexed Lang))
    propagateDown (AnnoS _ (Idx i Nothing) _) =
      MM.throwCompilerBugAt i "language not resolved for this node (unexpected Nothing in propagateDown)"
    propagateDown e@(AnnoS _ (Idx _ (Just lang0)) _) = f lang0 e
      where
        f ::
          Lang ->
          AnnoS (Indexed Type) One (Indexed (Maybe Lang)) ->
          MorlocMonad (AnnoS (Indexed Type) One (Indexed Lang))
        -- an inherited language, for a computation that can run there
        f lang (AnnoS g@(Idx gidx gt) (Idx i Nothing) e') = do
          lang' <- maybe (return lang) (\ts -> holdingLang gidx ts lang) (heldTypes e' gt)
          f lang' (AnnoS g (Idx i (Just lang')) e')
        f _ (AnnoS g (Idx i (Just lang)) e') = do
          e'' <- case e' of
            (AppS x xs) -> AppS <$> f lang x <*> mapM (f lang) xs
            (LamS vs x) -> LamS vs <$> f lang x
            (LstS xs) -> LstS <$> mapM (f lang) xs
            (TupS xs) -> TupS <$> mapM (f lang) xs
            (NamS rs) -> NamS <$> (zip (map fst rs) <$> mapM (f lang . snd) rs)
            UniS -> return UniS
            NullS -> return NullS
            (VarS v (One x)) -> VarS v . One <$> f lang x
            (BndS v) -> return (BndS v)
            (RealS si x) -> return (RealS si x)
            (IntS si x) -> return (IntS si x)
            (LogS x) -> return (LogS x)
            (StrS x) -> return (StrS x)
            (ConS tv n j xs) -> ConS tv n j <$> mapM (f lang) xs
            (ExeS x) -> return (ExeS x)
            (LetS v e1 e2) -> LetS v <$> f lang e1 <*> f lang e2
            (LetBndS v) -> return (LetBndS v)
            (CallS v) -> return (CallS v)
            (IfS c t elseE) -> IfS <$> f lang c <*> f lang t <*> f lang elseE
            (DoBlockS x) -> DoBlockS <$> f lang x
            (EvalS x) -> EvalS <$> f lang x
            (CoerceS c x) -> CoerceS c <$> f lang x
            (LabelS k x) -> LabelS k <$> f lang x
            (IntrinsicS intr xs) -> IntrinsicS intr <$> mapM (f lang) xs
          return (AnnoS g (Idx i lang) e'')

{- | This function is called on trees that contain no language-specific
components.  "GAST" refers to General Abstract Syntax Tree. The most common
GAST case, and the only one that is currently supported, is a expression
that merely rearranges data structures without calling any functions. Here
are a few examples:

 Constant values and containters (currently supported):
 f1 = 5
 f2 = [1,2,3]

 Variable values and containers (coming soon):
 f3 x = x

 f4 x = [1,2,x]

 Combinations of transformations on containers (possible, but not coming soon):
 f5 :: forall a b . (a, b) -> (b, a)
 f6 (x,y) = (y,x)

The idea could be elaborated into a full-fledged language.
-}
makeGAST ::
  AnnoS (Indexed Type) One (Indexed (Maybe Lang)) -> MorlocMonad (AnnoS (Indexed Type) One ())
makeGAST = mapAnnoSCM (\(Idx _ _) -> return ())

removeVarS :: AnnoS g One c -> AnnoS g One c
removeVarS (AnnoS g1 _ (VarS _ (One (AnnoS _ c2 x)))) = removeVarS (AnnoS g1 c2 x)
removeVarS (AnnoS g c (AppS x xs)) = AnnoS g c (AppS (removeVarS x) (map removeVarS xs))
removeVarS (AnnoS g c (LamS vs x)) = AnnoS g c (LamS vs (removeVarS x))
removeVarS (AnnoS g c (LstS xs)) = AnnoS g c (LstS (map removeVarS xs))
removeVarS (AnnoS g c (TupS xs)) = AnnoS g c (TupS (map removeVarS xs))
removeVarS (AnnoS g c (NamS rs)) = AnnoS g c (NamS (map (second removeVarS) rs))
removeVarS (AnnoS g c (LetS v e1 e2)) = AnnoS g c (LetS v (removeVarS e1) (removeVarS e2))
removeVarS (AnnoS g c (IfS cond thenE elseE)) = AnnoS g c (IfS (removeVarS cond) (removeVarS thenE) (removeVarS elseE))
removeVarS (AnnoS g c (DoBlockS e)) = AnnoS g c (DoBlockS (removeVarS e))
removeVarS (AnnoS g c (EvalS e)) = AnnoS g c (EvalS (removeVarS e))
removeVarS (AnnoS g c (CoerceS co e)) = AnnoS g c (CoerceS co (removeVarS e))
removeVarS (AnnoS g c (LabelS k e)) = AnnoS g c (LabelS k (removeVarS e))
removeVarS (AnnoS g c (IntrinsicS intr es)) = AnnoS g c (IntrinsicS intr (map removeVarS es))
removeVarS x = x

-- | Lift each non-exported self-recursive @where@-bound helper into its own
-- top-level rAST (Johnsson-style super-combinator conversion). Runs before
-- 'removeVarS' so the free-variable walker can see through 'VarS' wrappers.
extractRecursiveHelpers ::
  [AnnoS (Indexed Type) One (Indexed Lang)] ->
  MorlocMonad [AnnoS (Indexed Type) One (Indexed Lang)]
extractRecursiveHelpers rASTs = do
  exports <- MM.gets stateExports
  -- Each rAST is processed independently. Two exports may reuse the same
  -- @where@-bound name for unrelated helpers with different captures; a
  -- global capture map keyed by 'EVar' would conflate them.
  perTree <- mapM (processTree (Set.fromList exports)) rASTs
  let (modifiedList, helperListsPerTree) = unzip perTree
  return (modifiedList ++ concat helperListsPerTree)

processTree ::
  Set.Set Int ->
  AnnoS (Indexed Type) One (Indexed Lang) ->
  MorlocMonad
    ( AnnoS (Indexed Type) One (Indexed Lang)
    , [AnnoS (Indexed Type) One (Indexed Lang)]
    )
processTree exportSet tree = do
  (modifiedE, helpersE) <- extractFromRoot exportSet tree
  (modified0, helpers0) <- addStageEntries modifiedE helpersE
  let captureMap = closeFreeVars helpers0
  MM.modify (\st -> st {stateRecStages = Map.mapWithKey
    (\v (k, sv, n) -> (k, sv, maybe n length (Map.lookup v captureMap))) (stateRecStages st)})
  modified1 <- rewriteCalls captureMap modified0
  -- a let-bound value a helper captures becomes one of its parameters
  let helpers0' = [ (v, letToParam (Set.fromList (map fst (Map.findWithDefault [] v captureMap))) t)
                  | (v, t) <- helpers0 ]
  helpers1  <- mapM (\(v, t) -> (,) v <$> rewriteCalls captureMap t) helpers0'
  let helpers2 = map (uncurry (widenHelperLam captureMap)) helpers1
  return (modified1, helpers2)

-- | The first stage point of each recursive term defined in a tree, by the
-- name its back-edges use: the number of parameters of a definition whose
-- body does work before the lambda it returns.
stagePoints :: AnnoS (Indexed Type) Many Int -> Map EVar Int
stagePoints = go
  where
    go (AnnoS _ _ e) = case e of
      VarS v (Many alts) ->
        Map.unions (Map.fromList [ (v, length vs) | a <- alts, Just vs <- [staged v a] ] : map go alts)
      _ -> Map.unions (foldExprS (\x -> [go x]) e)
    staged v (AnnoS (Idx _ (FunT ts _)) _ (LamS vs body@(AnnoS _ _ be)))
      | length vs < length ts
      , notLam be
      , anyCallS (== v) body =
          Just vs
    staged _ _ = Nothing
    notLam (LamS _ _) = False
    notLam _ = True

-- | A recursive helper whose body does work before the lambda it returns
-- (a stage point after its first @k@ parameters) gets a second entry, the
-- /stage entry/, taking those @k@ parameters, running the work once, and
-- returning the closure. A partial application of the helper to exactly @k@
-- arguments calls the stage entry; a full application calls the helper.
addStageEntries ::
  AnnoS (Indexed Type) One (Indexed Lang) ->
  [(EVar, AnnoS (Indexed Type) One (Indexed Lang))] ->
  MorlocMonad
    ( AnnoS (Indexed Type) One (Indexed Lang)
    , [(EVar, AnnoS (Indexed Type) One (Indexed Lang))]
    )
addStageEntries modified helpers = do
  staged <- sequence
    [ do
        let stageV = EV (unEVar v <> "@stage")
            stageT = FunT (take k ts) (FunT (drop k ts) r)
        AnnoS (Idx gi' _) c' e' <- reindexOne t
        let AnnoS (Idx source _) _ _ = t
        MM.modify (\st -> st
          { stateName = Map.insert gi' stageV (stateName st)
          , stateNameSource = Map.insert gi' source (stateNameSource st)
          })
        return (v, stageV, k, stageT, AnnoS (Idx gi' stageT) c' e')
    | (v, t@(AnnoS (Idx _ (FunT ts r)) _ (LamS vs body))) <- helpers
    , let k = length vs
    , k < length ts
    , not (isLamBody body)
    ]
  MM.modify (\st -> st {stateRecStages = Map.union (Map.fromList [(v, (k, sv, 0)) | (v, sv, k, _, _) <- staged]) (stateRecStages st)})
  let rewriteAll x = foldr (\(v, sv, k, st, _) acc -> rewritePartialCalls v sv k st acc) x staged
      helpers' = [(v, rewriteAll t) | (v, t) <- helpers]
      stages = [(sv, rewriteAll t) | (_, sv, _, _, t) <- staged]
  return (rewriteAll modified, helpers' ++ stages)
  where
    isLamBody (AnnoS _ _ (LamS _ _)) = True
    isLamBody _ = False

-- | A partial application of recursive helper @v@ to its first @k@
-- arguments (written by the typechecker as @\vs -> v pre vs@, or applied
-- directly) becomes a call of its stage entry @stageV@.
rewritePartialCalls ::
  EVar -> EVar -> Int -> Type ->
  AnnoS (Indexed Type) One (Indexed Lang) ->
  AnnoS (Indexed Type) One (Indexed Lang)
rewritePartialCalls v stageV k stageT = go
  where
    go n@(AnnoS g c e) = case partialCall n of
      Just (hi, hc, args) -> AnnoS g c (AppS (AnnoS (Idx hi stageT) hc (CallS stageV)) (map go args))
      Nothing -> AnnoS g c (mapExprS go e)
    partialCall n@(AnnoS _ _ e)
      | Just (AnnoS (Idx hi _) hc (CallS v'), pre) <- etaParts n, v' == v, length pre == k = Just (hi, hc, pre)
      | AppS (AnnoS (Idx hi _) hc (CallS v')) xs <- e, v' == v, length xs == k = Just (hi, hc, xs)
      | otherwise = Nothing

-- | A copy of a tree with fresh indices carrying the state of those they copy.
reindexOne ::
  AnnoS (Indexed Type) One (Indexed Lang) ->
  MorlocMonad (AnnoS (Indexed Type) One (Indexed Lang))
reindexOne = reindexWith (\(Idx ci l) -> (`Idx` l) <$> newIndex ci)

-- | A copy with fresh general indices, and concrete annotations remade by
-- the given function.
reindexWith :: (c -> MorlocMonad c) -> AnnoS (Indexed Type) One c -> MorlocMonad (AnnoS (Indexed Type) One c)
reindexWith f (AnnoS (Idx gi t) c e) = do
  gi' <- newIndex gi
  c' <- f c
  AnnoS (Idx gi' t) c' <$> mapExprSM (reindexWith f) e

-- | A root lambda's manifold is built in its body's language (the body is
-- the manifold's code; the lambda is only its parameters), so the root is
-- annotated with that language. A shared specialization realized for a
-- caller in one language can place its body in another.
headInBody :: AnnoS g One (Indexed Lang) -> AnnoS g One (Indexed Lang)
headInBody (AnnoS g (Idx ci _) (LamS vs body)) =
  let body'@(AnnoS _ (Idx _ lang) _) = headInBody body
   in AnnoS g (Idx ci lang) (LamS vs body')
headInBody t = t

-- | A tree whose root term is renamed.
renameRoot :: EVar -> AnnoS g One c -> AnnoS g One c
renameRoot name (AnnoS g c (VarS _ x)) = AnnoS g c (VarS name x)
renameRoot _ t = t

-- | Walk an rAST, replacing every self-recursive @VarS v (One child)@ with
-- a bare @CallS v@ back-edge and returning the extracted body separately.
-- Extraction still recurses into 'child' so nested helpers are hoisted too.
extractFromTree ::
  Set.Set Int ->
  AnnoS (Indexed Type) One (Indexed Lang) ->
  MorlocMonad
    ( AnnoS (Indexed Type) One (Indexed Lang)
    , [(EVar, AnnoS (Indexed Type) One (Indexed Lang))]
    )
extractFromTree exports (AnnoS g@(Idx gi t) c e) = do
  (e', helpers) <- extractExpr exports e
  case (e, e') of
    -- A reference that now names an extracted helper: Treeify gave the
    -- reference the index of the definition it copied, which is the helper's
    -- manifold, so the reference takes an index of its own.
    (VarS _ _, CallS _) -> do
      gi' <- newIndex gi
      return (AnnoS (Idx gi' t) c e', helpers)
    _ -> return (AnnoS g c e', helpers)

-- | 'extractFromTree' at the root of a tree, whose index names it (an
-- export) and so is kept.
extractFromRoot ::
  Set.Set Int ->
  AnnoS (Indexed Type) One (Indexed Lang) ->
  MorlocMonad
    ( AnnoS (Indexed Type) One (Indexed Lang)
    , [(EVar, AnnoS (Indexed Type) One (Indexed Lang))]
    )
extractFromRoot exports (AnnoS g c e) = do
  (e', helpers) <- extractExpr exports e
  return (AnnoS g c e', helpers)

extractExpr ::
  Set.Set Int ->
  ExprS (Indexed Type) One (Indexed Lang) ->
  MorlocMonad
    ( ExprS (Indexed Type) One (Indexed Lang)
    , [(EVar, AnnoS (Indexed Type) One (Indexed Lang))]
    )
extractExpr exports (VarS v (One child@(AnnoS (Idx midx _) _ _)))
  | not (Set.member midx exports) && containsCallS v child = do
      -- Rename to @v@midx@ so each hoisted helper is uniquely addressable
      -- in Express.lookupRecursiveTarget's EVar-keyed reverse map, even
      -- when two exports share the same source name for their helpers.
      let newV = EV (unEVar v <> MT.pack ("@" <> show midx))
      (child', innerHelpers) <- extractFromTree exports child
      -- a helper hoisted from inside this one may call back to it (mutual
      -- recursion), so its back-edges are renamed with this one's
      let renamedChild = renameCallS v newV child'
          innerHelpers' = [(w, renameCallS v newV h) | (w, h) <- innerHelpers]
      MM.modify (\s -> s { stateName = Map.insert midx newV (stateName s) })
      return (CallS newV, (newV, renamedChild) : innerHelpers')
extractExpr exports (VarS v (One child)) = do
  (child', helpers) <- extractFromTree exports child
  return (VarS v (One child'), helpers)
extractExpr exports (AppS f xs) = do
  (f', fHelpers) <- extractFromTree exports f
  results <- mapM (extractFromTree exports) xs
  let (xs', xHelperLists) = unzip results
  return (AppS f' xs', fHelpers ++ concat xHelperLists)
extractExpr exports (LamS vs e) = do
  (e', helpers) <- extractFromTree exports e
  return (LamS vs e', helpers)
extractExpr exports (LstS xs) = do
  results <- mapM (extractFromTree exports) xs
  let (xs', helperLists) = unzip results
  return (LstS xs', concat helperLists)
extractExpr exports (TupS xs) = do
  results <- mapM (extractFromTree exports) xs
  let (xs', helperLists) = unzip results
  return (TupS xs', concat helperLists)
extractExpr exports (NamS rs) = do
  results <- mapM (extractFromTree exports . snd) rs
  let (vals', helperLists) = unzip results
  return (NamS (zip (map fst rs) vals'), concat helperLists)
extractExpr exports (LetS v e1 e2) = do
  (e1', h1) <- extractFromTree exports e1
  (e2', h2) <- extractFromTree exports e2
  return (LetS v e1' e2', h1 ++ h2)
extractExpr exports (IfS c t e) = do
  (c', h1) <- extractFromTree exports c
  (t', h2) <- extractFromTree exports t
  (e', h3) <- extractFromTree exports e
  return (IfS c' t' e', h1 ++ h2 ++ h3)
extractExpr exports (DoBlockS e) = do
  (e', helpers) <- extractFromTree exports e
  return (DoBlockS e', helpers)
extractExpr exports (EvalS e) = do
  (e', helpers) <- extractFromTree exports e
  return (EvalS e', helpers)
extractExpr exports (CoerceS c e) = do
  (e', helpers) <- extractFromTree exports e
  return (CoerceS c e', helpers)
extractExpr exports (LabelS k e) = do
  (e', helpers) <- extractFromTree exports e
  return (LabelS k e', helpers)
extractExpr exports (IntrinsicS intr es) = do
  results <- mapM (extractFromTree exports) es
  let (es', helperLists) = unzip results
  return (IntrinsicS intr es', concat helperLists)
extractExpr _ e = return (e, [])

-- | Free BndS occurrences in a tree, keyed by name, valued by the type at
-- the leaf. Respects LamS/LetS binders. Recurses through VarS so a captured
-- variable reachable only via a non-recursive where-bound value (dissolved
-- later by 'removeVarS') is still visible.
collectFreeBnds ::
  Set.Set EVar ->
  AnnoS (Indexed Type) One (Indexed Lang) ->
  Map EVar Type
collectFreeBnds = go
  where
    go bs (AnnoS (Idx _ t) _ (BndS v))
      | Set.member v bs = Map.empty
      | otherwise       = Map.singleton v t
    go bs (AnnoS (Idx _ t) _ (LetBndS v))
      | Set.member v bs = Map.empty
      | otherwise       = Map.singleton v t
    go bs (AnnoS _ _ (LamS vs body)) = go (Set.union bs (Set.fromList vs)) body
    go bs (AnnoS _ _ (LetS v e1 e2)) = Map.union (go bs e1) (go (Set.insert v bs) e2)
    go bs (AnnoS _ _ e)              = foldExprS (go bs) e

-- | Read the free let-bound variables @vs@ as lambda parameters.
letToParam ::
  Set.Set EVar ->
  AnnoS (Indexed Type) One (Indexed Lang) ->
  AnnoS (Indexed Type) One (Indexed Lang)
letToParam vs0 = go vs0
  where
    go vs n@(AnnoS g c e)
      | Set.null vs = n
      | otherwise = case e of
          LetBndS v | Set.member v vs -> AnnoS g c (BndS v)
          LamS xs body -> AnnoS g c (LamS xs (go (Set.difference vs (Set.fromList xs)) body))
          LetS v e1 e2 -> AnnoS g c (LetS v (go vs e1) (go (Set.delete v vs) e2))
          _ -> AnnoS g c (mapExprS (go vs) e)

-- | The helper calls in a tree, each with the variables bound around it
-- inside the tree.
callSitesIn ::
  Set.Set EVar ->
  AnnoS (Indexed Type) One (Indexed Lang) ->
  [(EVar, Set.Set EVar)]
callSitesIn helperNames = go Set.empty
  where
    go bs (AnnoS _ _ (CallS v))
      | Set.member v helperNames = [(v, bs)]
      | otherwise = []
    go bs (AnnoS _ _ (LamS vs body)) = go (Set.union bs (Set.fromList vs)) body
    go bs (AnnoS _ _ (LetS v e1 e2)) = go bs e1 ++ go (Set.insert v bs) e2
    go bs (AnnoS _ _ e) = foldExprS (go bs) e

-- | Info for each helper, precomputed once so 'closeFreeVars' doesn't
-- re-walk each body per lookup.
data HelperInfo = HelperInfo
  { hiParams  :: Set.Set EVar     -- ^ outer LamS binders
  , hiBodyFV  :: Map EVar Type    -- ^ FVs of the body (LamS/LetS-respecting)
  , hiCalls   :: [(EVar, Set.Set EVar)]
    -- ^ helper calls, each with the variables bound around it in the body
  }

helperInfo ::
  Set.Set EVar ->
  AnnoS (Indexed Type) One (Indexed Lang) ->
  HelperInfo
helperInfo helperNames tree = HelperInfo
  { hiParams  = case tree of AnnoS _ _ (LamS vs _) -> Set.fromList vs; _ -> Set.empty
  , hiBodyFV  = collectFreeBnds Set.empty tree
  , hiCalls   = callSitesIn helperNames tree
  }

-- | Least fixed point of the capture equation over the helper call graph.
-- For each helper name, returns the stable-ordered list of captured
-- @(name, type)@ pairs it must be lifted with.
--
-- >   cap(h) = FV(body(h)) + sum { cap(h') - bound(site) : site calls h' in h }
--
-- where @bound(site)@ is what the helper binds around that call (its own
-- parameters included): a variable a callee captures from a scope inside the
-- caller is passed at the call, not captured by the caller.
closeFreeVars ::
  [(EVar, AnnoS (Indexed Type) One (Indexed Lang))] ->
  Map EVar [(EVar, Type)]
closeFreeVars helpers =
  let helperNames = Set.fromList (map fst helpers)
      infos = [ (v, helperInfo helperNames t) | (v, t) <- helpers ]
      step acc = Map.fromList
        [ (v, Map.unions (hiBodyFV info : [ Map.withoutKeys (Map.findWithDefault Map.empty h acc) bound
                                          | (h, bound) <- hiCalls info ]))
        | (v, info) <- infos ]
      fixpoint acc =
        let acc' = step acc
         in if Map.map Map.keysSet acc' == Map.map Map.keysSet acc then acc' else fixpoint acc'
      closed = fixpoint (Map.fromList [ (v, Map.empty) | (v, _) <- infos ])
   in Map.map Map.toAscList closed

-- | Widen a helper's outer LamS/FunT by prepending its captured parameters.
-- The body was already rewritten by 'rewriteCalls'; injected LamS binders
-- shadow any outer name collision via the source-language's lexical rules.
widenHelperLam ::
  Map EVar [(EVar, Type)] ->
  EVar ->
  AnnoS (Indexed Type) One (Indexed Lang) ->
  AnnoS (Indexed Type) One (Indexed Lang)
widenHelperLam captureMap v tree =
  case Map.findWithDefault [] v captureMap of
    []       -> tree
    captured -> injectLam captured tree
  where
    injectLam captured (AnnoS (Idx gi t) c (LamS vs body)) =
      let (names, types) = unzip captured
      in AnnoS (Idx gi (widenFunType t types)) c (LamS (names ++ vs) body)
    injectLam _ other = other

-- | Prepend argument types to the FunT input list, recursing through any
-- surrounding EffectT layers so nested @\<E1\> \<E2\> (T -> U)@ widens
-- correctly.
widenFunType :: Type -> [Type] -> Type
widenFunType (FunT ins out) extra    = FunT (extra ++ ins) out
widenFunType (EffectT effs inner) extra = EffectT effs (widenFunType inner extra)
widenFunType t _ = t

-- | Rewrite every application of a widened helper to supply its captured
-- arguments, and eta-expand every bare CallS at a value position so
-- downstream Express always sees a fully-applied call. No-op when
-- @captureMap@ is empty (common case: no helpers extracted).
rewriteCalls ::
  Map EVar [(EVar, Type)] ->
  AnnoS (Indexed Type) One (Indexed Lang) ->
  MorlocMonad (AnnoS (Indexed Type) One (Indexed Lang))
rewriteCalls captureMap tree
  | Map.null captureMap = return tree
  | otherwise           = walk Set.empty tree
  where
    -- Injected BndS references an outer-scope variable that lives in the
    -- caller's language ('c'), not the callee's ('fc'); Express handles
    -- any cross-language transition at the callee.
    -- @lets@: the let-bound names in scope, which a call site passes as
    -- 'LetBndS'; any other captured name is a parameter here
    walk lets (AnnoS g c (AppS (AnnoS (Idx fi fT) fc (CallS v)) xs))
      | Just captured <- Map.lookup v captureMap
      , not (null captured) = do
          xs' <- mapM (walk lets) xs
          injected <- mapM (mkCapturedBnd lets c) captured
          let widenedT = widenFunType fT (map snd captured)
              headA = AnnoS (Idx fi widenedT) fc (CallS v)
          return $ AnnoS g c (AppS headA (injected ++ xs'))

    walk lets (AnnoS (Idx gi t) c (CallS v))
      | Just captured <- Map.lookup v captureMap
      , not (null captured) =
          etaExpandCallS (`Set.member` lets) gi t c v captured

    walk lets (AnnoS g c (LetS v e1 e2)) = do
      e1' <- walk lets e1
      e2' <- walk (Set.insert v lets) e2
      return (AnnoS g c (LetS v e1' e2'))
    walk lets (AnnoS g c (LamS vs body)) =
      AnnoS g c . LamS vs <$> walk (Set.difference lets (Set.fromList vs)) body
    walk lets (AnnoS g c e) = AnnoS g c <$> mapExprSM (walk lets) e

    mkCapturedBnd :: Set.Set EVar -> Indexed Lang -> (EVar, Type) -> MorlocMonad (AnnoS (Indexed Type) One (Indexed Lang))
    mkCapturedBnd lets c (name, t) = do
      gi <- MM.getCounter
      return $ AnnoS (Idx gi t) c (if Set.member name lets then LetBndS name else BndS name)

-- | Turn a bare @CallS v@ at value position into
--
-- >   λy1..ym. v X_1 .. X_k y1 .. ym
--
-- The outer AnnoS holds the caller-visible type (pre-lift signature); only
-- the inner @CallS@ carries the widened FunT for Express's arg-list sizing.
-- The outer index must be freshly allocated: Treeify writes @AnnoS gi gi@
-- at reference sites, so the CallS's own @gi@ is already claimed by the
-- extracted helper's manifold.
etaExpandCallS ::
  (EVar -> Bool) ->
  Int ->
  Type ->
  Indexed Lang ->
  EVar ->
  [(EVar, Type)] ->
  MorlocMonad (AnnoS (Indexed Type) One (Indexed Lang))
etaExpandCallS isLet gi t c v captured = do
  let (visibleIns, visibleOut, wrapEffect) = peelFunT t
      arity = length visibleIns
  freshNames  <- replicateM arity freshLamVar
  freshBndGis <- replicateM arity MM.getCounter
  capturedGis <- replicateM (length captured) MM.getCounter
  callGi      <- MM.getCounter
  appGi       <- MM.getCounter
  -- the lambda stands where the call was, and carries that index's state
  lamGi       <- newIndex gi
  let widenedT = widenFunType t (map snd captured)
      forwardBnds =
        [ AnnoS (Idx bgi ty) c (BndS n)
        | (bgi, n, ty) <- zip3 freshBndGis freshNames visibleIns
        ]
      capturedBnds =
        [ AnnoS (Idx cgi ty) c (if isLet n then LetBndS n else BndS n)
        | (cgi, (n, ty)) <- zip capturedGis captured
        ]
      callHead = AnnoS (Idx callGi widenedT) c (CallS v)
      appNode  = AnnoS (Idx appGi (wrapEffect visibleOut)) c
                       (AppS callHead (capturedBnds ++ forwardBnds))
  return $ AnnoS (Idx lamGi t) c (LamS freshNames appNode)
  where
    -- Peel every EffectT layer off @t@'s return, returning a reconstructor
    -- that re-attaches them so the inner AppS's carried type still reports
    -- the caller's effect stack.
    peelFunT (FunT ins out) = (ins, out, id)
    peelFunT (EffectT effs inner) =
      let (ins, out, wrap) = peelFunT inner
      in (ins, out, EffectT effs . wrap)
    peelFunT other = ([], other, id)

freshLamVar :: MorlocMonad EVar
freshLamVar = do
  i <- MM.getCounter
  return (EV (MT.pack ("$eta_" <> show i)))

-- | Does the tree contain a @CallS target@ anywhere?
-- | True if any 'CallS' back-edge in the tree names a variable satisfying @p@.
anyCallS :: (Foldable f) => (EVar -> Bool) -> AnnoS g f c -> Bool
anyCallS p = getAny . foldAnnoS check
  where
    check (AnnoS _ _ (CallS v)) = Any (p v)
    check _                     = Any False

anyIntrinsicS :: (Foldable f) => Intrinsic -> AnnoS g f c -> Bool
anyIntrinsicS intr = getAny . foldAnnoS check
  where
    check (AnnoS _ _ (IntrinsicS i _)) = Any (i == intr)
    check _                            = Any False

containsCallS :: (Foldable f) => EVar -> AnnoS g f c -> Bool
containsCallS target = anyCallS (== target)

-- | Rewrite every @CallS old@ target to @CallS new@.
renameCallS ::
  EVar -> EVar ->
  AnnoS (Indexed Type) One (Indexed Lang) ->
  AnnoS (Indexed Type) One (Indexed Lang)
renameCallS old new = go
  where
    go (AnnoS g c (CallS v)) | v == old = AnnoS g c (CallS new)
    go (AnnoS g c e)                    = AnnoS g c (mapExprS go e)

-- Check if this expression is a data structure that contains a function
-- value (a suspension among them). If so, the data structure is in the
-- same language as the parent, and so is each function value it holds
-- ('collapseElement'): a closure crosses a pool boundary as a value of its
-- home pool, and that pool is the one the container is built in.
isFunctionalData :: ExprS (Indexed Type) f a -> Bool
isFunctionalData (LstS xs) = any isFunctionalDataAnnoS xs
isFunctionalData (TupS xs) = any isFunctionalDataAnnoS xs
isFunctionalData (NamS (map snd -> xs)) = any isFunctionalDataAnnoS xs
isFunctionalData _ = False

isFunctionalDataAnnoS :: AnnoS (Indexed Type) f a -> Bool
isFunctionalDataAnnoS (AnnoS (Idx _ t) _ e) = isCallableType t || isFunctionalData e

-- | A function or a suspension.
isCallableType :: Type -> Bool
isCallableType (FunT _ _) = True
isCallableType (EffectT _ _) = True
isCallableType _ = False

-- | A value that is, or holds anywhere inside it, a function or a
-- suspension, which crosses a pool as a proxy calling home.
holdsCallable :: Type -> Bool
holdsCallable t = containsFunT t || containsEffectT t
