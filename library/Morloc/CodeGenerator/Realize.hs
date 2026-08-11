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

import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.Grammars.Common (propagateManifoldLabel)
import qualified Morloc.CodeGenerator.SystemConfig as MCS
import Morloc.Data.Doc
import Morloc.Data.Map (Map)
import qualified Morloc.Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Graph as Graph
import qualified Data.List as List
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.TypeEval as TE

realityCheck ::
  -- | one AST forest for each command exported from main
  [AnnoS (Indexed Type) Many Int] ->
  MorlocMonad
    ( [AnnoS (Indexed Type) One ()]
    , [AnnoS (Indexed Type) One (Indexed Lang)]
    )
realityCheck es = do
  -- translate modules into bitrees
  (gASTs0, rASTs0) <-
    -- select a single instance at each node in the tree
    mapM realize es
      -- separate unrealized (general) ASTs (uASTs) from realized ASTs (rASTs)
      |>> partitionEithers

  -- Extract non-exported recursive helpers into their own rASTs.
  -- This must happen before removeVarS so we can find the VarS wrappers.
  rASTs1 <- extractRecursiveHelpers rASTs0

  -- Now dissolve remaining (non-recursive) VarS wrappers
  let gASTs = map removeVarS gASTs0
      rASTs = map removeVarS rASTs1

  -- check and configure the system
  -- in the future, the results of this step may be used to winnow the build
  MCS.configure rASTs

  return (gASTs, rASTs)

-- | The realize objective: @(accumulated cost, number of language switches)@,
-- compared lexicographically. Cost dominates; switch-count is a PURE tiebreaker
-- that only resolves genuine cost-ties (it never overrides a real cost
-- difference), keeping language-agnostic terms in the parent's language to avoid
-- gratuitous boundaries. The same objective is used in both scoring and collapse
-- (replacing the old backward-only @+1@ biasedCost).
type Score = (Int, Int)

-- | Component-wise addition of two scores.
addScore :: Score -> Score -> Score
addScore (a, b) (c, d) = (a + c, b + d)

-- State for the realize scoring algorithm
data RState = RState
  { rLangs :: [Lang]
  , rApplied :: [AnnoS (Indexed Type) Many Int]
  , rBndVars :: Map EVar (AnnoS (Indexed Type) Many Int)
  , rLetVars :: Map EVar [(Lang, Score)] -- ^ let-bound variable -> RHS scores
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
-- The point is tractability, not tidiness: a point-free composition chain
-- @id . id . ..@ desugars to nested singly-used literal-lambda redexes that
-- collapse here to a single @\\x -> x@. Removing them removes the input on which
-- the scorer re-scores function-typed bound variables, the source of the
-- exponential blow-up on composition depth.
normalizePop1 ::
  AnnoS (Indexed Type) Many Int ->
  MorlocMonad (AnnoS (Indexed Type) Many Int)
-- empty lambda, no args left: unwrap, moving the discarded head's label up
normalizePop1 (AnnoS g1@(Idx g1Idx _) _ (AppS (AnnoS (Idx lamIdx _) _ (LamS [] (AnnoS _ c2 e))) [])) = do
  _ <- propagateManifoldLabel g1Idx lamIdx
  normalizePop1 (AnnoS g1 c2 e)
-- empty lambda with remaining args (over-application): unwrap the spent layer
normalizePop1 (AnnoS g1@(Idx g1Idx _) c1 (AppS (AnnoS (Idx lamIdx _) _ (LamS [] body)) es@(_ : _))) = do
  _ <- propagateManifoldLabel g1Idx lamIdx
  normalizePop1 (AnnoS g1 c1 (AppS body es))
-- beta-reduce one singly-used parameter (pure move); recurse on the residual
normalizePop1
  ( AnnoS
      i1
      tb1
      ( AppS
          (AnnoS (Idx i2 (FunT (_tv : tas) tb2)) c (LamS (v : vs) e2))
          (e1 : es)
        )
    )
    | countRefsMany v e2 <= 1 =
        normalizePop1 $
          AnnoS i1 tb1 $
            AppS
              (AnnoS (Idx i2 (FunT tas tb2)) c (LamS vs (substFreeBnd v e1 e2)))
              es
-- application to no args: unwrap, moving the discarded head's label up
normalizePop1 (AnnoS g1@(Idx g1Idx _) _ (AppS (AnnoS (Idx headIdx _) c2 e) [])) = do
  _ <- propagateManifoldLabel g1Idx headIdx
  normalizePop1 (AnnoS g1 c2 e)
-- every other node (incl. multiply-used or Many-headed applications): recurse
normalizePop1 (AnnoS g c e) = AnnoS g c <$> mapExprSM normalizePop1 e

-- | Count free @BndS@/@LetBndS@ references to @v@, stopping at any binder that
-- shadows @v@. Mirrors 'Morloc.CodeGenerator.LambdaEval.countRefs' for the @Many@
-- tree so counting and 'substFreeBnd' agree on exactly which sites they touch.
countRefsMany :: EVar -> AnnoS (Indexed Type) Many Int -> Int
countRefsMany v = go
  where
    go (AnnoS _ _ (BndS v')) | v == v' = 1
    go (AnnoS _ _ (LetBndS v')) | v == v' = 1
    go (AnnoS _ _ (LamS vs _)) | v `elem` vs = 0
    go (AnnoS _ _ (LetS v' _ _)) | v == v' = 0
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
    go e0@(AnnoS _ _ (LetS v' _ _)) | v == v' = e0
    go (AnnoS g c e) = AnnoS g c (mapExprS go e)

{- | Choose a single concrete implementation. In the future, this component
may be one of the more complex components of the morloc compiler. It will
probably need to be implemented using an optimizing SMT solver. It will
also need benchmarking data from all the implementations and possibly
statistical info describing inputs.
-}
realize ::
  AnnoS (Indexed Type) Many Int ->
  MorlocMonad
    ( Either
        (AnnoS (Indexed Type) One ())
        (AnnoS (Indexed Type) One (Indexed Lang))
    )
realize s0 = do
  registry <- MM.gets stateLangRegistry
  realizeWithRegistry registry s0

realizeWithRegistry ::
  LangRegistry ->
  AnnoS (Indexed Type) Many Int ->
  MorlocMonad
    ( Either
        (AnnoS (Indexed Type) One ())
        (AnnoS (Indexed Type) One (Indexed Lang))
    )
realizeWithRegistry registry s0 = do
  -- Normalize language-invariant (literal-lambda-head) redexes before scoring so
  -- the scorer is not fed composition chains it would re-score exponentially.
  s0' <- normalizePop1 s0
  e@(AnnoS _ li _) <- scoreAnnoS emptyRState s0' >>= collapseAnnoS Nothing
  case li of
    (Idx _ Nothing) -> makeGAST e |>> Left
    (Idx _ _) -> propagateDown e |>> Right
  where
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
    scoreAnnoS rstat (AnnoS gi ci e) = do
      (e', ci') <- scoreExpr rstat (e, ci)
      return $ AnnoS gi ci' e'

    -- \| Alternates with scoresAnnoS, finds the best score for each language at
    -- application nodes.
    scoreExpr ::
      RState ->
      (ExprS (Indexed Type) Many Int, Int) ->
      MorlocMonad (ExprS (Indexed Type) Many (Indexed [(Lang, Score)]), Indexed [(Lang, Score)])
    scoreExpr rstat (LstS xs, i) = do
      (xs', best) <- scoreMany rstat xs
      return (LstS xs', Idx i best)
    scoreExpr rstat (TupS xs, i) = do
      (xs', best) <- scoreMany rstat xs
      return (TupS xs', Idx i best)
    scoreExpr rstat (NamS rs, i) = do
      (xs, best) <- scoreMany rstat (map snd rs)
      return (NamS (zip (map fst rs) xs), Idx i best)
    scoreExpr rstat (LamS vs x, i) = do
      x' <- scoreAnnoS (updateRState vs rstat) x
      return (LamS vs x', Idx i (scoresOf x'))
    scoreExpr rstat (AppS f xs, i) = do
      -- store all applied arguments
      -- these may be bound to lambdas within f
      -- they are required for resolving the application language
      let rstat' = rstat {rLangs = [], rApplied = xs}

      f' <- scoreAnnoS rstat' f

      -- best scores for each language for f
      let scores = scoresOf f'
          rstat'' = emptyRState {rLangs = unique $ map fst scores}

      xs' <- mapM (scoreAnnoS rstat'') xs

      let pairss = [minPairs pairs | AnnoS _ (Idx _ pairs) _ <- xs']
      let best = scoreApp scores pairss

      return (AppS f' xs', Idx i best)
    -- non-recursive expressions
    scoreExpr rstat (UniS, i) = return (UniS, zipLang i rstat)
    scoreExpr rstat (NullS, i) = return (NullS, zipLang i rstat)
    scoreExpr rstat (VarS v (Many xs), i) = do
      (xs', best) <- scoreMany rstat xs
      return (VarS v (Many xs'), Idx i best)
    scoreExpr rstat (BndS v, i) = do
      case Map.lookup v (rBndVars rstat) of
        (Just e@(AnnoS (Idx _ (FunT _ _)) _ _)) -> do
          scores <- scoreAnnoS rstat e |>> scoresOf
          return (BndS v, Idx i scores)
        _ -> return (BndS v, zipLang i rstat)
    scoreExpr _ (ExeS x@(SrcCall src), i) = return (ExeS x, Idx i [(srcLang src, (callCost src, 0))])
    scoreExpr rstat (ExeS x@(PatCall _), i) = return (ExeS x, zipLang i rstat)
    scoreExpr rstat (RealS si x, i) = return (RealS si x, zipLang i rstat)
    scoreExpr rstat (IntS si x, i) = return (IntS si x, zipLang i rstat)
    scoreExpr rstat (LogS x, i) = return (LogS x, zipLang i rstat)
    scoreExpr rstat (StrS x, i) = return (StrS x, zipLang i rstat)
    scoreExpr rstat (LetS v e1 e2, i) = do
      e1' <- scoreAnnoS rstat e1
      -- Make the let-bound variable's RHS scores available to LetBndS
      -- references in the body. Without this, LetBndS would fall back to
      -- the calling context's lang preferences (zipLang on rstat.rLangs)
      -- and miss the binding's actual language requirement.
      let rstat' = rstat { rLetVars = Map.insert v (scoresOf e1') (rLetVars rstat) }
      e2' <- scoreAnnoS rstat' e2
      -- Score the chain like an application: the binding's RHS and the body
      -- both contribute, with cross-language penalties applied per-lang.
      let best = scoreApp [] [ minPairs (scoresOf e1')
                             , minPairs (scoresOf e2')
                             ]
      return (LetS v e1' e2', Idx i best)
    scoreExpr rstat (LetBndS v, i) =
      case Map.lookup v (rLetVars rstat) of
        Just scs -> return (LetBndS v, Idx i scs)
        Nothing -> return (LetBndS v, zipLang i rstat)
    scoreExpr rstat (CallS v, i) = return (CallS v, zipLang i rstat)
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
      let best = scoreApp [] [ minPairs (scoresOf c')
                             , minPairs (scoresOf t')
                             , minPairs (scoresOf e')
                             ]
      return (IfS c' t' e', Idx i best)
    scoreExpr rstat (DoBlockS x, i) = do
      x' <- scoreAnnoS rstat x
      return (DoBlockS x', Idx i (scoresOf x'))
    scoreExpr rstat (EvalS x, i) = do
      x' <- scoreAnnoS rstat x
      return (EvalS x', Idx i (scoresOf x'))
    scoreExpr rstat (CoerceS c x, i) = do
      x' <- scoreAnnoS rstat x
      return (CoerceS c x', Idx i (scoresOf x'))
    scoreExpr rstat (IntrinsicS intr xs, i) = do
      xs' <- mapM (scoreAnnoS rstat) xs
      let Idx _ langScores = zipLang i rstat
          best = case xs' of
            [] -> langScores
            _ -> scoreApp [] [minPairs (scoresOf x') | x' <- xs']
      return (IntrinsicS intr xs', Idx i best)

    -- calculate the score for an application based on the score of the function
    -- and the scores of the arguments
    scoreApp ::
      [ ( Lang -- the language of the ith calling function implementation
        , Score -- the score of the ith implementation
        )
      ] ->
      [ [ ( Lang -- the language of the jth implementation of the kth argument
          , Score -- the score of the jth implementation of the kth argument
          )
        ]
      ] ->
      [(Lang, Score)]
    -- if nothing is known, nothing is returned
    scoreApp [] (concat -> []) = []
    -- if none of the arguments are language-specific, the scores are based only
    -- on the functions
    scoreApp scores (concat -> []) = scores
    -- if the function is not language-specific, calculate the cost of calling
    -- all arguments from each possible language context
    scoreApp [] pairss =
      let score = [(lang, (0, 0)) | lang <- unique $ map fst (concat pairss)]
       in scoreApp score pairss
    -- if arguments and function have implementations, calculate cost relative to
    -- each function implementation
    scoreApp scores pairss =
      [ ( l1
        , foldl
            addScore
            s1
            [ minimumDef noScore [addScore s2 (transScore l1 l2) | (l2, s2) <- pairs]
            | pairs <- pairss
            ]
        )
      | (l1, s1) <- scores
      ]

    updateRState :: [EVar] -> RState -> RState
    updateRState [] rstat = rstat
    updateRState _ rstat@(RState _ [] _ _) = rstat
    updateRState (v : vs) rstat@(RState _ (p : ps) bound _) =
      updateRState vs $
        rstat {rApplied = ps, rBndVars = Map.insert v p bound}

    zipLang :: Int -> RState -> Indexed [(Lang, Score)]
    zipLang i (rLangs -> langs) = Idx i (zip langs (repeat (0, 0)))

    scoresOf :: AnnoS a Many (Indexed [(Lang, Score)]) -> [(Lang, Score)]
    scoresOf (AnnoS _ (Idx _ xs) _) = minPairs xs

    -- find the scores of all implementations from all possible language contexts
    scoreMany ::
      RState ->
      [AnnoS (Indexed Type) Many Int] ->
      MorlocMonad ([AnnoS (Indexed Type) Many (Indexed [(Lang, Score)])], [(Lang, Score)])
    scoreMany rstat xs0 = do
      xs1 <- mapM (scoreAnnoS rstat) xs0
      return (xs1, scoreMany' xs1)
      where
        scoreMany' :: [AnnoS (Indexed Type) Many (Indexed [(Lang, Score)])] -> [(Lang, Score)]
        scoreMany' xs =
          let pairss = [(minPairs . concat) [xs' | (AnnoS _ (Idx _ xs') _) <- xs]]
              langs' = unique (rLangs rstat <> concatMap (map fst) pairss)
           in [ ( l1
                , foldl
                    addScore
                    (0, 0)
                    [ minimumDef
                      noScore
                      [ addScore score (transScore l1 l2)
                      | (l2, score) <- pairs
                      ]
                    | pairs <- pairss
                    ]
                )
              | l1 <- langs'
              ]

    collapseAnnoS ::
      Maybe Lang ->
      AnnoS (Indexed Type) Many (Indexed [(Lang, Score)]) ->
      MorlocMonad (AnnoS (Indexed Type) One (Indexed (Maybe Lang)))
    collapseAnnoS l1 (AnnoS gi@(Idx _ gt) ci e) = do
      (e', ci') <- collapseExpr gt l1 (e, ci)
      -- Explainability: at high verbosity, record each node's language decision
      -- (its index, the parent/incoming language, and the language chosen). The
      -- exact tree-DP makes this a faithful, per-node account of "why this pool".
      let Idx idx mlang = ci'
      MM.sayVVV $
        "realize: node" <+> pretty idx
          <+> ("parent=" <> maybe "general" pretty l1)
          <+> ("chose=" <> maybe "general" pretty mlang)
      return (AnnoS gi ci' e')

    -- Sentinel "no realization" score. Cost dominates, so the switch component
    -- is irrelevant. Got 10 billion nodes in your AST? I didn't think so.
    noScore :: Score
    noScore = (999999999, 999999999)

    -- Cost of moving a value from parent language l1 to l2: the pairwise
    -- (transition/self) cost, plus one language switch when the languages differ.
    -- The switch component is the pure lexicographic tiebreaker (see 'Score'):
    -- among languages of equal cost it keeps the value in the parent's language,
    -- the boundary-minimizing choice the old backward-only +1 biasedCost made.
    transScore :: Lang -> Lang -> Score
    transScore l1 l2 = (pairwiseCost l1 l2, if l1 == l2 then 0 else 1)

    cost ::
      Maybe Lang -> -- parent language (if given)
      Lang -> -- child lang (should always be given if we are working from scored pairs)
      Score -> -- score
      Score
    cost (Just l1) l2 score = addScore score (transScore l1 l2)
    cost _ _ score = score

    -- FIXME: in the future, this function should be replaced by an estimate of
    -- the function runtime, for now I will just base it off languages.
    callCost :: Source -> Int
    callCost src = languageCost (srcLang src)

    collapseExpr ::
      Type ->
      Maybe Lang -> -- the language of the parent expression (if Nothing, then this is a GAST)
      (ExprS (Indexed Type) Many (Indexed [(Lang, Score)]), Indexed [(Lang, Score)]) ->
      MorlocMonad (ExprS (Indexed Type) One (Indexed (Maybe Lang)), Indexed (Maybe Lang))

    collapseExpr _ _ (VarS v (Many []), Idx i _) =
      MM.throwSourcedError i $ "No implementation found for" <+> squotes (pretty v)
    -- Select one implementation for the given term
    collapseExpr gt l1 (VarS v (Many xs), Idx i _) = do
      let minXs = minsBy (\(AnnoS _ (Idx _ ss) _) -> minimumMay [cost l1 l2 s | (l2, s) <- ss]) xs
      (x, lang) <- case minXs of
        [] -> MM.throwSourcedError i $ "No implementation found for" <+> squotes (pretty v)
        [x] -> handleOne x
        choices -> mapM handleOne choices >>= handleMany gt
      return (VarS v (One x), Idx i lang)
      where
        handleOne ::
          AnnoS (Indexed Type) Many (Indexed [(Lang, Score)]) ->
          MorlocMonad (AnnoS (Indexed Type) One (Indexed (Maybe Lang)), Maybe Lang)
        handleOne x@(AnnoS _ (Idx _ ss) e) = do
          let newLang =
                if isFunctionalData e
                  then l1
                  else fmap fst (minBy (\(l2, s) -> cost l1 l2 s) ss)
          x' <- collapseAnnoS newLang x
          return (x', newLang)

        handleMany ::
          Type ->
          [(AnnoS (Indexed Type) One (Indexed (Maybe Lang)), Maybe Lang)] ->
          MorlocMonad (AnnoS (Indexed Type) One (Indexed (Maybe Lang)), Maybe Lang)
        handleMany gt' xs' =
          -- Compatible match: structural equality with UnkT as wildcard.
          -- Preserves alias specificity (Array Int matches Array (UnkT a)
          -- but not List (UnkT a)) while handling unsolved generics.
          case [x | x@(AnnoS (Idx _ t) _ _, _) <- xs', compatibleType gt' t] of
            (x' : _) -> return x'
            [] -> do
              gscope <- MM.getGeneralScope i
              -- Reduce gt' one step toward the root type and retry.
              -- This walks the alias chain: C -> B -> A -> Str,
              -- preferring the most specific matching instance.
              case TE.reduceType gscope (type2typeu gt') of
                Just gt'' -> handleMany (typeOf gt'') xs'
                Nothing ->
                  -- reduceType only reduces the top-level constructor. When
                  -- aliases appear inside compound types (e.g., Celsius -> Celsius
                  -- where type Celsius = Int), reduce leaf aliases one step.
                  case TE.reduceTypeLeaves gscope (type2typeu gt') of
                    Just gt'' -> handleMany (typeOf gt'') xs'
                    Nothing ->
                      case xs' of
                        -- All candidates have identical types: duplicates from
                        -- different imports (e.g., mempty from root-py and root-cpp).
                        (x'@(AnnoS (Idx _ t0) _ _, _) : rest)
                          | compatibleType gt' t0
                          , all (\(AnnoS (Idx _ t) _ _, _) -> compatibleType t0 t) rest -> return x'
                        _ ->
                          MM.throwSourcedError i $
                            "No matching implementation found for" <+> squotes (pretty v)
                              <+> "at type" <+> pretty gt'

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
    collapseExpr _ l1 (LamS vs x, Idx i ss) = do
      lang <- chooseLanguage l1 (subtreeHasRec [x]) ss
      x' <- collapseAnnoS lang x
      return (LamS vs x', Idx i lang)
    collapseExpr _ l1 (AppS f xs, Idx i ss) = do
      lang <- chooseLanguage l1 (subtreeHasRec (f : xs)) ss
      f' <- collapseAnnoS lang f
      xs' <- mapM (collapseAnnoS lang) xs
      return (AppS f' xs', Idx i lang)
    -- Propagate data
    collapseExpr _ l1 (e@(LstS xs), Idx i ss) = do
      lang <- if isFunctionalData e then return l1 else chooseLanguage l1 (subtreeHasRec xs) ss
      xs' <- mapM (collapseAnnoS lang) xs
      return (LstS xs', Idx i lang)
    collapseExpr _ l1 (e@(TupS xs), Idx i ss) = do
      lang <- if isFunctionalData e then return l1 else chooseLanguage l1 (subtreeHasRec xs) ss
      xs' <- mapM (collapseAnnoS lang) xs
      return (TupS xs', Idx i lang)
    collapseExpr _ l1 (e@(NamS rs), Idx i ss) = do
      lang <- if isFunctionalData e then return l1 else chooseLanguage l1 (subtreeHasRec (map snd rs)) ss
      xs' <- mapM (collapseAnnoS lang . snd) rs
      return (NamS (zip (map fst rs) xs'), Idx i lang)
    -- collapse leaf expressions
    collapseExpr _ _ (ExeS x@(SrcCall src), Idx i _) = return (ExeS x, Idx i (Just (srcLang src)))
    collapseExpr _ lang (ExeS x@(PatCall _), Idx i _) = return (ExeS x, Idx i lang)
    collapseExpr _ lang (BndS v, Idx i _) = return (BndS v, Idx i lang)
    collapseExpr _ lang (UniS, Idx i _) = return (UniS, Idx i lang)
    collapseExpr _ lang (NullS, Idx i _) = return (NullS, Idx i lang)
    collapseExpr _ lang (RealS si x, Idx i _) = return (RealS si x, Idx i lang)
    collapseExpr _ lang (IntS si x, Idx i _) = return (IntS si x, Idx i lang)
    collapseExpr _ lang (LogS x, Idx i _) = return (LogS x, Idx i lang)
    collapseExpr _ lang (StrS x, Idx i _) = return (StrS x, Idx i lang)
    collapseExpr _ l1 (LetS v e1 e2, Idx i ss) = do
      lang <- chooseLanguage l1 (subtreeHasRec [e1, e2]) ss
      e1' <- collapseAnnoS lang e1
      e2' <- collapseAnnoS lang e2
      return (LetS v e1' e2', Idx i lang)
    collapseExpr _ lang (LetBndS v, Idx i _) = return (LetBndS v, Idx i lang)
    collapseExpr _ lang (CallS v, Idx i _) = return (CallS v, Idx i lang)
    collapseExpr _ l1 (IfS c t e, Idx i ss) = do
      lang <- chooseLanguage l1 (subtreeHasRec [c, t, e]) ss
      c' <- collapseAnnoS lang c
      t' <- collapseAnnoS lang t
      e' <- collapseAnnoS lang e
      return (IfS c' t' e', Idx i lang)
    collapseExpr _ l1 (DoBlockS x, Idx i ss) = do
      lang <- chooseLanguage l1 (subtreeHasRec [x]) ss
      x' <- collapseAnnoS lang x
      return (DoBlockS x', Idx i lang)
    collapseExpr _ l1 (EvalS x, Idx i ss) = do
      lang <- chooseLanguage l1 (subtreeHasRec [x]) ss
      x' <- collapseAnnoS lang x
      return (EvalS x', Idx i lang)
    collapseExpr _ l1 (CoerceS c x, Idx i ss) = do
      lang <- chooseLanguage l1 (subtreeHasRec [x]) ss
      x' <- collapseAnnoS lang x
      return (CoerceS c x', Idx i lang)
    collapseExpr _ l1 (IntrinsicS intr xs, Idx i ss) = do
      lang <- chooseLanguage l1 (subtreeHasRec xs) ss
      xs' <- mapM (collapseAnnoS lang) xs
      return (IntrinsicS intr xs', Idx i lang)

    -- True if the subtree carries a self-recursive back-edge. During realize
    -- every 'CallS' is such a back-edge; they are introduced before this pass
    -- and only renamed/lifted afterward by 'extractRecursiveHelpers'.
    hasRecCall :: (Foldable f) => AnnoS g f c -> Bool
    hasRecCall = anyCallS (const True)

    -- Whether this whole rAST is recursive at all (computed once). Non-recursive
    -- trees -- the common case -- then short-circuit 'subtreeHasRec' without any
    -- per-node subtree walk (so they stay O(n)); only actually-recursive rASTs,
    -- which are small extracted helpers, pay the per-node walk. Computed from the
    -- pre-scoring tree 's0'; 'normalizePop1' only beta-reduces literal-lambda
    -- redexes and never adds or removes a 'CallS', so this agrees with the scored
    -- tree the collapse actually walks.
    treeHasCall :: Bool
    treeHasCall = hasRecCall s0

    -- 'recSpine' for a container: only meaningful in a recursive rAST, so gate
    -- the (short-circuiting) per-child walk on 'treeHasCall'.
    subtreeHasRec :: (Foldable f) => [AnnoS g f c] -> Bool
    subtreeHasRec xs = treeHasCall && any hasRecCall xs

    -- 'recSpine' is True when the subtree being placed contains a recursive
    -- back-edge. The back-edge must return to the enclosing recursive
    -- manifold's language (the head); on the un-crossed recursive spine that
    -- head language is the incoming parent 'l1'. Realizing the subtree in any
    -- other language 'l2' therefore forces the back-edge to cross 'l2 -> head'
    -- every iteration. That crossing is invisible to the scorer (a recursive
    -- 'CallS' scores (0,0) in every language, via 'zipLang'), so without this
    -- correction the scorer splits a mixed-language recursive body onto its
    -- heavy leaf's pool, putting the back-edge across a pool boundary and
    -- blocking native-loop lowering. Adding the back-edge crossing here keeps a
    -- recursive body co-located with its head whenever that is cost-competitive,
    -- WITHOUT hard-pinning: a genuinely cheaper cross-pool body (e.g. cross-pool
    -- mutual recursion whose partner is another language entirely) still wins.
    chooseLanguage :: Maybe Lang -> Bool -> [(Lang, Score)] -> MorlocMonad (Maybe Lang)
    chooseLanguage l1 recSpine ss = do
      let recPenalty l2 = case l1 of
            Just h | recSpine && l2 /= h -> transScore l2 h
            _ -> (0, 0)
      case minBy snd [(l2, cost l1 l2 s2 `addScore` recPenalty l2) | (l2, s2) <- ss] of
        Nothing -> return Nothing
        (Just (l3, _)) -> return (Just l3)

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
        f lang (AnnoS g (Idx i Nothing) e') = f lang (AnnoS g (Idx i (Just lang)) e')
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
            (ExeS x) -> return (ExeS x)
            (LetS v e1 e2) -> LetS v <$> f lang e1 <*> f lang e2
            (LetBndS v) -> return (LetBndS v)
            (CallS v) -> return (CallS v)
            (IfS c t elseE) -> IfS <$> f lang c <*> f lang t <*> f lang elseE
            (DoBlockS x) -> DoBlockS <$> f lang x
            (EvalS x) -> EvalS <$> f lang x
            (CoerceS c x) -> CoerceS c <$> f lang x
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
  (modified0, helpers0) <- extractFromTree exportSet tree
  let captureMap = closeFreeVars helpers0
  modified1 <- rewriteCalls captureMap modified0
  helpers1  <- mapM (\(v, t) -> (,) v <$> rewriteCalls captureMap t) helpers0
  let helpers2 = map (uncurry (widenHelperLam captureMap)) helpers1
  return (modified1, helpers2)

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
extractFromTree exports (AnnoS g c e) = do
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
      let renamedChild = renameCallS v newV child'
      MM.modify (\s -> s { stateName = Map.insert midx newV (stateName s) })
      return (CallS newV, (newV, renamedChild) : innerHelpers)
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
    go bs (AnnoS _ _ (LamS vs body)) = go (Set.union bs (Set.fromList vs)) body
    go bs (AnnoS _ _ (LetS v e1 e2)) = Map.union (go bs e1) (go (Set.insert v bs) e2)
    go bs (AnnoS _ _ e)              = foldExprS (go bs) e

-- | CallS names reachable from a tree.
callSitesIn :: AnnoS (Indexed Type) One (Indexed Lang) -> Set.Set EVar
callSitesIn = foldAnnoS extract
  where
    extract (AnnoS _ _ (CallS v)) = Set.singleton v
    extract _                     = Set.empty

-- | Info for each helper, precomputed once so 'closeFreeVars' doesn't
-- re-walk each body per lookup.
data HelperInfo = HelperInfo
  { hiParams  :: Set.Set EVar     -- ^ outer LamS binders
  , hiBodyFV  :: Map EVar Type    -- ^ FVs of the body (LamS/LetS-respecting)
  , hiCallees :: Set.Set EVar     -- ^ helper names reachable via CallS
  }

helperInfo ::
  Set.Set EVar ->
  AnnoS (Indexed Type) One (Indexed Lang) ->
  HelperInfo
helperInfo helperNames tree = HelperInfo
  { hiParams  = case tree of AnnoS _ _ (LamS vs _) -> Set.fromList vs; _ -> Set.empty
  , hiBodyFV  = collectFreeBnds Set.empty tree
  , hiCallees = callSitesIn tree `Set.intersection` helperNames
  }

-- | Least fixed point of the FV closure equation over the helper call
-- graph. For each helper name, returns the stable-ordered list of
-- captured @(name, type)@ pairs it must be lifted with.
--
-- >   FV∞(h) = FV(body(h)) ∪ ⋃ { FV∞(h') : h' ∈ helperCallees(h) }
-- >          ∖ params(h)
--
-- Solved by walking @Data.Graph.stronglyConnComp@'s output in reverse
-- topological order: every callee outside the current SCC has been
-- closed already, so its FV∞ is in the accumulator. Members of a
-- cyclic SCC share their combined FV.
closeFreeVars ::
  [(EVar, AnnoS (Indexed Type) One (Indexed Lang))] ->
  Map EVar [(EVar, Type)]
closeFreeVars helpers =
  let helperNames = Set.fromList (map fst helpers)
      infoMap     = Map.fromList
        [ (v, helperInfo helperNames t) | (v, t) <- helpers ]
      infoOf v    = Map.findWithDefault (HelperInfo Set.empty Map.empty Set.empty) v infoMap

      sccs = Graph.stronglyConnComp
        [ (v, v, Set.toList (hiCallees (infoOf v))) | (v, _) <- helpers ]

      accumSCC :: Map EVar (Map EVar Type) -> Graph.SCC EVar -> Map EVar (Map EVar Type)
      accumSCC acc scc =
        let members = case scc of
              Graph.AcyclicSCC v -> [v]
              Graph.CyclicSCC vs -> vs
            memberSet = Set.fromList members
            fvBodies  = Map.unions [ hiBodyFV (infoOf v) | v <- members ]
            outerCallees = Set.unions
              [ Set.difference (hiCallees (infoOf v)) memberSet | v <- members ]
            fvFromCallees = Map.unions
              [ Map.findWithDefault Map.empty h acc | h <- Set.toList outerCallees ]
            fvSCC = Map.union fvBodies fvFromCallees
            entry v =
              (v, Map.filterWithKey (\k _ -> Set.notMember k (hiParams (infoOf v))) fvSCC)
        in Map.union (Map.fromList (map entry members)) acc

      closed = List.foldl' accumSCC Map.empty sccs
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
  | otherwise           = walk tree
  where
    -- Injected BndS references an outer-scope variable that lives in the
    -- caller's language ('c'), not the callee's ('fc'); Express handles
    -- any cross-language transition at the callee.
    walk (AnnoS g c (AppS (AnnoS (Idx fi fT) fc (CallS v)) xs))
      | Just captured <- Map.lookup v captureMap
      , not (null captured) = do
          xs' <- mapM walk xs
          injected <- mapM (mkCapturedBnd c) captured
          let widenedT = widenFunType fT (map snd captured)
              headA = AnnoS (Idx fi widenedT) fc (CallS v)
          return $ AnnoS g c (AppS headA (injected ++ xs'))

    walk (AnnoS (Idx _ t) c (CallS v))
      | Just captured <- Map.lookup v captureMap
      , not (null captured) =
          etaExpandCallS t c v captured

    walk (AnnoS g c e) = AnnoS g c <$> mapExprSM walk e

    mkCapturedBnd :: Indexed Lang -> (EVar, Type) -> MorlocMonad (AnnoS (Indexed Type) One (Indexed Lang))
    mkCapturedBnd c (name, t) = do
      gi <- MM.getCounter
      return $ AnnoS (Idx gi t) c (BndS name)

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
  Type ->
  Indexed Lang ->
  EVar ->
  [(EVar, Type)] ->
  MorlocMonad (AnnoS (Indexed Type) One (Indexed Lang))
etaExpandCallS t c v captured = do
  let (visibleIns, visibleOut, wrapEffect) = peelFunT t
      arity = length visibleIns
  freshNames  <- replicateM arity freshLamVar
  freshBndGis <- replicateM arity MM.getCounter
  capturedGis <- replicateM (length captured) MM.getCounter
  callGi      <- MM.getCounter
  appGi       <- MM.getCounter
  lamGi       <- MM.getCounter
  let widenedT = widenFunType t (map snd captured)
      forwardBnds =
        [ AnnoS (Idx bgi ty) c (BndS n)
        | (bgi, n, ty) <- zip3 freshBndGis freshNames visibleIns
        ]
      capturedBnds =
        [ AnnoS (Idx cgi ty) c (BndS n)
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

-- Check if this expression is a data structure that contains
-- a function. If so, then the data structure is must be in the
-- same language as the parent (since functions can't be serialized)
isFunctionalData :: ExprS (Indexed Type) f a -> Bool
isFunctionalData (LstS xs) = any isFunctionalDataAnnoS xs
isFunctionalData (TupS xs) = any isFunctionalDataAnnoS xs
isFunctionalData (NamS (map snd -> xs)) = any isFunctionalDataAnnoS xs
isFunctionalData _ = False

isFunctionalDataAnnoS :: AnnoS (Indexed Type) f a -> Bool
isFunctionalDataAnnoS (AnnoS (Idx _ t) _ e) = handleType t || isFunctionalData e
  where
    handleType :: Type -> Bool
    handleType (FunT _ _) = True
    handleType _ = False
