{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.CodeGenerator.LambdaEval
Description : Beta-reduce applied lambdas in the codegen AST
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Performs beta-reduction on lambda applications in the 'AnnoS' tree so
that the code generator sees only fully-applied function calls or
unapplied lambdas, never @(\\x -> body) arg@.
-}
module Morloc.CodeGenerator.LambdaEval
  ( applyLambdas
  , reindexTree
  ) where

import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.Grammars.Common (propagateManifoldLabel)
import Morloc.Frontend.Namespace (newIndex)
import qualified Morloc.Monad as MM
import qualified Data.Map as Map
import qualified Morloc.Data.GMap as GMap
import Morloc.Data.Doc (pretty, squotes, (<+>))
import Morloc.CodeGenerator.Serial (containsFunT)
import Morloc.CodeGenerator.Value (etaParts, isValue)
import qualified Morloc.Data.Text as MT
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)

-- {- | Remove lambdas introduced through substitution
--
-- For example:
--
--  bif x = add x 10
--  bar py :: "int" -> "int"
--  bar y = add y 30
--  f z = bar (bif z)
--
-- In Treeify.hs, the morloc declarations will be substituted in as lambdas. But
-- we want to preserve the link to any annotations (in this case, the annotation
-- that `bar` should be in terms of python ints). The morloc declarations can be
-- substituted in as follows:
--
--  f z = (\y -> add y 30) ((\x -> add x 10) z)
--
-- The indices for bif and bar that link the annotations to the functions are
-- relative to the lambda expressions, so this substitution preserves the link.
-- Typechecking can proceed safely.
--
-- The expression can be simplified:
--
--  f z = (\y -> add y 30) ((\x -> add x 10) z)
--  f z = (\y -> add y 30) (add z 10)            -- [z / x]
--  f z = add (add z 10) 30                      -- [add z 10 / y]
--
-- The simplified expression is what should be written in the generated code. It
-- would also be easier to typecheck and debug. So should these substitutions be
-- done immediately after parsing? We need to preserve
--  1. links to locations in the original source code (for error messages)
--  2. type annotations.
--  3. declaration names for generated comments and subcommands
--
-- Here is the original expression again, but annotated and indexed
--
--  (\x -> add_2 x_3 10_4)_1
--  (\y -> add_6 y_7 30_8)_5
--  (\z -> bar_10 (bif_11 z_12))_9
--
--  1: name="bif"
--  5: name="bar", type="int"@py -> "int"@py
--  9: name="f"
--
-- Each add is also associated with a type defined in a signature in an
-- unmentioned imported library, but those will be looked up by the typechecker
-- and will not be affected by rewriting.
--
-- Substitution requires reindexing. A definition can be used multiple times and
-- we need to distinguish between the use cases.
--
-- Replace bif and bar with their definition and create fresh indices:
--
--  (\z -> (\y -> add_18 y_19 30_20)_17 ((\x -> add_14 x_15 10_16)_13 z_12)_9
--
--  13,1: name="bif"
--  17,5: name="bar", type="int"@py -> "int"@py
--  9: name="f"
--
-- Now we can substitute for y
--
--  (\z -> add_18 ((\x -> add_14 x_15 10_16)_13 z_12)_9 30_20)
--
-- But this destroyed index 17 and the link to the python annotation. We can
-- preserve the type by splitting the annotation of bar.
--
--  13,1: name="bif"
--  18,17,5: name="bar"
--  12: "int"@py
--  13: "int"@py
--  9: name="f"
--
-- Index 18 should be associated with the *name* "bar", but not the type, since it
-- has been applied. The type of bar is now split between indices 12 and 13.
--
-- This case works fine, but it breaks down when types are polymorphic. If the
-- annotation of bar had been `a -> a`, then how would we type 12 and 13? We can't
-- say that `12 :: forall a . a` and `13 :: forall a . a`, since this
-- eliminates the constraint that the `a`s must be the same.
--
-- If instead we rewrite lambdas after typechecking, then everything works out.
--
-- Thus reduce is done here, rather than in Treeify.hs or Desugar.hs.
--
-- Lambda application can also NOT be done before collapsing from Many to One in
-- AnnoS. The reason is that in ((VarS (Many es)) 42), the values in es
-- may contain `CallS src` or `LamS vs e` types. The CallS terms cannot be
-- reduced but the lambdas can. So applying here would lead to divergence.
--
-- It also must be done BEFORE conversion to ExprM in `express`, where manifolds
-- are resolved.
-- -}
-- | Beta-reduce a tree, then give every function value the arity of its
-- type (see 'saturate').
applyLambdas :: Bool -> AnnoS (Indexed Type) One a -> MorlocMonad (AnnoS (Indexed Type) One a)
applyLambdas ai e = do
  e' <- reduceRoot ai e >>= saturateAt True ai
  if ai then return e' else groupCallbacks e'

-- | Reduce the root of a tree. A root of function type -- a command, a shared
-- specialization, a recursive helper -- is called with every input of its
-- type, so it is the lambda over those inputs, whatever its body computes
-- first: @let k = e in \\x -> b@ is @\\w -> let k = e in b[w/x]@, and a
-- partial application is applied to the rest. One call is one evaluation of
-- the root, so work the body does before the function it returns runs once
-- per call either way. The lambda keeps the root's index, by which the root
-- is named, configured and called.
reduceRoot :: Bool -> AnnoS (Indexed Type) One a -> MorlocMonad (AnnoS (Indexed Type) One a)
reduceRoot ai n@(AnnoS g@(Idx gi (FunT ts r)) c e) = case e of
  ExeS _ -> reduce ai n
  CallS _ -> reduce ai n
  LamS vs body -> case params vs body of
    (ps, inner)
      | length ps >= length ts -> AnnoS g c . LamS ps <$> reduce ai inner
      | otherwise -> do
          let extra = drop (length ps) ts
          ws <- mapM (const (freshClosureName (EV "w"))) extra
          app <- applyTo (FunT extra r) inner ws extra
          AnnoS g c . LamS (ps <> ws) <$> reduce ai app
  _ -> do
    ws <- mapM (const (freshClosureName (EV "w"))) ts
    gi' <- newPlainIndex gi
    app <- applyTo (FunT ts r) (AnnoS (Idx gi' (FunT ts r)) c e) ws ts
    AnnoS g c . LamS ws <$> reduce ai app
  where
    -- directly nested lambdas are one parameter list, up to the type's
    params vs (AnnoS _ _ (LamS ws body))
      | length vs < length ts = params (vs <> ws) body
    params vs body = (vs, body)
    applyTo ft f ws wts = do
      argIdxs <- mapM (const (newPlainIndex gi)) ws
      appIdx <- newPlainIndex gi
      let args = [AnnoS (Idx ix wt) c (BndS w) | (ix, w, wt) <- zip3 argIdxs ws wts]
          AnnoS (Idx fi _) fc fe = f
      return (AnnoS (Idx appIdx r) c (AppS (AnnoS (Idx fi ft) fc fe) args))
reduceRoot ai n = reduce ai n

-- | A lambda left with fewer parameters than its type has arguments is a
-- function value whose body computes the function it returns. Every use that
-- applies it has been reduced; what remains is passed or stored whole, and a
-- function value is called with all of its type's arguments, so it takes
-- them all: @\x -> e@ at @a -> b -> c@ becomes @\x w -> e w@. A lambda not at
-- the root of its tree also gets a stage entry ('stateStageEntries').
saturate :: Bool -> AnnoS (Indexed Type) One a -> MorlocMonad (AnnoS (Indexed Type) One a)
saturate = saturateAt False

saturateAt :: Bool -> Bool -> AnnoS (Indexed Type) One a -> MorlocMonad (AnnoS (Indexed Type) One a)
-- a lambda whose body is directly another lambda takes both parameter lists:
-- nothing runs between them
saturateAt top ai (AnnoS g c (LamS vs (AnnoS _ _ (LamS ws body)))) =
  saturateAt top ai (AnnoS g c (LamS (vs ++ ws) body))
saturateAt top ai (AnnoS g@(Idx gi t@(FunT ts r)) c (LamS vs body))
  | length vs < length ts
  , not (isLam body) = do
      let extra = drop (length vs) ts
      ws <- mapM (\_ -> freshClosureName (EV "w")) extra
      body' <- saturate ai body
      argIdxs <- mapM (const (newPlainIndex gi)) extra
      appIdx <- newPlainIndex gi
      let args = [AnnoS (Idx ix wt) c (BndS w) | (ix, w, wt) <- zip3 argIdxs ws extra]
          app = AnnoS (Idx appIdx r) c (AppS body' args)
      app' <- reduce ai app
      let flatLam = AnnoS g c (LamS (vs ++ ws) app')
      if ai || top
        then return flatLam
        else do
          -- the stage entry: the lambda as written, taking the parameters
          -- before the stage point and returning the closure of the rest
          stageBody <- reindexTree body'
          sIdx <- newIndex gi
          sName <- freshClosureName (EV "stage")
          letIdx <- newPlainIndex gi
          let stageT = FunT (take (length vs) ts) (FunT extra r)
              stageLam = AnnoS (Idx sIdx stageT) c (LamS vs stageBody)
          recordStage gi (length vs) sIdx
          return (AnnoS (Idx letIdx t) c (LetS sName stageLam flatLam))
-- A staged recursive function used as a value is, like a staged lambda, its
-- flat entry with its stage entry beside it. Realize writes one that captures
-- values as a lambda over its parameters calling it with the captured values
-- first ('Morloc.CodeGenerator.Realize.etaExpandCallS'), and the typechecker
-- writes a partial application of it as a lambda over the rest; both are
-- @\vs -> v (pre ++ vs)@. The staged value is built over the captured values,
-- and the arguments the program applies are a partial application of it, whose
-- stage the runtime runs when it has the arguments before the stage point.
saturateAt top ai n@(AnnoS (Idx gi t@(FunT ts r)) c (LamS vs (AnnoS _ _ (AppS (AnnoS _ _ (CallS v)) xs))))
  | not (ai || top)
  , length vs == length ts
  , (pre, post) <- splitAt (length xs - length vs) xs
  , map bndName post == map Just vs =
      MM.gets (Map.lookup v . stateRecStages) >>= \stage -> case stage of
        Just (k, stageV, ncap)
          | ncap <= length pre
          , (caps, user) <- splitAt ncap pre
          , fullTs <- [ut | AnnoS (Idx _ ut) _ _ <- user] ++ ts
          , k < length fullTs -> do
              value <- stagedValue gi c v stageV k caps fullTs r
              if null user
                then return value
                else do
                  fName <- freshClosureName (EV "f")
                  headIdx <- newPlainIndex gi
                  appIdx <- newPlainIndex gi
                  letIdx <- newPlainIndex gi
                  let partial = AnnoS (Idx appIdx t) c (AppS (AnnoS (Idx headIdx (FunT fullTs r)) c (LetBndS fName)) user)
                  return (AnnoS (Idx letIdx t) c (LetS fName value partial))
        _ -> return n
  where
    bndName (AnnoS _ _ (BndS x)) = Just x
    bndName _ = Nothing
-- a staged recursive function used as a value, capturing nothing
saturateAt top ai (AnnoS g@(Idx gi (FunT ts r)) c (CallS v))
  | not (ai || top) = MM.gets (Map.lookup v . stateRecStages) >>= \stage -> case stage of
      Just (k, stageV, 0) | k < length ts -> stagedValue gi c v stageV k [] ts r
      _ -> return (AnnoS g c (CallS v))
-- the function of an application is applied, not a value
saturateAt _ ai (AnnoS g c (AppS f xs)) = do
  f' <- case f of
    AnnoS _ _ (CallS _) -> return f
    _ -> saturate ai f
  AnnoS g c . AppS f' <$> mapM (saturate ai) xs
saturateAt _ ai (AnnoS g c e) = AnnoS g c <$> mapExprSM (saturate ai) e

-- | The value of staged recursive function @v@ (stage entry @stageV@, first
-- stage point @k@) over captured values @caps@, at parameter types @ts@ and
-- result @r@: @let stage = \as -> stageV caps as in \ys -> v caps ys@, the
-- flat lambda recorded as a staged closure.
stagedValue ::
  Int -> a -> EVar -> EVar -> Int -> [AnnoS (Indexed Type) One a] -> [Type] -> Type ->
  MorlocMonad (AnnoS (Indexed Type) One a)
stagedValue gi c v stageV k caps ts r = do
  let capTs = [ct | AnnoS (Idx _ ct) _ _ <- caps]
      restT = FunT (drop k ts) r
  as <- mapM (const (freshClosureName (EV "a"))) (take k ts)
  ys <- mapM (const (freshClosureName (EV "y"))) ts
  capsS <- mapM reindexTree caps
  capsF <- mapM reindexTree caps
  aIdxs <- mapM (const (newPlainIndex gi)) as
  yIdxs <- mapM (const (newPlainIndex gi)) ys
  sHead <- newPlainIndex gi
  sApp <- newPlainIndex gi
  fHead <- newPlainIndex gi
  fApp <- newPlainIndex gi
  fIdx <- newPlainIndex gi
  letIdx <- newPlainIndex gi
  sIdx <- newIndex gi
  sName <- freshClosureName (EV "stage")
  let bnds idxs names types = [AnnoS (Idx ix bt) c (BndS x) | (ix, x, bt) <- zip3 idxs names types]
      stageCall = AnnoS (Idx sApp restT) c (AppS (AnnoS (Idx sHead (FunT (capTs ++ take k ts) restT)) c (CallS stageV)) (capsS ++ bnds aIdxs as (take k ts)))
      flatCall = AnnoS (Idx fApp r) c (AppS (AnnoS (Idx fHead (FunT (capTs ++ ts) r)) c (CallS v)) (capsF ++ bnds yIdxs ys ts))
      stageLam = AnnoS (Idx sIdx (FunT (take k ts) restT)) c (LamS as stageCall)
      flatLam = AnnoS (Idx fIdx (FunT ts r)) c (LamS ys flatCall)
  recordStage fIdx k sIdx
  return (AnnoS (Idx letIdx (FunT ts r)) c (LetS sName stageLam flatLam))

-- | Record flat entry @flat@ as staged after @k@ arguments, with stage entry
-- @stage@. The stage entry keeps the flat entry's configuration but for the
-- cache: only full calls are cached.
recordStage :: Int -> Int -> Int -> MorlocMonad ()
recordStage flat k stage =
  MM.modify (\st -> st { stateStageEntries = Map.insert flat (k, stage) (stateStageEntries st)
                       , stateManifoldConfig = Map.adjust (\cfg -> cfg {manifoldConfigCache = Nothing}) stage (stateManifoldConfig st) })

-- | Hand each function a source is passed in the grouping the source calls it
-- with. Morloc does not distinguish @a -> b -> c@ from @a -> (b -> c)@, but
-- code in another language does, and the way it calls a function argument is
-- the source's signature as written: a parameter @(a -> b -> c)@ is called
-- with two arguments, @(a -> (b -> c))@ with one. A function taking more than
-- that is passed as @\a1..an -> g a1..an@: a partial application of it, which
-- runs any stage it reaches ('mlc_papply').
groupCallbacks :: AnnoS (Indexed Type) One a -> MorlocMonad (AnnoS (Indexed Type) One a)
groupCallbacks e0 = do
  sigs <- MM.gets stateSignatures
  let grouping =
        Map.fromList
          [ (srcKey src, (map groups (params (etype et)), et))
          | sig <- GMap.elems sigs
          , (et, tts) <- case sig of
              Monomorphic tt -> [(et, [tt]) | Just et <- [termGeneral tt]]
              -- a method is called as its class declares it
              Polymorphic _ _ et tts -> [(et, tts)]
          , tt <- tts
          , (_, Idx _ src) <- termConcrete tt
          ]
  go grouping e0
  where
    params (ForallU _ t) = params t
    params (FunU ts _) = ts
    params _ = []
    -- the argument groups a parameter's type is written with:
    -- @(a -> (b -> c))@ is [1, 1], @(a -> b -> c)@ is [2]
    groups (ForallU _ t) = groups t
    groups (FunU ts r) = length ts : groups r
    groups _ = []
    -- a function the source passes to the function it is passed, written
    -- with more than one group: morloc would call it with every argument
    passesGrouped (ForallU _ t) = passesGrouped t
    passesGrouped (FunU ts _) = any ((> 1) . length . groups) ts
    passesGrouped _ = False

    go grouping (AnnoS g@(Idx gi _) c (AppS f xs)) = do
      f' <- go grouping f
      xs' <- mapM (go grouping) xs
      xs'' <- case sourceOf f of
        Just src | Just (gss, et) <- Map.lookup (srcKey src) grouping -> do
          when (any passesGrouped (params (etype et))) $
            MM.throwSourcedError gi $
              "The source" <+> squotes (pretty (unEVar (srcAlias src)))
                <+> "passes a function to a function it is given, and its signature groups that"
                <+> "function's arguments; morloc cannot call a function in the grouping of another"
                <+> "language, so write that parameter's type without parentheses around an arrow."
          sequence [regroup x gs | (x, gs) <- zip xs' (gss ++ repeat [])]
        _ -> return xs'
      return (AnnoS g c (AppS f' xs''))
    go grouping (AnnoS g c e) = AnnoS g c <$> mapExprSM (go grouping) e

    srcKey src = (srcName src, srcLang src, srcPath src)

    sourceOf (AnnoS _ _ (ExeS (SrcCall src))) = Just src
    sourceOf (AnnoS _ _ (VarS _ (One x))) = sourceOf x
    sourceOf _ = Nothing

    -- the function in the written groups: @\a1..an -> g a1..an@, where that
    -- partial application is itself grouped by the groups after the first
    regroup x@(AnnoS (Idx gi (FunT ins out)) c _) (n : rest)
      | n > 0, n < length ins = do
          let (here, more) = splitAt n ins
              restT = FunT more out
          gName <- freshClosureName (EV "g")
          as <- mapM (const (freshClosureName (EV "a"))) here
          aIdxs <- mapM (const (newPlainIndex gi)) here
          headIdx <- newPlainIndex gi
          appIdx <- newPlainIndex gi
          lamIdx <- newPlainIndex gi
          letIdx <- newPlainIndex gi
          let args = [AnnoS (Idx ix at) c (BndS a) | (ix, a, at) <- zip3 aIdxs as here]
              call = AnnoS (Idx appIdx restT) c (AppS (AnnoS (Idx headIdx (FunT ins out)) c (LetBndS gName)) args)
          call' <- regroup call rest
          let AnnoS (Idx _ callT) _ _ = call'
              t' = FunT here callT
          return (AnnoS (Idx letIdx t') c (LetS gName x (AnnoS (Idx lamIdx t') c (LamS as call'))))
    regroup x _ = return x

-- | An argument to evaluate once: a value as it is, anything else reduced and
-- bound to a fresh name, returned with its binding.
bindArg :: Bool -> AnnoS (Indexed Type) One a -> MorlocMonad ([(EVar, AnnoS (Indexed Type) One a)], AnnoS (Indexed Type) One a)
bindArg ai x@(AnnoS (Idx xi xt) xc _)
  | isValue x = return ([], x)
  | otherwise = do
      x' <- reduce ai x
      v <- freshClosureName (EV "arg")
      ri <- newPlainIndex xi
      return ([(v, x')], AnnoS (Idx ri xt) xc (LetBndS v))

reduce ::
  -- | @alwaysInline@: on the nexus (gAST) path this is True. The pure nexus
  -- evaluator has no pool to hold a native closure and cannot serialize a
  -- function value, so every let-bound lambda MUST be inlined there,
  -- regardless of how many times it is used. On the pool (rAST) path it is
  -- False, so a multiply-referenced lambda is kept shared (see the LetS-of-LamS
  -- clause) to avoid exponential inlining.
  Bool ->
  AnnoS (Indexed Type) One a ->
  MorlocMonad (AnnoS (Indexed Type) One a)
-- Beta-reduce empty lambdas and empty applications. The discarded head
-- AnnoS may carry a user label (e.g. a labeled pointfree reference like
-- @big:sum@ whose body was eta-expanded by typecheck); transfer the
-- label to the surviving outer index so codegen still sees it.
reduce ai (AnnoS g1@(Idx g1Idx _) _ (AppS (AnnoS (Idx lamIdx _) _ (LamS [] (AnnoS _ c2 e))) [])) = do
  void (propagateManifoldLabel g1Idx lamIdx)
  reduce ai $ AnnoS g1 c2 e
-- Over-applied curried lambda. Beta-reducing `(\base -> \y -> ..) 3 x`
-- consumes `base`, leaving `AppS (LamS [] (\y -> ..)) [x]` -- an empty
-- lambda layer still standing between the remaining args and the function
-- it returns. Unwrap it so the inner lambda meets the leftover args (the
-- empty-args clause above only fires when no args remain, so without this
-- the LamS survives to codegen and errors with "unexpected LamS").
reduce ai (AnnoS g1@(Idx g1Idx _) c1 (AppS (AnnoS (Idx lamIdx _) _ (LamS [] body)) es@(_ : _))) = do
  void (propagateManifoldLabel g1Idx lamIdx)
  reduce ai $ AnnoS g1 c1 (AppS body es)
reduce ai (AnnoS g1@(Idx g1Idx _) _ (AppS (AnnoS (Idx headIdx _) c2 e) [])) = do
  void (propagateManifoldLabel g1Idx headIdx)
  reduce ai $ AnnoS g1 c2 e
-- A partial application the typechecker eta-expanded, @\\vs -> f pre vs@,
-- with a head known to be a lambda: it is the application @f pre@, which the
-- strict beta rule below evaluates once, running the work @f@ does before its
-- remaining parameters and keeping any later stage of it.
reduce ai n@(AnnoS g c (LamS _ _))
  | Just (f, pre) <- etaParts n
  , isKnownLambda f =
      -- the application computes what the lambda did, and keeps its index
      -- (an export's root is named by it)
      reduce ai (AnnoS g c (AppS f pre))
  where
    isKnownLambda (AnnoS _ _ (LamS _ _)) = True
    isKnownLambda (AnnoS _ _ (VarS _ (One x))) = isKnownLambda x
    isKnownLambda _ = False
-- The same, with a function value held in a variable as the head: it is the
-- partial application @f pre@, its arguments evaluated once, first; the pool
-- runtime runs @f@'s stage entry when it has one ('PapplyP').
reduce ai n@(AnnoS g@(Idx gIdx _) c (LamS _ _))
  | not ai
  , Just (f@(AnnoS _ _ fe), pre) <- etaParts n
  , isLocalVar fe = do
      (binds, pre') <- unzip <$> mapM (bindArg ai) pre
      let AnnoS (Idx _ lamT) _ _ = n
      wrapLets (Idx gIdx lamT) c (concat binds) (AnnoS g c (AppS f pre'))
  where
    isLocalVar (BndS _) = True
    isLocalVar (LetBndS _) = True
    isLocalVar _ = False
-- The same, with a head that is not a known lambda (a variable, a call): the
-- partial application stays, and its arguments are evaluated once, before
-- the function is built.
reduce ai n@(AnnoS g@(Idx gIdx _) c (LamS vs (AnnoS ga ca (AppS _ xs))))
  | Just (f, pre) <- etaParts n
  , any (not . isValue) pre = do
      (binds, pre') <- unzip <$> mapM (bindArg ai) pre
      let post = drop (length pre) xs
      inner <- reduce ai (AnnoS g c (LamS vs (AnnoS ga ca (AppS f (pre' ++ post)))))
      let AnnoS (Idx _ innerT) _ _ = inner
      wrapLets (Idx gIdx innerT) c (concat binds) inner
-- Push an application through a let in function position. A let-expression
-- whose body evaluates to a function (e.g. a top-level binding written as
-- `f = let v = ... in <function>`) ends up in function position when f is
-- applied. Without this rewrite, a LamS hidden inside the let body never
-- meets its arguments at the AppS-of-LamS pattern below, so beta-reduction
-- is skipped and code generation later errors with "unexpected LamS".
--
--   (let v = e1 in body) args  =>  let v = e1 in (body args)
--
-- The new inner AppS keeps the outer application's index and contextual
-- annotation (g1, c1) since it computes the same value as the original.
-- The outer let keeps its own annotations.
reduce ai (AnnoS g1@(Idx _ appT) c1 (AppS (AnnoS (Idx gLet _) cLet (LetS v e1 body)) es)) =
  reduce ai $
    AnnoS (Idx gLet appT) cLet $
      LetS v e1 (AnnoS g1 c1 (AppS body es))
-- Beta-reduce an applied lambda. An argument is evaluated once, at the
-- application (model/effects.md, law 5), so only a value may be
-- substituted into the body: substituting anything else would evaluate it at
-- each reference, or never if there is none. A value is substituted when its
-- parameter is used at most once (a move: 'substituteAnnoS' reuses the
-- argument for the first occurrence and clones only the extras, which keeps a
-- chain of reductions linear) or on the nexus path (which holds no closures);
-- otherwise it is bound once and shared. Any other argument is bound once by
-- a @let@, whatever the reference count.
reduce ai
  ( AnnoS
      i1@(Idx i1n i1t)
      tb1
      ( AppS
          ( AnnoS
              (Idx i2 (FunT (tv : tas) tb2))
              _
              (LamS (v : vs) e2)
            )
          (e1 : es)
        )
    ) = do
    -- A non-value is normalized here, once, to decide whether it reduces
    -- to a value; the result is reused below.
    let raw = isValue e1
    e1n0 <- if raw then return e1 else reduce ai e1
    -- A suspension built from arguments that are not values: the arguments
    -- are evaluated now, once, and the suspension built from their values.
    (hoisted, e1n) <- hoistThunkArgs e1n0
    let normalized x = if raw then reduce ai x else return x
    -- The reduction's result is what the labeled application computes, so
    -- a label, cache or log setting on the application moves to its root.
    moveConfig i1n =<< wrapLets i1 tb1 hoisted =<< case () of
      _ | isValue e1n && (ai || nrefs <= 1) ->
            substituteAnnoS v e1n e2 >>= reduce ai . rebuild
        | isValue e1n -> share normalized v e1n e2
        -- A function built by a computation, on the nexus path. The pure
        -- nexus evaluator holds data only, never a function value, so the
        -- computation's strict parts are bound once here and what remains,
        -- a choice among function values, is substituted.
        | isFunctionType tv && ai -> do
            pieces <- splitFunctionArg e1n
            case pieces of
              Just (binds, fn) -> substituteSplit binds fn
              Nothing ->
                MM.throwSourcedError (annIdx e1n)
                  "This argument computes a function, and a command evaluated without a language pool cannot hold a function value; import a language so the command runs in a pool, or pass the function directly."
        -- A function built by a computation, in a pool: its strict parts are
        -- bound once and the remaining choice among values is substituted;
        -- failing that, it is computed once into a closure, and every use
        -- receives a lambda that calls it.
        | isFunctionType tv -> splitFunctionArg e1n >>= \pieces -> case pieces of
          Just (binds, fn) -> substituteSplit binds fn
          Nothing -> do
            f <- freshClosureName v
            lam <- etaCall f tv e1n
            e2' <- substituteAnnoS v lam e2
            inner <- reduce ai (rebuild e2')
            letIx <- newPlainIndex i1n
            return (AnnoS (Idx letIx i1t) tb1 (LetS f e1n inner))
        -- A container holding function values, on the nexus path: its
        -- elements are bound once, in order, and the container of their
        -- values is substituted, so no function value is ever held.
        | ai && containsFunT tv -> splitFunctionArg e1n >>= \pieces -> case pieces of
          Just (binds, x) -> substituteSplit binds x
          Nothing -> share normalized v e1n e2
        | otherwise -> share normalized v e1n e2
  where
    nrefs = countRefs v e2
    annIdx (AnnoS (Idx gi _) _ _) = gi
    substituteSplit binds fn = do
      e2' <- substituteAnnoS v fn e2
      inner <- reduce ai (rebuild e2')
      wrapLets i1 tb1 binds inner
    -- Bind the argument once as @let v = e1 in e2@ (references rebound to
    -- let-form; see 'rebindBndToLet'). @!@ / @<-@ forces are hoisted into their
    -- own let bindings upstream (Restructure.hoistEvals, Desugar.desugarDo), so
    -- @e1@ is never a raw forced effect.
    share normalized x e1n body = do
      e1' <- normalized e1n
      e2r <- rebindBndToLet x body
      inner <- reduce ai (rebuild e2r)
      letIx <- newPlainIndex i1n
      return (AnnoS (Idx letIx i1t) tb1 (LetS x e1' inner))
    -- The residual application with one parameter/argument pair consumed.
    -- The residual lambda is a value built at the application site, so it
    -- carries the application's annotation (its language), not the
    -- definition's: left partially applied, it is the closure this site
    -- constructs.
    rebuild body =
      AnnoS i1 tb1 (AppS (AnnoS (Idx i2 (FunT tas tb2)) tb1 (LamS vs body)) es)
-- Normalize a computed-function head before applying. The head may reduce to a
-- 'LetS' or 'LamS' only AFTER its own lambda-evaluation -- e.g. forcing an
-- effectful do-block, @!{ _ <- eff; \\y -> .. }@ applied, cancels
-- @EvalS (DoBlockS ..)@ (below) to @let _ = !eff in \\y -> ..@. The structural
-- recursion at the bottom would process such a head but not re-examine the
-- application, leaving a 'LetS'/'LamS' in function position that codegen
-- rejects. Process the head first; if it became a let or lambda, re-dispatch so
-- push-through / beta-reduction fires. Heads that stay a bound variable, a
-- source call, or a forced non-do-block ('LetBndS', 'BndS', 'EvalS' of a plain
-- value) fall through unchanged and are handled at codegen.
reduce ai (AnnoS g c (AppS headA es)) = do
  headA' <- reduce ai headA
  case headA' of
    AnnoS _ _ (LetS {}) -> reduce ai (AnnoS g c (AppS headA' es))
    AnnoS _ _ (LamS {}) -> reduce ai (AnnoS g c (AppS headA' es))
    -- Forcing an effectful generator whose result is a function --
    -- @!(let v = !eff in \\y -> ..) x@ -- leaves an @EvalS (LetS ..)@ head
    -- whose body is a lambda. Push the application through the let so the
    -- lambda meets its arguments (and beta-reduces); the effect stays forced by
    -- the let's own bound @!eff@. Without this the function-typed let reaches
    -- the eta path in 'express', which re-applies it and rejects the LetS head.
    AnnoS _ _ (EvalS (AnnoS (Idx gLet _) cLet (LetS v e1 body))) ->
      reduce ai $ AnnoS (Idx gLet (annT g)) cLet $ LetS v e1 (AnnoS g c (AppS body es))
    -- A conditional choosing a function, applied: the condition is evaluated
    -- once, then the chosen function is applied to the arguments, each
    -- evaluated once before the choice.
    AnnoS _ _ (IfS cond th el) -> do
      (binds, es') <- unzip <$> mapM (bindArg ai) es
      esCopy <- mapM reindexTree es'
      let Idx _ appT = g
          branch b@(AnnoS (Idx bi _) bc _) xs = do
            ix <- newPlainIndex bi
            reduce ai (AnnoS (Idx ix appT) bc (AppS b xs))
      th' <- branch th es'
      el' <- branch el esCopy
      wrapLets g c (concat binds) (AnnoS g c (IfS cond th' el'))
    _ -> do
      es' <- mapM (reduce ai) es
      return $ case (headA', es') of
        (AnnoS _ _ (ExeS (PatCall (PatternStruct sel))), [x])
          | ai, Just (AnnoS _ cy y) <- project sel x -> AnnoS g cy y
        _ -> AnnoS g c (AppS headA' es')
  where
    annT (Idx _ t) = t
    -- on the nexus path, a field of a container literal that computes
    -- nothing is the element itself, so a function it holds reaches its
    -- uses as a lambda
    project sel x
      | isValue x = field sel x
      | otherwise = Nothing
    field SelectorEnd x = Just x
    field (SelectorIdx (i, sub) []) (AnnoS _ _ (TupS xs))
      | i >= 0, i < length xs = field sub (xs !! i)
    field (SelectorKey (k, sub) []) (AnnoS _ _ (NamS rs)) = lookup (Key k) rs >>= field sub
    field _ _ = Nothing
-- Inline let-bound lambdas (a right-hand side that reduces to one included),
-- using the same inline-vs-share @countRefs@ guard as the beta-redex clause
-- above. A singly-used lambda is beta-reduced away; a multiply-used one is
-- kept shared (each reference a 'LetBndS' lowered to a native closure call,
-- 'LocalCallP'). On the nexus path (@ai@) it is always inlined, since the
-- pure evaluator has no native closure to share.
reduce ai (AnnoS g c (LetS v e1 e2)) = do
  e1' <- reduce ai e1
  if isLam e1' && (ai || countRefs v e2 <= 1)
    then do
      inner <- substituteAnnoS v e1' e2 >>= reduce ai
      let AnnoS _ _ innerExpr = inner
      return (AnnoS g c innerExpr)
    else AnnoS g c . LetS v e1' <$> reduce ai e2
-- Cancel force-suspend: !{e} --> e. Keep the OUTER general type (the
-- EvalS already strips the effect wrapper) but the INNER concrete
-- annotation, so the chain's chosen language survives fusion.
reduce ai (AnnoS g _ (EvalS (AnnoS _ cInner (DoBlockS e)))) = do
  e' <- reduce ai e
  let AnnoS _ _ inner = e'
  return (AnnoS g cInner inner)
-- Every other node: recurse structurally.
reduce ai (AnnoS g c e) = AnnoS g c <$> mapExprSM (reduce ai) e

-- | A computed function value, split into the data it must evaluate now (a
-- let's right-hand side, a conditional's condition, a container's elements)
-- and a remainder that computes nothing when substituted: a value, a
-- conditional choosing among values, or a container of values. 'Nothing'
-- when the computation has no such form.
splitFunctionArg ::
  AnnoS (Indexed Type) One a ->
  MorlocMonad (Maybe ([(EVar, AnnoS (Indexed Type) One a)], AnnoS (Indexed Type) One a))
splitFunctionArg x@(AnnoS (Idx gi t) c ex)
  | isValue x = return (Just ([], x))
  | otherwise = case ex of
      LetS w rhs body -> fmap (\(bs, body') -> ((w, rhs) : bs, body')) <$> splitFunctionArg body
      IfS cond th el
        | Just th' <- choice th
        , Just el' <- choice el -> do
            (bc, cond') <- bindData cond
            return (Just (bc, AnnoS (Idx gi t) c (IfS cond' th' el')))
      TupS xs -> Just <$> bindAll xs (AnnoS (Idx gi t) c . TupS)
      LstS xs -> Just <$> bindAll xs (AnnoS (Idx gi t) c . LstS)
      NamS rs -> Just <$> bindAll (map snd rs) (AnnoS (Idx gi t) c . NamS . zip (map fst rs))
      _ -> return Nothing
  where
    bindAll xs rebuildWith = do
      pieces <- mapM bindData xs
      return (concatMap fst pieces, rebuildWith (map snd pieces))
    choice y@(AnnoS g' c' ey)
      | isValue y = Just y
      | IfS cond th el <- ey, isValue cond = do
          th' <- choice th
          el' <- choice el
          return (AnnoS g' c' (IfS cond th' el'))
      | otherwise = Nothing

-- | A value as it is, or a fresh let-bound variable for anything else, with
-- its binding.
bindData ::
  AnnoS (Indexed Type) One a ->
  MorlocMonad ([(EVar, AnnoS (Indexed Type) One a)], AnnoS (Indexed Type) One a)
bindData y@(AnnoS (Idx gi t) c _)
  | isValue y = return ([], y)
  | otherwise = do
      name <- freshClosureName (EV "arg")
      refIx <- newPlainIndex gi
      return ([(name, y)], AnnoS (Idx refIx t) c (LetBndS name))

isLam :: AnnoS g f c -> Bool
isLam (AnnoS _ _ (LamS _ _)) = True
isLam _ = False

-- | Move the manifold configuration at index @from@ to the root of @e@, when
-- the root is a different node.
moveConfig :: Int -> AnnoS (Indexed Type) One a -> MorlocMonad (AnnoS (Indexed Type) One a)
moveConfig from e@(AnnoS (Idx to _) _ _)
  | from == to = return e
  | otherwise = do
      MM.modify $ \s -> case Map.lookup from (stateManifoldConfig s) of
        Nothing -> s
        Just cfg ->
          s { stateManifoldConfig = Map.insert to cfg (Map.delete from (stateManifoldConfig s)) }
      return e

-- | A fresh index carrying @parent@'s index-keyed state except its manifold
-- configuration: a node the reduction introduces (a let, an eta-expansion)
-- computes nothing the user labeled, so a label, cache or log setting on the
-- expression it came from stays with that expression alone.
newPlainIndex :: Int -> MorlocMonad Int
newPlainIndex parent = do
  i <- newIndex parent
  MM.modify (\s -> s {stateManifoldConfig = Map.delete i (stateManifoldConfig s)})
  return i

-- | Split a suspension-building application into the arguments that are not
-- values (to be bound by lets, returned in order) and the application of the
-- function to values. Anything else is returned unchanged.
hoistThunkArgs ::
  AnnoS (Indexed Type) One a ->
  MorlocMonad ([(EVar, AnnoS (Indexed Type) One a)], AnnoS (Indexed Type) One a)
hoistThunkArgs e@(AnnoS g@(Idx _ (EffectT _ _)) c (AppS f xs))
  | isValue f && not (all isValue xs) = do
      pieces <- mapM hoist xs
      return (concatMap fst pieces, AnnoS g c (AppS f (map snd pieces)))
  | otherwise = return ([], e)
  where
    hoist x
      | isValue x = return ([], x)
      | otherwise = do
          -- a nested suspension builder: its own arguments first
          (inner, x') <- hoistThunkArgs x
          (bs, y) <- bindData x'
          return (inner <> bs, y)
hoistThunkArgs e = return ([], e)

-- | @let v1 = e1 in ... let vn = en in body@, each let annotated like the
-- application it came from.
wrapLets ::
  Indexed Type -> a -> [(EVar, AnnoS (Indexed Type) One a)] -> AnnoS (Indexed Type) One a ->
  MorlocMonad (AnnoS (Indexed Type) One a)
wrapLets _ _ [] body = return body
wrapLets i@(Idx gi t) c ((v, rhs) : rest) body = do
  inner <- wrapLets i c rest body
  letIx <- newPlainIndex gi
  return (AnnoS (Idx letIx t) c (LetS v rhs inner))

isFunctionType :: Type -> Bool
isFunctionType (FunT _ _) = True
isFunctionType _ = False

-- | A copy of a tree with fresh indices carrying the state of those they copy.
--
-- A staged lambda inside the tree is staged in the copy too: its stage entry
-- is the copy of its stage entry.
reindexTree :: AnnoS (Indexed Type) One a -> MorlocMonad (AnnoS (Indexed Type) One a)
reindexTree t0 = do
  renamedRef <- MM.liftIO (newIORef Map.empty)
  let go (AnnoS (Idx gi t) c e) = do
        gi' <- newIndex gi
        MM.liftIO (modifyIORef renamedRef (Map.insert gi gi'))
        AnnoS (Idx gi' t) c <$> mapExprSM go e
  t' <- go t0
  renamed <- MM.liftIO (readIORef renamedRef)
  entries <- MM.gets stateStageEntries
  let copied =
        Map.fromList
          [ (gi', (k, s'))
          | (gi, gi') <- Map.toList renamed
          , Just (k, s) <- [Map.lookup gi entries]
          , Just s' <- [Map.lookup s renamed]
          ]
  MM.modify (\st -> st {stateStageEntries = Map.union copied (stateStageEntries st)})
  return t'

-- | A fresh let-name for the closure a function-typed argument computes.
freshClosureName :: EVar -> MorlocMonad EVar
freshClosureName (EV v) = do
  k <- MM.getCounter
  return (EV (v <> "`c" <> MT.show' k))

-- | @\ys -> f ys@ at type @t@: a lambda value that calls the let-bound
-- closure @f@. Built with the annotations of the expression @f@ is bound to.
etaCall :: EVar -> Type -> AnnoS (Indexed Type) One a -> MorlocMonad (AnnoS (Indexed Type) One a)
etaCall f t@(FunT ins out) (AnnoS (Idx gi _) c _) = do
  ys <- mapM (const (freshClosureName (EV "eta"))) ins
  lamIx <- newPlainIndex gi
  appIx <- newPlainIndex gi
  headIx <- newPlainIndex gi
  argIxs <- mapM (\_ -> newPlainIndex gi) ins
  let args = [AnnoS (Idx ix ty) c (BndS y) | (ix, ty, y) <- zip3 argIxs ins ys]
      call = AnnoS (Idx appIx out) c (AppS (AnnoS (Idx headIx t) c (LetBndS f)) args)
  return (AnnoS (Idx lamIx t) c (LamS ys call))
-- not a function type: the closure is referenced directly
etaCall f _ (AnnoS (Idx gi t) c _) = do
  headIx <- newPlainIndex gi
  return (AnnoS (Idx headIx t) c (LetBndS f))

-- | Count free references to @v@, using the same shadowing rules as
-- 'substituteAnnoS' -- i.e. the number of sites 'substituteAnnoS' would
-- replace. Recursion stops at any binder that shadows @v@ (a lambda binding
-- @v@, or an inner let of @v@), matching the substitution's shadow handling.
countRefs :: EVar -> AnnoS (Indexed Type) One a -> Int
countRefs v = go
  where
    go (AnnoS _ _ (BndS v'))     | v == v'     = 1
    go (AnnoS _ _ (LetBndS v'))  | v == v'     = 1
    go (AnnoS _ _ (LamS vs _))   | v `elem` vs = 0
    go (AnnoS _ _ (LetS v' e1 _)) | v == v'    = go e1
    go (AnnoS _ _ e)                           = getSum (foldExprS (Sum . go) e)

-- | Traverse every FREE reference to @v@ (as 'BndS' or 'LetBndS'), applying
-- @act@ to each such node; stop at any binder that shadows @v@ (a 'LamS'
-- binding @v@, or an inner 'LetS' of @v@). This is the shared "free occurrence
-- of @v@" rule behind 'substituteAnnoS' and 'rebindBndToLet' (and, as a fold,
-- 'countRefs') -- keeping it in one place guarantees they agree on exactly
-- which references they touch, the correctness precondition of the
-- 'reduce' share branch.
onFreeRef ::
  EVar ->
  (AnnoS (Indexed Type) One a -> MorlocMonad (AnnoS (Indexed Type) One a)) ->
  AnnoS (Indexed Type) One a ->
  MorlocMonad (AnnoS (Indexed Type) One a)
onFreeRef v act = f
  where
    f e0@(AnnoS _ _ (BndS v'))     | v == v'     = act e0
    f e0@(AnnoS _ _ (LetBndS v'))  | v == v'     = act e0
    f e0@(AnnoS _ _ (LamS vs _))   | v `elem` vs = return e0  -- shadowed
    -- a non-recursive let shadows v in its body, not in its right-hand side
    f (AnnoS g c (LetS v' e1 e2)) | v == v'      = (\e1' -> AnnoS g c (LetS v' e1' e2)) <$> f e1
    f (AnnoS g c e)                              = AnnoS g c <$> mapExprSM f e

-- | Substitute every free reference to @v@ with @r@. The FIRST free occurrence
-- reuses @r@ as-is (a move, no copy); every SUBSEQUENT occurrence gets a
-- REINDEXED copy with fresh manifold ids. Reindexing exists only to stop
-- several inserted copies from collapsing to one manifold at codegen (each site
-- would then see the first site's value, e.g. a producer
-- @\\sink -> do { sink a; sink b }@); a single occurrence has nothing to
-- disambiguate, so it is moved rather than copied. Reusing the original for one
-- site and cloning only the extras keeps a chain of reductions linear -- a
-- singly-used parameter never re-copies its (growing) argument -- while still
-- giving every extra site a distinct manifold. Correct for any reference count,
-- so callers need not branch on it. See the module header ("Substitution
-- requires reindexing").
substituteAnnoS ::
  EVar ->
  AnnoS (Indexed Type) One a ->
  AnnoS (Indexed Type) One a ->
  MorlocMonad (AnnoS (Indexed Type) One a)
substituteAnnoS v r target = do
  reused <- MM.liftIO (newIORef False)
  let place _ = do
        seen <- MM.liftIO (readIORef reused)
        if seen
          then reindexAnnoS r
          else MM.liftIO (writeIORef reused True) >> return r
  onFreeRef v place target

-- | Rebind @v@ from lambda-form to let-form: rewrite each free reference to
-- @LetBndS v@, keeping its index and type. Used by the 'reduce' share
-- branch when it turns @(\\v -> e2) e1@ into @let v = e1 in e2@: @v@ was a
-- lambda parameter, but a let variable is lowered through 'express''s
-- 'LetBndS' clause.
rebindBndToLet ::
  EVar ->
  AnnoS (Indexed Type) One a ->
  MorlocMonad (AnnoS (Indexed Type) One a)
rebindBndToLet v = onFreeRef v (\(AnnoS g c _) -> return (AnnoS g c (LetBndS v)))

-- | Assign fresh indices to every node of an 'AnnoS' subtree, preserving each
-- node's type annotation. Uses 'newIndex', which copies ALL index-keyed state
-- (source map, manifold config/label, name, signatures, ...) from the old
-- index to the fresh one -- so an inserted copy keeps its log label, cache
-- config, and diagnostics, not just its srcloc. Used by 'substituteAnnoS' so
-- each inserted copy of a definition/lambda gets distinct manifold ids while
-- retaining its per-manifold metadata.
reindexAnnoS :: AnnoS (Indexed Type) One a -> MorlocMonad (AnnoS (Indexed Type) One a)
reindexAnnoS (AnnoS (Idx i t) c e) = do
  i' <- newIndex i
  e' <- mapExprSM reindexAnnoS e
  return (AnnoS (Idx i' t) c e')
