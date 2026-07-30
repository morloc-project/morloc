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
  ) where

import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.Grammars.Common (propagateManifoldLabel)
import Morloc.Frontend.Namespace (newIndex)
import qualified Morloc.Monad as MM
import Data.IORef (newIORef, readIORef, writeIORef)

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
-- Thus applyLambdas is done here, rather than in Treeify.hs or Desugar.hs.
--
-- Lambda application can also NOT be done before collapsing from Many to One in
-- AnnoS. The reason is that in ((VarS (Many es)) 42), the values in es
-- may contain `CallS src` or `LamS vs e` types. The CallS terms cannot be
-- reduced but the lambdas can. So applying here would lead to divergence.
--
-- It also must be done BEFORE conversion to ExprM in `express`, where manifolds
-- are resolved.
-- -}
applyLambdas ::
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
applyLambdas ai (AnnoS g1@(Idx g1Idx _) _ (AppS (AnnoS (Idx lamIdx _) _ (LamS [] (AnnoS _ c2 e))) [])) = do
  void (propagateManifoldLabel g1Idx lamIdx)
  applyLambdas ai $ AnnoS g1 c2 e
-- Over-applied curried lambda. Beta-reducing `(\base -> \y -> ..) 3 x`
-- consumes `base`, leaving `AppS (LamS [] (\y -> ..)) [x]` -- an empty
-- lambda layer still standing between the remaining args and the function
-- it returns. Unwrap it so the inner lambda meets the leftover args (the
-- empty-args clause above only fires when no args remain, so without this
-- the LamS survives to codegen and errors with "unexpected LamS").
applyLambdas ai (AnnoS g1@(Idx g1Idx _) c1 (AppS (AnnoS (Idx lamIdx _) _ (LamS [] body)) es@(_ : _))) = do
  void (propagateManifoldLabel g1Idx lamIdx)
  applyLambdas ai $ AnnoS g1 c1 (AppS body es)
applyLambdas ai (AnnoS g1@(Idx g1Idx _) _ (AppS (AnnoS (Idx headIdx _) c2 e) [])) = do
  void (propagateManifoldLabel g1Idx headIdx)
  applyLambdas ai $ AnnoS g1 c2 e
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
applyLambdas ai (AnnoS g1 c1 (AppS (AnnoS gLet cLet (LetS v e1 body)) es)) =
  applyLambdas ai $
    AnnoS gLet cLet $
      LetS v e1 (AnnoS g1 c1 (AppS body es))
-- Beta-reduce an applied lambda. A singly-used parameter is substituted into
-- the body (inline branch); a multiply-used one is bound once as a shared @let@
-- (share branch, each use a distinct 'LocalCallP'). 'substituteAnnoS' reuses the
-- argument for the first occurrence and clones only the extras, so inlining a
-- singly-used parameter is a move (no copy) -- the property that keeps a chain
-- of reductions from duplicating its argument multiplicatively (2^depth). The
-- 'usedAsForeignCallback' exception keeps a function-typed argument to a foreign
-- source call on the inline path, where 'EffectBoundary' can force its effect at
-- each callback site.
applyLambdas ai
  ( AnnoS
      i1@(Idx i1n i1t)
      tb1
      ( AppS
          ( AnnoS
              (Idx i2 (FunT (_tv : tas) tb2))
              c
              (LamS (v : vs) e2)
            )
          (e1 : es)
        )
    )
    -- Testing @nrefs <= 1@ before 'usedAsForeignCallback' short-circuits the
    -- common single-use case without its extra traversal.
  | ai || nrefs <= 1 || usedAsForeignCallback v e2 =
      substituteAnnoS v e1 e2 >>= applyLambdas ai . rebuild
  -- Share: bind the argument once as @let v = e1 in e2@ (references rebound to
  -- let-form; see 'rebindBndToLet'). Effect-equivalent, not effect-changing:
  -- @!@ / @<-@ forces are hoisted into their own let bindings upstream
  -- (Restructure.hoistEvals, Desugar.desugarDo), so @e1@ is never a raw forced
  -- effect -- only pure data, a thunk-constructing call, or a lambda -- and
  -- constructing it once versus many times is unobservable.
  | otherwise = do
      e1' <- applyLambdas ai e1
      e2r <- rebindBndToLet v e2
      inner <- applyLambdas ai (rebuild e2r)
      letIx <- newIndex i1n
      return (AnnoS (Idx letIx i1t) tb1 (LetS v e1' inner))
  where
    nrefs = countRefs v e2
    -- The residual application with one parameter/argument pair consumed.
    rebuild body =
      AnnoS i1 tb1 (AppS (AnnoS (Idx i2 (FunT tas tb2)) c (LamS vs body)) es)
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
applyLambdas ai (AnnoS g c (AppS headA es)) = do
  headA' <- applyLambdas ai headA
  case headA' of
    AnnoS _ _ (LetS {}) -> applyLambdas ai (AnnoS g c (AppS headA' es))
    AnnoS _ _ (LamS {}) -> applyLambdas ai (AnnoS g c (AppS headA' es))
    -- Forcing an effectful generator whose result is a function --
    -- @!(let v = !eff in \\y -> ..) x@ -- leaves an @EvalS (LetS ..)@ head
    -- whose body is a lambda. Push the application through the let so the
    -- lambda meets its arguments (and beta-reduces); the effect stays forced by
    -- the let's own bound @!eff@. Without this the function-typed let reaches
    -- the eta path in 'express', which re-applies it and rejects the LetS head.
    AnnoS _ _ (EvalS (AnnoS gLet cLet (LetS v e1 body))) ->
      applyLambdas ai $ AnnoS gLet cLet $ LetS v e1 (AnnoS g c (AppS body es))
    _ -> AnnoS g c . AppS headA' <$> mapM (applyLambdas ai) es
-- Inline let-bound lambdas, using the same inline-vs-share @countRefs@ guard as
-- the beta-redex clause above. A singly-used lambda is beta-reduced away; a
-- multiply-used one is kept shared (each reference a 'LetBndS' lowered to a
-- native closure call, 'LocalCallP'). On the nexus path (@ai@) it is always
-- inlined, since the pure evaluator has no native closure to share.
applyLambdas ai (AnnoS g c (LetS v e1@(AnnoS _ _ (LamS _ _)) e2))
  | ai || countRefs v e2 <= 1 = do
      e1' <- applyLambdas ai e1
      e2' <- substituteAnnoS v e1' e2
      inner <- applyLambdas ai e2'
      let AnnoS _ _ innerExpr = inner
      return (AnnoS g c innerExpr)
  | otherwise = do
      e1' <- applyLambdas ai e1
      e2' <- applyLambdas ai e2
      return (AnnoS g c (LetS v e1' e2'))
-- Cancel force-suspend: !{e} --> e. Keep the OUTER general type (the
-- EvalS already strips the effect wrapper) but the INNER concrete
-- annotation, so the chain's chosen language survives fusion.
applyLambdas ai (AnnoS g _ (EvalS (AnnoS _ cInner (DoBlockS e)))) = do
  e' <- applyLambdas ai e
  let AnnoS _ _ inner = e'
  return (AnnoS g cInner inner)
-- Every other node: recurse structurally.
applyLambdas ai (AnnoS g c e) = AnnoS g c <$> mapExprSM (applyLambdas ai) e

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
    go (AnnoS _ _ (LetS v' _ _)) | v == v'     = 0
    go (AnnoS _ _ e)                           = getSum (foldExprS (Sum . go) e)

-- | Traverse every FREE reference to @v@ (as 'BndS' or 'LetBndS'), applying
-- @act@ to each such node; stop at any binder that shadows @v@ (a 'LamS'
-- binding @v@, or an inner 'LetS' of @v@). This is the shared "free occurrence
-- of @v@" rule behind 'substituteAnnoS' and 'rebindBndToLet' (and, as a fold,
-- 'countRefs') -- keeping it in one place guarantees they agree on exactly
-- which references they touch, the correctness precondition of the
-- 'applyLambdas' share branch.
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
    f e0@(AnnoS _ _ (LetS v' _ _)) | v == v'     = return e0  -- shadowed
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
-- @LetBndS v@, keeping its index and type. Used by the 'applyLambdas' share
-- branch when it turns @(\\v -> e2) e1@ into @let v = e1 in e2@: @v@ was a
-- lambda parameter, but a let variable is lowered through 'express''s
-- 'LetBndS' clause.
rebindBndToLet ::
  EVar ->
  AnnoS (Indexed Type) One a ->
  MorlocMonad (AnnoS (Indexed Type) One a)
rebindBndToLet v = onFreeRef v (\(AnnoS g c _) -> return (AnnoS g c (LetBndS v)))

-- | Is @v@ passed directly as a FUNCTION-typed argument to a foreign source
-- call anywhere in the body? Such a closure is invoked as @f(x)@ inside the
-- foreign source implementation, which discards the effect thunk, so its effect
-- must be forced eagerly at the callback boundary
-- ('EffectBoundary.maybeForceCallbackArg') -- which cannot be done at a shared
-- use site. So these stay on the clone path (each clone becomes a direct
-- callback argument); every other multiply-used parameter is shared.
-- Conservatively over-approximating among function-typed args (shadowing is not
-- tracked): a false positive merely clones, which is safe.
usedAsForeignCallback :: EVar -> AnnoS (Indexed Type) One a -> Bool
usedAsForeignCallback v = go
  where
    go (AnnoS _ _ (AppS (AnnoS _ _ (ExeS (SrcCall _))) args)) | any isRef args = True
    go (AnnoS _ _ e) = getAny (foldExprS (Any . go) e)
    -- a FUNCTION-typed ref only: a data argument to a source call is never
    -- invoked, so it must not be diverted onto the clone path. Descend into a
    -- structured argument (list/tuple/record of closures) too, so a callback
    -- nested there is also kept inline -- 'EffectBoundary.maybeForceCallbackArg'
    -- forces such nested closures at the same boundary.
    isRef (AnnoS (Idx _ (FunT _ _)) _ (BndS v'))    = v == v'
    isRef (AnnoS (Idx _ (FunT _ _)) _ (LetBndS v')) = v == v'
    isRef (AnnoS _ _ (LstS es))                     = any isRef es
    isRef (AnnoS _ _ (TupS es))                     = any isRef es
    isRef (AnnoS _ _ (NamS rs))                     = any (isRef . snd) rs
    isRef _ = False

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
