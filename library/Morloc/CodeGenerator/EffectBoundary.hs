{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.CodeGenerator.EffectBoundary
Description : Central force/suspend insertion at effect boundaries
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

A suspension @<E> T@ is a value distinct from @T@: a thunk of a
computation, a closure of no arguments in every pool. Only a force runs
it, and every force runs it again. The one place eagerness enters is the
host adapter: a host language has no thunks, so at a sourced function the
compiler suspends the host call (its result is declared @<E> T@) and runs
a morloc callback's result for the host ('CallbackReturn'); at the
program boundary it runs the root of a command's result and builds a
constant suspension from a command's argument.

Every position in the codegen IR sits under some /boundary/ -- the
outermost export return, an argument to a foreign consumer, the receive
slot of a cross-language RPC, etc. Each boundary imposes a calling
convention on the value at that position: either the result of a run
(a plain 'T') or the suspension itself.

The module centralises the invariant with two passes:

  * 'insertEffectBoundaries' walks the 'PolyHead' tree and inserts
    'PolyEval' / 'PolyDoBlock' wherever the declared type disagrees
    with the boundary's calling convention. Peephole cancellation folds
    'Force . Suspend' pairs back to identity.

  * 'checkEffectBoundaries' asserts every plain-expecting boundary has
    no residual 'EffectT' in the declared type. Any violation is a
    compiler bug -- either the insertion pass missed a row or the row's
    rewrite is incorrect.

'insertExportBoundaries' is a specialised entry point called from
'Express.express' with the export function's 'Type' explicit (it is not
stored on 'PolyHead'). It handles the two boundaries unique to exports:
'ExportArg' Suspend for thunk-typed input args and 'ExportRoot' Force
for the return position.
-}
module Morloc.CodeGenerator.EffectBoundary
  ( BoundaryContext (..)
  , checkEffectBoundaries
  , insertEffectBoundaries
  , insertExportBoundaries
  , boundaryExpectsPlain
  , polyOuterType
  ) where

import Morloc.CodeGenerator.Namespace
import qualified Morloc.Data.GMap as GMap
import Morloc.Data.Doc
import qualified Morloc.Monad as MM

-- | The calling convention imposed by the surrounding boundary.
data BoundaryContext
  = -- | Outermost manifold body of an export; the return value is
    -- serialized to the wire.
    ExportRoot
  | -- | Inner non-export manifold body in the same pool; the return
    -- value flows to the enclosing PolyExpr, which imposes its own
    -- calling convention. Pass-through here.
    LocalRoot
  | -- | Receiver-side of a 'PolyRemoteInterface': the callee manifold's
    -- return value is serialized to the wire.
    ForeignCalleeReturn
  | -- | Caller-side receive of a 'PolyRemoteInterface' application: the
    -- deserialized wire value must match the enclosing consumer's
    -- expected type. Pass-through here; the enclosing context imposes
    -- the convention.
    ForeignCallerReceive
  | -- | Return of a lambda passed as an argument to a foreign consumer
    -- (which invokes it as @f(x)@ and expects the effect to fire).
    CallbackReturn
  | -- | Return of a user-declared foreign source function whose
    -- signature carries an outer '<E> T'. The user's source
    -- implementation is eager (returns 'T'), so the value at this
    -- position must be suspended to match the declared type.
    SourceCall
  | -- | Input to a wire serializer ('SerializeS', 'AppPoolS' arg
    -- position). A suspension here crosses as a closure, like any
    -- function value; nothing is forced.
    SerializeSink
  deriving (Eq, Show)

-- | Does this boundary demand a plain value (as opposed to a thunk)?
--
-- 'True'  means a thunk-shaped value at this position needs 'PolyEval'.
-- 'False' with a plain value at a position whose type is declared '<E>
-- T' needs 'PolyDoBlock'; other 'False' cases are pass-through.
boundaryExpectsPlain :: BoundaryContext -> Bool
boundaryExpectsPlain ExportRoot           = True
boundaryExpectsPlain LocalRoot            = False
boundaryExpectsPlain ForeignCalleeReturn  = True
boundaryExpectsPlain ForeignCallerReceive = False
boundaryExpectsPlain CallbackReturn       = True
boundaryExpectsPlain SourceCall           = False
boundaryExpectsPlain SerializeSink        = False

-- | The outer 'Type' of a 'PolyExpr' node, if derivable. 'PolyApp'
-- collapses a 'FunT'-typed head to its return. 'PolyIf' folds branches
-- preferring the effect-typed one so a surrounding 'PolyEval' stays
-- load-bearing when either branch produces a thunk.
polyOuterType :: PolyExpr -> Maybe Type
polyOuterType (PolyRemoteInterface _ (Idx _ t) _ _ _) = Just t
polyOuterType (PolyExe (Idx _ t) _)                   = Just t
polyOuterType (PolyBndVar (C (Idx _ t)) _)            = Just t
polyOuterType (PolyBndVar (B t) _)                    = Just t
polyOuterType (PolyLetVar (Idx _ t) _)                = Just t
polyOuterType (PolyDoBlock (Idx _ t) _)               = Just t
polyOuterType (PolyEval (Idx _ t) _)                  = Just t
polyOuterType (PolyCoerce _ (Idx _ t) _)              = Just t
polyOuterType (PolyIntrinsic (Idx _ t) _ _)           = Just t
polyOuterType (PolyNull (Idx _ t))                    = Just t
polyOuterType (PolyManifold _ _ _ _ body)             = polyOuterType body
polyOuterType (PolyReturn body)                       = polyOuterType body
polyOuterType (PolyLet _ _ body)                      = polyOuterType body
polyOuterType (PolyCacheBody _ _ _ body)              = polyOuterType body
polyOuterType (PolyDebugWrap _ _ body)                = polyOuterType body
polyOuterType (PolyApp fn _) = case polyOuterType fn of
  Just (FunT _ ret) -> Just ret
  other             -> other
polyOuterType (PolyIf _ t e) = case polyOuterType t of
  Just tp | hasOuterEffect tp -> Just tp
  _                           -> polyOuterType e
-- A loop's outer type is its base-case return type (the continue produces
-- no value); it is carried on the body's base branch.
-- A loop's outer type is its BASE-case return type. Walk to a base leaf,
-- skipping the continue leaves (control flow, not values); after
-- 'insertEffectBoundaries' forces the bases, a base leaf is a 'PolyEval' whose
-- inner (plain) type is reported, so 'ExportRoot' sees a plain value.
polyOuterType (PolyLoop _ _ body) = loopBaseType body
  where
    loopBaseType (PolyIf _ t e) = case loopBaseType t of
      Just x -> Just x
      Nothing -> loopBaseType e
    loopBaseType (PolyLet _ _ b) = loopBaseType b
    loopBaseType (PolyDoBlock _ b) = loopBaseType b
    loopBaseType (PolyReturn x) = loopBaseType x
    loopBaseType (PolyLoopContinue _) = Nothing
    loopBaseType leaf = polyOuterType leaf
polyOuterType _                                       = Nothing

-- | Does the outer layer of the type declare an effect?
hasOuterEffect :: Type -> Bool
hasOuterEffect (EffectT _ _) = True
hasOuterEffect _             = False

-- | Post-'insertEffectBoundaries' invariant check. Walks the tree; at
-- every 'plain'-expecting boundary ('ExportRoot',
-- 'ForeignCalleeReturn', 'CallbackReturn') the
-- declared type must have no outer 'EffectT'. Any violation is a
-- compiler bug -- either the insertion pass missed a row or the row's
-- rewrite is incorrect.
--
-- Runs at the same pipeline stage as insertion (Poly, before Segment).
-- Failure surfaces at @stack test@ time with a call-stack trace,
-- naming the manifold and boundary that broke the invariant.
checkEffectBoundaries :: PolyHead -> MorlocMonad ()
checkEffectBoundaries (PolyHead _ midx _ body) = walk midx ExportRoot body

walk :: Int -> BoundaryContext -> PolyExpr -> MorlocMonad ()
walk m ctx e = do
  case polyOuterType e of
    Just t | mismatch ctx t -> throwBoundaryBug m ctx t e
    _ -> return ()
  descend m ctx e

-- | Does the declared type at this position violate the boundary's
-- expected calling convention?
mismatch :: BoundaryContext -> Type -> Bool
mismatch ctx t
  | boundaryExpectsPlain ctx = hasOuterEffect t
  -- The pass-through boundaries (LocalRoot / ForeignCallerReceive /
  -- SourceCall) require declared-vs-value agreement to be checked at
  -- the child, not at the parent, since the parent itself is transparent
  -- to the convention.
  | otherwise = False

throwBoundaryBug :: Int -> BoundaryContext -> Type -> PolyExpr -> MorlocMonad a
throwBoundaryBug m ctx t node =
  MM.throwCompilerBug $
    "EffectBoundary invariant violated at manifold m"
      <> pretty m <> ":\n"
      <> "  boundary : " <> viaShow ctx <> "\n"
      <> "  declared : " <> pretty t <> "\n"
      <> "  node     : " <> pretty node <> "\n"
      <> "  expected : a plain (non-'EffectT') value at this position.\n"
      <> "Root cause: 'insertEffectBoundaries' did not reconcile this position.\n"
      <> "Fix: add or correct the row of the boundary table in\n"
      <> "     Morloc.CodeGenerator.EffectBoundary that covers this shape."

-- | Recurse into children with the ambient boundary appropriate for
-- each child slot.
--
-- The bulk of the walker is uniform: the ambient context stays the same
-- as it descends through control-flow / organisational nodes. Only a
-- few constructors switch the context:
--
--   * 'PolyManifold' opens a fresh 'LocalRoot' for its body -- the
--     manifold body's return flows to the enclosing use-site, whose
--     boundary discipline the parent already installed.
--
--   * 'PolyRemoteInterface' opens 'ForeignCalleeReturn' for the callee
--     body (which will be serialised).
--
--   * 'PolyApp' with a 'PolyExe' head whose executor is 'SrcCallP'
--     places the callee-return slot at 'SourceCall'; but 'PolyExe'
--     itself is checked as a leaf, so this is enforced by the
--     'polyOuterType' check on 'PolyExe' returning the source function's
--     declared return type.
descend :: Int -> BoundaryContext -> PolyExpr -> MorlocMonad ()
descend m _ (PolyManifold _ _ _ _ body) = walk m LocalRoot body
descend m _ (PolyRemoteInterface _ _ _ _ body) = walk m ForeignCalleeReturn body
descend m ctx (PolyReturn body) = walk m ctx body
descend m _ (PolyLet _ e1 e2) = do
  walk m LocalRoot e1
  walk m LocalRoot e2
descend m ctx (PolyApp f xs) = do
  walk m ctx f
  mapM_ (walk m LocalRoot) xs
descend m ctx (PolyIf c t' e) = do
  walk m LocalRoot c
  walk m ctx t'
  walk m ctx e
descend m _ (PolyCacheBody _ _ _ body) = walk m LocalRoot body
descend m _ (PolyDebugWrap _ _ body) = walk m LocalRoot body
-- 'PolyDoBlock' and 'PolyEval' are the Suspend and Force coercions;
-- once we cross one, the boundary has been discharged and the
-- underlying value lives at 'LocalRoot' regardless of what the caller
-- expected. Descending past them with the caller's convention would
-- re-check EffectT nodes the coercion already reconciled.
descend m _ (PolyDoBlock _ body) = walk m LocalRoot body
descend m _ (PolyEval _ body) = walk m LocalRoot body
descend m ctx (PolyCoerce _ _ body) = walk m ctx body
descend m _ (PolyList _ _ xs) = mapM_ (walk m LocalRoot) xs
descend m _ (PolyTuple _ xs) = mapM_ (walk m LocalRoot . snd) xs
descend m _ (PolyRecord _ _ _ rs) = mapM_ (walk m LocalRoot . snd . snd) rs
descend m _ (PolyIntrinsic _ _ xs) = mapM_ (walk m LocalRoot) xs
descend m _ (PolyVariant _ _ _ xs) = mapM_ (walk m LocalRoot) xs
-- Walk the loop body at the enclosing ctx: base leaves (forced to plain by
-- 'rewrite's loop case) flow to that boundary; a 'PolyLoopContinue' leaf is
-- control flow with 'polyOuterType' = Nothing, so it never trips the boundary
-- check, and its args are walked at LocalRoot.
descend m ctx (PolyLoop _ _ body) = walk m ctx body
descend m _ (PolyLoopContinue es) = mapM_ (walk m LocalRoot) es
descend _ _ _ = return ()

-- | Poly-stage insertion pass. Walks 'PolyExpr' and installs the
-- boundary coercions: 'PolyDoBlock' at Suspend boundaries, 'PolyEval'
-- at Force boundaries.
insertEffectBoundaries :: PolyHead -> MorlocMonad PolyHead
insertEffectBoundaries (PolyHead lang midx args body) = do
  body' <- rewrite midx body
  return $ PolyHead lang midx args body'

-- | Threads the ambient 'PolyManifold' midx, used to index any
-- 'PolyDoBlock' / 'PolyEval' the pass has to synthesise.
rewrite :: Int -> PolyExpr -> MorlocMonad PolyExpr
rewrite m (PolyApp fn xs) = do
  fn'  <- rewrite m fn
  xs'  <- mapM (rewrite m) xs
  -- A closure passed directly to a foreign source call is invoked as
  -- @f(x)@ by the source implementation, which discards the thunk, so its
  -- effect must be forced eagerly here (the 'CallbackReturn' boundary).
  -- Every other consumer -- a local 'LocalCallP', an export, a wire
  -- crossing -- forces the closure's result at its own consumption site,
  -- so intra-pool closures are left as thunks.
  let xs'' = if isSrcCallHead fn' then map maybeForceCallbackArg xs' else xs'
  maybeSuspendRemoteReceive <$> maybeSuspendSourceCall fn' xs''
rewrite _ (PolyManifold l m' f k e) = do
  e' <- rewrite m' e
  return $ PolyManifold l m' f k e'
rewrite m (PolyRemoteInterface l ti is rf e) = do
  e' <- rewrite m e
  return $ PolyRemoteInterface l ti is rf (forceCalleeBody e')
rewrite m (PolyLet i e1 e2)     = PolyLet i <$> rewrite m e1 <*> rewrite m e2
rewrite m (PolyReturn e)        = PolyReturn <$> rewrite m e
rewrite m (PolyCacheBody l m' as e) = PolyCacheBody l m' as <$> rewrite m e
rewrite m (PolyDebugWrap m' as e)   = PolyDebugWrap m' as <$> rewrite m e
rewrite m (PolyDoBlock t e)     = PolyDoBlock t <$> rewrite m e
rewrite m (PolyEval t e) = do
  e' <- rewrite m e
  return $ cancelPolyEval t e'
rewrite m (PolyCoerce c t e)    = PolyCoerce c t <$> rewrite m e
rewrite m (PolyIf c t' e) = PolyIf <$> rewrite m c <*> rewrite m t' <*> rewrite m e
-- Force the loop's base leaves so their <IO> is discharged before the
-- serialize sink / export boundary (the continue leaves are control flow and
-- are left unforced by 'forceReturnPosition's loop case).
rewrite m (PolyLoop t ids e) = forceReturnPosition m . PolyLoop t ids <$> rewrite m e
rewrite m (PolyLoopContinue es) = PolyLoopContinue <$> mapM (rewrite m) es
rewrite m (PolyList v ts xs)  = PolyList v ts <$> mapM (rewrite m) xs
rewrite m (PolyTuple v xs)    =
  PolyTuple v <$> mapM (\(t,x) -> (,) t <$> rewrite m x) xs
rewrite m (PolyRecord o v ps rs) =
  PolyRecord o v ps <$>
    mapM (\(k,(t,x)) -> (,) k . (,) t <$> rewrite m x) rs
rewrite m (PolyIntrinsic t intr xs) =
  PolyIntrinsic t intr <$> mapM (rewrite m) xs
rewrite m (PolyVariant t n i xs) = PolyVariant t n i <$> mapM (rewrite m) xs
rewrite _ leaf = return leaf

-- | If a 'PolyApp' of a source call has an application-result type
-- carrying an outer 'EffectT', suspend the application in 'PolyDoBlock'
-- and peel the effect layer from the head 'PolyExe'.
--
-- Handles two shapes uniformly:
--
--   * @PolyExe (FunT ins (EffectT effs ret)) (SrcCallP _)@ applied to
--     @length ins@ arguments -- the standard n-ary source case.
--
--   * @PolyExe (EffectT effs ret) (SrcCallP _)@ applied to zero
--     arguments -- a nullary source declared '<E> T' directly (no
--     'FunT' wrapper), e.g. @gen_small :: <IO> [Int]@.
--
-- Partial applications (arity mismatch) are left untouched; their
-- result type is a function type, not a value at a boundary.
--
-- The arguments are values: each is computed once, at the application,
-- and the suspension captures the result. An argument that is not already
-- a variable or a literal is bound outside the suspension, so running the
-- suspension twice runs the host call twice and nothing else.
maybeSuspendSourceCall :: PolyExpr -> [PolyExpr] -> MorlocMonad PolyExpr
maybeSuspendSourceCall fn@(PolyExe (Idx gidx exeT) (SrcCallP src)) xs = do
  declared <- declaredResultIsSuspension gidx src (length xs)
  case appReturn exeT (length xs) of
    Just (EffectT effs ret) | declared -> do
      let fn' = PolyExe (Idx gidx (peelReturn exeT)) (SrcCallP src)
          argTypes = case exeT of
            FunT ins _ -> map Just ins
            _ -> repeat Nothing
      (binds, xs') <- unzip <$> zipWithM bindArg argTypes xs
      let suspended = PolyDoBlock (Idx gidx (EffectT effs ret)) (PolyApp fn' xs')
      return $ foldr (\(i, e) body -> PolyLet i e body) suspended (concat binds)
    _ -> return (PolyApp fn xs)
  where
    bindArg _ x | isAtom x = return ([], x)
    bindArg mt x = do
      i <- MM.getCounter
      -- A callback's result was forced for the host ('maybeForceCallbackArg'),
      -- so the value bound here has the peeled function type.
      let t = case mt of
            Just ty -> Idx gidx (peelCallbackResult ty)
            Nothing -> Idx gidx (maybe (VarT (TV "Unit")) id (polyOuterType x))
      return ([(i, x)], PolyLetVar t i)

    -- Mirrors 'maybeForceCallbackArg', which descends into lists, tuples and
    -- records of callbacks.
    peelCallbackResult (FunT ins (EffectT _ r)) = FunT ins r
    peelCallbackResult (AppT c ts) = AppT c (map peelCallbackResult ts)
    peelCallbackResult (NamT o v ps rs) = NamT o v ps [(k, peelCallbackResult ft) | (k, ft) <- rs]
    peelCallbackResult ty = ty

    isAtom (PolyBndVar _ _) = True
    isAtom (PolyLetVar _ _) = True
    isAtom (PolyInt _ _) = True
    isAtom (PolyReal _ _) = True
    isAtom (PolyStr _ _) = True
    isAtom (PolyLog _ _) = True
    isAtom (PolyNull _) = True
    isAtom (PolyEnum _ _ _) = True
    isAtom _ = False

    appReturn (FunT ins ret) n | n == length ins = Just ret
    appReturn (FunT _ _) _ = Nothing
    appReturn t 0 = Just t
    appReturn _ _ = Nothing

    peelReturn (FunT ins (EffectT _ ret)) = FunT ins ret
    peelReturn (EffectT _ ret)            = ret
    peelReturn t                          = t
maybeSuspendSourceCall fn xs = return (PolyApp fn xs)

-- | Whether the source function's own signature declares its result, after
-- the given number of arguments, as a suspension. The host adapter applies
-- to that declaration: the host function is eager and its call is the body
-- of the suspension. A result that is a type variable instantiated to a
-- suspension (an instance method such as an index access) is a value the
-- host hands back as it is, a thunk among them, and is not adapted. The
-- signature is found by the source it implements, since the call's own
-- index may belong to the definition around it; a source with no
-- signature on record is judged by its instantiated type.
declaredResultIsSuspension :: Int -> Source -> Int -> MorlocMonad Bool
declaredResultIsSuspension _ src n = do
  sgmap <- MM.gets stateSignatures
  let declared = [e | sg <- GMap.elems sgmap, Just e <- [signatureOf sg]]
  return $ case declared of
    (e : _) -> suspended (etype e)
    [] -> True
  where
    signatureOf (Monomorphic (TermTypes (Just e) srcs _))
      | any (sameSource . snd) srcs = Just e
    signatureOf (Polymorphic _ _ e ts)
      | any (any (sameSource . snd) . termConcrete) ts = Just e
    signatureOf _ = Nothing
    -- The host function itself: its name in its language and file, whatever
    -- alias a module gives it.
    sameSource (Idx _ s) = srcName s == srcName src && srcPath s == srcPath src && srcLang s == srcLang src
    suspended t = case resultAfter (peelForall t) of
      Just (EffectU _ _) -> True
      _ -> False
    peelForall (ForallU _ t) = peelForall t
    peelForall t = t
    resultAfter (FunU ins ret)
      | n == length ins = Just ret
      | n < length ins = Just (FunU (drop n ins) ret)
      | otherwise = Nothing
    resultAfter t
      | n == 0 = Just t
      | otherwise = Nothing

-- | 'ForeignCallerReceive' Suspend. A remote call whose result is a
-- suspension is itself the suspension on the caller's side: the callee's
-- entry point runs one layer ('ForeignCalleeReturn'), so the caller holds
-- a thunk whose body is the call, and forcing it is the call. The
-- interface's own type is the value the wire carries: one layer peeled.
maybeSuspendRemoteReceive :: PolyExpr -> PolyExpr
maybeSuspendRemoteReceive
    (PolyApp (PolyRemoteInterface lang (Idx gidx t) argIds rf inner) xs)
  | EffectT _ inner' <- t =
      PolyDoBlock (Idx gidx t)
        (PolyApp (PolyRemoteInterface lang (Idx gidx inner') argIds rf inner) xs)
maybeSuspendRemoteReceive e = e

-- | Peephole cancellation for 'PolyEval'. If the value reached after
-- unwrapping any adjacent 'PolyDoBlock' is already plain, drop the
-- 'PolyEval': forcing a plain value is meaningless (@val()@ raises at
-- runtime) and 'Force . Suspend = id'. If the inner value still carries
-- an outer 'EffectT' (e.g. a 'PolyIf' whose branches are 'PolyDoBlock's,
-- or a 'PolyManifold' body ending in an unforced thunk), the 'PolyEval'
-- MUST stay so the surrounding context sees a plain value.
cancelPolyEval :: Indexed Type -> PolyExpr -> PolyExpr
cancelPolyEval t e =
  let inner = case e of
        PolyDoBlock _ x -> x
        x               -> x
   in case polyOuterType inner of
        Just innerT | not (hasOuterEffect innerT) -> inner
        _                                         -> PolyEval t e

-- | 'CallbackReturn' Force. A closure passed as an argument to a foreign
-- source call is invoked as @f(x)@ by the source implementation, which
-- discards the return's outer structure and cares only about the side
-- effect. If the closure's return is '<E> T', @f(x)@ merely materialises
-- the thunk and drops it, so the effect never fires -- force it here (one
-- 'PolyEval' per 'EffectT' layer). Only lambda-shaped manifolds
-- ('ManifoldPart'/'ManifoldPass') are closures, and 'forceReturnPosition'
-- is a no-op unless the return carries an effect, so pure callbacks are
-- untouched.
--
-- This is the ONLY place a closure's return is force-adjusted. Intra-pool
-- closures called via 'LocalCallP' are deliberately left as thunks: their
-- result is forced by the caller's own consumption boundary (a do-bind, a
-- return, a wire crossing), and leaving the closure body a thunk keeps its
-- rendered type consistent between its definition and its use sites (a
-- forced body renders @T@ but an argument slot is typed @<E> T@).
maybeForceCallbackArg :: PolyExpr -> PolyExpr
maybeForceCallbackArg e@(PolyManifold _ m form _ _)
  | isLambdaForm form = forceReturnPosition m e
-- A callback nested inside a structured argument (a list/tuple/record of
-- closures passed to the source call) is invoked exactly the same way by the
-- foreign code, so descend into the structure and force those too. The
-- container's element TYPE is peeled in lock-step ('peelCallbackType'): forcing
-- a closure element makes its value render the plain @T@, so the container that
-- holds it must be declared with the peeled element type or the two disagree.
-- Non-closure elements fall through unchanged (both the value and the type
-- peel are no-ops on a non-effectful element).
maybeForceCallbackArg (PolyList v ts es) =
  PolyList v (map peelCallbackType ts) (map maybeForceCallbackArg es)
maybeForceCallbackArg (PolyTuple v xs) =
  PolyTuple v [(peelCallbackType t, maybeForceCallbackArg x) | (t, x) <- xs]
maybeForceCallbackArg (PolyRecord nt v ts fs) =
  PolyRecord nt v (map peelCallbackType ts)
    [(k, (peelCallbackType t, maybeForceCallbackArg x)) | (k, (t, x)) <- fs]
maybeForceCallbackArg e = e

-- | Peel one 'EffectT' layer off the RETURN of a function-typed element, so
-- @Int -> \<E\> ()@ becomes @Int -> ()@. Mirrors the value-level force of a
-- callback ('forceReturnPosition' / 'forceLayers'): once the closure's
-- result is run, the slot that holds it must carry the peeled type. A no-op
-- on any non-effectful or non-function type.
peelCallbackType :: Indexed Type -> Indexed Type
peelCallbackType (Idx i t) = Idx i (peel t)
  where
    peel (FunT ins (EffectT _ inner)) = FunT ins inner
    peel other = other

-- | A lambda-shaped manifold form (an unapplied or partially-applied
-- function value), as opposed to a saturated 'ManifoldFull' call.
isLambdaForm :: ManifoldForm c b -> Bool
isLambdaForm (ManifoldPart _ _) = True
isLambdaForm (ManifoldPass _)   = True
isLambdaForm _                  = False

-- | The head of a direct foreign source-call application.
isSrcCallHead :: PolyExpr -> Bool
isSrcCallHead (PolyExe _ (SrcCallP _)) = True
isSrcCallHead _                        = False

-- | Walk to the return position of a manifold body (through 'PolyReturn'
-- and 'PolyLet' tails), and if the value there has an outer 'EffectT',
-- wrap in 'PolyEval' per layer. Peephole-cancels any 'PolyDoBlock'
-- already sitting at the return position (which the 'SourceCall'
-- Suspend rule may have inserted just above).
forceReturnPosition :: Int -> PolyExpr -> PolyExpr
forceReturnPosition m (PolyReturn e) = PolyReturn (forceReturnPosition m e)
forceReturnPosition m (PolyLet i v e) = PolyLet i v (forceReturnPosition m e)
forceReturnPosition m (PolyManifold l m' f k e) =
  PolyManifold l m' f k (forceReturnPosition m e)
-- A loop's base branch is its return position; force effects there. The
-- continue back-edge produces no value and must NOT be forced (its args are
-- new loop-carried values, not the manifold's return).
forceReturnPosition m (PolyLoop t ids e) = PolyLoop t ids (goLoop e)
  where
    goLoop (PolyIf c a b) = PolyIf c (goLoop a) (goLoop b)
    goLoop (PolyLet i v x) = PolyLet i v (goLoop x)
    goLoop cont@(PolyLoopContinue _) = cont
    goLoop base = forceReturnPosition m base
forceReturnPosition m e =
  case polyOuterType e of
    Just t | hasOuterEffect t -> forceLayers m t e
    _ -> e

-- | Force exactly one layer: running a suspension yields its result,
-- which may itself be a suspension. Peephole-cancels a 'PolyDoBlock' at
-- the head so @Force . Suspend = id@.
forceLayers :: Int -> Type -> PolyExpr -> PolyExpr
forceLayers _ (EffectT _ _) (PolyDoBlock _ inside) = inside
forceLayers m (EffectT _ inner) e = PolyEval (Idx m inner) e
forceLayers _ _ e = e

-- | Entry-point boundary pass. Called from
-- 'Morloc.CodeGenerator.Express.express' with the manifold's full 'Type'
-- (which 'PolyHead' does not carry). Handles two boundaries in one visit:
--
--   * 'ExportArg' Suspend, for a command only: the program's caller is a
--     host that has values, not suspensions, so an argument declared
--     '<E> T' arrives as a plain 'T' and every reference to it is wrapped
--     in 'PolyDoBlock', a suspension with a constant result. A helper's
--     arguments arrive from another manifold, a suspension among them as
--     a closure, and need no adapting.
--
--   * 'ExportRoot' Force: the manifold body's return position is walked
--     (through 'PolyReturn' / 'PolyLet' / 'PolyManifold') and a root
--     'EffectT' of the declared return type is run -- one 'PolyEval' --
--     so the wire ships the value the caller asked for.
--
-- Kept separate from 'insertEffectBoundaries' because the export type
-- is not stored in 'PolyHead'; 'Express.express' calls both passes,
-- with the 'Type' threaded here explicitly.
insertExportBoundaries :: Bool -> Int -> Type -> PolyHead -> PolyHead
insertExportBoundaries isCommand cidx t (PolyHead lang midx args body) =
  let inputTs = case t of FunT inputs _ -> inputs; _ -> []
      thunkArgIds = [ann a | isCommand, (a, EffectT _ _) <- zip args inputTs]
      retT = case t of FunT _ ret -> ret; t' -> t'
      body' = suspendThunkArgs thunkArgIds body
      body'' = forceExportReturn cidx retT body'
   in PolyHead lang midx args body''
  where
    suspendThunkArgs :: [Int] -> PolyExpr -> PolyExpr
    suspendThunkArgs [] e = e
    suspendThunkArgs ids e = go ids e

    go ids (PolyBndVar (C (Idx ci (EffectT effs inner))) i)
      | i `elem` ids = wrapSuspends ci (EffectT effs inner) i
    go ids (PolyManifold l m f k e) = PolyManifold l m f k (go ids e)
    go ids (PolyLet i e1 e2) = PolyLet i (go ids e1) (go ids e2)
    go ids (PolyReturn e) = PolyReturn (go ids e)
    go ids (PolyApp e es) = PolyApp (go ids e) (map (go ids) es)
    go ids (PolyCacheBody lbl cm cargs e) = PolyCacheBody lbl cm cargs (go ids e)
    go ids (PolyEval ti e) = PolyEval ti (go ids e)
    go ids (PolyDoBlock ti e) = PolyDoBlock ti (go ids e)
    go ids (PolyCoerce c ti e) = PolyCoerce c ti (go ids e)
    go ids (PolyIntrinsic ti intr es) = PolyIntrinsic ti intr (map (go ids) es)
    go ids (PolyList v ti es) = PolyList v ti (map (go ids) es)
    go ids (PolyTuple v es) = PolyTuple v (map (fmap (go ids)) es)
    go ids (PolyRecord o v ps rs) = PolyRecord o v ps (map (fmap (fmap (go ids))) rs)
    go ids (PolyIf c t' e) = PolyIf (go ids c) (go ids t') (go ids e)
    go ids (PolyRemoteInterface l ti is rf e) =
      PolyRemoteInterface l ti is rf (go ids e)
    go _ e = e

    -- Peel 'EffectT' layers, wrapping each in 'PolyDoBlock'; the
    -- innermost 'BndVar' carries the fully-unwrapped type.
    wrapSuspends ci (EffectT effs inner) i =
      PolyDoBlock (Idx ci (EffectT effs inner)) (wrapSuspends ci inner i)
    wrapSuspends ci inner i = PolyBndVar (C (Idx ci inner)) i

    forceExportReturn c rt (PolyReturn e) = PolyReturn (forceExportReturn c rt e)
    forceExportReturn c rt (PolyManifold l m f k e) =
      PolyManifold l m f k (forceExportReturn c rt e)
    forceExportReturn c rt (PolyLet i e1 e2) =
      PolyLet i e1 (forceExportReturn c rt e2)
    forceExportReturn c rt e
      | hasOuterEffect rt = forceLayers c rt e
      | otherwise = e

-- | 'ForeignCalleeReturn' Force. The callee of a 'PolyRemoteInterface'
-- runs in a foreign pool; its entry point is the run of one layer of its
-- result, whose value ships across the wire, while the caller holds the
-- call itself as a suspension ('maybeSuspendRemoteReceive'). The
-- 'SourceCall' Suspend rule and this Force are cancelled by the
-- 'forceLayers' peephole when they meet at a source call return.
forceCalleeBody :: PolyExpr -> PolyExpr
forceCalleeBody (PolyManifold l m f k body) =
  PolyManifold l m f k (forceReturnPosition m body)
forceCalleeBody e = e

