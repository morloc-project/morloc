{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.CodeGenerator.EffectBoundary
Description : Central force/suspend insertion at effect boundaries
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Every position in the codegen IR sits under some /boundary/ -- the
outermost export return, an argument to a foreign consumer, the receive
slot of a cross-language RPC, a wire serializer, etc. Each boundary
imposes a calling convention on the value at that position: either a
plain 'T' or a thunk '<E> T' (represented in every target language as
a nullary callable).

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
  , forceSerializedThunk
  ) where

import Morloc.CodeGenerator.Namespace
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
    -- position): must be a plain value; a thunk here must be forced.
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
boundaryExpectsPlain SerializeSink        = True

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
polyOuterType _                                       = Nothing

-- | Does the outer layer of the type declare an effect?
hasOuterEffect :: Type -> Bool
hasOuterEffect (EffectT _ _) = True
hasOuterEffect _             = False

-- | Post-'insertEffectBoundaries' invariant check. Walks the tree; at
-- every 'plain'-expecting boundary ('ExportRoot',
-- 'ForeignCalleeReturn', 'CallbackReturn', 'SerializeSink') the
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
  return
    . maybeSuspendRemoteReceive
    $ maybeSuspendSourceCall fn' xs'
rewrite _ (PolyManifold l m' f k e) = do
  e' <- rewrite m' e
  return $ PolyManifold l m' f k (maybeForceCallbackReturn m' f e')
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
rewrite m (PolyIf c t' e) = do
  c'  <- rewrite m c
  t'' <- rewrite m t'
  e'  <- rewrite m e
  return $ suspendMixedIfBranches m c' t'' e'
rewrite m (PolyList v ts xs)  = PolyList v ts <$> mapM (rewrite m) xs
rewrite m (PolyTuple v xs)    =
  PolyTuple v <$> mapM (\(t,x) -> (,) t <$> rewrite m x) xs
rewrite m (PolyRecord o v ps rs) =
  PolyRecord o v ps <$>
    mapM (\(k,(t,x)) -> (,) k . (,) t <$> rewrite m x) rs
rewrite m (PolyIntrinsic t intr xs) =
  PolyIntrinsic t intr <$> mapM (rewrite m) xs
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
maybeSuspendSourceCall :: PolyExpr -> [PolyExpr] -> PolyExpr
maybeSuspendSourceCall fn@(PolyExe (Idx gidx exeT) (SrcCallP src)) xs =
  case appReturn exeT (length xs) of
    Just (EffectT effs ret) ->
      let fn' = PolyExe (Idx gidx (peelReturn exeT)) (SrcCallP src)
       in PolyDoBlock (Idx gidx (EffectT effs ret)) (PolyApp fn' xs)
    _ -> PolyApp fn xs
  where
    appReturn (FunT ins ret) n | n == length ins = Just ret
    appReturn (FunT _ _) _ = Nothing
    appReturn t 0 = Just t
    appReturn _ _ = Nothing

    peelReturn (FunT ins (EffectT _ ret)) = FunT ins ret
    peelReturn (EffectT _ ret)            = ret
    peelReturn t                          = t
maybeSuspendSourceCall fn xs = PolyApp fn xs

-- | 'ForeignCallerReceive' Suspend. When a cross-language RPC value is
-- received back into the caller pool, the wire format has stripped the
-- '<E>' layer (see 'makeSerialAST'' 'EffectF' case in Serial.hs). If
-- the caller-side 'PolyRemoteInterface' still declares an outer
-- 'EffectT', the deserialized plain value must be re-wrapped so the
-- enclosing manifold's declared return type still matches its body.
--
-- Peels ALL outer 'EffectT' layers -- one 'PolyDoBlock' per layer -- so
-- nested '<E1><E2> T' is handled if it ever appears ('mkEffectT'
-- flattens in practice, but the peeler doesn't rely on that).
maybeSuspendRemoteReceive :: PolyExpr -> PolyExpr
maybeSuspendRemoteReceive
    (PolyApp (PolyRemoteInterface lang (Idx gidx t) argIds rf inner) xs)
  | EffectT _ _ <- t =
      wrapPeeled gidx t
        (PolyApp (PolyRemoteInterface lang (Idx gidx (peelAllEffects t))
                                       argIds rf inner) xs)
  where
    wrapPeeled g (EffectT effs inner') e =
      PolyDoBlock (Idx g (EffectT effs inner')) (wrapPeeled g inner' e)
    wrapPeeled _ _ e = e
maybeSuspendRemoteReceive e = e

-- | Peel every outer 'EffectT' layer.
peelAllEffects :: Type -> Type
peelAllEffects (EffectT _ inner) = peelAllEffects inner
peelAllEffects t = t

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

-- | Mixed-effect 'PolyIf' branch Suspend. If the folded outer type of a
-- 'PolyIf' has an 'EffectT' (per 'polyOuterType' 's "at-least-as-
-- effectful-as-most-effectful-branch" rule), every branch that is
-- itself plain must be wrapped in 'PolyDoBlock' so its runtime shape
-- (a nullary callable in every target language) agrees with the other
-- branches -- and with any surrounding assignment @T ni = branch;@
-- where @T@ is the guard's thunk type.
--
-- Without this rule, C++ emits @std::function<int()> helper = 42;@ on
-- a pure branch and gcc rejects the assignment. Python and R happen to
-- accept the same shape (int / integer assigned to a variable), so the
-- runtime bug is a C++-first regression, but the rule fires in every
-- language uniformly for consistency.
--
-- The 'Idx m' index is the ambient 'PolyManifold' 's midx, threaded
-- through 'rewrite'; the effect layer at this position is just
-- typedef-lookup context for downstream lowering.
suspendMixedIfBranches :: Int -> PolyExpr -> PolyExpr -> PolyExpr -> PolyExpr
suspendMixedIfBranches m cond thenB elseB =
  case polyOuterType (PolyIf cond thenB elseB) of
    Just guardT
      | hasOuterEffect guardT ->
          PolyIf cond (suspendIfPlain m guardT thenB) (suspendIfPlain m guardT elseB)
    _ -> PolyIf cond thenB elseB
  where
    -- 'PolyDoBlock' is already a thunk at codegen regardless of its
    -- stored type ('mkEffectT' can collapse '<{}> T = T', so the type
    -- may say plain while the runtime shape is a lambda). Wrapping
    -- again would produce a lambda-of-lambda; check structurally
    -- instead of by 'polyOuterType' alone.
    suspendIfPlain m' guardT branch = case branch of
      PolyDoBlock{} -> branch
      _ -> case polyOuterType branch of
        Just t | not (hasOuterEffect t) -> PolyDoBlock (Idx m' guardT) branch
        _                               -> branch

-- | 'CallbackReturn' Force. A lambda passed as an argument to a foreign
-- consumer will be invoked as @f(x)@; the consumer discards the return
-- value's outer structure and cares only about the side effect. If the
-- lambda's declared return is '<E> T', @f(x)@ merely materialises the
-- thunk and drops it, so the effect never fires. 'ManifoldPart' and
-- 'ManifoldPass' are the two lambda-shaped 'ManifoldForm's; either with
-- an 'EffectT' return must be forced. One 'PolyEval' per 'EffectT'
-- layer.
maybeForceCallbackReturn :: Int -> ManifoldForm None (Maybe Type) -> PolyExpr -> PolyExpr
maybeForceCallbackReturn midx form body
  | isLambdaForm form = forceReturnPosition midx body
  | otherwise = body
  where
    isLambdaForm (ManifoldPart _ _) = True
    isLambdaForm (ManifoldPass _)   = True
    isLambdaForm _                  = False

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
forceReturnPosition m e =
  case polyOuterType e of
    Just t | hasOuterEffect t -> forceLayers m t e
    _ -> e

-- | One 'PolyEval' per 'EffectT' layer. Peephole-cancels adjacent
-- 'PolyDoBlock' at the head so @Force . Suspend = id@.
forceLayers :: Int -> Type -> PolyExpr -> PolyExpr
forceLayers m (EffectT _ inner) (PolyDoBlock _ inside) =
  forceLayers m inner inside
forceLayers m (EffectT _ inner) e =
  forceLayers m inner (PolyEval (Idx m inner) e)
forceLayers _ _ e = e

-- | Export-boundary pass. Called from 'Morloc.CodeGenerator.Express.express'
-- with the export's full 'Type' (which 'PolyHead' does not carry). Handles
-- two boundaries in one visit:
--
--   * 'ExportArg' Suspend: every 'PolyBndVar' reference to an arg whose
--     declared type is 'EffectT' is wrapped in 'PolyDoBlock'. The wire
--     delivers the CLI arg as a plain value (peeled), so the manifold
--     body must re-suspend it before the manifold's own '<E> T' uses
--     invoke it as a thunk.
--
--   * 'ExportRoot' Force: the manifold body's return position is walked
--     (through 'PolyReturn' / 'PolyLet' / 'PolyManifold') and every
--     'EffectT' layer of the declared return type is forced -- one
--     'PolyEval' per layer -- so the wire ships a plain value.
--
-- Kept separate from 'insertEffectBoundaries' because the export type
-- is not stored in 'PolyHead'; 'Express.express' calls both passes,
-- with the export 'Type' threaded here explicitly.
insertExportBoundaries :: Int -> Type -> PolyHead -> PolyHead
insertExportBoundaries cidx t (PolyHead lang midx args body) =
  let inputTs = case t of FunT inputs _ -> inputs; _ -> []
      thunkArgIds = [ann a | (a, EffectT _ _) <- zip args inputTs]
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
-- runs in a foreign pool and must ship a plain value across the wire;
-- 'makeSerialAST'' strips 'EffectF' from the wire schema, so a
-- callee-side thunk in the return position would serialise as an
-- unserializable callable (Python 'lambda', C++ 'std::function', etc.)
-- and the caller-side deserialize would fail. The 'SourceCall' Suspend
-- rule and this Force are cancelled by the 'forceLayers' peephole when
-- they meet at a source call return.
forceCalleeBody :: PolyExpr -> PolyExpr
forceCalleeBody (PolyManifold l m f k body) =
  PolyManifold l m f k (forceReturnPosition m body)
forceCalleeBody e = e

-- | 'SerializeSink' peephole. When a value about to cross the wire is
-- a bare 'DoBlockN', wrap it in 'EvalN' so the serializer sees a plain
-- value, not a callable. Sources of 'DoBlockN' at a serialize sink
-- today are effect-tagged intrinsics ('@save', '@load'), cross-language
-- RPC receiver-side wraps, and 'PolyDoBlock' at a position the upstream
-- 'PolyEval' insertion did not cover. Kept at 'NativeExpr' granularity
-- because two of those three sources are NativeExpr-only concerns.
forceSerializedThunk :: NativeExpr -> NativeExpr
forceSerializedThunk ne@(DoBlockN _ inner) = EvalN (typeFof inner) ne
forceSerializedThunk ne = ne
