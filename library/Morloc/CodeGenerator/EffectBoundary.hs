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
host adapter ('adapt'): a host language has no thunks, so at a sourced
function the compiler suspends the host call (its result is declared
@<E> T@) and converts every value that crosses it by its type, however
the value was built -- a morloc function the host will call runs its
suspension, and a host function morloc will call builds one, at any depth
of records, tuples, lists, optionals and packable types. At the program
boundary it runs the root of a command's result and builds a constant
suspension from a command's argument.

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
import qualified Morloc.Data.Map as Map
import qualified Data.Set as Set
import qualified Morloc.TypeEval as TE
import qualified Morloc.BaseTypes as BT
import qualified Control.Monad.State as CMS
import Control.Monad.Except (catchError)
import Morloc.CodeGenerator.Infer (inferConcreteType)
import qualified Morloc.LangRegistry as LR
import qualified Morloc.Language as ML
import Morloc.CodeGenerator.Instance (findFunctorMap, resolveInstanceForType)
import Morloc.Typecheck.Internal (unqualify)
import Morloc.CodeGenerator.Serial (PackerInstance (..), findPackerInstances)

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
-- a closure is a function value: what its body returns is not its type
polyOuterType (PolyManifold _ _ f _ _) | isLambdaForm f = Nothing
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

-- | The two sides of a remote call must agree on the value the wire
-- carries: the callee's entry point runs one layer of its result and the
-- caller receives the interface type, so the callee's return has a root
-- suspension exactly when the interface type has one (a suspension of a
-- suspension ships its inner thunk as a closure). Walked at
-- 'ForeignCalleeReturn' with that agreement as the check, in place of the
-- plain-value rule of the other boundaries.
walkCallee :: Int -> Type -> PolyExpr -> MorlocMonad ()
walkCallee m iface body = do
  case polyOuterType body of
    Just t | hasOuterEffect t /= hasOuterEffect iface ->
      MM.throwCompilerBug $
        "EffectBoundary invariant violated at manifold m"
          <> pretty m <> ":\n"
          <> "  boundary : " <> viaShow ForeignCalleeReturn <> "\n"
          <> "  callee   : " <> pretty t <> "\n"
          <> "  caller   : " <> pretty iface <> "\n"
          <> "  expected : the callee's return and the caller's interface type to\n"
          <> "             agree on whether the wire carries a suspension.\n"
          <> "Root cause: 'insertEffectBoundaries' peeled one side and not the other."
    _ -> return ()
  descend m ForeignCalleeReturn body

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
descend m _ (PolyRemoteInterface _ (Idx _ t) _ _ body) = walkCallee m t body
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
  body' <- rewrite lang midx body
  return $ PolyHead lang midx args body'

-- | Threads the ambient 'PolyManifold' midx, used to index any
-- 'PolyDoBlock' / 'PolyEval' the pass has to synthesise, and the pool the
-- expression is generated into, which is the pool any adapter closure
-- belongs to.
rewrite :: Lang -> Int -> PolyExpr -> MorlocMonad PolyExpr
rewrite lang m (PolyApp fn xs) = do
  fn'  <- rewrite lang m fn
  xs'  <- mapM (rewrite lang m) xs
  case (fn', xs') of
    -- A field read through a record whose value is the host's reads a value
    -- in the host's convention. A read applied to further arguments is the
    -- read followed by a call of what it read.
    (PolyExe (Idx gi (FunT (recT : rest) r)) (PatCallP pat@(PatternStruct sel)), x : ys) -> do
      scope <- MM.getGeneralScope
      hostRecs <- hostRecords lang gi [recT]
      let hv = HostView scope Set.empty hostRecs Set.empty Nothing
          fieldT = if null rest then r else FunT rest r
      if readsHostRecord hv sel recT && needsAdapt hv Nothing fieldT
        then do
          let getter = PolyExe (Idx gi (FunT [recT] (hostType hv Nothing fieldT))) (PatCallP pat)
          value <- adapt Inbound lang gi hv Nothing fieldT (PolyApp getter [x])
          if null ys
            then return value
            else do
              v <- MM.getCounter
              return $ PolyLet v value (PolyApp (PolyExe (Idx gi fieldT) (LocalCallP v)) ys)
        else return (PolyApp fn' xs')
    (PolyRemoteInterface {}, _) -> crossPool lang m fn' xs'
    _ -> maybeSuspendRemoteReceive <$> sourceCall lang fn' xs'
rewrite _ _ (PolyManifold l m' f k e) = do
  e' <- rewrite l m' e
  return $ PolyManifold l m' f k e'
rewrite lang m (PolyRemoteInterface l ti is rf e) = do
  e' <- rewrite lang m e
  return $ PolyRemoteInterface l ti is rf (forceCalleeBody e')
rewrite lang m (PolyLet i e1 e2)     = PolyLet i <$> rewrite lang m e1 <*> rewrite lang m e2
rewrite lang m (PolyReturn e)        = PolyReturn <$> rewrite lang m e
rewrite lang m (PolyCacheBody l m' as e) = PolyCacheBody l m' as <$> rewrite lang m e
rewrite lang m (PolyDebugWrap m' as e)   = PolyDebugWrap m' as <$> rewrite lang m e
rewrite lang m (PolyDoBlock t e)     = PolyDoBlock t <$> rewrite lang m e
rewrite lang m (PolyEval t e) = do
  e' <- rewrite lang m e
  return $ cancelPolyEval t e'
rewrite lang m (PolyCoerce c t e)    = PolyCoerce c t <$> rewrite lang m e
rewrite lang m (PolyIf c t' e) = PolyIf <$> rewrite lang m c <*> rewrite lang m t' <*> rewrite lang m e
-- Force the loop's base leaves so their <IO> is discharged before the
-- serialize sink / export boundary (the continue leaves are control flow and
-- are left unforced by 'forceReturnPosition's loop case).
rewrite lang m (PolyLoop t ids e) = forceReturnPosition m . PolyLoop t ids <$> rewrite lang m e
rewrite lang m (PolyLoopContinue es) = PolyLoopContinue <$> mapM (rewrite lang m) es
rewrite lang m (PolyList v ts xs)  = PolyList v ts <$> mapM (rewrite lang m) xs
rewrite lang m (PolyTuple v xs)    =
  PolyTuple v <$> mapM (\(t,x) -> (,) t <$> rewrite lang m x) xs
rewrite lang m (PolyRecord o v@(Idx _ name) ps rs) = do
  rs' <- mapM (\(k,(t,x)) -> (,) k . (,) t <$> rewrite lang m x) rs
  -- A record whose value is the host's is built in the host's convention.
  let recT = NamT o name (map val ps) [(k, ty) | (k, (Idx _ ty, _)) <- rs']
  hostRecs <- hostRecords lang m [recT]
  if Set.member name hostRecs
    then do
      scope <- MM.getGeneralScope
      let hv = HostView scope Set.empty hostRecs Set.empty Nothing
      PolyRecord o v ps <$> sequence
        [ (,) k . (,) (Idx i (hostType hv Nothing ty)) <$> adapt Outbound lang m hv Nothing ty x
        | (k, (Idx i ty, x)) <- rs' ]
    else return (PolyRecord o v ps rs')
rewrite lang m (PolyIntrinsic t@(Idx _ resT) intr xs) = do
  xs' <- mapM (rewrite lang m) xs
  PolyIntrinsic t intr <$> case (intr, xs') of
    (IntrReplay, [h, f]) -> (\f' -> [h, f']) <$> intrinsicCallback lang m intr resT h f
    (IntrSpawn, [h, f]) -> (\f' -> [h, f']) <$> intrinsicCallback lang m intr resT h f
    _ -> return xs'
rewrite lang m (PolyVariant t n i xs) = PolyVariant t n i <$> mapM (rewrite lang m) xs
rewrite _ _ leaf = return leaf

-- | Rebind every reference to variable @i@ so it names @i'@ instead. A
-- variable is referenced in two ways -- as a 'PolyBndVar' node, and as an
-- index in an enclosed manifold's captured (context) arguments -- and both
-- must move, or a closure below would still capture the unadapted value.
-- Indices are minted from one counter and never reused, so no binder below
-- can shadow @i@ and the walk needs no scope.
substBndVar :: Int -> Int -> Type -> PolyExpr -> PolyExpr
substBndVar i i' t = go
  where
    go (PolyBndVar (C (Idx gi _)) j) | j == i = PolyLetVar (Idx gi t) i'
    go (PolyBndVar _ j) | j == i = PolyLetVar (Idx i' t) i'
    go (PolyManifold l m f k e) = PolyManifold l m (form f) k (go e)
    go (PolyExe ti (LocalCallP j)) | j == i = PolyExe ti (LocalCallP i')
    go (PolyExe ti (PapplyP j)) | j == i = PolyExe ti (PapplyP i')
    go (PolyRemoteInterface l ti is rf e) =
      PolyRemoteInterface l ti (map idx is) rf (go e)
    go (PolyLet j a b) = PolyLet j (go a) (go b)
    go (PolyReturn e) = PolyReturn (go e)
    go (PolyApp h xs) = PolyApp (go h) (map go xs)
    go (PolyCacheBody lbl m as e) = PolyCacheBody lbl m as (go e)
    go (PolyDebugWrap m as e) = PolyDebugWrap m as (go e)
    go (PolyList v ts xs) = PolyList v ts (map go xs)
    go (PolyTuple v xs) = PolyTuple v [(ty, go x) | (ty, x) <- xs]
    go (PolyRecord o v ps rs) = PolyRecord o v ps [(key, (ty, go x)) | (key, (ty, x)) <- rs]
    go (PolyDoBlock ty e) = PolyDoBlock ty (go e)
    go (PolyEval ty e) = PolyEval ty (go e)
    go (PolyCoerce co ty e) = PolyCoerce co ty (go e)
    go (PolyIf a b c) = PolyIf (go a) (go b) (go c)
    go (PolyLoop ty ids e) = PolyLoop ty (map idx ids) (go e)
    go (PolyLoopContinue xs) = PolyLoopContinue (map go xs)
    go (PolyIntrinsic ty intr xs) = PolyIntrinsic ty intr (map go xs)
    go (PolyVariant ty n j xs) = PolyVariant ty n j (map go xs)
    go leaf = leaf

    idx j = if j == i then i' else j
    arg (Arg j x) = Arg (idx j) x
    -- A captured argument and a saturated call's arguments are references;
    -- a bound argument is a binder and keeps its own index.
    form (ManifoldFull xs) = ManifoldFull (map arg xs)
    form (ManifoldPass ys) = ManifoldPass ys
    form (ManifoldPart ctx bnd) = ManifoldPart (map arg ctx) bnd

-- | A sourced call, with every value that crosses it adapted to the host's
-- calling convention ('adapt'): each argument outbound, the result inbound.
-- If the source's declared result, after the arguments it is given, is a
-- suspension, the host function is eager and its call is the body of the
-- suspension: the application is suspended in 'PolyDoBlock' and the effect
-- peeled from the head's type.
--
-- A nullary source declared '<E> T' directly (no 'FunT' wrapper) is the same
-- case applied to zero arguments. A partial application is left untouched:
-- its result is a function, not a value at the boundary.
--
-- The arguments of a suspended call are values: each is computed once, at
-- the application, and the suspension captures the result. An argument that
-- is not already a variable or a literal is bound outside the suspension, so
-- running the suspension twice runs the host call twice and nothing else.
sourceCall :: Lang -> PolyExpr -> [PolyExpr] -> MorlocMonad PolyExpr
sourceCall lang fn@(PolyExe (Idx gidx exeT0) (SrcCallP src)) xs = do
  scope <- MM.getGeneralScope
  (opaque, declared) <- declaredSignature gidx src
  -- The row may be spelled through an alias (@type IOInt = <IO> Int@).
  let exeT = expandT scope exeT0
      n = length xs
  hostRecs <- hostRecords lang gidx [exeT]
  let hv = HostView scope opaque hostRecs Set.empty Nothing
  case splitAtArity n exeT of
    Nothing -> return (PolyApp fn xs)
    Just (ins, ret0) -> do
      let ret = expandT scope ret0
      let (dIns, dRet) = case declared of
            Nothing -> (replicate n Nothing, Nothing)
            Just d -> case splitAtArity n (expandT scope d) of
              Just (dis, dr) -> (map Just dis, Just dr)
              Nothing -> (replicate n Nothing, Nothing)
          -- Without a signature on record the source is judged by its
          -- instantiated type. A result that is a type variable of the
          -- signature instantiated to a suspension (an instance method such
          -- as an index access) is a value the host hands back as it is.
          suspended = case (declared, dRet) of
            (Nothing, _) -> isEffect ret
            (_, Just dr) -> isEffect (expandT scope dr) && isEffect ret
            _ -> False
      xs' <- sequence
        [ adapt Outbound lang gidx hv d t x | (d, t, x) <- zip3 dIns ins xs ]
      let hostIns = zipWith (hostType hv) dIns ins
          head' r = if null ins then r else FunT hostIns r
      case ret of
        EffectT effs r | suspended -> do
          let dr = peelDecl dRet
              fn' = PolyExe (Idx gidx (head' (hostType hv dr r))) (SrcCallP src)
          (binds, xs'') <- unzip <$> zipWithM bindArg hostIns xs'
          adapted <- adapt Inbound lang gidx hv dr r (PolyApp fn' xs'')
          let suspendedCall = PolyDoBlock (Idx gidx (EffectT effs r)) adapted
          return $ foldr (\(i, e) body -> PolyLet i e body) suspendedCall (concat binds)
        _
          | needsAdapt hv dRet ret || or (zipWith (needsAdapt hv) dIns ins) -> do
              let fn' = PolyExe (Idx gidx (head' (hostType hv dRet ret))) (SrcCallP src)
              adapt Inbound lang gidx hv dRet ret (PolyApp fn' xs')
          | otherwise -> return (PolyApp fn xs')
  where
    bindArg _ x | isAtom x = return ([], x)
    bindArg t x = do
      i <- MM.getCounter
      return ([(i, x)], PolyLetVar (Idx gidx t) i)

    isAtom (PolyBndVar _ _) = True
    isAtom (PolyLetVar _ _) = True
    isAtom (PolyInt _ _) = True
    isAtom (PolyReal _ _) = True
    isAtom (PolyStr _ _) = True
    isAtom (PolyLog _ _) = True
    isAtom (PolyNull _) = True
    isAtom (PolyEnum _ _ _) = True
    -- A function that captures nothing is built at no cost and with no
    -- effect, so building it inside the suspension is the same as outside;
    -- bound outside, it would be a value the suspension has to capture.
    isAtom (PolyManifold _ _ (ManifoldPass _) _ _) = True
    isAtom (PolyManifold _ _ (ManifoldPart [] _) _ _) = True
    isAtom _ = False

    isEffect (EffectT _ _) = True
    isEffect _ = False
sourceCall _ fn xs = return (PolyApp fn xs)

-- | A call into another pool. A record whose value is the host's in a
-- compiled pool ('hostRecords') holds its functions in the host's convention
-- there and in morloc's convention in a dynamic pool, so where one crosses
-- between two pools that hold it differently, the functions inside it are
-- converted on the side that holds morloc's convention and the wire carries
-- the host's. Nothing else crosses differently.
crossPool :: Lang -> Int -> PolyExpr -> [PolyExpr] -> MorlocMonad PolyExpr
crossPool lang m (PolyRemoteInterface l (Idx ri t) ids rf inner0) xs0 = do
  scope <- MM.getGeneralScope
  let view recs only = HostView scope Set.empty recs Set.empty (Just only)
      -- the callee's entry runs the root layer of a suspension it returns
      payload = case expandT scope t of
        EffectT _ c -> c
        x -> x
  -- A call that names no parameters passes the callee's own, in order.
  let params
        | length ids == length xs0 = map Just ids
        | PolyManifold _ _ (ManifoldFull as) _ _ <- inner0
        , length as == length xs0 = [Just i | Arg i _ <- as]
        | otherwise = map (const Nothing) xs0
  args <- mapM (crossArg view) (zip params xs0)
  here <- hostRecords lang m [payload]
  there <- hostRecords l m [payload]
  let toHere = Set.difference here there
      fromThere = Set.difference there here
  if Set.null toHere && Set.null fromThere && all (\(c, _, conv) -> not c && isNothing conv) args
    then return (maybeSuspendRemoteReceive (PolyApp (PolyRemoteInterface l (Idx ri t) ids rf inner0) xs0))
    else do
      inner1 <- foldM (\inner (_, _, conv) -> maybe (return inner) ($ inner) conv) inner0 args
      inner2 <-
        if Set.null toHere
          then return inner1
          else adapt Outbound l m (view there toHere) Nothing payload inner1
      let wireT = if Set.null toHere && Set.null fromThere then t else hostType (view Set.empty (Set.union toHere fromThere)) Nothing t
          received = maybeSuspendRemoteReceive
            (PolyApp (PolyRemoteInterface l (Idx ri wireT) ids rf inner2) [x | (_, x, _) <- args])
      if Set.null fromThere
        then return received
        else adapt Inbound lang m (view here fromThere) Nothing t received
  where
    -- An argument is converted by the caller when the callee holds it as
    -- the host's, and by the callee, on its parameter, when the caller does.
    -- Each is (whether the caller converted it, the argument, the callee's
    -- conversion of its parameter).
    crossArg view (mi, x) = case polyOuterType x of
      Nothing -> return (False, x, Nothing)
      Just ta -> do
        argHere <- hostRecords lang m [ta]
        argThere <- hostRecords l m [ta]
        let toThere = Set.difference argThere argHere
            fromHere = Set.difference argHere argThere
        x' <-
          if Set.null toThere
            then return x
            else adapt Outbound lang m (view argHere toThere) Nothing ta x
        let param inner = case (mi, inner) of
              (Just i, PolyManifold il im form k body) -> do
                i' <- MM.getCounter
                let wireA = hostType (view Set.empty fromHere) Nothing ta
                adapted <- adapt Inbound il im (view argThere fromHere) Nothing ta (PolyBndVar (C (Idx im wireA)) i)
                MM.modify $ \st -> st {stateArgTypes =
                  Map.insert i (Idx im wireA) (Map.insert i' (Idx im ta) (stateArgTypes st))}
                return $ PolyManifold il im form k (PolyLet i' adapted (substBndVar i i' ta body))
              _ -> MM.throwCompilerBug "crossPool: an argument to convert has no parameter in the callee"
        return
          ( not (Set.null toThere)
          , x'
          , if Set.null fromHere then Nothing else Just param )
crossPool lang _ fn xs = maybeSuspendRemoteReceive <$> sourceCall lang fn xs

-- | The arguments and result of a function type applied to @n@ arguments,
-- or a nullary value's type as its own result.
splitAtArity :: Int -> Type -> Maybe ([Type], Type)
splitAtArity n (FunT ins ret)
  | n == length ins = Just (ins, ret)
  | otherwise = Nothing
splitAtArity 0 t = Just ([], t)
splitAtArity _ _ = Nothing

-- | The signature a source function is declared with, and the type
-- variables it quantifies. The signature is found by the source it
-- implements, since the call's own index may belong to the definition
-- around it.
declaredSignature :: Int -> Source -> MorlocMonad (Set.Set TVar, Maybe Type)
declaredSignature _ src = do
  sgmap <- MM.gets stateSignatures
  let declared = [e | sg <- GMap.elems sgmap, Just e <- [signatureOf sg]]
  return $ case declared of
    (e : _) ->
      let (vs, t) = unqualify (etype e)
       in (Set.fromList vs, Just (unresolvedType2type t))
    [] -> (Set.empty, Nothing)
  where
    signatureOf (Monomorphic (TermTypes (Just e) srcs _))
      | any (sameSource . snd) srcs = Just e
    signatureOf (Polymorphic _ _ e ts)
      | any (any (sameSource . snd) . termConcrete) ts = Just e
    signatureOf _ = Nothing
    -- The host function itself: its name in its language and file, whatever
    -- alias a module gives it.
    sameSource (Idx _ s) = srcName s == srcName src && srcPath s == srcPath src && srcLang s == srcLang src

-- | The other direction of a callback boundary: a host that receives a
-- morloc callback may call it with a value of its own making, so each
-- parameter of the callback arrives in the host's convention. Its declared
-- type is the host's view of it, and the body sees the value through
-- inbound 'adapt', so every use inside the callback is an ordinary morloc
-- value again.
adaptCallbackParams :: Lang -> Int -> HostView -> [Maybe Type] -> PolyExpr -> MorlocMonad PolyExpr
adaptCallbackParams lang m hv ds (PolyManifold l mi (ManifoldPart ctx bnd) k body) = do
  (bnd', body') <- foldM adaptOne (bnd, body) (zip ds bnd)
  return (PolyManifold l mi (ManifoldPart ctx bnd') k body')
  where
    adaptOne (bs, b) (d, Arg i (Just t))
      | needsAdapt hv d t = do
          i' <- MM.getCounter
          let hostT = hostType hv d t
          adapted <- adapt Inbound lang m hv d t (PolyBndVar (C (Idx m hostT)) i)
          MM.modify $ \st -> st {stateArgTypes =
            Map.insert i (Idx m hostT) (Map.insert i' (Idx m t) (stateArgTypes st))}
          let bs' = [if j == i then Arg j (Just hostT) else a | a@(Arg j _) <- bs]
          return (bs', PolyLet i' adapted (substBndVar i i' t b))
    adaptOne acc _ = return acc
adaptCallbackParams _ _ _ _ e = return e

-- | Which way a value crosses between morloc and a host. A host has no
-- suspensions: a function it holds returns its result, where a morloc
-- function value of type @A -> \<E\> C@ returns a suspension of it.
-- Outbound is a morloc value handed to a host, which calls it and takes the
-- result, so the adapter runs the suspension ("a morloc function handed to a
-- host at an @A -> \<E\> C@ slot is passed as @\a -> force (g a)@", the law,
-- rule 8). Inbound is a host value handed to morloc: applying the adapter
-- builds the suspension the type promises. An adapter is contravariant: a
-- function's arguments cross the other way.
data AdaptDir = Inbound | Outbound

flipDir :: AdaptDir -> AdaptDir
flipDir Inbound = Outbound
flipDir Outbound = Inbound

-- | How a host sees the values crossing one of its calls: the scope that
-- expands the types, and the type variables of its signature. The declared
-- type is walked beside the value's own, and below a type variable of the
-- declaration the host holds the value without looking inside it, so
-- nothing there is adapted: a sourced @map :: (a -> b) -> f a -> f b@ at
-- @b = \<IO\> C@ builds suspensions, it does not run them.
--
-- The last field names the record types whose value in this pool is already
-- the host's ('hostRecords'): nothing inside one is adapted where it
-- crosses, because it was adapted where it was built.
--
-- The last field names the records being rebuilt around the value at hand,
-- so a type that contains itself is refused rather than rebuilt forever.
--
-- The last field limits a conversion to the functions inside the named
-- records ('crossPool'); with 'Nothing' every function is converted.
data HostView = HostView
  { _hvScope :: Scope
  , _hvOpaque :: Set.Set TVar
  , _hvHostRecs :: Set.Set TVar
  , _hvActive :: Set.Set Type
  , hvOnly :: Maybe (Set.Set TVar)
  }

-- | A type with the aliases and record names at its head expanded.
expandT :: Scope -> Type -> Type
expandT scope t = go (64 :: Int) (type2typeu t)
  where
    -- Only the head is resolved: the walks above expand each part as they
    -- reach it, and a type that recurs through an alias (@type Pat =
    -- [Pat]@) has no full expansion.
    go 0 u = unresolvedType2type u
    go n u = case TE.reduceType scope u of
      Just u' -> go (n - 1) u'
      Nothing -> unresolvedType2type u

-- | The declared type at a position is a type variable of the signature.
opaqueAt :: HostView -> Maybe Type -> Bool
opaqueAt (HostView _ vs _ _ _) (Just (VarT v)) = Set.member v vs
opaqueAt _ _ = False

-- | The declared type of each part of a declared type, where its shape
-- matches the value's; 'Nothing' where there is no declaration to consult,
-- and the value's own type decides.
declFun :: HostView -> Int -> Maybe Type -> ([Maybe Type], Maybe Type)
declFun (HostView scope _ _ _ _) n (Just d) = case expandT scope d of
  FunT ds r | length ds == n -> (map Just ds, Just r)
  _ -> (replicate n Nothing, Nothing)
declFun _ n Nothing = (replicate n Nothing, Nothing)

peelDecl :: Maybe Type -> Maybe Type
peelDecl (Just (EffectT _ d)) = Just d
peelDecl _ = Nothing

declAt :: HostView -> (Type -> Maybe a) -> Maybe Type -> Maybe a
declAt (HostView scope _ _ _ _) f (Just d) = f (expandT scope d)
declAt _ _ Nothing = Nothing

declArgs :: HostView -> Int -> Maybe Type -> [Maybe Type]
declArgs hv n d = case declAt hv (\x -> case x of AppT _ ds | length ds == n -> Just ds; _ -> Nothing) d of
  Just ds -> map Just ds
  Nothing -> replicate n Nothing

declField :: HostView -> Key -> Maybe Type -> Maybe Type
declField hv k = declAt hv (\x -> case x of NamT _ _ _ rs -> lookup k rs; _ -> Nothing)

declOptional :: HostView -> Maybe Type -> Maybe Type
declOptional hv = declAt hv (\x -> case x of OptionalT d -> Just d; _ -> Nothing)

declEffect :: HostView -> Maybe Type -> Maybe Type
declEffect hv = declAt hv (\x -> case x of EffectT _ d -> Just d; _ -> Nothing)

-- | Whether a function result's effect is run for the host here: not below
-- a type variable of the host's signature, and not where only the
-- functions inside certain records are converted and this one is outside.
runs :: HostView -> Maybe Type -> Bool
runs hv d = isNothing (hvOnly hv) && not (opaqueAt hv d)

-- | The view inside a record: every function in one of the named records
-- is converted.
withinRecord :: TVar -> HostView -> HostView
withinRecord v hv = case hvOnly hv of
  Just rs | Set.member v rs -> hv {hvOnly = Nothing}
  _ -> hv

-- | @eager[[ ]]@ (the law, rule 8): the type a value has on the host's side
-- of the boundary. A host has no thunks, so a function it holds returns its
-- result rather than a suspension of it, at every depth -- a function's
-- arguments and result, a container's elements, a record's fields, the
-- result of a suspension (@eager[[\<E\> R]] = () -> eager[[R]]@). A type
-- with nothing to adapt is returned as it was written.
hostType :: HostView -> Maybe Type -> Type -> Type
hostType hv d t = maybe t id (hostTypeM hv d t)

needsAdapt :: HostView -> Maybe Type -> Type -> Bool
needsAdapt hv d t = isJust (hostTypeM hv d t)

-- | 'Nothing' where the host's view of the value is the value's own type.
hostTypeM :: HostView -> Maybe Type -> Type -> Maybe Type
hostTypeM hv = hostTypeOn hv Set.empty

-- | 'hostTypeM' below the records on the path to this type. A record
-- reached again inside itself is taken as it is here; 'rebuild' refuses
-- to rebuild a value of such a type.
hostTypeOn :: HostView -> Set.Set Type -> Maybe Type -> Type -> Maybe Type
hostTypeOn hv@(HostView scope _ hostRecs _ _) seen0 d t
  | opaqueAt hv d = Nothing
  | any (`Set.member` seen0) here = Nothing
  | otherwise = case expandT scope t of
      FunT as r ->
        let (das, dr) = declFun hv (length as) d
            as' = zipWith (hostTypeOn hv seen) das as
            r' = case r of
              EffectT _ c | runs hv dr -> Just (fromMaybe c (hostTypeOn hv seen (declEffect hv dr) c))
              _ -> hostTypeOn hv seen dr r
         in if all isNothing as' && isNothing r'
              then Nothing
              else Just (FunT (zipWith fromMaybe as as') (fromMaybe r r'))
      EffectT effs c -> EffectT effs <$> hostTypeOn hv seen (declEffect hv d) c
      NamT _ v _ _ | Set.member v hostRecs -> Nothing
      NamT o v ps rs ->
        let rs' = [hostTypeOn (withinRecord v hv) seen (declField hv k d) ft | (k, ft) <- rs]
         in if all isNothing rs'
              then Nothing
              else Just (NamT o v ps [(k, fromMaybe ft ft') | ((k, ft), ft') <- zip rs rs'])
      OptionalT a -> OptionalT <$> hostTypeOn hv seen (declOptional hv d) a
      AppT h ts ->
        let ts' = zipWith (hostTypeOn hv seen) (declArgs hv (length ts) d) ts
         in if all isNothing ts'
              then Nothing
              else Just (AppT h (zipWith fromMaybe ts ts'))
      _ -> Nothing
  where
    here = typeKeys scope t
    seen = foldr Set.insert seen0 here

-- | Adapt a value crossing the host boundary in direction @dir@: @t@ is the
-- value's morloc type and @d@ the host's declaration of it. The expression
-- is in the convention of the side it comes from, and the result in the
-- convention of the side it goes to. A value with nothing to adapt is
-- returned untouched.
--
-- A value written in place -- a lambda, a container literal, a remote call
-- -- is adapted where it is built. Any other value is bound once and rebuilt
-- by its type: a function or suspension is wrapped, a record or tuple is
-- rebuilt field by field, and a list is mapped over. A type that cannot be
-- rebuilt is refused.
adapt :: AdaptDir -> Lang -> Int -> HostView -> Maybe Type -> Type -> PolyExpr -> MorlocMonad PolyExpr
adapt dir lang g hv@(HostView scope _ _ _ _) d t0 e
  | not (needsAdapt hv d t0) = return e
  | otherwise = case e of
      PolyReturn x -> PolyReturn <$> adapt dir lang g hv d t0 x
      PolyLet i v x -> PolyLet i v <$> adapt dir lang g hv d t0 x
      PolyManifold l m f k x | not (isLambdaForm f) -> do
        hv' <- inPool l m hv t0
        PolyManifold l m f k <$> adapt dir l m hv' d t0 x
      PolyManifold l m f@(ManifoldPart _ _) k body
        | Outbound <- dir, FunT as r <- t -> do
            hv' <- inPool l m hv t0
            let (das, dr) = declFun hv' (length as) d
            body' <- case r of
              EffectT _ c | runs hv' dr ->
                mapReturnPosition (adapt dir l m hv' (declEffect hv' dr) c) (forceReturnPosition m body)
              _ -> mapReturnPosition (adapt dir l m hv' dr r) body
            adaptCallbackParams l m hv' das (PolyManifold l m f k body')
      PolyApp (PolyRemoteInterface l (Idx i ti) ids rf inner) xs
        | Outbound <- dir -> do
            hv' <- inPool l g hv t0
            inner' <- adapt dir l g hv' d t0 inner
            return $ PolyApp (PolyRemoteInterface l (Idx i (hostType hv d ti)) ids rf inner') xs
      PolyList v ts xs
        | AppT _ [a] <- t -> do
            let da = case declArgs hv 1 d of [x] -> x; _ -> Nothing
            xs' <- mapM (adapt dir lang g hv da a) xs
            return $ PolyList v [Idx i (hostType hv da ty) | Idx i ty <- ts] xs'
      PolyTuple v xs
        | AppT _ ts <- t, length ts == length xs -> do
            let ds = declArgs hv (length ts) d
            xs' <- sequence
              [ (,) (Idx i (hostType hv dx tx)) <$> adapt dir lang g hv dx tx x
              | (dx, tx, (Idx i _, x)) <- zip3 ds ts xs ]
            return $ PolyTuple v xs'
      PolyRecord o v ps rs
        | NamT _ name _ fts <- t -> do
            rs' <- sequence
              [ case lookup k fts of
                  Just ft -> do
                    let dk = declField hv k d
                    x' <- adapt dir lang g (withinRecord name hv) dk ft x
                    return (k, (Idx i (hostType (withinRecord name hv) dk ft), x'))
                  Nothing -> return (k, (Idx i ty, x))
              | (k, (Idx i ty, x)) <- rs ]
            return $ PolyRecord o v ps rs'
      PolyCoerce c (Idx i ty) x
        | OptionalT a <- t -> do
            x' <- adapt dir lang g hv (declOptional hv d) a x
            return $ PolyCoerce c (Idx i (hostType hv d ty)) x'
      PolyNull (Idx i ty) -> return $ PolyNull (Idx i (hostType hv d ty))
      _ -> rebuild dir lang g hv d t e
  where
    t = expandT scope t0

-- | The host view of values built in another pool: which records hold the
-- host's values depends on the language they live in.
inPool :: Lang -> Int -> HostView -> Type -> MorlocMonad HostView
inPool l g (HostView scope opaque _ active only) t = (\r -> HostView scope opaque r active only) <$> hostRecords l g [t]

-- | Apply a transformation to the value a manifold body returns: through
-- 'PolyReturn', 'PolyLet', an inline manifold, both arms of a conditional,
-- and the base leaves of a loop.
mapReturnPosition :: (PolyExpr -> MorlocMonad PolyExpr) -> PolyExpr -> MorlocMonad PolyExpr
mapReturnPosition f = go
  where
    go (PolyReturn e) = PolyReturn <$> go e
    go (PolyLet i v e) = PolyLet i v <$> go e
    go (PolyManifold l m fm k e) | not (isLambdaForm fm) = PolyManifold l m fm k <$> go e
    go (PolyIf c a b) = PolyIf c <$> go a <*> go b
    go (PolyLoop t ids e) = PolyLoop t ids <$> goLoop e
    go e = f e
    goLoop (PolyIf c a b) = PolyIf c <$> goLoop a <*> goLoop b
    goLoop (PolyLet i v x) = PolyLet i v <$> goLoop x
    goLoop cont@(PolyLoopContinue _) = return cont
    goLoop base = go base

-- | Adapt a value by its type alone: bind it once, then wrap or rebuild it.
rebuild :: AdaptDir -> Lang -> Int -> HostView -> Maybe Type -> Type -> PolyExpr -> MorlocMonad PolyExpr
rebuild dir lang g (HostView scope opaque hostRecs active0 only) d t0 e
  | any (`Set.member` active0) (typeKeys scope t0) =
      refuseCrossing lang g t0 "its type contains itself"
  | otherwise = rebuildIn dir lang g
      (HostView scope opaque hostRecs (foldr Set.insert active0 (typeKeys scope t0)) only) d t0 e

rebuildIn :: AdaptDir -> Lang -> Int -> HostView -> Maybe Type -> Type -> PolyExpr -> MorlocMonad PolyExpr
rebuildIn dir lang g hv@(HostView scope _ _ _ _) d t0 e = do
  v <- MM.getCounter
  let here = case dir of
        Outbound -> t
        Inbound -> hostType hv d t
      var = PolyLetVar (Idx g here) v
  body <- case t of
    FunT as r -> wrapFunction v here as r
    EffectT effs c -> do
      let dc = declEffect hv d
          forced = PolyEval (Idx g (case dir of Outbound -> c; Inbound -> hostType hv dc c)) var
      inner <- adapt dir lang g hv dc c forced
      return $ PolyDoBlock (Idx g (EffectT effs (case dir of Outbound -> hostType hv dc c; Inbound -> c))) inner
    NamT o name ps fts -> do
      fs <- sequence
        [ do
            let dk = declField hv k d
                here' = case dir of Outbound -> ft; Inbound -> hostType (withinRecord name hv) dk ft
                there = case dir of Outbound -> hostType (withinRecord name hv) dk ft; Inbound -> ft
                getter = PolyApp
                  (PolyExe (Idx g (FunT [here] here')) (PatCallP (PatternStruct (SelectorKey (unKey k, SelectorEnd) []))))
                  [var]
            x <- adapt dir lang g (withinRecord name hv) dk ft getter
            return (k, (Idx g there, x))
        | (k, ft) <- fts ]
      return $ PolyRecord o (Idx g name) (map (Idx g) ps) fs
    AppT (VarT h) ts | h == BT.tuple (length ts) -> do
      let ds = declArgs hv (length ts) d
      xs <- sequence
        [ do
            let here' = case dir of Outbound -> tx; Inbound -> hostType hv dx tx
                there = case dir of Outbound -> hostType hv dx tx; Inbound -> tx
                getter = PolyApp
                  (PolyExe (Idx g (FunT [here] here')) (PatCallP (PatternStruct (SelectorIdx (i, SelectorEnd) []))))
                  [var]
            x <- adapt dir lang g hv dx tx getter
            return (Idx g there, x)
        | (i, dx, tx) <- zip3 [0 ..] ds ts ]
      return $ PolyTuple (Idx g h) xs
    OptionalT a -> do
      let da = declOptional hv d
      (each, aThere) <- element da a
      return $ PolyIntrinsic (Idx g (OptionalT aThere)) IntrMapOptional [each, var]
    AppT (VarT h) ts -> do
      mapSrc <- case ts of
        [_] -> resolveInstanceForType findFunctorMap lang t
        _ -> return Nothing
      case (mapSrc, ts) of
        (Just src, [a]) -> do
          let da = case declArgs hv 1 d of [x] -> x; _ -> Nothing
              there = case dir of Outbound -> hostType hv d t; Inbound -> t
          (each, aThere) <- element da a
          return $ PolyApp (PolyExe (Idx g (FunT [FunT [elemHere da a] aThere, here] there)) (SrcCallP src)) [each, var]
        _ -> packed h ts var here
    _ -> refuse
  return (PolyLet v e body)
  where
    t = expandT scope t0

    elemHere da a = case dir of
      Outbound -> a
      Inbound -> hostType hv da a

    -- A closure adapting one element of a container, and the element's
    -- type after it.
    element da a = do
      let aHere = elemHere da a
          aThere = case dir of Outbound -> hostType hv da a; Inbound -> a
      y <- MM.getCounter
      m <- MM.freshManifoldIndex g
      MM.modify $ \st -> st {stateArgTypes = Map.insert y (Idx g aHere) (stateArgTypes st)}
      x <- adapt dir lang g hv da a (PolyBndVar (C (Idx g aHere)) y)
      return (PolyManifold lang m (ManifoldPart [] [Arg y (Just aHere)]) Transparent (PolyReturn x), aThere)

    -- A type with a Packable instance in this language is adapted through
    -- the form it packs to: unpacked, adapted, and packed again.
    packed h ts var here = do
      instances <- findPackerInstances
      case [ (vs, headArgs, wire, srcs)
           | pin <- instances
           , let (vs, headU) = unqualify (piHead pin)
           , AppU (VarU h') headArgs <- [headU]
           , h' == h
           , length headArgs == length ts
           , Just srcs <- [Map.lookup lang (piSources pin)]
           , let wire = snd (unqualify (piWire pin))
           ] of
        ((_, headArgs, wireU, (packSrc, unpackSrc)) : _) -> do
          let instantiate args = foldr
                (\(p, a) w -> case p of VarU pv -> substituteTVar pv a w; _ -> w)
                (unresolvedType2type wireU)
                (zip headArgs args)
              wireT = instantiate ts
              dWire = case declArgs hv (length ts) d of
                ds | all isJust ds -> Just (instantiate (catMaybes ds))
                _ -> Nothing
              wireHere = case dir of Outbound -> wireT; Inbound -> hostType hv dWire wireT
              wireThere = case dir of Outbound -> hostType hv dWire wireT; Inbound -> wireT
              there = case dir of Outbound -> hostType hv d t; Inbound -> t
              unpacked = PolyApp (PolyExe (Idx g (FunT [here] wireHere)) (SrcCallP unpackSrc)) [var]
          adapted <- adapt dir lang g hv dWire wireT unpacked
          return $ PolyApp (PolyExe (Idx g (FunT [wireThere] there)) (SrcCallP packSrc)) [adapted]
        [] -> refuse

    refuse :: MorlocMonad a
    refuse = refuseCrossing lang g t0 ("a" <+> pretty lang <+> "value of this type cannot be rebuilt to run it for the host")

    -- A function value: a closure of the pool's own making that calls it,
    -- converting each argument the other way and the result this way.
    wrapFunction v calleeT as r = do
      let (das, dr) = declFun hv (length as) d
          slotT da a = case dir of
            Inbound -> a
            Outbound -> hostType hv da a
      ids <- mapM (const MM.getCounter) as
      m <- MM.freshManifoldIndex g
      args <- sequence
        [ adapt (flipDir dir) lang g hv da a (PolyBndVar (C (Idx g (slotT da a))) i)
        | (i, da, a) <- zip3 ids das as ]
      let applied = PolyApp (PolyExe (Idx g calleeT) (LocalCallP v)) args
      body <- case r of
        EffectT effs c | runs hv dr -> do
          let dc = declEffect hv dr
          case dir of
            Outbound -> adapt dir lang g hv dc c (PolyEval (Idx g c) applied)
            Inbound -> PolyDoBlock (Idx g (EffectT effs c)) <$> adapt dir lang g hv dc c applied
        _ -> adapt dir lang g hv dr r applied
      MM.modify $ \st -> st
        { stateArgTypes =
            foldr (\(i, ty) -> Map.insert i (Idx g ty)) (stateArgTypes st)
              ((v, calleeT) : [(i, slotT da a) | (i, da, a) <- zip3 ids das as])
        -- A host's callable has no identity to send to another pool, and
        -- re-running the call that produced it would run its effects again,
        -- so a closure wrapping one may not cross.
        , stateHostOriginClosures = case dir of
            Inbound -> Set.insert m (stateHostOriginClosures st)
            Outbound -> stateHostOriginClosures st
        }
      return $ PolyManifold lang m
        (ManifoldPart [Arg v None] [Arg i (Just (slotT da a)) | (i, da, a) <- zip3 ids das as])
        Transparent (PolyReturn body)

-- | The function @replay and @spawn call as @f(x)@ in their pool helper,
-- exactly as a host calls a callback: @replay :: IStream a -> ([a] -> \<IO\>
-- ()) -> \<IO\> ()@ and @spawn :: IStream a -> (OStream a -> \<IO\> ()) ->
-- \<IO\> ()@, whose result type is the function's.
intrinsicCallback :: Lang -> Int -> Intrinsic -> Type -> PolyExpr -> PolyExpr -> MorlocMonad PolyExpr
intrinsicCallback lang m intr resT h f = do
  scope <- MM.getGeneralScope
  let argT a = case intr of
        IntrReplay -> AppT (VarT BT.list) [a]
        _ -> AppT (VarT BT.ostreamVar) [a]
  case expandT scope <$> polyOuterType h of
    Just (AppT _ [a]) -> adapt Outbound lang m (HostView scope Set.empty Set.empty Set.empty Nothing) Nothing (FunT [argT a] resT) f
    _ -> case f of
      PolyManifold l m' form k body | isLambdaForm form ->
        return (PolyManifold l m' form k (forceReturnPosition m' body))
      _ -> MM.throwCompilerBug $ "The function passed to" <+> viaShow intr <+> "has no known type"

-- | The record types among these whose value in this pool is already the
-- host's. In a compiled language a record mapped to a type the program
-- declares (@record Rust => Ops = "Ops"@) has the field types written in
-- that declaration, which follow the host's convention: building one is the
-- outbound boundary for its fields and reading a field the inbound one, and
-- the value crosses to a host as it is. A record the compiler generates
-- (@"struct"@), and every record of a dynamic language, holds morloc values.
hostRecords :: Lang -> Int -> [Type] -> MorlocMonad (Set.Set TVar)
hostRecords lang g ts = do
  reg <- MM.gets stateLangRegistry
  if not (LR.registryIsCompiled reg (ML.langName lang))
    then return Set.empty
    else do
      scope <- MM.getGeneralScope
      let recs = Map.fromList [(v, t) | t@(NamT _ v _ _) <- concatMap (subTypes scope) ts]
      Set.fromList . catMaybes <$> mapM declared (Map.toList recs)
  where
    declared (v, t) = do
      st0 <- CMS.get
      ( do
          c <- inferConcreteType lang (Idx g t)
          return $ case c of
            NamF _ (FV _ (CV cv)) _ _ | cv /= "struct" -> Just v
            _ -> Nothing
        ) `catchError` (\_ -> CMS.put st0 >> return Nothing)

-- | Refuse a value that cannot be adapted where it crosses to a host.
refuseCrossing :: Lang -> Int -> Type -> MDoc -> MorlocMonad a
refuseCrossing lang g t why = MM.throwSourcedError g $
  "A value of type" <+> pretty t <+> "cannot cross between morloc and a"
    <+> pretty lang <+> "function: it holds a function whose result is an effect,"
    <+> "and" <+> why <> "."

-- | How a type is known on the path from an outer type to it: as written,
-- and by name if it is a record. A type that recurs through an alias
-- (@type Pat = [Pat]@) or a record meets one of its keys again below itself.
typeKeys :: Scope -> Type -> [Type]
typeKeys scope t = t : case expandT scope t of
  NamT _ v _ _ -> [VarT v]
  _ -> []

-- | A type and every type inside it, each with its aliases and records
-- expanded.
subTypes :: Scope -> Type -> [Type]
subTypes scope = go Set.empty
  where
    go seen0 t0
      | any (`Set.member` seen0) (typeKeys scope t0) = []
      | otherwise =
          let seen = foldr Set.insert seen0 (typeKeys scope t0)
           in case expandT scope t0 of
                t@(NamT _ _ ps rs) -> t : concatMap (go seen) (ps <> map snd rs)
                t@(FunT as r) -> t : concatMap (go seen) (r : as)
                t@(AppT h as) -> t : concatMap (go seen) (h : as)
                t@(EffectT _ x) -> t : go seen x
                t@(OptionalT x) -> t : go seen x
                t -> [t]

-- | Whether a field access reads through a record whose value is the
-- host's, so that what it reads is in the host's convention.
readsHostRecord :: HostView -> Selector -> Type -> Bool
readsHostRecord hv@(HostView scope _ hostRecs _ _) sel t = case (sel, expandT scope t) of
  (SelectorKey (k, rest) [], NamT _ v _ rs) ->
    Set.member v hostRecs || maybe False (readsHostRecord hv rest) (lookup (Key k) rs)
  (SelectorIdx (i, rest) [], AppT _ ts) | i < length ts -> readsHostRecord hv rest (ts !! i)
  _ -> False

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

-- | A lambda-shaped manifold form (an unapplied or partially-applied
-- function value), as opposed to a saturated 'ManifoldFull' call.
isLambdaForm :: ManifoldForm c b -> Bool
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
  PolyManifold l m f k (forceValuePosition m body)
forceCalleeBody e = e

-- | 'forceReturnPosition' for the value a manifold returns: a closure there
-- is the value itself (a function), and what its body returns is the
-- closure's own business, forced where it is called.
forceValuePosition :: Int -> PolyExpr -> PolyExpr
forceValuePosition m (PolyReturn e) = PolyReturn (forceValuePosition m e)
forceValuePosition m (PolyLet i v e) = PolyLet i v (forceValuePosition m e)
forceValuePosition _ e@(PolyManifold _ _ f _ _) | isLambdaForm f = e
forceValuePosition m (PolyManifold l m' f k e) =
  PolyManifold l m' f k (forceValuePosition m e)
forceValuePosition m e = forceReturnPosition m e

