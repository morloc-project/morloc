{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.CodeGenerator.Reduce
Description : Compile-time reduction of intrinsics
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Walks the SerialManifold tree after serialization and replaces compile-time
intrinsics (@version, @compiled, @lang, @schema, @typeof, @datafile) with
string literals. Runtime intrinsics (@save, @load, @hash) pass through
unchanged to code generation.
-}
module Morloc.CodeGenerator.Reduce (reduce) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Control.Monad.State (gets)
import Morloc.CodeGenerator.Namespace
import Morloc.ProgramBuilder.Paths (resolveDatafileAgainstRoot)
import qualified Morloc.Version as V

reduce :: SerialManifold -> MorlocMonad SerialManifold
reduce sm = do
  timestamp <- liftIO $ do
    now <- getCurrentTime
    return . T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
  let ver = T.pack V.versionStr
  reduceManifold ver timestamp sm

reduceManifold :: Text -> Text -> SerialManifold -> MorlocMonad SerialManifold
reduceManifold ver ts (SerialManifold m lang form hf se) =
  SerialManifold m lang form hf <$> reduceSerialExpr ver ts lang se

reduceSerialExpr :: Text -> Text -> Lang -> SerialExpr -> MorlocMonad SerialExpr
reduceSerialExpr ver ts _ (ManS sm) = ManS <$> reduceManifold ver ts sm
reduceSerialExpr ver ts lang (AppPoolS t pc args) =
  AppPoolS t pc <$> mapM (reduceSerialArg ver ts lang) args
reduceSerialExpr ver ts lang (AppRecS t i args) =
  AppRecS t i <$> mapM (reduceSerialExpr ver ts lang) args
reduceSerialExpr ver ts lang (AppForeignRecS t i sock args) =
  AppForeignRecS t i sock <$> mapM (reduceSerialExpr ver ts lang) args
reduceSerialExpr ver ts lang (ReturnS se) = ReturnS <$> reduceSerialExpr ver ts lang se
reduceSerialExpr ver ts lang (SerialLetS i e1 e2) =
  SerialLetS i <$> reduceSerialExpr ver ts lang e1 <*> reduceSerialExpr ver ts lang e2
reduceSerialExpr ver ts lang (NativeLetS i ne se) =
  NativeLetS i <$> reduceNativeExpr ver ts lang ne <*> reduceSerialExpr ver ts lang se
reduceSerialExpr ver ts lang (SerializeS ast ne) =
  SerializeS ast <$> reduceNativeExpr ver ts lang ne
reduceSerialExpr ver ts lang (CacheBodyS t resSa lbl mid args body) =
  CacheBodyS t resSa lbl mid args <$> reduceSerialExpr ver ts lang body
reduceSerialExpr ver ts lang (DebugWrapS t mid args body) =
  DebugWrapS t mid args <$> reduceSerialExpr ver ts lang body
-- Reduce descends INTO each loop-body leaf (peepholes on the per-iteration
-- guard/let/base/continue work) but the loop is a scope barrier.
reduceSerialExpr ver ts lang (LoopS t ids body) =
  LoopS t ids
    <$> bimapM (reduceNativeExpr ver ts lang) (reduceSerialExpr ver ts lang) body
reduceSerialExpr _ _ _ e = return e

reduceSerialArg :: Text -> Text -> Lang -> SerialArg -> MorlocMonad SerialArg
reduceSerialArg ver ts _ (SerialArgManifold sm) = SerialArgManifold <$> reduceManifold ver ts sm
reduceSerialArg ver ts lang (SerialArgExpr se) = SerialArgExpr <$> reduceSerialExpr ver ts lang se

reduceNativeManifold :: Text -> Text -> NativeManifold -> MorlocMonad NativeManifold
reduceNativeManifold ver ts (NativeManifold m lang form ne) =
  NativeManifold m lang form <$> reduceNativeExpr ver ts lang ne

reduceNativeExpr :: Text -> Text -> Lang -> NativeExpr -> MorlocMonad NativeExpr
-- compile-time intrinsics: replace with string literals
reduceNativeExpr ver _ _ (IntrinsicN t IntrVersion _ []) = return $ makeStr t ver
reduceNativeExpr _ ts _ (IntrinsicN t IntrCompiled _ []) = return $ makeStr t ts
reduceNativeExpr _ _ lang (IntrinsicN t IntrLang _ []) = return $ makeStr t (langName lang)
-- @datafile: resolve relative path to installed data file location
reduceNativeExpr ver ts lang (IntrinsicN t IntrDatafile _ [pathArg]) = do
  pathArg' <- reduceNativeExpr ver ts lang pathArg
  case extractStr pathArg' of
    Just relPath -> do
      -- Data files are mirrored beside sources at the ROOT; resolve against
      -- the source root (shared with the nexus side via Paths).
      mRoot <- gets stateBuildRoot
      return $ makeStr t (resolveDatafileAgainstRoot mRoot relPath)
    Nothing ->
      return $ makeStr t "<datafile: could not resolve path>"
-- runtime intrinsics: recurse into children but keep the intrinsic node
reduceNativeExpr ver ts lang (IntrinsicN t intr msch es) =
  IntrinsicN t intr msch <$> mapM (reduceNativeExpr ver ts lang) es
-- recursive cases
reduceNativeExpr ver ts _ (ManN nm) = ManN <$> reduceNativeManifold ver ts nm
reduceNativeExpr ver ts lang (AppExeN t exe args) =
  AppExeN t exe <$> mapM (reduceNativeArg ver ts lang) args
reduceNativeExpr ver ts lang (ReturnN ne) = ReturnN <$> reduceNativeExpr ver ts lang ne
reduceNativeExpr ver ts lang (SerialLetN i se ne) =
  SerialLetN i <$> reduceSerialExpr ver ts lang se <*> reduceNativeExpr ver ts lang ne
reduceNativeExpr ver ts lang (NativeLetN i ne1 ne2) = do
  ne1' <- reduceNativeExpr ver ts lang ne1
  ne2' <- reduceNativeExpr ver ts lang ne2
  -- Whether a let-bound variable holds a thunk is a property of what was bound
  -- to it, and this is the only place both are visible. A binding that is
  -- plainly a value can never need forcing at its use sites, so a force landing
  -- on one of those references is spurious. Anything else keeps its force: an
  -- effectful call binds a suspended computation, and @catch depends on that
  -- suspension surviving to the point where it can be intercepted.
  return $
    if isProvablyPlain ne1'
      then NativeLetN i ne1' (dropForceOn i ne2')
      else NativeLetN i ne1' ne2'
reduceNativeExpr ver ts lang (DeserializeN t ast se) =
  DeserializeN t ast <$> reduceSerialExpr ver ts lang se
reduceNativeExpr ver ts lang (ListN fv t es) =
  ListN fv t <$> mapM (reduceNativeExpr ver ts lang) es
reduceNativeExpr ver ts lang (TupleN fv es) =
  TupleN fv <$> mapM (reduceNativeExpr ver ts lang) es
reduceNativeExpr ver ts lang (RecordN o fv tps rs) =
  RecordN o fv tps <$> mapM (\(k, ne) -> (,) k <$> reduceNativeExpr ver ts lang ne) rs
reduceNativeExpr ver ts lang (DoBlockN t ne) = DoBlockN t <$> reduceNativeExpr ver ts lang ne
-- Forcing a conditional is forcing whichever branch it takes, so the force
-- distributes over the arms and the conditional itself becomes eager. This is
-- what lets an effect-typed `?`/`:` be used as a value: a deferred effect is
-- carried as a nullary thunk, and two arms producing thunks produce two
-- DIFFERENT thunk types, which no single binding can name. Forcing inside each
-- arm leaves the conditional yielding the value type both arms agree on.
--
-- Distributing also subsumes the wrapped case. An effect-typed conditional
-- reaches here with both arms wrapped in a DoBlockN (Serialize.hs
-- `wrapEffectArms`), and pushing the force inside cancels against each wrap by
-- the `EvalN . DoBlockN = id` rule below -- eliminating the
-- `helper0 = (lambda: X); ...; n7()` round-trip that every `?`/`:` inside a do
-- block would otherwise pay.
--
-- The forced conditional is eager, so its type is the effect-stripped value
-- type: an arm coerced into the effect slot carries an EffectF type but reduces
-- to a plain value here, and typing the IfN by that raw EffectF would make the
-- pool declare a thunk and then assign it a bare value.
reduceNativeExpr ver ts lang (EvalN et (IfN _ c th el)) = do
  c' <- reduceNativeExpr ver ts lang c
  th' <- reduceNativeExpr ver ts lang (EvalN (stripEffectF (typeFof th)) th)
  el' <- reduceNativeExpr ver ts lang (EvalN (stripEffectF (typeFof el)) el)
  return $ IfN (stripEffectF et) c' th' el'
-- Generalized `EvalN . DoBlockN = id`, robust to a CoerceToOptional wedged
-- between the force and the thunk. This is the optional-widened `@catch` shape:
-- `@catch` collapses to a plain-typed value and the `<E>`+`?` migrate onto a
-- CoerceN, so a naive `EvalN (DoBlockN ..)` match misses it and a spurious
-- `mlc_catch(..)()` is emitted on a value. Peel the transparent coerce stack to
-- the head, then:
--   * DoBlockN head      -> commute the force inside and cancel (rebuild coerces)
--   * provably-plain head -> the force is spurious: drop it, keep the coerces so
--                            `?T` widening (incl. Rust `Some(..)`) survives
--   * anything else       -> KEEP the force (fail-closed: never silently drop a
--                            force that might be load-bearing).
-- Unit-typed DoBlockN is still skipped (C++ `void` return; see the ?/: peephole).
reduceNativeExpr ver ts lang (EvalN et wrapped) =
  case peelCoerce wrapped of
    (rebuild, DoBlockN t x)
      | not (isDoBlockUnit t) -> rebuild <$> reduceNativeExpr ver ts lang x
    (rebuild, inner)
      | isProvablyPlain inner -> rebuild <$> reduceNativeExpr ver ts lang (retypeAsValue inner)
    _ -> EvalN et <$> reduceNativeExpr ver ts lang wrapped
reduceNativeExpr ver ts lang (CoerceN c t ne) = CoerceN c t <$> reduceNativeExpr ver ts lang ne
reduceNativeExpr ver ts lang (IfN t c th el) =
  IfN t <$> reduceNativeExpr ver ts lang c <*> reduceNativeExpr ver ts lang th <*> reduceNativeExpr ver ts lang el
reduceNativeExpr ver ts lang (MapOptionalN t wt src ne) =
  MapOptionalN t wt src <$> reduceNativeExpr ver ts lang ne
-- leaf nodes
reduceNativeExpr _ _ _ e = return e

reduceNativeArg :: Text -> Text -> Lang -> NativeArg -> MorlocMonad NativeArg
reduceNativeArg ver ts _ (NativeArgManifold nm) = NativeArgManifold <$> reduceNativeManifold ver ts nm
reduceNativeArg ver ts lang (NativeArgExpr ne) = NativeArgExpr <$> reduceNativeExpr ver ts lang ne

-- | True when the DoBlockN wraps a value whose inner type is Unit.
-- The C++ pool's `lcMakeDoBlock` special-cases this shape:
-- `[=](){ expr; return mlc::Unit{}; }` because the inner intrinsic call
-- (e.g. `_mlc_save_voidstar`) returns C++ `void`. If the peephole
-- strips this thunk, the `void` propagates into `put_value(void, ...)`
-- and pool.cpp fails to compile.
isDoBlockUnit :: TypeF -> Bool
isDoBlockUnit t = case stripEffectF t of
  VarF (FV tv _) -> tv == TV "Unit"
  _ -> False

-- | Peel a stack of CoerceN wrappers, returning a function that rebuilds them
-- around a now-EAGER value plus the innermost (head) node. The rebuilt coerces
-- are effect-stripped: the rebuild is applied only where the enclosing force is
-- dropped, so the value is eager and a leftover `EffectF` outer type would make
-- the pool declare a thunk (std::function) for a bare value -- the same hazard
-- the sibling ?/: peephole avoids via `stripEffectF` (see above). Only CoerceN
-- is peeled: its non-child fields are not reducible, so rebuilding is otherwise
-- sound. Any other node is treated as an opaque head (fail-closed).
peelCoerce :: NativeExpr -> (NativeExpr -> NativeExpr, NativeExpr)
peelCoerce (CoerceN c t x) =
  let (f, h) = peelCoerce x in (CoerceN c (stripEffectF t) . f, h)
peelCoerce e = (id, e)

-- | Rewrite the spurious forces on one let-bound index. Used only where the
-- bound expression has been shown to be a value.
dropForceOn :: Int -> NativeExpr -> NativeExpr
dropForceOn i = mmap defaultValue {mapNativeExpr = go}
  where
    go (EvalN _ (LetVarN t j)) | j == i = LetVarN (stripEffectF t) j
    -- as in 'retypeAsValue': the annotation was never true of this node
    go e = e

-- | Correct the annotation on a node that was mis-typed as a computation.
--
-- This does not turn a computation into a value -- there is no such operation.
-- A computation of type @\<E\> T@ yields a T only by being run, and running it
-- is a choice with consequences: @\<IO\> Time@ yields a different answer every
-- time. What this repairs is the inverse mistake. A plain value acquires an
-- @EffectF@ annotation purely from the position it was returned at, and that
-- annotation is what makes the pool declare a thunk (a Rust @MorlocFn0@, a C++
-- @std::function@) and then assign a bare value to it. The node never held a
-- computation, so the annotation was never true of it.
--
-- Applied only where the caller has already established, from the binding
-- rather than from the type, that the node holds a value.
retypeAsValue :: NativeExpr -> NativeExpr
retypeAsValue (BndVarN t i) = BndVarN (stripEffectF t) i
retypeAsValue e = e

-- | Nodes that are definitely values (never a suspended thunk), so a force
-- landing on one is spurious and may be dropped. Deliberately conservative:
-- anything not listed here stays opaque and KEEPS its force (fail-closed), so a
-- real thunk is never mistaken for a value and dropped.
isProvablyPlain :: NativeExpr -> Bool
isProvablyPlain IntN{}    = True
isProvablyPlain RealN{}   = True
isProvablyPlain StrN{}    = True
isProvablyPlain LogN{}    = True
isProvablyPlain NullN{}   = True
isProvablyPlain ListN{}   = True
isProvablyPlain TupleN{}  = True
isProvablyPlain RecordN{} = True
isProvablyPlain (IntrinsicN t _ _ _) = not (isEffectF t)
-- An argument arrives evaluated, so a force landing on a bare parameter is
-- spurious.
--
-- The type cannot decide this. A value of type @\<E\> T@ is a computation, and
-- a plain value returned at an @\<E\> T@ position acquires the same annotation
-- from its position alone -- so the two are indistinguishable by type while
-- being different things at runtime: one is a value, the other a suspension
-- that must be run to yield one. What separates them is shape. A computation
-- reaches a force as the expression that built it, a 'DoBlockN' or a 'ManN'
-- wrapping one, because an effectful value must be bound with '<-' before it
-- can be used where a plain type is expected, and that binding runs it in
-- place.
isProvablyPlain BndVarN{} = True
isProvablyPlain _ = False

makeStr :: TypeF -> Text -> NativeExpr
makeStr (VarF fv) x = StrN fv x
makeStr _ x = StrN (FV (TV "Str") (CV "str")) x

-- | Extract the string value from a StrN node
extractStr :: NativeExpr -> Maybe Text
extractStr (StrN _ x) = Just x
extractStr _ = Nothing
