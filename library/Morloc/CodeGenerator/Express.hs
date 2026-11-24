{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.CodeGenerator.Express
Description : Resolve type aliases and infer concrete types in manifold trees
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Transforms the parameterized 'AnnoS' trees into a form where every
node carries both its general type and inferred concrete type. This is
the step where language-specific type aliases are resolved and type
parameters are fully instantiated.
-}
module Morloc.CodeGenerator.Express
  ( express
  , addCacheWraps
  , addDebugWraps
  , addLoopWraps
  , addNativeRecEntries
  , etaReduceForwarders
  , bareFoldSteps
  , polyFreeVars
  , recursiveTargetOf
  ) where

import Data.Foldable (toList)
import Morloc.Data.Zip (zipPrefix, zipSame, zipWithSame)
import qualified Morloc.Data.Zip as Zip
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Morloc.BaseTypes as BT
import qualified Morloc.CodeGenerator.Grammars.Macro as Macro
import Morloc.CodeGenerator.EffectBoundary (insertExportBoundaries, polyOuterType)
import Morloc.CodeGenerator.Grammars.Common (hasManifoldLabel)
import Morloc.CodeGenerator.IFile
  ( WalkStep (..)
  , selectorToWalkSteps
  , walkStepsToPath
  , bracketIndexSteps
  , bracketSliceSteps
  )
import Morloc.CodeGenerator.Infer
import Morloc.CodeGenerator.LanguageDescriptor (ldAllowStringNull, loadLangDescriptorFromText)
import qualified Morloc.DataFiles as DF
import qualified Morloc.Language as ML
import Morloc.CodeGenerator.Namespace
import Morloc.Data.Doc
import qualified Morloc.Data.Map as Map
import qualified Morloc.LangRegistry as LR
import qualified Morloc.Monad as MM
import qualified Morloc.TypeEval as TE
import qualified Morloc.Frontend.Instances as FI
import Morloc.Typecheck.Internal (emptyGamma, functorMapMethod, getterMethods, indexMethods, prettyConstraint, sliceMethods, unqualify)
import Morloc.CodeGenerator.Serial (containsEffectT, containsFunT, findCoveringPacker, makeSerialAST)
import GHC.Stack (HasCallStack, withFrozenCallStack)

mkIdx :: AnnoS g One (Indexed c, d) -> Type -> Indexed Type
mkIdx (AnnoS _ (Idx i _, _) _) = Idx i

-- | Smart constructor that classifies a PolyManifold by whether its midx
-- carries a label, whose hooks need the manifold to stay a function of its
-- own; later passes read the 'ManifoldKind' rather than the configuration.
mkPolyManifold ::
  Lang -> Int -> ManifoldForm None (Maybe Type) -> PolyExpr -> MorlocMonad PolyExpr
mkPolyManifold lang midx form body = do
  observable <- hasManifoldLabel midx
  -- the flat entry of a staged closure: its stage entry takes the same
  -- context arguments, so the runtime can call it with the captured values
  staged <- Map.member midx <$> MM.gets stateStageEntries
  when staged $ case form of
    ManifoldPart ctx _ -> MM.modify' (\st -> st {stateStageContext = Map.insert midx (length ctx) (stateStageContext st)})
    ManifoldPass _ -> MM.modify' (\st -> st {stateStageContext = Map.insert midx 0 (stateStageContext st)})
    _ -> return ()
  let kind = if observable then Preserved else Transparent
  return $ PolyManifold lang midx form kind body

-- C3 invariant: the record literal's key order must match the declared
-- schema's key order. The bidirectional checkE rule in
-- Frontend/Typecheck.hs reorders NamS against the declared NamU; this
-- assertion catches any future regression of that pass and turns a
-- silent wire-format corruption into a sourced compiler-internal error.
assertRecordKeyOrder ::
  Int -> [(Key, a)] -> [(Key, b)] -> MorlocMonad ()
assertRecordKeyOrder midx entries rs
  | map fst entries == map fst rs = return ()
  | otherwise = MM.throwSourcedError midx $
      "Compiler-internal error: record literal key order does not match"
      <+> "declared schema order (C3 invariant violated)."
      <+> "Literal keys:" <+> hsep (punctuate "," (map (pretty . fst) entries))
      <> "; declared keys:" <+> hsep (punctuate "," (map (pretty . fst) rs))

express :: AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) -> MorlocMonad PolyHead
express e@(AnnoS (Idx midx t) (Idx cidx _, _) _) = do
  -- Store the return effect labels for downstream metadata (per-manifold
  -- effect-label tracking in CppTranslator etc.); the labels are
  -- stripped from the type by 'insertExportBoundaries'.
  let retEffects = extractReturnEffects t
  MM.modify' (\s -> s { stateManifoldEffects = Map.insert midx retEffects (stateManifoldEffects s) })
  -- A command's arguments come from the program's caller as values; a
  -- helper's arrive from another manifold, a suspension among them as a
  -- closure. Only a command adapts its arguments.
  isCommand <- elem midx <$> MM.gets stateExports
  insertExportBoundaries isCommand cidx t <$> expressCore e
  where
    extractReturnEffects (FunT _ (EffectT effs _)) = effs
    extractReturnEffects (EffectT effs _) = effs
    extractReturnEffects _ = Set.empty

-- | Post-pass: walk every 'PolyHead' and wrap the body of any
-- 'PolyManifold' whose midx is configured @cache: true@ with a
-- 'PolyCacheBody'. The wrap fires once per call into the labeled
-- manifold -- regardless of whether the call originated from a
-- socket dispatch or from another manifold in the same pool -- so a
-- single insertion point covers both observability paths.
--
-- The cache decision lives entirely in 'stateManifoldConfig'; no
-- per-language code generator needs to re-check the YAML or thread
-- labels through its own pipeline. Centralising it here also makes
-- the dead-code analyser happy: the 'PolyCacheBody' constructor is
-- the only producer, and downstream passes only need a single new
-- case per IR transformation.
--
-- Run after 'express' and before 'segment' so the new node propagates
-- through monomorphisation and serialization alongside the existing
-- IR.
addCacheWraps :: PolyHead -> MorlocMonad PolyHead
addCacheWraps (PolyHead lang midx args body) = PolyHead lang midx args <$> cacheWrapExpr body

-- | Replace each closure that only forwards to a function value it captures
-- (@\\xs -> f xs@, the eta-abstraction a function value in a function slot
-- is given) by that function value. The wrapper carries no adaptation of its
-- own (one that did would have a different body), and keeping it hides the
-- value's identity: a function passed along a loop would gain one wrapper
-- per iteration, and a staged closure would lose its stage entry. A closure
-- passing fewer arguments than the function takes is a partial application
-- with an arity of its own (HOST-2), and stays.
etaReduceForwarders :: PolyHead -> PolyHead
etaReduceForwarders (PolyHead lang m args body) = PolyHead lang m args (go body)
  where
    go e = case mapPolySubExprs go e of
      PolyManifold _ _ (ManifoldPart [Arg j _] bound) k
        (PolyReturn (PolyApp (PolyExe t@(Idx _ (FunT ins _)) (LocalCallP j')) xs))
          | j == j'
          , k /= Preserved
          , length ins == length xs
          , map bndVarId xs == map (Just . ann) bound ->
              PolyBndVar (C t) j
      e' -> e'

bndVarId :: PolyExpr -> Maybe Int
bndVarId (PolyBndVar _ i) = Just i
bndVarId _ = Nothing

-- | Pass a fold's step and combine as the sourced function itself, instead
-- of the closure that only forwards to it, when it is one in the fold's own
-- language and the closure adapts nothing (COPY-7).
bareFoldSteps :: PolyHead -> PolyHead
bareFoldSteps (PolyHead lang m args body) = PolyHead lang m args (go lang body)
  where
    go l e = case e of
      PolyManifold l' mid f k b -> PolyManifold l' mid f k (go l' b)
      PolyRemoteInterface l' t ids form b -> PolyRemoteInterface l' t ids form (go l' b)
      PolyIntrinsic t intr [i, s, c, f]
        | intr `elem` [IntrFold, IntrFoldFile] -> PolyIntrinsic t intr [go l i, bare l s, bare l c, go l f]
      PolyIntrinsic t IntrFoldPut [s, h, x] -> PolyIntrinsic t IntrFoldPut [bare l s, go l h, go l x]
      _ -> mapPolySubExprs (go l) e
    bare l x = case x of
      PolyManifold l' _ (ManifoldPass bound) Transparent
        (PolyReturn (PolyApp exe@(PolyExe (Idx _ (FunT ins out)) (SrcCallP _ src)) xs))
          | l' == l
          , not (any (\t -> containsFunT t || containsEffectT t) (out : ins))
          , srcLang src == l
          , not (srcOperator src)
          , null (srcRsize src)
          , length ins == length xs
          , map bndVarId xs == map (Just . ann) bound
          , map Just ins == map val bound ->
              exe
      _ -> go l x

-- | Give every recursive manifold or shared specialization that a caller in
-- its own pool reaches a native entry point, and record it.
--
-- A recursive function (or a shared specialization) is lifted into a
-- manifold of its own, which is the pool's serial entry: it takes and
-- returns packets. A caller in another pool needs that, but a caller in the
-- same pool pays for it on every level of the recursion -- each level
-- writes its whole remaining argument to shared memory and reads it back,
-- so a walk down a spine of n nodes copies on the order of n^2 nodes. Tail
-- recursion escapes this by becoming a loop ('addLoopWraps'); a recursion
-- that combines the results of its calls cannot.
--
-- The manifold's body moves to a new ID and is marked as one that survives
-- lowering as a real function, which makes it a native function of the
-- pool: native arguments in, a native value out. The original ID keeps the
-- serial entry, now a wrapper around it, so a foreign caller and the
-- dispatch table are unaffected. Which callers use which entry is settled
-- during serialization, where a call's pool is finally known.
--
-- A manifold that carries a label or a cache is left alone: its hooks are
-- keyed to its ID, and moving its body would move them with it. A debug wrap
-- moves into the native entry, so every level of the recursion still runs
-- under it, named for the manifold it traces.
addNativeRecEntries :: [PolyHead] -> MorlocMonad [PolyHead]
addNativeRecEntries phs = do
  let targets = Set.fromList [mid | ph <- phs, mid <- recTargets ph]
  mapM (splitEntry targets) phs
  where
    -- Every manifold a back-edge in this head names. A back-edge marked as
    -- crossing may still land in its target's pool (segmentation can move
    -- a call), so the mark does not decide.
    recTargets :: PolyHead -> [Int]
    recTargets (PolyHead _ _ _ body) = go body
      where
        go (PolyExe _ (RecCallP mid _)) = [mid]
        go e = concatMap go (polySubExprs e)

    splitEntry :: Set.Set Int -> PolyHead -> MorlocMonad PolyHead
    splitEntry targets ph@(PolyHead lang midx args body) = do
      held <- allHeld lang args
      if Set.member midx targets && splittable body && (held || isLoop body || not (crosses body))
        then splitHead
        else return ph
      where
       splitHead = do
          -- A manifold index, not an expression index: the two counters are
          -- separate, and this also carries the source position over so the
          -- frame that runs the body is the one diagnostics name.
          midx' <- MM.freshManifoldIndex midx
          name <- MM.gets stateName
          MM.modify' $ \s ->
            s { stateNativeRecEntries = Map.insert midx midx' (stateNativeRecEntries s)
              , stateName = maybe (stateName s)
                  (\n -> Map.insert midx' n (stateName s)) (Map.lookup midx name)
              , stateNameSource = Map.insert midx' midx (stateNameSource s)
              }
          -- a body that is this manifold's own (same id) is what the native
          -- entry runs; left wrapped it would be emitted as a second
          -- function of the same name
          let native = case body of
                PolyManifold _ m _ _ inner | m == midx -> inner
                _ -> body
          return $ PolyHead lang midx args (PolyManifold lang midx' (ManifoldFull args) Preserved native)

    -- A value the pool cannot hold is carried as the packet it arrived in,
    -- which the native entry's callers, passing one value per argument,
    -- cannot supply; a body that crosses with one keeps the serial entry.
    -- A loop's native entry carries such a value as its packet.
    allHeld :: Lang -> [Arg None] -> MorlocMonad Bool
    allHeld lang as = do
      argTypes <- MM.gets stateArgTypes
      reg <- MM.gets stateLangRegistry
      let compiled = LR.registryIsCompiled reg (langName lang)
      and <$> mapM
        (\(Arg i _) -> case Map.lookup i argTypes of
            Just (Idx ti ty) -> canHoldType compiled lang ti ty
            Nothing -> return False)
        as

    crosses :: PolyExpr -> Bool
    crosses (PolyRemoteInterface {}) = True
    crosses e = any crosses (polySubExprs e)

    -- A body that is a tail loop, under structural wrappers.
    isLoop :: PolyExpr -> Bool
    isLoop (PolyLoop {}) = True
    isLoop (PolyManifold _ _ _ _ x) = isLoop x
    isLoop (PolyReturn x) = isLoop x
    isLoop _ = False

    -- A body whose shape already commits the manifold to something else: a
    -- label or a cache is keyed to the manifold's own ID and would move with
    -- the body. A body that reaches another pool is split like any other:
    -- its native entry takes values, and an argument wanted as a packet for
    -- the crossing is serialized where the body runs
    -- ('Morloc.CodeGenerator.Serialize').
    splittable :: PolyExpr -> Bool
    splittable (PolyCacheBody {}) = False
    splittable (PolyDebugWrap _ _ x) = splittable x
    splittable (PolyManifold _ _ _ Preserved _) = False
    splittable (PolyManifold _ _ _ _ x) = splittable x
    splittable (PolyReturn x) = splittable x
    splittable _ = True

-- | Whether a driver language can host a native tail-loop. Python/R lower via
-- 'genericMakeLoop'; C++ and Rust via the member's 'lcMakeLoop' (non-const
-- native locals reassigned in place). Single source of truth for the gate.
langSupportsNativeLoop :: Lang -> Bool
langSupportsNativeLoop lang = langName lang `elem` ["py", "r", "cpp", "rust"]

-- | Lower an eligible tail-recursive manifold to a native 'PolyLoop'. Runs after
-- 'express'/'addCacheWraps'/'addDebugWraps' and before 'insertEffectBoundaries'
-- and 'segment', so the loop node propagates through the rest of the pipeline.
-- Applies to any self-recursive 'PolyHead' -- an extracted helper OR an exported
-- directly-recursive function (both carry a @RecCallP@ back to their own midx).
--
-- Tail-ness is decided structurally at THIS stage rather than from a frontend
-- flag: 'rewriteLoopTail' turns every TAIL-position self back-edge into a
-- 'PolyLoopContinue', so any @RecCallP@ still remaining in @loopBody@ is a
-- non-tail back-edge. Eligibility (first cut): at least one self back-edge, none
-- remaining after the rewrite (all tail), not @--debug@ (recursion preserves the
-- nested trace), same-pool (@crossLang == Nothing@), a driver language that can
-- host a loop, and the single-guard canonical shape. Cross-pool loops and the
-- type-based crossing gates come later; anything ineligible falls back to the
-- existing round-trip recursion.
addLoopWraps :: PolyHead -> MorlocMonad PolyHead
addLoopWraps ph@(PolyHead lang midx args body) = do
  debugOn <- MM.gets stateDebugTrace
  let backEdges = collectRecCalls midx body
      loopBody = rewriteLoopTail midx body
  -- Ordered cheapest-first: the O(1) flag/language gates short-circuit before
  -- the body traversals ('backEdges'), which in turn precede forcing 'loopBody'
  -- (the 'rewriteLoopTail' rebuild) in the last two conjuncts.
  if not debugOn
       && langSupportsNativeLoop lang
       && not (null backEdges)
       && all (isNothing . fst) backEdges
       -- An effectful loop is only sound to lower when its result is consumed
       -- EAGERLY (the body's return is forced, a 'PolyEval'). A deferred/
       -- suspended <IO> result would have its base effect fire at loop-run time
       -- instead of force time; gate those to recursion. Pure loops are always ok.
       && (not (effectfulLoop backEdges) || returnEager body)
       -- Every self back-edge is in tail position: 'rewriteLoopTail' converted
       -- each tail one to a 'PolyLoopContinue', so a non-empty result here means
       -- a back-edge survived in a non-tail position -- fall back to recursion.
       && null (collectRecCalls midx loopBody)
       -- Well-formed loop body: a guard/let tree whose every continue is in a
       -- descendable tail position (see 'isLoopableBody'). Anything else (a
       -- continue stranded in a base leaf, or under a do-block) falls back to the
       -- existing round-trip recursion below.
       && isLoopableBody loopBody
    then do
      let ityp = snd (head backEdges)
          ids = map ann args
      -- Loud-fail two invariants the emitters (genericMakeLoop / lcMakeLoop /
      -- rustMakeLoop) silently assume, now that co-location routes more programs
      -- through this path. (1) Every continue carries exactly one value per
      -- loop-carried id; otherwise the emitters' `zipWith` over (ids, values)
      -- drops reassignments and the loop never advances its state. (2) The
      -- rewrite introduced no new free variable (beyond the ids the continue
      -- carries): dropping a recursion wrapper (rewriteLoopTail) must not strand
      -- a reference to one of that wrapper's own bound params.
      mapM_
        (\n ->
          if n == length ids
            then return ()
            else MM.throwSourcedError midx $
              "Compiler-internal error: loop continue carries" <+> pretty n <+>
              "values but the loop has" <+> pretty (length ids) <+> "carried variables")
        (loopContinueArities loopBody)
      let stranded = polyFreeVars loopBody
            `Set.difference` (polyFreeVars body `Set.union` Set.fromList ids)
      if Set.null stranded
        then return ()
        else MM.throwSourcedError midx $
          "Compiler-internal error: loop lowering stranded variable reference(s)"
          <+> list (map pretty (Set.toList stranded))
      MM.sayVVV $ "addLoopWraps: lowering midx" <+> pretty midx <+> "to a native loop over" <+> list (map pretty ids)
      return $ PolyHead lang midx args (PolyLoop ityp ids loopBody)
    else return ph
  where
    -- Every self back-edge in the body, as (crossLang, result type). The
    -- 'RecCallP' 'PolyExe' carries @Idx i (FunT inputs out)@; the loop's result
    -- type is @Idx i out@.
    -- The loop's result type (the recursion's return) carries an outer effect.
    effectfulLoop bs = case bs of
      ((_, Idx _ (EffectT _ _)) : _) -> True
      _ -> False

    -- Whether the manifold body's return position is eagerly forced (a
    -- 'PolyEval'), as opposed to suspended (a 'PolyDoBlock', i.e. deferred).
    returnEager (PolyManifold _ _ _ _ e) = returnEager e
    returnEager (PolyReturn e) = returnEager e
    returnEager (PolyLet _ _ e) = returnEager e
    returnEager (PolyEval _ _) = True
    returnEager _ = False

    collectRecCalls :: Int -> PolyExpr -> [(Maybe Lang, Indexed Type)]
    collectRecCalls mid = go
      where
        go (PolyExe (Idx i t) (RecCallP mid' cl))
          | mid' == mid = [(cl, Idx i (resultType t))]
        go e = concatMap go (polySubExprs e)
        resultType (FunT _ out) = out
        resultType t = t

    -- The arity (count of carried values) of every 'PolyLoopContinue' in a
    -- rewritten loop body, used by 'addLoopWraps' to check that each continue
    -- supplies exactly one value per loop-carried id.
    loopContinueArities :: PolyExpr -> [Int]
    loopContinueArities e =
      (case e of PolyLoopContinue xs -> [length xs]; _ -> [])
        ++ concatMap loopContinueArities (polySubExprs e)

    -- A well-formed loop body that 'serialExpr'\'s 'buildLoopBody' can extract:
    -- a decision tree of guards ('PolyIf') and lets over base leaves and
    -- 'PolyLoopContinue' leaves. The load-bearing check is that every
    -- 'PolyLoopContinue' sits in a position 'buildLoopBody' descends into (an
    -- 'if' branch, a let body, or under 'PolyReturn') -- NEVER buried in a base
    -- leaf, a guard condition, or a let RHS (which lower as plain values and
    -- would reject the continue). A continue stranded in a base (e.g. under a
    -- kept 'PolyManifold' wrapper) fails this and falls back to recursion. Guard
    -- conditions and let RHSs cannot themselves hold a continue ('rewriteLoopTail'
    -- never rewrites those positions) but are checked for safety.
    isLoopableBody :: PolyExpr -> Bool
    isLoopableBody = ok
      where
        ok (PolyIf c t e) = not (hasContinue c) && ok t && ok e
        ok (PolyLet _ rhs b) = not (hasContinue rhs) && ok b
        ok (PolyReturn e) = ok e
        -- A do-block is descended: an effectful continue path lowers its
        -- effects/binds as loop-body lets before the continue reassignment.
        ok (PolyDoBlock _ e) = ok e
        ok (PolyLoopContinue _) = True
        ok leaf = not (hasContinue leaf)

    hasContinue (PolyLoopContinue _) = True
    hasContinue e = any hasContinue (polySubExprs e)

    -- Rewrite the manifold's return position into a loop body: base leaves
    -- become 'PolyReturn'; each tail back-edge becomes a 'PolyLoopContinue'
    -- carrying the new loop-carried values, dropping the per-branch
    -- 'PolyManifold' wrapper so the continue is not stranded in a value
    -- position. Control/effect wrappers (If/Let/DoBlock) are preserved.
    rewriteLoopTail :: Int -> PolyExpr -> PolyExpr
    rewriteLoopTail mid loopBody = fst (goT loopBody)
      where
        -- 'goT' returns the rewritten expression paired with a flag: True when
        -- the rewrite produced a 'PolyLoopContinue' anywhere inside. The flag is
        -- threaded up so effect wrappers can decide whether to keep themselves
        -- without re-walking the subtree.
        goT :: PolyExpr -> (PolyExpr, Bool)
        goT (PolyReturn x) = goT x
        goT (PolyIf c t e) =
          let (t', ct) = goT t
              (e', ce) = goT e
           in (PolyIf c t' e', ct || ce)
        -- A do-block or sub-manifold wrapping a CONTINUE is the effect-thunk
        -- suspension of the recursive tail call: drop it so the continue is BARE
        -- (a wrapped continue carries an <IO> type that 'checkEffectBoundaries'
        -- rejects, and the continue is control flow, not a value). Its context
        -- args are the enclosing loop-carried vars, so the inlined body still
        -- references them by the same ids. Any per-iteration effect inside
        -- desugars to a 'PolyLet' RHS that survives. A wrapper around a BASE (its
        -- own effect thunk) has no continue -- keep it whole and force it
        -- downstream.
        goT (PolyDoBlock ti x) = dropIfContinue (PolyDoBlock ti) x
        -- A manifold carrying hooks (a label) is a call of its own: its
        -- back-edge stays a call, so the recursion is not made a loop.
        goT x@(PolyManifold _ _ _ Preserved _) = (x, False)
        goT (PolyManifold l m form k inner) = dropIfContinue (PolyManifold l m form k) inner
        -- A 'PolyEval' at the return position forces the (effectful) result; it
        -- is tail-transparent for loop purposes -- descend through it so an <IO>
        -- tail recursion under a force becomes a continue. Base leaves are
        -- re-forced by 'insertEffectBoundaries' (forceReturnPosition over the
        -- loop) so dropping the eval here does not lose the base's force.
        goT (PolyEval _ x) = goT x
        goT (PolyLet i a b) = let (b', cb) = goT b in (PolyLet i a b', cb)
        goT (PolyApp (PolyExe _ (RecCallP mid' _)) xs) | mid' == mid = (PolyLoopContinue xs, True)
        goT (PolyExe _ (RecCallP mid' _)) | mid' == mid = (PolyLoopContinue [], True)
        goT leaf = (PolyReturn leaf, False)

        -- A wrapper whose inner has no continue is a base leaf, kept whole.
        dropIfContinue rebuild inner =
          let (inner', c) = goT inner
           in if c then (inner', True) else (PolyReturn (rebuild inner), False)

cacheWrapExpr :: PolyExpr -> MorlocMonad PolyExpr
cacheWrapExpr (PolyManifold l m form k inner) = do
  inner' <- cacheWrapExpr inner
  mLbl <- cacheLabelOfMidx m
  let argsList = collectArgs form
  return $ case mLbl of
    Just lbl ->
      PolyManifold l m form k (PolyCacheBody lbl m argsList inner')
    Nothing -> PolyManifold l m form k inner'
cacheWrapExpr (PolyApp f args) =
  PolyApp <$> cacheWrapExpr f <*> mapM cacheWrapExpr args
cacheWrapExpr (PolyPat g app) = PolyPat g <$> mapM cacheWrapExpr app
cacheWrapExpr (PolyCacheBody lbl m args body) =
  PolyCacheBody lbl m args <$> cacheWrapExpr body
cacheWrapExpr (PolyLet i e1 e2) =
  PolyLet i <$> cacheWrapExpr e1 <*> cacheWrapExpr e2
cacheWrapExpr (PolyReturn x) = PolyReturn <$> cacheWrapExpr x
cacheWrapExpr (PolyIf c t' e) =
  PolyIf <$> cacheWrapExpr c <*> cacheWrapExpr t' <*> cacheWrapExpr e
cacheWrapExpr (PolyDoBlock ti x) = PolyDoBlock ti <$> cacheWrapExpr x
cacheWrapExpr (PolyEval ti x) = PolyEval ti <$> cacheWrapExpr x
cacheWrapExpr (PolyCoerce c ti x) = PolyCoerce c ti <$> cacheWrapExpr x
cacheWrapExpr (PolyList v ts xs) = PolyList v ts <$> mapM cacheWrapExpr xs
cacheWrapExpr (PolyTuple v xs) =
  PolyTuple v <$> mapM (\(t, x) -> (,) t <$> cacheWrapExpr x) xs
cacheWrapExpr (PolyRecord o v ps rs) =
  PolyRecord o v ps <$>
    mapM (\(k, (t, x)) -> (,) k . (,) t <$> cacheWrapExpr x) rs
cacheWrapExpr (PolyIntrinsic ti intr xs) =
  PolyIntrinsic ti intr <$> mapM cacheWrapExpr xs
cacheWrapExpr (PolyRemoteInterface l ti is rf inner) =
  PolyRemoteInterface l ti is rf <$> cacheWrapExpr inner
cacheWrapExpr leaf = return leaf

-- | Walk a 'PolyHead' and wrap every 'PolyManifold' body in a
-- 'PolyDebugWrap'. Gated entirely on 'stateDebugTrace' so a build
-- without @morloc make --debug@ pays nothing. The wrap lowers to a
-- per-language try/catch + dump-args-on-throw at codegen.
--
-- Pure-runtime manifolds (whose body never reaches a foreign call;
-- 'ManifoldKind' = 'ManifoldGenericPure') are left unwrapped because
-- their failures go through the runtime's own error path, not the
-- foreign-exception scaffold.
addDebugWraps :: PolyHead -> MorlocMonad PolyHead
addDebugWraps (PolyHead lang midx args body) = do
  enabled <- MM.gets stateDebugTrace
  if not enabled
    then return $ PolyHead lang midx args body
    else do
      body' <- debugWrapExpr body
      -- Skip the head-level wrap only when the body already wraps the
      -- SAME midx (point-free fusion of the head's own manifold). Wraps
      -- at a different midx inside the body migrate to a different
      -- pool during segmentation -- if we suppressed the head wrap on
      -- that basis, the calling pool would have no wrap to catch the
      -- foreign-call exception. Args the typemap can't resolve are
      -- silently dropped in 'lowerDebugWrap' (best-effort by design).
      let body'' = if hasDebugWrapAt midx body' || returnsFunction body'
                    then body'
                    else PolyDebugWrap midx args body'
      return $ PolyHead lang midx args body''

-- | Detect a 'PolyDebugWrap' at 'midx' already present in the body so
-- the head-level wrap doesn't double up when 'debugWrapExpr' wrapped
-- an enclosing 'PolyManifold' of the same midx.
hasDebugWrapAt :: Int -> PolyExpr -> Bool
hasDebugWrapAt m (PolyDebugWrap m' _ _) | m == m' = True
hasDebugWrapAt m (PolyDebugWrap _ _ inner) = hasDebugWrapAt m inner
hasDebugWrapAt m (PolyManifold _ _ _ _ inner) = hasDebugWrapAt m inner
hasDebugWrapAt m (PolyCacheBody _ _ _ inner) = hasDebugWrapAt m inner
hasDebugWrapAt m (PolyReturn e) = hasDebugWrapAt m e
hasDebugWrapAt m (PolyLet _ e1 e2) = hasDebugWrapAt m e1 || hasDebugWrapAt m e2
hasDebugWrapAt m (PolyApp h xs) = hasDebugWrapAt m h || any (hasDebugWrapAt m) xs
hasDebugWrapAt m (PolyPat _ app) = any (hasDebugWrapAt m) app
hasDebugWrapAt m (PolyDoBlock _ e) = hasDebugWrapAt m e
hasDebugWrapAt m (PolyEval _ e) = hasDebugWrapAt m e
hasDebugWrapAt m (PolyCoerce _ _ e) = hasDebugWrapAt m e
hasDebugWrapAt m (PolyIf a b c) = hasDebugWrapAt m a || hasDebugWrapAt m b || hasDebugWrapAt m c
-- Kept symmetric with 'debugWrapExpr', which recurses into each of these.
hasDebugWrapAt m (PolyList _ _ xs) = any (hasDebugWrapAt m) xs
hasDebugWrapAt m (PolyTuple _ xs) = any (hasDebugWrapAt m . snd) xs
hasDebugWrapAt m (PolyRecord _ _ _ rs) = any (hasDebugWrapAt m . snd . snd) rs
hasDebugWrapAt m (PolyIntrinsic _ _ xs) = any (hasDebugWrapAt m) xs
hasDebugWrapAt m (PolyRemoteInterface _ _ _ _ inner) = hasDebugWrapAt m inner
hasDebugWrapAt _ _ = False

-- | A wrap passes its body's value through the wire form, which a function
-- made by host code does not have (HOST-9), so a body returning a function
-- is left unwrapped; tracing is best-effort.
returnsFunction :: PolyExpr -> Bool
returnsFunction e = case polyOuterType e of
  Just t -> holds t
  Nothing -> False
  where
    holds FunT {} = True
    holds (EffectT _ t) = holds t
    holds t = containsFunT t

debugWrapExpr :: PolyExpr -> MorlocMonad PolyExpr
debugWrapExpr (PolyManifold l m form k inner)
  | returnsFunction inner = PolyManifold l m form k <$> debugWrapExpr inner
debugWrapExpr (PolyManifold l m form k inner) = do
  inner' <- debugWrapExpr inner
  case sourcedCallee inner of
    Just v -> MM.modify' (\s -> s {stateDebugCallee = Map.insert m v (stateDebugCallee s)})
    Nothing -> return ()
  let argsList = collectArgs form
  return $ PolyManifold l m form k (PolyDebugWrap m argsList inner')
debugWrapExpr (PolyApp head' args') =
  PolyApp <$> debugWrapExpr head' <*> mapM debugWrapExpr args'
debugWrapExpr (PolyPat g app) = PolyPat g <$> mapM debugWrapExpr app
debugWrapExpr (PolyCacheBody lbl m args' body) =
  PolyCacheBody lbl m args' <$> debugWrapExpr body
debugWrapExpr (PolyDebugWrap m args' body) =
  PolyDebugWrap m args' <$> debugWrapExpr body
debugWrapExpr (PolyLet i e1 e2) =
  PolyLet i <$> debugWrapExpr e1 <*> debugWrapExpr e2
debugWrapExpr (PolyReturn x) = PolyReturn <$> debugWrapExpr x
debugWrapExpr (PolyIf c t' e) =
  PolyIf <$> debugWrapExpr c <*> debugWrapExpr t' <*> debugWrapExpr e
debugWrapExpr (PolyDoBlock ti x) = PolyDoBlock ti <$> debugWrapExpr x
debugWrapExpr (PolyEval ti x) = PolyEval ti <$> debugWrapExpr x
debugWrapExpr (PolyCoerce c ti x) = PolyCoerce c ti <$> debugWrapExpr x
debugWrapExpr (PolyList v ts xs) = PolyList v ts <$> mapM debugWrapExpr xs
debugWrapExpr (PolyTuple v xs) =
  PolyTuple v <$> mapM (\(t, x) -> (,) t <$> debugWrapExpr x) xs
debugWrapExpr (PolyRecord o v ps rs) =
  PolyRecord o v ps <$>
    mapM (\(k, (t, x)) -> (,) k . (,) t <$> debugWrapExpr x) rs
debugWrapExpr (PolyIntrinsic ti intr xs) =
  PolyIntrinsic ti intr <$> mapM debugWrapExpr xs
debugWrapExpr (PolyRemoteInterface l ti is rf inner) =
  PolyRemoteInterface l ti is rf <$> debugWrapExpr inner
debugWrapExpr leaf = return leaf

-- | The user-written sourced function a manifold body ends in a call of,
-- looking through the returns, bindings and wrappers around that call.
sourcedCallee :: PolyExpr -> Maybe EVar
sourcedCallee (PolyReturn e) = sourcedCallee e
sourcedCallee (PolyLet _ _ e) = sourcedCallee e
sourcedCallee (PolyDoBlock _ e) = sourcedCallee e
sourcedCallee (PolyEval _ e) = sourcedCallee e
sourcedCallee (PolyCoerce _ _ e) = sourcedCallee e
sourcedCallee (PolyDebugWrap _ _ e) = sourcedCallee e
sourcedCallee (PolyCacheBody _ _ _ e) = sourcedCallee e
sourcedCallee (PolyApp (PolyExe _ (SrcCallP (Written _) src)) _) = Just (srcAlias src)
sourcedCallee (PolyExe _ (SrcCallP (Written _) src)) = Just (srcAlias src)
sourcedCallee _ = Nothing

-- | Look up @midx@ in 'stateManifoldConfig'; return its label group
-- when the manifold has @cache: true@ AND a label, otherwise
-- 'Nothing'. Everything cache-related lives in this map; nothing
-- else needs to know about it.
cacheLabelOfMidx :: Int -> MorlocMonad (Maybe Text)
cacheLabelOfMidx midx = do
  mcfg <- MM.gets (Map.lookup midx . stateManifoldConfig)
  return $ case mcfg of
    Just cfg
      | manifoldConfigCache cfg == Just True
      , Just lbl <- manifoldConfigLabel cfg ->
          Just lbl
    _ -> Nothing

-- | Extract the manifold's bound args as a flat @[Arg None]@ list so
-- the cache wrap can reference them by name at codegen.
collectArgs :: ManifoldForm None (Maybe Type) -> [Arg None]
collectArgs = abilist (\i _ -> Arg i None) (\i _ -> Arg i None)

-- | HOST-3 at a call, for a source whose signature 'Link.checkRsize' could
-- not evaluate where it is declared.
checkRsizeArity :: Int -> Source -> Int -> MorlocMonad ()
checkRsizeArity idx src arity = case srcRsize src of
  [] -> return ()
  ns
    | sum ns < arity -> return ()
    | otherwise ->
        MM.throwSourcedError idx $
          "The rsize declaration on" <+> squotes (pretty (unEVar (srcAlias src)))
            <+> "declares groups of" <+> hsep (punctuate comma (map pretty ns))
            <+> "arguments, but a call of it takes" <+> pretty arity
            <> "; the declared groups must leave at least one argument for the last group"

-- | Pair two lists the program's types make equally long, such as a call's
-- arguments and its function's inputs. A difference is a compiler defect,
-- reported at the expression.
zipExact :: HasCallStack => Int -> [a] -> [b] -> MorlocMonad [(a, b)]
zipExact i xs ys = case Zip.zipExact xs ys of
  Just pairs -> return pairs
  Nothing ->
    withFrozenCallStack
      (MM.throwCompilerBugAt i ("paired lists differ in length (" <> pretty (length xs) <> " and " <> pretty (length ys) <> ")"))

-- | An element of a literal, expressed at its type and returned with it.
expressTyped :: Lang -> (AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]), Type) -> MorlocMonad (Indexed Type, PolyExpr)
expressTyped lang (x, t) = let it = mkIdx x t in (,) it <$> expressPolyExprWrap lang it x

-- | A parameter with its type.
typedArg :: (Arg a, Type) -> Arg (Maybe Type)
typedArg (Arg i _, t) = Arg i (Just t)

-- | A call's arguments, each with its input type. A partial application
-- gives fewer arguments than the function has inputs; more is a compiler
-- defect, since over-application is resolved before this stage.
zipArgs :: HasCallStack => Int -> [a] -> [b] -> MorlocMonad [(a, b)]
zipArgs i xs ts
  | length xs <= length ts = return (zipPrefix xs ts)
  | otherwise =
      withFrozenCallStack
        (MM.throwCompilerBugAt i ("a call has" <+> pretty (length xs) <+> "arguments for" <+> pretty (length ts) <+> "inputs"))

expressCore :: AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) -> MorlocMonad PolyHead
expressCore (AnnoS (Idx midx c@(FunT inputs _)) (Idx cidx lang, _) (ExeS exe)) = do
  ids <- MM.takeFromCounter (length inputs)
  exe' <- case exe of
    (SrcCall (Idx si src)) -> checkRsizeArity midx src (length inputs) >> return (SrcCallP (Written si) src)
  let lambdaVals = zipWithSame (PolyBndVar . C . Idx cidx) inputs ids
  return
    . PolyHead lang midx [Arg i None | i <- ids]
    . PolyReturn
    $ PolyApp (PolyExe (Idx midx c) exe') lambdaVals
-- Point-free export aliasing a recursive function. extractRecursiveHelpers
-- has pulled the helper into its own manifold and left a bare CallS back-edge
-- at the export root. Eta-expand here so the PolyHead carries the function's
-- arity; otherwise the export manifold would be emitted with zero args while
-- the nexus manifest correctly records the type-driven arity.
expressCore (AnnoS (Idx midx c@(FunT inputs out)) (Idx cidx lang, _) (CallS v)) = do
  (mid, crossLang) <- lookupRecursiveTarget lang v
  ids <- MM.takeFromCounter (length inputs)
  let lambdaVals = zipWithSame (PolyBndVar . C . Idx cidx) inputs ids
      headArgs = [Arg i None | i <- ids]
  case out of
    EffectT effs innerOut ->
      return
        . PolyHead lang midx headArgs
        . PolyReturn
        . PolyDoBlock (Idx cidx (EffectT effs innerOut))
        $ PolyApp (PolyExe (Idx midx (FunT inputs innerOut)) (RecCallP mid crossLang)) lambdaVals
    _ ->
      return
        . PolyHead lang midx headArgs
        . PolyReturn
        $ PolyApp (PolyExe (Idx midx c) (RecCallP mid crossLang)) lambdaVals
expressCore (AnnoS (Idx midx _) (_, lambdaArgs) (LamS _ (AnnoS (Idx _ applicationType) (c, _) x))) =
  expressCore (AnnoS (Idx midx applicationType) (c, lambdaArgs) x)
expressCore (AnnoS (Idx midx (AppT (VarT v) ts)) (Idx cidx lang, args) (LstS xs))
  | [t] <- tyArgs ts = do
      xs' <- mapM (\x -> expressPolyExprWrap lang (mkIdx x t) x) xs
      -- Preserve the FULL applied type args (kind positions included)
      -- so downstream language code generators can see phantom dims at
      -- the SerialList layer. Macro substitution filters kind args.
      let x = PolyList (Idx cidx v) (map (Idx cidx) ts) xs'
      return $ PolyHead lang midx [Arg i None | Arg i _ <- args] (PolyReturn x)
-- LstS at a type whose head is a type-alias (e.g. @type Pat = [Pat]@
-- used as a manifold return type). Reduce one alias step and retry.
-- Same shape as the NamS reduce-and-retry below.
expressCore (AnnoS (Idx midx t) (Idx cidx lang, args) (LstS xs)) = do
  mayT <- evalGeneralStep (type2typeu t)
  case mayT of
    (Just t') -> expressCore (AnnoS (Idx midx (typeOf t')) (Idx cidx lang, args) (LstS xs))
    Nothing -> MM.throwSourcedError midx $ "Invalid list form: " <> pretty t
-- TupS literal where the head IS the TupleN constructor: the type args
-- are the slot types, so zip directly. For any other @AppT (VarT v) ts@
-- (user alias whose args don't correspond to slot types -- e.g. the
-- @NatLit n@ in a phantom-Nat tuple alias like
-- @type FixedPair (n :: Nat) a = (a, a)@), fall through to the
-- reduce-and-retry below so the alias body's slot types drive
-- element-wise expression. See the parallel guard in Typecheck.hs C2.
expressCore (AnnoS (Idx midx (AppT (VarT v) (tyArgs -> ts))) (Idx cidx lang, args) (TupS xs))
  | v == BT.tuple (length xs)
  , length ts == length xs = do
      xs' <- mapM (expressTyped lang) (zipSame xs ts)
      let x = PolyTuple (Idx cidx v) xs'
      return $ PolyHead lang midx [Arg i None | Arg i _ <- args] (PolyReturn x)
-- TupS at a non-tuple head (user alias, including phantom-Nat aliases,
-- and bare @VarT WrappedLL@ for @type WrappedLL = (Int, LL)@). Reduce
-- one alias step and retry; same shape as the NamS reduce-and-retry.
expressCore (AnnoS (Idx midx t) (Idx cidx lang, args) (TupS xs)) = do
  mayT <- evalGeneralStep (type2typeu t)
  case mayT of
    (Just t') -> expressCore (AnnoS (Idx midx (typeOf t')) (Idx cidx lang, args) (TupS xs))
    Nothing -> MM.throwSourcedError midx $ "Invalid tuple form: " <> pretty t
expressCore (AnnoS (Idx midx (NamT o v ps rs)) (Idx cidx lang, args) (NamS entries)) = do
  -- C3 invariant: the literal's key order must equal the declared
  -- schema's key order. Typecheck.hs reorders NamS against the
  -- declared NamU; if that ever regresses, this assertion catches the
  -- mis-zip at the point where it would corrupt the wire format.
  assertRecordKeyOrder midx entries rs
  xs' <- mapM (\((_, x), (k, t)) -> (,) k <$> expressTyped lang (x, t)) (zipSame entries rs)
  let x = PolyRecord o (Idx cidx v) (map (Idx cidx) ps) xs'
  return $ PolyHead lang midx [Arg i None | Arg i _ <- args] (PolyReturn x)
expressCore (AnnoS (Idx midx t) (Idx cidx lang, args) (NamS entries)) = do
  mayT <- evalGeneralStep (type2typeu t)
  case mayT of
    (Just t') -> expressCore (AnnoS (Idx midx (typeOf t')) (Idx cidx lang, args) (NamS entries))
    Nothing -> MM.throwSourcedError midx $ "Missing concrete:" <+> "t=" <> pretty t
expressCore e = expressDefault e

reduceType :: Scope -> Type -> Maybe Type
reduceType scope t0 =
  let tu0 = type2typeu t0
   in case TE.evaluateStep scope tu0 of
        (Just tu1) -> if tu0 == tu1 then Nothing else Just (typeOf tu1)
        Nothing -> Nothing

expressDefault :: AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) -> MorlocMonad PolyHead
expressDefault e0@(AnnoS (Idx midx t) (Idx cidx lang, args) _) =
  PolyHead lang midx [Arg i None | Arg i _ <- args] . ensurePolyReturn
    <$> expressPolyExprWrap lang (Idx cidx t) e0
  where
    -- ensure the manifold body has PolyReturn at the return position
    ensurePolyReturn (PolyReturn x) = PolyReturn x
    ensurePolyReturn (PolyLet i e1 e2) = PolyLet i e1 (ensurePolyReturn e2)
    -- this manifold's own body returns; another manifold at the return
    -- position (a call, or a closure) is the value returned
    ensurePolyReturn (PolyManifold l m f k e)
      | m == midx && not (isClosureForm f) = PolyManifold l m f k (ensurePolyReturn e)
    ensurePolyReturn x = PolyReturn x

expressPolyExprWrap ::
  Lang ->
  Indexed Type ->
  AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) ->
  MorlocMonad PolyExpr
expressPolyExprWrap l t e = do
  reg <- MM.gets stateLangRegistry
  bconf <- MM.gets stateBuildConfig
  expressPolyExpr (decideRemoteness reg bconf Nothing) l t e

-- A call between two languages is a remote (socket) call unless they are
-- co-located members of one pool (e.g. cpp and its guest futhark), in which
-- case it stays an in-process call. For non-member languages 'coLocated'
-- reduces to '==', so this is unchanged for cpp/py/r.
decideRemoteness :: LR.LangRegistry -> BuildConfig -> Maybe ManifoldConfig -> Lang -> Lang -> Maybe RemoteForm
decideRemoteness reg _ Nothing l1 l2
  | LR.coLocated reg l1 l2 = Nothing
  | otherwise = Just ForeignCall
decideRemoteness reg bconf (Just mconfig) l1 l2 = case manifoldConfigRemote mconfig of
  Nothing
    | LR.coLocated reg l1 l2 -> Nothing
    | otherwise -> Just ForeignCall
  Just res -> case (buildConfigSlurmSupport bconf, not (LR.coLocated reg l1 l2)) of
    (Just True, _) -> Just $ RemoteCall res
    (_, True) -> Just $ ForeignCall
    _ -> Nothing

-- Express a function argument. A do-block passed where a suspension
-- parameter is expected is suspended whole -- identically to a bare
-- effectful application argument -- so the callee receives the
-- suspension. The EffectT stays on the inner expression so its source
-- call is suspended by 'EffectBoundary'; a return-position do-block is
-- run by the boundary pass.
expressPolyArg ::
  Lang ->
  Indexed Type ->
  AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) ->
  MorlocMonad PolyExpr
expressPolyArg parentLang pc@(Idx _ (EffectT _ _)) (AnnoS (Idx midx t@(EffectT _ innerT)) (Idx cidx lang, args) (DoBlockS x)) = do
  -- the block's body has the unwrapped type, as in 'expressPolyExpr'
  x' <- expressPolyExprWrap lang (mkIdx x innerT) x
  let e = PolyDoBlock (Idx cidx t) x'
  expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e
expressPolyArg l pc e = expressPolyExprWrap l pc e

-- | The wire type for every slot the slicer / indexer expects: bounds
-- always arrive as @?Int64@ (the return shape of @__to_index__@); a
-- bare @Int64@ never reaches the runtime.
optI64T :: Type
optI64T = OptionalT (VarT BT.i64)

-- | Rewrite an applied @PatternBracketSlice@ into the resolved
-- per-language @Sliceable.__get_slice__@ instance call, with each
-- bound wrapped in its own @IndexLike.__to_index__@ cast. Used by
-- 'expressPolyExpr' on the local-pool path.
expressBracketSlice
  :: Lang
  -> Int                       -- outer manifold index, reused for the synthetic SrcCall PolyExe
  -> PatEvidence
  -> Type                      -- bracket pattern result type
  -> (Type, PolyExpr)          -- start
  -> (Type, PolyExpr)          -- stop
  -> (Type, PolyExpr)          -- step
  -> (Type, PolyExpr)          -- receiver
  -> MorlocMonad PolyExpr
expressBracketSlice callLang midx pev out s e p (rcvT, rcvE) = do
  -- IFile receivers route to the runtime walker (IntrIFileWalk).
  isIFile <- typeHeadIs BT.ifileVar rcvT
  if isIFile
    then emitIFileWalk callLang midx pev out bracketSliceSteps rcvE [s, e, p]
    else case patReceiver pev of
      Just ev
        | Just _ <- FI.evidenceMethod getterMethods ev ->
            emitPatternAccessibleCall callLang midx out ev pev bracketSliceSteps rcvT [s, e, p] rcvE
        | Just m <- FI.evidenceMethod sliceMethods ev -> do
            src <- evidenceSource midx callLang ev m
            bounds <- wrapBounds callLang midx pev [s, e, p]
            let fT = FunT [optI64T, optI64T, optI64T, rcvT] out
            return $ PolyApp (PolyExe (Idx midx fT) (builtSrcCall src)) (bounds <> [rcvE])
      _ -> noInstance midx callLang "Sliceable.__get_slice__ or SliceableDim.__get_slice_dim__" rcvT

-- | Rewrite an applied @PatternBracketIndex@ into the resolved
-- per-language @Indexable.__access_index__@ instance call, with the
-- index wrapped in @IndexLike.__to_index__@. @__access_index__@ takes
-- @?Int64@ to match @__to_index__@'s return shape; the source
-- implementations error on a Null index since an absent index has no
-- semantic meaning at an access position (only at a slice position).
expressBracketIndex
  :: Lang
  -> Int
  -> PatEvidence
  -> Type
  -> (Type, PolyExpr)          -- index
  -> (Type, PolyExpr)          -- receiver
  -> MorlocMonad PolyExpr
expressBracketIndex callLang midx pev out i (rcvT, rcvE) = do
  -- IFile receivers route to the runtime walker (IntrIFileWalk), which
  -- materializes the element and checks bounds.
  isIFile <- typeHeadIs BT.ifileVar rcvT
  if isIFile
    then emitIFileWalk callLang midx pev out bracketIndexSteps rcvE [i]
    else case patReceiver pev of
      Just ev
        | Just _ <- FI.evidenceMethod getterMethods ev ->
            emitPatternAccessibleCall callLang midx out ev pev bracketIndexSteps rcvT [i] rcvE
        | Just m <- FI.evidenceMethod indexMethods ev -> do
            src <- evidenceSource midx callLang ev m
            bounds <- wrapBounds callLang midx pev [i]
            let fT = FunT [optI64T, rcvT] out
            return $ PolyApp (PolyExe (Idx midx fT) (builtSrcCall src)) (bounds <> [rcvE])
      _ -> noInstance midx callLang "Indexable.__access_index__" rcvT

-- | Walk the alias chain and check whether the receiver type's head
-- TVar matches `target`. Mirrors the alias-walking strategy of
-- instance resolution so a user-defined `type FastaFile = IFile
-- Sequence` is detected as IFile-headed.
typeHeadIs :: TVar -> Type -> MorlocMonad Bool
typeHeadIs target originalType = do
  scope <- MM.getGeneralScope
  return $ any ((== target) . extractKey) (TE.aliasChain scope (type2typeu originalType))

-- | Emit a unified @IntrIFileWalk@ call for any pattern application
-- on an IFile receiver. The walk-step chain (struct fields followed
-- by an optional terminal bracket op) is encoded as a path string
-- consumed by the runtime walker. Runtime args correspond to bracket
-- bounds in DFS order.
emitIFileWalk
  :: Lang
  -> Int                       -- midx
  -> PatEvidence               -- the bounds' IndexLike evidence
  -> Type                      -- result type (the value the walker materializes)
  -> [WalkStep]                -- the static walk chain
  -> PolyExpr                  -- the IFile-typed handle
  -> [(Type, PolyExpr)]        -- bracket bounds, in DFS order, at their own types
  -> MorlocMonad PolyExpr
emitIFileWalk callLang midx pev out steps rcvE bounds = do
  -- every walker takes its bounds as optional Int64s
  runtimeArgs <- wrapBounds callLang midx pev bounds
  let pathText = walkStepsToPath steps
  let strType = VarT BT.str
  pathPoly <- dispatchPrimLit midx callLang strType BT.str
                              (\tv -> PolyStr (Idx midx tv) pathText)
  return $ PolyIntrinsic (Idx midx out) IntrIFileWalk
    (pathPoly : rcvE : runtimeArgs)

-- | Emit a call to a user-declared @PatternAccessible@ instance's
-- @__extract_pattern__@ method for a non-IFile receiver.
--
-- Class signature at the call site:
--
-- @
--   __extract_pattern__ :: PatternChain (w a) b -> [?Int64] -> w a -> b
-- @
--
-- The path string is the same canonical form the runtime walker
-- consumes (@.foo@, @.[]@, @.[:]@, @.(a;b)@); @PatternChain@ is a
-- newtype over @Str@, so at the wire level the first arg is a plain
-- string. Each bracket bound is wrapped in @IndexLike.__to_index__@
-- so the runtime args list is a homogeneous @[?Int64]@; the class
-- method receives the whole list rather than variadic bounds, which
-- lets the user's implementation walk the pattern with a single
-- fixed-arity call.
emitPatternAccessibleCall
  :: Lang
  -> Int                       -- midx
  -> Type                      -- result type (the value the user's function produces)
  -> Evidence                  -- the receiver's PatternAccessible instance
  -> PatEvidence
  -> [WalkStep]                -- static walk chain
  -> Type                      -- receiver type
  -> [(Type, PolyExpr)]        -- bracket bounds in DFS order: (type, expression)
  -> PolyExpr                  -- receiver expression
  -> MorlocMonad PolyExpr
emitPatternAccessibleCall callLang midx out ev pev steps rcvT bounds rcvE = do
  src <- evidenceSource midx callLang ev (EV BT.extractPatternMethod)
  let pathText = walkStepsToPath steps
  pathPoly <- dispatchPrimLit midx callLang (VarT BT.str) BT.str
                              (\tv -> PolyStr (Idx midx tv) pathText)
  wrappedBounds <- wrapBounds callLang midx pev bounds
  let argsElemT    = optI64T
      argsListPoly = PolyList (Idx midx BT.list) [Idx midx (TyA argsElemT)] wrappedBounds
      argsListT    = AppT (VarT BT.list) [TyA argsElemT]
      -- PatternChain is a newtype over Str; downstream alias
      -- resolution collapses `PatternChain (rcvT) out` to Str at
      -- the wire level, matching the PolyStr argument.
      patternChainT = AppT (VarT BT.patternChainVar) [TyA rcvT, TyA out]
      fT           = FunT [patternChainT, argsListT, rcvT] out
  return $ PolyApp (PolyExe (Idx midx fT) (builtSrcCall src))
                   [pathPoly, argsListPoly, rcvE]

-- | A getter on an IFile routes to the walker, and on a receiver with a
-- 'PatternAccessible' instance to that instance, whatever the selector;
-- otherwise it is read structurally. A setter always is: both of those read a
-- value out of a receiver, and a setter rebuilds its receiver.
--
-- Returns an UNWRAPPED 'PolyExpr'; callers wrap in 'PolyReturn' when the
-- surrounding context needs it.
dispatchPatCall
  :: Lang
  -> Int                       -- midx
  -> Indexed Type              -- the pattern as a function of its arguments
  -> PatEvidence
  -> Type                      -- its result
  -> PatApp (Type, PolyExpr)   -- the arguments, each at its input type
  -> MorlocMonad PolyExpr
dispatchPatCall callLang midx g pev out app = case app of
  PatSlice s e p rcv -> expressBracketSlice callLang midx pev out s e p rcv
  PatIndex i rcv -> expressBracketIndex callLang midx pev out i rcv
  PatGet sel bounds (rcvT, rcvE) -> do
    steps <- selectorToWalkSteps midx sel
    isIFile <- typeHeadIs BT.ifileVar rcvT
    if isIFile
      then emitIFileWalk callLang midx pev out steps rcvE bounds
      else case patReceiver pev of
        Just ev | Just _ <- FI.evidenceMethod getterMethods ev ->
          emitPatternAccessibleCall callLang midx out ev pev steps rcvT bounds rcvE
        _ -> structural
  _ -> structural
  where
    structural = return (PolyPat g (fmap snd app))

-- | A pattern's arguments, each with its input type.
patWithInputs :: Int -> PatApp a -> [Type] -> MorlocMonad (PatApp (a, Type))
patWithInputs i app ts = do
  pairs <- zipArgs i (toList app) ts
  case applyPattern (patAppPattern app) pairs of
    Just typed -> return typed
    Nothing -> MM.throwCompilerBugAt i "a pattern's arguments do not fit it"

-- | The source of a method in the node's language, from the instance the
-- evidence selected. An instance with none there is rejected rather than
-- replaced by another (REAL-3, CLS-16).
evidenceSource :: Int -> Lang -> Evidence -> EVar -> MorlocMonad (Indexed Source)
evidenceSource i lang ev m = do
  env <- MM.gets stateInstances
  case FI.methodSourcesIn env ev m lang of
    [src] -> return src
    many@(_ : _) ->
      MM.throwSourcedError i $
        "The instance selected here has several" <+> pretty lang <+> "sources of" <+> squotes (pretty m) <> ":"
          <+> hsep (map (\(Idx _ s) -> squotes (pretty (srcName s))) many)
          <> ". Import fewer modules or remove one"
    [] -> MM.throwSourcedError i $ case ev of
      EvInstance key _ _
        | FI.methodHasBody env ev m ->
            "The instance" <+> prettyConstraint (FI.keyConstraint key) <+> "selected here implements"
              <+> squotes (pretty m) <+> "by a morloc body, which a pattern cannot call. Give it a source in"
              <+> pretty lang
        | otherwise ->
            "The instance" <+> prettyConstraint (FI.keyConstraint key) <+> "selected here gives no"
              <+> pretty lang <+> "implementation of" <+> squotes (pretty m)
      _ -> "No" <+> pretty lang <+> "implementation of" <+> squotes (pretty m)

-- | What a pool cannot call: nothing in the program serves this type.
noInstance :: Int -> Lang -> Text -> Type -> MorlocMonad a
noInstance i lang methodLabel t = do
  requireDetermined i t
  MM.throwSourcedError i $
    "No" <+> pretty methodLabel <+> "instance found for"
      <+> pretty t <+> "in language" <+> pretty lang
      <> line <> missingClassHint methodLabel

-- | A one-line hint appended to typeclass-missing errors. Bracket
-- patterns and the implicit-map intrinsic only exist if the user has
-- a module that brings their backing typeclasses into scope --
-- @IndexLike@ / @Indexable@ / @Sliceable@ / @SliceableDim@ live in
-- @internal@; the stdlib @Functor@ lives in @root@. Concrete
-- instances are provided by the stdlib's language-specific root
-- modules (community alternatives are equally valid). A bare user
-- module that uses bracket syntax without importing any of these
-- gets a "no instance" error that is otherwise opaque about which
-- module is missing.
missingClassHint :: Text -> Doc ann
missingClassHint methodLabel
  | "IndexLike" `T.isPrefixOf` methodLabel =
      "(class IndexLike lives in 'internal'; instances are provided by the stdlib's language-specific root modules)"
  | "Indexable" `T.isPrefixOf` methodLabel =
      "(class Indexable lives in 'internal'; instances are provided by the stdlib's language-specific root modules)"
  | "Sliceable" `T.isPrefixOf` methodLabel =
      "(class Sliceable / SliceableDim live in 'internal'; instances are provided by the stdlib's language-specific root modules)"
  | "Functor" `T.isPrefixOf` methodLabel =
      "(the stdlib class Functor is defined in 'root'; per-language 'map' instances are provided by the stdlib's language-specific root modules)"
  | otherwise = ""

-- | Wrap a typed bound argument in its @IndexLike.__to_index__@
-- instance call. With @__to_index__ :: i -> ?Int64@, every bound --
-- including Optional-typed expressions and the literal @NullS@ from
-- the desugar's @(Null :: ?Int64)@ -- can be dispatched through the
-- same lookup: the @IndexLike (?T)@ instance for each integer width
-- forwards @Null@ through, and the non-Optional @IndexLike T@ instances
-- wrap the value in @Just(cast)@. A missing instance raises a sourced
-- codegen error with the bound's type and target language.
wrapBounds :: Lang -> Int -> PatEvidence -> [(Type, PolyExpr)] -> MorlocMonad [PolyExpr]
wrapBounds lang midx pev bounds =
  mapM (uncurry (wrapBoundInToIndex lang midx)) (zipPrefix (patBounds pev <> repeat Nothing) bounds)

wrapBoundInToIndex
  :: Lang
  -> Int
  -> Maybe Evidence
  -> (Type, PolyExpr)
  -> MorlocMonad PolyExpr
wrapBoundInToIndex lang midx (Just ev) (boundType, expr) = do
  src <- evidenceSource midx lang ev (EV "__to_index__")
  return $ PolyApp (PolyExe (Idx midx (FunT [boundType] optI64T)) (builtSrcCall src)) [expr]
wrapBoundInToIndex lang midx Nothing (boundType, _) = do
  requireDetermined midx boundType
  MM.throwSourcedError midx $
    "No IndexLike.__to_index__ instance found for" <+> pretty boundType
    <+> "in language" <+> pretty lang
    <+> "(needed to cast bracket bound to ?Int64)"
    <> line <> missingClassHint "IndexLike"


-- | Render a 'TypeF' to a flat per-language form string with macro
-- arguments substituted at every level. Two morloc types that resolve
-- to the same native runtime type render to the same string, which
-- makes the literal-dispatch \"is a wrap needed?\" comparison
-- invariant to surface-level macro position numbering (e.g.
-- @std::vector<$2>@ for @Vector (n :: Nat) a@ and @std::vector<$1>@
-- for @List a@ both render to @std::vector<double>@ for an
-- element of @Real@, even though their templates differ in macro
-- index). Multi-positional templates with transposed args still
-- render to different strings because their arguments are
-- substituted into different macro slots.
--
-- This reuses 'Macro.expandMacro' so the rendering logic stays in
-- one place. The result is for comparison only; the codegen
-- elsewhere uses its own language-specific renderers.
renderConcreteForm :: Scope -> TypeF -> Text
renderConcreteForm _ (VarF (FV _ cv)) = unCVar cv
renderConcreteForm gscope (AppF (VarF (FV _ cv)) args) =
  let typeArgs = tyArgs args
  in Macro.expandMacro (unCVar cv) (map (renderConcreteForm gscope) typeArgs)
renderConcreteForm _ (NamF _ (FV _ cv) _ _) = unCVar cv
renderConcreteForm gscope (RecF (FV _ cv) args) = Macro.expandMacro (unCVar cv) (map (renderConcreteForm gscope) (tyArgs args))
renderConcreteForm gscope (EnumF (FV _ cv) args _) = Macro.expandMacro (unCVar cv) (map (renderConcreteForm gscope) (tyArgs args))
renderConcreteForm gscope (VariantF (FV _ cv) args _) = Macro.expandMacro (unCVar cv) (map (renderConcreteForm gscope) (tyArgs args))
renderConcreteForm gscope (OptionalF t) = "?" <> renderConcreteForm gscope t
renderConcreteForm gscope (FunF ts t) =
  "(" <> T.intercalate "," (map (renderConcreteForm gscope) ts) <> ")->" <> renderConcreteForm gscope t
renderConcreteForm _ (UnkF (FV _ cv)) = unCVar cv
renderConcreteForm gscope (AppF h args) =
  let typeArgs = tyArgs args
  in renderConcreteForm gscope h <> "<" <> T.intercalate "," (map (renderConcreteForm gscope) typeArgs) <> ">"

-- | Decide whether a literal at native type with outer TVar @nativeTv@
-- and wire-form outer TVar @wireTv@ needs a @Packable@ wrap in language
-- @lang@. A wrap is needed iff the native's per-language form differs
-- from the wire form's. If a wrap is structurally needed but no
-- @Packable@ instance exists for this language, throws a sourced
-- error.
maybePackableWrap :: Int -> Lang -> Type -> Type -> MorlocMonad (Maybe (Indexed Source))
maybePackableWrap midx lang nativeT wireT = do
  let nativeTv = extractKey (type2typeu nativeT)
      wireTv = extractKey (type2typeu wireT)
  if nativeTv == wireTv
    then return Nothing
    else do
      -- Compare the FULL rendered per-language form (with macros
      -- substituted at every level), not just the outer CV templates.
      -- A literal at @Vector 4 Real@ and the same literal at its wire
      -- form @List Real@ both render to @std::vector<double>@ in C++,
      -- so no wrap is needed despite the macro templates differing
      -- (@std::vector<$2>@ vs @std::vector<$1>@). Transposed-arg cases
      -- (e.g. @std::map<$2,$1>@ vs @std::map<$1,$2>@ applied to args
      -- swapped accordingly) ALSO render to the same string -- which
      -- is correct, since the user's newtype declaration explicitly
      -- chose to swap argument positions.
      nativeTF <- inferConcreteType lang (Idx midx nativeT)
      wireTF   <- inferConcreteType lang (Idx midx wireT)
      gscope <- MM.getGeneralScope
      let nativeRendered = renderConcreteForm gscope nativeTF
          wireRendered   = renderConcreteForm gscope wireTF
          -- A form whose template has no placeholders (R's "list") renders
          -- alike for every argument, so the arguments of a form the type
          -- declares are compared too. A type with no form of its own here
          -- has its representation's (NEWT-2).
          typeArgs (AppF _ args) = Just (map (renderConcreteForm gscope) (tyArgs args))
          typeArgs _ = Nothing
      ownForm <- Map.member nativeTv <$> MM.getConcreteScope lang
      let argsAgree = case (typeArgs nativeTF, typeArgs wireTF) of
            (Just as, Just bs) -> as == bs
            _ -> True
      if not ownForm || (nativeRendered == wireRendered && argsAgree)
        then return Nothing
        else do
          maySrc <- findCoveringPacker lang midx nativeT
          case maySrc of
            Just src -> return (Just src)
            Nothing -> MM.throwSourcedError midx $
              "Cannot construct a value of"
              <+> squotes (pretty nativeTv)
              <+> "from a"
              <+> pretty wireTv
              <+> "literal in"
              <+> pretty lang <> "."
              <+> "The native per-language form"
              <+> dquotes (pretty nativeRendered)
              <+> "differs from the wire form's"
              <+> dquotes (pretty wireRendered) <> ";"
              <+> "declare 'instance Packable"
              <+> pretty wireTv
              <+> pretty nativeTv
              <+> "where source"
              <+> pretty lang
              <+> "..."
              <+> "(... as pack, ... as unpack)'."

-- | Build a literal at @userT@ from its wire form @inner@ (at @wireT@) with
-- the Packable packer @src@, the way a received value of that type is built,
-- so the two cannot differ (NATIVE-9). In Python a value whose form is a
-- numpy array is built by the decoder when received, and no packer runs;
-- @conform@ builds a literal the same way. R builds every list literal by
-- its decoder's rule in serialization. A value holding a function has no
-- schema to decode it by.
packLiteral :: Int -> Lang -> Type -> Type -> Indexed Source -> PolyExpr -> MorlocMonad PolyExpr
packLiteral midx lang wireT userT src inner
  | containsFunT userT || not wireIsList = return packed
  | langName' == "py" = do
      form <- inferConcreteType lang (Idx midx userT)
      gscope <- MM.getGeneralScope
      return $
        if T.takeWhile (/= '<') (renderConcreteForm gscope form) == "numpy.ndarray"
          then PolyIntrinsic (Idx midx userT) IntrConform [inner]
          else packed
  -- An R value whose serial form is no Packable is decoded at its own type
  -- and not packed when received, so its literal is the list at that type,
  -- which serialization builds by the decoder's rule.
  | langName' == "r"
  , PolyList (Idx c _) _ xs <- inner
  , AppT (VarT userTV) userArgs <- userT = do
      ast <- inferConcreteType lang (Idx midx userT) >>= makeSerialAST midx lang
      return $ case ast of
        SerialPack {} -> packed
        _ -> PolyList (Idx c userTV) (map (Idx c) userArgs) xs
  | otherwise = return packed
  where
    langName' = ML.langName lang
    wireIsList = case wireT of
      AppT (VarT v) _ -> v == BT.list
      _ -> False
    packed = PolyApp (PolyExe (Idx midx (FunT [wireT] userT)) (builtSrcCall src)) [inner]

-- | LstS dispatch counterpart to 'dispatchPrimLit'. Computes the
-- wire-parent root of @userT@, expects it to be list-shaped, and either
-- emits a natural PolyList or wraps it with the @Packable@ @unpack@
-- source. @userArgs@ is the user-annotated arg list (may be empty when
-- the literal is annotated at a bare-VarT newtype-list); the
-- wire-parent's arg list is used inside the wrap and as the fallback
-- in the bare-VarT case.
dispatchListLit ::
  Int ->         -- midx
  Int ->         -- cidx
  Lang ->
  Type ->        -- user-facing type
  TVar ->        -- outer TVar of userT
  [ArgT] ->      -- user's annotated args (empty for bare-VarT case)
  [PolyExpr] ->  -- already-elaborated children
  MorlocMonad PolyExpr
dispatchListLit midx cidx lang userT userTV userArgs xs' = do
  scope <- MM.getGeneralScope
  let wireT = typeOf (TE.wireParentRoot scope (type2typeu userT))
  case wireT of
    AppT (VarT wireTV) wireArgs -> do
      maySrc <- maybePackableWrap midx lang userT wireT
      case maySrc of
        Nothing
          -- Bare-VarT case (e.g. @type Pat = [Pat]@ or @newtype Bytes
          -- = List UInt8@): the user's outer TVar takes no arguments,
          -- so tagging the @PolyList@ with @userTV@ AND attaching the
          -- wire-parent's args would produce a malformed @AppF Pat
          -- [List Pat]@ at @typeFof@ time. Tag with the wire-parent's
          -- TVar (e.g. @List@) so the resulting @AppF@ has the
          -- correct list shape; the user's alias identity is
          -- recovered downstream by the alias-walking serialisation
          -- pass.
          | null userArgs ->
              return $ PolyList (Idx cidx wireTV) (map (Idx cidx) wireArgs) xs'
          -- Parametric case (e.g. @[1,2,3] :: Vec 3 Int@): the user
          -- supplied an arg list, so tag with @userTV@ and keep the
          -- user's args (including phantom Nat dims).
          | otherwise ->
              return $ PolyList (Idx cidx userTV) (map (Idx cidx) userArgs) xs'
        Just src ->
          packLiteral midx lang (AppT (VarT wireTV) wireArgs) userT src $
            PolyList (Idx cidx wireTV) (map (Idx cidx) wireArgs) xs'
    _ -> MM.throwSourcedError midx "Expected a list type"

-- | Emit a primitive literal at user-facing type @userT@ (whose outer
-- TVar is @userTV@), or wrap it with the @Packable@ @unpack@ source
-- for @userTV@ when the native's per-language form diverges from the
-- wire form's. @mkLit@ builds the natural literal given the TVar at
-- which it should be tagged (user's TVar when emitting naturally,
-- wire-form's TVar when emitting inside a wrap).
-- | Reject a NUL byte in a string literal bound for a language that cannot
-- represent one.
--
-- R refuses to parse a source-level @"\000"@ at all, so the generated pool
-- would die before any runtime guard could fire; the failure has to be caught
-- while the literal still has a source position. That position is why the
-- check lives here rather than in the pool printer, which is pure and can only
-- abort with a call stack naming the compiler instead of the program.
--
-- The scan runs before the descriptor is consulted so that a program with no
-- NUL-bearing literal -- which is nearly all of them -- pays one 'T.any' per
-- literal and never reads a descriptor.
checkStringNul :: Int -> Lang -> Text -> MorlocMonad ()
checkStringNul idx lang s
  | not (T.any (== '\0') s) = return ()
  | langAllowsStringNul lang = return ()
  | otherwise =
      MM.throwSourcedError idx $
        "This string literal contains a NUL byte, which the"
        <+> pretty (ML.langName lang)
        <+> "pool cannot represent in its native string type."
        <+> "Move the literal to a language that can (Python, C++, or the"
        <+> "nexus itself), or remove the NUL byte."
        <+> "See the allow_string_null field in the language's lang.yaml."

-- | Whether a language's native string type admits an embedded NUL, read from
-- the embedded lang.yaml registry. A language with no descriptor, or one whose
-- descriptor does not parse, is treated as permissive -- the same default the
-- descriptor itself carries, so an unreadable descriptor cannot turn into a
-- spurious rejection of the user's program.
langAllowsStringNul :: Lang -> Bool
langAllowsStringNul lang =
  case lookup (T.unpack (ML.langName lang)) embeddedRegistry of
    Just yamlText -> case loadLangDescriptorFromText yamlText of
      Right desc -> ldAllowStringNull desc
      Left _ -> True
    Nothing -> True
  where
    embeddedRegistry =
      [(n, DF.embededFileText ef) | (n, ef) <- DF.langRegistryFiles]

dispatchPrimLit ::
  Int ->                  -- midx
  Lang ->
  Type ->                 -- user-facing type (e.g. VarT Speed)
  TVar ->                 -- outer TVar of userT
  (TVar -> PolyExpr) ->   -- natural literal constructor parameterised by tag TVar
  MorlocMonad PolyExpr
dispatchPrimLit midx lang userT userTV mkLit = do
  scope <- MM.getGeneralScope
  let wireTU = TE.wireParentRoot scope (type2typeu userT)
      wireTV = extractKey wireTU
  maySrc <- maybePackableWrap midx lang userT (typeOf wireTU)
  case maySrc of
    Nothing -> return (mkLit userTV)
    Just src -> packLiteral midx lang (typeOf wireTU) userT src (mkLit wireTV)

expressPolyExpr ::
  (Lang -> Lang -> Maybe RemoteForm) ->
  Lang ->
  Indexed Type ->
  AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) ->
  MorlocMonad PolyExpr
-- A label: its configuration moves to this node's manifold. A labeled
-- function value is wrapped like any function value passed on; a labeled call
-- is a manifold of its own in the caller's language, taking the values it
-- uses, so the hooks wrap that one call and nothing its arguments computed.
expressPolyExpr _ parentLang pc (AnnoS (Idx midx t) (_, args) (LabelS k inner@(AnnoS _ ic ie))) = do
  cfg <- MM.gets (Map.lookup k . stateManifoldConfig)
  MM.modify' $ \s -> s {stateManifoldConfig = maybe id (Map.insert midx) cfg (stateManifoldConfig s)}
  reg <- MM.gets stateLangRegistry
  bconf <- MM.gets stateBuildConfig
  let findRemote = decideRemoteness reg bconf cfg
      form = ManifoldFull (map unvalue args)
  case t of
    FunT _ _ -> expressPolyExpr findRemote parentLang pc (AnnoS (Idx midx t) ic ie)
    _ -> do
      body <- returnedPoly <$> expressPolyExprWrap parentLang (Idx midx t) inner
      case findRemote parentLang parentLang of
        Nothing -> mkPolyManifold parentLang midx form body
        Just remote -> do
          job <- MM.freshManifoldIndex midx
          innerWrap <- mkPolyManifold parentLang job form body
          let remoteApp =
                PolyReturn $ PolyApp
                  (PolyRemoteInterface parentLang pc [] remote innerWrap)
                  [PolyBndVar (A parentLang) i | Arg i _ <- args]
          mkPolyManifold parentLang midx form remoteApp
  where
    returnedPoly (PolyLet i e1 e2) = PolyLet i e1 (returnedPoly e2)
    returnedPoly r@(PolyReturn _) = r
    returnedPoly x = PolyReturn x
expressPolyExpr
  findRemote
  parentLang
  _
  ( AnnoS
      (Idx midx (FunT lamInputTypes lamOutType))
      (Idx cidxLam _, lamArgs)
      ( LamS
          vs
          ( AnnoS
              _
              (Idx _ appLang, appArgs)
              ( AppS
                  funExpr@(AnnoS (Idx _ (FunT callInputTypes _)) (Idx _ callLang, headArgs) _)
                  xs
                )
            )
        )
    )
    | isLocal = do
        -- The lambda's arguments are its captures followed by its own
        -- parameters; a body need not use every parameter, so the split is
        -- taken from the lambda, not from the application inside it.
        let nContextArgs = length lamArgs - length vs
            contextArgs = map unvalue (take nContextArgs lamArgs)
        typedLambdaArgs <- map typedArg <$> zipExact midx (drop nContextArgs lamArgs) (take (length vs) lamInputTypes)
        xs' <- zipArgs midx xs callInputTypes >>= mapM (\(x, t) -> expressPolyExprWrap appLang (mkIdx x t) x)

        call <- expressPolyApp callLang funExpr xs'
        -- Callback-return force lifted to Poly-stage 'EffectBoundary'.
        mkPolyManifold parentLang midx (ManifoldPart contextArgs typedLambdaArgs) call
    | not isLocal = do
        let nContextArgs = length lamArgs - length vs
        typedPassedArgs <- map typedArg <$> zipExact midx (drop nContextArgs lamArgs) (take (length vs) lamInputTypes)
        xsInfo <- mapM partialExpress xs

        let xs' = map (\(_, _, e) -> e) xsInfo
            -- the foreign body evaluates the head too, so it receives the
            -- values the head refers to (a captured closure it calls) along
            -- with those of the arguments
            callArgs = unique (concatMap (\(rs, _, _) -> rs) xsInfo <> [i | Arg i _ <- headArgs])
            args = [i | Arg i _ <- appArgs]
            allParentArgs = args <> [i | (_, Just (i, _), _) <- xsInfo]
            lets = [PolyLet i e | (_, Just (i, e), _) <- xsInfo]
            passedParentArgs = unique (concat [[r | r <- allParentArgs, r == i] | i <- callArgs])

            lambdaTypeMap = zipSame vs (map (Idx cidxLam) (take (length vs) lamInputTypes))
            -- The values handed to the interface are exactly the indices the
            -- callee binds, in that order. An argument realized in another
            -- language is computed here and bound to a fresh index, so the
            -- list of indices the call names is not the list of this
            -- lambda's own arguments; supplying one while naming the other
            -- leaves the callee reading a variable nothing assigned.
            appArgVar = [(i, v) | Arg i v <- appArgs]
            letArgVar = [(i, e) | (_, Just (i, _), e) <- xsInfo]
            boundVars = map boundVar passedParentArgs
            boundVar i = case lookup i letArgVar of
              Just e -> e
              Nothing -> case lookup i appArgVar of
                Just v -> PolyBndVar (maybe (A parentLang) C (lookup v lambdaTypeMap)) i
                Nothing -> error "unreachable: a called index is bound here or minted here"
            untypedContextArgs = map unvalue $ take nContextArgs lamArgs

            localForm = ManifoldPart untypedContextArgs typedPassedArgs

            foreignForm = ManifoldFull [Arg i None | i <- passedParentArgs]

        call <- expressPolyApp callLang funExpr xs'

        innerWrap <- mkPolyManifold callLang midx foreignForm call
        let remoteApp =
              PolyApp
                ( PolyRemoteInterface callLang (Idx cidxLam lamOutType) passedParentArgs (fromJust remote)
                    innerWrap
                )
                boundVars
            -- Cross-language RPC caller/callee force lifted to
            -- Poly-stage 'EffectBoundary'.
        mkPolyManifold parentLang midx localForm (chain lets (PolyReturn remoteApp))
    where
      remote = findRemote parentLang callLang
      isLocal = isNothing remote

      chain :: [a -> a] -> a -> a
      chain [] x = x
      chain (f : fs) x = chain fs (f x)

      partialExpress ::
        AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) ->
        MorlocMonad
          ( [Int]
          , Maybe (Int, PolyExpr)
          , PolyExpr
          )
      partialExpress (AnnoS (Idx _ t) (Idx cidx _argLang, [Arg idx _]) (BndS _)) = do
        let x' = PolyBndVar (C (Idx cidx t)) idx
        return ([idx], Nothing, x')
      partialExpress x@(AnnoS (Idx _ t) (Idx cidx argLang, args) _)
        | argLang == callLang = do
            let argParentType = Idx cidx t
            x' <- expressPolyExprWrap argLang argParentType x
            return ([i | Arg i _ <- args], Nothing, x')
        | otherwise = do
            let argparentType = Idx cidx t
            letVal <- expressPolyExprWrap argLang argparentType x
            idx <- MM.getCounter

            let x' = PolyLetVar (Idx cidx t) idx
            return ([idx], Just (idx, letVal), x')
expressPolyExpr _ parentLang _ (AnnoS lambdaType@(Idx midx _) (Idx _ lang, manifoldArguments) (LamS vs body)) = do
  -- A lambda value lives in the language of the expression that builds it
  -- (Realize pins it there); this clause has no crossing for a lambda in a
  -- foreign slot, so a disagreement here would land the lambda's manifold
  -- in the wrong pool.
  when (lang /= parentLang) $
    MM.throwCompilerBugAt midx $
      "a lambda value is realized in " <> pretty lang
        <> " inside an expression realized in " <> pretty parentLang
        <> "; a lambda takes the language of the expression that builds it"
  -- Only the leading @length vs@ inputs belong to THIS lambda; when the body
  -- returns a function (a curried type flattened by 'normalizeType'), the
  -- surplus inputs belong to the returned closure, expressed within @body@.
  -- The body's type is what is left once this lambda's inputs are consumed.
  -- It is the type a crossing inside the body carries: with the lambda's
  -- whole type the body would cross as a function value, and a suspension
  -- at its root would be run by the callee and reflected by the caller.
  let (inputTypes, bodyType) = splitFunAt (length vs) (val lambdaType)
  body' <- expressPolyExprWrap lang (mkIdx body bodyType) body

  let contextArguments = map unvalue $ take (length manifoldArguments - length vs) manifoldArguments
      boundArguments = map unvalue $ drop (length contextArguments) manifoldArguments
  typeBoundArguments <- map typedArg <$> zipExact midx boundArguments inputTypes
  -- Callback-return force lifted to Poly-stage 'EffectBoundary'.

  mkPolyManifold lang midx (ManifoldPart contextArguments typeBoundArguments) (PolyReturn body')
-- Inline source call: emitted as a subexpression, unless it carries a label
-- (a labeled partial application), which needs a manifold to attach to.
expressPolyExpr
  findRemote
  parentLang
  _
  ( AnnoS
      (Idx midx _)
      (_, outerArgs)
      (AppS f@(AnnoS (Idx _ (FunT inputs _)) (Idx cidxCall callLang, _) (ExeS (SrcCall (Idx _ src)))) xs)
    )
    | srcInline src && isLocal = do
        checkRsizeArity midx src (length inputs)
        xsExpr <- zipArgs midx xs inputs >>= mapM (\(x, t) -> expressPolyArg callLang (Idx cidxCall t) x)
        labeled <- hasManifoldLabel midx
        if labeled
          then do
            func <- expressPolyApp callLang f xsExpr
            mkPolyManifold callLang midx (ManifoldFull (map unvalue outerArgs)) func
          else expressPolyApp callLang f xsExpr >>= stripPolyReturn
    where
      remote = findRemote parentLang callLang
      isLocal = isNothing remote
      stripPolyReturn (PolyReturn e) = return e
      stripPolyReturn e = return e
-- Inline pattern call: skip PolyManifold. A pattern (record/struct
-- accessors, format patterns) renders as a direct expression in the
-- target language (e.g. `x.field` for .isFile). Wrapping in
-- PolyManifold creates a nested manifold whose body's `return` leaks
-- into the parent's scope when the manifold gets inlined by codegen.
-- Treating a pattern like srcInline matches its semantics - it's
-- always a leaf expression, never a real function call.
--
-- Bracket patterns (.[i], .[s:e:p]) are rewritten here into the
-- resolved per-language @Sliceable@ / @Indexable@ instance call, with
-- each non-Null bound wrapped in the @IndexLike@ instance cast for the
-- bound's own type. This is where the polymorphism of the bound types
-- (allowed all the way through typecheck) collapses to the language's
-- concrete @__to_index__@ source binding. A missing instance becomes a
-- sourced codegen error citing the type and the language.
expressPolyExpr
  findRemote
  parentLang
  pc
  (AnnoS (Idx midx _) (_, outerArgs) (PatS g@(Idx _ ft) (Idx cidxCall callLang, _) pev app))
    | isLocal = do
        mapM_ requireBoundDetermined (bracketBounds app)
        typed <- patWithInputs midx app inputs
        xsExpr <- mapM (\(x, t) -> (,) t <$> expressPolyArg callLang (Idx cidxCall t) x) typed
        dispatchPatCall callLang midx g pev out xsExpr
    -- Cross-pool bracket: receiver lives in one pool, surrounding
    -- expression in another. The call runs in its own pool's manifold, with
    -- the same dispatch as the local path: its per-bound @__to_index__@ casts
    -- and per-receiver instance calls, not raw bracket syntax.
    | otherwise = do
        mapM_ requireBoundDetermined (bracketBounds app)
        typed <- patWithInputs midx app inputs
        xsExpr <- mapM (\(x, t) -> (,) t <$> expressPolyArg callLang (mkIdx x t) x) typed
        call <- dispatchPatCall callLang midx g pev out xsExpr
        -- The inner manifold's body needs PolyReturn to emit a 'return' in
        -- the pool source.
        innerWrap <- mkPolyManifold callLang midx (ManifoldFull (map unvalue outerArgs)) (PolyReturn call)
        let remoteApp =
              PolyReturn $ PolyApp
                ( PolyRemoteInterface callLang pc [] (fromJust remote) innerWrap )
                [PolyBndVar (A parentLang) i | Arg i _ <- outerArgs]
        mkPolyManifold parentLang midx (ManifoldFull (map unvalue outerArgs)) remoteApp
    where
      (inputs, out) = splitFunAt (length app) ft
      remote = findRemote parentLang callLang
      isLocal = isNothing remote
      -- POLY-6 at the bound itself rather than at the bracket.
      requireBoundDetermined (AnnoS (Idx i t) _ _) = requireDetermined i t
      bracketBounds (PatIndex b _) = [b]
      bracketBounds (PatSlice a b c _) = [a, b, c]
      bracketBounds (PatGet _ bs _) = bs
      bracketBounds _ = []
expressPolyExpr
  findRemote
  parentLang
  pc
  ( AnnoS
      (Idx midx _)
      (_, args)
      (AppS f@(AnnoS (Idx _ (FunT inputs _)) (Idx cidxCall callLang, _) _) xs)
    )
    | isLocal = do
        xsExpr <- zipArgs midx xs inputs >>= mapM (\(x, t) -> expressPolyArg callLang (Idx cidxCall t) x)

        func <- expressPolyApp callLang f xsExpr
        mkPolyManifold callLang midx (ManifoldFull (map unvalue args)) func
    | not isLocal = do
        xsExpr <- zipArgs midx xs inputs >>= mapM (\(x, t) -> expressPolyArg callLang (mkIdx x t) x)
        func <- expressPolyApp callLang f xsExpr
        innerWrap <- mkPolyManifold callLang midx (ManifoldFull (map unvalue args)) func
        let remoteApp =
              PolyReturn $ PolyApp
                ( PolyRemoteInterface callLang pc [] (fromJust remote) innerWrap )
                [PolyBndVar (A parentLang) i | Arg i _ <- args]
            -- Cross-language RPC caller/callee force lifted to
            -- Poly-stage 'EffectBoundary'.
        mkPolyManifold parentLang midx (ManifoldFull (map unvalue args)) remoteApp
    where
      remote = findRemote parentLang callLang
      isLocal = isNothing remote
-- Implicit eta-abstraction of a bare function value passed where a function is
-- expected. A computed value ('isComputedValue') is not eta-abstracted: its
-- own value clause (below) binds the native closure for 'LocalCallP'.
expressPolyExpr
  findRemote
  parentLang
  (val -> FunT pinputs poutput)
  e@(AnnoS (Idx midx (FunT callInputs _)) (Idx cidx callLang, _) inner)
    | not (isComputedValue inner), not (isLocalPartial inner), isLocal = do
        ids <- MM.takeFromCounter (length callInputs)
        let lambdaVals = bindVarIds ids (map (C . Idx cidx) callInputs)
            lambdaTypedArgs = zipWithSame annotate ids (map Just callInputs)
        retapp <- expressPolyApp parentLang e lambdaVals
        -- Implicit eta-abstraction of a function value. Any variables the
        -- wrapped value closes over (e.g. a let-bound closure `f <- ...;
        -- applyIt f 10`) are captured as CONTEXT args so the trampoline
        -- manifold receives them through its partial rather than referencing
        -- them as undefined free variables. When it captures nothing (a
        -- top-level function value) the wrapper stays a bare 'ManifoldPass'.
        -- Callback-return force lifted to Poly-stage 'EffectBoundary'.
        let ctxIds = Set.toList (polyFreeVars retapp `Set.difference` Set.fromList ids)
            form = case ctxIds of
              [] -> ManifoldPass lambdaTypedArgs
              _ -> ManifoldPart [Arg i None | i <- ctxIds] lambdaTypedArgs
        mkPolyManifold callLang midx form retapp
    | not (isComputedValue inner), not (isLocalPartial inner) = do
        ids <- MM.takeFromCounter (length callInputs)
        let lambdaArgs = [Arg i None | i <- ids]
            lambdaTypedArgs = map (`Arg` Nothing) ids
            callVals = bindVarIds ids (map (C . Idx cidx) callInputs)
        retapp <- expressPolyApp callLang e callVals
        -- The values the wrapped value closes over (a captured closure) are
        -- captured here, as in the local case above, and passed across with
        -- the arguments.
        let ctxIds = Set.toList (polyFreeVars retapp `Set.difference` Set.fromList ids)
            ctxArgs = [Arg i None | i <- ctxIds]
        innerWrap <- mkPolyManifold callLang midx (ManifoldFull (ctxArgs <> lambdaArgs)) retapp
        passed <- zipExact midx pinputs (map ann lambdaArgs)
        let remoteApp =
              PolyReturn $ PolyApp
                ( PolyRemoteInterface callLang (Idx cidx poutput) (ctxIds <> map ann lambdaArgs) (fromJust remote)
                    innerWrap
                )
                ( [PolyBndVar (A parentLang) i | i <- ctxIds]
                    <> [PolyBndVar (C (Idx cidx t)) i | (t, i) <- passed]
                )
            outerForm = case ctxIds of
              [] -> ManifoldPass lambdaTypedArgs
              _ -> ManifoldPart ctxArgs lambdaTypedArgs
            -- Cross-language eta-abstraction: force lifted to
            -- Poly-stage 'EffectBoundary'.
        mkPolyManifold parentLang midx outerForm remoteApp
    where
      remote = findRemote parentLang callLang
      isLocal = isNothing remote
expressPolyExpr _ _ _ (AnnoS (Idx i c) (Idx cidx _, rs) (BndS v)) = do
  case [j | (Arg j v') <- rs, v == v'] of
    [r] -> return $ PolyBndVar (C (Idx cidx c)) r
    rs' ->
      MM.throwSourcedError i $
        "Expected VarS"
          <+> dquotes (pretty v)
          <+> "of type"
          <+> parens (pretty c)
          <+> "to match exactly one argument, found:"
          <+> list (map pretty rs')
          <> "\n  v:" <+> pretty v
          <> "\n  cidx:" <+> pretty cidx
          <> "\n  gidx:" <+> pretty cidx
          <> "\n  rs:" <+> list (map pretty rs)
expressPolyExpr _ _ _ (AnnoS (Idx i c) (Idx cidx _, rs) (LetBndS v)) = do
  case [j | (Arg j v') <- rs, v == v'] of
    [r] -> return $ PolyLetVar (Idx cidx c) r
    _ -> MM.throwSourcedError i $ "Undefined let-bound variable:" <+> pretty v
expressPolyExpr
  _
  parentLang
  pc
  (AnnoS (Idx midx _) (Idx cidx lang, args) (LetS v e1 e2)) = do
    let bodyArgs = case e2 of AnnoS _ (_, ba) _ -> ba
    letId <- case [j | Arg j v' <- bodyArgs, v' == v] of
      [j] -> return j
      _ -> MM.getCounter
    let e1Type = case e1 of AnnoS (Idx _ t) _ _ -> mkIdx e1 t
    -- Express children under the LetS's OWN language (from Realize), not the
    -- caller's. expressContainer wraps in a cross-language manifold when the
    -- chain's lang differs from parentLang. This fuses sequential
    -- same-language calls (especially do-blocks) into one manifold.
    e1' <- expressPolyExprWrap lang e1Type e1
    e2' <- expressPolyExprWrap lang pc e2
    let e = PolyLet letId e1' e2'
    expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e
-- A pure-data literal whose recorded type was coerced to an effect type by an
-- effect-typed conditional arm (e.g. @? c = @throw : (x, x)@, @: [x]@, @: 42@).
-- The literal carries no effect, so strip the EffectT wrapper and express the
-- plain data -- the same stripping a do-block tail gets via its own clause.
-- Without this the container clauses reject the EffectT-headed type ("Expected
-- a tuple/list/record type").
expressPolyExpr fr pl pc (AnnoS (Idx midx (EffectT _ innerT)) c e)
  | isPureDataLit e = expressPolyExpr fr pl pc (AnnoS (Idx midx innerT) c e)
  where
    isPureDataLit LstS{}  = True
    isPureDataLit TupS{}  = True
    isPureDataLit NamS{}  = True
    isPureDataLit IntS{}  = True
    isPureDataLit RealS{} = True
    isPureDataLit StrS{}  = True
    isPureDataLit ConS{} = True
    isPureDataLit LogS{}  = True
    isPureDataLit UniS    = True
    isPureDataLit _       = False
expressPolyExpr _ _ _ (AnnoS (Idx midx t@(VarT v)) (Idx cidx lang, _) (RealS _ x)) =
  dispatchPrimLit midx lang t v (\tv -> PolyReal (Idx cidx tv) x)
expressPolyExpr _ _ _ (AnnoS (Idx midx t@(VarT v)) (Idx cidx lang, _) (IntS _ x)) =
  dispatchPrimLit midx lang t v (\tv -> PolyInt (Idx cidx tv) x)
expressPolyExpr _ _ _ (AnnoS (Idx midx t@(VarT v)) (Idx cidx lang, _) (LogS x)) =
  dispatchPrimLit midx lang t v (\tv -> PolyLog (Idx cidx tv) x)
expressPolyExpr _ _ _ (AnnoS (Idx midx t@(VarT v)) (Idx cidx lang, _) (StrS x)) = do
  checkStringNul midx lang x
  dispatchPrimLit midx lang t v (\tv -> PolyStr (Idx cidx tv) x)
-- A `data` constructor reaching a pool. It is a literal of the enum type
-- itself, so unlike the primitive literals above it takes no
-- 'dispatchPrimLit' wire-parent step: an enum HAS no wire parent to convert
-- from, and asking for one produces a spurious demand for a `Packable`
-- instance between the type and a phantom parent.
-- Which form a constructor takes is a property of its TYPE, not of the
-- constructor: an argument-free arm of a payload-bearing `data` is still a
-- tagged pointer, because a value of that type has to be able to hold any
-- arm. Deciding on this constructor's own arity would give `Leaf` a
-- one-byte enum form inside a `Tree` whose other arm carries fields.
-- The type is matched at its head but carried whole. A parameterized `data`
-- arrives applied (@Box Int@), and the arguments decide both the wire form
-- and the native declaration, so dropping them here would leave every arm's
-- field types standing at the declaration's own parameters.
expressPolyExpr findRemote parentLang _ (AnnoS (Idx _ t) (Idx cidx lang, _) (ConS _ n i xs))
  | Just v <- dataHeadTVar t = do
      scope <- MM.getGeneralScope
      if TE.dataIsEnum scope (TE.whnf scope (VarU v))
        then return $ PolyEnum (Idx cidx t) n i
        else do
          xs' <- mapM (\x@(AnnoS (Idx xi xt) _ _) -> expressPolyExprWrap lang (Idx xi xt) x) xs
          return $ PolyVariant (Idx cidx t) n i xs'
  where
    _ = (findRemote, parentLang)
expressPolyExpr _ _ _ (AnnoS (Idx midx t@(VarT v)) (Idx cidx lang, _) UniS) =
  dispatchPrimLit midx lang t v (\tv -> PolyNull (Idx cidx (VarT tv)))
-- Null carries the full type it stands in for, arguments included, so the
-- native form inferred from it at typeFof time keeps a parameterised
-- payload's arguments.
expressPolyExpr _ _ _ (AnnoS (Idx midx t) (Idx cidx _, _) NullS) =
  case t of
    -- The type is kept whole, nested Optionals included, so the absent value
    -- has the native form of the slot it fills. An undetermined payload is
    -- refused where its native form is inferred (POLY-6).
    OptionalT _ -> return $ PolyNull (Idx cidx t)
    _ -> MM.throwCompilerBug $
      "Null literal" <+> pretty midx <+> "typed" <+> dquotes (pretty t) <+> "rather than as an optional"
-- A list literal at type `Foo a1 ... an` has exactly one element-type arg
-- (the rest, if any, are Nat-kinded phantom dims, e.g. for Vector n a).
-- Extract the single non-Nat arg for typing children, but PRESERVE the
-- full args list in PolyList so phantom dims survive into language code
-- generators (Nat positions render as mempty in macro expansion).
expressPolyExpr _ parentLang pc (AnnoS (Idx midx userT@(AppT (VarT v) ts)) (Idx cidx lang, args) (LstS xs))
  | [tElem] <- tyArgs ts = do
      xs' <- mapM (\x -> expressPolyExprWrap lang (mkIdx x tElem) x) xs
      e <- dispatchListLit midx cidx lang userT v ts xs'
      expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e
-- LstS at a bare @VarT v@ where @v@ is a newtype (or transparent alias)
-- whose wire-parent root is list-shaped. Use the wire-parent's element
-- type for child typing and let 'dispatchListLit' decide whether to
-- emit naturally (e.g. @newtype Bytes = List UInt8@ with no per-language
-- override emits a bare list) or wrap with the @ListLike v@ converter.
expressPolyExpr _ parentLang pc (AnnoS (Idx midx userT@(VarT v)) (Idx cidx lang, args) (LstS xs)) = do
  scope <- MM.getGeneralScope
  let wireT = typeOf (TE.wireParentRoot scope (type2typeu userT))
  case wireT of
    AppT (VarT _) wireArgs
      | [elemT] <- tyArgs wireArgs -> do
          xs' <- mapM (\x -> expressPolyExprWrap lang (mkIdx x elemT) x) xs
          e <- dispatchListLit midx cidx lang userT v [] xs'
          expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e
    _ -> MM.throwSourcedError midx "Expected a list type"
-- A list literal at a multi-non-Nat-arg @AppT@ whose @Packable@ wire form
-- is list-shaped. E.g. @[("a",1)] :: Map Str Int@ with @instance
-- Packable (Map a b) (List (Tuple2 a b))@. The Packable wire form is
-- substituted, the children are typed against the wire form's element
-- type, and the result is wrapped with the @Packable@ @unpack@ source.
-- This is the codegen counterpart to the typecheck-side tier-2 lookup
-- in 'Morloc.Frontend.Typecheck.checkE'.
expressPolyExpr _ parentLang pc (AnnoS (Idx midx userT@(AppT (VarT _) _)) (Idx cidx lang, args) (LstS xs)) = do
  scopeW <- MM.getGeneralScope
  envW <- MM.gets stateInstances
  let mayWire = FI.packableWireForm scopeW envW (type2typeu userT)
  case mayWire of
    Just wireTU
      | AppT (VarT wireV) wireArgs <- typeOf wireTU
      , [elemT] <- tyArgs wireArgs -> do
          xs' <- mapM (\x -> expressPolyExprWrap lang (mkIdx x elemT) x) xs
          let wireT = AppT (VarT wireV) wireArgs
              inner = PolyList (Idx cidx wireV) (map (Idx cidx) wireArgs) xs'
          maySrc <- maybePackableWrap midx lang userT wireT
          built <- case maySrc of
            Just src -> packLiteral midx lang wireT userT src inner
            -- the type's form here is its wire form's
            Nothing -> return inner
          expressContainer pc (Idx midx parentLang) (Idx cidx lang) args built
    _ -> tryReduceLstS userT
  where
    tryReduceLstS t = do
      scope <- MM.getGeneralScope
      case reduceType scope t of
        Just t' -> expressPolyExprWrap parentLang pc (AnnoS (Idx midx t') (Idx cidx lang, args) (LstS xs))
        Nothing -> MM.throwSourcedError midx "Expected a list type"
-- A list literal whose recorded type head is a type-alias (e.g.
-- @type Pat = [Pat]@ used at the bare-name position, recorded as
-- @VarT "Pat"@ rather than @AppT (VarT "List") [...]@). The two patterns
-- above match when the head is already the list constructor, or when
-- the bare head's wire-parent reduces to list-shape. For aliases whose
-- wire-parent walk doesn't expose a list outer head (transparent aliases
-- through non-list types), fall through to one-step reduce-and-retry.
expressPolyExpr _ pl pc (AnnoS (Idx midx t) c e@(LstS _)) = do
  scope <- MM.getGeneralScope
  case reduceType scope t of
    Just t' -> expressPolyExprWrap pl pc (AnnoS (Idx midx t') c e)
    Nothing -> MM.throwSourcedError midx "Expected a list type"
-- Same guard as expressCore: only treat the head as a tuple when it IS the
-- TupleN constructor. A bare @length ts == length xs@ match would catch
-- user aliases like @FixedPair (n :: Nat) a@ whose type args (NatLit n,
-- a) are NOT the slot types; the cross-language wrap inside
-- expressContainer would then use a Nat literal as the foreign call's
-- parent type and fail downstream in makeSerialAST'. Falling through to the reduce-and-retry below expands the
-- alias to its body's tuple constructor.
expressPolyExpr _ parentLang pc (AnnoS (Idx midx (AppT (VarT v) (tyArgs -> ts))) (Idx cidx lang, args) (TupS xs))
  | v == BT.tuple (length xs)
  , length ts == length xs = do
      xs' <- mapM (expressTyped lang) (zipSame xs ts)
      let e = PolyTuple (Idx cidx v) xs'
      expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e
-- A tuple literal whose recorded type head is a type-alias or a
-- newtype-of-tuple (e.g. @type LL a = (a, ?(LL a))@ or
-- @newtype Matrix m n a = ((Int, Int), Vector (m * n) a)@). The
-- outer head's argument list does NOT zip 1:1 with the literal's
-- entries -- the body does. Walk one step through the wire-parent
-- (@expandWireParent@ walks both transparent aliases AND newtypes,
-- halting at primitives) to expose the tuple body.
--
-- If the outer type has a @Packable@ instance, we must additionally
-- inject the pack source so the foreign call sees the wrapped
-- native form (e.g. a @Matrix 2 3 Real@ literal materialises as a
-- tuple-of-tuple-and-Vector at the wire level and then gets passed
-- through @morloc_packMatrix@ to become an @mlc::Tensor2<double>@
-- before the call). Without the pack wrap, the codegen would hand
-- the tuple directly to a function expecting the wrapped native.
expressPolyExpr _ pl pc (AnnoS (Idx midx t) c@(Idx cidx lang, _) e@(TupS _)) = do
  scope <- MM.getGeneralScope
  case TE.expandWireParent scope (type2typeu t) of
    Just t' -> do
      let bodyT = typeOf t'
      maySrc <- maybePackableWrap midx lang t bodyT
      case maySrc of
        Just src -> do
          expressPolyExprWrap lang (Idx cidx bodyT) (AnnoS (Idx midx bodyT) c e)
            >>= packLiteral midx lang bodyT t src
        Nothing -> expressPolyExprWrap pl pc (AnnoS (Idx midx bodyT) c e)
    Nothing -> MM.throwSourcedError midx "Expected a tuple type"
expressPolyExpr _ parentLang pc (AnnoS (Idx midx (NamT o v ps rs)) (Idx cidx lang, args) (NamS entries)) = do
  -- C3 invariant: see comment at expressCore NamT above.
  assertRecordKeyOrder midx entries rs
  xs' <- mapM (\((_, x), (k, t)) -> (,) k <$> expressTyped lang (x, t)) (zipSame entries rs)
  let e = PolyRecord o (Idx cidx v) (map (Idx cidx) ps) xs'
  expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e
expressPolyExpr _ pl pc (AnnoS (Idx i t) c e@(NamS _)) = do
  scope <- MM.getGeneralScope
  case reduceType scope t of
    (Just t') -> expressPolyExprWrap pl pc (AnnoS (Idx i t') c e)
    Nothing -> MM.throwSourcedError i $ "Record literal typed" <+> dquotes (pretty t) <+> "rather than as a record"
-- Recursive call used as a value (not applied via AppS)
expressPolyExpr _ parentLang _ (AnnoS (Idx i c) (Idx _cidx _, _) (CallS v)) = do
  (mid, crossLang) <- lookupRecursiveTarget parentLang v
  -- Strip EffectT from return type (serial manifolds force thunks)
  case c of
    FunT inputs (EffectT effs out) ->
      return . PolyDoBlock (Idx i (EffectT effs out))
        $ PolyExe (Idx i (FunT inputs out)) (RecCallP mid crossLang)
    _ ->
      return $ PolyExe (Idx i c) (RecCallP mid crossLang)
expressPolyExpr _ _ _ (AnnoS (Idx i _) _ (AppS (AnnoS _ _ (BndS v)) _)) =
  MM.throwSourcedError i $
    "Undefined function" <+> dquotes (pretty v) <> ", did you forget an import?"
expressPolyExpr _ _ _ (AnnoS _ _ (AppS (AnnoS _ _ (LamS vs _)) _)) =
  error $ "All applications of lambdas should have been eliminated of length " <> show (length vs)
expressPolyExpr _ parentLang pc (AnnoS (Idx midx t) (Idx cidx lang, args) (IfS cond thenE elseE)) = do
  let boolType = VarT (TV "Bool")
  cond' <- expressPolyExprWrap lang (mkIdx cond boolType) cond
  thenE' <- expressPolyExprWrap lang (mkIdx thenE t) thenE
  elseE' <- expressPolyExprWrap lang (mkIdx elseE t) elseE
  let e = PolyIf cond' thenE' elseE'
  expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e
expressPolyExpr _ parentLang pc (AnnoS (Idx midx t) (Idx cidx lang, args) (DoBlockS x)) = do
  -- The inner expression has the unwrapped type (without EffectT).
  -- Passing EffectT through would cause cross-language calls to generate
  -- effect-wrapped return types for pure functions.
  let innerT = case t of EffectT _ inner -> inner; _ -> t
  x' <- expressPolyExprWrap lang (mkIdx x innerT) x
  let e = PolyDoBlock (Idx cidx t) x'
  expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e
-- A newtype cast runs in the language realization chose for it, where the
-- newtype has no form of its own (NEWT-7); a crossing to it is the cast.
expressPolyExpr _ parentLang pc (AnnoS (Idx midx t) (Idx cidx lang, args) (CoerceS coercion@(CoerceNewtype _ _) x)) = do
  x' <- expressPolyExprWrap lang (mkIdx x (annoTypeOf x)) x
  expressContainer pc (Idx midx parentLang) (Idx cidx lang) args (PolyCoerce coercion (Idx cidx t) x')
  where
    annoTypeOf (AnnoS (Idx _ tx) _ _) = tx
expressPolyExpr _ parentLang _ (AnnoS (Idx _ t) (Idx cidx _, _) (CoerceS coercion x@(AnnoS (Idx _ innerType) _ _))) = do
  x' <- expressPolyExprWrap parentLang (Idx cidx innerType) x
  return $ PolyCoerce coercion (Idx cidx t) x'
expressPolyExpr _ parentLang _ (AnnoS (Idx _ t) (Idx cidx _lang, _) (EvalS x@(AnnoS (Idx _ tx) _ _))) = do
  -- Source-level force. Cross-language handling (peel the
  -- 'PolyRemoteInterface' type and force the callee) is the
  -- responsibility of the Poly-stage 'EffectBoundary' pass, which
  -- walks the resulting tree and reconciles both sides of any
  -- 'PolyRemoteInterface' it finds. The forced expression keeps its own
  -- type (the suspension) below the force: a crossing there carries the
  -- suspension, and the force runs it here.
  x' <- expressPolyExprWrap parentLang (Idx cidx tx) x
  return $ PolyEval (Idx cidx t) x'
-- An IntrMap in a pool is what instance resolution left: a map the plain list
-- instance serves. The one walk of an IFile a slice and a chain of fields read
-- together has its step chain applied by the runtime walker to each element of
-- the slice, eliminating both the post-slice map and the deep-copy of any
-- record fields the selector throws away.
expressPolyExpr _ parentLang pc (AnnoS (Idx midx t) (Idx cidx lang, args) (IntrinsicS IntrMap [funcE, listE])) = do
  fused <- tryFuseSliceSelectorChain midx lang funcE listE t
  e <- case fused of
    Just polyExpr -> return polyExpr
    Nothing -> do
      let AnnoS (Idx _ listT) _ _ = listE
          AnnoS (Idx _ funcT) _ _ = funcE
      scope <- MM.getGeneralScope
      env <- MM.gets stateInstances
      mapSrc <- case FI.resolveArg scope emptyGamma env [functorMapMethod] (type2typeu listT) of
        Right (Just ev) -> evidenceSource midx lang ev (EV "map")
        Right Nothing -> noInstance midx lang "Functor.map" listT
        Left err -> MM.throwSourcedError midx $ "Cannot map over" <+> pretty listT <> ":" <+> FI.explainResolveError prettyConstraint err
      funcExpr <- expressPolyExprWrap lang (mkIdx funcE funcT) funcE
      listExpr <- expressPolyExprWrap lang (mkIdx listE listT) listE
      return $ PolyApp (PolyExe (Idx midx (FunT [funcT, listT] t)) (builtSrcCall mapSrc)) [funcExpr, listExpr]
  expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e
expressPolyExpr _ parentLang pc (AnnoS (Idx midx t) (Idx cidx lang, args) (IntrinsicS intr xs)) = do
  xs' <- mapM (\x@(AnnoS (Idx xi xt) _ _) -> expressPolyExprWrap lang (Idx xi xt) x) xs
  let e = PolyIntrinsic (Idx cidx t) intr xs'
  expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e

-- Nullary source/pattern call (e.g., clockResNs :: {Int})
expressPolyExpr
  findRemote
  parentLang
  pc
  f@(AnnoS (Idx midx _) (Idx _ callLang, args) (ExeS _))
    | isLocal = do
        call <- expressPolyApp callLang f []
        mkPolyManifold callLang midx (ManifoldFull (map unvalue args)) call
    | otherwise = do
        call <- expressPolyApp callLang f []
        innerWrap <- mkPolyManifold callLang midx (ManifoldFull (map unvalue args)) call
        let remoteApp =
              PolyReturn $ PolyApp
                ( PolyRemoteInterface callLang pc [] (fromJust remote) innerWrap )
                [PolyBndVar (A parentLang) i | Arg i _ <- args]
        mkPolyManifold parentLang midx (ManifoldFull (map unvalue args)) remoteApp
    where
      remote = findRemote parentLang callLang
      isLocal = isNothing remote
expressPolyExpr _ _ parentType x@(AnnoS (Idx m t) _ _) = do
  MM.sayVVV "Bad case"
  MM.sayVVV $ "  t :: " <> pretty t
  name' <- MM.metaName m
  case name' of
    (Just v) ->
      MM.throwSourcedError m $
        "Missing concrete:"
          <> "\n  t:" <+> viaShow t
          <> "\n  v:" <+> pretty v
          <> "\n parentType:" <+> pretty parentType
          <> "\n x:" <+> pretty x
    Nothing ->
      MM.throwSourcedError m $
        "Missing concrete in unnamed function:"
          <> "\n  t:" <+> pretty t
          <> "\n parentType:" <+> pretty parentType
          <> "\n x:" <+> pretty x

-- | The head of a type a `data` constructor may inhabit: the name itself, or
-- the name of an applied parameterized type. Nothing else can carry a
-- constructor.
dataHeadTVar :: Type -> Maybe TVar
dataHeadTVar (VarT v) = Just v
dataHeadTVar (AppT (VarT v) _) = Just v
dataHeadTVar _ = Nothing

-- | Bind a computed function value to a fresh variable, then call it.
bindThenCall :: Indexed Type -> PolyExpr -> [PolyExpr] -> MorlocMonad PolyExpr
bindThenCall fnT fe es = do
  i <- MM.getCounter
  return . PolyLet i fe . PolyReturn $ PolyApp (PolyLetVar fnT i) es

expressPolyApp ::
  Lang ->
  AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) ->
  [PolyExpr] ->
  MorlocMonad PolyExpr
expressPolyApp _ (AnnoS g@(Idx gi (FunT inputs _)) _ (ExeS (SrcCall (Idx si src)))) xs = do
  checkRsizeArity gi src (length inputs)
  return . PolyReturn $ PolyApp (PolyExe g (SrcCallP (Written si) src)) xs
expressPolyApp _ (AnnoS g _ (ExeS (SrcCall (Idx si src)))) xs =
  return . PolyReturn $ PolyApp (PolyExe g (SrcCallP (Written si) src)) xs
-- A function value computed by an application or an intrinsic (a
-- constructor's field, a throw), applied: bind it, then call it.
expressPolyApp lang f@(AnnoS g _ e) es
  | isComputedHead e = do
      fe <- expressPolyExprWrap lang g f
      bindThenCall g fe es
  where
    isComputedHead (AppS _ _) = True
    isComputedHead PatS {} = True
    isComputedHead (IntrinsicS _ _) = True
    isComputedHead _ = False
expressPolyApp _ (AnnoS g (_, args) (BndS v)) xs = do
  case [j | (Arg j u) <- args, u == v] of
    [j] -> return . PolyReturn $ PolyApp (PolyExe g (localApply g j xs)) xs
    _ -> error "Unreachable? BndS value should have been wired uniquely to args previously"
-- A let-bound function value applied in head position. A multiply-referenced
-- let lambda is kept shared (not inlined) by 'applyLambdas'; each use reaches
-- here as a 'LetBndS' head and lowers to a native closure call, exactly as the
-- 'BndS' (lambda-argument) case above.
expressPolyApp _ (AnnoS g (_, args) (LetBndS v)) xs = do
  case [j | (Arg j u) <- args, u == v] of
    [j] -> return . PolyReturn $ PolyApp (PolyExe g (localApply g j xs)) xs
    _ -> error "Unreachable? LetBndS value should have been wired uniquely to args previously"
-- A function value produced by a runtime effect and applied. A `<-` bind
-- leaves a forced function value ('EvalS') -- or an inline effectful block
-- ('DoBlockS') -- in head position. Force/evaluate it, bind the resulting
-- native closure, and call it via 'PolyLetVar' -> 'LocalCallP', as the
-- computed-head case above does. We peel the wrapper and express
-- its INNER (effect-typed) expression directly, rather than re-expressing the
-- function-typed wrapper node: the latter re-enters the eta-abstraction path
-- ('expressPolyExpr' on a bare @FunT@ value), which calls back into
-- 'expressPolyApp' and loops.
expressPolyApp lang (AnnoS (Idx _ t) (Idx cidx _, _) (EvalS x)) es = do
  x' <- expressPolyExprWrap lang (Idx cidx t) x
  bindThenCall (Idx cidx t) (PolyEval (Idx cidx t) x') es
expressPolyApp lang (AnnoS (Idx _ t) (Idx cidx _, _) (DoBlockS x)) es = do
  let innerT = case t of EffectT _ inner -> inner; _ -> t
  x' <- expressPolyExprWrap lang (mkIdx x innerT) x
  bindThenCall (Idx cidx t) (PolyDoBlock (Idx cidx t) x') es
-- A conditional that yields a function value, applied in head position (e.g. a
-- let-bound @? c = f : g@ inlined at its single call site). Express each branch
-- as a function value, bind the resulting conditional closure, and call it via
-- 'PolyLetVar' -> 'LocalCallP', mirroring the 'EvalS' / 'DoBlockS' clauses
-- above.
expressPolyApp lang (AnnoS (Idx i t) (Idx cidx clang, _) (IfS cond thenE elseE)) es = do
  -- the branches are expressed in the conditional's language and the call
  -- emitted in the caller's, with no crossing between them
  when (clang /= lang) $
    MM.throwCompilerBugAt i $
      "a conditional function value applied in" <+> pretty lang <+> "is realized in" <+> pretty clang
  let boolType = VarT (TV "Bool")
  cond' <- expressPolyExprWrap clang (mkIdx cond boolType) cond
  thenE' <- expressPolyExprWrap clang (mkIdx thenE t) thenE
  elseE' <- expressPolyExprWrap clang (mkIdx elseE t) elseE
  bindThenCall (Idx cidx t) (PolyIf cond' thenE' elseE') es
expressPolyApp lang (AnnoS (Idx i t) _ (CallS v)) xs = do
  (mid, crossLang) <- lookupRecursiveTarget lang v
  -- Serial manifolds force thunks before serializing, so strip EffectT from the
  -- return type and wrap in PolyDoBlock to reconstruct the thunk after deserializing.
  case t of
    FunT inputs (EffectT effs out) ->
      return . PolyReturn
        . PolyDoBlock (Idx i (EffectT effs out))
        $ PolyApp (PolyExe (Idx i (FunT inputs out)) (RecCallP mid crossLang)) xs
    _ ->
      return . PolyReturn $ PolyApp (PolyExe (Idx i t) (RecCallP mid crossLang)) xs
expressPolyApp _ (AnnoS _ _ (LamS _ _)) _ = error "unexpected LamS - should have been handled"
expressPolyApp _ (AnnoS _ _ (VarS _ _)) _ = error "unexpected VarS - should have been substituted"
expressPolyApp _ (AnnoS (Idx i t) _ e) _ =
  MM.throwSourcedError i $
    "expressPolyApp: function position has unexpected expression form:"
      <+> tagExpr e <+> "of type" <+> pretty t
  where
    tagExpr :: ExprS (Indexed Type) One (Indexed Lang, [Arg EVar]) -> MDoc
    tagExpr (UniS) = "UniS"
    tagExpr (NullS) = "NullS"
    tagExpr (BndS v) = "BndS" <+> pretty v
    tagExpr (LetBndS v) = "LetBndS" <+> pretty v
    tagExpr (CallS v) = "CallS" <+> pretty v
    tagExpr (ExeS _) = "ExeS"
    tagExpr (LamS _ _) = "LamS"
    tagExpr (AppS _ _) = "AppS"
    tagExpr PatS {} = "PatS"
    tagExpr (LstS _) = "LstS"
    tagExpr (TupS _) = "TupS"
    tagExpr (NamS _) = "NamS"
    tagExpr (RealS _ _) = "RealS"
    tagExpr (IntS _ _) = "IntS"
    tagExpr (LogS _) = "LogS"
    tagExpr (StrS _) = "StrS"
    tagExpr (ConS _ _ _ _) = "ConS"
    tagExpr (DoBlockS _) = "DoBlockS"
    tagExpr (EvalS _) = "EvalS"
    tagExpr (CoerceS _ _) = "CoerceS"
    tagExpr (IfS _ _ _) = "IfS"
    tagExpr (LetS v _ _) = "LetS" <+> pretty v
    tagExpr (VarS v _) = "VarS" <+> pretty v
    tagExpr (IntrinsicS _ _) = "IntrinsicS"
    tagExpr (LabelS _ _) = "LabelS"

-- | Detect the desugar-emitted @IntrMap (\v -> .selector v) (.[s:e:p] f)@
-- pattern where @f@ is IFile-typed, and emit a fused IFileWalk whose
-- step chain is @[WalkBracketSlice] ++ selectorSteps@. Returns Nothing
-- if the pattern does not fire, so the caller calls the list's @map@.
tryFuseSliceSelectorChain
  :: Int
  -> Lang
  -> AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar])
  -> AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar])
  -> Type
  -> MorlocMonad (Maybe PolyExpr)
tryFuseSliceSelectorChain midx callLang funcE listE outT =
  case (matchSelectorLambda funcE, matchSliceOnIFile listE) of
    (Just sel, Just ((sE, eE, pE), rcvExpr, rcvT, pev)) -> do
      isIFile <- typeHeadIs BT.ifileVar rcvT
      if not isIFile
        then return Nothing
        else do
          tailSteps <- selectorToWalkSteps midx sel
          -- Field/Key only tails are safe and cover the canonical
          -- ".[:].0" / ".[:].foo" pattern. Bracket-in-tail would need
          -- runtime args the slice walker doesn't allocate space for;
          -- fall back in that case.
          if not (all isStaticFieldStep tailSteps)
            then return Nothing
            else do
              let steps = bracketSliceSteps ++ tailSteps
              [sT, eT, pT] <- mapM (boundTypeAt midx listE) [0, 1, 2]
              -- Express each bound expression, then __to_index__ wrap
              -- it the same way the standalone slice path does.
              sP <- expressPolyArg callLang (mkIdx sE sT) sE
              eP <- expressPolyArg callLang (mkIdx eE eT) eE
              pP <- expressPolyArg callLang (mkIdx pE pT) pE
              rcvP <- expressPolyArg callLang (mkIdx rcvExpr rcvT) rcvExpr
              fmap Just $ emitIFileWalk callLang midx pev outT steps rcvP [(sT, sP), (eT, eP), (pT, pP)]
    _ -> return Nothing
  where
    isStaticFieldStep (WalkField _) = True
    isStaticFieldStep (WalkKey _)   = True
    isStaticFieldStep _             = False

matchSelectorLambda
  :: AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar])
  -> Maybe Selector
matchSelectorLambda (AnnoS _ _ (LamS [v] body)) =
  case body of
    AnnoS _ _ (PatS _ _ _ (PatGet sel [] (AnnoS _ _ (BndS v'))))
      | v == v' -> Just sel
    _ -> Nothing
matchSelectorLambda _ = Nothing

matchSliceOnIFile
  :: AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar])
  -> Maybe ( ( AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar])
             , AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar])
             , AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) )
           , AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar])
           , Type
           , PatEvidence )
matchSliceOnIFile (AnnoS _ _ (PatS _ _ pev (PatSlice sE eE pE rcvE@(AnnoS (Idx _ rcvT) _ _)))) =
  Just ((sE, eE, pE), rcvE, rcvT, pev)
matchSliceOnIFile _ = Nothing

boundTypeAt
  :: Int
  -> AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar])
  -> Int
  -> MorlocMonad Type
boundTypeAt midx (AnnoS _ _ (PatS (Idx _ (FunT inputs _)) _ _ _)) n
  | n < length inputs = return (inputs !! n)
  | otherwise = MM.throwSourcedError midx
      "chain fusion: bound-type index out of range"
boundTypeAt midx _ _ =
  MM.throwSourcedError midx
    "chain fusion: cannot inspect slice's input types"

expressContainer ::
  Indexed Type -> Indexed Lang -> Indexed Lang -> [Arg EVar] -> PolyExpr -> MorlocMonad PolyExpr
expressContainer pc (Idx midx parentLang) (Idx _ lang) args e
  | parentLang /= lang = do
      innerWrap <- mkPolyManifold lang midx (ManifoldFull (map unvalue args)) (wrapReturn e)
      return $ PolyApp
        ( PolyRemoteInterface lang pc [i | Arg i _ <- args] ForeignCall innerWrap )
        [PolyBndVar (A parentLang) i | Arg i _ <- args]
  | otherwise = return e
  where
    -- Push PolyReturn through PolyLet to the tail. Skip wrap if the tail
    -- is already a PolyReturn or a PolyManifold (whose own body produces a
    -- return when MonoManifold is unwrapped during serialize). Avoids
    -- `return(return(...))` in generated pools.
    wrapReturn (PolyLet i e1 e2) = PolyLet i e1 (wrapReturn e2)
    wrapReturn r@(PolyReturn _) = r
    wrapReturn r@(PolyManifold _ _ _ _ _) = r
    wrapReturn x = PolyReturn x

unvalue :: Arg a -> Arg None
unvalue (Arg i _) = Arg i None

-- | Free variable indices a PolyExpr references from its enclosing scope. Used
-- to capture the closure variables of an eta-abstracted function value as
-- context args. A nested PolyManifold contributes only its own context-arg
-- indices (the variables IT captures from this scope); its bound args and body
-- locals belong to it. A PolyLet binder is removed from its body's free set.
polyFreeVars :: PolyExpr -> Set.Set Int
polyFreeVars = go
  where
    go (PolyBndVar _ i) = Set.singleton i
    go (PolyLetVar _ i) = Set.singleton i
    -- A locally-bound function value applied via 'LocalCallP j' references
    -- the closure bound at index @j@ in the enclosing scope; other executable
    -- forms (source / pattern / recursive calls) name no enclosing variable.
    go (PolyExe _ (LocalCallP j)) = Set.singleton j
    go (PolyExe _ (PapplyP j)) = Set.singleton j
    go (PolyApp e es) = Set.unions (map go (e : es))
    go (PolyReturn e) = go e
    go (PolyLet i e1 e2) = Set.union (go e1) (Set.delete i (go e2))
    go (PolyManifold _ _ form _ _) = Set.fromList [i | Arg i _ <- manifoldContext form]
    go (PolyList _ _ es) = Set.unions (map go es)
    go (PolyTuple _ xs) = Set.unions (map (go . snd) xs)
    go (PolyRecord _ _ _ rs) = Set.unions (map (go . snd . snd) rs)
    go (PolyCacheBody _ _ _ e) = go e
    go (PolyDebugWrap _ _ e) = go e
    -- Value-wrapper forms carry sub-expressions in the SAME scope, so their
    -- free variables must flow through. Omitting any of these silently drops
    -- the captures of an eta-abstracted value that contains it, producing a
    -- closure that references undefined enclosing variables.
    go (PolyIf c t e) = Set.unions [go c, go t, go e]
    go (PolyDoBlock _ e) = go e
    go (PolyEval _ e) = go e
    go (PolyCoerce _ _ e) = go e
    go (PolyIntrinsic _ _ es) = Set.unions (map go es)
    go (PolyVariant _ _ _ es) = Set.unions (map go es)
    -- The loop-carried ids are re-bound each iteration, so they are local to the
    -- loop; a continue's values are ordinary sub-expressions in the loop scope.
    go (PolyLoop _ ids e) = Set.difference (go e) (Set.fromList ids)
    go (PolyLoopContinue xs) = Set.unions (map go xs)
    -- A cross-pool call names the enclosing indices it sends. They also
    -- reach here through the application's argument list whenever the two
    -- agree, so this clause changes nothing in a well-formed tree -- and
    -- it is what makes the free-variable computation total rather than
    -- dependent on every construction site remembering to keep them in
    -- step. The callee body is a separate scope and contributes nothing.
    go (PolyRemoteInterface _ _ is _ _) = Set.fromList is
    go (PolyExe _ (SrcCallP _ _)) = Set.empty
    go (PolyPat _ app) = Set.unions (map go (toList app))
    go (PolyExe _ (RecCallP _ _)) = Set.empty
    go (PolyLog _ _) = Set.empty
    go (PolyReal _ _) = Set.empty
    go (PolyInt _ _) = Set.empty
    go (PolyStr _ _) = Set.empty
    go (PolyEnum {}) = Set.empty
    go (PolyNull _) = Set.empty

-- | The manifold a call of @v@ (a 'CallS' name) invokes: a concrete manifold
-- (one with a language) named @v@, else the recursion target @v@ names.
-- Apply it to the three maps once and reuse the result: the reverse name map
-- is built when the maps are given.
recursiveTargetOf :: Map.Map Int EVar -> Map.Map Int Lang -> Map.Map EVar Int -> EVar -> Maybe Int
recursiveTargetOf nameMap langMap recTargets =
  let reverseMap = Map.fromList [(name, idx) | (idx, name) <- Map.toList nameMap, Map.member idx langMap]
  in \v -> case Map.lookup v reverseMap of
    Just idx -> Just idx
    Nothing -> case Map.lookup v recTargets of
      Just idx | Map.member idx langMap -> Just idx
      _ -> Nothing

-- | Resolve a function name to its manifold ID and determine if the call is cross-language.
-- Returns (manifold ID, Nothing) for same-pool calls, (manifold ID, Just targetLang) for foreign calls.
-- Searches all manifolds in stateName, not just exports, to support non-exported recursive helpers.
lookupRecursiveTarget :: Lang -> EVar -> MorlocMonad (Int, Maybe Lang)
lookupRecursiveTarget parentLang v = do
  nameMap <- MM.gets stateName
  langMap <- MM.gets stateManifoldLang
  reg <- MM.gets stateLangRegistry
  recTargets <- MM.gets stateRecursionTargets
  case recursiveTargetOf nameMap langMap recTargets v of
    (Just mid) -> do
      -- A cross-language recursive call only when the target is NOT co-located
      -- with the caller; a co-located member (futhark in cpp) is an in-process call.
      let crossLang = case Map.lookup mid langMap of
            Just tl | not (LR.coLocated reg parentLang tl) -> Just tl
            _ -> Nothing
      return (mid, crossLang)
    Nothing -> do
      skipped <- MM.gets stateSkippedGeneric
      case Map.lookup v skipped of
        Just (i, t) -> rejectUndetermined i t
        Nothing -> return ()
      MM.throwCompilerBug $ "Cannot resolve recursive call to" <+> pretty v

bindVarIds :: [Int] -> [Three Lang Type (Indexed Type)] -> [PolyExpr]
bindVarIds [] [] = []
bindVarIds (i : args) (t : types) = PolyBndVar t i : bindVarIds args types
bindVarIds [] ts = error $ "bindVarIds: too few arguments: " <> show ts
bindVarIds _ [] = error "bindVarIds: too few types"

-- | Applying the local function value @j@ (of the type in @g@) to @xs@: a
-- call, or, with fewer arguments than it takes, a partial application.
localApply :: Indexed Type -> Int -> [a] -> ExecutableExpressionPool
localApply (Idx _ (FunT ins _)) j xs
  | length xs < length ins = PapplyP j
localApply _ j _ = LocalCallP j

-- | A partial application of a local function value, which is built where
-- it is written (never eta-abstracted: that would redo it at each call).
isLocalPartial :: ExprS (Indexed Type) One c -> Bool
isLocalPartial (AppS (AnnoS g _ h) xs)
  | isLocalHead h, PapplyP _ <- localApply g 0 xs = True
  where
    isLocalHead (BndS _) = True
    isLocalHead (LetBndS _) = True
    isLocalHead _ = False
isLocalPartial _ = False

-- A computed function value that produces its result through evaluation rather
-- than being a bare callable: a let, a conditional, a forced thunk, an inline
-- effectful block, or an intrinsic (a constructor's field, a throw). Such
-- values are bound and applied via 'LocalCallP', never eta-abstracted (see the
-- eta clause in 'expressPolyExpr'), so their work runs once.
isComputedValue :: ExprS g One c -> Bool
isComputedValue (LetS {}) = True
isComputedValue (IfS {}) = True
isComputedValue (EvalS {}) = True
isComputedValue (DoBlockS {}) = True
isComputedValue (IntrinsicS {}) = True
isComputedValue _ = False
