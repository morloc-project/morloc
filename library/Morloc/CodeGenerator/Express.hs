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
  ) where

import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Morloc.BaseTypes as BT
import qualified Morloc.CodeGenerator.Grammars.Macro as Macro
import Morloc.CodeGenerator.Infer
import Morloc.CodeGenerator.Namespace
import Morloc.Data.Doc
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.Data.Map as Map
import qualified Morloc.Monad as MM
import qualified Morloc.TypeEval as TE
import Morloc.Typecheck.Internal (findPackableWireForm, unqualify)

mkIdx :: AnnoS g One (Indexed c, d) -> Type -> Indexed Type
mkIdx (AnnoS _ (Idx i _, _) _) = Idx i

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

setManifoldConfig ::
  Int ->
  AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) ->
  MorlocMonad ()
setManifoldConfig midx (AnnoS _ _ (AppS (AnnoS (Idx fidx _) _ (VarS _ _)) _)) = linkConfigIndex midx fidx
setManifoldConfig midx (AnnoS _ _ (AppS (AnnoS (Idx fidx _) _ (ExeS _)) _)) = linkConfigIndex midx fidx
setManifoldConfig midx (AnnoS _ _ (AppS e _)) = setManifoldConfig midx e
setManifoldConfig midx (AnnoS _ _ (LamS _ e)) = setManifoldConfig midx e
setManifoldConfig midx (AnnoS _ _ (DoBlockS e)) = setManifoldConfig midx e
setManifoldConfig midx (AnnoS _ _ (EvalS e)) = setManifoldConfig midx e
setManifoldConfig midx (AnnoS _ _ (CoerceS _ e)) = setManifoldConfig midx e
setManifoldConfig _ (AnnoS _ _ (IntrinsicS _ _)) = return ()
setManifoldConfig midx (AnnoS _ _ (IfS _ t _)) = setManifoldConfig midx t
setManifoldConfig _ (AnnoS _ _ (CallS _)) = return ()
setManifoldConfig _ _ = return ()

linkConfigIndex :: Int -> Int -> MorlocMonad ()
linkConfigIndex midx fidx = do
  s <- MM.get
  case Map.lookup fidx (stateManifoldConfig s) of
    Nothing -> return ()
    (Just mconfig) -> do
      MM.put (s {stateManifoldConfig = Map.insert midx mconfig (stateManifoldConfig s)})

propagateScope :: Int -> Int -> MorlocMonad ()
propagateScope calleeIdx appIdx = do
  s <- MM.get
  case GMap.yIsX calleeIdx appIdx (stateConcreteTypedefs s) of
    (Just gmap') -> MM.put $ s {stateConcreteTypedefs = gmap'}
    Nothing -> return ()

express :: AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) -> MorlocMonad PolyHead
express e@(AnnoS (Idx midx t) (Idx cidx _, _) _) = do
  -- Store the return effect labels before forceExportThunks strips them
  let retEffects = extractReturnEffects t
  MM.modify (\s -> s { stateManifoldEffects = Map.insert midx retEffects (stateManifoldEffects s) })
  forceExportThunks cidx t <$> expressCore e
  where
    extractReturnEffects (FunT _ (EffectT effs _)) = effs
    extractReturnEffects (EffectT effs _) = effs
    extractReturnEffects _ = Set.empty

-- At the export boundary, thunks cannot be serialized. This function:
--   1. Wraps thunk-typed args in PolyDoBlock so they are received as plain
--      values from the CLI and suspended inside the pool.
--   2. Wraps thunk return types in PolyEval so they are evaluated before
--      serialization back to the user.
forceExportThunks :: Int -> Type -> PolyHead -> PolyHead
forceExportThunks cidx t (PolyHead lang midx args body) =
  let inputTs = case t of FunT inputs _ -> inputs; _ -> []
      thunkArgIds = [ann a | (a, EffectT _ _) <- zip args inputTs]
      retT = case t of FunT _ ret -> ret; t' -> t'
      body' = suspendThunkArgs thunkArgIds body
      body'' = forceAtReturn cidx retT body'
   in PolyHead lang midx args body''
  where
    -- Wrap BndVar references to thunk-typed args in PolyDoBlock.
    -- The arg is deserialized as the inner type; the suspend creates the thunk.
    suspendThunkArgs [] e = e
    suspendThunkArgs ids e = goExpr ids e

    goExpr ids (PolyBndVar (C (Idx ci (EffectT effs inner))) i)
      | i `elem` ids = wrapSuspends ci (EffectT effs inner) i
    goExpr ids (PolyManifold l m f e) = PolyManifold l m f (goExpr ids e)
    goExpr ids (PolyLet i e1 e2) = PolyLet i (goExpr ids e1) (goExpr ids e2)
    goExpr ids (PolyReturn e) = PolyReturn (goExpr ids e)
    goExpr ids (PolyApp e es) = PolyApp (goExpr ids e) (map (goExpr ids) es)
    goExpr ids (PolyEval ti e) = PolyEval ti (goExpr ids e)
    goExpr ids (PolyDoBlock ti e) = PolyDoBlock ti (goExpr ids e)
    goExpr ids (PolyCoerce c ti e) = PolyCoerce c ti (goExpr ids e)
    goExpr ids (PolyIntrinsic ti intr es) = PolyIntrinsic ti intr (map (goExpr ids) es)
    goExpr ids (PolyList v ti es) = PolyList v ti (map (goExpr ids) es)
    goExpr ids (PolyTuple v es) = PolyTuple v (map (fmap (goExpr ids)) es)
    goExpr ids (PolyRecord o v ps rs) = PolyRecord o v ps (map (fmap (fmap (goExpr ids))) rs)
    goExpr ids (PolyIf c t' e) = PolyIf (goExpr ids c) (goExpr ids t') (goExpr ids e)
    goExpr ids (PolyRemoteInterface l ti is rf e) = PolyRemoteInterface l ti is rf (goExpr ids e)
    goExpr _ e = e

    -- Peel EffectT layers, wrapping each in PolyDoBlock, with the innermost
    -- BndVar carrying the fully-unwrapped type.
    wrapSuspends ci (EffectT effs inner) i =
      PolyDoBlock (Idx ci (EffectT effs inner)) (wrapSuspends ci inner i)
    wrapSuspends ci inner i = PolyBndVar (C (Idx ci inner)) i

    forceAtReturn c rt (PolyReturn e) = PolyReturn (wrapForces c rt e)
    forceAtReturn c rt (PolyManifold l m f e) = PolyManifold l m f (forceAtReturn c rt e)
    forceAtReturn c rt (PolyLet i e1 e2) = PolyLet i e1 (forceAtReturn c rt e2)
    forceAtReturn c rt e = wrapForces c rt e

    wrapForces c (EffectT _ inner) e = wrapForces c inner (PolyEval (Idx c inner) e)
    wrapForces _ _ e = e

expressCore :: AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) -> MorlocMonad PolyHead
expressCore (AnnoS (Idx midx c@(FunT inputs _)) (Idx cidx lang, _) (ExeS exe)) = do
  ids <- MM.takeFromCounter (length inputs)
  exe' <- case exe of
    (SrcCall src) -> return $ SrcCallP src
    (PatCall pat) -> return $ PatCallP pat
  let lambdaVals = fromJust $ safeZipWith PolyBndVar (map (C . Idx cidx) inputs) ids
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
  let lambdaVals = fromJust $ safeZipWith PolyBndVar (map (C . Idx cidx) inputs) ids
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
expressCore (AnnoS (Idx midx _) (_, lambdaArgs) (LamS _ e@(AnnoS (Idx _ applicationType) (c, _) x))) = do
  setManifoldConfig midx e
  expressCore (AnnoS (Idx midx applicationType) (c, lambdaArgs) x)
expressCore (AnnoS (Idx midx (AppT (VarT v) ts)) (Idx cidx lang, args) (LstS xs))
  | [t] <- filter (not . isKindTypeT) ts = do
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
  mayT <- evalGeneralStep midx (type2typeu t)
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
expressCore (AnnoS (Idx midx (AppT (VarT v) ts)) (Idx cidx lang, args) (TupS xs))
  | v == BT.tuple (length xs)
  , length ts == length xs = do
      let idxTs = zipWith mkIdx xs ts
      xs' <- fromJust <$> safeZipWithM (expressPolyExprWrap lang) idxTs xs
      let x = PolyTuple (Idx cidx v) (fromJust $ safeZip idxTs xs')
      return $ PolyHead lang midx [Arg i None | Arg i _ <- args] (PolyReturn x)
-- TupS at a non-tuple head (user alias, including phantom-Nat aliases,
-- and bare @VarT WrappedLL@ for @type WrappedLL = (Int, LL)@). Reduce
-- one alias step and retry; same shape as the NamS reduce-and-retry.
expressCore (AnnoS (Idx midx t) (Idx cidx lang, args) (TupS xs)) = do
  mayT <- evalGeneralStep midx (type2typeu t)
  case mayT of
    (Just t') -> expressCore (AnnoS (Idx midx (typeOf t')) (Idx cidx lang, args) (TupS xs))
    Nothing -> MM.throwSourcedError midx $ "Invalid tuple form: " <> pretty t
expressCore (AnnoS (Idx midx (NamT o v ps rs)) (Idx cidx lang, args) (NamS entries)) = do
  -- C3 invariant: the literal's key order must equal the declared
  -- schema's key order. Typecheck.hs reorders NamS against the
  -- declared NamU; if that ever regresses, this assertion catches the
  -- mis-zip at the point where it would corrupt the wire format.
  assertRecordKeyOrder midx entries rs
  let idxTypes = zipWith mkIdx (map snd entries) (map snd rs)
  xs' <- fromJust <$> safeZipWithM (expressPolyExprWrap lang) idxTypes (map snd entries)
  let x = PolyRecord o (Idx cidx v) (map (Idx cidx) ps) (zip (map fst rs) (zip idxTypes xs'))
  return $ PolyHead lang midx [Arg i None | Arg i _ <- args] (PolyReturn x)
expressCore (AnnoS (Idx midx t) (Idx cidx lang, args) (NamS entries)) = do
  mayT <- evalGeneralStep midx (type2typeu t)
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
    ensurePolyReturn (PolyManifold l m f e) = PolyManifold l m f (ensurePolyReturn e)
    ensurePolyReturn x = PolyReturn x

expressPolyExprWrap ::
  Lang ->
  Indexed Type ->
  AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) ->
  MorlocMonad PolyExpr
expressPolyExprWrap l t e@(AnnoS (Idx midx _) _ (LamS _ lamExpr)) = do
  setManifoldConfig midx lamExpr
  expressPolyExprWrapCommon l t e
expressPolyExprWrap l t e = expressPolyExprWrapCommon l t e

expressPolyExprWrapCommon ::
  Lang -> Indexed Type -> AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) -> MorlocMonad PolyExpr
expressPolyExprWrapCommon l t e@(AnnoS _ _ (AppS (AnnoS (Idx gidxCall _) _ _) _)) = do
  bconf <- MM.gets stateBuildConfig
  mconMap <- MM.gets stateManifoldConfig
  expressPolyExpr (decideRemoteness bconf (Map.lookup gidxCall mconMap)) l t e
expressPolyExprWrapCommon l t e@(AnnoS (Idx midx _) _ _) = do
  bconf <- MM.gets stateBuildConfig
  mconMap <- MM.gets stateManifoldConfig
  expressPolyExpr (decideRemoteness bconf (Map.lookup midx mconMap)) l t e

decideRemoteness :: BuildConfig -> Maybe ManifoldConfig -> Lang -> Lang -> Maybe RemoteForm
decideRemoteness _ Nothing l1 l2
  | l1 == l2 = Nothing
  | otherwise = Just ForeignCall
decideRemoteness _ (Just (ManifoldConfig _ _ Nothing)) l1 l2
  | l1 == l2 = Nothing
  | otherwise = Just ForeignCall
decideRemoteness bconf (Just (ManifoldConfig _ _ (Just res))) l1 l2 =
  case (buildConfigSlurmSupport bconf, l1 /= l2) of
    (Just True, _) -> Just $ RemoteCall res
    (_, True) -> Just $ ForeignCall
    _ -> Nothing

-- Express a function argument. A do-block passed where an effect-typed
-- (thunk) parameter is expected must be suspended whole -- identically to
-- a bare effectful application argument -- so the handler receives an
-- unevaluated thunk. Keep the EffectT on the inner expression so its
-- source call is auto-suspended (Grammars.Common). The shared DoBlockS
-- clause still strips EffectT for a return/export-position do-block, which
-- forceExportThunks / pushForceIntoRemote discharge at the boundary.
expressPolyArg ::
  Lang ->
  Indexed Type ->
  AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) ->
  MorlocMonad PolyExpr
expressPolyArg parentLang pc@(Idx _ (EffectT _ _)) (AnnoS (Idx midx t@(EffectT _ _)) (Idx cidx lang, args) (DoBlockS x)) = do
  x' <- expressPolyExprWrap lang (mkIdx x t) x
  let e = PolyDoBlock (Idx cidx t) x'
  return $ expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e
expressPolyArg l pc e = expressPolyExprWrap l pc e

-- | Look up the @Packable@ @pack@ source for a native type with outer
-- TVar @nativeTv@ in language @lang@. In @class Packable a b where
-- pack :: a -> b@, @a@ is the decomposed/directly-serialisable form
-- and @b@ is the wrapped form; @pack@ is the constructor direction
-- (build the wrapped @b@ from its decomposed @a@). Pack/unpack
-- compose recursively to marshal data toward the wire format. The
-- instance head is identified by @pack@'s output type (the wrapped
-- native). Returns @Nothing@ if no matching instance is present.
findPackablePack :: Lang -> TVar -> MorlocMonad (Maybe Source)
findPackablePack lang nativeTv = do
  sigmap <- MM.gets stateTypeclasses
  case Map.lookup (EV "pack") sigmap of
    Nothing -> return Nothing
    Just inst -> return $ listToMaybe
      [ src
      | TermTypes (Just et) cs _ <- instanceTerms inst
      , termOutputHead et == nativeTv
      , (_, Idx _ src) <- cs
      , srcLang src == lang
      ]
  where
    termOutputHead :: EType -> TVar
    termOutputHead et = case snd (unqualify (etype et)) of
      FunU _ out -> extractKey out
      other      -> extractKey other

-- | Look up the @IndexLike@ @__to_index__@ instance for an input type
-- in language @lang@. In @class IndexLike i where __to_index__ :: i ->
-- ?Int64@, instances are keyed by the WHOLE first-argument type --
-- not just its head TVar -- because the stdlib distinguishes
-- @IndexLike Int@ from @IndexLike (?Int)@, both of which would
-- collapse to @TVar Int@ under 'extractKey'.
findIndexLikeToIndex :: Lang -> TypeU -> MorlocMonad (Maybe Source)
findIndexLikeToIndex lang inputType = do
  sigmap <- MM.gets stateTypeclasses
  case Map.lookup (EV "__to_index__") sigmap of
    Nothing -> return Nothing
    Just inst -> return $ listToMaybe
      [ src
      | TermTypes (Just et) cs _ <- instanceTerms inst
      , termInputType et == Just inputType
      , (_, Idx _ src) <- cs
      , srcLang src == lang
      ]
  where
    termInputType :: EType -> Maybe TypeU
    termInputType et = case snd (unqualify (etype et)) of
      FunU (firstArg : _) _ -> Just firstArg
      _                     -> Nothing

-- | Look up the @Sliceable@ @__get_slice__@ instance for a container
-- type in language @lang@. The instance method signature is @?Int64
-- -> ?Int64 -> ?Int64 -> f a -> f a@; the container head sits at
-- argument index 3 (the receiver position). Receivers don't need
-- Optional discrimination, so head-only matching ('extractKey') is
-- sufficient.
findSliceableGetSlice :: Lang -> TypeU -> MorlocMonad (Maybe Source)
findSliceableGetSlice lang receiverType =
  findInstanceByArgHead 3 (EV "__get_slice__") lang (extractKey receiverType)

-- | Look up the @Indexable@ @__access_index__@ instance for a
-- container type in language @lang@. The instance method signature is
-- @?Int64 -> f a -> a@; the container head sits at argument index 1.
findIndexableAccessIndex :: Lang -> TypeU -> MorlocMonad (Maybe Source)
findIndexableAccessIndex lang receiverType =
  findInstanceByArgHead 1 (EV "__access_index__") lang (extractKey receiverType)

-- | Look up the @SliceableDim@ @__get_slice_dim__@ instance for a
-- Nat-parameterized container. Argument position of the receiver is
-- the same as @Sliceable@ (3): @?Int64 -> ?Int64 -> ?Int64 -> f n a ->
-- f m a@.
findSliceableDimGetSliceDim :: Lang -> TypeU -> MorlocMonad (Maybe Source)
findSliceableDimGetSliceDim lang receiverType =
  findInstanceByArgHead 3 (EV "__get_slice_dim__") lang (extractKey receiverType)

-- | Look up the @Functor@ @map@ instance for a container type in
-- language @lang@. The instance method signature is @(a -> b) -> f a
-- -> f b@; the container head sits at argument index 1. Used by the
-- @IntrMap@ pool-path lowering to resolve the desugar's implicit map
-- over a bracket-accessor chain.
findFunctorMap :: Lang -> TypeU -> MorlocMonad (Maybe Source)
findFunctorMap lang receiverType =
  findInstanceByArgHead 1 (EV "map") lang (extractKey receiverType)

-- | Shared helper: look up the per-language source binding of a
-- class method whose receiver type head occupies a known argument
-- position. Used by the @Sliceable@ / @Indexable@ / @SliceableDim@ /
-- @Functor@ helpers above.
findInstanceByArgHead :: Int -> EVar -> Lang -> TVar -> MorlocMonad (Maybe Source)
findInstanceByArgHead pos method lang containerTv = do
  sigmap <- MM.gets stateTypeclasses
  case Map.lookup method sigmap of
    Nothing -> return Nothing
    Just inst -> return $ listToMaybe
      [ src
      | TermTypes (Just et) cs _ <- instanceTerms inst
      , receiverHead et == Just containerTv
      , (_, Idx _ src) <- cs
      , srcLang src == lang
      ]
  where
    receiverHead :: EType -> Maybe TVar
    receiverHead et = case snd (unqualify (etype et)) of
      FunU args _ | length args > pos -> Just (extractKey (args !! pos))
      _                               -> Nothing

-- | The wire type for every slot the slicer / indexer expects: bounds
-- always arrive as @?Int64@ (the return shape of @__to_index__@); a
-- bare @Int64@ never reaches the runtime.
optI64T :: Type
optI64T = OptionalT (VarT (TV "Int64"))

-- | Rewrite an applied @PatternBracketSlice@ into the resolved
-- per-language @Sliceable.__get_slice__@ instance call, with each
-- bound wrapped in its own @IndexLike.__to_index__@ cast. Used by
-- 'expressPolyExpr' on the local-pool path.
expressBracketSlice
  :: Lang
  -> Int                       -- outer manifold index, reused for the synthetic SrcCall PolyExe
  -> [Type]                    -- bracket pattern input types: [startT, stopT, stepT, rcvT]
  -> Type                      -- bracket pattern result type
  -> [PolyExpr]                -- already-expressed args
  -> MorlocMonad PolyExpr
expressBracketSlice callLang midx inputs out xsExpr =
  case (inputs, xsExpr) of
    ([sT, eT, pT, rcvT], [sE, eE, pE, rcvE]) -> do
      -- Try Sliceable first (List, Str, ...). If absent, try
      -- SliceableDim (Vector, future Tensor ...) -- both have the same
      -- slot shape @?Int64 -> ?Int64 -> ?Int64 -> recv -> result@
      -- from the codegen's point of view; only the result type's Nat
      -- dim differs, and the typechecker has already produced the
      -- right result type via bracketSliceResultType.
      getSliceSrc <- requireInstance midx
        (firstJustM
          [ resolveInstanceForType findSliceableGetSlice    callLang midx rcvT
          , resolveInstanceForType findSliceableDimGetSliceDim callLang midx rcvT
          ])
        callLang "Sliceable.__get_slice__ or SliceableDim.__get_slice_dim__" rcvT
      sW <- wrapBoundInToIndex callLang midx sT sE
      eW <- wrapBoundInToIndex callLang midx eT eE
      pW <- wrapBoundInToIndex callLang midx pT pE
      let fT = FunT [optI64T, optI64T, optI64T, rcvT] out
      return $ PolyApp (PolyExe (Idx midx fT) (SrcCallP getSliceSrc)) [sW, eW, pW, rcvE]
    _ -> MM.throwSourcedError midx "PatternBracketSlice expects 4 arguments"

-- | Run a list of @Maybe@-returning monadic lookups in order, returning
-- the first @Just@. Used to fan out a single bracket op across multiple
-- typeclasses (e.g. @Sliceable@ then @SliceableDim@) when the receiver
-- type may have an instance under either.
firstJustM :: Monad m => [m (Maybe a)] -> m (Maybe a)
firstJustM []       = return Nothing
firstJustM (m : ms) = do
  r <- m
  case r of
    Just _  -> return r
    Nothing -> firstJustM ms

-- | Rewrite an applied @PatternBracketIndex@ into the resolved
-- per-language @Indexable.__access_index__@ instance call, with the
-- index wrapped in @IndexLike.__to_index__@. @__access_index__@ takes
-- @?Int64@ to match @__to_index__@'s return shape; the source
-- implementations error on a Null index since an absent index has no
-- semantic meaning at an access position (only at a slice position).
expressBracketIndex
  :: Lang
  -> Int
  -> [Type]                    -- [indexT, rcvT]
  -> Type
  -> [PolyExpr]                -- already-expressed args
  -> MorlocMonad PolyExpr
expressBracketIndex callLang midx inputs out xsExpr =
  case (inputs, xsExpr) of
    ([iT, rcvT], [iE, rcvE]) -> do
      accessSrc <- requireInstance midx
        (resolveInstanceForType findIndexableAccessIndex callLang midx rcvT)
        callLang "Indexable.__access_index__" rcvT
      iW <- wrapBoundInToIndex callLang midx iT iE
      let fT = FunT [optI64T, rcvT] out
      return $ PolyApp (PolyExe (Idx midx fT) (SrcCallP accessSrc)) [iW, rcvE]
    _ -> MM.throwSourcedError midx "PatternBracketIndex expects 2 arguments"

-- | Resolve a typeclass-method instance for a value's type by walking
-- the alias chain. Tests the type's outermost head TVar against the
-- per-TVar lookup; on miss, reduces the type one alias step
-- (via 'TE.reduceType') and retries. Returns the first hit, or
-- 'Nothing' if the chain is exhausted. This is the key mechanism by
-- which @type Array a = List a@ inherits @instance Indexable List@:
-- the call site type @Array Int@ misses on @Array@, reduces to
-- @List Int@, then hits.
resolveInstanceForType
  :: (Lang -> TypeU -> MorlocMonad (Maybe Source))
  -> Lang
  -> Int                  -- midx, used to recover the source scope
  -> Type
  -> MorlocMonad (Maybe Source)
resolveInstanceForType perTypeLookup lang midx originalType = do
  scope <- MM.getGeneralScope midx
  go scope (type2typeu originalType)
  where
    go scope t = do
      mSrc <- perTypeLookup lang t
      case mSrc of
        Just src -> return (Just src)
        Nothing -> case TE.reduceType scope t of
          Just t' | t' /= t -> go scope t'
          _ -> return Nothing

requireInstance
  :: Int
  -> MorlocMonad (Maybe Source)
  -> Lang
  -> Text
  -> Type
  -> MorlocMonad Source
requireInstance midx lookup_ lang methodLabel t = do
  mSrc <- lookup_
  case mSrc of
    Just src -> return src
    Nothing -> MM.throwSourcedError midx $
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
wrapBoundInToIndex
  :: Lang
  -> Int
  -> Type
  -> PolyExpr
  -> MorlocMonad PolyExpr
wrapBoundInToIndex lang midx boundType expr = do
  mSrc <- resolveInstanceForType findIndexLikeToIndex lang midx boundType
  case mSrc of
    Just src ->
      let wrapFT = FunT [boundType] optI64T
      in return $ PolyApp (PolyExe (Idx midx wrapFT) (SrcCallP src)) [expr]
    Nothing -> MM.throwSourcedError midx $
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
renderConcreteForm :: TypeF -> Text
renderConcreteForm (VarF (FV _ cv)) = unCVar cv
renderConcreteForm (AppF (VarF (FV _ cv)) args) =
  let (typeArgs, kindCount) = partitionKindArgsF args
  in Macro.expandMacro (unCVar cv) (map renderConcreteForm typeArgs) kindCount
renderConcreteForm (NamF _ (FV _ cv) _ _) = unCVar cv
renderConcreteForm (NatLitF n) = T.pack (show n)
renderConcreteForm NatVoidF = "_"
renderConcreteForm (StrLitF s) = s
renderConcreteForm StrVoidF = "_"
renderConcreteForm (RecF (FV _ cv)) = unCVar cv
renderConcreteForm (OptionalF t) = "?" <> renderConcreteForm t
renderConcreteForm (EffectF _ t) = renderConcreteForm t
renderConcreteForm (FunF ts t) =
  "(" <> T.intercalate "," (map renderConcreteForm ts) <> ")->" <> renderConcreteForm t
renderConcreteForm (UnkF (FV _ cv)) = unCVar cv
renderConcreteForm (AppF h args) =
  let (typeArgs, _) = partitionKindArgsF args
  in renderConcreteForm h <> "<" <> T.intercalate "," (map renderConcreteForm typeArgs) <> ">"

-- | Decide whether a literal at native type with outer TVar @nativeTv@
-- and wire-form outer TVar @wireTv@ needs a @Packable@ wrap in language
-- @lang@. A wrap is needed iff the native's per-language form differs
-- from the wire form's. If a wrap is structurally needed but no
-- @Packable@ instance exists for this language, throws a sourced
-- error.
maybePackableWrap :: Int -> Lang -> Type -> Type -> MorlocMonad (Maybe Source)
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
      let nativeRendered = renderConcreteForm nativeTF
          wireRendered   = renderConcreteForm wireTF
      if nativeRendered == wireRendered
        then return Nothing
        else do
          maySrc <- findPackablePack lang nativeTv
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
  [Type] ->      -- user's annotated args (empty for bare-VarT case)
  [PolyExpr] ->  -- already-elaborated children
  MorlocMonad PolyExpr
dispatchListLit midx cidx lang userT userTV userArgs xs' = do
  scope <- MM.getGeneralScope midx
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
          let inner = PolyList (Idx cidx wireTV) (map (Idx cidx) wireArgs) xs'
              funT = FunT [AppT (VarT wireTV) wireArgs] userT
          in return $ PolyApp (PolyExe (Idx midx funT) (SrcCallP src)) [inner]
    _ -> MM.throwSourcedError midx "Expected a list type"

-- | Emit a primitive literal at user-facing type @userT@ (whose outer
-- TVar is @userTV@), or wrap it with the @Packable@ @unpack@ source
-- for @userTV@ when the native's per-language form diverges from the
-- wire form's. @mkLit@ builds the natural literal given the TVar at
-- which it should be tagged (user's TVar when emitting naturally,
-- wire-form's TVar when emitting inside a wrap).
dispatchPrimLit ::
  Int ->                  -- midx
  Lang ->
  Type ->                 -- user-facing type (e.g. VarT Speed)
  TVar ->                 -- outer TVar of userT
  (TVar -> PolyExpr) ->   -- natural literal constructor parameterised by tag TVar
  MorlocMonad PolyExpr
dispatchPrimLit midx lang userT userTV mkLit = do
  scope <- MM.getGeneralScope midx
  let wireTU = TE.wireParentRoot scope (type2typeu userT)
      wireTV = extractKey wireTU
  maySrc <- maybePackableWrap midx lang userT (typeOf wireTU)
  case maySrc of
    Nothing -> return (mkLit userTV)
    Just src ->
      let funT = FunT [typeOf wireTU] userT
      in return $ PolyApp (PolyExe (Idx midx funT) (SrcCallP src)) [mkLit wireTV]

expressPolyExpr ::
  (Lang -> Lang -> Maybe RemoteForm) ->
  Lang ->
  Indexed Type ->
  AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) ->
  MorlocMonad PolyExpr
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
                  funExpr@(AnnoS (Idx gidxCall (FunT callInputTypes _)) (Idx _ callLang, _) _)
                  xs
                )
            )
        )
    )
    | isLocal = do
        propagateScope gidxCall midx
        let nContextArgs = length appArgs - length vs
            contextArgs = map unvalue (take nContextArgs appArgs)

            typedLambdaArgs =
              fromJust $
                safeZipWith
                  (\(Arg i _) t -> Arg i (Just t))
                  (drop nContextArgs lamArgs)
                  lamInputTypes

        xs' <- fromJust <$> safeZipWithM (expressPolyExprWrap appLang) (zipWith mkIdx xs callInputTypes) xs

        call <- expressPolyApp parentLang funExpr xs'

        return
          . PolyManifold parentLang midx (ManifoldPart contextArgs typedLambdaArgs)
          $ call
    | not isLocal = do
        propagateScope gidxCall midx

        xsInfo <- mapM partialExpress xs

        let xs' = map (\(_, _, e) -> e) xsInfo
            callArgs = unique (concatMap (\(rs, _, _) -> rs) xsInfo)
            args = [i | Arg i _ <- appArgs]
            allParentArgs = args <> [i | (_, Just (i, _), _) <- xsInfo]
            lets = [PolyLet i e | (_, Just (i, e), _) <- xsInfo]
            passedParentArgs = concat [[r | r <- allParentArgs, r == i] | i <- callArgs]
            nContextArgs = length appArgs - length vs

            lambdaTypeMap = zip vs (map (Idx cidxLam) lamInputTypes)
            boundVars =
              [ PolyBndVar (maybe (A parentLang) C (lookup v lambdaTypeMap)) i
              | Arg i v <- appArgs
              ]
            untypedContextArgs = map unvalue $ take nContextArgs appArgs
            typedPassedArgs = fromJust $ safeZipWith (\(Arg i _) t -> Arg i (Just t)) (drop nContextArgs lamArgs) lamInputTypes

            localForm = ManifoldPart untypedContextArgs typedPassedArgs

            foreignForm = ManifoldFull [Arg i None | i <- passedParentArgs]

        call <- expressPolyApp parentLang funExpr xs'

        return
          . PolyManifold parentLang midx localForm
          . chain lets
          . PolyReturn
          . PolyApp
            ( PolyRemoteInterface callLang (Idx cidxLam lamOutType) passedParentArgs (fromJust remote)
                . PolyManifold callLang midx foreignForm
                $ call
            )
          $ boundVars
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
expressPolyExpr _ _ _ (AnnoS lambdaType@(Idx midx _) (Idx _ lang, manifoldArguments) (LamS vs body)) = do
  body' <- expressPolyExprWrap lang lambdaType body

  inputTypes <- case val lambdaType of
    (FunT ts _) -> return ts
    _ -> return []

  let contextArguments = map unvalue $ take (length manifoldArguments - length vs) manifoldArguments
      boundArguments = map unvalue $ drop (length contextArguments) manifoldArguments
      typeBoundArguments = fromJust $ safeZipWith (\t (Arg i _) -> Arg i (Just t)) inputTypes boundArguments

  return
    . PolyManifold lang midx (ManifoldPart contextArguments typeBoundArguments)
    . PolyReturn
    $ body'
-- Inline source call: skip PolyManifold, emit as direct subexpression
expressPolyExpr
  findRemote
  parentLang
  _
  ( AnnoS
      (Idx midx _)
      _
      (AppS f@(AnnoS (Idx gidxCall (FunT inputs _)) (Idx cidxCall callLang, _) (ExeS (SrcCall src))) xs)
    )
    | srcInline src && isLocal = do
        propagateScope gidxCall midx
        xsExpr <- zipWithM (expressPolyArg callLang) (map (Idx cidxCall) inputs) xs
        expressPolyApp parentLang f xsExpr >>= stripPolyReturn
    where
      remote = findRemote parentLang callLang
      isLocal = isNothing remote
      stripPolyReturn (PolyReturn e) = return e
      stripPolyReturn e = return e
-- Inline pattern call: skip PolyManifold. PatCallP (record/struct
-- accessors, format patterns) renders as a direct expression in the
-- target language (e.g. `x.field` for .isFile). Wrapping in
-- PolyManifold creates a nested manifold whose body's `return` leaks
-- into the parent's scope when the manifold gets inlined by codegen.
-- Treating PatCallP like srcInline matches its semantics - it's
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
  ( AnnoS
      (Idx midx _)
      (_, outerArgs)
      (AppS f@(AnnoS (Idx gidxCall (FunT inputs out)) (Idx cidxCall callLang, _) (ExeS (PatCall pat))) xs)
    )
    | isLocal = do
        propagateScope gidxCall midx
        xsExpr <- zipWithM (expressPolyArg callLang) (map (Idx cidxCall) inputs) xs
        case pat of
          PatternBracketSlice ->
            expressBracketSlice callLang midx inputs out xsExpr
          PatternBracketIndex ->
            expressBracketIndex callLang midx inputs out xsExpr
          _ ->
            expressPolyApp parentLang f xsExpr >>= stripPolyReturn
    -- Cross-pool bracket: receiver lives in one pool, surrounding
    -- expression in another. Without this branch, the not-isLocal
    -- generic AppS handler at the next clause would call
    -- expressPolyApp, which builds a @PatCallP@; the per-language
    -- Generic.hs::genericEvalPattern fallback then emits raw bracket
    -- syntax (e.g. R's @m[[(i) + 1]]@) and skips the per-bound
    -- @__to_index__@ cast plus the per-receiver @__access_index__@ /
    -- @__get_slice__@ dispatch. That fallback is also semantically
    -- broken for negative indices in R (@m[[-1]]@ means "all except
    -- first", and @m[[0]]@ errors) and for mixed-width indices in
    -- C++. The fix is to run the same SrcCall rewrite as the
    -- isLocal branch, then wrap it in the cross-pool PolyManifold /
    -- PolyRemoteInterface structure used by the generic not-isLocal
    -- handler.
    | not isLocal = do
        propagateScope gidxCall midx
        let idxInputTypes = zipWith mkIdx xs inputs
        mayXs <- safeZipWithM (expressPolyArg callLang) idxInputTypes xs
        let xsExpr = fromJust mayXs
        -- The inner callLang PolyManifold body needs PolyReturn to
        -- emit a 'return' statement in the pool source; without it C++
        -- generates a non-void function with no return and hits a
        -- syntax / control-reaches-end error. @expressBracketSlice@ /
        -- @expressBracketIndex@ return a bare @PolyApp@ (suited to the
        -- inline isLocal path that strips PolyReturn anyway), so we
        -- wrap them here. @expressPolyApp@ already returns
        -- PolyReturn-wrapped on the non-bracket fallback path.
        bracketCall <- case pat of
          PatternBracketSlice ->
            PolyReturn <$> expressBracketSlice callLang midx inputs out xsExpr
          PatternBracketIndex ->
            PolyReturn <$> expressBracketIndex callLang midx inputs out xsExpr
          _ ->
            expressPolyApp parentLang f xsExpr
        return
          . PolyManifold parentLang midx (ManifoldFull (map unvalue outerArgs))
          . PolyReturn
          . PolyApp
            ( PolyRemoteInterface callLang pc [] (fromJust remote)
                . PolyManifold callLang midx (ManifoldFull (map unvalue outerArgs))
                $ bracketCall
            )
          $ [PolyBndVar (A parentLang) i | Arg i _ <- outerArgs]
    where
      remote = findRemote parentLang callLang
      isLocal = isNothing remote
      stripPolyReturn (PolyReturn e) = return e
      stripPolyReturn e = return e
expressPolyExpr
  findRemote
  parentLang
  pc
  ( AnnoS
      (Idx midx _)
      (_, args)
      (AppS f@(AnnoS (Idx gidxCall (FunT inputs _)) (Idx cidxCall callLang, _) _) xs)
    )
    | isLocal = do
        propagateScope gidxCall midx
        xsExpr <- zipWithM (expressPolyArg callLang) (map (Idx cidxCall) inputs) xs

        func <- expressPolyApp parentLang f xsExpr
        return
          . PolyManifold callLang midx (ManifoldFull (map unvalue args))
          $ func
    | not isLocal = do
        propagateScope gidxCall midx
        let idxInputTypes = zipWith mkIdx xs inputs
        mayXs <- safeZipWithM (expressPolyArg callLang) idxInputTypes xs
        func <- expressPolyApp parentLang f (fromJust mayXs)
        return
          . PolyManifold parentLang midx (ManifoldFull (map unvalue args))
          . PolyReturn
          . PolyApp
            ( PolyRemoteInterface callLang pc [] (fromJust remote)
                . PolyManifold callLang midx (ManifoldFull (map unvalue args))
                $ func
            )
          $ [PolyBndVar (A parentLang) i | Arg i _ <- args]
    where
      remote = findRemote parentLang callLang
      isLocal = isNothing remote
expressPolyExpr
  findRemote
  parentLang
  (val -> FunT pinputs poutput)
  e@(AnnoS (Idx midx (FunT callInputs _)) (Idx cidx callLang, _) _)
    | isLocal = do
        ids <- MM.takeFromCounter (length callInputs)
        let lambdaVals = bindVarIds ids (map (C . Idx cidx) callInputs)
            lambdaTypedArgs = fromJust $ safeZipWith annotate ids (map Just callInputs)
        retapp <- expressPolyApp parentLang e lambdaVals
        return
          . PolyManifold callLang midx (ManifoldPass lambdaTypedArgs)
          $ retapp
    | otherwise = do
        ids <- MM.takeFromCounter (length callInputs)
        let lambdaArgs = [Arg i None | i <- ids]
            lambdaTypedArgs = map (`Arg` Nothing) ids
            callVals = bindVarIds ids (map (C . Idx cidx) callInputs)
        retapp <- expressPolyApp callLang e callVals
        return
          . PolyManifold parentLang midx (ManifoldPass lambdaTypedArgs)
          . PolyReturn
          . PolyApp
            ( PolyRemoteInterface callLang (Idx cidx poutput) (map ann lambdaArgs) (fromJust remote)
                . PolyManifold callLang midx (ManifoldFull lambdaArgs)
                $ retapp
            )
          $ fromJust
          $ safeZipWith (PolyBndVar . C) (map (Idx cidx) pinputs) (map ann lambdaArgs)
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
        letId = case [j | Arg j v' <- bodyArgs, v' == v] of
          [j] -> j
          _ -> cidx
    let e1Type = case e1 of AnnoS (Idx _ t) _ _ -> mkIdx e1 t
    -- Express children under the LetS's OWN language (from Realize), not the
    -- caller's. expressContainer wraps in a cross-language manifold when the
    -- chain's lang differs from parentLang. This fuses sequential
    -- same-language calls (especially do-blocks) into one manifold.
    e1' <- expressPolyExprWrap lang e1Type e1
    e2' <- expressPolyExprWrap lang pc e2
    let e = PolyLet letId e1' e2'
    return $ expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e
expressPolyExpr _ _ _ (AnnoS (Idx midx t@(VarT v)) (Idx cidx lang, _) (RealS _ x)) =
  dispatchPrimLit midx lang t v (\tv -> PolyReal (Idx cidx tv) x)
expressPolyExpr _ _ _ (AnnoS (Idx midx t@(VarT v)) (Idx cidx lang, _) (IntS _ x)) =
  dispatchPrimLit midx lang t v (\tv -> PolyInt (Idx cidx tv) x)
expressPolyExpr _ _ _ (AnnoS (Idx midx t@(VarT v)) (Idx cidx lang, _) (LogS x)) =
  dispatchPrimLit midx lang t v (\tv -> PolyLog (Idx cidx tv) x)
expressPolyExpr _ _ _ (AnnoS (Idx midx t@(VarT v)) (Idx cidx lang, _) (StrS x)) =
  dispatchPrimLit midx lang t v (\tv -> PolyStr (Idx cidx tv) x)
expressPolyExpr _ _ _ (AnnoS (Idx midx t@(VarT v)) (Idx cidx lang, _) UniS) =
  dispatchPrimLit midx lang t v (\tv -> PolyNull (Idx cidx (VarT tv)))
-- Null carries the full type it stands in for. Earlier this was just
-- a @TVar@ extracted via @innerNullVar@, but a bare constructor name
-- loses the arg list of parameterised aliases. For
-- @Null :: ?(BTree Int)@, peeling Optional and discarding the @[Int]@
-- yielded just @BTree@, whose downstream @inferConcreteVar@ chain
-- resolved to an @FV BTree "tuple"@ FVar that the typeFof of the
-- enclosing tuple/record/if surfaced as a bare-args VarF -- which
-- @makeSerialAST@'s @dispatchVarF@ then could not interpret.
--
-- Now @innerNullType@ returns the full inner @Type@ (still peeling
-- Optional wrappers for nested @?(?T)@ idempotence). Downstream
-- consumers run @inferConcreteType@ on the stored type and surface
-- the proper @TypeF@ at typeFof time.
expressPolyExpr _ _ _ (AnnoS (Idx midx t) (Idx cidx _, _) NullS) =
  case canonNullType t of
    Just t' -> return $ PolyNull (Idx cidx t')
    Nothing -> MM.throwSourcedError midx $
      "Cannot infer type for Null literal of type"
      <+> dquotes (pretty t) <> "."
      <+> "This usually means an unsolved generic term escaped typechecking."
  where
    -- Require the outermost to be @OptionalT@ (which the typecheck
    -- always supplies for @NullS@ via @synthE NullS@'s
    -- @OptionalU v@). Then collapse nested Optionals (idempotence:
    -- ?(?T) == ?T) and require a concrete head underneath. The stored
    -- type is the canonical @?T@ where @T@ is @VarT@ or
    -- @AppT (VarT _) _@, so downstream typeFof presents a single
    -- Optional layer matching the slot's expected shape.
    canonNullType (OptionalT inner) = OptionalT <$> peelOpt inner
    canonNullType _                 = Nothing
    peelOpt (OptionalT inner)       = peelOpt inner
    peelOpt t'@(VarT _)             = Just t'
    peelOpt t'@(AppT (VarT _) _)    = Just t'
    peelOpt _                       = Nothing
-- A list literal at type `Foo a1 ... an` has exactly one element-type arg
-- (the rest, if any, are Nat-kinded phantom dims, e.g. for Vector n a).
-- Extract the single non-Nat arg for typing children, but PRESERVE the
-- full args list in PolyList so phantom dims survive into language code
-- generators (Nat positions render as mempty in macro expansion).
expressPolyExpr _ parentLang pc (AnnoS (Idx midx userT@(AppT (VarT v) ts)) (Idx cidx lang, args) (LstS xs))
  | [tElem] <- filter (not . isKindTypeT) ts = do
      xs' <- mapM (\x -> expressPolyExprWrap lang (mkIdx x tElem) x) xs
      e <- dispatchListLit midx cidx lang userT v ts xs'
      return $ expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e
-- LstS at a bare @VarT v@ where @v@ is a newtype (or transparent alias)
-- whose wire-parent root is list-shaped. Use the wire-parent's element
-- type for child typing and let 'dispatchListLit' decide whether to
-- emit naturally (e.g. @newtype Bytes = List UInt8@ with no per-language
-- override emits a bare list) or wrap with the @ListLike v@ converter.
expressPolyExpr _ parentLang pc (AnnoS (Idx midx userT@(VarT v)) (Idx cidx lang, args) (LstS xs)) = do
  scope <- MM.getGeneralScope midx
  let wireT = typeOf (TE.wireParentRoot scope (type2typeu userT))
  case wireT of
    AppT (VarT _) wireArgs
      | [elemT] <- filter (not . isKindTypeT) wireArgs -> do
          xs' <- mapM (\x -> expressPolyExprWrap lang (mkIdx x elemT) x) xs
          e <- dispatchListLit midx cidx lang userT v [] xs'
          return $ expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e
    _ -> MM.throwSourcedError midx "Expected a list type"
-- A list literal at a multi-non-Nat-arg @AppT@ whose @Packable@ wire form
-- is list-shaped. E.g. @[("a",1)] :: Map Str Int@ with @instance
-- Packable (Map a b) (List (Tuple2 a b))@. The Packable wire form is
-- substituted, the children are typed against the wire form's element
-- type, and the result is wrapped with the @Packable@ @unpack@ source.
-- This is the codegen counterpart to the typecheck-side tier-2 lookup
-- in 'Morloc.Frontend.Typecheck.checkE'.
expressPolyExpr _ parentLang pc (AnnoS (Idx midx userT@(AppT (VarT v) _)) (Idx cidx lang, args) (LstS xs)) = do
  mayWire <- findPackableWireForm (type2typeu userT)
  case mayWire of
    Just wireTU
      | AppT (VarT wireV) wireArgs <- typeOf wireTU
      , [elemT] <- filter (not . isKindTypeT) wireArgs -> do
          xs' <- mapM (\x -> expressPolyExprWrap lang (mkIdx x elemT) x) xs
          maySrc <- findPackablePack lang v
          case maySrc of
            Just src ->
              let inner = PolyList (Idx cidx wireV) (map (Idx cidx) wireArgs) xs'
                  funT = FunT [AppT (VarT wireV) wireArgs] userT
              in return $ expressContainer pc (Idx midx parentLang) (Idx cidx lang) args
                   (PolyApp (PolyExe (Idx midx funT) (SrcCallP src)) [inner])
            Nothing -> MM.throwSourcedError midx $
              "Packable wire form for"
              <+> squotes (pretty v)
              <+> "is list-shaped but no 'unpack' source is declared for"
              <+> pretty lang <> "."
    _ -> tryReduceLstS userT
  where
    tryReduceLstS t = do
      scope <- MM.getGeneralScope midx
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
  scope <- MM.getGeneralScope midx
  case reduceType scope t of
    Just t' -> expressPolyExprWrap pl pc (AnnoS (Idx midx t') c e)
    Nothing -> MM.throwSourcedError midx "Expected a list type"
-- Same guard as expressCore: only treat the head as a tuple when it IS the
-- TupleN constructor. A bare @length ts == length xs@ match would catch
-- user aliases like @FixedPair (n :: Nat) a@ whose type args (NatLit n,
-- a) are NOT the slot types; the cross-language wrap inside
-- expressContainer would then use a Nat literal as the foreign call's
-- parent type and surface downstream as @makeSerialAST' error on type:
-- NatLitF n@. Falling through to the reduce-and-retry below expands the
-- alias to its body's tuple constructor.
expressPolyExpr _ parentLang pc (AnnoS (Idx midx (AppT (VarT v) ts)) (Idx cidx lang, args) (TupS xs))
  | v == BT.tuple (length xs)
  , length ts == length xs = do
      let idxTs = zipWith mkIdx xs ts
      xs' <- fromJust <$> safeZipWithM (expressPolyExprWrap lang) idxTs xs
      let e = PolyTuple (Idx cidx v) (fromJust $ safeZip idxTs xs')
      return $ expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e
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
  scope <- MM.getGeneralScope midx
  case TE.expandWireParent scope (type2typeu t) of
    Just t' -> do
      let bodyT = typeOf t'
      maySrc <- findPackablePack lang (extractKey (type2typeu t))
      case maySrc of
        Just src -> do
          inner <- expressPolyExprWrap lang (Idx cidx bodyT) (AnnoS (Idx midx bodyT) c e)
          let funT = FunT [bodyT] t
          return $ PolyApp (PolyExe (Idx midx funT) (SrcCallP src)) [inner]
        Nothing -> expressPolyExprWrap pl pc (AnnoS (Idx midx bodyT) c e)
    Nothing -> MM.throwSourcedError midx "Expected a tuple type"
expressPolyExpr _ parentLang pc (AnnoS (Idx midx (NamT o v ps rs)) (Idx cidx lang, args) (NamS entries)) = do
  -- C3 invariant: see comment at expressCore NamT above.
  assertRecordKeyOrder midx entries rs
  let tsIdx = zipWith mkIdx (map snd entries) (map snd rs)
  xs' <- fromJust <$> safeZipWithM (expressPolyExprWrap lang) tsIdx (map snd entries)
  let e = PolyRecord o (Idx cidx v) (map (Idx cidx) ps) (zip (map fst rs) (zip tsIdx xs'))
  return $ expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e
expressPolyExpr _ pl pc (AnnoS (Idx i t) c e@(NamS _)) = do
  scope <- MM.getGeneralScope i
  case reduceType scope t of
    (Just t') -> expressPolyExprWrap pl pc (AnnoS (Idx i t') c e)
    Nothing -> error "Expected a record type"
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
  return $ expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e
expressPolyExpr _ parentLang pc (AnnoS (Idx midx t) (Idx cidx lang, args) (DoBlockS x)) = do
  -- The inner expression has the unwrapped type (without EffectT).
  -- Passing EffectT through would cause cross-language calls to generate
  -- effect-wrapped return types for pure functions.
  let innerT = case t of EffectT _ inner -> inner; _ -> t
  x' <- expressPolyExprWrap lang (mkIdx x innerT) x
  let e = PolyDoBlock (Idx cidx t) x'
  return $ expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e
expressPolyExpr _ parentLang _ (AnnoS (Idx _ t) (Idx cidx _, _) (CoerceS coercion x)) = do
  let innerType = unapplyCoercion coercion t
  x' <- expressPolyExprWrap parentLang (Idx cidx innerType) x
  return $ PolyCoerce coercion (Idx cidx t) x'
expressPolyExpr _ parentLang _ (AnnoS (Idx _ t) (Idx cidx _lang, _) (EvalS x)) = do
  -- Always use pushForceIntoRemote: if the inner expression contains a
  -- PolyRemoteInterface (cross-language call), it strips EffectT Set.empty so the remote
  -- pool forces the thunk and serializes the concrete result. If no
  -- PolyRemoteInterface is found (same-language), it falls back to PolyEval.
  -- We cannot rely on parentLang /= lang because Realize.hs assigns both to
  -- the same language when the EvalS node lives in a same-language context,
  -- even if the inner expression calls into a foreign language.
  x' <- expressPolyExprWrap parentLang (Idx cidx t) x
  return $ pushForceIntoRemote (Idx cidx t) x'
-- IntrMap (the desugar-emitted implicit map for bracket-accessor
-- chains): for the pool path, resolve the per-language
-- @Functor.map@ instance for the receiver's container type and
-- replace the intrinsic with a direct SrcCall. Walks the alias chain
-- so e.g. an @Array a = List a@ receiver picks up @instance Functor
-- List@. Missing instance is a sourced codegen error citing the type
-- and the language.
expressPolyExpr _ parentLang pc (AnnoS (Idx midx t) (Idx cidx lang, args) (IntrinsicS IntrMap [funcE, listE])) = do
  let AnnoS (Idx _ listT) _ _ = listE
      AnnoS (Idx _ funcT) _ _ = funcE
  mapSrc <- requireInstance midx
    (resolveInstanceForType findFunctorMap lang midx listT)
    lang "Functor.map" listT
  funcExpr <- expressPolyExprWrap lang (mkIdx funcE funcT) funcE
  listExpr <- expressPolyExprWrap lang (mkIdx listE listT) listE
  let mapFunT = FunT [funcT, listT] t
      e = PolyApp (PolyExe (Idx midx mapFunT) (SrcCallP mapSrc)) [funcExpr, listExpr]
  return $ expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e
expressPolyExpr _ parentLang pc (AnnoS (Idx midx t) (Idx cidx lang, args) (IntrinsicS intr xs)) = do
  xs' <- mapM (\x@(AnnoS (Idx xi xt) _ _) -> expressPolyExprWrap lang (Idx xi xt) x) xs
  let e = PolyIntrinsic (Idx cidx t) intr xs'
  return $ expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e

-- Nullary source/pattern call (e.g., clockResNs :: {Int})
expressPolyExpr
  findRemote
  parentLang
  pc
  f@(AnnoS (Idx midx _) (Idx _ callLang, args) (ExeS _))
    | isLocal = do
        call <- expressPolyApp parentLang f []
        return
          . PolyManifold callLang midx (ManifoldFull (map unvalue args))
          $ call
    | otherwise = do
        call <- expressPolyApp callLang f []
        return
          . PolyManifold parentLang midx (ManifoldFull (map unvalue args))
          . PolyReturn
          . PolyApp
            ( PolyRemoteInterface callLang pc [] (fromJust remote)
                . PolyManifold callLang midx (ManifoldFull (map unvalue args))
                $ call
            )
          $ [PolyBndVar (A parentLang) i | Arg i _ <- args]
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

expressPolyApp ::
  Lang ->
  AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) ->
  [PolyExpr] ->
  MorlocMonad PolyExpr
expressPolyApp _ (AnnoS g _ (ExeS (SrcCall src))) xs =
  return . PolyReturn $ PolyApp (PolyExe g (SrcCallP src)) xs
expressPolyApp _ (AnnoS g _ (ExeS (PatCall pat))) xs =
  return . PolyReturn $ PolyApp (PolyExe g (PatCallP pat)) xs
expressPolyApp lang f@(AnnoS g@(Idx i _) _ (AppS _ _)) es = do
  fe <- expressPolyExprWrap lang g f
  return
    . PolyLet i fe
    . PolyReturn
    $ PolyApp (PolyLetVar g i) es
expressPolyApp _ (AnnoS g (_, args) (BndS v)) xs = do
  case [j | (Arg j u) <- args, u == v] of
    [j] -> return . PolyReturn $ PolyApp (PolyExe g (LocalCallP j)) xs
    _ -> error "Unreachable? BndS value should have been wired uniquely to args previously"
expressPolyApp parentLang (AnnoS (Idx i t) _ (CallS v)) xs = do
  (mid, crossLang) <- lookupRecursiveTarget parentLang v
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
    tagExpr (LstS _) = "LstS"
    tagExpr (TupS _) = "TupS"
    tagExpr (NamS _) = "NamS"
    tagExpr (RealS _ _) = "RealS"
    tagExpr (IntS _ _) = "IntS"
    tagExpr (LogS _) = "LogS"
    tagExpr (StrS _) = "StrS"
    tagExpr (DoBlockS _) = "DoBlockS"
    tagExpr (EvalS _) = "EvalS"
    tagExpr (CoerceS _ _) = "CoerceS"
    tagExpr (IfS _ _ _) = "IfS"
    tagExpr (LetS v _ _) = "LetS" <+> pretty v
    tagExpr (VarS v _) = "VarS" <+> pretty v
    tagExpr (IntrinsicS _ _) = "IntrinsicS"

expressContainer ::
  Indexed Type -> Indexed Lang -> Indexed Lang -> [Arg EVar] -> PolyExpr -> PolyExpr
expressContainer pc (Idx midx parentLang) (Idx _ lang) args e
  | parentLang /= lang =
      PolyApp
        ( PolyRemoteInterface lang pc [i | Arg i _ <- args] ForeignCall
            . PolyManifold lang midx (ManifoldFull (map unvalue args))
            $ wrapReturn e
        )
        $ [PolyBndVar (A parentLang) i | Arg i _ <- args]
  | otherwise = e
  where
    -- Push PolyReturn through PolyLet to the tail. Skip wrap if the tail
    -- is already a PolyReturn or a PolyManifold (whose own body produces a
    -- return when MonoManifold is unwrapped during serialize). Avoids
    -- `return(return(...))` in generated pools.
    wrapReturn (PolyLet i e1 e2) = PolyLet i e1 (wrapReturn e2)
    wrapReturn r@(PolyReturn _) = r
    wrapReturn r@(PolyManifold _ _ _ _) = r
    wrapReturn x = PolyReturn x

unvalue :: Arg a -> Arg None
unvalue (Arg i _) = Arg i None

{- | Handle cross-language force by stripping EffectT from the callee's function
return type. The source function actually returns the unwrapped type; the
EffectT wrapper is a type-system abstraction. By removing it, Common.hs won't
auto-wrap in DoBlockN, so the raw value is serialized directly.
If no PolyRemoteInterface is found, falls back to wrapping in PolyEval.
-}
pushForceIntoRemote :: Indexed Type -> PolyExpr -> PolyExpr
pushForceIntoRemote t = go
  where
    go (PolyManifold l m f e) = PolyManifold l m f (go e)
    go (PolyReturn e) = PolyReturn (go e)
    go (PolyLet i e1 e2) = PolyLet i e1 (go e2)
    go (PolyApp (PolyRemoteInterface lang _ args remote callee) xs) =
      PolyApp (PolyRemoteInterface lang t args remote (stripThunkReturn callee)) xs
    go e = PolyEval t e -- fallback for local expressions

    -- Strip EffectT from the function's return type inside the callee manifold
    stripThunkReturn (PolyManifold l m f body) = PolyManifold l m f (stripInBody body)
    stripThunkReturn e = stripInBody e

    stripInBody (PolyReturn e) = PolyReturn (stripInExe e)
    stripInBody (PolyLet i e1 e2) = PolyLet i e1 (stripInBody e2)
    stripInBody e = stripInExe e

    stripInExe (PolyApp (PolyExe (Idx gidx (FunT inputs (EffectT _ out))) exe) xs) =
      PolyApp (PolyExe (Idx gidx (FunT inputs out)) exe) xs
    stripInExe (PolyApp (PolyExe (Idx gidx (EffectT _ out)) exe) xs) =
      PolyApp (PolyExe (Idx gidx out) exe) xs
    stripInExe e = e

-- | Resolve a function name to its manifold ID and determine if the call is cross-language.
-- Returns (manifold ID, Nothing) for same-pool calls, (manifold ID, Just targetLang) for foreign calls.
-- Searches all manifolds in stateName, not just exports, to support non-exported recursive helpers.
lookupRecursiveTarget :: Lang -> EVar -> MorlocMonad (Int, Maybe Lang)
lookupRecursiveTarget parentLang v = do
  nameMap <- MM.gets stateName
  langMap <- MM.gets stateManifoldLang
  -- Filter to concrete manifolds only (those in langMap) to avoid picking up
  -- general/polymorphic indices that don't have serial manifold definitions
  let reverseMap = Map.fromList [(name, idx) | (idx, name) <- Map.toList nameMap, Map.member idx langMap]
  case Map.lookup v reverseMap of
    (Just mid) -> do
      let crossLang = case Map.lookup mid langMap of
            Just tl | tl /= parentLang -> Just tl
            _ -> Nothing
      return (mid, crossLang)
    Nothing -> MM.throwSystemError $ "Cannot resolve recursive call to" <+> pretty v

bindVarIds :: [Int] -> [Three Lang Type (Indexed Type)] -> [PolyExpr]
bindVarIds [] [] = []
bindVarIds (i : args) (t : types) = PolyBndVar t i : bindVarIds args types
bindVarIds [] ts = error $ "bindVarIds: too few arguments: " <> show ts
bindVarIds _ [] = error "bindVarIds: too few types"
