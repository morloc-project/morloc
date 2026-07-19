{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.CodeGenerator.Serialize
Description : Insert pack\/unpack operations at cross-language call boundaries
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Walks the segmented manifold tree and inserts serialization\/deserialization
calls wherever data crosses a language boundary (interprocess communication).
Uses 'Serial.makeSerialAST' to determine the packing strategy for each type.
The result is the 'SerialManifold' tree consumed by the translators.
-}
module Morloc.CodeGenerator.Serialize
  ( serialize
  ) where

import Data.Text (Text)
import qualified Morloc.BaseTypes as BT
import Morloc.CodeGenerator.EffectBoundary (forceSerializedThunk)
import Morloc.CodeGenerator.Infer
import Morloc.CodeGenerator.Namespace
import qualified Morloc.CodeGenerator.Serial as Serial
import qualified Morloc.Config as MC
import Morloc.Data.Doc
import qualified Morloc.Data.Map as Map
import qualified Morloc.Monad as MM

{- | This step is performed after segmentation, so all terms are in the same
language. Here we need to determine where inputs are (de)serialized and the
serialization states of arguments and variables.
-}
serialize :: MonoHead -> MorlocMonad SerialManifold
serialize (MonoHead lang m0 args0 headForm0 e0) = do
  form0 <- ManifoldFull <$> mapM prepareArg args0

  se1 <- serialExpr m0 e0
  let sm = SerialManifold m0 lang form0 headForm0 se1
  wireSerial lang sm
  where
    inferType = inferConcreteType lang
    inferTypeUniversal = inferConcreteTypeUniversal lang
    inferVar = inferConcreteVar lang

    typemap = makeTypemap m0 e0

    prepareArg ::
      Arg None ->
      MorlocMonad (Arg (Or TypeS TypeF))
    prepareArg (Arg i _) = case Map.lookup i typemap of
      Nothing -> return $ Arg i (L PassthroughS)
      (Just (Right t)) -> do
        t' <- inferType t
        return $ Arg i (L (typeSof t'))
      (Just (Left t)) -> do
        MM.sayVVV "Warning: using universal inference at prepareArg"
        t' <- inferTypeUniversal t
        return $ Arg i (L (typeSof t'))

    contextArg ::
      Int ->
      MorlocMonad (Or TypeS TypeF)
    contextArg i = case Map.lookup i typemap of
      (Just (Right t)) -> do
        t' <- inferType t
        return $ LR (typeSof t') t'
      Nothing -> return $ L PassthroughS
      (Just (Left t)) -> do
        MM.sayVVV "Warning: using universal inference at contextArg"
        t' <- inferTypeUniversal t
        return $ LR (typeSof t') t'

    boundArg :: Int -> MorlocMonad TypeF
    boundArg i = case Map.lookup i typemap of
      (Just (Right t)) -> inferType t
      Nothing -> error "Untyped native arg"
      (Just (Left t)) -> do
        MM.sayVVV "Warning: using universal inference at boundArg"
        inferTypeUniversal t

    serialExpr ::
      Int ->
      MonoExpr ->
      MorlocMonad SerialExpr
    serialExpr currentM orig@(MonoManifold m _ kind inner)
      -- 'Preserved' manifolds carry observability hooks (logging etc.) and
      -- must survive into codegen as real function definitions. Route
      -- through 'nativeExpr' (which preserves the structure as
      -- NativeManifold) and serialize the result.
      --
      -- The 'm /= currentM' guard avoids a self-wrap collision at the
      -- export root: 'expressDefault' produces a 'PolyHead m ...
      -- (PolyManifold m ...)' pair for labeled exports, and the outer
      -- 'SerialManifold m' will already carry the log wrap via
      -- 'lcMakeFunction'. Preserving the inner manifold at the same midx
      -- would emit a second function with native signature, colliding
      -- with the export's serial signature.
      | kind == Preserved && m /= currentM = do
          ne <- nativeExpr m orig
          se <- serializeS "preserved manifold" m (forceSerializedThunk ne)
          -- If the body was 'MonoReturn'-wrapped (standard shape from
          -- 'ensurePolyReturn'), the inner 'ReturnN' lives inside the
          -- NativeManifold function; surface 'ReturnS' here so the
          -- enclosing manifold emits its own 'return' statement.
          case inner of
            MonoReturn _ -> return (ReturnS se)
            _ -> return se
      | otherwise = serialExpr m inner
    serialExpr m (MonoLet i e1 e2) =
      let (m1, e1') = unwrapLetDef m e1
       in case inferState e1 of
            Serialized -> SerialLetS i <$> serialExpr m1 e1' <*> serialExpr m e2
            Unserialized -> do
              ne1 <- nativeExpr m1 e1'
              NativeLetS i ne1 <$> serialExpr m e2
    serialExpr _ (MonoLetVar t i) = do
      t' <- inferType t
      return $ LetVarS (Just t') i
    serialExpr m (MonoReturn e) = ReturnS <$> serialExpr m e
    serialExpr _ (MonoApp (MonoPoolCall t m docs remoteCall contextArgs) es) = do
      contextArgs' <- mapM (typeArg Serialized . ann) contextArgs
      let poolCall' = PoolCall m docs remoteCall contextArgs'
      es' <- mapM (serialArg m) es
      t' <- inferType t
      return $ AppPoolS t' poolCall' es'
    serialExpr m (MonoCacheBody lbl midx args body) =
      lowerCacheBody Serialized m lbl midx args body
    serialExpr m (MonoDebugWrap midx args body) =
      lowerDebugWrap Serialized m midx args body
    serialExpr _ (MonoBndVar (A _) i) = return $ BndVarS Nothing i
    serialExpr _ (MonoBndVar (B _) i) =
      case Map.lookup i typemap of
        (Just (Right t)) -> BndVarS <$> fmap Just (inferType t) <*> pure i
        _ -> return $ BndVarS Nothing i
    serialExpr _ (MonoBndVar (C t) i) = BndVarS <$> fmap Just (inferType t) <*> pure i
    serialExpr m (MonoIf cond thenE elseE) = do
      ne <- nativeExpr m (MonoIf cond thenE elseE)
      serializeS "serialE MonoIf" m (forceSerializedThunk ne)
    -- Thunk-producing intrinsics: convert to native and serialize with the
    -- inner type (strip EffectF) so the wire format matches the forced value.
    serialExpr m (MonoDoBlock _ e) = serialExpr m e
    serialExpr _ (MonoExe _ _) = error "Can represent MonoSrc as SerialExpr"
    serialExpr _ MonoPoolCall {} = error "MonoPoolCall does not map to a SerialExpr"
    serialExpr _ (MonoApp MonoManifold {} _) = error "Illegal?"
    serialExpr m e = nativeExpr m e >>= serializeS "serialE e" m . forceSerializedThunk

    serialArg ::
      Int ->
      MonoExpr ->
      MorlocMonad SerialArg
    serialArg _ e@(MonoManifold m _ _ _) = do
      se <- serialExpr m e
      case se of
        (ManS sm) -> return $ SerialArgManifold sm
        _ -> error "Unreachable?"
    serialArg _ MonoPoolCall {} = error "This step should be unreachable"
    serialArg _ (MonoExe _ _) = error "This step should be unreachable"
    serialArg _ (MonoReturn _) = error "Return should not happen hear (really I should remove this term completely)"
    serialArg m e = SerialArgExpr <$> serialExpr m e

    nativeArg ::
      Int ->
      MonoExpr ->
      MorlocMonad NativeArg
    nativeArg _ e@(MonoManifold m _ _ _) = do
      ne <- nativeExpr m e
      case ne of
        (ManN nm) -> return $ NativeArgManifold nm
        _ -> error "Unreachable?"
    nativeArg _ MonoPoolCall {} = error "This step should be unreachable"
    nativeArg _ (MonoExe _ _) = error "This step should be unreachable"
    nativeArg _ (MonoReturn _) = error "Return should not happen here (really I should remove this term completely)"
    nativeArg m e = NativeArgExpr <$> nativeExpr m e

    nativeExpr ::
      Int ->
      MonoExpr ->
      MorlocMonad NativeExpr
    nativeExpr _ (MonoManifold m form _ e) = do
      ne <- nativeExpr m e
      form' <- abimapM (\i _ -> contextArg i) (\i _ -> boundArg i) form
      return . ManN $ NativeManifold m lang form' ne
    nativeExpr _ MonoPoolCall {} = error "MonoPoolCall does not map to NativeExpr"
    nativeExpr m (MonoLet i e1 e2) =
      let (m1, e1') = unwrapLetDef m e1
       in case inferState e1 of
            Serialized -> do
              ne2 <- nativeExpr m e2
              SerialLetN i <$> serialExpr m1 e1' <*> pure ne2
            Unserialized -> do
              ne1 <- nativeExpr m1 e1'
              ne2 <- nativeExpr m e2
              return $ NativeLetN i ne1 ne2
    nativeExpr _ (MonoLetVar t i) = LetVarN <$> inferType t <*> pure i
    nativeExpr m (MonoReturn e) = ReturnN <$> nativeExpr m e
    -- Cross-language recursive call: serialize args, call via socket, deserialize result
    nativeExpr m (MonoApp (MonoExe (Idx idx t0) (RecCallP mid (Just targetLang))) es) = do
      let (_, outputType) = case t0 of
            FunT its ot -> (its, ot)
            _ -> ([], t0)
      nativeArgs <- mapM (nativeExpr m) es
      serializedArgs <- mapM (serializeS "foreignRecArg" m) nativeArgs
      resultType <- inferType (Idx idx outputType)
      config <- MM.ask
      reg <- MM.gets stateLangRegistry
      let socket = MC.setupServerAndSocket config reg targetLang
          serialCall = AppForeignRecS resultType mid socket serializedArgs
      naturalizeN "foreignRecCall" m lang resultType serialCall
    -- Same-language recursive call: serialize args, call serial manifold, deserialize result
    nativeExpr m (MonoApp (MonoExe (Idx idx t0) (RecCallP mid Nothing)) es) = do
      let (_, outputType) = case t0 of
            FunT its ot -> (its, ot)
            _ -> ([], t0)
      -- Build native args, then serialize each one
      nativeArgs <- mapM (nativeExpr m) es
      serializedArgs <- mapM (serializeS "recArg" m) nativeArgs
      -- Return type of the serial manifold call
      resultType <- inferType (Idx idx outputType)
      -- Create serial expression: call the serial manifold with serialized args
      let serialCall = AppRecS resultType mid serializedArgs
      -- Deserialize the result back to native
      naturalizeN "recCall" m lang resultType serialCall
    nativeExpr m (MonoApp (MonoExe (Idx idx t0) exe) es) = do
      args <- mapM (nativeArg m) es
      let (inputTypes, outputType) = case t0 of
            FunT its ot -> (its, ot)
            _ -> ([], t0)
      appType <- case drop (length es) inputTypes of
        [] -> inferType (Idx idx outputType)
        remaining -> inferType $ Idx idx (FunT remaining outputType)

      return $ AppExeN appType exe args
    nativeExpr m e@(MonoApp (MonoPoolCall t _ _ _ _) _) = do
      e' <- serialExpr m e
      t' <- inferType t
      -- 'EffectBoundary.insertEffectBoundaries' peels the outer 'EffectT'
      -- (if any) from the enclosing 'PolyRemoteInterface' upstream, so
      -- @t@ is guaranteed plain here.
      naturalizeN "nativeE MonoApp" m lang t' e'
    nativeExpr m (MonoApp (MonoLetVar (Idx idx (FunT inputTypes outputType)) i) es) = do
      args <- mapM (nativeArg m) es
      appType <- case drop (length es) inputTypes of
        [] -> inferType (Idx idx outputType)
        remaining -> inferType $ Idx idx (FunT remaining outputType)
      return $ AppExeN appType (LocalCallP i) args
    nativeExpr m (MonoCacheBody lbl midx args body) = do
      -- Inside a Preserved manifold the bound vars are native, so the
      -- wrap must reference n0/n1/... -- 'serialExpr' here would emit
      -- s0/s1/... and miscompile.
      se <- lowerCacheBody Unserialized m lbl midx args body
      let t' = case typeSof se of
            SerialS tf -> tf
            _ -> error "CacheBody body must lower to a serial form"
      naturalizeN "MonoCacheBody" m lang t' se
    nativeExpr m (MonoDebugWrap midx args body) = do
      -- Same reasoning as MonoCacheBody: inside Preserved the bound
      -- vars are native and must reference n0/n1/...
      se <- lowerDebugWrap Unserialized m midx args body
      let t' = case typeSof se of
            SerialS tf -> tf
            _ -> error "DebugWrap body must lower to a serial form"
      naturalizeN "MonoDebugWrap" m lang t' se
    nativeExpr _ (MonoApp _ _) = error "Illegal application"
    nativeExpr _ (MonoExe t exe) = ExeN <$> inferType t <*> pure exe
    nativeExpr _ (MonoBndVar (A _) _) = error "MonoBndVar must have a type if used in native context"
    nativeExpr _ (MonoBndVar (B _) i) =
      case Map.lookup i typemap of
        (Just (Right t)) -> BndVarN <$> inferType t <*> pure i
        _ -> error "No type found"
    nativeExpr _ (MonoBndVar (C t) i) = BndVarN <$> inferType t <*> pure i
    -- Resolve the head FVar via the FULL applied type so that
    -- parametrised typedefs (e.g. R's @type Vector n Bool = "logical"@
    -- vs @type Vector n a = "list"@) pick the rule that matches the
    -- args, not just the head TVar. The bare-TVar @inferVar@ path
    -- picks the first typedef by head name and silently emits the
    -- wrong CV when no Packable wrap is inserted -- see the analogous
    -- MonoNull comment below.
    nativeExpr m (MonoList (Idx vIdx vTv) args es) = do
      let argTs = map (\(Idx _ t) -> t) args
          fullT = if null args then VarT vTv else AppT (VarT vTv) argTs
      fullTf <- inferType (Idx vIdx fullT)
      let (headFV, argTfs) = case fullTf of
            VarF fv -> (fv, [])
            AppF (VarF fv) ts -> (fv, ts)
            _ -> error "MonoList head must resolve to VarF or AppF (VarF _)"
      ListN headFV argTfs <$> mapM (nativeExpr m) es
    nativeExpr m (MonoTuple v rs) =
      TupleN
        <$> inferVar v
        <*> mapM (nativeExpr m . snd) rs
    nativeExpr m (MonoRecord o v ps rs) =
      RecordN o
        <$> inferVar v
        <*> mapM inferType ps
        <*> mapM (secondM (nativeExpr m . snd)) rs
    nativeExpr _ (MonoLog v x) = LogN <$> inferVar v <*> pure x
    nativeExpr _ (MonoReal v x) = RealN <$> inferVar v <*> pure x
    nativeExpr _ (MonoInt v x) = IntN <$> inferVar v <*> pure x
    nativeExpr _ (MonoStr v x) = StrN <$> inferVar v <*> pure x
    -- MonoNull now carries an Indexed Type for the full type the
    -- Null inhabits (e.g. @?(BTree Int)@). Use @inferType@ (=
    -- @inferConcreteType@) rather than @inferVar@ so the resulting
    -- @TypeF@ preserves the alias's args; the bare-TVar @inferVar@
    -- path collapsed parameterised aliases through their body's head
    -- and lost the args (see the @PolyNull@ comment in Namespace.hs).
    nativeExpr _ (MonoNull v) = NullN <$> inferType v
    nativeExpr m (MonoIf cond thenE elseE) = do
      condNe <- nativeExpr m cond
      thenNe <- nativeExpr m thenE
      elseNe <- nativeExpr m elseE
      let ifType = case (thenNe, elseNe) of
            (NullN _, _) -> typeFof elseNe
            (_, NullN _) -> typeFof thenNe
            _ -> typeFof thenNe
      return $ IfN ifType condNe thenNe elseNe
    nativeExpr m (MonoDoBlock t e) = DoBlockN <$> inferType t <*> nativeExpr m e
    nativeExpr m (MonoEval t e) = EvalN <$> inferType t <*> nativeExpr m e
    nativeExpr m (MonoCoerce c t e) = CoerceN c <$> inferType t <*> nativeExpr m e
    -- Runtime intrinsics with thunk return types (save/load): the C functions
    -- (mlc_save, mlc_load) are eager, so we wrap them in DoBlockN to produce a
    -- proper thunk that EvalN can call.
    --
    -- IntrIFileWalk is intentionally NOT in this list: its morloc-level
    -- result type is the bare element type (Express.hs assigns @out@
    -- without an <IO> wrapper), so wrapping it in DoBlockN produces a
    -- thunk that no EvalN insertion site invokes. The walker is eager;
    -- emit it as a plain inline call and let surrounding expressions
    -- consume the value directly.
    nativeExpr m (MonoIntrinsic t intr es)
      | intr `elem` [IntrSave, IntrSaveM, IntrSaveJ, IntrLoad, IntrRead,
                     IntrOpen, IntrClose, IntrFSchema,
                     IntrFLength, IntrNext, IntrStream,
                     IntrWrite, IntrAppend, IntrConcat, IntrFlush,
                     IntrStdin, IntrStdout, IntrStderr, IntrThrow,
                     IntrCatch] = do
          tf <- inferType t
          esBase <- mapM (nativeExpr m) es
          -- @catch's args must both reach mlc_catch as no-arg callables;
          -- see 'thunkifyForCatch' below.
          let es' = case intr of
                IntrCatch -> map thunkifyForCatch esBase
                _         -> esBase
          es'' <- unpackDataArgIfNeeded m intr es'
          msch <- intrinsicSchema m intr tf es''
          let innerTf = case tf of
                EffectF _ inner -> inner
                other -> other
          mPacker <- loadResultPacker m intr innerTf
          let rawInnerTf = case mPacker of
                Just (_, wireTf) -> wireTf
                Nothing -> innerTf
              raw = IntrinsicN rawInnerTf intr msch es''
          wrapped <- case mPacker of
            Just (packerSrc, _) ->
              return $ AppExeN innerTf (SrcCallP packerSrc) [NativeArgExpr raw]
            Nothing -> return raw
          -- Wrap in DoBlockN only when the result still carries an effect
          -- (needs to be a thunk that EvalN or a language-native catch can
          -- invoke). @catch fully strips its Err effect when the residual
          -- row is empty, yielding a plain-typed value; wrapping such a
          -- value in DoBlockN turns it into a callable that never gets
          -- forced when passed as a function argument.
          return $ case tf of
            EffectF _ _ -> DoBlockN tf wrapped
            _           -> wrapped
    nativeExpr m (MonoIntrinsic t intr es) = do
      tf <- inferType t
      es' <- mapM (nativeExpr m) es
      es'' <- unpackDataArgIfNeeded m intr es'
      msch <- intrinsicSchema m intr tf es''
      mPacker <- loadResultPacker m intr tf
      let rawTf = case mPacker of
            Just (_, wireTf) -> wireTf
            Nothing -> tf
          raw = IntrinsicN rawTf intr msch es''
      case mPacker of
        Just (packerSrc, _) ->
          return $ AppExeN tf (SrcCallP packerSrc) [NativeArgExpr raw]
        Nothing -> return raw

    -- For data-bearing runtime intrinsics (@save/@savej/@savem/@show/@hash),
    -- the runtime expects the value in *wire form*. The normal cross-pool
    -- path inserts the unpacker in expandSerialize's SerialPack arm; we
    -- mirror that here so intrinsics flow through the same pack/unpack
    -- machinery as ordinary functions instead of feeding the runtime a
    -- user-side struct it cannot serialize.
    -- Idempotently wrap a NativeExpr in a DoBlockN so it renders as a
    -- no-arg thunk. Any effect-typed NativeExpr is already thunk-shaped
    -- via the enclosing DoBlockN-wrap step; a structural pattern-match
    -- would have to enumerate every pass-through constructor
    -- (MapOptionalN, CoerceN, ReturnN, ...) and silently misbehave when
    -- a new one is added, so we dispatch on the type wrapper instead.
    thunkifyForCatch :: NativeExpr -> NativeExpr
    thunkifyForCatch e = case typeFof e of
      EffectF _ _ -> e
      t           -> DoBlockN t e


    unpackDataArgIfNeeded ::
      Int -> Intrinsic -> [NativeExpr] -> MorlocMonad [NativeExpr]
    -- @save's first argument is the compression level, not the data;
    -- the data is the second positional arg. Apply the unpacker there.
    unpackDataArgIfNeeded m IntrSave (levelArg : dataArg : rest) = do
      rest' <- packDataArg m dataArg
      return (levelArg : rest' ++ rest)
    -- @write's args are [level, handle, value]; the value is at index 2.
    unpackDataArgIfNeeded m IntrWrite (levelArg : handleArg : dataArg : rest) = do
      rest' <- packDataArg m dataArg
      return (levelArg : handleArg : rest' ++ rest)
    -- @savem/@savej's args are [path, value]; the value is at index 1.
    unpackDataArgIfNeeded m intr (pathArg : dataArg : rest)
      | intr `elem` [IntrSaveM, IntrSaveJ] = do
          rest' <- packDataArg m dataArg
          return (pathArg : rest' ++ rest)
    unpackDataArgIfNeeded m intr (dataArg : rest)
      | intr `elem` [IntrShow, IntrHash] = do
          rest' <- packDataArg m dataArg
          return (rest' ++ rest)
    unpackDataArgIfNeeded _ _ args = return args

    -- Pack a single data arg through its language's unpacker if it has
    -- a Packable instance. Returns either a single-element list with the
    -- wrapped expression or the original arg unchanged.
    packDataArg :: Int -> NativeExpr -> MorlocMonad [NativeExpr]
    packDataArg m dataArg = do
      ast <- Serial.makeSerialAST m lang (typeFof dataArg)
      case ast of
        SerialPack _ (packer, _) -> do
          let unpackerSrc = typePackerReverse packer
              unpackedType = typePackerUnpacked packer
          return [AppExeN unpackedType (SrcCallP unpackerSrc) [NativeArgExpr dataArg]]
        _ -> return [dataArg]

    -- Symmetric to unpackDataArgIfNeeded: @load and @read return the wire
    -- form of the user's type. When the user-facing inner type has a
    -- Packable instance, expandDeserialize's SerialPack arm inserts the
    -- packer; we mirror that here for the intrinsic result. Returns the
    -- packer source + the wire-form type so the caller can both rebuild
    -- the intrinsic's type with the wire form (so the runtime call uses
    -- the wire-side template) and wrap the result with the packer.
    loadResultPacker ::
      Int -> Intrinsic -> TypeF -> MorlocMonad (Maybe (Source, TypeF))
    loadResultPacker m intr resultTf
      | intr `elem` [IntrLoad, IntrRead] = do
          ast <- Serial.makeSerialAST m lang resultTf
          case ast of
            SerialPack _ (packer, _) ->
              return $ Just (typePackerForward packer, typePackerUnpacked packer)
            _ -> return Nothing
      | otherwise = return Nothing

    -- Compute the msgpack schema string for runtime intrinsics
    intrinsicSchema :: Int -> Intrinsic -> TypeF -> [NativeExpr] -> MorlocMonad (Maybe Text)
    -- @save's data is the second positional arg (after the level).
    intrinsicSchema m IntrSave _ (_levelArg : dataArg : _) = do
      ast <- Serial.makeSerialAST m lang (typeFof dataArg)
      return . Just . render $ Serial.serialAstToMsgpackSchema ast
    -- @savem/@savej's data is the second positional arg (after the path).
    intrinsicSchema m intr _ (_pathArg : dataArg : _)
      | intr `elem` [IntrSaveM, IntrSaveJ] = do
          ast <- Serial.makeSerialAST m lang (typeFof dataArg)
          return . Just . render $ Serial.serialAstToMsgpackSchema ast
    intrinsicSchema m intr _ (dataArg:_)
      | intr `elem` [IntrHash, IntrShow, IntrSchema] = do
          ast <- Serial.makeSerialAST m lang (typeFof dataArg)
          return . Just . render $ Serial.serialAstToMsgpackSchema ast
    intrinsicSchema _ IntrTypeof _ (dataArg:_) =
      -- @typeof yields the user-facing type name as a compile-time constant
      -- string. The string is stored in the Intrinsic node's schema slot and
      -- emitted as a literal by the translator; the argument is erased.
      return . Just $ renderTypeFName (typeFof dataArg)
    intrinsicSchema m IntrLoad tf _ = do
      -- For @load, the return type is <IO, Err> a; the schema is for a.
      let unwrap (EffectF _ inner) = unwrap inner
          unwrap other = other
          dataType = unwrap tf
      ast <- Serial.makeSerialAST m lang dataType
      return . Just . render $ Serial.serialAstToMsgpackSchema ast
    intrinsicSchema m IntrRead tf _ = do
      -- For @read, the return type is <Err> a; the schema is for a.
      let unwrap (EffectF _ inner) = unwrap inner
          unwrap other = other
          dataType = unwrap tf
      ast <- Serial.makeSerialAST m lang dataType
      return . Just . render $ Serial.serialAstToMsgpackSchema ast
    intrinsicSchema m IntrIFileWalk tf _ = do
      -- The schema describes the result type the walker materializes
      -- (the per-language wrapper deserializes the voidstar via
      -- from_voidstar<T>). For bracket-index/struct chains the result
      -- is a single element type; for bracket-slice it is a list type.
      -- Either way, the post-EffectF type carries the right shape.
      let unwrap (EffectF _ inner) = unwrap inner
          unwrap other = other
          dataType = unwrap tf
      ast <- Serial.makeSerialAST m lang dataType
      return . Just . render $ Serial.serialAstToMsgpackSchema ast
    intrinsicSchema m IntrNext tf _ = do
      -- @next returns the sub-packet as `[a]`. The wrapper drops the
      -- EffectF wrap and serialises the list type so the per-language
      -- from_voidstar call materialises it correctly.
      let unwrap (EffectF _ inner) = unwrap inner
          unwrap other = other
          dataType = unwrap tf
      ast <- Serial.makeSerialAST m lang dataType
      return . Just . render $ Serial.serialAstToMsgpackSchema ast
    -- @write's data arg (at index 2, after level Int and handle) carries `[a]`;
    -- schema describes that list type so to_voidstar produces the right
    -- voidstar layout for the runtime's flatten_to_buffer.
    intrinsicSchema m IntrWrite _ (_levelArg : _handleArg : dataArg : _) = do
      ast <- Serial.makeSerialAST m lang (typeFof dataArg)
      return . Just . render $ Serial.serialAstToMsgpackSchema ast
    -- @open on IFile/IStream reads its schema off disk; codegen routes
    -- through the generic `_mlc_open(path, kind)` entry so we return
    -- Nothing. @open on OStream needs the storage schema at open time.
    intrinsicSchema m IntrOpen tf _ = case unwrapHandleHead tf of
      Just (v, a) | v == BT.ostreamVar ->
        Just <$> renderStorageSchema m v a
      _ -> return Nothing
    intrinsicSchema m IntrAppend tf _ = case unwrapHandleHead tf of
      Just (v, a) -> Just <$> renderStorageSchema m v a
      Nothing     -> return Nothing
    intrinsicSchema m intr tf _
      | intr `elem` [IntrStdin, IntrStdout, IntrStderr] =
          case unwrapHandleHead tf of
            Just (v, a) -> Just <$> renderStorageSchema m v a
            Nothing     -> return Nothing
    intrinsicSchema _ _ _ _ = return Nothing

    renderStorageSchema :: Int -> TVar -> TypeF -> MorlocMonad Text
    renderStorageSchema m v a = do
      ast <- Serial.makeSerialAST m lang (BT.handleStorageTypeF v a)
      return . render $ Serial.serialAstToMsgpackSchema ast

    unwrapHandleHead :: TypeF -> Maybe (TVar, TypeF)
    unwrapHandleHead (EffectF _ inner) = unwrapHandleHead inner
    unwrapHandleHead (AppF (VarF (FV v _)) (a : _)) = Just (v, a)
    unwrapHandleHead _ = Nothing

    -- Render a TypeF as a user-facing Morloc type string (for @typeof).
    -- Uses the general type variable name (not the language-concrete one),
    -- matching what the user wrote in their source.
    renderTypeFName :: TypeF -> Text
    renderTypeFName = render . go
      where
        go (UnkF (FV t _)) = pretty t
        go (VarF (FV t _)) = pretty t
        go (NamF _ (FV t _) params _) =
          case params of
            [] -> pretty t
            ps -> parens (pretty t <+> hsep (map go ps))
        go (RecF (FV t _)) = pretty t
        go (AppF con args) = parens (go con <+> hsep (map go args))
        go (FunF args ret) =
          parens (hsep (punctuate " ->" (map go args ++ [go ret])))
        go (EffectF _ t) = go t
        go (OptionalF t) = "?" <> go t
        go (NatLitF n) = pretty n
        go NatVoidF = "_"
        go (StrLitF s) = dquotes (pretty s)
        go StrVoidF = "_"

    lowerCacheBody ::
      SerializationState ->
      Int ->
      Text ->
      Int ->
      [Arg None] ->
      MonoExpr ->
      MorlocMonad SerialExpr
    lowerCacheBody state m lbl midx args body = do
      body' <- serialExpr m body
      args' <- mapM (\(Arg i _) -> do
                        arg@(Arg _ tm) <- typeArg state i
                        sa <- case tm of
                          Native tf -> Serial.makeSerialAST m lang tf
                          Serial tf -> Serial.makeSerialAST m lang tf
                          Passthrough -> MM.throwCompilerBug
                            $ "lowerCacheBody: cannot hash a Passthrough arg (arg "
                            <> pretty i <> "); the cache wrap needs a schema for"
                            <+> "each arg, so passthrough args must be resolved upstream"
                          Function _ _ -> MM.throwCompilerBug
                            $ "lowerCacheBody: cannot hash a Function arg (arg "
                            <> pretty i <> "); function values have no wire form"
                        return (arg, sa)
                    ) args
      let t' = case typeSof body' of
            SerialS tf -> tf
            _ -> error "CacheBody body must lower to a serial form"
      resSa <- Serial.makeSerialAST m lang t'
      return $ CacheBodyS t' resSa lbl midx args' body'

    -- | Parallel of 'lowerCacheBody' for the debug-trace wrap. Pairs
    -- each arg with its 'SerialAST' so the catch block can serialize
    -- the arg into a packet before dumping. Passthrough / function
    -- args are silently skipped here (with @SerialNone@) rather than
    -- throwing -- their omission from the trace is acceptable, and
    -- failing the build would defeat --debug's purpose as a
    -- diagnostic tool.
    lowerDebugWrap ::
      SerializationState ->
      Int ->
      Int ->
      [Arg None] ->
      MonoExpr ->
      MorlocMonad SerialExpr
    lowerDebugWrap state m midx args body = do
      body' <- serialExpr m body
      -- Debug-trace is best-effort: an arg whose type the typemap
      -- can't infer (e.g. untyped passthrough into a remote-dispatch
      -- sub-manifold) is silently dropped rather than crashing the
      -- build. Same for passthrough/function args, which have no
      -- wire form to dump.
      args' <- fmap catMaybes $ mapM (\(Arg i _) ->
                  case Map.lookup i typemap of
                    Nothing -> return Nothing
                    Just _ -> do
                      arg@(Arg _ tm) <- typeArg state i
                      case tm of
                        Native tf -> do
                          sa <- Serial.makeSerialAST m lang tf
                          return $ Just (arg, sa)
                        Serial tf -> do
                          sa <- Serial.makeSerialAST m lang tf
                          return $ Just (arg, sa)
                        Passthrough -> return Nothing
                        Function _ _ -> return Nothing
                ) args
      -- The TypeF stored on DebugWrapS is unused at codegen; pick
      -- whichever serial type we can recover from the body. AppPoolS
      -- (a remote-dispatch body) has typeSof = FunctionS _ (SerialS
      -- t), not SerialS t directly, so peel a function result if
      -- present.
      let t' = case extractSerial (typeSof body') of
            Just tf -> tf
            Nothing -> error "DebugWrap body must lower to a serial form"
      return $ DebugWrapS t' midx args' body'

    extractSerial :: TypeS -> Maybe TypeF
    extractSerial (SerialS tf) = Just tf
    extractSerial (FunctionS _ inner) = extractSerial inner
    extractSerial _ = Nothing

    typeArg ::
      SerializationState ->
      Int ->
      MorlocMonad (Arg TypeM)
    typeArg s i = case (s, Map.lookup i typemap) of
      (Serialized, Just (Right t)) -> do
        t' <- inferType t
        return $ Arg i (Serial t')
      (Serialized, Nothing) -> return $ Arg i Passthrough
      (Serialized, Just (Left t)) -> do
        MM.sayVVV $ "typeArg universal inference of unindexed type " <> pretty t
        t' <- inferTypeUniversal t
        return $ Arg i (Serial t')
      (Unserialized, Just (Right t)) -> do
        t' <- inferType t
        return $ Arg i (Native t')
      (Unserialized, Nothing) -> error "Bug: untyped non-passthrough value"
      (Unserialized, Just (Left t)) -> do
        MM.sayVVV $ "typeArg universal inference of unindexed type " <> pretty t
        t' <- inferTypeUniversal t
        return $ Arg i (Native t')

    makeTypemap :: Int -> MonoExpr -> Map.Map Int (Either Type (Indexed Type))
    makeTypemap _ (MonoLetVar t i) = Map.singleton i (Right t)
    makeTypemap parentIndex (MonoBndVar (B t) i) = Map.singleton i (Right (Idx parentIndex t))
    makeTypemap _ (MonoBndVar (C t) i) = Map.singleton i (Right t)
    makeTypemap _ (MonoManifold midx (manifoldBound -> ys) _ e) =
      Map.union (Map.fromList [(i, Left t) | (Arg i (Just t)) <- ys]) (makeTypemap midx e)
    makeTypemap parentIdx (MonoLet _ e1 e2) = Map.union (makeTypemap parentIdx e1) (makeTypemap parentIdx e2)
    makeTypemap parentIdx (MonoReturn e) = makeTypemap parentIdx e
    makeTypemap parentIdx (MonoEval _ e) = makeTypemap parentIdx e
    makeTypemap parentIdx (MonoDoBlock _ e) = makeTypemap parentIdx e
    makeTypemap parentIdx (MonoCoerce _ _ e) = makeTypemap parentIdx e
    makeTypemap parentIdx (MonoIntrinsic _ _ es) = Map.unionsWith mergeTypes (map (makeTypemap parentIdx) es)
    makeTypemap parentIdx (MonoIf cond thenE elseE) =
      Map.unionsWith mergeTypes [makeTypemap parentIdx cond, makeTypemap parentIdx thenE, makeTypemap parentIdx elseE]
    makeTypemap _ (MonoApp (MonoExe (ann -> idx) _) es) = Map.unionsWith mergeTypes (map (makeTypemap idx) es)
    makeTypemap parentIdx (MonoApp e es) = Map.unionsWith mergeTypes (map (makeTypemap parentIdx) (e : es))
    makeTypemap parentIdx (MonoCacheBody _ _ _ e) = makeTypemap parentIdx e
    makeTypemap parentIdx (MonoDebugWrap _ _ e) = makeTypemap parentIdx e
    makeTypemap _ (MonoList (ann -> idx) _ es) = Map.unionsWith mergeTypes (map (makeTypemap idx) es)
    makeTypemap _ (MonoTuple (ann -> idx) (map snd -> es)) = Map.unionsWith mergeTypes (map (makeTypemap idx) es)
    makeTypemap _ (MonoRecord _ (ann -> idx) _ (map (snd . snd) -> es)) = Map.unionsWith mergeTypes (map (makeTypemap idx) es)
    makeTypemap _ _ = Map.empty

    mergeTypes :: Either Type (Indexed Type) -> Either Type (Indexed Type) -> Either Type (Indexed Type)
    mergeTypes (Right t) _ = Right t
    mergeTypes _ (Right t) = Right t
    mergeTypes x _ = x

    serializeS :: MDoc -> Int -> NativeExpr -> MorlocMonad SerialExpr
    serializeS _ m se =
      SerializeS <$> Serial.makeSerialAST m lang (typeFof se) <*> pure se

    inferState :: MonoExpr -> SerializationState
    inferState (MonoApp MonoPoolCall {} _) = Serialized
    inferState (MonoApp MonoExe {} _) = Unserialized
    inferState (MonoApp (MonoManifold _ _ _ e) _) = inferState e
    inferState (MonoLet _ _ e) = inferState e
    inferState (MonoReturn e) = inferState e
    inferState (MonoManifold _ _ _ e) = inferState e
    inferState (MonoIf _ thenE _) = inferState thenE
    inferState MonoPoolCall {} = Unserialized
    inferState MonoBndVar {} = Unserialized
    inferState _ = Unserialized

{- | Unwrap structural MonoManifold/MonoReturn wrappers from a let definition.
MonoManifold contributes its index (for type lookups); MonoReturn is the
manifold's return semantics, which is meaningless in a let-binding context.

Manifolds tagged 'Preserved' carry observability hooks and survive the
strip -- the kind was set once at 'PolyManifold' construction in
'Express.hs' and threaded through 'Segment.hs'. The 'm /= currentM'
guard is the same self-wrap collision guard used in 'serialExpr'.
-}
unwrapLetDef :: Int -> MonoExpr -> (Int, MonoExpr)
unwrapLetDef currentM orig@(MonoManifold m _ kind _)
  | kind == Preserved && m /= currentM = (m, orig)
unwrapLetDef _ (MonoManifold m _ _ (MonoReturn e)) = (m, e)
unwrapLetDef _ (MonoManifold m _ _ e) = (m, e)
unwrapLetDef m (MonoReturn e) = (m, e)
unwrapLetDef m e = (m, e)

naturalizeN :: MDoc -> Int -> Lang -> TypeF -> SerialExpr -> MorlocMonad NativeExpr
naturalizeN _ m lang t se =
  DeserializeN t <$> Serial.makeSerialAST m lang t <*> pure se

class IsSerializable a where
  serialLet :: Int -> SerialExpr -> a -> a
  nativeLet :: Int -> NativeExpr -> a -> a

instance IsSerializable SerialExpr where
  -- When the body is a 'CacheBodyS', push the new let-binding inside
  -- the cache wrap so the wrapped computation (and its dependencies)
  -- runs only on a cache miss. This is critical for 'cache: true':
  -- without it, 'wireSerial.letWrap' inserts the manifold's
  -- deserialization lets AROUND the cache check, so the
  -- 'morloc.get_value' calls run unconditionally and defeat the
  -- point of caching.
  serialLet i se (CacheBodyS t resSa lbl m args inner) =
    CacheBodyS t resSa lbl m args (SerialLetS i se inner)
  -- Same push-in for 'DebugWrapS': lets MUST live inside the try
  -- frame so the catch block fires on deserialization failures, not
  -- just on body failures. If lets were hoisted outside, the wrap
  -- would silently never see exceptions raised by 'morloc.get_value'.
  serialLet i se (DebugWrapS t m args inner) =
    DebugWrapS t m args (SerialLetS i se inner)
  serialLet i se body = SerialLetS i se body
  nativeLet i ne (CacheBodyS t resSa lbl m args inner) =
    CacheBodyS t resSa lbl m args (NativeLetS i ne inner)
  nativeLet i ne (DebugWrapS t m args inner) =
    DebugWrapS t m args (NativeLetS i ne inner)
  nativeLet i ne body = NativeLetS i ne body

instance IsSerializable NativeExpr where
  serialLet = SerialLetN
  nativeLet = NativeLetN

type D a = (Map.Map Int Request, a)

wireSerial :: Lang -> SerialManifold -> MorlocMonad SerialManifold
wireSerial lang sm0@(SerialManifold m0 _ _ _ _) = foldSerialManifoldM fm sm0 |>> snd
  where
    defs = makeMonoidFoldDefault Map.empty (Map.unionWith (<>))

    fm =
      FoldManifoldM
        { opSerialManifoldM = wireSerialManifold
        , opNativeManifoldM = wireNativeManifold
        , opSerialExprM = wireSerialExpr
        , opNativeExprM = wireNativeExpr
        , opSerialArgM = monoidSerialArg defs
        , opNativeArgM = monoidNativeArg defs
        }

    wireSerialManifold :: SerialManifold_ (D SerialExpr) -> MorlocMonad (D SerialManifold)
    wireSerialManifold (SerialManifold_ m _ form headForm (req, e)) = do
      let form' = afirst (specialize req) form
          req' = Map.map fst (manifoldToMap form')
      e' <- letWrap m form' req e
      return (req', SerialManifold m lang form' headForm e')

    wireNativeManifold :: NativeManifold_ (D NativeExpr) -> MorlocMonad (D NativeManifold)
    wireNativeManifold (NativeManifold_ m _ form (req, e)) = do
      let form' = afirst (specialize req) form
          req' = Map.map fst (manifoldToMap form')
      e' <- letWrap m form' req e
      return (req', NativeManifold m lang form' e')

    wireSerialExpr (LetVarS_ t i) = return (Map.singleton i SerialContent, LetVarS t i)
    wireSerialExpr (BndVarS_ t i) = return (Map.singleton i SerialContent, BndVarS t i)
    wireSerialExpr (AppPoolS_ t p@(PoolCall _ _ _ pargs) args) = do
      let req1 = Map.unionsWith (<>) (map fst args)
          req2 = Map.fromList [(i, requestOf tm) | Arg i tm <- pargs]
          req3 = Map.unionWith (<>) req1 req2
      return (req3, AppPoolS t p (map snd args))
    wireSerialExpr (AppRecS_ t mid args) = do
      let req = Map.unionsWith (<>) (map fst args)
      return (req, AppRecS t mid (map snd args))
    wireSerialExpr (AppForeignRecS_ t mid socket args) = do
      let req = Map.unionsWith (<>) (map fst args)
      return (req, AppForeignRecS t mid socket (map snd args))
    wireSerialExpr (SerialLetS_ i (req1, se1) (req2, se2)) = do
      let req' = Map.unionWith (<>) req1 req2
      e' <- case Map.lookup i req2 of
        (Just NativeContent) -> case typeSof se1 of
          (SerialS tf) -> NativeLetS i <$> naturalizeN "a" m0 lang tf se1 <*> pure se2
          (FunctionS _ (SerialS tf)) -> NativeLetS i <$> naturalizeN "a" m0 lang tf se1 <*> pure se2
          _ -> error "Unuseable let definition"
        (Just NativeAndSerialContent) -> case typeSof se1 of
          (SerialS tf) -> do
            ne1 <- naturalizeN "a" m0 lang tf (LetVarS (Just tf) i)
            return $ SerialLetS i se1 (NativeLetS i ne1 se2)
          (FunctionS _ (SerialS tf)) -> do
            ne1 <- naturalizeN "a" m0 lang tf (LetVarS (Just tf) i)
            return $ SerialLetS i se1 (NativeLetS i ne1 se2)
          _ -> error "Unuseable let definition"
        _ -> return $ SerialLetS i se1 se2
      return (req', e')
    wireSerialExpr (NativeLetS_ i (req1, ne1) (req2, se2)) = do
      let req' = Map.unionWith (<>) req1 req2
      e' <- case Map.lookup i req2 of
        (Just SerialContent) -> SerialLetS i <$> serializeS "b" m0 (typeFof ne1) ne1 <*> pure se2
        (Just NativeAndSerialContent) -> do
          let tf = typeFof ne1
          sv <- serializeS "b" m0 tf (LetVarN tf i)
          return $ NativeLetS i ne1 (SerialLetS i sv se2)
        _ -> return $ NativeLetS i ne1 se2
      return (req', e')
    wireSerialExpr e = monoidSerialExpr defs e

    wireNativeExpr ::
      NativeExpr_ (D NativeManifold) (D SerialExpr) (D NativeExpr) (D SerialArg) (D NativeArg) ->
      MorlocMonad (D NativeExpr)
    wireNativeExpr (LetVarN_ t i) = return (Map.singleton i NativeContent, LetVarN t i)
    wireNativeExpr (BndVarN_ t i) = return (Map.singleton i NativeContent, BndVarN t i)
    wireNativeExpr (SerialLetN_ i (req1, se1) (req2, ne2)) = do
      let req' = Map.unionWith (<>) req1 req2
      e' <- case Map.lookup i req2 of
        (Just NativeContent) -> case typeSof se1 of
          (SerialS tf) -> NativeLetN i <$> naturalizeN "a" m0 lang tf se1 <*> pure ne2
          (FunctionS _ (SerialS tf)) -> NativeLetN i <$> naturalizeN "a" m0 lang tf se1 <*> pure ne2
          _ -> error "Unuseable let definition"
        (Just NativeAndSerialContent) -> case typeSof se1 of
          (SerialS tf) -> do
            ne1 <- naturalizeN "a" m0 lang tf (LetVarS (Just tf) i)
            return $ SerialLetN i se1 (NativeLetN i ne1 ne2)
          (FunctionS _ (SerialS tf)) -> do
            ne1 <- naturalizeN "a" m0 lang tf (LetVarS (Just tf) i)
            return $ SerialLetN i se1 (NativeLetN i ne1 ne2)
          _ -> error "Unuseable let definition"
        _ -> return $ SerialLetN i se1 ne2
      return (req', e')
    wireNativeExpr (NativeLetN_ i (req1, ne1) (req2, ne2)) = do
      let req' = Map.unionWith (<>) req1 req2
      e' <- case Map.lookup i req2 of
        (Just SerialContent) -> SerialLetN i <$> serializeS "b" m0 (typeFof ne1) ne1 <*> pure ne2
        (Just NativeAndSerialContent) -> do
          let tf = typeFof ne1
          sv <- serializeS "b" m0 tf (LetVarN tf i)
          return $ NativeLetN i ne1 (SerialLetN i sv ne2)
        _ -> return $ NativeLetN i ne1 ne2
      return (req', e')
    wireNativeExpr e = monoidNativeExpr defs e

    specialize :: Map.Map Int Request -> Int -> Or TypeS TypeF -> Or TypeS TypeF
    specialize req i r = case (Map.lookup i req, r) of
      (Nothing, _) -> L PassthroughS
      (Just SerialContent, LR t _) -> L t
      (Just NativeContent, LR _ t) -> R t
      _ -> r

    letWrap ::
      (IsSerializable e, HasRequest t, MayHaveTypeF t) =>
      Int ->
      ManifoldForm (Or TypeS TypeF) t ->
      Map.Map Int Request ->
      e ->
      MorlocMonad e
    letWrap m form0 req0 e0 = do
      foldlM wrapAsNeeded e0 (Map.toList req0)
      where
        formMap = manifoldToMap form0

        wrapAsNeeded :: (IsSerializable e) => e -> (Int, Request) -> MorlocMonad e
        wrapAsNeeded e (i, req) = case (req, Map.lookup i formMap) of
          (SerialContent, Just (NativeContent, Just t)) -> serialLet i <$> serializeS "wan 1" m t (BndVarN t i) <*> pure e
          (NativeAndSerialContent, Just (NativeContent, Just t)) -> serialLet i <$> serializeS "wan 2" m t (BndVarN t i) <*> pure e
          (NativeContent, Just (SerialContent, Just t)) -> nativeLet i <$> naturalizeN "wan 3" m lang t (BndVarS (Just t) i) <*> pure e
          (NativeAndSerialContent, Just (SerialContent, Just t)) -> nativeLet i <$> naturalizeN "wan 4" m lang t (BndVarS (Just t) i) <*> pure e
          _ -> return e

    manifoldToMap ::
      (HasRequest t, MayHaveTypeF t) =>
      ManifoldForm (Or TypeS TypeF) t ->
      Map.Map Int (Request, Maybe TypeF)
    manifoldToMap form = f form
      where
        mapRequestFromXs xs = Map.fromList [(i, (requestOf t, mayHaveTypeF t)) | (Arg i t) <- typeMofRs xs]
        mapRequestFromYs ys = Map.fromList [(i, (requestOf t, mayHaveTypeF t)) | (Arg i t) <- ys]

        f (ManifoldFull xs) = mapRequestFromXs xs
        f (ManifoldPass ys) = mapRequestFromYs ys
        f (ManifoldPart xs ys) = Map.union (mapRequestFromXs xs) (mapRequestFromYs ys)

    serializeS :: MDoc -> Int -> TypeF -> NativeExpr -> MorlocMonad SerialExpr
    serializeS _ m t se =
      SerializeS <$> Serial.makeSerialAST m lang t <*> pure se

data Request = SerialContent | NativeContent | NativeAndSerialContent
  deriving (Ord, Eq, Show)

class HasRequest a where
  requestOf :: a -> Request

instance HasRequest TypeM where
  requestOf Passthrough = SerialContent
  requestOf (Serial _) = SerialContent
  requestOf (Native _) = NativeContent
  requestOf (Function _ _) = NativeContent

instance HasRequest SerialExpr where
  requestOf _ = SerialContent

instance HasRequest NativeExpr where
  requestOf _ = NativeContent

instance HasRequest SerialArg where
  requestOf _ = SerialContent

instance HasRequest NativeArg where
  requestOf _ = NativeContent

instance HasRequest TypeS where
  requestOf _ = SerialContent

instance HasRequest TypeF where
  requestOf _ = NativeContent

instance Semigroup Request where
  SerialContent <> SerialContent = SerialContent
  NativeContent <> NativeContent = NativeContent
  _ <> _ = NativeAndSerialContent

data SerializationState = Serialized | Unserialized
  deriving (Show, Eq, Ord)
