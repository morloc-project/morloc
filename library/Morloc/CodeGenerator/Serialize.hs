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
import Morloc.CodeGenerator.Infer
import Morloc.CodeGenerator.Namespace
import qualified Morloc.CodeGenerator.Serial as Serial
import qualified Morloc.Config as MC
import Morloc.Data.Doc
import qualified Morloc.Data.Map as Map
import qualified Morloc.LangRegistry as LR
import qualified Morloc.Monad as MM

{- | This step is performed after segmentation, so all terms are in the same
language. Here we need to determine where inputs are (de)serialized and the
serialization states of arguments and variables.
-}
serialize :: MonoHead -> MorlocMonad SerialManifold
serialize mh = do
  reg <- MM.gets stateLangRegistry
  serializeHosted reg mh

-- After segmentation a 'MonoHead' carries the language its terms were WRITTEN
-- in. For a co-located guest (e.g. Futhark hosted in the C++ pool) that home
-- language has no runtime in the pool: the manifold body executes as
-- host-native code and its values are host-native objects. Serialization must
-- therefore resolve concrete types and packing strategy under the HOST
-- language ('LR.poolOf'), so a host-only 'Packable' (e.g. the C++ Matrix
-- packer) is visible and the value marshals exactly as a native host value
-- would. 'poolOf' is identity for ordinary (self-hosting) languages.
serializeHosted :: LR.LangRegistry -> MonoHead -> MorlocMonad SerialManifold
serializeHosted reg (MonoHead lang0 m0 args0 headForm0 e0) = do
  form0 <- ManifoldFull <$> mapM prepareArg args0

  se1 <- serialExpr m0 e0
  let sm = SerialManifold m0 lang form0 headForm0 se1
  wireSerial lang sm
  where
    lang = LR.poolOf reg lang0
    inferType = inferConcreteType lang
    -- Universal inference has no index of its own -- these are the sites
    -- where the type reached us unindexed -- so errors and `data` arm
    -- resolution are reported against the manifold being serialized.
    inferTypeUniversal = inferConcreteTypeUniversal lang m0
    inferVar = inferConcreteVar lang

    typemap = makeTypemap m0 e0

    prepareArg ::
      Arg None ->
      MorlocMonad (Arg (Or TypeS TypeF))
    prepareArg (Arg i _) = case Map.lookup i typemap of
      Nothing -> return $ Arg i (L PassthroughS)
      (Just (Right t)) -> do
        t' <- inferType t
        return $ Arg i (L (serialArgType t'))
      (Just (Left t)) -> do
        MM.sayVVV "Warning: using universal inference at prepareArg"
        t' <- inferTypeUniversal t
        return $ Arg i (L (serialArgType t'))
      where
        -- A function-typed argument, a suspension included (a function of
        -- no arguments), has no wire form of its own: it crosses as a
        -- closure and is reflected on the far side, so it routes to the
        -- closure-reflecting 'SerialS' path.
        serialArgType tf = case tf of
          sf@(FunF {}) -> SerialS sf
          sf -> typeSof sf

    contextArg ::
      Int ->
      MorlocMonad (Or TypeS TypeF)
    contextArg i = case Map.lookup i typemap of
      (Just (Right t)) -> funcAwareOr <$> inferType t
      Nothing -> return $ L PassthroughS
      (Just (Left t)) -> do
        MM.sayVVV "Warning: using universal inference at contextArg"
        funcAwareOr <$> inferTypeUniversal t
      where
        -- A function value (a captured closure) has no wire form, so it is
        -- native-only: advertising a serial side ('LR') would let the caller
        -- serialize the closure -- 'put_value' of a partial fails at runtime.
        -- Non-function context args keep both forms.
        funcAwareOr t' = case t' of
          FunF {} -> R t'
          _       -> LR (typeSof t') t'

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
    serialExpr currentM orig@(MonoManifold m form kind inner)
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
          se <- serializeS "preserved manifold" m ne
          -- If the body was 'MonoReturn'-wrapped (standard shape from
          -- 'ensurePolyReturn'), the inner 'ReturnN' lives inside the
          -- NativeManifold function; surface 'ReturnS' here so the
          -- enclosing manifold emits its own 'return' statement.
          case inner of
            MonoReturn _ -> return (ReturnS se)
            _ -> return se
      -- A function-valued manifold (a closure: a 'ManifoldPart'/'ManifoldPass'
      -- with remaining bound parameters) is a first-class VALUE, not a
      -- computation to inline. Stripping it ('serialExpr m inner') would splice
      -- its body and drop its parameters -- corrupting a closure that crosses a
      -- boundary as a serialized RETURN (the mirror of the function-valued
      -- ARGUMENT path at 'serialArgType'). Route it through 'nativeExpr' +
      -- 'serializeS' so its 'typeFof' (a function) serializes as a
      -- 'SerialClosure' (reify), exactly as 'unwrapLetDef' keeps a let-bound
      -- closure whole.
      | isClosureForm form = do
          ne <- nativeExpr m orig
          se <- serializeS "closure value" m ne
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
      serializeS "serialE MonoIf" m ne
    -- Native-loop lowering. Walk the loop body -- a decision tree of guards
    -- ('MonoIf') and lets over base and continue leaves -- into a 'LoopBody'.
    -- Guards/continue-values/let-RHS are lowered through 'nativeExpr' over the
    -- loop-carried native locals ('ids') so they read their CURRENT (reassigned)
    -- values; a base leaf is serialized from that native value (never the stale
    -- serial param packet). 'addLoopWraps' has gated to a well-formed loop body
    -- (every 'MonoLoopContinue' reachable in a tail position), so a continue in
    -- a value/base position is a compiler bug -- 'nativeExpr' rejects it loud.
    serialExpr m (MonoLoop t ids body) = do
      t' <- inferType t
      LoopS t' ids <$> buildLoopBody body
      where
        buildLoopBody :: MonoExpr -> MorlocMonad (LoopBody NativeExpr SerialExpr)
        buildLoopBody (MonoIf cond thenB elseB) = do
          condNe <- nativeExpr m cond
          LoopIf condNe <$> buildLoopBody thenB <*> buildLoopBody elseB
        buildLoopBody (MonoReturn e) = buildLoopBody e
        -- Descend a do-block on the continue path: its inner binds (per-iteration
        -- effects) become loop-body lets emitted before the continue reassignment.
        buildLoopBody (MonoLoopContinue args)
          | length args == length ids = LoopContinue <$> mapM (nativeExpr m) args
          | otherwise = error $
              "morloc bug: MonoLoopContinue arity " <> show (length args)
                <> " does not match loop-carried ids " <> show (length ids)
        buildLoopBody (MonoLet i e1 e2) =
          let (m1, e1') = unwrapLetDef m e1
           in case inferState e1 of
                Serialized -> LoopSLet i <$> serialExpr m1 e1' <*> buildLoopBody e2
                Unserialized -> do
                  ne1 <- nativeExpr m1 e1'
                  LoopNLet i ne1 <$> buildLoopBody e2
        -- Any other leaf is a base case: serialize the CURRENT native value.
        buildLoopBody base =
          LoopBase <$> (nativeExpr m base >>= serializeS "loop base" m)
    serialExpr _ (MonoLoopContinue {}) = error "morloc: MonoLoopContinue reached serialExpr outside MonoLoop extraction"
    serialExpr _ (MonoExe _ _) = error "Can represent MonoSrc as SerialExpr"
    serialExpr _ MonoPoolCall {} = error "MonoPoolCall does not map to a SerialExpr"
    serialExpr _ (MonoApp MonoManifold {} _) = error "Illegal?"
    serialExpr m e = nativeExpr m e >>= serializeS "serialE e" m

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
    nativeExpr _ (MonoLoop {}) = error "morloc bug: MonoLoop in native position (loops are serial-only)"
    nativeExpr _ (MonoLoopContinue {}) = error "morloc bug: MonoLoopContinue in native position"
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
    -- Recursive call. The Express-time 'crossLang' mark was decided relative to
    -- the recursion's TEXTUAL parent pool, but segmentation can relocate a
    -- back-edge into a different pool (agnostic edges follow a foreign argument;
    -- a marked edge can land inside its own target's pool). Ignore the stale mark
    -- and re-derive co-location against 'lang' -- the pool this manifold was
    -- actually segmented into, now finally known -- and 'stateManifoldLang', the
    -- authoritative target pool: a socket 'foreign_call' ('AppForeignRecS') when
    -- the target is not co-located, otherwise a local same-pool 'AppRecS'. For a
    -- genuinely cross-pool recursion this reproduces the original mark exactly
    -- (Express set 'Just tl' from the same 'stateManifoldLang' lookup).
    nativeExpr m (MonoApp (MonoExe (Idx idx t0) (RecCallP mid _)) es) = do
      let (_, outputType) = case t0 of
            FunT its ot -> (its, ot)
            _ -> ([], t0)
      nativeArgs <- mapM (nativeExpr m) es
      serializedArgs <- mapM (serializeS "recArg" m) nativeArgs
      resultType <- inferType (Idx idx outputType)
      langMap <- MM.gets stateManifoldLang
      serialCall <- case Map.lookup mid langMap of
        Just targetLang | not (LR.coLocated reg lang targetLang) -> do
          let socket = MC.setupServerAndSocket reg targetLang
          return (AppForeignRecS resultType mid socket serializedArgs)
        _ -> return (AppRecS resultType mid serializedArgs)
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
    -- The complete type comes from the declaration, not from the arm being
    -- built: a constructor names one arm, but the value's wire form
    -- describes them all. Inference is what reads the declaration -- and,
    -- for a parameterized `data`, what instantiates its parameters with
    -- this constructor's own type arguments before reading the arms.
    nativeExpr _ (MonoEnum v@(Idx vidx _) n i) = do
      tf <- inferType v
      case tf of
        EnumF{} -> return $ EnumN tf n i
        _ -> MM.throwSourcedError vidx $
               "Constructor" <+> squotes (pretty n)
                 <+> "does not resolve to an enum:" <+> pretty tf
    nativeExpr args (MonoVariant v@(Idx vidx _) n i xs) = do
      tf <- inferType v
      case tf of
        VariantF{} -> VariantN tf n i <$> mapM (nativeExpr args) xs
        _ -> MM.throwSourcedError vidx $
               "Constructor" <+> squotes (pretty n)
                 <+> "does not resolve to a sum type:" <+> pretty tf
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
      -- The arms agree on one type (a suspension arm meets a suspension
      -- arm, both closures); a Null arm defers to its sibling's type.
      let ifType = case (thenNe, elseNe) of
            (NullN _, _) -> typeFof elseNe
            _ -> typeFof thenNe
      return $ IfN ifType condNe thenNe elseNe
    nativeExpr _ (MonoDoBlock _ _) =
      error "morloc bug: a suspension reached serialization unlowered (Suspension.lowerSuspensions runs first)"
    nativeExpr m (MonoEval t e) = EvalN <$> inferType t <*> nativeExpr m e
    nativeExpr m (MonoCoerce c t e) = CoerceN c <$> inferType t <*> nativeExpr m e
    -- Runtime intrinsics (save/load, streams, @try, ...). The C functions
    -- are eager; an intrinsic declared with a suspension result reaches
    -- here as the body of the closure manifold that suspends it
    -- ('Suspension.lowerSuspensions'), typed by its result.
    nativeExpr m (MonoIntrinsic t@(Idx tidx gt) intr es)
      | intr `elem` [IntrSave, IntrSaveM, IntrSaveJ, IntrLoad, IntrRead,
                     IntrOpen, IntrClose, IntrFSchema,
                     IntrFLength, IntrStreamLayout, IntrNext, IntrStream,
                     IntrWrite, IntrAppend, IntrConcat, IntrFlush,
                     IntrStdin, IntrStdout, IntrStderr, IntrThrow,
                     IntrTell, IntrTmpfile,
                     IntrTry] = do
          when (intr `elem` [IntrLoad, IntrRead, IntrNext, IntrOpen, IntrStdin]) $
            Serial.checkReadDataType tidx intr gt
          tf <- inferType t
          esBase <- mapM (nativeExpr m) es
          -- @try's body must reach mlc_try as a no-arg callable; see
          -- 'thunkifyForTry' below.
          let es' = case intr of
                IntrTry -> map thunkifyForTry esBase
                _       -> esBase
          es'' <- unpackDataArgIfNeeded m intr es'
          msch <- intrinsicSchema m intr tf es''
          let innerTf = tf
              -- A fallible intrinsic's own call still produces the bare
              -- value; the Try is built around it below. Everything from
              -- here to the wrap therefore works with the PAYLOAD type --
              -- in particular the result packer, which is keyed on the
              -- user-facing type and would find nothing against a Try.
              payloadTf = stripTryF innerTf
              -- @try itself already IS the wrap; wrapping it again would
              -- nest a Try inside a Try and hand the lowering a payload
              -- type where it expects the variant.
              isFallible = intr /= IntrTry && payloadTf /= innerTf
          -- @try produces the Try itself, so it keeps the variant type and
          -- takes no result packer: whatever packing its body needed has
          -- already happened inside the body.
          mPacker <- if intr == IntrTry
                       then return Nothing
                       else loadResultPacker m intr payloadTf
          let rawInnerTf
                | intr == IntrTry = innerTf
                | otherwise = case mPacker of
                    Just (_, wireTf) -> wireTf
                    Nothing -> payloadTf
              raw = IntrinsicN rawInnerTf intr msch es''
          packed <- case mPacker of
            Just (packerSrc, _) ->
              return $ AppExeN payloadTf (SrcCallP packerSrc) [NativeArgExpr raw]
            Nothing -> return raw
          -- Reuse @try rather than a bespoke wrap: converting a raised
          -- failure into an Ok/Err value is exactly what it does, and
          -- routing through it keeps one lowering (and one per-language
          -- mlc_try) instead of two. The packer runs inside the body, so a
          -- packer that raises is caught like any other failure.
          let wrapped
                | isFallible =
                    IntrinsicN innerTf IntrTry Nothing [thunkifyForTry packed]
                | otherwise = packed
          return wrapped
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
    -- @try's body reaches mlc_try as a callable of no arguments that it
    -- runs at most once. A suspension already is one. A pure body is
    -- wrapped in a 'DoBlockN', a rendering device for an inline lambda
    -- that the try runs immediately in the same scope; it never escapes
    -- as a value, so it needs none of a suspension's machinery. This is
    -- what lets @try catch a foreign function that raises from otherwise
    -- pure code.
    thunkifyForTry :: NativeExpr -> NativeExpr
    thunkifyForTry e@(DoBlockN _ _) = e
    thunkifyForTry e = case typeFof e of
      FunF [] _ -> e
      t         -> DoBlockN t e


    unpackDataArgIfNeeded ::
      Int -> Intrinsic -> [NativeExpr] -> MorlocMonad [NativeExpr]
    -- @save's args are [level, path, value]; the value is at index 2.
    -- Apply the unpacker there.
    unpackDataArgIfNeeded m IntrSave (levelArg : pathArg : dataArg : rest) = do
      rest' <- packDataArg m dataArg
      return (levelArg : pathArg : rest' ++ rest)
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
    -- @save's data is the third positional arg (after the level and path).
    intrinsicSchema m IntrSave _ (_levelArg : _pathArg : dataArg : _) = do
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
      -- For @load, the result type is Try Str a; the schema is for a.
      let dataType = stripTryF tf
      ast <- Serial.makeSerialAST m lang dataType
      return . Just . render $ Serial.serialAstToMsgpackSchema ast
    intrinsicSchema m IntrRead tf _ = do
      -- For @read, the result type is Try Str a; the schema is for a.
      let dataType = stripTryF tf
      ast <- Serial.makeSerialAST m lang dataType
      return . Just . render $ Serial.serialAstToMsgpackSchema ast
    intrinsicSchema m IntrIFileWalk tf _ = do
      -- The schema describes the result type the walker materializes
      -- (the per-language wrapper deserializes the voidstar via
      -- from_voidstar<T>). For bracket-index/struct chains the result
      -- is a single element type; for bracket-slice it is a list type.
      -- Either way, the result type carries the right shape.
      let dataType = stripTryF tf
      ast <- Serial.makeSerialAST m lang dataType
      return . Just . render $ Serial.serialAstToMsgpackSchema ast
    intrinsicSchema m IntrNext tf _ = do
      -- @next yields the sub-packet as `[a]`; the list type is serialised so
      -- the per-language from_voidstar call materialises it correctly.
      let dataType = stripTryF tf
      ast <- Serial.makeSerialAST m lang dataType
      return . Just . render $ Serial.serialAstToMsgpackSchema ast
    intrinsicSchema m IntrStreamLayout tf _ = do
      -- @streamLayout yields `[(U64,U64,U64)]`; the list-of-triple type is
      -- serialised so the per-language from_voidstar call materialises it
      -- (an ordinary composite, as for @next).
      let dataType = stripTryF tf
      ast <- Serial.makeSerialAST m lang dataType
      return . Just . render $ Serial.serialAstToMsgpackSchema ast
    -- @write's data arg (at index 2, after level Int and handle) carries `[a]`;
    -- schema describes that list type so to_voidstar produces the right
    -- voidstar layout for the runtime's flatten_to_buffer.
    intrinsicSchema m IntrWrite _ (_levelArg : _handleArg : dataArg : _) = do
      ast <- Serial.makeSerialAST m lang (typeFof dataArg)
      return . Just . render $ Serial.serialAstToMsgpackSchema ast
    -- @open on IFile reads its schema off disk; codegen routes through the
    -- generic `_mlc_open(path, kind)` entry so we return Nothing. @open on
    -- OStream/IStream needs the storage schema at open time (the typed
    -- `_mlc_open_ostream`/`_mlc_open_istream` entries): OStream to write the
    -- stream header, IStream to declare the schema for the stdin sentinel.
    intrinsicSchema m IntrOpen tf _ = case unwrapHandleHead tf of
      Just (v, a) | v == BT.ostreamVar || v == BT.istreamVar ->
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
    -- @close on a Str path (not a handle) is a registered-temp-file unlink.
    -- Mark it so the translator emits mlc_unlink_tmp; handle closes carry no
    -- schema and fall through to the generic close.
    intrinsicSchema _ IntrClose _ (argNE : _)
      | isStrHead (typeFof argNE) = return (Just BT.closeTmpUnlinkMarker)
      where
        isStrHead (VarF (FV v _)) = v == BT.str
        isStrHead _ = False
    intrinsicSchema _ _ _ _ = return Nothing

    renderStorageSchema :: Int -> TVar -> TypeF -> MorlocMonad Text
    renderStorageSchema m v a = do
      ast <- Serial.makeSerialAST m lang (BT.handleStorageTypeF v a)
      return . render $ Serial.serialAstToMsgpackSchema ast

    -- The payload of a Try's Ok arm, or the type unchanged when it is not
    -- one. Intrinsics that report failure as data wrap their result, but
    -- the schema a runtime entry point needs is still the inner one.
    stripTryF :: TypeF -> TypeF
    stripTryF (VariantF _ _ arms)
      | Just [payload] <- lookup BT.tryOkCtor arms = payload
    stripTryF other = other

    unwrapHandleHead :: TypeF -> Maybe (TVar, TypeF)
    -- The handle now arrives inside the Try the intrinsic returns, so peel
    -- the Ok arm before reading the head.
    unwrapHandleHead (VariantF _ _ arms)
      | Just [payload] <- lookup BT.tryOkCtor arms = unwrapHandleHead payload
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
        go (EnumF (FV t _) _ _) = pretty t
        go (VariantF (FV t _) _ _) = pretty t
        go (AppF con args) = parens (go con <+> hsep (map go args))
        go (FunF args ret) =
          parens (hsep (punctuate " ->" (map go args ++ [go ret])))
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
    makeTypemap parentIdx (MonoLoop _ _ e) = makeTypemap parentIdx e
    makeTypemap parentIdx (MonoLoopContinue es) = Map.unionsWith mergeTypes (map (makeTypemap parentIdx) es)
    -- A locally-defined function value (a closure) applied via 'LocalCallP j':
    -- record j's FUNCTION type from the head annotation. Without this the
    -- closure index has no typemap entry, so the arg machinery treats it as a
    -- serializable scalar and tries to serialize the closure (functions have no
    -- wire form). The head's 'Idx' carries the function type 'FunT ins out'.
    makeTypemap _ (MonoApp (MonoExe hg@(Idx idx _) (LocalCallP j)) es) =
      Map.unionsWith mergeTypes (Map.singleton j (Right hg) : map (makeTypemap idx) es)
    makeTypemap _ (MonoApp (MonoExe (ann -> idx) _) es) = Map.unionsWith mergeTypes (map (makeTypemap idx) es)
    makeTypemap parentIdx (MonoApp e es) = Map.unionsWith mergeTypes (map (makeTypemap parentIdx) (e : es))
    makeTypemap parentIdx (MonoCacheBody _ _ _ e) = makeTypemap parentIdx e
    makeTypemap parentIdx (MonoDebugWrap _ _ e) = makeTypemap parentIdx e
    makeTypemap _ (MonoList (ann -> idx) _ es) = Map.unionsWith mergeTypes (map (makeTypemap idx) es)
    makeTypemap _ (MonoTuple (ann -> idx) (map snd -> es)) = Map.unionsWith mergeTypes (map (makeTypemap idx) es)
    makeTypemap _ (MonoRecord _ (ann -> idx) _ (map (snd . snd) -> es)) = Map.unionsWith mergeTypes (map (makeTypemap idx) es)
    -- A constructor's arguments are ordinary expressions and must be walked
    -- like any other container's. Falling to the catch-all leaves them
    -- unregistered, so a manifold built around one receives its arguments
    -- serialized while the body expects native values.
    makeTypemap _ (MonoVariant (ann -> idx) _ _ es) = Map.unionsWith mergeTypes (map (makeTypemap idx) es)
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
    -- A function-valued manifold (non-empty bound args) is a first-class
    -- closure -- a NATIVE value, whatever its body does internally. Following
    -- into the body (as the saturated 'ManifoldFull' case must) would report
    -- 'Serialized' whenever an arm crosses a language boundary, driving a
    -- spurious closure reify at the let binding even though the closure is
    -- consumed locally. Agrees with 'unwrapLetDef', which keeps such a manifold
    -- whole for native-partial lowering.
    -- A Preserved manifold is one that must survive lowering as a real
    -- function -- it carries a user label, so codegen has to have something
    -- to wrap. That makes it a native value at its binding site for the same
    -- reason a closure is, whatever its body does internally. Reporting
    -- 'Serialized' here sends the binding down 'serialExpr', which strips the
    -- very manifold 'unwrapLetDef' just decided to keep (the keep-guard tests
    -- @m /= currentM@, and unwrapLetDef hands back the manifold's own index),
    -- and the label then has no manifold to attach to in either pool.
    inferState (MonoManifold _ form kind e)
      | kind == Preserved = Unserialized
      | isClosureForm form = Unserialized
      | otherwise = inferState e
    inferState (MonoIf _ thenE _) = inferState thenE
    inferState (MonoLoop _ _ e) = inferState e
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

A function-valued manifold ('ManifoldPart'/'ManifoldPass' -- non-empty
bound args) is a first-class closure and also survives the strip: it must
reach 'lowerManifold' so the manifold becomes a standalone def and the
let-binding is materialized as its native partial (functools.partial /
std::bind / R closure). Stripping it would splice the lambda body inline
with its parameters unbound. 'ManifoldFull' (empty bound -- a saturated
call producing a value) keeps the strip.
-}
unwrapLetDef :: Int -> MonoExpr -> (Int, MonoExpr)
unwrapLetDef currentM orig@(MonoManifold m _ kind _)
  | kind == Preserved && m /= currentM = (m, orig)
unwrapLetDef currentM orig@(MonoManifold m form _ _)
  | isClosureForm form && m /= currentM = (m, orig)
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
    wireSerialManifold (SerialManifold_ m _ form headForm (req, e)) =
      case loopCarriedTypes e of
        Nothing -> do
          let form' = afirst (specialize req) form
              req' = Map.map fst (manifoldToMap form')
          e' <- letWrap m form' req e
          return (req', SerialManifold m lang form' headForm e')
        -- A native loop: a carried slot used only serially (a foreign-call
        -- argument) never records a native type, so 'prepareArg' gave it
        -- 'L PassthroughS'. Recover each carried slot's native type from the
        -- continue value that reassigns it, then patch the form so the slot
        -- carries its native type and force it 'NativeContent', so 'letWrap'
        -- deserializes the entry packet into the native 'nvarNamer' local the
        -- continue reassigns and the loop's per-iteration re-serialization reads
        -- (see the 'LoopS_' handler).
        Just carriedTM -> do
          let form1 = patchCarriedForm carriedTM form
              reqForced = foldr (\i -> Map.insert i NativeContent) req (Map.keys carriedTM)
              form' = afirst (specialize reqForced) form1
              req' = Map.map fst (manifoldToMap form')
          e' <- letWrap m form' reqForced e
          return (req', SerialManifold m lang form' headForm e')

    -- First 'LoopContinue' leaf on the body spine (the back-edge reachable
    -- without descending into a base). Shared by 'carriedTypes' below and the
    -- 'LoopS_' handler.
    firstContinue :: LoopBody ne se -> Maybe [ne]
    firstContinue (LoopContinue nes) = Just nes
    firstContinue (LoopIf _ a b) = case firstContinue a of
      (Just x) -> Just x
      Nothing -> firstContinue b
    firstContinue (LoopNLet _ _ b) = firstContinue b
    firstContinue (LoopSLet _ _ b) = firstContinue b
    firstContinue (LoopBase _) = Nothing

    -- Native types of loop-carried slots, read positionally from the continue
    -- value that reassigns each slot. Requires the wired body (native leaves).
    carriedTypes :: [Int] -> LoopBody NativeExpr se -> Maybe (Map.Map Int TypeF)
    carriedTypes ids body = Map.fromList . zip ids . map typeFof <$> firstContinue body

    -- 'carriedTypes' resolved on a manifold body spine; 'Nothing' if the body has
    -- no native loop. The loop sits on the spine ('addLoopWraps' makes it the
    -- whole body, possibly under structural wrappers).
    loopCarriedTypes :: SerialExpr -> Maybe (Map.Map Int TypeF)
    loopCarriedTypes (LoopS _ ids body) = carriedTypes ids body
    loopCarriedTypes (ReturnS x) = loopCarriedTypes x
    loopCarriedTypes (SerialLetS _ _ x) = loopCarriedTypes x
    loopCarriedTypes (NativeLetS _ _ x) = loopCarriedTypes x
    loopCarriedTypes (CacheBodyS _ _ _ _ _ x) = loopCarriedTypes x
    loopCarriedTypes (DebugWrapS _ _ _ x) = loopCarriedTypes x
    loopCarriedTypes _ = Nothing

    -- Make the continue-derived native type authoritative for every serial
    -- carried slot: recover the type of a serial-only slot (which 'prepareArg'
    -- left 'L PassthroughS') and override a stale base-occurrence type with the
    -- plain type the continue actually circulates. Function-typed slots
    -- (native-only closures, a carried suspension among them) are left alone.
    patchCarriedForm ::
      Map.Map Int TypeF ->
      ManifoldForm (Or TypeS TypeF) TypeS ->
      ManifoldForm (Or TypeS TypeF) TypeS
    patchCarriedForm carriedTM = afirst patch
      where
        patch i (L _) | Just tf <- Map.lookup i carriedTM, not (isFunF tf) = L (SerialS tf)
        patch _ orT = orT
        isFunF (FunF {}) = True
        isFunF _ = False

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
    -- Native-loop reconciliation. The default 'monoidSerialExpr' rebuilds a
    -- 'LoopS' body verbatim, skipping the serial<->native wiring the other cases
    -- get. Three fixes:
    --   (c) an internal 'LoopSLet' consumed natively downstream (a foreign-call
    --       result destructured by a '.0'/'.1' projection) is naturalized,
    --       mirroring the non-loop 'SerialLetS_' reconciliation.
    --   (b) a carried slot used serially (a foreign-call argument, read by index
    --       's<i>') is stale after the first iteration -- the entry packet is
    --       never reassigned. Re-serialize the CURRENT native value at the top of
    --       every iteration ('LoopSLet i (serialize (BndVarN i))'), shadowing the
    --       entry packet so the index-based foreign-call read is fresh. The
    --       carried native type comes from the continue value that reassigns the
    --       slot ('wireSerialManifold' patches the form + forces 'letWrap' so the
    --       'nvarNamer' local actually exists).
    --   (a) force every carried slot 'NativeContent' so 'letWrap' deserializes
    --       each entry packet into that native local.
    wireSerialExpr (LoopS_ t ids body) = do
      (mergedReq, body') <- wireLoopBody body
      let carriedTM = maybe Map.empty id (carriedTypes ids body')
          serialUsed = [i | i <- Map.keys carriedTM, serialish (Map.lookup i mergedReq)]
      -- (b) Re-serialize each serially-used carried slot from its CURRENT native
      -- value at the top of every iteration, shadowing the stale entry packet: the
      -- foreign call reads its args by index 's<i>' (see the loop-carry lowering
      -- in Grammars/Translator/Generic.hs 'lcMakeLoop'), so the packet must be
      -- refreshed from the reassigned 'nvarNamer' local.
      body'' <-
        foldlM
          ( \b i ->
              let tf = carriedTM Map.! i
               in (\se -> LoopSLet i se b) <$> serializeS "loop-reserialize" m0 tf (BndVarN tf i)
          )
          body'
          serialUsed
      -- (a) force every carried slot 'NativeContent' so 'letWrap' deserializes
      -- each entry packet into the native local the continue reassigns.
      let req' = Map.union (Map.fromList [(i, NativeContent) | i <- ids]) mergedReq
      return (req', LoopS t ids body'')
      where
        serialish (Just SerialContent) = True
        serialish (Just NativeAndSerialContent) = True
        serialish _ = False

        serialInner (SerialS tf) = Just tf
        serialInner (FunctionS _ (SerialS tf)) = Just tf
        serialInner _ = Nothing

        -- Function values have no wire form, so a serially-requested native
        -- loop-local of function type must stay native (mirrors the guards in
        -- 'patchCarriedForm' / 'specialize').
        isFunF (FunF {}) = True
        isFunF _ = False

        -- Wire the loop body bottom-up, returning the merged request map of the
        -- subtree alongside the rewired body (so a 'LoopSLet's downstream request
        -- is the child's returned map -- no per-node re-fold). (c) An internal
        -- 'LoopSLet' consumed natively downstream is naturalized, mirroring the
        -- non-loop 'SerialLetS_' reconciliation.
        wireLoopBody (LoopIf (rc, ne) tb eb) = do
          (rt, tb') <- wireLoopBody tb
          (re, eb') <- wireLoopBody eb
          return (Map.unionsWith (<>) [rc, rt, re], LoopIf ne tb' eb')
        -- Native->serial reconciliation (mirrors the non-loop 'NativeLetS_', and
        -- the opposite of the 'LoopSLet' naturalize above): a native loop-local
        -- consumed serially downstream -- e.g. a destructured tuple component
        -- ('.0'/'.1' of a carried tuple) passed to a foreign-call leaf, which the
        -- emitter reads by index 's<i>' -- needs a serial form bound at its own
        -- (branch-local) scope. It cannot be hoisted to the loop top like a
        -- carried slot ('serialUsed'), because the local does not exist there.
        wireLoopBody (LoopNLet i (rn, ne) b) = do
          (rb, b') <- wireLoopBody b
          leaf <- case Map.lookup i rb of
            (Just SerialContent) | not (isFunF (typeFof ne)) ->
              (\se -> LoopSLet i se b') <$> serializeS "loop-nat-to-ser" m0 (typeFof ne) ne
            (Just NativeAndSerialContent) | not (isFunF (typeFof ne)) -> do
              sv <- serializeS "loop-nat-to-ser" m0 (typeFof ne) (LetVarN (typeFof ne) i)
              return (LoopNLet i ne (LoopSLet i sv b'))
            _ -> return (LoopNLet i ne b')
          return (Map.unionWith (<>) rn rb, leaf)
        wireLoopBody (LoopSLet i (rs, se) b) = do
          (rb, b') <- wireLoopBody b
          leaf <- case (Map.lookup i rb, serialInner (typeSof se)) of
            (Just NativeContent, Just tf) ->
              (\ne1 -> LoopNLet i ne1 b') <$> naturalizeN "loop-nat" m0 lang tf se
            (Just NativeAndSerialContent, Just tf) -> do
              ne1 <- naturalizeN "loop-nat" m0 lang tf (LetVarS (Just tf) i)
              return (LoopSLet i se (LoopNLet i ne1 b'))
            _ -> return (LoopSLet i se b')
          return (Map.unionWith (<>) rs rb, leaf)
        wireLoopBody (LoopBase (rb, se)) = return (rb, LoopBase se)
        wireLoopBody (LoopContinue nes) =
          return (Map.unionsWith (<>) (map fst nes), LoopContinue (map snd nes))
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
      -- A native-only arg (no serial side) -- e.g. a captured function value,
      -- which has no wire form -- must stay native regardless of the request.
      -- Downgrading it to Passthrough would make a later pass try to serialize
      -- the closure.
      (_, R t) -> R t
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
