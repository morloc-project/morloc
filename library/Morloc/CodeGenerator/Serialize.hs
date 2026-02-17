{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.CodeGenerator.Serialize
Description : Insert serialization/deserialization operations at language boundaries
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc.CodeGenerator.Serialize
  ( serialize
  ) where

import Morloc.CodeGenerator.Infer
import Morloc.CodeGenerator.Namespace
import Morloc.Data.Doc
import qualified Morloc.Data.Map as Map
import qualified Morloc.Monad as MM
import qualified Morloc.CodeGenerator.Serial as Serial

-- | This step is performed after segmentation, so all terms are in the same
-- language. Here we need to determine where inputs are (de)serialized and the
-- serialization states of arguments and variables.
serialize :: MonoHead -> MorlocMonad SerialManifold
serialize (MonoHead lang m0 args0 headForm0 e0) = do
  form0 <- ManifoldFull <$> mapM prepareArg args0

  MM.sayVVV $
    "In serialize for" <+> "m"
      <> pretty m0 <+> pretty lang <+> "segment"
      <> "\n  form0:" <+> pretty form0
      <> "\n  typemap:" <+> viaShow typemap
      <> "\n  This map we made from the expression:\n  "
      <> pretty e0

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
    serialExpr _ (MonoManifold m form e) = do
      MM.sayVVV $ "serialExpr MonoManifold m" <> pretty m <> parens (pretty form)
      serialExpr m e
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
    serialExpr _ (MonoBndVar (A _) i) = return $ BndVarS Nothing i
    serialExpr _ (MonoBndVar (B _) i) =
      case Map.lookup i typemap of
        (Just (Right t)) -> BndVarS <$> fmap Just (inferType t) <*> pure i
        _ -> return $ BndVarS Nothing i
    serialExpr _ (MonoBndVar (C t) i) = BndVarS <$> fmap Just (inferType t) <*> pure i
    serialExpr _ (MonoExe _ _) = error "Can represent MonoSrc as SerialExpr"
    serialExpr _ MonoPoolCall {} = error "MonoPoolCall does not map to a SerialExpr"
    serialExpr _ (MonoApp MonoManifold {} _) = error "Illegal?"
    serialExpr m e = nativeExpr m e >>= serializeS "serialE e" m

    serialArg ::
      Int ->
      MonoExpr ->
      MorlocMonad SerialArg
    serialArg _ e@(MonoManifold m form _) = do
      MM.sayVVV $ "serialArg MonoManifold m" <> pretty m <> parens (pretty form)
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
    nativeArg _ e@(MonoManifold m form _) = do
      MM.sayVVV $ "nativeArg MonoManifold m" <> pretty m <> parens (pretty form)
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
    nativeExpr _ (MonoManifold m form e) = do
      MM.sayVVV $ "nativeExpr MonoManifold m" <> pretty m <> parens (pretty form)
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
    nativeExpr m (MonoApp (MonoExe (Idx idx t0) exe) es) = do
      args <- mapM (nativeArg m) es
      let (inputTypes, outputType) = case t0 of
            FunT its ot -> (its, ot)
            _ -> ([], t0)
      appType <- case drop (length es) inputTypes of
        [] -> inferType (Idx idx outputType)
        remaining -> inferType $ Idx idx (FunT remaining outputType)

      qualifiers <- MM.gets stateTypeQualifier

      let qsAll = maybe [] id (Map.lookup idx qualifiers)
          qs = [(v, t) | (v, t, 1) <- qsAll]

      case [v | (v, _, i) <- qsAll, i > 1] of
        [] -> return ()
        vs -> MM.sayVVV $ "Warning: skipping higher-kinded qualifiers:" <+> list (map pretty vs)

      ftypes <-
        mapM
          ( \t -> do
              inferType (Idx idx (typeOf t))
          )
          (map snd qs)
      let vs = map (unTVar . fst) qs
          qs' = zip vs ftypes

      return $ AppExeN appType exe qs' args
    nativeExpr m e@(MonoApp (MonoPoolCall t _ _ _ _) _) = do
      e' <- serialExpr m e
      t' <- inferType t
      MM.sayVVV $ "nativeExpr MonoApp:" <+> pretty t'
      naturalizeN "nativeE MonoApp" m lang t' e'

    nativeExpr m (MonoApp (MonoLetVar (Idx idx (FunT inputTypes outputType)) i) es) = do
      MM.sayVVV $ "MonoLetVar case"
      args <- mapM (nativeArg m) es
      appType <- case drop (length es) inputTypes of
        [] -> inferType (Idx idx outputType)
        remaining -> inferType $ Idx idx (FunT remaining outputType)
      return $ AppExeN appType (LocalCallP i) [] args
    nativeExpr _ (MonoApp e es) = do
      MM.sayVVV "nativeExprr MonoApp"
      MM.sayVVV $ "e:" <+> pretty e
      MM.sayVVV $ "es:" <+> list (map pretty es)
      error "Illegal application"
    nativeExpr _ (MonoExe t exe) = ExeN <$> inferType t <*> pure exe
    nativeExpr _ (MonoBndVar (A _) _) = error "MonoBndVar must have a type if used in native context"
    nativeExpr _ (MonoBndVar (B _) i) =
      case Map.lookup i typemap of
        (Just (Right t)) -> BndVarN <$> inferType t <*> pure i
        _ -> error "No type found"
    nativeExpr _ (MonoBndVar (C t) i) = BndVarN <$> inferType t <*> pure i
    nativeExpr m (MonoList v t es) =
      ListN
        <$> inferVar v
        <*> inferType t
        <*> mapM (nativeExpr m) es
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
    nativeExpr _ (MonoNull v) = NullN <$> inferVar v
    nativeExpr m (MonoSuspend t e) = SuspendN <$> inferType t <*> nativeExpr m e
    nativeExpr m (MonoForce t e) = ForceN <$> inferType t <*> nativeExpr m e

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
    makeTypemap _ (MonoManifold midx (manifoldBound -> ys) e) =
      Map.union (Map.fromList [(i, Left t) | (Arg i (Just t)) <- ys]) (makeTypemap midx e)
    makeTypemap parentIdx (MonoLet _ e1 e2) = Map.union (makeTypemap parentIdx e1) (makeTypemap parentIdx e2)
    makeTypemap parentIdx (MonoReturn e) = makeTypemap parentIdx e
    makeTypemap parentIdx (MonoForce _ e) = makeTypemap parentIdx e
    makeTypemap parentIdx (MonoSuspend _ e) = makeTypemap parentIdx e
    makeTypemap _ (MonoApp (MonoExe (ann -> idx) _) es) = Map.unionsWith mergeTypes (map (makeTypemap idx) es)
    makeTypemap parentIdx (MonoApp e es) = Map.unionsWith mergeTypes (map (makeTypemap parentIdx) (e : es))
    makeTypemap _ (MonoList (ann -> idx) _ es) = Map.unionsWith mergeTypes (map (makeTypemap idx) es)
    makeTypemap _ (MonoTuple (ann -> idx) (map snd -> es)) = Map.unionsWith mergeTypes (map (makeTypemap idx) es)
    makeTypemap _ (MonoRecord _ (ann -> idx) _ (map (snd . snd) -> es)) = Map.unionsWith mergeTypes (map (makeTypemap idx) es)
    makeTypemap _ _ = Map.empty

    mergeTypes :: Either Type (Indexed Type) -> Either Type (Indexed Type) -> Either Type (Indexed Type)
    mergeTypes (Right t) _ = Right t
    mergeTypes _ (Right t) = Right t
    mergeTypes x _ = x

    serializeS :: MDoc -> Int -> NativeExpr -> MorlocMonad SerialExpr
    serializeS msg m se = do
      MM.sayVVV $ "serializeS" <+> pretty m <> ":" <+> msg
      SerializeS <$> Serial.makeSerialAST m lang (typeFof se) <*> pure se

    inferState :: MonoExpr -> SerializationState
    inferState (MonoApp MonoPoolCall {} _) = Serialized
    inferState (MonoApp MonoExe {} _) = Unserialized
    inferState (MonoApp (MonoManifold _ _ e) _) = inferState e
    inferState (MonoLet _ _ e) = inferState e
    inferState (MonoReturn e) = inferState e
    inferState (MonoManifold _ _ e) = inferState e
    inferState MonoPoolCall {} = Unserialized
    inferState MonoBndVar {} = error "Ambiguous bound term"
    inferState _ = Unserialized

-- | Unwrap structural MonoManifold/MonoReturn wrappers from a let definition.
-- MonoManifold contributes its index (for type lookups); MonoReturn is the
-- manifold's return semantics, which is meaningless in a let-binding context.
unwrapLetDef :: Int -> MonoExpr -> (Int, MonoExpr)
unwrapLetDef _ (MonoManifold m _ (MonoReturn e)) = (m, e)
unwrapLetDef _ (MonoManifold m _ e) = (m, e)
unwrapLetDef m (MonoReturn e) = (m, e)
unwrapLetDef m e = (m, e)

naturalizeN :: MDoc -> Int -> Lang -> TypeF -> SerialExpr -> MorlocMonad NativeExpr
naturalizeN msg m lang t se = do
  MM.sayVVV $ "naturalizeN at" <+> msg
  DeserializeN t <$> Serial.makeSerialAST m lang t <*> pure se

class IsSerializable a where
  serialLet :: Int -> SerialExpr -> a -> a
  nativeLet :: Int -> NativeExpr -> a -> a

instance IsSerializable SerialExpr where
  serialLet = SerialLetS
  nativeLet = NativeLetS

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
    serializeS msg m t se = do
      MM.sayVVV $ "serializeS" <+> pretty m <> ":" <+> msg
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
