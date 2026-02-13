{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.CodeGenerator.Serial
Description : Process serialization trees
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc.CodeGenerator.Serial
  ( makeSerialAST
  , chooseSerializationCycle
  , isSerializable
  , prettySerialOne
  , serialAstToType
  , shallowType
  , serialAstToMsgpackSchema
  , encode64
  , decode64
  ) where

import qualified Data.Char as C
import qualified Data.Text as DT
import qualified Morloc.BaseTypes as BT
import Morloc.CodeGenerator.Infer
import Morloc.CodeGenerator.Namespace
import Morloc.Data.Doc
import qualified Morloc.Data.Map as Map
import qualified Morloc.Monad as MM
import qualified Morloc.TypeEval as TE
import Morloc.Typecheck.Internal (apply, qualify, substitute, subtype, unqualify)

-- The type of serialization data as JSON, currently
serialType :: Lang -> CVar
serialType Python3Lang = CV "str"
serialType RLang = CV "character"
serialType CppLang = CV "std::string"
serialType _ = error "Ah hell, you know I don't know that language"

-- | recurse all the way to a serializable type
serialAstToType :: SerialAST -> TypeF
serialAstToType (SerialPack _ (_, s)) = serialAstToType s
serialAstToType (SerialList v s) = AppF (VarF v) [serialAstToType s]
serialAstToType (SerialTuple v ss) = AppF (VarF v) (map serialAstToType ss)
serialAstToType (SerialObject o n ps rs) =
  let ts = map (serialAstToType . snd) rs
   in NamF o n ps (zip (map fst rs) ts)
serialAstToType (SerialReal x) = VarF x
serialAstToType (SerialFloat32 x) = VarF x
serialAstToType (SerialFloat64 x) = VarF x
serialAstToType (SerialInt x) = VarF x
serialAstToType (SerialInt8 x) = VarF x
serialAstToType (SerialInt16 x) = VarF x
serialAstToType (SerialInt32 x) = VarF x
serialAstToType (SerialInt64 x) = VarF x
serialAstToType (SerialUInt x) = VarF x
serialAstToType (SerialUInt8 x) = VarF x
serialAstToType (SerialUInt16 x) = VarF x
serialAstToType (SerialUInt32 x) = VarF x
serialAstToType (SerialUInt64 x) = VarF x
serialAstToType (SerialBool x) = VarF x
serialAstToType (SerialString x) = VarF x
serialAstToType (SerialNull x) = VarF x
-- passthrough type, it cannot be deserialized or serialized, only passed in from a different language
serialAstToType (SerialUnknown v) = UnkF v

encode64 :: Int -> String
encode64 i
  | i < 0 = error "Negative size - not in this universe my dear"
  | i < 10 = [C.chr (C.ord '0' + i)] -- 0-9
  | i < 36 = [C.chr (C.ord 'a' + i - 10)]
  | i < 62 = [C.chr (C.ord 'A' + i - 36)]
  | i == 62 = "+"
  | i == 63 = "/"
  | otherwise = "=" <> (encode64 (mod i 64)) <> (encode64 (div i 64))

decode64 :: String -> Int
decode64 (x : xs)
  | x >= '0' && x <= '9' = C.ord x - C.ord '0'
  | x >= 'a' && x <= 'z' = C.ord x - C.ord 'a' + C.ord '0'
  | x >= 'A' && x <= 'Z' = C.ord x - C.ord 'A' + C.ord 'a' + C.ord '0'
  | x == '+' = 62
  | x == '/' = 63
  | x == '=' = decode64 [head xs] + 64 * decode64 (tail xs)
  | otherwise = error "illegal character"
decode64 [] = 0

encode64D :: Int -> MDoc
encode64D i = pretty (encode64 i)

serialAstToMsgpackSchema :: SerialAST -> MDoc
serialAstToMsgpackSchema (SerialPack v (_, s)) = addHint v <> serialAstToMsgpackSchema s
serialAstToMsgpackSchema (SerialList v s) = addHint v <> "a" <> serialAstToMsgpackSchema s
serialAstToMsgpackSchema (SerialTuple v ss) = addHint v <> "t" <> encode64D (length ss) <> foldl (<>) "" (map serialAstToMsgpackSchema ss)
serialAstToMsgpackSchema (SerialObject _ v _ rs) = addHint v <> "m" <> encode64D (length rs) <> foldl (<>) "" (map keypair rs)
  where
    keypair :: (Key, SerialAST) -> MDoc
    keypair (k, s) = (encode64D . DT.length . unKey $ k) <> pretty (unKey k) <> serialAstToMsgpackSchema s
serialAstToMsgpackSchema (SerialReal v) = addHint v <> "f8" -- 64 bit float
serialAstToMsgpackSchema (SerialFloat32 v) = addHint v <> "f4"
serialAstToMsgpackSchema (SerialFloat64 v) = addHint v <> "f8"
serialAstToMsgpackSchema (SerialInt v) = addHint v <> "i4" -- 32 bit integer, will need to extend this soon
serialAstToMsgpackSchema (SerialInt8 v) = addHint v <> "i1"
serialAstToMsgpackSchema (SerialInt16 v) = addHint v <> "i2"
serialAstToMsgpackSchema (SerialInt32 v) = addHint v <> "i4"
serialAstToMsgpackSchema (SerialInt64 v) = addHint v <> "i8"
serialAstToMsgpackSchema (SerialUInt v) = addHint v <> "u4"
serialAstToMsgpackSchema (SerialUInt8 v) = addHint v <> "u1"
serialAstToMsgpackSchema (SerialUInt16 v) = addHint v <> "u2"
serialAstToMsgpackSchema (SerialUInt32 v) = addHint v <> "u4"
serialAstToMsgpackSchema (SerialUInt64 v) = addHint v <> "u8"
serialAstToMsgpackSchema (SerialBool v) = addHint v <> "b"
serialAstToMsgpackSchema (SerialString v) = addHint v <> "s"
serialAstToMsgpackSchema (SerialNull v) = addHint v <> "z"
serialAstToMsgpackSchema (SerialUnknown v) = addHint v <> "?" -- I guess this works as a general bad new character?

addHint :: FVar -> MDoc
addHint (FV _ (CV "")) = "" -- no hint if no concrete type is defined
-- this is helpful in the nexus
addHint (FV _ (CV v)) = "<" <> pretty v <> ">"

-- | get only the toplevel type
shallowType :: SerialAST -> TypeF
shallowType (SerialPack _ (p, _)) = typePackerPacked p
shallowType (SerialList v s) = AppF (VarF v) [shallowType s]
shallowType (SerialTuple v ss) = AppF (VarF v) $ map shallowType ss
shallowType (SerialObject o n ps rs) =
  let ts = map (shallowType . snd) rs
   in NamF o n ps (zip (map fst rs) ts)
shallowType (SerialReal x) = VarF x
shallowType (SerialFloat32 x) = VarF x
shallowType (SerialFloat64 x) = VarF x
shallowType (SerialInt x) = VarF x
shallowType (SerialInt8 x) = VarF x
shallowType (SerialInt16 x) = VarF x
shallowType (SerialInt32 x) = VarF x
shallowType (SerialInt64 x) = VarF x
shallowType (SerialUInt x) = VarF x
shallowType (SerialUInt8 x) = VarF x
shallowType (SerialUInt16 x) = VarF x
shallowType (SerialUInt32 x) = VarF x
shallowType (SerialUInt64 x) = VarF x
shallowType (SerialBool x) = VarF x
shallowType (SerialString x) = VarF x
shallowType (SerialNull x) = VarF x
shallowType (SerialUnknown v) = UnkF v

findPackers ::
  Lang ->
  MorlocMonad
    ( [(([TVar], TypeU), Source)]
    , [(([TVar], TypeU), Source)]
    )
findPackers lang = do
  sigmap <- MM.gets stateTypeclasses

  MM.sayVVV $
    "findPackers"
      <> "\n  sigmap:" <+> viaShow sigmap

  packers <- case Map.lookup (EV "pack") sigmap of
    (Just (Instance _ _ _ ts)) -> return $ concatMap f ts
    Nothing -> return []

  unpackers <- case Map.lookup (EV "unpack") sigmap of
    (Just (Instance _ _ _ ts)) -> return $ concatMap f ts
    Nothing -> return []

  return (packers, unpackers)
  where
    f :: TermTypes -> [(([TVar], TypeU), Source)]
    f (TermTypes (Just et) (map (val . snd) -> srcs) _) =
      let (vs, t) = unqualify $ etype et
       in [((vs, t), src) | src <- srcs, srcLang src == lang]
    f (TermTypes Nothing _ _) = []

-- Takes a map of packers with concrete type names as keys. A single concrete
-- type name may map to many single types. For example, the python type "dict"
-- might represent a Map with homogenous keys and values or many things that
-- might be objects in other languages. Similarly, the python "tuple" type maps
-- to tuples of all sizes -- each of which is a different type in both the
-- morloc general type system and many other languages. So the map contains a
-- list of possible packers. Matching the concrete type name to the right packer
-- will be done through subtyping.
makeSerialAST :: Int -> Lang -> TypeF -> MorlocMonad SerialAST
makeSerialAST m lang t0 = do
  -- ([(([TVar], TypeU), Source)], ...)
  (packs, unpacks) <- findPackers lang

  MM.sayVVV $ "packs:" <+> viaShow packs
  MM.sayVVV $ "unpacks:" <+> viaShow unpacks

  (_, gscope) <- getScope m lang

  -- Map TVar ((TypeU, Source), (TypeU, Source))
  let typepackers =
        Map.fromListWith
          (<>)
          [ (extractKey b1, [(length vs1, qualify vs1 a1, qualify vs1 b1, src1, src2)])
          | ((vs1, FunU [a1] b1), src1) <- packs
          , ((vs2, FunU [a2] _), src2) <- unpacks
          , extractKey b1 == extractKey a2
          , length vs1 == length vs2
          ]

  makeSerialAST' gscope typepackers t0
  where
    makeSerialAST' ::
      Scope ->
      Map.Map TVar [(Int, TypeU, TypeU, Source, Source)] ->
      TypeF ->
      MorlocMonad SerialAST
    -- If the type is unknown in this language, then it must be a passthrough
    -- type. So it will only be represented in the serialization form. As a
    -- string, for now.
    makeSerialAST' _ _ (UnkF (FV gv _)) = return $ SerialUnknown (FV gv (serialType lang))
    makeSerialAST' gscope typepackers ft@(VarF v@(FV gv cv))
      | finalType == BT.unitU = return $ SerialNull v
      | finalType == BT.boolU = return $ SerialBool v
      | finalType == BT.strU = return $ SerialString v
      | finalType == BT.realU = return $ SerialReal v
      | finalType == BT.f32U = return $ SerialFloat32 v
      | finalType == BT.f64U = return $ SerialFloat64 v
      | finalType == BT.intU = return $ SerialInt v
      | finalType == BT.i8U = return $ SerialInt8 v
      | finalType == BT.i16U = return $ SerialInt16 v
      | finalType == BT.i32U = return $ SerialInt32 v
      | finalType == BT.i64U = return $ SerialInt64 v
      | finalType == BT.uintU = return $ SerialUInt v
      | finalType == BT.u8U = return $ SerialUInt8 v
      | finalType == BT.u16U = return $ SerialUInt16 v
      | finalType == BT.u32U = return $ SerialUInt32 v
      | finalType == BT.u64U = return $ SerialUInt64 v
      | otherwise = case Map.lookup gv typepackers of
          (Just ps) -> do
            packers <- mapM makeTypePacker ps
            unpacked <- mapM (makeSerialAST' gscope typepackers . typePackerUnpacked) packers
            selection <- selectPacker (zip packers unpacked)
            return $ SerialPack v selection
          Nothing -> MM.throwSourcedError m $
              "Cannot find constructor in VarF" <+> dquotes (pretty v) <+> " finalType=" <> pretty finalType
      where
        finalType =
          let t = fst $ unweaveTypeF ft
           in either (const t) id (TE.evaluateType gscope t)

        makeTypePacker :: (Int, TypeU, TypeU, Source, Source) -> MorlocMonad TypePacker
        makeTypePacker (0, generalUnpackedType, generalPackedType, forwardSource, reverseSource) = do
          packedType <- inferConcreteType lang (Idx m (typeOf generalPackedType))
          unpackedType <- inferConcreteType lang (Idx m (typeOf generalUnpackedType))
          return $
            TypePacker
              { typePackerPacked = packedType
              , typePackerUnpacked = unpackedType
              , typePackerForward = forwardSource
              , typePackerReverse = reverseSource
              }
        makeTypePacker (nparam, _, _, _, _) =
          MM.throwSourcedError m $ "Unexpected parameters for atomic variable:" <+> pretty nparam

        -- Select the first packer we happen across. This is a very key step and
        -- eventually this function should be replaced with one more carefully
        -- considered. But for now, I don't have any great criterion for
        -- choosing.
        selectPacker :: [(TypePacker, SerialAST)] -> MorlocMonad (TypePacker, SerialAST)
        selectPacker [] = MM.throwSourcedError m $ "Cannot find constructor for" <+> pretty cv <+> "in selectPacker"
        selectPacker [x] = return x
        selectPacker _ = MM.throwSourcedError m "Two you say, oh, get out of here"
    makeSerialAST' _ _ t@(FunF _ _) =
      MM.throwSourcedError m $ "Cannot serialize functions at" <+> pretty m <> ":" <+> pretty t
    makeSerialAST' gscope typepackers ft@(AppF (VarF fv@(FV generalTypeName _)) ts@(firstType : _))
      | finalVar == Just BT.list = SerialList fv <$> makeSerialAST' gscope typepackers firstType
      | finalVar == Just (BT.tuple (length ts)) =
          SerialTuple fv <$> mapM (makeSerialAST' gscope typepackers) ts
      | otherwise = case Map.lookup generalTypeName typepackers of
          (Just ps) -> do
            packers <- catMaybes <$> mapM (resolvePacker lang m ft) ps
            unpacked <- mapM (makeSerialAST' gscope typepackers . typePackerUnpacked) packers
            selection <- selectPacker (zip packers unpacked)
            return $ SerialPack fv selection
          Nothing ->
            MM.throwSourcedError m $
              "Cannot find" <+> pretty generalTypeName <+> "from" <+> dquotes (pretty fv)
                <> "\n  ft:" <+> pretty ft
                <> "\n  finalVar:" <+> pretty finalVar
                <> "\n  gscope:" <+> viaShow gscope
                <> "\n  general t:" <+> (viaShow . fst $ unweaveTypeF ft)
                <> "\n  concrete t:" <+> (viaShow . snd $ unweaveTypeF ft)
                <> "\n  typepackers:" <+> viaShow typepackers
      where
        basevar :: TypeU -> Maybe TVar
        basevar (VarU v) = Just v
        basevar (ExistU _ _ _) = Nothing
        basevar (ForallU _ _) = Nothing
        basevar (FunU _ _) = Nothing
        basevar (AppU t _) = basevar t
        basevar (NamU _ v _ _) = Just v

        finalVar =
          let t = fst $ unweaveTypeF ft
           in basevar $ either (const t) id (TE.evaluateType gscope t)

        selectPacker :: [(TypePacker, SerialAST)] -> MorlocMonad (TypePacker, SerialAST)
        selectPacker [] =
          MM.throwSourcedError m $
            "Cannot find constructor in selectPacker for" <+> pretty ft
              <> "\n  ft:" <+> pretty ft
              <> "\n  generalTypeName (key):" <+> pretty generalTypeName
              <> "\n  typepackers:" <+> viaShow typepackers
              <> "\n  Map.lookup generalTypeName typepackers:" <+> viaShow (Map.lookup generalTypeName typepackers)
        selectPacker (x : _) = return x
    makeSerialAST' gscope typepackers (NamF o n ps rs) = do
      ts <- mapM (makeSerialAST' gscope typepackers . snd) rs
      return $ SerialObject o n ps (zip (map fst rs) ts)
    makeSerialAST' _ _ t = MM.throwSourcedError m $ "makeSerialAST' error on type:" <+> pretty t

resolvePacker ::
  Lang ->
  Int ->
  TypeF ->
  (Int, TypeU, TypeU, Source, Source) ->
  MorlocMonad (Maybe TypePacker)
resolvePacker lang m0 resolvedType@(AppF _ _) (_, unpackedGeneralType, packedGeneralType, srcPacked, srcUnpacked) = do
  packedConcreteType <- inferConcreteTypeU lang (Idx m0 packedGeneralType)
  unpackedConcreteType <- inferConcreteTypeU lang (Idx m0 unpackedGeneralType)
  maybeUnpackedType <-
    resolveP
      resolvedType
      packedConcreteType
      unpackedConcreteType
      (packedGeneralType, unpackedGeneralType)

  case maybeUnpackedType of
    (Just unpackedType) ->
      return . Just $
        TypePacker
          { typePackerPacked = resolvedType
          , typePackerUnpacked = unpackedType
          , typePackerForward = srcPacked
          , typePackerReverse = srcUnpacked
          }
    Nothing -> return Nothing
  where
    -- Both sides of the packer function are guaranteed to have the same
    -- generic values, this is guaranteed by the implementation of
    -- Restructure.hs. So it is sufficient to resolve the generics in the packed
    -- type and map them to the unpacked type.
    --
    -- Example:
    --
    --  resolveP ("dict" "str" "int") ("dict" a b) ("list" ("list" a b) --> ("list" ("list" "str" "int"))
    --                    x_r             x_u                y_u                       y_r
    --
    -- x_u is the unresolved packed type that is extracted before typechecking
    -- x_r is equal to x_u after type inference
    --
    -- () |- x_u <: x_y -| g
    -- y_r = apply g y_u
    --
    -- y_u is the unresolved unpacked type that is extracted with x_u
    --
    -- y_u and y_r are both processed by Restructure.hs and are both guaranteed
    -- to share the same set of generics. We can find the identity of these
    -- generics by subtyping x_u against x_y. The produced context contains
    -- the types for each generic variable. The context can be applied to
    -- y_u to get the final desired y_r.
    resolveP ::
      TypeF -> -- resolved packed type (e.g., "dict" "str" "int")
      TypeU -> -- unresolved packed type (e.g., "dict" a b)
      TypeU -> -- unresolved unpacked type (e.g., "list" ("list" a b))
      (TypeU, TypeU) -> -- The general unresolved packed and unpacked types
      MorlocMonad (Maybe TypeF) -- the resolved unpacked types
    resolveP a b c generalTypes = do
      let (ga, ca) = unweaveTypeF a
      unpackedConcreteType <- case subtype Map.empty b ca (Gamma 0 [] Map.empty) of
        (Left typeErr) ->
          MM.throwSourcedError m0 $
            "There was an error raised in subtyping while resolving serialization"
              <> "\nThe packer involved maps the type:"
              <> "\n  "
              <> (pretty . fst) generalTypes
              <> "\n\nTo the serialized form:"
              <> "\n  "
              <> (pretty . snd) generalTypes
              <> "\n\nHere the unresolved concrete packed type:"
              <> "\n  b:" <+> pretty b
              <> "\n\nShould be the subtype of the resolved packed type:"
              <> "\n  a:" <+> pretty a
              <> "\n\nThe generic terms in b should be resolved through subtyping and used to resolve the unpacked type:"
              <> "\n  c:" <+> pretty c
              <> "\n\nHowever, the b <: a step failed:\n"
              <> typeErr
              <> "\n\nThe packer function may not be generic enough to pack the type you specify, if this is the case, you may need to simplify the datatype"
        (Right g) -> do
          return (apply g (existential c))

      maybeUnpackedGeneralType <- case generalTypes of
        (u, gc) -> do
          -- where u  is the unresolved general packed type that was stored in Desugar.hs
          --       gc is the unresolved general unpacked type
          case subtype Map.empty u ga (Gamma 0 [] Map.empty) of
            (Left _) -> return Nothing
            (Right g) -> do
              return . Just $ apply g (existential gc)

      return $ case maybeUnpackedGeneralType of
        (Just resolvedUnpackedGeneralType) -> Just $ weaveTypeF resolvedUnpackedGeneralType unpackedConcreteType
        Nothing -> Nothing

    -- Replaces each generic term with an existential term of the same name
    existential :: TypeU -> TypeU
    existential (ForallU v t0) = substitute v (existential t0)
    existential t0 = t0
resolvePacker _ m0 _ _ = MM.throwSourcedError m0 $ "No packer found for this type"

cv2tv :: CVar -> TVar
cv2tv (CV x) = TV x

tv2cv :: TVar -> CVar
tv2cv (TV x) = CV x

unweaveTypeF :: TypeF -> (TypeU, TypeU)
unweaveTypeF (UnkF (FV gv cv)) = (VarU gv, VarU (cv2tv cv))
unweaveTypeF (VarF (FV gv cv)) = (VarU gv, VarU (cv2tv cv))
unweaveTypeF (FunF ts t) =
  let (gt, ct) = unweaveTypeF t
      (gts, cts) = unzip $ map unweaveTypeF ts
   in (FunU gts gt, FunU cts ct)
unweaveTypeF (AppF t ts) =
  let (gt, ct) = unweaveTypeF t
      (gts, cts) = unzip $ map unweaveTypeF ts
   in (AppU gt gts, AppU ct cts)
unweaveTypeF (NamF n (FV gv cv) ps rs) =
  let (psg, psc) = unzip $ map unweaveTypeF ps
      keys = map fst rs
      (vsg, vsc) = unzip $ map (unweaveTypeF . snd) rs
   in (NamU n gv psg (zip keys vsg), NamU n (cv2tv cv) psc (zip keys vsc))

weaveTypeF :: TypeU -> TypeU -> TypeF
weaveTypeF (VarU gv) (VarU cv) = VarF (FV gv (tv2cv cv))
weaveTypeF (FunU tsg tg) (FunU tsc tc) = FunF (zipWith weaveTypeF tsg tsc) (weaveTypeF tg tc)
weaveTypeF (AppU tg tsg) (AppU tc tsc) = AppF (weaveTypeF tg tc) (zipWith weaveTypeF tsg tsc)
weaveTypeF (NamU n gv psg rsg) (NamU _ cv psc rsc) =
  NamF
    n
    (FV gv (tv2cv cv))
    (zipWith weaveTypeF psg psc)
    ( zip
        (map fst rsg)
        (zipWith weaveTypeF (map snd rsg) (map snd rsc))
    )
weaveTypeF ((ExistU gv _ _)) (ExistU cv _ _) = UnkF (FV gv (tv2cv cv))
weaveTypeF gt ct = error . show $ (gt, ct)

{- | Given a list of possible ways to (de)serialize data between two languages,
choose one (or none if the list is empty). Currently I just take the first
in the list, but different cycles may have very different performance, so
this will be an important optimization step later on.
-}
chooseSerializationCycle ::
  [(SerialAST, SerialAST)] ->
  Maybe (SerialAST, SerialAST)
chooseSerializationCycle [] = Nothing
chooseSerializationCycle (x : _) = Just x

{- | Determine if a SerialAST can be directly translated to JSON, if not it
will need to be further reduced.
-}
isSerializable :: SerialAST -> Bool
isSerializable (SerialPack _ _) = False
isSerializable (SerialList _ x) = isSerializable x
isSerializable (SerialTuple _ xs) = all isSerializable xs
isSerializable (SerialObject _ _ _ rs) = all (isSerializable . snd) rs
isSerializable (SerialReal _) = True
isSerializable (SerialFloat32 _) = True
isSerializable (SerialFloat64 _) = True
isSerializable (SerialInt _) = True
isSerializable (SerialInt8 _) = True
isSerializable (SerialInt16 _) = True
isSerializable (SerialInt32 _) = True
isSerializable (SerialInt64 _) = True
isSerializable (SerialUInt _) = True
isSerializable (SerialUInt8 _) = True
isSerializable (SerialUInt16 _) = True
isSerializable (SerialUInt32 _) = True
isSerializable (SerialUInt64 _) = True
isSerializable (SerialBool _) = True
isSerializable (SerialString _) = True
isSerializable (SerialNull _) = True
isSerializable (SerialUnknown _) = True -- are you feeling lucky?

prettySerialOne :: SerialAST -> MDoc
prettySerialOne (SerialPack _ _) = "SerialPack"
prettySerialOne (SerialList v x) = "SerialList" <> angles (pretty v) <> parens (prettySerialOne x)
prettySerialOne (SerialTuple v xs) = "SerialTuple" <> angles (pretty v) <> tupled (map prettySerialOne xs)
prettySerialOne (SerialObject r _ _ rs) =
  block 4 ("SerialObject@" <> viaShow r) $
    vsep (map (\(k, v) -> parens (viaShow k) <> "=" <> prettySerialOne v) rs)
prettySerialOne (SerialReal _) = "SerialReal"
prettySerialOne (SerialFloat32 _) = "SerialFloat32"
prettySerialOne (SerialFloat64 _) = "SerialFloat64"
prettySerialOne (SerialInt _) = "SerialInt"
prettySerialOne (SerialInt8 _) = "SerialInt8"
prettySerialOne (SerialInt16 _) = "SerialInt16"
prettySerialOne (SerialInt32 _) = "SerialInt32"
prettySerialOne (SerialInt64 _) = "SerialInt64"
prettySerialOne (SerialUInt _) = "SerialUInt"
prettySerialOne (SerialUInt8 _) = "SerialUInt8"
prettySerialOne (SerialUInt16 _) = "SerialUInt16"
prettySerialOne (SerialUInt32 _) = "SerialUInt32"
prettySerialOne (SerialUInt64 _) = "SerialUInt64"
prettySerialOne (SerialBool _) = "SerialBool"
prettySerialOne (SerialString _) = "SerialString"
prettySerialOne (SerialNull _) = "SerialNull"
prettySerialOne (SerialUnknown _) = "SerialUnknown"
