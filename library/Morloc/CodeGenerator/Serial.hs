{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

{-|
Module      : Morloc.CodeGenerator.Serial
Description : Process serialization trees
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.CodeGenerator.Serial
  ( makeSerialAST 
  , chooseSerializationCycle
  , isSerializable
  , prettySerialOne
  , serialAstToType
  , serialAstToJsonType
  , shallowType
  ) where

import Morloc.CodeGenerator.Internal
import Morloc.CodeGenerator.Namespace
import qualified Morloc.Monad as MM
import qualified Data.Map as Map
import qualified Morloc.Frontend.Lang.DefaultTypes as Def
import Morloc.Pretty (prettyPackMap)
import Morloc.Data.Doc
import Morloc.Typecheck.Internal (subtype, apply, unqualify, substitute)
import Control.Monad.Except (Except, runExcept, throwError)
import qualified Morloc.Data.Text as MT

defaultSerialConcreteType :: Lang -> MT.Text
defaultSerialConcreteType Python3Lang = "str"
defaultSerialConcreteType RLang = "character"
defaultSerialConcreteType CppLang = "std::string"
defaultSerialConcreteType _ = error "Ah hell, you know I don't know that language"

isPrimitiveType :: (Maybe Lang -> [TypeU]) -> Lang -> MT.Text -> Bool
isPrimitiveType lookupDefault lang concreteName
  = elem concreteName
  $ [ v | (VarU (TV (Just _) v)) <- lookupDefault (Just lang)]

-- | recurse all the way to a serializable type
serialAstToType :: SerialAST -> TypeF
serialAstToType (SerialPack _ (_, s)) = serialAstToType s
serialAstToType (SerialList v s) = AppF (VarF v) [serialAstToType s]
serialAstToType (SerialTuple v ss) = AppF (VarF v) (map serialAstToType ss)
serialAstToType (SerialObject o n ps rs) =
  let ts = map (serialAstToType . snd) rs
  in NamF o n ps (zip (map fst rs) ts)
serialAstToType (SerialReal   x) = VarF x
serialAstToType (SerialInt    x) = VarF x
serialAstToType (SerialBool   x) = VarF x
serialAstToType (SerialString x) = VarF x
serialAstToType (SerialNull   x) = VarF x
-- passthrough type, it cannot be deserialized or serialized, only passed in from a different language
serialAstToType (SerialUnknown v) = UnkF v

serialAstToJsonType :: SerialAST -> JsonType
serialAstToJsonType (SerialPack _ (_, s)) = serialAstToJsonType s
serialAstToJsonType (SerialList (FV _ v) s) = ArrJ v [serialAstToJsonType s]
serialAstToJsonType (SerialTuple (FV _ v) ss) = ArrJ v (map serialAstToJsonType ss)
serialAstToJsonType (SerialObject _ (FV _ n) _ rs) = NamJ n (map (bimap (\(FV _ x) -> x) serialAstToJsonType) rs)
serialAstToJsonType (SerialReal    (FV _ v)) = VarJ v
serialAstToJsonType (SerialInt     (FV _ v)) = VarJ v
serialAstToJsonType (SerialBool    (FV _ v)) = VarJ v
serialAstToJsonType (SerialString  (FV _ v)) = VarJ v
serialAstToJsonType (SerialNull    (FV _ v)) = VarJ v
serialAstToJsonType (SerialUnknown (FV _ v)) = VarJ v -- the unknown type is the serialization type

-- | get only the toplevel type
shallowType :: SerialAST -> TypeF
shallowType (SerialPack _ (p, _)) = typePackerPacked p
shallowType (SerialList v s) = AppF (VarF v) [shallowType s]
shallowType (SerialTuple v ss) = AppF (VarF v) $ map shallowType ss
shallowType (SerialObject o n ps rs) =
  let ts = map (shallowType . snd) rs
  in NamF o n ps (zip (map fst rs) ts)
shallowType (SerialReal   x) = VarF x
shallowType (SerialInt    x) = VarF x
shallowType (SerialBool   x) = VarF x
shallowType (SerialString x) = VarF x
shallowType (SerialNull   x) = VarF x
shallowType (SerialUnknown v) = UnkF v

-- Takes a map of packers with concrete type names as keys. A single concrete
-- type name may map to many single types. For example, the python type "dict"
-- might represent a Map with homogenous keys and values or many things that
-- might be objects in other languages. Similarly, the python "tuple" type maps
-- to tuples of all sizes -- each of which is a different type in both the
-- morloc general type system and many other languages. So the map contains a
-- list of possible packers. Matching the concrete type name to the right packer
-- will be done through subtyping.
makeSerialAST
  :: Map.Map MT.Text [ResolvedPacker]
  -> Lang
  -> TypeF
  -> Except MDoc SerialAST
makeSerialAST packmap lang = makeSerialAST'
  where
    makeSerialAST' :: TypeF -> Except MDoc SerialAST
    -- If the type is unknown in this language, then it must be a passthrough
    -- type. So it will only be represented in the serialization form. As a
    -- string, for now.
    makeSerialAST' (UnkF (FV gv _)) = return $ SerialUnknown (FV gv (defaultSerialConcreteType lang))
    makeSerialAST' (VarF v@(FV _ cv))
      | isPrimitiveType Def.defaultNull   lang cv = return $ SerialNull   v
      | isPrimitiveType Def.defaultBool   lang cv = return $ SerialBool   v
      | isPrimitiveType Def.defaultString lang cv = return $ SerialString v
      | isPrimitiveType Def.defaultReal   lang cv = return $ SerialReal   v
      | isPrimitiveType Def.defaultInt    lang cv = return $ SerialInt    v
      | otherwise = case Map.lookup cv packmap of
          (Just ps) -> do
            packers <- mapM makeTypePacker ps
            unpacked <- mapM (makeSerialAST' . typePackerUnpacked) packers
            selection <- selectPacker (zip packers unpacked)
            return $ SerialPack v selection
          Nothing -> throwError
            $ "Cannot find constructor" <+> dquotes (pretty v)
            <+> "in packmap:\n" <> prettyMap packmap
      where
        makeTypePacker :: ResolvedPacker -> Except MDoc TypePacker
        makeTypePacker u = do
            packedType <- weaveTypes (typeOf . fst <$> resolvedPackerGeneralTypes u) (typeOf (resolvedPackedType u))
            unpackedType <- weaveTypes (typeOf . snd <$> resolvedPackerGeneralTypes u) (typeOf (resolvedUnpackedType u))
            return $ TypePacker
              { typePackerPacked   = typeP2typeF packedType
              , typePackerUnpacked = typeP2typeF unpackedType
              , typePackerForward  = resolvedPackerForward u
              , typePackerReverse  = resolvedPackerReverse u
              }

        -- Select the first packer we happen across. This is a very key step and
        -- eventually this function should be replaced with one more carefully
        -- considered. But for now, I don't have any great criterion for
        -- choosing.
        selectPacker :: [(TypePacker, SerialAST)] -> Except MDoc (TypePacker, SerialAST)
        selectPacker [] = throwError $ "Cannot find constructor for" <+> pretty cv
        selectPacker (x:_) = return x


    makeSerialAST' (FunF _ _)
      = throwError "Cannot serialize functions"
    makeSerialAST' t@(AppF (VarF v@(FV generalTypeName concreteTypeName)) ts@(firstType:_))
      | generalTypeName == Def.listG = SerialList v <$> makeSerialAST' firstType
      | generalTypeName == Def.tupleG (length ts) = SerialTuple v <$> mapM makeSerialAST' ts
      | otherwise = case Map.lookup concreteTypeName packmap of
          (Just ps) -> do
            packers <- catMaybes <$> mapM (resolvePacker lang t) ps
            unpacked <- mapM (makeSerialAST' . typePackerUnpacked) packers
            selection <- selectPacker (zip packers unpacked)
            return $ SerialPack v selection
          Nothing -> throwError
            $ "Cannot find constructor" <+> dquotes (pretty v)
            <> "<" <> pretty (length ts) <> ">"
            <+> "in packmap:\n" <> prettyMap packmap
      where
         selectPacker :: [(TypePacker, SerialAST)] -> Except MDoc (TypePacker, SerialAST)
         selectPacker [] = throwError $ "Cannot find constructor for" <+> pretty t
         selectPacker (x:_) = return x

    makeSerialAST' (NamF o n ps rs) = do
      ts <- mapM (makeSerialAST' . snd) rs
      return $ SerialObject o n ps (zip (map fst rs) ts)
    makeSerialAST' t = throwError $ "makeSerialAST' error on type:" <+> pretty t

resolvePacker :: Lang -> TypeF -> ResolvedPacker -> Except MDoc (Maybe TypePacker)
resolvePacker lang packedType@(AppF _ ts1) p@(unqualify . resolvedPackedType -> (_, AppU _ ts2))
    | length ts1 == length ts2 = do
        unpackedType <- resolveP packedType (resolvedPackedType p)
                                            (resolvedUnpackedType p)
                                            (resolvedPackerGeneralTypes p)
        return . Just $ TypePacker
            { typePackerPacked = packedType
            , typePackerUnpacked = unpackedType
            , typePackerForward = resolvedPackerForward p
            , typePackerReverse = resolvedPackerReverse p
            }
    | otherwise = return Nothing
    where
        -- Both sides of the packer function are guaranteed to have the same
        -- generic values, this is guaranteed by the implementation of
        -- Desugar.hs. So it is sufficient to resolve the generics in the packed
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
        -- y_u and y_r are both processed by Desugar.hs and are both guaranteed
        -- to share the same set of generics. We can find the identity of these
        -- generics by subtyping x_u against x_y. The produced context contains
        -- the types for each generic variable. The context can be applied to
        -- y_u to get the final desired y_r.
        resolveP
            :: TypeF -- resolved packed type (e.g., "dict" "str" "int")
            -> TypeU -- unresolved packed type (e.g., "dict" a b)
            -> TypeU -- unresolved unpacked type (e.g., "list" ("list" a b))
            -> Maybe (TypeU, TypeU) -- The general unresolved packed and unpacked types
            -> Except MDoc TypeF -- the resolved unpacked types
        resolveP a b c generalTypes = do
            let (ca, ga) = unweaveTypeF lang a
            unpackedConcreteType <- case subtype b ca (Gamma 0 []) of
                (Left typeErr) -> throwError
                    $  "There was an error raised in subtyping while resolving serialization"
                    <> "\nThe packer involved maps the type:"
                    <> "\n  " <> maybe "<missing type>" (pretty . fst) generalTypes
                    <> "\n\nTo the serialized form:"
                    <> "\n  " <> maybe "<missing type>" (pretty . snd) generalTypes
                    <> "\n\nHere the unresolved concrete packed type:"
                    <> "\n  b:" <+> pretty b
                    <> "\n\nShould be the subtype of the resolved packed type:"
                    <> "\n  a:" <+> pretty a
                    <> "\n\nThe generic terms in b should be resolved through subtyping and used to resolve the unpacked type:"
                    <> "\n  c:" <+> pretty c
                    <> "\n\nHowever, the b <: a step failed:\n"
                    <> pretty typeErr
                    <> "\n\nThe packer function may not be generic enough to pack the type you specify, if this is the case, you may need to simplify the datatype"
                (Right g) -> do
                    return (apply g (existential c))

            unpackedGeneralType <- case generalTypes of
                (Just (u, gc)) ->
                    -- where r  is the resolved general type (no generics)
                    --       u  is the unresolved general packed type that was stored in Desugar.hs
                    --       gc is the unresolved general unpacked type
                    case subtype u ga (Gamma 0 []) of
                        (Left typeErr) -> throwError $ pretty typeErr
                        (Right g) -> do
                            return . Just $ apply g (existential gc)
                _ -> return Nothing

            weaveTypeF unpackedGeneralType unpackedConcreteType

        unweaveTypeF :: Lang -> TypeF -> (TypeU, TypeU)
        unweaveTypeF l (UnkF (FV gv cv)) = (VarU (TV Nothing gv), VarU (TV (Just l) cv))
        unweaveTypeF l (VarF (FV gv cv)) = (VarU (TV Nothing gv), VarU (TV (Just l) cv))
        unweaveTypeF l (FunF ts t) =
            let (gt, ct) = unweaveTypeF l t
                (gts, cts) = unzip $ map (unweaveTypeF l) ts
            in (FunU gts gt, FunU cts ct)
        unweaveTypeF l (AppF t ts) =
            let (gt, ct) = unweaveTypeF l t
                (gts, cts) = unzip $ map (unweaveTypeF l) ts
            in (AppU gt gts, AppU ct cts)
        unweaveTypeF l (NamF n (FV gv cv) ps rs) =
            let (psg, psc) = unzip $ map (unweaveTypeF l) ps
                (ksg, ksc) = unzip $ map (\(FV gk ck, _) -> (gk, ck)) rs
                (vsg, vsc) = unzip $ map (unweaveTypeF l . snd) rs
            in (NamU n (TV Nothing gv) psg (zip ksg vsg), NamU n (TV (Just l) cv) psc (zip ksc vsc))

        weaveTypeF :: Maybe TypeU -> TypeU -> Except MDoc TypeF
        weaveTypeF Nothing t = throwError
            $  "No general type found for serializable concrete type:" <+> pretty t
            <> "\n  All serializable types must have general types"
        weaveTypeF (Just (VarU (TV Nothing gv))) (VarU (TV _ cv)) = return $ VarF (FV gv cv)
        weaveTypeF (Just (FunU tsg tg)) (FunU tsc tc) = FunF <$> zipWithM weaveTypeF (map Just tsg) tsc <*> weaveTypeF (Just tg) tc
        weaveTypeF (Just (AppU tg tsg)) (AppU tc tsc) = AppF <$> weaveTypeF (Just tg) tc <*> zipWithM weaveTypeF (map Just tsg) tsc
        weaveTypeF (Just (NamU n (TV Nothing gv) psg rsg)) (NamU _ (TV _ cv) psc rsc) =
            NamF n (FV gv cv) <$> zipWithM weaveTypeF (map Just psg) psc <*> (
              zip (zipWith FV (map fst rsg) (map fst rsc)) <$>
              zipWithM weaveTypeF (map (Just . snd) rsg) (map snd rsc)
            )
        weaveTypeF _ _ = undefined

        -- Replaces each generic term with an existential term of the same name
        existential :: TypeU -> TypeU
        existential (ForallU v t0) = substitute v (existential t0)
        existential t0 = t0
resolvePacker _ _ _ = throwError "No packer found for this type"


prettyMap :: Map.Map MT.Text [ResolvedPacker] -> MDoc
prettyMap p =
    "----- pacmaps -----\n" <>
    vsep (map (uncurry prettyMapEntry) (Map.toList p)) <> "\n" <>
    "-------------------\n"

prettyMapEntry :: MT.Text -> [ResolvedPacker] -> MDoc
prettyMapEntry fv ps
    = pretty fv <> "\n  "
    <> vsep (map prettyMapPacker ps)

prettyMapPacker :: ResolvedPacker -> MDoc 
prettyMapPacker p
    = braces $ vsep
      [ "resolvedPackerTerm:" <+> pretty (resolvedPackerTerm p)
      , "resolvedPackedType:" <+> pretty (resolvedPackedType p)
      , "resolvedUnpackedType:" <+> pretty (resolvedUnpackedType p)
      , "resolvedPackerForward:" <+> pretty (resolvedPackerForward p)
      , "resolvedPackerReverse:" <+> pretty (resolvedPackerReverse p)
      , "resolvedPackerGeneralTypes:" <+> case resolvedPackerGeneralTypes p of
           Nothing -> "Nothing"
           (Just (t1, t2)) -> tupled [pretty t1, pretty t2]
      ]

-- | Given a list of possible ways to (de)serialize data between two languages,
-- choose one (or none if the list is empty). Currently I just take the first
-- in the list, but different cycles may have very different performance, so
-- this will be an important optimization step later on.
chooseSerializationCycle
  :: [(SerialAST, SerialAST)]
  -> Maybe (SerialAST, SerialAST)
chooseSerializationCycle [] = Nothing
chooseSerializationCycle (x:_) = Just x

-- | Determine if a SerialAST can be directly translated to JSON, if not it
-- will need to be further reduced.
isSerializable :: SerialAST -> Bool
isSerializable (SerialPack _ _) = False
isSerializable (SerialList _ x) = isSerializable x
isSerializable (SerialTuple _ xs) = all isSerializable xs 
isSerializable (SerialObject _ _ _ rs) = all (isSerializable . snd) rs 
isSerializable (SerialReal   _) = True
isSerializable (SerialInt    _) = True
isSerializable (SerialBool   _) = True
isSerializable (SerialString _) = True
isSerializable (SerialNull   _) = True
isSerializable (SerialUnknown _) = True -- are you feeling lucky?

prettySerialOne :: SerialAST -> MDoc
prettySerialOne (SerialPack _ _) = "SerialPack"
prettySerialOne (SerialList v x) = "SerialList" <> angles (pretty v) <> parens (prettySerialOne x)
prettySerialOne (SerialTuple v xs) = "SerialTuple" <> angles (pretty v) <> tupled (map prettySerialOne xs)
prettySerialOne (SerialObject r _ _ rs)
  = block 4 ("SerialObject@" <> viaShow r)
  $ vsep (map (\(k,v) -> parens (viaShow k) <> "=" <> prettySerialOne v) rs)
prettySerialOne (SerialReal   _) = "SerialReal"
prettySerialOne (SerialInt    _) = "SerialInt"
prettySerialOne (SerialBool   _) = "SerialBool"
prettySerialOne (SerialString _) = "SerialString"
prettySerialOne (SerialNull   _) = "SerialNull"
prettySerialOne (SerialUnknown _) = "SerialUnknown"
