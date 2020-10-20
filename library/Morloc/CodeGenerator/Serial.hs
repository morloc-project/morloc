{-|
Module      : Morloc.CodeGenerator.Serial
Description : Short description
Copyright   : (c) Zebulun Arendsee, 2020
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.CodeGenerator.Serial
  ( makeSerialAST 
  , findSerializationCycles 
  , chooseSerializationCycle
  , isSerializable
  , prettySerialOne
  , serialAstToType
  ) where

import Morloc.CodeGenerator.Namespace
import Morloc.Frontend.PartialOrder (substitute)
import Morloc.Frontend.Treeify (resolve, substituteT)
import qualified Morloc.Monad as MM
import qualified Morloc.Data.Text as MT
import qualified Data.Map as Map
import qualified Morloc.Frontend.Lang.DefaultTypes as Def
import Morloc.Pretty (prettyType, prettyPackMap)
import Morloc.Data.Doc

typeEqual :: Type -> UnresolvedType -> Bool
typeEqual (VarT (TV _ v1)) (VarU (TV _ v2)) = v1 == v2
typeEqual (ArrT (TV _ v1) ts1) (ArrU (TV _ v2) us2)
  | length ts1 /= length us2 = False
  | otherwise = foldl (&&) (v1 == v2) (zipWith typeEqual ts1 us2 )
typeEqual _ _ = False

-- Convert a default unresolved type to a standard type
type2default :: Type -> UnresolvedType
type2default (VarT v) = VarU v
type2default (ArrT v ts) = ArrU v (map type2default ts)
type2default (NamT v rs) = NamU v (zip (map fst rs) (map (type2default . snd) rs))
type2default (FunT _ _) = error "default types should never be functions"

serialAstToType :: Lang -> SerialAST One -> MorlocMonad Type
serialAstToType _    (SerialPack (One (p, _))) = return $ typePackerCType p
serialAstToType lang (SerialList s) = do
  t <- serialAstToType lang s
  return . partialResolve . head . Def.defaultList (Just lang) $ partialUnresolve t
serialAstToType lang (SerialTuple ss) = do
  ts <- mapM (serialAstToType lang) ss
  return . partialResolve . head . Def.defaultTuple (Just lang) . map partialUnresolve $ ts
serialAstToType lang (SerialObject v rs) = return $ VarT v
serialAstToType lang (SerialNum    x) = return $ VarT (TV (Just lang) x)
serialAstToType lang (SerialBool   x) = return $ VarT (TV (Just lang) x)
serialAstToType lang (SerialString x) = return $ VarT (TV (Just lang) x)
serialAstToType lang (SerialNull   x) = return $ VarT (TV (Just lang) x)
serialAstToType lang (SerialUnknown _) = MM.throwError . SerializationError
                                       $ "Cannot guess serialization type"

partialResolve :: UnresolvedType -> Type
partialResolve (VarU v) = VarT v
partialResolve (ArrU v ts) = ArrT v (map partialResolve ts)
partialResolve (NamU v rs) = NamT v (zip (map fst rs) (map (partialResolve . snd) rs))

partialUnresolve :: Type -> UnresolvedType
partialUnresolve (VarT v) = VarU v
partialUnresolve (ArrT v ts) = ArrU v (map partialUnresolve ts)
partialUnresolve (NamT v rs) = NamU v (zip (map fst rs) (map (partialUnresolve . snd) rs))

makeSerialAST
  :: PackMap -- Map.Map (TVar, Int) [UnresolvedPacker]
  -> Type
  -> MorlocMonad (SerialAST Many)
makeSerialAST _ (UnkT (TV _ v)) = return $ SerialUnknown v
makeSerialAST m t@(VarT v@(TV lang s))
  | length nulls > 0 = return $ SerialNull s
  | length bools > 0 = return $ SerialBool s
  | length strings > 0 = return $ SerialString s
  | length numbers > 0 = return $ SerialNum s
  | otherwise = makeSerialAST m (ArrT v [])
  where
    nulls = filter (typeEqual t) (Def.defaultNull lang)
    bools = filter (typeEqual t) (Def.defaultBool lang)
    strings = filter (typeEqual t) (Def.defaultString lang)
    numbers = filter (typeEqual t) (Def.defaultNumber lang)
makeSerialAST _ (FunT _ _) = MM.throwError . SerializationError $ "Cannot serialize functions"
makeSerialAST m t@(ArrT v@(TV lang s) ts)
  | length ts == 1 && length (filter (typeEqual t) (Def.defaultList lang (type2default $ ts !! 0))) > 0
    = SerialList <$> makeSerialAST m (ts !! 0)
  | length tuples > 0 = SerialTuple <$> mapM (makeSerialAST m) ts
  | otherwise = case Map.lookup (v, length ts) m of
      (Just ps) -> do        
        ps' <- mapM (resolvePacker ts) ps
        ts' <- mapM (makeSerialAST m) (map typePackerCType ps')
        return $ SerialPack (Many (zip ps' ts'))
      Nothing -> MM.throwError . SerializationError . render
        $ "Cannot find constructor for" <+> squotes (prettyType t) <+> "in packmap:\n" <>
          prettyPackMap m 
  where
    tuples = filter (typeEqual t) (Def.defaultTuple lang (map type2default ts))
makeSerialAST m (NamT v rs) = do
  ts <- mapM (makeSerialAST m) (map snd rs)
  return $ SerialObject v (zip (map fst rs) ts) 

resolvePacker :: [Type] -> UnresolvedPacker -> MorlocMonad TypePacker
resolvePacker ts u = do 
  t <- resolveType ts (unresolvedPackerCType u) 
  return $ TypePacker
    { typePackerCType = t
    , typePackerForward = unresolvedPackerForward u
    , typePackerReverse = unresolvedPackerReverse u
    }

resolveType :: [Type] -> UnresolvedType -> MorlocMonad Type
resolveType [] (ForallU _ _) = MM.throwError . SerializationError $ "Packer parity error"
resolveType [] u = resolve u
resolveType (t:ts) (ForallU v u) = substituteT v t <$> resolveType ts u
resolveType (_:_) _ = MM.throwError . SerializationError $ "Packer parity error"

-- | Given serialization trees for two languages, where each serialization tree
-- may contain, try to find
findSerializationCycles
  :: ([(SerialAST One, SerialAST One)] -> Maybe (SerialAST One, SerialAST One))
  -- ^ pruning function
  -> SerialAST Many
  -> SerialAST Many
  -> Maybe (SerialAST One, SerialAST One)
findSerializationCycles choose x y = f x y where
  f :: SerialAST Many -> SerialAST Many -> Maybe (SerialAST One, SerialAST One) 
  -- reduce constructs until we get down to something that has general meaning
  f (SerialPack (Many ss1)) s2
    = choose
    . catMaybes
    $ [ fmap (\(x,y)->(SerialPack (One (p1,x)),y)) (f s1 s2)
      | (p1,s1) <- ss1]
  -- same as above, just swap the arguments
  f s1 s2@(SerialPack _) = case f s2 s1 of 
    (Just (x,y)) -> Just (y,x)
    Nothing -> Nothing
  f (SerialList s1) (SerialList s2) = case f s1 s2 of 
      (Just (s1', s2')) -> Just (SerialList s1', SerialList s2')
      Nothing -> Nothing
  f (SerialTuple ts1) (SerialTuple ts2)
    | length ts1 /= length ts1 = Nothing
    | otherwise = case fmap unzip . sequence $ zipWith f ts1 ts2 of
        (Just (xs,ys)) -> Just (SerialTuple xs, SerialTuple ys)
        Nothing -> Nothing
  f (SerialObject v1 rs1) (SerialObject v2 rs2)
    | map fst rs1 /= map fst rs2 = Nothing 
    | otherwise = case fmap unzip . sequence $ zipWith f ts1 ts2 of
        Nothing -> Nothing
        Just (rs1', rs2') -> Just ( SerialObject v1 (zip (map fst rs1) rs1')
                                  , SerialObject v2 (zip (map fst rs2) rs2'))
      where
        ts1 = map snd rs1
        ts2 = map snd rs1
  f (SerialNum    x1) (SerialNum    x2) = Just (SerialNum    x1, SerialNum    x2)
  f (SerialBool   x1) (SerialBool   x2) = Just (SerialBool   x1, SerialBool   x2)
  f (SerialString x1) (SerialString x2) = Just (SerialString x1, SerialString x2)
  f (SerialNull   x1) (SerialNull   x2) = Just (SerialNull   x1, SerialNull   x2)
  f (SerialUnknown v1) (SerialUnknown v2) = Just (SerialUnknown v1, SerialUnknown v2)
  f _ _ = Nothing

-- | Given a list of possible ways to (de)serialize data between two languages,
-- choose one (or none if the list is empty). Currently I just take the first
-- in the list, but different cycles may have very different performance, so
-- this will be an important optimization step later on.
chooseSerializationCycle
  :: [(SerialAST One, SerialAST One)]
  -> Maybe (SerialAST One, SerialAST One)
chooseSerializationCycle [] = Nothing
chooseSerializationCycle (x:_) = Just x

-- | Determine if a SerialAST can be directly translated to JSON, if not it
-- will need to be further reduced.
isSerializable :: Functor f => SerialAST f -> Bool
isSerializable (SerialPack _) = False
isSerializable (SerialList x) = isSerializable x
isSerializable (SerialTuple xs) = all isSerializable xs 
isSerializable (SerialObject _ rs) = all isSerializable (map snd rs) 
isSerializable (SerialNum    _) = True
isSerializable (SerialBool   _) = True
isSerializable (SerialString _) = True
isSerializable (SerialNull   _) = True
isSerializable (SerialUnknown _) = True -- are you feeling lucky?

prettySerialOne :: SerialAST One -> MDoc
prettySerialOne (SerialPack _) = "SerialPack"
prettySerialOne (SerialList x) = "SerialList" <> parens (prettySerialOne x)
prettySerialOne (SerialTuple xs) = "SerialTuple" <> tupled (map prettySerialOne xs)
prettySerialOne (SerialObject _ rs)
  = block 4 "SerialObject"
  $ vsep (map (\(k,v) -> pretty k <> "=" <> prettySerialOne v) rs)
prettySerialOne (SerialNum    _) = "SerialNum"
prettySerialOne (SerialBool   _) = "SerialBool"
prettySerialOne (SerialString _) = "SerialString"
prettySerialOne (SerialNull   _) = "SerialNull"
prettySerialOne (SerialUnknown _) = "SerialUnknown"
