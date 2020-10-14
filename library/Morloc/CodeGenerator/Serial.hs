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
  ) where

import Morloc.CodeGenerator.Namespace
import Morloc.Frontend.PartialOrder (substitute)
import Morloc.Frontend.Treeify (resolve, substituteT)
import qualified Morloc.Monad as MM
import qualified Morloc.Data.Text as MT
import qualified Data.Map as Map

data UnresolvedPacker = UnresolvedPacker
  { unresolvedPackerCType   :: UnresolvedType -- The decomposed (unpacked) type
  , unresolvedPackerForward :: [Source]
  -- ^ The unpack function, there may be more than one, the compiler will make
  -- a half-hearted effort to find the best one. It is called "Forward" since
  -- it is moves one step towards serialization.
  , unresolvedPackerReverse :: [Source]
  }

data TypePacker = TypePacker
  { typePackerCType   :: Type
  , typePackerForward :: [Source]
  , typePackerReverse :: [Source]
  }

data SerialAST f
  = SerialPack (f (TypePacker, SerialAST f))
  | SerialList (SerialAST f)
  | SerialTuple [SerialAST f]
  | SerialObject TVar [(MT.Text, SerialAST f)]
  | SerialVar MT.Text
  -- ^ this should be a type that is recognized by the default json serializer
  -- for example, "double" or "std::string" in C++
  | SerialUnknown MT.Text
  -- ^ depending on the language, this may or may not raise an error down the
  -- line, the parameter contains the variable name, which is useful only for
  -- source code comments.

makeSerialAST
  :: Map.Map (TVar, Int) [UnresolvedPacker]
  -> Type
  -> MorlocMonad (SerialAST Many)
makeSerialAST _ (UnkT (TV _ v)) = return $ SerialUnknown v
makeSerialAST m (VarT v) = makeSerialAST m (ArrT v [])
makeSerialAST _ (FunT _ _) = MM.throwError . SerializationError $ "Cannot serialize functions"
makeSerialAST m (ArrT v ts) = case Map.lookup (v, length ts) m of
  (Just ps) -> do        
    ps' <- mapM (resolvePacker ts) ps
    ts' <- mapM (makeSerialAST m) (map typePackerCType ps')
    return $ SerialPack (Many (zip ps' ts'))
  Nothing -> MM.throwError . SerializationError $ "Cannot find constructor" 
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
  f (SerialVar v1) (SerialVar v2) = Just (SerialVar v1, SerialVar v2)
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
isSerializable (SerialVar _) = True -- we'll burn this bridge when we come to it
isSerializable (SerialUnknown _) = True -- are you feeling lucky?
