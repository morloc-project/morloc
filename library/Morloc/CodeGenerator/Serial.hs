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
  , serialAstToType'
  , shallowType
  ) where

import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.Internal
import Morloc.Frontend.Namespace (resolve)
import qualified Morloc.Monad as MM
import qualified Data.Map as Map
import qualified Morloc.Frontend.Lang.DefaultTypes as Def
import Morloc.Pretty (prettyPackMap)
import Morloc.Data.Doc

pv2tv :: PVar -> TVar
pv2tv (PV lang _ v) = TV (Just lang) v

defaultListFirst :: TypeP -> TypeP
defaultListFirst t = defaultListAll t !! 0

defaultTupleFirst :: [TypeP] -> TypeP
defaultTupleFirst ts = defaultTupleAll ts !! 0

-- | A metaphor for America
dummies :: Maybe Lang -> [UnresolvedType]
dummies lang = repeat $ VarU (TV lang "dummy")

defaultListAll :: TypeP -> [TypeP]
defaultListAll t =
  [ ArrP (PV lang gtype v) [t]
  | (ArrU (TV (Just lang) v) _) <- Def.defaultList (langOf t) (head (dummies (langOf t)))
  ]
  where
    gtype = case Def.defaultList Nothing (head (dummies Nothing)) of
      ((ArrU (TV _ v1) _):_) -> Just v1
      _ -> Nothing

isList :: TypeP -> Bool
isList (ArrP (PV lang _ v) [_]) =
  let ds = Def.defaultList (Just lang) (head (dummies (Just lang)))
  in length [v' | (ArrU (TV _ v') _) <- ds, v == v'] > 0
isList _ = False

defaultTupleAll :: [TypeP] -> [TypeP]
defaultTupleAll [] = error $ "Illegal empty tuple"
defaultTupleAll ts@(t:_) =
    [ ArrP (PV lang gtype v) ts
    | (ArrU (TV (Just lang) v) _) <- Def.defaultTuple (langOf t) (take (length ts) (dummies (langOf t)))
    ]
  where
    gtype = case Def.defaultTuple Nothing (take (length ts) (dummies Nothing)) of
      ((ArrU (TV _ v1) _):_) -> Just v1
      _ -> Nothing

isTuple :: TypeP -> Bool
isTuple (ArrP (PV lang _ v) ts) =
  let ds = Def.defaultTuple (Just lang) (take (length ts) (dummies (Just lang)))
  in length [v' | (ArrU (TV _ v') _) <- ds, v == v'] > 0
isTuple _ = False

isPrimitiveType :: (Maybe Lang -> [UnresolvedType]) -> TypeP -> Bool
isPrimitiveType lookupDefault t =
  let xs = filter (typeEqual t)
         $ [ VarP (PV lang gtype v)
           | (VarU (TV (Just lang) v)) <- lookupDefault (langOf t)]
  in length xs > 0
  where
    gtype = case lookupDefault Nothing of
      ((VarU (TV _ g)):_) -> Just g
      _ -> Nothing

-- | recurse all the way to a serializable type
serialAstToType :: SerialAST One -> MorlocMonad TypeP
serialAstToType (SerialPack _ (One (_, s))) = serialAstToType s
serialAstToType (SerialList s) = serialAstToType s |>> defaultListFirst
serialAstToType (SerialTuple ss) = mapM serialAstToType ss |>> defaultTupleFirst
serialAstToType (SerialObject r v ps rs) = do
  rs' <- mapM (serialAstToType . snd) rs
  return $ NamP r v ps (zip (map fst rs) rs')
serialAstToType (SerialNum    x) = return $ VarP x
serialAstToType (SerialBool   x) = return $ VarP x
serialAstToType (SerialString x) = return $ VarP x
serialAstToType (SerialNull   x) = return $ VarP x
serialAstToType (SerialUnknown x)
  = MM.throwError . SerializationError . render
  $ "Cannot guess serialization type:" <+> viaShow x

-- | recurse all the way to a serializable type, unsafe
serialAstToType' :: SerialAST One -> TypeP
serialAstToType' (SerialPack _ (One (_, s))) = serialAstToType' s
serialAstToType' (SerialList s) = defaultListFirst $ serialAstToType' s
serialAstToType' (SerialTuple ss) = defaultTupleFirst $ map serialAstToType' ss
serialAstToType' (SerialObject r v ps rs) = NamP r v ps (zip (map fst rs) (map (serialAstToType' . snd) rs))
serialAstToType' (SerialNum    x) = VarP x
serialAstToType' (SerialBool   x) = VarP x
serialAstToType' (SerialString x) = VarP x
serialAstToType' (SerialNull   x) = VarP x
serialAstToType' (SerialUnknown _) = error "Cannot guess serialization type"


-- | get only the toplevel type
shallowType :: SerialAST One -> MorlocMonad TypeP
shallowType (SerialPack _ (One (p, _))) = return (typePackerFrom p)
shallowType (SerialList s) = shallowType s |>> defaultListFirst
shallowType (SerialTuple ss) = mapM shallowType ss |>> defaultTupleFirst
shallowType (SerialObject r v ps rs) = do
  ts <- mapM shallowType (map snd rs)
  return $ NamP r v ps (zip (map fst rs) ts)
shallowType (SerialNum    x) = return $ VarP x
shallowType (SerialBool   x) = return $ VarP x
shallowType (SerialString x) = return $ VarP x
shallowType (SerialNull   x) = return $ VarP x
shallowType (SerialUnknown _) = MM.throwError . SerializationError
                                       $ "Cannot guess serialization type"

makeSerialAST
  :: GMeta
  -> TypeP
  -> MorlocMonad (SerialAST Many)
makeSerialAST _ (UnkP v) = return $ SerialUnknown v
makeSerialAST m t@(VarP v@(PV _ _ _))
  | isPrimitiveType Def.defaultNull   t = return $ SerialNull   v
  | isPrimitiveType Def.defaultBool   t = return $ SerialBool   v
  | isPrimitiveType Def.defaultString t = return $ SerialString v
  | isPrimitiveType Def.defaultNumber t = return $ SerialNum    v
  | otherwise = makeSerialAST m (ArrP v [])
makeSerialAST _ (FunP _ _)
  = MM.throwError . SerializationError
  $ "Cannot serialize functions"
makeSerialAST m t@(ArrP v@(PV _ _ s) ts)
  | isList t = SerialList <$> makeSerialAST m (ts !! 0)
  | isTuple t = SerialTuple <$> mapM (makeSerialAST m) ts
  | otherwise = case Map.lookup (pv2tv v, length ts) (metaPackers m) of
      (Just ps) -> do
        ps' <- mapM (resolvePacker t ts) ps
        ts' <- mapM (makeSerialAST m) (map typePackerType ps')
        return $ SerialPack v (Many (zip ps' ts'))
      Nothing -> MM.throwError . SerializationError . render
        $ "Cannot find constructor" <+> dquotes (pretty s)
        <> "<" <> pretty (length ts) <> ">"
        <+> "in packmap:\n" <> prettyPackMap (metaPackers m)
makeSerialAST m (NamP r v ps rs) = do
  ts <- mapM (makeSerialAST m) (map snd rs)
  return $ SerialObject r v ps (zip (map fst rs) ts)

pvarEqual :: PVar -> PVar -> Bool
pvarEqual (PV lang1 _ v1) (PV lang2 _ v2) = lang1 == lang2 && v1 == v2 

typeEqual :: TypeP -> TypeP -> Bool
typeEqual (VarP v1) (VarP v2) = pvarEqual v1 v2
typeEqual (ArrP v1 ts1) (ArrP v2 ts2)
  | length ts1 /= length ts2 = False
  | otherwise = foldl (&&) (pvarEqual v1 v2) (zipWith typeEqual ts1 ts2 )
typeEqual (NamP _ v1 _ rs1) (NamP _ v2 _ rs2)
  =  (pvarEqual v1 v2)
  && map fst rs1 == map fst rs2
  && foldl (&&) True (zipWith typeEqual (map snd rs1) (map snd rs2))
typeEqual _ _ = False


resolvePacker :: TypeP -> [TypeP] -> UnresolvedPacker -> MorlocMonad TypePacker
resolvePacker packedType ts u = do 
  t <- resolveType ts (unresolvedPackerCType u) 
  return $ TypePacker
    { typePackerType = t
    , typePackerFrom = packedType
    , typePackerForward = unresolvedPackerForward u
    , typePackerReverse = unresolvedPackerReverse u
    }

resolveType :: [TypeP] -> UnresolvedType -> MorlocMonad TypeP
resolveType [] (ForallU _ _) = MM.throwError . SerializationError $ "Packer parity error"
resolveType [] u = weaveTypes Nothing (resolve u)
resolveType (t:ts) (ForallU v u) = substituteP v t <$> resolveType ts u
resolveType (_:_) _ = MM.throwError . SerializationError $ "Packer parity error"

-- | substitute all appearances of a given variable with a given new type
substituteP :: TVar -> TypeP -> TypeP -> TypeP
substituteP v0 r0 t0 = sub t0
  where
    sub :: TypeP -> TypeP
    sub t'@(UnkP _) = t'
    sub t'@(VarP (PV lang _ v'))
      | v0 == (TV (Just lang) v') = r0
      | otherwise = t'
    sub (FunP t1 t2) = FunP (sub t1) (sub t2)
    sub (ArrP v' ts) = ArrP v' (map sub ts)
    sub (NamP r v' ps rs) = NamP r v' (map sub ps) [(x, sub t') | (x, t') <- rs]

-- | Given serialization trees for two languages, where each serialization tree
-- may contain, try to find
findSerializationCycles
  :: ([(SerialAST One, SerialAST One)] -> Maybe (SerialAST One, SerialAST One))
  -- ^ pruning function
  -> SerialAST Many
  -> SerialAST Many
  -> Maybe (SerialAST One, SerialAST One)
findSerializationCycles choose x0 y0 = f x0 y0 where
  f :: SerialAST Many -> SerialAST Many -> Maybe (SerialAST One, SerialAST One) 
  -- reduce constructs until we get down to something that has general meaning
  f (SerialPack v (Many ss1)) s2
    = choose
    . catMaybes
    $ [ fmap (\(x,y)->(SerialPack v (One (p1,x)),y)) (f s1 s2)
      | (p1,s1) <- ss1]
  -- same as above, just swap the arguments
  f s1 s2@(SerialPack _ _) = case f s2 s1 of 
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
  f (SerialObject r1 v1 ps1 rs1) (SerialObject r2 v2 ps2 rs2)
    | map fst rs1 /= map fst rs2 = Nothing 
    | otherwise = case fmap unzip . sequence $ zipWith f ts1 ts2 of
        Nothing -> Nothing
        Just (rs1', rs2') -> Just ( SerialObject r1 v1 ps1 (zip (map fst rs1) rs1')
                                  , SerialObject r2 v2 ps2 (zip (map fst rs2) rs2'))
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
isSerializable (SerialPack _ _) = False
isSerializable (SerialList x) = isSerializable x
isSerializable (SerialTuple xs) = all isSerializable xs 
isSerializable (SerialObject _ _ _ rs) = all isSerializable (map snd rs) 
isSerializable (SerialNum    _) = True
isSerializable (SerialBool   _) = True
isSerializable (SerialString _) = True
isSerializable (SerialNull   _) = True
isSerializable (SerialUnknown _) = True -- are you feeling lucky?

prettySerialOne :: SerialAST One -> MDoc
prettySerialOne (SerialPack _ _) = "SerialPack"
prettySerialOne (SerialList x) = "SerialList" <> parens (prettySerialOne x)
prettySerialOne (SerialTuple xs) = "SerialTuple" <> tupled (map prettySerialOne xs)
prettySerialOne (SerialObject r _ _ rs)
  = block 4 ("SerialObject@" <> viaShow r)
  $ vsep (map (\(k,v) -> parens (viaShow k) <> "=" <> prettySerialOne v) rs)
prettySerialOne (SerialNum    _) = "SerialNum"
prettySerialOne (SerialBool   _) = "SerialBool"
prettySerialOne (SerialString _) = "SerialString"
prettySerialOne (SerialNull   _) = "SerialNull"
prettySerialOne (SerialUnknown _) = "SerialUnknown"
