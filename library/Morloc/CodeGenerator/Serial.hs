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
import qualified Morloc.Monad as MM
import qualified Data.Map as Map
import qualified Morloc.Frontend.Lang.DefaultTypes as Def
import Morloc.Pretty (prettyPackMap)
import Morloc.Data.Doc

defaultSerialType :: Lang -> TypeP
defaultSerialType Python3Lang = VarP (PV Python3Lang (Just "Str") ("str"))
defaultSerialType RLang = VarP (PV RLang (Just "Str") ("character"))
defaultSerialType CppLang = VarP (PV CppLang (Just "Str") ("std::string"))
defaultSerialType _ = error "Ah hell, you know I don't know that language"

-- extracts the language specific term from the paired term
pv2tv :: PVar -> TVar
pv2tv (PV lang _ v) = TV (Just lang) v

defaultListFirst :: TypeP -> TypeP
defaultListFirst t = defaultListAll t !! 0

defaultTupleFirst :: [TypeP] -> TypeP
defaultTupleFirst ts = defaultTupleAll ts !! 0

-- | An infinite line of dummies ...
dummies :: Maybe Lang -> [TypeU]
dummies lang = repeat $ VarU (TV lang "dummy")

defaultListAll :: TypeP -> [TypeP]
defaultListAll t@(langOf' -> lang)
  = [AppP (VarP $ PV lang (Just Def.listG) v) [t] | v <- Def.listC lang]

isList (AppP (VarP (PV lang _ v)) [_]) =
  let ds = Def.defaultList (Just lang) (head (dummies (Just lang)))
  in length [v' | (AppU (VarU (TV _ v')) _) <- ds, v == v'] > 0
isList _ = False

defaultTupleAll :: [TypeP] -> [TypeP]
defaultTupleAll [] = error "Cannot have an empty tuple?"
defaultTupleAll ts@(t:_) =
  let lang = langOf' t
      gt = Just $ Def.tupleG (length ts)
      cts = Def.tupleC (length ts) lang
  in [AppP (VarP (PV lang gt ct)) ts | ct <- cts]


isTuple :: TypeP -> Bool
isTuple (AppP (VarP (PV lang _ v)) (length -> i)) = elem v (Def.tupleC i lang)
isTuple _ = False

isPrimitiveType :: (Maybe Lang -> [TypeU]) -> TypeP -> Bool
isPrimitiveType lookupDefault t =
  let xs = filter (typeEqual t)
         $ [ VarP (PV lang generalType v)
           | (VarU (TV (Just lang) v)) <- lookupDefault (langOf t)]
  in length xs > 0
  where
    generalType = case lookupDefault Nothing of
      ((VarU (TV _ g)):_) -> Just g
      _ -> Nothing

-- | recurse all the way to a serializable type
serialAstToType :: SerialAST One -> MorlocMonad TypeP
serialAstToType (SerialPack _ (One (_, s))) = serialAstToType s
serialAstToType (SerialList s) = serialAstToType s |>> defaultListFirst
serialAstToType (SerialTuple ss) = mapM serialAstToType ss |>> defaultTupleFirst
serialAstToType (SerialObject o n ps rs) = do
  ts <- mapM (serialAstToType . snd) rs
  return $ NamP o n ps (zip (map fst rs) ts)
serialAstToType (SerialReal   x) = return $ VarP x
serialAstToType (SerialInt    x) = return $ VarP x
serialAstToType (SerialBool   x) = return $ VarP x
serialAstToType (SerialString x) = return $ VarP x
serialAstToType (SerialNull   x) = return $ VarP x
serialAstToType (SerialUnknown (PV lang _ _)) = return $ defaultSerialType lang

-- | recurse all the way to a serializable type, unsafe
serialAstToType' :: SerialAST One -> TypeP
serialAstToType' (SerialPack _ (One (_, s))) = serialAstToType' s
serialAstToType' (SerialList s) = defaultListFirst $ serialAstToType' s
serialAstToType' (SerialTuple ss) = defaultTupleFirst $ map serialAstToType' ss
serialAstToType' (SerialObject o n ps rs)
  = NamP o n ps (zip (map fst rs) (map (serialAstToType' . snd) rs))
serialAstToType' (SerialReal   x) = VarP x
serialAstToType' (SerialInt    x) = VarP x
serialAstToType' (SerialBool   x) = VarP x
serialAstToType' (SerialString x) = VarP x
serialAstToType' (SerialNull   x) = VarP x
serialAstToType' (SerialUnknown (PV lang _ _)) = defaultSerialType lang


-- | get only the toplevel type
shallowType :: SerialAST One -> MorlocMonad TypeP
shallowType (SerialPack _ (One (p, _))) = return (typePackerFrom p)
shallowType (SerialList s) = shallowType s |>> defaultListFirst
shallowType (SerialTuple ss) = mapM shallowType ss |>> defaultTupleFirst
shallowType (SerialObject o n ps rs) = do
  ts <- mapM shallowType (map snd rs)
  return $ NamP o n ps (zip (map fst rs) ts)
shallowType (SerialReal   x) = return $ VarP x
shallowType (SerialInt    x) = return $ VarP x
shallowType (SerialBool   x) = return $ VarP x
shallowType (SerialString x) = return $ VarP x
shallowType (SerialNull   x) = return $ VarP x
shallowType (SerialUnknown (PV lang _ _)) = return $ defaultSerialType lang

makeSerialAST
  :: PackMap -- type PackMap = Map (TVar, Int) [UnresolvedPacker]
  -> TypeP
  -> MorlocMonad (SerialAST Many)
makeSerialAST _ (UnkP v) = return $ SerialUnknown v
makeSerialAST m t@(VarP v@(PV _ _ _))
  | isPrimitiveType Def.defaultNull   t = return $ SerialNull   v
  | isPrimitiveType Def.defaultBool   t = return $ SerialBool   v
  | isPrimitiveType Def.defaultString t = return $ SerialString v
  | isPrimitiveType Def.defaultReal   t = return $ SerialReal   v
  | isPrimitiveType Def.defaultInt    t = return $ SerialInt    v
  | otherwise = makeSerialAST m (AppP (VarP v) [])
makeSerialAST _ (FunP _ _)
  = MM.throwError . SerializationError
  $ "Cannot serialize functions"
makeSerialAST _ (AppP _ []) = undefined
makeSerialAST m t@(AppP (VarP v@(PV _ _ s)) ts@(t0:_))
  | isList t = SerialList <$> makeSerialAST m t0
  | isTuple t = SerialTuple <$> mapM (makeSerialAST m) ts
  | otherwise = case Map.lookup (pv2tv v, length ts) m of
        (Just ps) -> do
          ps' <- mapM (resolvePacker t ts) ps
          ts' <- mapM (makeSerialAST m . typePackerType) ps'
          return $ SerialPack v (Many (zip ps' ts'))
        Nothing -> MM.throwError . SerializationError . render
          $ "Cannot find constructor" <+> dquotes (pretty s)
          <> "<" <> pretty (length ts) <> ">"
          <+> "in packmap:\n" <> prettyPackMap m
makeSerialAST m (NamP o n ps rs) = do
  ts <- mapM (makeSerialAST m . snd) rs
  return $ SerialObject o n ps (zip (map fst rs) ts)
makeSerialAST _ _ = undefined


pvarEqual :: PVar -> PVar -> Bool
pvarEqual (PV lang1 _ v1) (PV lang2 _ v2) = lang1 == lang2 && v1 == v2 

typeEqual :: TypeP -> TypeP -> Bool
typeEqual (VarP v1) (VarP v2) = pvarEqual v1 v2
typeEqual (FunP [] t1) (FunP [] t2) = typeEqual t1 t2
typeEqual (FunP (t11:rs1) t12) (FunP (t21:rs2) t22)
 = typeEqual t11 t21 && typeEqual (FunP rs1 t12) (FunP rs2 t22)
typeEqual (AppP v1 []) (AppP v2 []) = typeEqual v1 v2
typeEqual (AppP v1 (t1:rs1)) (AppP v2 (t2:rs2))
 = typeEqual t1 t2 && typeEqual (AppP v1 rs1) (AppP v2 rs2)
typeEqual (NamP o1 n1 ps1 []) (NamP o2 n2 ps2 [])
  = o1 == o2 && n1 == n2 && length ps1 == length ps2

-- ps1 and ps2 don't need to be tested, since the typechecker will have
-- ensured they are equivalent IF the main records are equivalent.
typeEqual (NamP o1 n1 ps1 ((k1,t1):rs1)) (NamP o2 n2 ps2 es2) =
  -- equality does not depend on order
  case filterApart (\(k2, _) -> k1 == k2) es2 of 
    -- if the key was found
    (Just (_, t2), rs2) ->
         -- then ensure the values are the same
         typeEqual t1 t2
         -- and check the (n-1) records
      && typeEqual (NamP o1 n1 ps1 rs1) (NamP o2 n2 ps2 rs2)
    (Nothing, _) -> False
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

resolveType :: [TypeP] -> TypeU -> MorlocMonad TypeP
resolveType [] (ForallU _ _) = MM.throwError . SerializationError $ "Packer parity error"
resolveType [] u = weaveTypes Nothing (typeOf u)
resolveType (t:ts) (ForallU v u) = substituteTVar v t <$> resolveType ts u
resolveType (_:_) _ = MM.throwError . SerializationError $ "Packer parity error"

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
  f (SerialReal   x1) (SerialReal   x2) = Just (SerialReal   x1, SerialReal   x2)
  f (SerialInt    x1) (SerialInt    x2) = Just (SerialInt    x1, SerialInt    x2)
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
isSerializable (SerialReal   _) = True
isSerializable (SerialInt    _) = True
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
prettySerialOne (SerialReal   _) = "SerialReal"
prettySerialOne (SerialInt    _) = "SerialInt"
prettySerialOne (SerialBool   _) = "SerialBool"
prettySerialOne (SerialString _) = "SerialString"
prettySerialOne (SerialNull   _) = "SerialNull"
prettySerialOne (SerialUnknown _) = "SerialUnknown"
