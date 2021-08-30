{-|
Module      : Morloc.CodeGenerator.Typecheck
Description : Check the concrete type of an unambiguous AST (SAnno One)
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.CodeGenerator.Typecheck
(
  typecheck
) where

import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.Internal
import Morloc.CodeGenerator.Grammars.Common
import Morloc.Typecheck.Internal
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Morloc.Frontend.Lang.DefaultTypes as MLD

typecheck
  :: SAnno (Indexed Type) One (Indexed Lang)
  -> MorlocMonad (SAnno Int One (Indexed TypeP))
typecheck e = do
  packers <- MM.gets statePackers
  e' <- retrieveTypes e
  let g0 = Gamma {gammaCounter = 0, gammaContext = []}
  case synth g0 e' of
    (Left err) -> MM.throwError . ConcreteTypeError $ err
    (Right (_, _, e'')) -> weaveAndResolve e''


-- | Load the known concrete types into the tree. This is all the information
-- necessary for concrete type checking.
retrieveTypes
  :: SAnno (Indexed Type) One (Indexed Lang)
  -> MorlocMonad (SAnno (Indexed Type) One (Indexed (Lang, [EType])))
retrieveTypes (SAnno (One (x, Idx i lang)) g@(Idx j _)) = do
  ts <- case x of
    (CallS src) -> do
      mayts <- lookupSig j
      case fmap termConcrete mayts of
        (Just ts) -> case [es | (_, src', es, _) <- ts, src == src] of
          [es] -> return es
          _ -> MM.throwError . CallTheMonkeys $ "Malformed TermTypes"
        Nothing -> MM.throwError . CallTheMonkeys $ "Missing TermTypes"
    _ -> return []

  x' <- case x of
    UniS -> return UniS
    (VarS v) -> return $ VarS v
    (AccS x k) -> AccS <$> retrieveTypes x <*> pure k
    (ListS xs) -> ListS <$> mapM retrieveTypes xs
    (TupleS xs) -> TupleS <$> mapM retrieveTypes xs
    (LamS vs f) -> LamS vs <$> retrieveTypes f
    (AppS f xs) -> AppS <$> retrieveTypes f <*> mapM retrieveTypes xs
    (NumS x) -> return $ NumS x
    (LogS x) -> return $ LogS x
    (StrS x) -> return $ StrS x
    (RecS rs) -> do
      xs' <- mapM (retrieveTypes . snd) rs
      return $ RecS (zip (map fst rs) xs')
    (FixS) -> return $ FixS
    (CallS src) -> return $ CallS src

  return $ SAnno (One (x', Idx i (lang, ts))) g


weaveAndResolve
  :: SAnno (Indexed Type) One (Indexed UnresolvedType)
  -> MorlocMonad (SAnno Int One (Indexed TypeP))
weaveAndResolve (SAnno (One (x, Idx i ct)) (Idx j gt)) = do
  pt <- weaveResolvedTypes gt (resolve ct)
  x' <- case x of
    UniS -> return UniS
    (VarS v) -> return $ VarS v
    (AccS x k) -> AccS <$> weaveAndResolve x <*> pure k
    (ListS xs) -> ListS <$> mapM weaveAndResolve xs
    (TupleS xs) -> TupleS <$> mapM weaveAndResolve xs
    (LamS vs x) -> LamS vs <$> weaveAndResolve x
    (AppS f xs) -> AppS <$> weaveAndResolve f <*> mapM weaveAndResolve xs
    (NumS x) -> return $ NumS x
    (LogS x) -> return $ LogS x
    (StrS x) -> return $ StrS x
    (RecS rs) -> do
      xs <- mapM (weaveAndResolve . snd) rs
      return $ RecS (zip (map fst rs) xs)
    FixS -> return FixS
    (CallS src) -> return $ CallS src
  return $ SAnno (One (x', Idx i pt)) j



synth
  :: Gamma
  -> SAnno (Indexed Type) One (Indexed (Lang, [EType]))
  -> Either
       TypeError
       ( Gamma
       , UnresolvedType
       , SAnno (Indexed Type) One (Indexed UnresolvedType)
       )
--
-- ----------------------------------------- <primitive>
--  g |- <primitive expr> => <primitive type> -| g
--
--  Primitive types may have many possible defaults in a given language. For
--  example, in Rust a primitive Num may be a signed or unsigned, be a float or
--  an int, and have a size ranging from 8 to 128 bits. If no concrete types
--  are available, then the first default value will be used when the
--  UnresolvedType is resolved. 

-- Uni=>
synth g (SAnno (One (UniS, (Idx i (lang, _)))) gt) = do
  let ts = MLD.defaultNull (Just lang)
      (g', t) = newvarRich [] (MLD.defaultNull (Just lang)) (Just lang) g
  return (g' +> t, t, SAnno (One (UniS, Idx i t)) gt)
synth _ _ = undefined

-- Num=>
synth g (SAnno (One (NumS x, (Idx i (lang, _)))) gt) = do
  let ts = MLD.defaultNumber (Just lang)
      (g', t) = newvarRich [] (MLD.defaultNull (Just lang)) (Just lang) g
  return (g' +> t, t, SAnno (One (NumS x, Idx i t)) gt)
synth _ _ = undefined

-- Str=>
synth g (SAnno (One (StrS x, (Idx i (lang, _)))) gt) = do
  let ts = MLD.defaultString (Just lang)
      (g', t) = newvarRich [] (MLD.defaultNull (Just lang)) (Just lang) g
  return (g' +> t, t, SAnno (One (StrS x, Idx i t)) gt)

-- Log=>
synth g (SAnno (One (LogS x, (Idx i (lang, _)))) gt) = do
  let ts = MLD.defaultBool (Just lang)
      (g', t) = newvarRich [] (MLD.defaultNull (Just lang)) (Just lang) g
  return (g' +> t, t, SAnno (One (LogS x, Idx i t)) gt)

synth _ (SAnno (One (FixS, (Idx i (lang, _)))) gt) = undefined
synth g (SAnno (One (VarS v, Idx i (lang, _))) gt) = undefined
synth g (SAnno (One (AccS x k, Idx i (lang, _))) gt) = undefined
synth g (SAnno (One (ListS xs, Idx i (lang, _))) gt) = undefined
synth g (SAnno (One (TupleS xs, Idx i (lang, _))) gt) = undefined
synth g (SAnno (One (LamS vs f, Idx i (lang, _))) gt) = undefined
synth g (SAnno (One (AppS f xs, Idx i (lang, _))) gt) = undefined
synth g (SAnno (One (RecS rs, Idx i (lang, _))) gt) = undefined
synth g (SAnno (One (CallS src, Idx i (lang, es))) gt) = undefined



check
  :: Gamma
  -> SAnno (Indexed Type) One (Indexed (Lang, [EType]))
  -> UnresolvedType
  -> Either
        TypeError
        ( Gamma
        , UnresolvedType
        , SAnno (Indexed Type) One (Indexed UnresolvedType)
        )
--  g1,x:A |- e <= B -| g2,x:A,g3
-- ----------------------------------------- -->I
--  g1 |- \x.e <= A -> B -| g2
check g1 (SAnno (One (LamS vs x, _)) _) t1@(FunU a b) = undefined

--  g1,x |- e <= A -| g2,x,g3
-- ----------------------------------------- Forall.I
--  g1 |- e <= Forall x.A -| g2
check g1 e1 t2@(ForallU x a) = undefined

--  g1 |- e => A -| g2
--  g2 |- [g2]A <: [g2]B -| g3
-- ----------------------------------------- Sub
--  g1 |- e <= B -| g3
check g1 e1 b = undefined



synthApply
  :: Gamma
  -> SAnno (Indexed Type) One (Lang, [EType])
  -> UnresolvedType
  -> Either
       TypeError
       ( Gamma
       , UnresolvedType
       , SAnno (Indexed Type) One (Indexed UnresolvedType)
       )
--  g1 |- e <= A -| g2
-- ----------------------------------------- -->App
--  g1 |- A->C o e =>> C -| g2
synthApply g e (FunU a b) = undefined

--  g1,Ea |- [Ea/a]A o e =>> C -| g2
-- ----------------------------------------- Forall App
--  g1 |- Forall x.A o e =>> C -| g2
synthApply g e (ForallU x s) = undefined

--  g1[Ea2, Ea1, Ea=Ea1->Ea2] |- e <= Ea1 -| g2
-- ----------------------------------------- EaApp
--  g1[Ea] |- Ea o e =>> Ea2 -| g2
synthApply g e (ExistU v@(TV lang _) [] _) = undefined

synthApply _ e t = undefined


lookupSourceTypes :: Int -> Source -> MorlocMonad [EType]
lookupSourceTypes i src = do
  mayts <- lookupSig i
  case mayts of
    Nothing -> MM.throwError . CallTheMonkeys $ "Missing TermTypes for source"
    (Just ts) -> case [ es | (_, src', es, _) <- termConcrete ts, src' == src] of
      [es'] -> return es'
      _ -> MM.throwError . CallTheMonkeys $ "Expected exactly one list of types for a source"


-- I don't need explicit convert functions, necessarily. The pack functions can
-- be used to convert between values that are in the same language. Because
-- they hae the same general types and the general types define the packed
-- form. Minimizing convert steps would certainly be a valuable optimization,
-- but I can leave that for later.

-- -- | Ensure that all concrete source signatures match general types
-- checkSources :: SAnno (Indexed Type) One (Indexed Lang) -> MorlocMonad ()
-- checkSources (SAnno (Many xs) i) = do
--   mayts <- lookupSig i
--   case mayts |>> toTypePairs >>= mapM (uncurry checkConcrete) of
--     (Just ((e1, e2):_)) -> undefined -- create error message for mismatched general/concrete types
--     _ -> return ()
--   where
--   toTypePairs :: TermTypes -> [(EType, EType)]
--   toTypePairs (TermTypes Nothing _ _) = []
--   toTypePairs (TermTypes (Just gt) cts _) = [(gt, ct) | ct <- concat [ts | (_, _, ts, _) <- cts]]
--
--   -- return Nothing if the types are the same, otherwise return the types
--   checkConcrete :: EType -> EType -> Maybe (EType, EType)
--   checkConcrete e1 e2
--     | checkConcreteType e1 e2 = Nothing
--     | otherwise = Just (e1, e2)
--
-- -- | This is a key function that is exported primarily so it can be tested.
-- checkConcreteType :: EType -> EType -> Bool
-- checkConcreteType = undefined
