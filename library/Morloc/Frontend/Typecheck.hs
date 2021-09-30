{-|
Module      : Morloc.Frontend.Typecheck
Description : Core inference module
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.Frontend.Typecheck (typecheck) where

import Morloc.Frontend.Namespace
import Morloc.Frontend.Internal
import Morloc.Typecheck.Internal
import qualified Morloc.Frontend.Lang.DefaultTypes as MLD
import qualified Morloc.Data.DAG as MDD
import qualified Morloc.Data.GMap as GMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Morloc.Data.Text as MT
import qualified Control.Monad.Reader as R

import Morloc.Data.Doc hiding (putDoc)
import Morloc.Frontend.Pretty
import Data.Text.Prettyprint.Doc.Render.Terminal (putDoc, AnsiStyle)
import qualified Control.Monad.State as CMS

-- true facts to keep in mind:
--   * indexing is a good idea, I need it at very least to link source code lines to
--     error messages

-- | Each SAnno object in the input list represents one exported function.
-- Modules, scopes, imports and and everything else are abstracted away,
-- wrapped into GMeta or stored in the Stack state.
typecheck
  :: [SAnno Int Many Int]
  -> MorlocMonad [SAnno (Indexed Type) Many Int]
typecheck es = mapM typecheckGeneral es |>> map resolveTypes

resolveTypes :: SAnno (Indexed TypeU) Many Int -> SAnno (Indexed Type) Many Int
resolveTypes (SAnno (Many es) (Idx i t)) = SAnno (Many (map (\(e, i) -> (f e, i)) es)) (Idx i (typeOf t)) where
  f :: SExpr (Indexed TypeU) Many Int -> SExpr (Indexed Type) Many Int
  f (AccS x k) = AccS (resolveTypes x) k
  f (AppS x xs) = AppS (resolveTypes x) (map resolveTypes xs) 
  f (LamS vs x) = LamS vs (resolveTypes x)
  f (LstS xs) = LstS (map resolveTypes xs)
  f (TupS xs) = TupS (map resolveTypes xs)
  f (NamS rs) = NamS (zip (map fst rs) (map (resolveTypes . snd) rs))
  f (NumS x) = NumS x
  f (LogS x) = LogS x
  f (StrS x) = StrS x
  f (CallS x) = CallS x
  f UniS = UniS


typecheckGeneral
  :: SAnno Int Many Int
  -> MorlocMonad (SAnno (Indexed TypeU) Many Int)
typecheckGeneral x = do
  s <- CMS.gets stateSignatures
  case typecheckGeneralPure (lookupType s) initialContext x of
    (Left err) -> undefined
    (Right x') -> return x'
  where
    initialContext = Gamma
      { gammaCounter = 0
      , gammaContext = []
      }
    
    lookupType :: GMap Int Int TermTypes -> Int -> Maybe TypeU
    lookupType m i = case GMap.lookup i m of
      GMapNoFst -> Nothing
      GMapNoSnd -> Nothing
      GMapJust (TermTypes t _ _) -> fmap etype t

-- | Check the general types, do nothing to the concrete types which may only be
-- solved after segregation. Later the concrete types will need to be checked
-- for type consistency, correctness of packers, inferences of packers (both
-- for serialization and for casting).
typecheckGeneralPure
  :: (Int -> Maybe TypeU)
  -> Gamma
  -> SAnno Int Many Int
  -> Either (Indexed TypeError) (SAnno (Indexed TypeU) Many Int)
typecheckGeneralPure f g e = fmap (\(_,_,e) -> e) (synthG f g e)

synthG
  :: (Int -> Maybe TypeU)
  -> Gamma
  -> SAnno Int Many Int
  -> Either
       (Indexed TypeError)
       ( Gamma
       , TypeU
       , SAnno (Indexed TypeU) Many Int
       )
synthG _ _ (SAnno (Many []) _) = impossible
synthG l g0 (SAnno (Many ((e, j):es)) i) = do
  (g1, t1, e') <- synthE l i g0 e
  (g2, t2, SAnno (Many es') _) <- checkG l g1 (SAnno (Many es) i) t1
  return (g2, t2, SAnno (Many ((e', j):es')) (Idx i t2))

checkG
  :: (Int -> Maybe TypeU)
  -> Gamma
  -> SAnno Int Many Int
  -> TypeU
  -> Either
       (Indexed TypeError)
       ( Gamma
       , TypeU
       , SAnno (Indexed TypeU) Many Int
       )
checkG l g (SAnno (Many []) i) t = return (g, t, SAnno (Many []) (Idx i t)) 
checkG l g0 (SAnno (Many ((e, j):es)) i) t0 = do 
  (g1, t1, e') <- checkE l g0 e t0
  (g2, t2, SAnno (Many es') idType) <- checkG l g1 (SAnno (Many es) i) t1
  return (g2, t2, SAnno (Many ((e', j):es')) idType)


synthE
  :: (Int -> Maybe TypeU)
  -> Int
  -> Gamma
  -> SExpr Int Many Int
  -> Either
       (Indexed TypeError)
       ( Gamma
       , TypeU
       , SExpr (Indexed TypeU) Many Int
       )
synthE l i g (UniS) = return (g, MLD.defaultGeneralType UniS, UniS)
synthE l i g (NumS x) = return (g, MLD.defaultGeneralType (NumS x), NumS x)
synthE l i g (LogS x) = return (g, MLD.defaultGeneralType (LogS x), LogS x)
synthE l i g (StrS x) = return (g, MLD.defaultGeneralType (StrS x), StrS x)
synthE l i g (AccS e k) = do
  (g1, t1, e1) <- synthG l g e
  valType <- case t1 of
    (NamU _ _ _ rs) -> case lookup k rs of
      Nothing -> Left $ Idx i (KeyError k t1)
      (Just t) -> return t
    _ -> Left $ Idx i (KeyError k t1)
  return (g1, valType, AccS e1 k)
synthE l i g (AppS f []) = do
  (g1, t1, f1) <- synthG l g f
  return (g1, t1, AppS f1 [])
synthE l i g0 (AppS f xs) = do

  -- get the potentially qualified function type and expression
  (g1, qfunType, qfunExpr) <- synthG l g0 f

  -- unqualify the expression
  (g2, uFunType, uFunExpr) <- application g1 qfunType qfunExpr

  -- extract output type from the type of f
  (ts, outputType) <- case uFunType of
    (FunU ts t) -> return (ts, t)
    _ -> impossible

  -- create a tuple from the input arguments
  let tupleType = head $ MLD.defaultTuple Nothing ts

  -- check the tuple of argument expressions against a tuple of argument types,
  -- collect the types of missing arguments (partial application)
  (leftoverTypes, (g3, argTupleType, argTupleExpr)) <- case compare (length ts) (length xs) of
    -- there are more inputs than arguments: partial application
    GT -> case splitAt (length xs) ts of
      (args, remainder) -> checkE l g2 (TupS xs) tupleType |>> (,) remainder
    -- there are the same number of inputs and arguments: full application
    EQ -> checkE l g2 (TupS xs) tupleType |>> (,) []
    -- there are more arguments than inputs: TYPE ERROR!!!
    LT -> Left (Idx i TooManyArguments)

  -- extract the types of the input arguments
  inputTypes <- case argTupleType of
    (AppU _ ts') -> return ts'
    _ -> impossible

  -- extract the input expressions
  inputExprs <- case argTupleExpr of
    (TupS xs') -> return xs'
    _ -> impossible

  -- synthesize the final type
  finalType <- case leftoverTypes of
    -- full application, just return the output type
    [] -> return outputType
    -- partial application, create a new function with unapplied types
    ts -> return (FunU ts outputType)

  -- put the AppS back together with the synthesized function and input expressions
  return (g3, finalType, AppS uFunExpr inputExprs)

synthE l i g0 (LamS (v@(EV n):vs) x) = do
  let mark = MarkG (TV Nothing n)
      g1 = g0 +> mark
      (g2, headType) = bindTerm g1 v

  (g3, tailType, tailExpr) <- synthE l i g2 (LamS vs x)

  fullType <- case tailType of
    (FunU tailInputs tailOutput) -> return (FunU (headType:tailInputs) tailOutput)
    _ -> impossible -- LamS type is always a function (see base case)

  fullExpr <- case tailExpr of
    (LamS vs x) -> return $ LamS (v:vs) x
    _ -> impossible -- synthExpr does not change data constructors

  g4 <- cut' i mark g3

  return (g4, fullType, fullExpr)
  where
    bindTerm :: Gamma -> EVar -> (Gamma, TypeU)
    bindTerm g0 v =
      let (g1, t) = newvar Nothing g0
          idx = AnnG v t
      in (g1 +> idx, t)



synthE l i g (LstS []) =
  let (g1, itemType) = newvar Nothing g
      tupleType = head $ MLD.defaultList Nothing itemType
  in return (g, tupleType, LstS [])
synthE l i g (LstS (e:es)) = do
  (g1, itemType, itemExpr) <- synthG l g e 
  (g2, listType, listExpr) <- checkE l g1 (LstS es) (head $ MLD.defaultList Nothing itemType)
  case listExpr of
    (LstS es') -> return (g2, listType, LstS (itemExpr:es'))
    _ -> impossible
synthE l i g (TupS []) =
  let t = head $ MLD.defaultTuple Nothing []
  in return (g, t, LstS [])
synthE l i g (TupS (e:es)) = do
  -- synthesize head
  (g1, itemType, itemExpr) <- synthG l g e

  -- synthesize tail
  (g2, tupleType, tupleExpr) <- synthE l i g (TupS es)

  -- merge the head and tail
  t3 <- case tupleType of
    (AppU _ ts) -> return . head $ MLD.defaultTuple Nothing (itemType:ts)
    _ -> impossible -- the general tuple will always be (AppU _ _)

  xs' <- case tupleExpr of
    (TupS xs') -> return xs'
    _ -> impossible -- synth does not change data constructors

  return (g2, t3, TupS (itemExpr:xs'))

synthE l i g (NamS rs) = undefined
synthE l i g (CallS src) = undefined
synthE l i g (VarS v) = Left $ (Idx i (UnboundVariable v))


checkE
  :: (Int -> Maybe TypeU)
  -> Gamma
  -> SExpr Int Many Int
  -> TypeU
  -> Either
       (Indexed TypeError)
       ( Gamma
       , TypeU
       , SExpr (Indexed TypeU) Many Int
       )
checkE = undefined


application
  :: Gamma
  -> TypeU
  -> SAnno (Indexed TypeU) Many Int
  -> Either
       (Indexed TypeError)
       ( Gamma
       , TypeU
       , SAnno (Indexed TypeU) Many Int
       )
application g0 t0 (SAnno (Many ((e0, i):es0)) m@(Idx j _)) = do
  (g1, t1, e1) <- applicationExpr j g0 t0 e0
  (g2, t2, e2) <- application g1 t1 (SAnno (Many es0) m)
  case e2 of
    (SAnno (Many es1) _) -> return (g2, t2, SAnno (Many ((e1, i):es1)) m) 
    _ -> impossible

applicationExpr
  :: Int
  -> Gamma
  -> TypeU
  -> SExpr (Indexed TypeU) Many Int
  -> Either
       (Indexed TypeError)
       ( Gamma
       , TypeU
       , SExpr (Indexed TypeU) Many Int
       )
applicationExpr i g0 (ForallU v t) e = applicationExpr i (g0 +> ExistG v [] []) (substitute v t) e
applicationExpr _ g0 t@(FunU _ _) e = return (g0, t, e)
applicationExpr i _ _ _ = Left (Idx i ApplicationOfNonFunction)

cut' :: Int -> GammaIndex -> Gamma -> Either (Indexed TypeError) Gamma
cut' i idx g = case cut idx g of
  (Left x) -> Left (Idx i x)
  (Right x) -> Right x
