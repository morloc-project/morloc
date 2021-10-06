{-|
Module      : Morloc.Frontend.Typecheck
Description : Core inference module
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.Frontend.Typecheck (typecheck, resolveTypes) where

import Morloc.Frontend.Namespace
import Morloc.Typecheck.Internal
import Morloc.Pretty
import Morloc.Typecheck.Pretty
import Morloc.Data.Doc
import qualified Morloc.Frontend.Lang.DefaultTypes as MLD
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.Monad as MM

import qualified Control.Monad.State as CMS

-- true facts to keep in mind:
--   * indexing is a good idea, I need it at very least to link source code lines to
--     error messages

-- | Each SAnno object in the input list represents one exported function.
-- Modules, scopes, imports and and everything else are abstracted away,
-- wrapped into GMeta or stored in the Stack state.
typecheck
  :: [SAnno Int Many Int]
  -> MorlocMonad [SAnno (Indexed TypeU) Many Int]
typecheck es = mapM typecheckGeneral es

resolveTypes :: SAnno (Indexed TypeU) Many Int -> SAnno (Indexed Type) Many Int
resolveTypes (SAnno (Many es) (Idx i t))
  = SAnno (Many (map (\(e, i') -> (f e, i')) es)) (Idx i (typeOf t)) where
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
  f (VarS x) = VarS x


typecheckGeneral
  :: SAnno Int Many Int
  -> MorlocMonad (SAnno (Indexed TypeU) Many Int)
typecheckGeneral x = do
  s <- CMS.gets stateSignatures
  case typecheckGeneralPure (lookupType s) initialContext x of
    -- -- to see the SAnno expression
    -- (Left _) -> error . show . render . prettySAnno viaShow viaShow $ x
    (Left (Idx _ err')) -> MM.throwError $ GeneralTypeError err'
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
typecheckGeneralPure f g e = fmap (\(_, _, e') -> e') (synthG f g e)

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
synthG _ _ (SAnno (Many []) i) = Left $ Idx i EmptyExpression
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
checkG _ g (SAnno (Many []) i) t = return (g, t, SAnno (Many []) (Idx i t)) 
checkG l g0 (SAnno (Many ((e, j):es)) i) t0 = do 
  (g1, t1, e') <- checkE l i g0 e t0
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

synthE _ _ g (UniS) = return (g, MLD.defaultGeneralType UniS, UniS)
synthE _ _ g (NumS x) = return (g, MLD.defaultGeneralType (NumS x), NumS x)
synthE _ _ g (LogS x) = return (g, MLD.defaultGeneralType (LogS x), LogS x)
synthE _ _ g (StrS x) = return (g, MLD.defaultGeneralType (StrS x), StrS x)

synthE l i g (AccS e k) = do
  (g1, t1, e1) <- synthG l g e
  valType <- case t1 of
    (NamU _ _ _ rs) -> case lookup k rs of
      Nothing -> Left $ Idx i (KeyError k t1)
      (Just t) -> return t
    _ -> Left $ Idx i (KeyError k t1)
  return (g1, valType, AccS e1 k)
synthE l _ g (AppS f []) = do
  (g1, t1, f1) <- synthG l g f
  return (g1, t1, AppS f1 [])
synthE l i g0 e@(AppS f xs) = do

  -- get the potentially qualified function type and expression
  (g1, qFunType, qFunExpr) <- synthG l g0 f

  -- unqualify the expression
  (g2, uFunType, uFunExpr) <- application g1 qFunType qFunExpr

  -- extract output type from the type of f
  (ts, outputType) <- case uFunType of
    (FunU ts t) -> return (ts, t)
    _ -> error "impossible"

  {- Showing `map fst [(1,True),(2,False)]` as an example
  qFunType:   forall a b . (a -> b) -> List a -> List b
  g1: []
  uFunType:   (<a[][]> -> <b[][]>) -> List <a[][]>\n -> List <b[][]>
  g2: [ExistG b [] [], ExistG a [] []]
  g3: [ExistG: t1 [] [], SolvedG: t0 = Tuple2 Num Bool, ExistG: b [] [], SolvedG: a = Tuple2 <t1[][]> <t2[][]>]
  finalType: List <b[][]>
  -}
 
  -- check the tuple of argument expressions against a tuple of argument types,
  -- collect the types of missing arguments (partial application)
  (leftoverTypes, (g3, argTupleType, argTupleExpr)) <- case compare (length ts) (length xs) of
    -- there are more inputs than arguments: partial application
    GT -> case splitAt (length xs) ts of
      (ts', remainder) -> let tupleType = head $ MLD.defaultTuple Nothing ts'
                          in checkE l i g2 (TupS xs) tupleType |>> (,) remainder
    -- there are the same number of inputs and arguments: full application
    EQ -> let tupleType = head $ MLD.defaultTuple Nothing ts
          in checkE l i g2 (TupS xs) tupleType |>> (,) []
    -- there are more arguments than inputs: TYPE ERROR!!!
    LT -> Left (Idx i TooManyArguments)

  -- error . show . render . vsep $ map prettyGammaIndex (gammaContext g3)

  -- extract the types of the input arguments
  inputTypes <- case argTupleType of
    (AppU _ ts') -> return ts'
    _ -> error "impossible"

  -- extract the input expressions
  inputExprs <- case argTupleExpr of
    (TupS xs') -> return xs'
    _ -> error "impossible"

  -- synthesize the final type
  finalType <- case leftoverTypes of
    -- full application, just return the output type
    [] -> return outputType
    -- partial application, create a new function with unapplied types
    ts' -> return (FunU ts' outputType)

  -- error . show . render $ prettyGreenTypeU finalType <+> "||" <+> prettyGreenTypeU (apply g3 finalType)

  -- put the AppS back together with the synthesized function and input expressions
  return (g3, apply g3 finalType, AppS uFunExpr inputExprs)

synthE l i g0 (LamS vs x) = do
  let (g1, ts) = statefulMap (\g' _ -> newvar Nothing g') g0 vs
      (g2, t) = newvar Nothing g1
      marks = zipWith AnnG vs ts
      g3 = g2 ++> marks 
  (g4, t', x') <- checkG l g3 x t
  let funType = apply g4 (FunU ts t')
  g5 <- cut' i (head marks) g4
  return (g5, funType, LamS vs x')
  where
    bindTerm :: Gamma -> EVar -> (Gamma, TypeU)
    bindTerm g0' v' =
      let (g1', t) = newvar Nothing g0'
          idx = AnnG v' t
      in (g1' +> idx, t)

synthE _ _ g (LstS []) =
  let (g1, itemType) = newvar Nothing g
      listType = head $ MLD.defaultList Nothing itemType
  in return (g1, listType, LstS [])
synthE l i g (LstS (e:es)) = do
  (g1, itemType, itemExpr) <- synthG l g e 
  (g2, listType, listExpr) <- checkE l i g1 (LstS es) (head $ MLD.defaultList Nothing itemType)
  case listExpr of
    (LstS es') -> return (g2, listType, LstS (itemExpr:es'))
    _ -> error "impossible"

synthE _ _ g (TupS []) =
  let t = head $ MLD.defaultTuple Nothing []
  in return (g, t, TupS [])
synthE l i g (TupS (e:es)) = do
  -- synthesize head
  (g1, itemType, itemExpr) <- synthG l g e

  -- synthesize tail
  (g2, tupleType, tupleExpr) <- synthE l i g1 (TupS es)

  -- merge the head and tail
  t3 <- case tupleType of
    (AppU _ ts) -> return . head $ MLD.defaultTuple Nothing (apply g2 itemType : ts)
    _ -> error "impossible" -- the general tuple will always be (AppU _ _)

  xs' <- case tupleExpr of
    (TupS xs') -> return xs'
    _ -> error "impossible" -- synth does not change data constructors

  return (g2, t3, TupS (itemExpr:xs'))

synthE _ _ g (NamS []) = return (g, head $ MLD.defaultRecord Nothing [], NamS [])
synthE l i g0 (NamS ((k,x):rs)) = do
  -- type the head
  (g1, headType, headExpr) <- synthG l g0 x

  -- type the tail
  (g2, tailType, tailExpr) <- synthE l i g1 (NamS rs)

  -- merge the head with tail
  t <- case tailType of
    (NamU o1 n1 ps1 rs1) -> return $ NamU o1 n1 ps1 ((k, apply g2 headType):rs1)
    _ -> error "impossible" -- the synthE on NamS will always return NamU type

  tailExprs <- case tailExpr of
    (NamS xs') -> return xs'
    _ -> error "impossible" -- synth does not change data constructors

  return (g2, t, NamS ((k, headExpr):tailExprs))

-- Sources are axiomatic. They are they type they are said to be.
synthE l i g (CallS src) = do
  t <- case l i of 
    (Just t) -> return t
    Nothing -> Left (Idx i (MissingGeneralSignature src))
  return (g, t, CallS src)

-- Any morloc variables should have been expanded by treeify. Any bound
-- variables should be checked against. I think (this needs formalization).
synthE l i g (VarS v) = do
  -- is this a bound variable that has already been solved
  case lookupE v g of 
    -- yes, return the solved type
    (Just t) -> return (g, t, VarS v)
    -- no, so is it a variable that has a type annotation?
    Nothing -> case l i of 
      (Just t) -> return (g, t, VarS v)
      -- no, then I have no idea what it is, so make a new existential
      Nothing ->
        let (g', t) = newvar Nothing g
        in return (g', t, VarS v)


checkE
  :: (Int -> Maybe TypeU)
  -> Int
  -> Gamma
  -> SExpr Int Many Int
  -> TypeU
  -> Either
       (Indexed TypeError)
       ( Gamma
       , TypeU
       , SExpr (Indexed TypeU) Many Int
       )
checkE l i g1 (LstS (e:es)) (AppU v [t]) = do
  (g2, t2, _) <- checkG l g1 e t 
  -- LstS [] will go to the normal Sub case
  checkE l i g2 (LstS es) (AppU v [t2])
checkE l _ g1 (LamS [] e1) (FunU as1 b1) = do
  (g2, b2, e2) <- checkG l g1 e1 b1
  return (g2, FunU as1 b2, LamS [] e2)
checkE l i g1 (LamS (v:vs) e1) (FunU (a1:as1) b1) = do
  -- defined x:A
  let vardef = AnnG v a1
      g2 = g1 +> vardef
  -- peal off one layer of bound terms and check
  (g3, t3, e2) <- checkE l i g2 (LamS vs e1) (FunU as1 b1)

  -- construct the final type
  t4 <- case t3 of
    (FunU as2 b2) -> return $ FunU (a1:as2) b2
    _ -> error "impossible"

  let t5 = apply g3 t4

  -- construct the final expression
  e3 <- case e2 of
    (LamS vs' body) -> return $ LamS (v:vs') body
    _ -> error "impossible"

  -- ignore trailing context `x:A,g3`
  g4 <- cut' i vardef g3

  return (g4, t5, e3)

checkE l i g1 e1 t2@(ForallU x a) = do
  (g2, _, e2) <- checkE l i (g1 +> VarG x) e1 a
  g3 <- cut' i (VarG x) g2
  let t3 = apply g3 t2
  return (g3, t3, e2)

checkE l i g1 e1 b = do
  (g2, a, e2) <- synthE l i g1 e1
  let a' = apply g2 a
      b' = apply g2 b
  g3 <- case subtype a' b' g2 of
    (Left err') -> Left (Idx i err')
    (Right x) -> Right x
  return (g3, apply g3 a', e2)


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
application g0 t0 e0@(SAnno (Many []) _) = return (g0, t0, e0)
application g0 t0 (SAnno (Many ((e0, i):es0)) m@(Idx j _)) = do
  (g1, t1, e1) <- applicationExpr j g0 t0 e0
  (g2, t2, e2) <- application g1 t1 (SAnno (Many es0) m)
  case e2 of
    (SAnno (Many es1) _) -> return (g2, t2, SAnno (Many ((e1, i):es1)) m) 

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
