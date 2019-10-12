module PropertyTypeTests
  ( propertyTypeTests
  ) where

import Morloc.Namespace
import Morloc.Parser.Parser
import Morloc.TypeChecker.Infer
import Morloc.TypeChecker.Util
import Morloc.TypeChecker.API

import qualified Control.Monad as CM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Test.QuickCheck as QC
import Test.Tasty
import Test.Tasty.QuickCheck as TQC

propertyTypeTests =
  testGroup
    "Typechecking property tests"
   -- generalization
    [ TQC.testProperty "size(Gen(t)) >= size(t)" $ \t ->
        typeSize (generalize t) >= typeSize t
   -- -- quantifier term renaming
   -- , TQC.testProperty "e == unrename(rename e)" renameTest
   -- substitution
    , TQC.testProperty "size([v/<v>]t) == size(t)" $ \(v, t) ->
        typeSize (substitute v t) == typeSize t
   -- subtype tests
    , TQC.testProperty "t <: t" $ \t -> subtypeOf t t []
   -- -- generalizeE tests
   -- , TQC.testProperty "unannotate(e) == unannotate(generalizeE(e))" $
   --     \e -> unannotate e == unannotate (map generalizeE e)
   -- apply
    , TQC.testProperty "apply [] t == t" $ \t -> apply [] t == (t :: Type)
   -- -- applyE
   -- , TQC.testProperty "applyE [] e == e" $
   --    \e -> applyE [] e == e
   -- free
    , TQC.testProperty "length(free t) <= size t" $ \t ->
        Set.size (free t) <= typeSize t
   -- -- infer1
   -- , TQC.testProperty "i4.2 == annotationOf(i4.3)" infer1
   -- -- infer2
   -- , TQC.testProperty "unannotate e == unannotate (infer e)" infer2
    ]

-- -- remove all type annotations and type signatures
-- unannotate :: [Expr] -> [Expr]
-- unannotate = concat . map unannotate' where
--   unannotate' (AnnE e t) = unannotate' e
--   unannotate' (ListE xs) = return $ ListE (map unannotate' xs)
--   unannotate' (TupleE xs) = return $ TupleE (map unannotate' xs)
--   unannotate' (LamE v e) = return $ LamE v (unannotate' e)
--   unannotate' (AppE e1 e2) = return $ AppE (unannotate' e1) (unannotate' e2)
--   unannotate' (Declaration v e) = return $ Declaration v (unannotate' e)
--   unannotate' (Signature v t) = []
--   unannotate' e = [e]
--
-- annotationOf :: Expr -> Maybe Type
-- annotationOf (AnnE e t) = Just t
-- annotationOf _ = Nothing
typeSize :: Type -> Int
typeSize (UniT) = 1
typeSize (VarT _) = 1
typeSize (ExistT _) = 1
typeSize (Forall _ t) = 1 + typeSize t
typeSize (FunT t1 t2) = 1 + typeSize t1 + typeSize t2
typeSize (ArrT _ xs) = 1 + sum (map typeSize xs)
typeSize (RecT xs) = 1 + sum (map (typeSize . snd) xs)

subtypeOf :: Type -> Type -> Gamma -> Bool
subtypeOf t1 t2 g =
  case runStack (subtype t1 t2 g) of
    (Right _, _) -> True
    (Left _, _) -> False

-- infer1 :: [Expr] -> Bool
-- infer1 es = case runStack (typecheck es) of
--   (Right (g, t, es'), _) -> all (infer1' t) es'
--   (Left e, _) -> False
--   where
--     infer1' t (Declaration _ _) = True
--     infer1' t (Signature _ _) = True
--     infer1' t e2 = case (annotationOf e2) of
--       (Just t') -> t' == t
--       Nothing -> False
--
-- infer2 :: [Expr] -> Bool
-- infer2 es = case runStack (typecheck es) of
--   (Right (g, t, es'), _) -> unannotate es == unannotate es'
--   (Left _, _) -> False
--
-- renameTest :: [Expr] -> Bool
-- renameTest es = case runStack (mapM renameExpr es) of
--   (Right es', _) -> map unrenameExpr es' == es
--   (Left _, _) -> False
instance QC.Arbitrary Type where
  arbitrary = arbitraryType 3 []
  shrink (UniT) = [VarT (TV Nothing "X")]
  shrink (VarT (TV Nothing "X")) = []
  shrink (VarT _) = [VarT (TV Nothing "X")]
  shrink (ExistT _) = [VarT (TV Nothing "X")]
  shrink (Forall v t) = QC.shrink t ++ [t] ++ [Forall v t' | t' <- QC.shrink t]
  shrink (FunT t1 t2) =
    QC.shrink t1 ++
    QC.shrink t2 ++
    [t1, t2] ++
    [FunT t1' t2' | (t1', t2') <- QC.shrink (t1, t2)] ++
    [FunT t1' t2 | t1' <- QC.shrink t1] ++ [FunT t1 t2' | t2' <- QC.shrink t2]
  shrink (ArrT _ []) = [] -- this expression should not be generated
  shrink (ArrT v@(TV Nothing "L") [p1, p2, p3]) =
    [VarT (TV Nothing "X")] ++
    QC.shrink p1 ++
    QC.shrink p2 ++
    QC.shrink p3 ++
    QC.shrink (ArrT (TV Nothing "K") [p1, p2]) ++
    QC.shrink (ArrT (TV Nothing "K") [p1, p3]) ++
    QC.shrink (ArrT (TV Nothing "K") [p2, p3]) ++
    [ArrT v [p1', p2', p3'] | (p1', p2', p3') <- QC.shrink (p1, p2, p3)]
  shrink (ArrT v@(TV Nothing "K") [p1, p2]) =
    [VarT (TV Nothing "X")] ++
    QC.shrink p1 ++
    QC.shrink p2 ++
    QC.shrink (ArrT (TV Nothing "J") [p1]) ++
    QC.shrink (ArrT (TV Nothing "J") [p2]) ++
    [ArrT v [p1', p2'] | (p1', p2') <- QC.shrink (p1, p2)]
  shrink (ArrT (TV Nothing "J") [p]) = [VarT (TV Nothing "X")] ++ QC.shrink p
  shrink (ArrT v (p:ps)) =
    [VarT (TV Nothing "X")] ++
    [ ArrT v (p' : ps')
    | p' <- QC.shrink p
    , (ArrT _ ps') <- QC.shrink (ArrT v ps)
    ] ++
    [ArrT v (p : ps') | (ArrT _ ps') <- QC.shrink (ArrT v ps)]
  shrink (RecT []) = [VarT (TV Nothing "X")]
  shrink (RecT xs) = [VarT (TV Nothing "X")] ++ [RecT (tail xs)]
  -- | RecT [(TVar, Type)]

arbitraryType :: Int -> [TVar] -> QC.Gen Type
arbitraryType depth vs =
  QC.oneof
    [ arbitraryType' depth vs
    , Forall <$> pure (newvar' vs) <*> arbitraryType depth (newvar' vs : vs)
    ]
  where
    variables = [1 ..] >>= flip CM.replicateM ['a' .. 'z']
    newvar' vs' = TV Nothing (T.pack $ variables !! length vs')

arbitraryType' :: Int -> [TVar] -> QC.Gen Type
arbitraryType' 0 vs = atomicType vs
arbitraryType' depth vs =
  QC.oneof
    [ atomicType vs
    , FunT <$> arbitraryType' (depth - 1) vs <*> arbitraryType' (depth - 1) vs
    , QC.elements [ExistT (TV Nothing "e1"), ExistT (TV Nothing "e2"), ExistT (TV Nothing "e3")]
    , QC.frequency
        [ (3, arbitraryArrT (depth - 1) vs (TV Nothing "J") 1)
        , (2, arbitraryArrT (depth - 1) vs (TV Nothing "K") 2)
        , (1, arbitraryArrT (depth - 1) vs (TV Nothing "L") 3)
        ]
    ]

atomicType :: [TVar] -> QC.Gen Type
atomicType [] = QC.elements [VarT (TV Nothing "A"), VarT (TV Nothing "B"), VarT (TV Nothing "C")]
atomicType vs = QC.oneof [atomicType [], QC.elements (map (\v -> VarT v) vs)]

arbitraryArrT :: Int -> [TVar] -> TVar -> Int -> QC.Gen Type
arbitraryArrT depth vs v arity =
  ArrT <$> pure v <*> CM.replicateM arity (arbitraryType' depth vs)

-- instance QC.Arbitrary Expr where
--   arbitrary = arbitraryExpr 3 3 [] []
--   shrink _ = []
--
-- arbitraryExpr :: Int -> Int -> [(EVar, Expr)] -> [(EVar, Type)] -> QC.Gen Expr
-- arbitraryExpr t n es ss = QC.oneof [
--       arbitraryDeclaration t n es ss
--     , arbitrarySignature   t n es ss
--     , arbitraryFinalE n es ss []
--   ]
--
-- evars = (map (\i -> (EV . T.pack) ('x':show i)) [0..])
--
-- arbitraryDeclaration :: Int -> Int -> [(EVar, Expr)] -> [(EVar, Type)] -> QC.Gen Expr
-- arbitraryDeclaration t n es ss = do
--   -- This generator cannot generate a signature for a declared variable.
--   -- This is of course a spectacular limitation, but I adding type annotations
--   -- to a generated expression seems a bit hard.
--   let v = evars !! (length es + length ss)
--   e <- arbitraryFinalE n es ss []
--   r <- arbitraryExpr (t-1) n ((v,e):es) ss
--   return $ Declaration v e r
--
-- arbitrarySignature :: Int -> Int -> [(EVar, Expr)] -> [(EVar, Type)] -> QC.Gen Expr
-- arbitrarySignature t n es ss = do
--   let v = evars !! (length es + length ss)
--   e <- arbitraryType n []
--   r <- arbitraryExpr (t-1) n es ((v,e):ss)
--   return $ Signature v e r
--
-- arbitraryFinalE :: Int -> [(EVar, Expr)] -> [(EVar, Type)] -> [EVar] -> QC.Gen Expr
-- arbitraryFinalE t es ss ls
--   | t > 1 = QC.frequency
--       [ (length(vars), QC.elements vars)
--       , (1+t, fmap (\p -> ListE [p] ) arbitraryPrimitive)
--       , (1+t, QC.frequency [ (2, arbitraryTuple 2 (t-1) es ss ls)
--                            , (1, arbitraryTuple 3 (t-1) es ss ls)])
--       , (1+t, arbitraryPrimitive)
--       , (4*t, LamE <$> return evar <*> arbitraryFinalE (t-1) es ss (evar : ls))
--       ]
--   | otherwise = QC.frequency
--       [ (length(vars), QC.elements vars)
--       , (1, arbitraryPrimitive)
--       ]
--   where
--     vars = [VarE v | v <- map fst es ++ map fst ss ++ ls]
--     evar = evars !! (length es + length ss + length ls)
--
-- arbitraryTuple :: Int -> Int -> [(EVar, Expr)] -> [(EVar, Type)] -> [EVar] ->  QC.Gen Expr
-- arbitraryTuple i t es ts ls
--   | i == 2 = TupleE <$> sequence [ arbitraryFinalE t es ts ls
--                                  , arbitraryFinalE t es ts ls]
--   | i == 3 = TupleE <$> sequence [ arbitraryFinalE t es ts ls
--                                  , arbitraryFinalE t es ts ls
--                                  , arbitraryFinalE t es ts ls]
--   | otherwise = return $ VarE (EV "clusterfuck")
--
-- arbitraryPrimitive :: QC.Gen Expr
-- arbitraryPrimitive = QC.oneof [
--       fmap IntE (QC.arbitrary :: QC.Gen Integer)
--     , fmap NumE (QC.arbitrary :: QC.Gen Double)
--     , fmap LogE (QC.arbitrary :: QC.Gen Bool)
--     , QC.elements [StrE "foo", StrE "bar"]
--   ]
instance QC.Arbitrary GammaIndex where
  arbitrary = undefined
  shrink = undefined

instance QC.Arbitrary TVar where
  arbitrary = QC.elements [TV Nothing "a", TV Nothing "b", TV Nothing "c"]
  shrink _ = []

instance QC.Arbitrary EVar where
  arbitrary = undefined
  shrink = undefined
