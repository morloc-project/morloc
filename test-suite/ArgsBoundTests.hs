{-# LANGUAGE OverloadedStrings #-}

-- | A serial variable that names an argument of its manifold is tagged as
-- one, so a manifold returning an argument packet returns a copy of it.
module ArgsBoundTests (argsBoundTests) where

import Morloc.CodeGenerator.Grammars.Common (serialManifoldArgsBound)
import Morloc.CodeGenerator.Namespace
import Morloc.Language (makeLang)
import Morloc.Namespace.Prim
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

argsBoundTests :: TestTree
argsBoundTests =
  testGroup
    "serial variables naming manifold arguments"
    [ testCase "a returned let variable naming an argument becomes bound" $
        returned (ReturnS (LetVarS t 1)) @?= Just (True, 1)
    , testCase "a let variable naming no argument stays a let variable" $
        returned (SerialLetS 2 (LetVarS t 1) (ReturnS (LetVarS t 2))) @?= Just (False, 2)
    , testCase "a serial let of the argument's index hides it in its body" $
        returned (SerialLetS 1 (LetVarS t 1) (ReturnS (LetVarS t 1))) @?= Just (False, 1)
    , testCase "a serial let's right-hand side still sees the argument" $
        rhsTag (SerialLetS 1 (LetVarS t 1) (ReturnS (LetVarS t 1))) @?= Just True
    , testCase "a native let of the argument's index does not hide it" $
        returned (NativeLetS 1 (IntN (FV (TV "Int") (CV "int")) 0) (ReturnS (LetVarS t 1))) @?= Just (True, 1)
    , testCase "a nested manifold is tagged by its own arguments" $
        returned (ReturnS (ManS (manifold [2] (ReturnS (LetVarS t 1))))) @?= Just (False, 1)
    ]
  where
    t = Just intF
    intF = VarF (FV (TV "Int") (CV "int"))
    lang = makeLang "cpp" "cpp"
    manifold args body =
      SerialManifold 0 lang (ManifoldFull [Arg i (R intF) | i <- args]) HeadManifoldFormLocalForeign body

    run e = case serialManifoldArgsBound (manifold [1] e) of
      SerialManifold _ _ _ _ body -> body

    -- the variable returned at the end of the body: (is it bound, index)
    returned e = final (run e)
    final (ReturnS (BndVarS _ i)) = Just (True, i)
    final (ReturnS (LetVarS _ i)) = Just (False, i)
    final (ReturnS (ManS (SerialManifold _ _ _ _ body))) = final body
    final (SerialLetS _ _ e) = final e
    final (NativeLetS _ _ e) = final e
    final _ = Nothing

    rhsTag e = case run e of
      SerialLetS _ (BndVarS _ _) _ -> Just True
      SerialLetS _ (LetVarS _ _) _ -> Just False
      _ -> Nothing
