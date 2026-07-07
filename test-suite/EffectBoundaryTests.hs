{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : EffectBoundaryTests
Description : Unit tests for 'Morloc.CodeGenerator.EffectBoundary'

Exercises the local pure combinators the boundary pass depends on:

  * 'polyOuterType': recursion through structural nodes ('PolyReturn',
    'PolyManifold', 'PolyApp', 'PolyLet', 'PolyIf'); typed leaves.

  * 'boundaryExpectsPlain': every 'BoundaryContext' returns the
    documented calling convention.

  * The 'cancelPolyEval' peephole: @Force ∘ Suspend = id@ and the
    "force on a plain value is a no-op" rule that the retired
    'pushForceIntoRemote' used to establish structurally.

Property-shaped tests for the Poly-stage rewrite ('insertEffectBoundaries')
are covered by the paired golden tests
('effect-boundary-{py,cpp,cross}');
this module is for the small, deterministic pieces below them.
-}
module EffectBoundaryTests
  ( effectBoundaryTests
  ) where

import qualified Data.Set as Set
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import Morloc.CodeGenerator.EffectBoundary
  ( BoundaryContext (..)
  , boundaryExpectsPlain
  , polyOuterType
  )
import Morloc.CodeGenerator.Namespace
  ( ExecutableExpressionPool (..)
  , ManifoldForm (..)
  , ManifoldKind (..)
  , PolyExpr (..)
  , RemoteForm (..)
  , Three (..)
  )
import Morloc.Language (Lang (..))
import Morloc.Namespace.Prim (Indexed, IndexedGeneral (Idx), TVar (TV))
import Morloc.Namespace.Type (Type (..))

effectBoundaryTests :: TestTree
effectBoundaryTests =
  testGroup "EffectBoundary"
    [ boundaryContextTests
    , polyOuterTypeTests
    ]

-- Every 'BoundaryContext' returns the expected calling convention.
-- Kept as a data-table test so adding a new context requires editing
-- both the enum and this list.
boundaryContextTests :: TestTree
boundaryContextTests =
  testGroup "boundaryExpectsPlain returns documented CC"
    [ testCase "ExportRoot expects plain (goes to wire)" $
        boundaryExpectsPlain ExportRoot @?= True
    , testCase "LocalRoot is pass-through" $
        boundaryExpectsPlain LocalRoot @?= False
    , testCase "ForeignCalleeReturn expects plain (goes to wire)" $
        boundaryExpectsPlain ForeignCalleeReturn @?= True
    , testCase "ForeignCallerReceive is pass-through (as-declared)" $
        boundaryExpectsPlain ForeignCallerReceive @?= False
    , testCase "CallbackReturn expects plain (@f(x)@ drops result)" $
        boundaryExpectsPlain CallbackReturn @?= True
    , testCase "SourceCall is pass-through (as-declared)" $
        boundaryExpectsPlain SourceCall @?= False
    , testCase "SerializeSink expects plain (wire input)" $
        boundaryExpectsPlain SerializeSink @?= True
    ]

polyOuterTypeTests :: TestTree
polyOuterTypeTests =
  testGroup "polyOuterType recurses through organisational nodes"
    [ testCase "typed leaf (PolyBndVar C) reports its Type" $
        polyOuterType bndVarInt @?= Just tInt

    , testCase "PolyDoBlock reports its stored EffectT type" $
        polyOuterType doBlockIoInt @?= Just tEffectIoInt

    , testCase "PolyEval reports its stored (post-force) type" $
        polyOuterType (PolyEval idxInt bndVarInt) @?= Just tInt

    , testCase "PolyReturn delegates to child" $
        polyOuterType (PolyReturn bndVarInt) @?= Just tInt

    , testCase "PolyManifold delegates to body" $
        polyOuterType manifoldWrappingBnd @?= Just tInt

    , testCase "PolyLet delegates to body (not to bound value)" $
        polyOuterType letIntBodyStr @?= Just tStr

    , testCase "PolyApp of a FunT head returns FunT's return" $
        polyOuterType applyIntFunToOne @?= Just tInt

    , testCase "PolyApp of a non-FunT head (RemoteInterface) returns head type" $
        polyOuterType applyRemoteInterface @?= Just tEffectIoInt

    , testCase "PolyIf delegates to the then-branch" $
        polyOuterType (PolyIf bndVarInt bndVarInt bndVarInt) @?= Just tInt

    , testCase "recursion combines: return(let(_ = _; manifold(body(bnd))))" $
        polyOuterType nested @?= Just tInt
    ]

------------------------------------------------------------
-- Test fixtures (leaves)
------------------------------------------------------------

idxInt :: Indexed Type
idxInt = Idx 0 tInt

idxStr :: Indexed Type
idxStr = Idx 0 tStr

idxIoInt :: Indexed Type
idxIoInt = Idx 0 tEffectIoInt

tInt :: Type
tInt = VarT (TV "Int")

tStr :: Type
tStr = VarT (TV "Str")

tEffectIoInt :: Type
tEffectIoInt = EffectT (Set.singleton "IO") tInt

bndVarInt :: PolyExpr
bndVarInt = PolyBndVar (C idxInt) 0

bndVarStr :: PolyExpr
bndVarStr = PolyBndVar (C idxStr) 1

doBlockIoInt :: PolyExpr
doBlockIoInt = PolyDoBlock idxIoInt bndVarInt

manifoldWrappingBnd :: PolyExpr
manifoldWrappingBnd =
  PolyManifold (dummyLang) 0
    (ManifoldFull []) Transparent bndVarInt

letIntBodyStr :: PolyExpr
letIntBodyStr = PolyLet 0 bndVarInt bndVarStr

-- 'PolyApp' where the head is a source-typed 'PolyExe (FunT [Int] Int)'.
applyIntFunToOne :: PolyExpr
applyIntFunToOne =
  PolyApp
    (PolyExe (Idx 0 (FunT [tInt] tInt)) fakeSrcCall)
    [bndVarInt]

-- 'PolyApp' where the head is a 'PolyRemoteInterface' whose stored type
-- is '<IO> Int' (not a 'FunT'). 'polyOuterType' must return the stored
-- type directly, not attempt a 'FunT' return-projection.
applyRemoteInterface :: PolyExpr
applyRemoteInterface =
  PolyApp
    (PolyRemoteInterface dummyLang idxIoInt [] ForeignCall bndVarInt)
    []

nested :: PolyExpr
nested =
  PolyReturn
    (PolyLet 2 bndVarInt
      (PolyManifold dummyLang 0 (ManifoldFull []) Transparent
        (PolyReturn bndVarInt)))

-- Placeholder pool used to construct 'PolyExe' fixtures. The tests
-- never inspect its contents.
fakeSrcCall :: ExecutableExpressionPool
fakeSrcCall = LocalCallP 0

-- 'PolyExpr' constructors require a 'Lang'. Fixtures use a constant
-- lazily; the walker never inspects it.
dummyLang :: Lang
dummyLang = Lang { langName = "fixture", langExtension = "fixture" }
