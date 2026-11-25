{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : FutharkTupleTests
-- Description : Unit tests for the Futhark guest's tuple support: which morloc
--               signature shapes the guest accepts and which it rejects.
--
-- These exercise the morloc-side decomposition ('decomposeSig') and the host
-- C++ type rendering ('hostTypeOfFT', 'hostEntryTypes'), which need
-- no Futhark toolchain. The accept/reject boundary (non-nested tuples of
-- scalars, Vector, and Matrix) is enforced here before any manifest comparison.
module FutharkTupleTests
  ( futharkTupleTests
  ) where

import Data.Text (Text)
import qualified Morloc.Monad as MM
import qualified System.Directory as SD
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import qualified Morloc.BaseTypes as BT
import Morloc.CodeGenerator.Guest.Futhark (FNum (..), FutharkEntry (..), FutharkType (..), TupleOps (..), decomposeSig, hostEntryTypes, hostTypeOfFT)
import Morloc.CodeGenerator.Namespace (TVar (TV), TypeU (..), pattern NatVarU)
import Morloc.Data.Doc (render)
import Morloc.Namespace.Prim (Defaultable (..))
import Morloc.Namespace.State (Config (..), MorlocError, MorlocMonad)

-- | Run a MorlocMonad-returning action and extract the Either result.
runMM :: MorlocMonad a -> IO (Either MorlocError a)
runMM action = do
  home <- SD.getHomeDirectory
  let cfg =
        Config
          { configHome = home <> "/.local/share/morloc"
          , configState = home <> "/.local/share/morloc"
          , configLibrary = home <> "/.local/share/src/morloc"
          , configTmpDir = home <> "/.morloc/tmp"
          , configBuildConfig = home <> "/.morloc/.build-config.yaml"
          , configLangOverrides = mempty
          }
  ((r, _), _) <- MM.runMorlocMonad Nothing 0 cfg defaultValue action
  return r

-- ---------------------------------------------------------------------------
-- Type builders
-- ---------------------------------------------------------------------------

-- a scalar morloc constructor, e.g. F32
sc :: Text -> TypeU
sc = VarU . TV

-- Vector n <elem>
vec :: TypeU -> TypeU
vec e = AppU (VarU (TV "Vector")) [NatVarU (TV "n"), e]

-- Matrix m n <elem>
mat :: TypeU -> TypeU
mat e = AppU (VarU (TV "Matrix")) [NatVarU (TV "m"), NatVarU (TV "n"), e]

tup :: [TypeU] -> TypeU
tup = BT.tupleU

fn :: [TypeU] -> TypeU -> TypeU
fn = FunU

-- assert a signature decomposes cleanly
accepts :: String -> TypeU -> TestTree
accepts name sig = testCase name $ do
  result <- runMM (decomposeSig sig)
  case result of
    Left e -> assertFailure ("expected acceptance, got error: " <> show e)
    Right _ -> return ()

-- assert a signature is rejected
rejects :: String -> TypeU -> TestTree
rejects name sig = testCase name $ do
  result <- runMM (decomposeSig sig)
  case result of
    Left _ -> return ()
    Right _ -> assertFailure "expected rejection, but the signature was accepted"

futharkTupleTests :: TestTree
futharkTupleTests =
  testGroup
    "Futhark guest :: tuple support"
    [ acceptTests
    , rejectTests
    , renderTests
    ]

-- ---------------------------------------------------------------------------
-- Accept: supported tuple shapes
-- ---------------------------------------------------------------------------

acceptTests :: TestTree
acceptTests =
  testGroup
    "accepts"
    [ accepts "tuple argument of scalar + vector" $
        fn [tup [sc "F32", vec (sc "F32")]] (sc "F32")
    , accepts "heterogeneous tuple return (I32, F32, Vector)" $
        fn [sc "I32", vec (sc "F32")] (tup [sc "I32", sc "F32", vec (sc "F32")])
    , accepts "tuple argument carrying a matrix field" $
        fn [tup [mat (sc "F32"), sc "F32"]] (sc "F32")
    , accepts "eight-field tuple is allowed" $
        fn [tup (replicate 8 (sc "F32"))] (sc "F32")
    ]

-- ---------------------------------------------------------------------------
-- Reject: unsupported shapes
-- ---------------------------------------------------------------------------

rejectTests :: TestTree
rejectTests =
  testGroup
    "rejects"
    [ rejects "nested tuple argument" $
        fn [tup [tup [sc "F32", sc "F32"], sc "F32"]] (sc "F32")
    , rejects "nested tuple return" $
        fn [sc "F32"] (tup [tup [sc "F32", sc "F32"], sc "F32"])
    , rejects "array of tuples" $
        fn [vec (tup [sc "F32", sc "F32"])] (sc "F32")
    , rejects "nine-field tuple exceeds the arity cap" $
        fn [tup (replicate 9 (sc "F32"))] (sc "F32")
    , rejects "tuple of a non-numeric element" $
        fn [tup [sc "Str", sc "F32"]] (sc "F32")
    ]

-- ---------------------------------------------------------------------------
-- Render: the C++ host type for a tuple signature
-- ---------------------------------------------------------------------------

renderTests :: TestTree
renderTests =
  testGroup
    "host type rendering"
    [ testCase "tuple arg renders to std::tuple, return to scalar" $ do
        Right (args, ret) <- runMM (decomposeSig (fn [tup [sc "F32", vec (sc "F32")]] (sc "F32")))
        map (render . hostTypeOfFT) args @?= ["std::tuple<float, std::vector<float>>"]
        render (hostTypeOfFT ret) @?= "float"
    , testCase "tuple return with a matrix field renders nested" $ do
        Right (_, ret) <- runMM (decomposeSig (fn [sc "F32"] (tup [sc "F32", mat (sc "F32")])))
        render (hostTypeOfFT ret) @?= "std::tuple<float, mlc::Tensor2<float>>"
    , testCase "an entry's host types come from its manifest types" $ do
        let ops = TupleOps "struct t *" "new_t" "free_t"
            entry = FutharkEntry "e" "entry_e"
              [FTTuple ops [("p0", FNum "f32" 0), ("p1", FNum "f32" 1)]] (FTNum (FNum "f64" 2))
            (args, ret) = hostEntryTypes entry
        map render args @?= ["std::tuple<float, std::vector<float>>"]
        render ret @?= "mlc::Tensor2<double>"
    , testCase "a list of scalars is not a scalar" $ do
        r <- runMM (decomposeSig (fn [AppU (VarU (TV "List")) [sc "F32"]] (sc "F32")))
        either (const (return ())) (const (assertFailure "[F32] was accepted")) r
    ]
