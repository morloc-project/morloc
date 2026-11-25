-- \|
-- Module      : Main
-- Description : Test suite entry point combining unit, property, and golden tests
--
-- Golden tests are not listed here. Every test directory under
-- test-suite/golden is discovered and run; see GoldenMakefileTests.
--
-- MORLOC_TEST_SELECT=unit or =golden runs only that part (default: both).
import qualified System.Directory as SD
import System.Environment (lookupEnv)
import Test.Tasty

import AbiTests (abiTests)
import BuildParamsTests (buildParamsTests)
import EffectBoundaryTests (effectBoundaryTests)
import EnvSpecTests (envSpecTests)
import FutharkTupleTests (futharkTupleTests)
import GoldenMakefileTests (discoverGoldenTests)
import GoldenShard (Shard (..), lookupShard)
import GoldenShardTests (goldenShardTests)
import InstanceResolveTests (instanceResolveTests)
import IrrefutablePatternLexerTests (irrefutablePatternLexerTests)
import LangSupportTests (langSupportTests)
import MorlocDepsTests (morlocDepsTests)
import PackageTests (packageTests)
import NativeDepsTests (nativeDepsTests)
import PatternChainTests (patternChainTests)
import ArgsBoundTests (argsBoundTests)
import IndexAllocationTests (indexAllocationTests)
import LangRegistryCostTests (langRegistryCostTests)
import PropertyTests (propertyTests)
import UsefulnessTests (usefulnessTests)
import RecSolverTests (recSolverTests)
import NatDecideTests (natDecideTests)
import RefutablePatternTests (refutablePatternTests)
import LockFileTests (lockFileTests)
import RustPoolBuildTests (rustPoolBuildTests)
import SchemaHintTests (schemaHintTests)
import SizeParseTests (sizeParseTests)
import Spec (specTests)
import VariantMergeTests (variantMergeTests)
import GenDefsTests (genDefsTests)
import NominalTests (nominalTests)
import ValueArgsTests (valueArgsTests, kindOfTests)
import ManifoldNameTests (manifoldNameTests)
import SystemConfigTests (systemConfigTests)
import Types
import LastReadTests (lastReadTests)
import OwnershipCheckTests (ownershipCheckTests)
import VersionConstraintTests (versionConstraintTests)

unitTests :: [TestTree]
unitTests =
  [ specTests
  , instanceResolveTests
  , unitTypeTests
  , lastReadTests
  , ownershipCheckTests
  , recSolverTests
  , abiTests
  , buildParamsTests
  , envSpecTests
  , futharkTupleTests
  , unitValuecheckTests
  , typeOrderTests
  , typeAliasTests
  , numericLiteralAliasTests
  , pendingNumLitTests
  , propertyTests
  , usefulnessTests
  , whereTests
  , orderInvarianceTests
  , signatureContractTests
  , constraintContractTests
  , definitionLadderTests
  , whitespaceTests
  , infixOperatorTests
  , recordLiteralOrderTests
  , recordIdentityTests
  , typeIdentityTests
  , aliasExpansionTests
  , accessorInWhereTests
  , anonymousRecordOrderTests
  , solvedKindCheckTests
  , substituteTVarTests
  , subtypeTests
  , complexityRegressionTests
  , definitionArityTests
  , effectSubtypeTests
  , effectSynthesisTests
  , effectErrorTests
  , evalSugarTests
  , effectEscapabilityTests
  , effectPartialApplicationTests
  , polymorphicEffectRowTests
  , effectCoverageMessageTests
  , namespaceErrorTests
  , typeclassTests
  , natErrorTests
  , natArithTests
  , natLabelTests
  , natKindPromotionTests
  , natDimTests
  , gradualDesugarTests
  , typedefKindVarTests
  , letBindingTests
  , irrefutablePatternTests
  , aliasConstructorTests
  , aliasTransparencyTests
  , newtypeTests
  , literalDispatchTests
  , recursiveRecordTests
  , bidirectionalAppCheckTests
  , postArgPropagationTests
  , tuplePatternLambdaTests
  , withDocstringTests
  , parseDocstringTests
  , epilogueDocstringTests
  , streamIntrinsicTests
  , patternSelectorTests
  , sumTypeTests
  , variantTests
  , evalSandboxTests
  , typeRenderParenTests
  , sourceNameTests
  , tupleNameTests
  , suspensionLawTests
  , datafileTests
  , morlocDepsTests
  , versionConstraintTests
  , sizeParseTests
  , variantMergeTests
  , genDefsTests
  , nominalTests
  , valueArgsTests
  , kindOfTests
  , natDecideTests
  , manifoldNameTests
  , patternChainTests
  , argsBoundTests
  , indexAllocationTests
  , langRegistryCostTests
  , irrefutablePatternLexerTests
  , refutablePatternTests
  , rustPoolBuildTests
  , lockFileTests
  , packageTests
  , nativeDepsTests
  , effectBoundaryTests
  , schemaHintTests
  , systemConfigTests
  , langSupportTests
  , goldenShardTests
  ]

main :: IO ()
main = do
  wd <- SD.getCurrentDirectory >>= SD.makeAbsolute
  shard <- lookupShard
  select <- lookupEnv "MORLOC_TEST_SELECT"
  (wantUnits, wantGoldens) <- case select of
    Nothing -> return (True, True)
    Just "" -> return (True, True)
    Just "all" -> return (True, True)
    Just "unit" -> return (True, False)
    Just "golden" -> return (False, True)
    Just other -> ioError . userError $
      "MORLOC_TEST_SELECT='" ++ other ++ "' is not one of unit, golden, all"
  goldens <-
    if wantGoldens
      then discoverGoldenTests shard (wd ++ "/test-suite/tools/shims") (wd ++ "/test-suite/golden")
      else return []
  -- Unit tests are cheap; one shard runs them.
  let units = case shard of
        Just (Shard i _) | i /= 1 -> []
        _ | wantUnits -> unitTests
        _ -> []
  defaultMain $ testGroup "Morloc tests" (units ++ goldens)
