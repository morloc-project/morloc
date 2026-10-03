-- \|
-- Module      : Main
-- Description : Test suite entry point combining unit, property, and golden tests
--
-- Golden tests are not listed here. Every directory under
-- test-suite/golden-tests is discovered and run; see GoldenMakefileTests.
import qualified System.Directory as SD
import Test.Tasty

import AbiTests (abiTests)
import BuildParamsTests (buildParamsTests)
import EffectBoundaryTests (effectBoundaryTests)
import EnvSpecTests (envSpecTests)
import FutharkTupleTests (futharkTupleTests)
import GoldenMakefileTests (discoverGoldenTests)
import GoldenShard (Shard (..), lookupShard)
import GoldenShardTests (goldenShardTests)
import IrrefutablePatternLexerTests (irrefutablePatternLexerTests)
import LangSupportTests (langSupportTests)
import MorlocDepsTests (morlocDepsTests)
import PatternChainTests (patternChainTests)
import PropertyTests (propertyTests)
import RecSolverTests (recSolverTests)
import RefutablePatternTests (refutablePatternTests)
import LockFileTests (lockFileTests)
import RustPoolBuildTests (rustPoolBuildTests)
import SchemaHintTests (schemaHintTests)
import SizeParseTests (sizeParseTests)
import VariantMergeTests (variantMergeTests)
import SystemConfigTests (systemConfigTests)
import UnitTypeTests
import VersionConstraintTests (versionConstraintTests)

unitTests :: [TestTree]
unitTests =
  [ unitTypeTests
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
  , whereTests
  , orderInvarianceTests
  , signatureContractTests
  , constraintContractTests
  , definitionLadderTests
  , whitespaceTests
  , infixOperatorTests
  , recordLiteralOrderTests
  , recordIdentityTests
  , aliasExpansionTests
  , accessorInWhereTests
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
  , suspensionLawTests
  , morlocDepsTests
  , versionConstraintTests
  , sizeParseTests
  , variantMergeTests
  , patternChainTests
  , irrefutablePatternLexerTests
  , refutablePatternTests
  , rustPoolBuildTests
  , lockFileTests
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
  goldens <- discoverGoldenTests shard (wd ++ "/test-suite/golden-tests")
  -- Unit tests are cheap; one shard runs them.
  let units = case shard of
        Just (Shard i _) | i /= 1 -> []
        _ -> unitTests
  defaultMain $ testGroup "Morloc tests" (units ++ goldens)
