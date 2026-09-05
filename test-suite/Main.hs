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
import IrrefutablePatternLexerTests (irrefutablePatternLexerTests)
import LangSupportTests (langSupportTests)
import MorlocDepsTests (morlocDepsTests)
import PatternChainTests (patternChainTests)
import PropertyTests (propertyTests)
import RefutablePatternTests (refutablePatternTests)
import SchemaHintTests (schemaHintTests)
import SizeParseTests (sizeParseTests)
import SystemConfigTests (systemConfigTests)
import UnitTypeTests
import VersionConstraintTests (versionConstraintTests)

unitTests :: [TestTree]
unitTests =
  [ unitTypeTests
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
  , whitespaceTests
  , infixOperatorTests
  , recordLiteralOrderTests
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
  , catchRowInheritTests
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
  , evalSandboxTests
  , morlocDepsTests
  , versionConstraintTests
  , sizeParseTests
  , patternChainTests
  , irrefutablePatternLexerTests
  , refutablePatternTests
  , effectBoundaryTests
  , schemaHintTests
  , systemConfigTests
  , langSupportTests
  ]

main :: IO ()
main = do
  wd <- SD.getCurrentDirectory >>= SD.makeAbsolute
  goldens <- discoverGoldenTests (wd ++ "/test-suite/golden-tests")
  defaultMain $ testGroup "Morloc tests" (unitTests ++ goldens)
