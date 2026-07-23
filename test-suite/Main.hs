-- \|
-- Module      : Main
-- Description : Test suite entry point combining unit, property, and golden tests
import qualified System.Directory as SD
import Test.Tasty

import EffectBoundaryTests (effectBoundaryTests)
import GoldenMakefileTests (goldenMakefileTest)
import IrrefutablePatternLexerTests (irrefutablePatternLexerTests)
import MorlocDepsTests (morlocDepsTests)
import PatternChainTests (patternChainTests)
import PropertyTests (propertyTests)
import RefutablePatternTests (refutablePatternTests)
import SchemaHintTests (schemaHintTests)
import SizeParseTests (sizeParseTests)
import UnitTypeTests

main :: IO ()
main = do
  wd <- SD.getCurrentDirectory >>= SD.makeAbsolute
  let golden = \msg f -> goldenMakefileTest msg (wd ++ "/test-suite/golden-tests/" ++ f)
  defaultMain $
    testGroup
      "Morloc tests"
      [ unitTypeTests
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
      , withDocstringTests
      , morlocDepsTests
      , sizeParseTests
      , patternChainTests
      , irrefutablePatternLexerTests
      , refutablePatternTests
      , effectBoundaryTests
      , schemaHintTests

      -- -- These tests pass locally and when I run the same container that I
      -- -- use in github actions. Yet these tests freeze in an infinite loop
      -- -- with no STDERR output on github. I have no idea why. But for now I'm
      -- -- just going to comment them out. Rememver uncomment them on dev cycles
      -- -- so that they are tested somewhere, at least.
      -- , golden "specialization-1-c" "specialization-1-c"
      -- , golden "specialization-2-c" "specialization-2-c"
      -- , golden "specialization-1-py - numpy" "specialization-1-py"
      -- , golden "specialization-2-py - bytes/bytearray" "specialization-2-py"
      -- , golden "specialization-1-r" "specialization-1-r"

      , golden "lambda-reindex" "lambda-reindex"
      , golden "collect-formatters" "collect-formatters"
      , golden "detect-imported-handler" "detect-imported-handler"
      , golden "collect-cross-module" "collect-cross-module"
      , golden "vector-u8-cpp" "vector-u8-cpp"
      , golden "close-nontemp-error" "close-nontemp-error"
      , golden "whole-render-cpp" "whole-render-cpp"
      , golden "stdout-stream-format" "stdout-stream-format"

      , golden "vector-gradual-desugar-py" "vector-gradual-desugar-py"
      , golden "vector-gradual-desugar-c" "vector-gradual-desugar-c"

      , golden "intrinsic-catch-chained-fallible-py" "intrinsic-catch-chained-fallible-py"
      , golden "intrinsic-io-catch" "intrinsic-io-catch"
      , golden "intrinsic-catch-polymorphic-row" "intrinsic-catch-polymorphic-row"
      , golden "intrinsic-load-schema-mismatch" "intrinsic-load-schema-mismatch"
      , golden "intrinsic-throw-cpp" "intrinsic-throw-cpp"
      , golden "intrinsic-throw-py" "intrinsic-throw-py"
      , golden "intrinsic-throw-r" "intrinsic-throw-r"
      , golden "intrinsic-catch-cpp" "intrinsic-catch-cpp"
      , golden "intrinsic-catch-py" "intrinsic-catch-py"
      , golden "intrinsic-catch-r" "intrinsic-catch-r"
      , golden "intrinsic-catch-rejects-non-err" "intrinsic-catch-rejects-non-err"
      , golden "intrinsic-catch-cross-pool" "intrinsic-catch-cross-pool"
      , golden "intrinsic-catch-varwidth" "intrinsic-catch-varwidth"
      , golden "catch-cross-language-load" "catch-cross-language-load"
      , golden "wire-schema-no-hints" "wire-schema-no-hints"

      , golden "int-literal-promoted-to-real" "int-literal-promoted-to-real"
      , golden "backtick-operator-py" "backtick-operator-py"
      , golden "crosslang-io-thunk-wrap" "crosslang-io-thunk-wrap"
      , golden "effect-boundary-py" "effect-boundary-py"
      , golden "effect-boundary-cpp" "effect-boundary-cpp"
      , golden "terminal-actions-py" "terminal-actions-py"
      , golden "terminal-polymorphic-py" "terminal-polymorphic-py"
      , golden "effect-boundary-cross" "effect-boundary-cross"
      , golden "effect-interaction" "effect-interaction"
      , golden "recursive-where-capture-multi" "recursive-where-capture-multi"
      , golden "recursive-where-capture-py" "recursive-where-capture-py"
      , golden "recursive-where-capture-effect-py" "recursive-where-capture-effect-py"

      , golden "cli-stream-packet-list-receiver" "cli-stream-packet-list-receiver"
      , golden "cli-stream-packet-istream-receiver" "cli-stream-packet-istream-receiver"

      , golden "view-stdin-stream" "view-stdin-stream"
      , golden "view-guardrails" "view-guardrails"
      , golden "view-jsonl" "view-jsonl"
      , golden "view-packets" "view-packets"
      , golden "view-patterns" "view-patterns"
      , golden "nexus-file-footerless" "nexus-file-footerless"
      , golden "pattern-corpus" "pattern-corpus"
      , golden "irrefutable-patterns" "irrefutable-patterns"
      , golden "refutable-patterns" "refutable-patterns"
      , golden "refutable-guards" "refutable-guards"
      , golden "intrmap-tuple-broadcast" "intrmap-tuple-broadcast"
      , golden "ifile-data-patterns" "ifile-data-patterns"
      , golden "ostream-write-roundtrip" "ostream-write-roundtrip"
      , golden "ostream-implicit-close" "ostream-implicit-close"
      , golden "istream-multi-subpacket-roundtrip" "istream-multi-subpacket-roundtrip"
      , golden "istream-compressed-subpacket" "istream-compressed-subpacket"

      -- Write-buffer correctness tests (Part A of the buffering work).
      -- These exercise @write's coalescing behaviour, @flush boundaries,
      -- oversize-element handling, element-level split on overflow,
      -- variable-width-type round-trip, and index-section growth.
      , golden "write-buffer-coalesces" "write-buffer-coalesces"
      , golden "write-buffer-flush-explicit" "write-buffer-flush-explicit"
      , golden "write-buffer-flush-no-op-on-empty" "write-buffer-flush-no-op-on-empty"
      , golden "write-buffer-oversize-element" "write-buffer-oversize-element"
      , golden "write-buffer-multi-element-split" "write-buffer-multi-element-split"
      , golden "write-buffer-variable-width-roundtrip" "write-buffer-variable-width-roundtrip"
      , golden "write-buffer-index-grow" "write-buffer-index-grow"

      -- Shared SHM registry contracts: cross-pool handle passing,
      -- multi-writer / multi-reader semantics, forgotten-close sweep,
      -- explicit-share-only enforcement. B5 (crash recovery) and B7
      -- (cross-dispatch persistence) need additional harness work.
      , golden "shared-registry-multi-pool-write" "shared-registry-multi-pool-write"
      , golden "shared-registry-multi-pool-read" "shared-registry-multi-pool-read"
      , golden "shared-registry-cross-pool-handle-passing" "shared-registry-cross-pool-handle-passing"
      , golden "shared-registry-forgotten-close-swept" "shared-registry-forgotten-close-swept"
      , golden "shared-registry-double-open-rejected" "shared-registry-double-open-rejected"
      , golden "shared-registry-stress (wait ~10s)" "shared-registry-stress"

      , golden "cli-docstring-negatives" "cli-docstring-negatives"
      , golden "cli-docstring-shapes" "cli-docstring-shapes"
      , golden "cli-docstring-map-table" "cli-docstring-map-table"

      , golden "intrinsic-save-load-compressed" "intrinsic-save-load-compressed"
      , golden "nexus-packet-output-zstd" "nexus-packet-output-zstd"

      , golden "debug-trace" "debug-trace"
      , golden "debug-trace-dedup" "debug-trace-dedup"
      , golden "debug-trace-recursive" "debug-trace-recursive"
      , golden "debug-trace-size-cap" "debug-trace-size-cap"
      , golden "debug-trace-cross-pool" "debug-trace-cross-pool"
      , golden "debug-trace-multi-arg" "debug-trace-multi-arg"
      , golden "debug-trace-replay" "debug-trace-replay"
      , golden "debug-trace-state-reset" "debug-trace-state-reset"

      , golden "run-summary" "run-summary"
      , golden "cache-basics" "cache-basics"
      , golden "cache-concurrent" "cache-concurrent"
      , golden "cache-body-exception" "cache-body-exception"
      , golden "cache-nested" "cache-nested"
      , golden "cache-pool-hash" "cache-pool-hash"
      , golden "cache-shm-backed" "cache-shm-backed"
      , golden "cache-hash-include" "cache-hash-include"
      , golden "cache-distinct-inputs" "cache-distinct-inputs"

      , golden "bracket-accessors" "bracket-accessors"
      , golden "bracket-accessors-pure" "bracket-accessors-pure"
      , golden "bracket-accessors-ifile" "bracket-accessors-ifile"
      , golden "pattern-accessible-coherence-error" "pattern-accessible-coherence-error"
      , golden "ifile-array-cross-pool" "ifile-array-cross-pool"

      , golden "stdio-roundtrip" "stdio-roundtrip"
      , golden "stdio-single-open" "stdio-single-open"
      , golden "stdio-both-writers" "stdio-both-writers"
      , golden "stdio-filter" "stdio-filter"
      , golden "remote-streaming-consolidated" "remote-streaming-consolidated"

      , golden "native-recursive-illegal" "native-recursive-illegal"
      , golden "native-recursive-mixed" "native-recursive-mixed"
      , golden "native-recursive-record" "native-recursive-record"
      , golden "native-recursive-list" "native-recursive-list"
      , golden "native-recursive-parameterized" "native-recursive-parameterized"
      , golden "native-recursive-tuple" "native-recursive-tuple"
      , golden "recursive-type-literals" "recursive-type-literals"
      , golden "missing-concrete-record-error" "missing-concrete-record-error"

      , golden "multiprocessing-py-1" "multiprocessing-py-1"
      , -- bug regression tests from doc-agents code-tester (v0.74.0)
        -- Each test asserts correct behavior; currently FAIL until bug is fixed
        golden "bug-load-type-infer" "bug-load-type-infer"
      , golden "bug-intrinsic-schema-crash" "bug-intrinsic-schema-crash"

      , golden "thunk-basic" "thunk-basic"
      , golden "thunk-effects" "thunk-effects"
      , golden "thunk-do" "thunk-do"
      , golden "thunk-let" "thunk-let"
      , golden "thunk-interop" "thunk-interop"
      , golden "thunk-nullary-interop" "thunk-nullary-interop"
      , golden "thunk-force" "thunk-force"
      , golden "thunk-export" "thunk-export"
      , golden "thunk-choose" "thunk-choose"
      , golden "thunk-export-guard" "thunk-export-guard"
      , golden "thunk-guard-cross" "thunk-guard-cross"
      , golden "thunk-cross-force" "thunk-cross-force"
      , golden "thunk-callback-cpp" "thunk-callback-cpp"
      , golden "thunk-callback-cross-py" "thunk-callback-cross-py"
      , golden "thunk-eval-forall" "thunk-eval-forall"
      , golden "thunk-eval-hk" "thunk-eval-hk"
      , golden "two-module" "two-module"
      , golden "records-alias" "records-alias"
      , golden "infix" "infix"
      , golden "infix-import" "infix-import"
      , golden "infix-generic" "infix-generic"
      , golden "infix-polyglot" "infix-polyglot"
      , golden "infix-typeclass-import" "infix-typeclass-import"
      , golden "infix-typeclass-polyglot" "infix-typeclass-polyglot"
      , golden "infix-typeclass-simple" "infix-typeclass-simple"
      , golden "operator-sections" "operator-sections"
      , golden "eval-basic" "eval-basic"
      , golden "claude-test-1" "claude-test-1"
      , golden "claude-test-2" "claude-test-2"
      , golden "claude-test-3" "claude-test-3"
      , golden "claude-test-4" "claude-test-4"
      -- , golden "claude-test-5" "claude-test-5"
      , golden "claude-test-6" "claude-test-6"
      , golden "claude-test-7" "claude-test-7"
      , golden "claude-test-8" "claude-test-8"
      , golden "claude-test-9" "claude-test-9"
      , golden "claude-test-10" "claude-test-10"
      , golden "claude-test-11" "claude-test-11"
      , golden "claude-test-12" "claude-test-12"
      , golden "claude-test-13" "claude-test-13"
      , golden "claude-test-14" "claude-test-14"
      , golden "claude-test-15" "claude-test-15"
      , golden "claude-test-16" "claude-test-16"
      , golden "claude-test-17" "claude-test-17"
      , golden "claude-test-18" "claude-test-18"
      , golden "claude-test-19" "claude-test-19"
      , golden "claude-test-20" "claude-test-20"
      , golden "tensor-nat-labeled" "tensor-nat-labeled"
      , golden "tensor-nat-basic" "tensor-nat-basic"
      , golden "slurm-label-codegen" "slurm-label-codegen"
      , golden "let-crosslang" "let-crosslang"
      , golden "let-do-fusion" "let-do-fusion"
      , golden "let-do-fusion-effects" "let-do-fusion-effects"
      , golden "let-do-fusion-namecollide" "let-do-fusion-namecollide"
      , golden "functional-data-1" "functional-data-1"
      , golden "functional-data-2" "functional-data-2"
      , golden "functional-data-3a" "functional-data-3a"
      , golden "functional-data-3b" "functional-data-3b"
      , golden "functional-data-3c" "functional-data-3c"
      , golden "functional-data-3d-py" "functional-data-3d-py"
      , golden "functional-data-3d-c" "functional-data-3d-c"
      , golden "functional-data-3d-r" "functional-data-3d-r"
      , golden "functional-data-3e" "functional-data-3e"
      , golden "functional-data-3f" "functional-data-3f"
      , golden "functional-data-4" "functional-data-4"
      , golden "functional-data-5" "functional-data-5"
      , golden "pattern-getters" "pattern-getters"
      , golden "pattern-setters" "pattern-setters"
      , golden "pattern-setters-pure" "pattern-setters-pure"
      , golden "type-annotations-1" "type-annotations-1"
      , golden "native-morloc-1" "native-morloc-1"
      , golden "native-morloc-2" "native-morloc-2"
      , golden "native-morloc-3" "native-morloc-3"
      , golden "native-morloc-4" "native-morloc-4"
      , golden "native-morloc-5" "native-morloc-5"
      , golden "native-morloc-6" "native-morloc-6"
      , golden "native-morloc-7" "native-morloc-7"
      , golden "native-morloc-8" "native-morloc-8"
      , golden "native-morloc-9" "native-morloc-9"
      , golden "nexus-let-pure" "nexus-let-pure"
      , golden "nexus-let-lambda" "nexus-let-lambda"
      , golden "nexus-toplevel-null" "nexus-toplevel-null"
      , golden "nexus-cli-grammar" "nexus-cli-grammar"
      , golden "terminal-action-rename" "terminal-action-rename"
      , golden "cli-error-hints" "cli-error-hints"
      , golden "cli-error-diagnostics" "cli-error-diagnostics"
      , golden "nexus-file-and-view" "nexus-file-and-view"
      , golden "nexus-file-stream" "nexus-file-stream"
      , golden "demo-trimming" "demo-trimming"
      , golden "formatting" "formatting"
      , golden "record-docstrings" "record-docstrings"
      , golden "command-groups" "command-groups"
      , golden "typeclasses-1" "typeclasses-1"
      , golden "typeclasses-2" "typeclasses-2"
      , golden "typeclasses-3" "typeclasses-3"
      , golden "typeclasses-4" "typeclasses-4"
      , golden "typeclasses-5" "typeclasses-5"
      , golden "typeclasses-6" "typeclasses-6"
      , golden "typeclasses-7" "typeclasses-7"
      , golden "typeclasses-8" "typeclasses-8"
      , golden "typeclasses-9" "typeclasses-9"
      , golden "typeclass-stress" "typeclass-stress"
      , golden "alias-dedup-1" "alias-dedup-1"
      , golden "alias-concrete-bugs" "alias-concrete-bugs"
      , golden "alias-constructor-equiv" "alias-constructor-equiv"
      , golden "alias-array-monoid" "alias-array-monoid"
      , golden "alias-array-deque-specialized" "alias-array-deque-specialized"
      , golden "alias-cousins-share-instance" "alias-cousins-share-instance"
      , golden "alias-instance-rejected" "alias-instance-rejected"
      , golden "newtype-dispatch-no-instance" "newtype-dispatch-no-instance"
      , golden "newtype-dispatch-packable-positive" "newtype-dispatch-packable-positive"
      , golden "packable-map-literal" "packable-map-literal"
      , golden "alias-string-hierarchy" "alias-string-hierarchy"
      , golden "phantom-dimension" "phantom-dimension"
      , golden "poly-list-1" "poly-list-1"
      , golden "higher-kinded-types" "higher-kinded-types"
      , golden "string-encoding" "string-encoding"
      , golden "string-encoding-utf8" "string-encoding-utf8"
      , golden "uint8-cross-language" "uint8-cross-language"
      , golden "string-json-parsing" "string-json-parsing"
      , golden "string-multiline" "string-multiline"
      , golden "string-interpolation" "string-interpolation"
      , golden "string-escape" "string-escape"
      , golden "string-nul-interop" "string-nul-interop"
      , golden "string-nul-r-rejects" "string-nul-r-rejects"
      , golden "string-nul-skip-check" "string-nul-skip-check"
      , golden "string-pretty" "string-pretty"
      , golden "unicode-source" "unicode-source"
      , golden "unicode-source-cpp" "unicode-source-cpp"
      , golden "unicode-interpolation" "unicode-interpolation"
      , golden "unicode-interop" "unicode-interop"
      , golden "unicode-edge-cases" "unicode-edge-cases"
      , golden "no-shm-tmpdir-trilang" "no-shm-tmpdir-trilang"
      , golden "packer-definitions-1" "packer-definitions-1"
      , golden "packer-definitions-2" "packer-definitions-2"
      , golden "packer-definitions-3" "packer-definitions-3"
      , golden "packer-definitions-4" "packer-definitions-4"
      , golden "packer-definitions-5" "packer-definitions-5"
      , golden "import-1" "import-1"
      , golden "import-2" "import-2"
      , -- tests the bug solved involving the lambdaScope function in
        -- Generate.hs:reserialize. See that commit message.
        golden "edge-cases-1" "edge-cases-1"
      , golden "edge-cases-2" "edge-cases-2"
      , golden "type-synthesis-1" "type-synthesis-1"
      , golden "type-synthesis-2" "type-synthesis-2"
      , golden "argument-form-1-c" "argument-form-1-c"
      , golden "argument-form-1-py" "argument-form-1-py"
      , golden "argument-form-1-r" "argument-form-1-r"
      , golden "argument-form-2-c" "argument-form-2-c"
      , golden "argument-form-2-py" "argument-form-2-py"
      , golden "argument-form-2-r" "argument-form-2-r"
      , -- see github issue #7
        golden "argument-form-3-c" "argument-form-3-c"
      , golden "argument-form-3-py" "argument-form-3-py"
      , golden "argument-form-3-r" "argument-form-3-r"
      , golden "composition" "composition"
      , golden "generic-hofs-1" "generic-hofs-1"
      , golden "generic-hofs-2" "generic-hofs-2"
      , golden "eta-reduction-1" "eta-reduction-1"
      , golden "eta-reduction-2" "eta-reduction-2"
      , golden "eta-reduction-3" "eta-reduction-3"
      , golden "eta-reduction-4" "eta-reduction-4"
      , golden "eta-reduction-5" "eta-reduction-5"
      , golden "eta-reduction-6" "eta-reduction-6"
      , golden "eta-reduction-7" "eta-reduction-7"
      , golden "eta-reduction-8-py" "eta-reduction-8-py"
      , golden "eta-reduction-8-cpp" "eta-reduction-8-cpp"
      , golden "path-shadowing-c" "path-shadowing-c"
      , golden "path-shadowing-py" "path-shadowing-py"
      , golden "path-shadowing-r" "path-shadowing-r"
      , golden "local-import-root-py" "local-import-root-py"
      , golden "local-import-cousin-py" "local-import-cousin-py"
      , golden "local-import-nested-py" "local-import-nested-py"
      , golden "argument-form-4-c" "argument-form-4-c"
      , golden "argument-form-4-py" "argument-form-4-py"
      , golden "argument-form-4-r" "argument-form-4-r"
      , golden "argument-form-5-c" "argument-form-5-c"
      , golden "argument-form-5-py" "argument-form-5-py"
      , golden "argument-form-5-r" "argument-form-5-r"
      , golden "argument-form-6-c" "argument-form-6-c"
      , golden "argument-form-6-py" "argument-form-6-py"
      , golden "argument-form-6-r" "argument-form-6-r"
      , golden "argument-form-7-c" "argument-form-7-c"
      , golden "argument-form-7-py" "argument-form-7-py"
      , golden "argument-form-7-r" "argument-form-7-r"
      , golden "argument-form-8-c" "argument-form-8-c"
      , golden "argument-form-8-py" "argument-form-8-py"
      , golden "argument-form-8-r" "argument-form-8-r"
      , golden "interop-1-py" "interop-1-py"
      , golden "interop-1-r" "interop-1-r"
      , golden "interop-2" "interop-2"
      , -- 3a
        golden "interop-3a-cp" "interop-3a-cp"
      , golden "interop-3a-pr" "interop-3a-pr"
      , golden "interop-3a-rc" "interop-3a-rc"
      , golden "interop-3a-pp" "interop-3a-pp"
      , -- 3b
        golden "interop-3b-cp" "interop-3b-cp"
      , golden "interop-3b-pr" "interop-3b-pr"
      , golden "interop-3b-rc" "interop-3b-rc"
      , golden "interop-3b-pp" "interop-3b-pp"
      , -- 3c
        golden "interop-3c-cp" "interop-3c-cp"
      , golden "interop-3c-pr" "interop-3c-pr"
      , golden "interop-3c-rc" "interop-3c-rc"
      , golden "interop-3c-pp" "interop-3c-pp"
      , -- 3d
        golden "interop-3d-cp" "interop-3d-cp"
      , golden "interop-3d-pr" "interop-3d-pr"
      , golden "interop-3d-rc" "interop-3d-rc"
      , golden "interop-3d-pp" "interop-3d-pp"
      , -- 3e
        golden "interop-3e-cp" "interop-3e-cp"
      , golden "interop-3e-pr" "interop-3e-pr"
      , golden "interop-3e-rc" "interop-3e-rc"
      , golden "interop-3e-pp" "interop-3e-pp"
      , -- 3f - test serialization type bug
        golden "interop-3f" "interop-3f"
      , -- other random interop tests (I should kill them)
        golden "interop-4" "interop-4"
      , golden "interop-5" "interop-5"
      , golden "interop-6" "interop-6"
      , golden "interop-7" "interop-7"
      , golden "interop-8-r-to-c" "interop-8-r-to-c"
      , golden "interop-8-r-to-py" "interop-8-r-to-py"
      , golden "interop-8-py-to-r" "interop-8-py-to-r"
      , golden "interop-9" "interop-9"
      , golden "interop-10" "interop-10"
      , golden "interop-11" "interop-11"
      , golden "manifold-form-0" "manifold-form-0"
      , golden "manifold-form-0x" "manifold-form-0x"
      , golden "manifold-form-1" "manifold-form-1"
      , golden "manifold-form-2" "manifold-form-2"
      , golden "manifold-form-2x" "manifold-form-2x"
      , golden "manifold-form-3" "manifold-form-3"
      , golden "manifold-form-3x" "manifold-form-3x"
      , golden "manifold-form-4_c" "manifold-form-4_c"
      , golden "manifold-form-4_py" "manifold-form-4_py"
      , golden "manifold-form-4_r" "manifold-form-4_r"
      , golden "manifold-form-5_c" "manifold-form-5_c"
      , golden "manifold-form-5_py" "manifold-form-5_py"
      , golden "manifold-form-5_r" "manifold-form-5_r"
      , golden "manifold-form-6_c" "manifold-form-6_c"
      , golden "manifold-form-6_py" "manifold-form-6_py"
      , golden "manifold-form-6_r" "manifold-form-6_r"
      , -- see github issue #9
        golden "manifold-form-7_c" "manifold-form-7_c"
      , golden "manifold-form-7_py" "manifold-form-7_py"
      , golden "manifold-form-7_r" "manifold-form-7_r"
      , -- per-label logging (log: true) across all three pool languages
        golden "log-labels" "log-labels"
      , -- test records
        golden "records-primitive" "records-primitive"
      , golden "records-complex-1" "records-complex-1"
      , golden "records-complex-2" "records-complex-2"
      , golden "records-nested" "records-nested"
      , golden "records-alias" "records-alias"
      , golden "record-literal-ordering" "record-literal-ordering"
      , golden "record-nested-getter-chain" "record-nested-getter-chain"
      , golden "selection-1" "selection-1"
      , golden "selection-2" "selection-2"
      , golden "selection-3" "selection-3"
      , golden "selection-4" "selection-4"
      , -- import two instances in one languages for a function
        -- this is also a test of a function that is defind in a local file
        -- -- With the new stricter implementation, these tests no longer pass
        -- -- They can be reinstated when the morloc compiler learns to
        -- -- distinguish the functions reasonably
        -- , golden "multiple-instances-1-c" "multiple-instances-1-c"
        -- , golden "multiple-instances-1-py" "multiple-instances-1-py"
        -- , golden "multiple-instances-1-r" "multiple-instances-1-r"
        -- multiple sources and a declaration
        golden "multiple-instances-2-c" "multiple-instances-2-c"
      , golden "multiple-instances-2-py" "multiple-instances-2-py"
      , golden "multiple-instances-2-r" "multiple-instances-2-r"
      , golden "multi-lang-mempty-py" "multi-lang-mempty-py"
      , golden "bare-selector-args" "bare-selector-args"
      , golden "bare-selector-chain" "bare-selector-chain"
      , -- tests of module forms
        -- where *-sid
        --   s - number of sourced instances
        --   i - number of imported instances
        --   d - number of declared instances
        golden "module-form-00n" "module-form-00n"
      , golden "module-form-011" "module-form-011"
      , golden "module-form-01n" "module-form-01n"
      , golden "module-form-0n0" "module-form-0n0"
      , golden "module-form-0n1" "module-form-0n1"
      , golden "module-form-101" "module-form-101"
      , golden "module-form-10n" "module-form-10n"
      , golden "module-form-110" "module-form-110"
      , golden "module-form-111" "module-form-111"
      , golden "module-form-1n0" "module-form-1n0"
      , golden "module-form-n00" "module-form-n00"
      , golden "module-form-n01" "module-form-n01"
      , golden "module-form-n10" "module-form-n10"
      , golden "instance-dup-source-same-lang" "instance-dup-source-same-lang"
      , -- tests of serialization
        -- , golden "c  S" "serial-form-1-c"
        -- , golden "py S" "serial-form-1-py"
        -- , golden "r  S" "serial-form-1-r"
        golden "C serial-form-2-c" "serial-form-2-c"
      , golden "C serial-form-2-py" "serial-form-2-py"
      , golden "C serial-form-2-r" "serial-form-2-r"
      , -- , golden "c  R" "serial-form-3-c"
        -- , golden "py R" "serial-form-3-py"
        -- , golden "r  R" "serial-form-3-r"
        -- outer simple type
        golden "S(S) serial-form-4-c" "serial-form-4-c"
      , golden "S(S) serial-form-4-py" "serial-form-4-py"
      , golden "S(S) serial-form-4-r" "serial-form-4-r"
      , golden "S(C) serial-form-5-c" "serial-form-5-c"
      , golden "S(C) serial-form-5-py" "serial-form-5-py"
      , golden "S(C) serial-form-5-r" "serial-form-5-r"
      , golden "S(R) serial-form-6-c" "serial-form-6-c"
      , golden "S(R) serial-form-6-py" "serial-form-6-py"
      , golden "S(R) serial-form-6-r" "serial-form-6-r"
      , -- outer constructed type
        golden "C(S) serial-form-7-c" "serial-form-7-c"
      , golden "C(S) serial-form-7-py" "serial-form-7-py"
      , golden "C(S) serial-form-7-r" "serial-form-7-r"
      , golden "C(C) serial-form-8-c" "serial-form-8-c"
      , -- , golden "C(C) serial-form-8-py" "serial-form-8-py"
        golden "C(C) serial-form-8-r" "serial-form-8-r"
      , golden "C(R) serial-form-9-c" "serial-form-9-c"
      , golden "C(R) serial-form-9-py" "serial-form-9-py"
      , golden "C(R) serial-form-9-r" "serial-form-9-r"
      , -- outer record type
        golden "R(S) serial-form-10-c" "serial-form-10-c"
      , golden "R(S) serial-form-10-py" "serial-form-10-py"
      , golden "R(S) serial-form-10-r" "serial-form-10-r"
      , golden "R(C) serial-form-11-c" "serial-form-11-c"
      , golden "R(C) serial-form-11-py" "serial-form-11-py"
      , golden "R(C) serial-form-11-r" "serial-form-11-r"
      , -- object handling
        golden "object-1-c" "object-1-c"
      , golden "object-1-py" "object-1-py"
      , golden "object-1-r" "object-1-r"
      , -- scoping
        golden "scoping-1" "scoping-1"
      , golden "scoping-2" "scoping-2"
      , golden "scoping-3" "scoping-3"
      , golden "scoping-4" "scoping-4"
      , golden "scoping-5" "scoping-5"
      , golden "scoping-6" "scoping-6"
      , golden "scoping-7" "scoping-7"
      , golden "scoping-8" "scoping-8"
      , golden "scoping-9" "scoping-9"
      , golden "scoping-10" "scoping-10"
      , golden "scoping-11" "scoping-11"
      , golden "scoping-12" "scoping-12"
      , golden "scoping-13" "scoping-13"
      , -- type alias transitive resolution
        golden "type-alias-transitive" "type-alias-transitive"
      , -- testing packet transmission
        golden "packets-large (wait ~10s)" "packets-large"
      , golden "packets-interop (wait ~10s)" "packets-interop"
      , -- many tests of higher-order functions
        golden "hofs-1" "hofs-1"
      , -- test errors
        golden "errors (wait ~10s)" "errors"
      , golden "feature-integration-1" "feature-integration-1"
      , golden "let-expressions" "let-expressions"
      , golden "guards-py" "guards-py"
      , golden "guards-cpp" "guards-cpp"
      , golden "guards-r" "guards-r"
      , golden "guards-let-py" "guards-let-py"
      , golden "guards-let-cpp" "guards-let-cpp"
      , golden "guards-let-r" "guards-let-r"
      , golden "guards-inline-cpp" "guards-inline-cpp"
      , golden "do-block-guard-statement" "do-block-guard-statement"
      , golden "recursion-direct-py" "recursion-direct-py"
      , golden "recursion-direct-cpp" "recursion-direct-cpp"
      , golden "recursion-direct-r" "recursion-direct-r"
      , golden "recursion-mutual-py" "recursion-mutual-py"
      , golden "recursion-mutual-cpp" "recursion-mutual-cpp"
      , golden "recursion-mutual-r" "recursion-mutual-r"
      , golden "recursion-cross-py-cpp" "recursion-cross-py-cpp"
      , golden "recursion-cross-r-cpp" "recursion-cross-r-cpp"
      , golden "recursion-helper-py" "recursion-helper-py"
      , golden "recursion-helper-cpp" "recursion-helper-cpp"
      , golden "recursion-thunk-py" "recursion-thunk-py"
      , golden "recursion-thunk-helper-cpp" "recursion-thunk-helper-cpp"
      , -- optional type tests
        golden "optional-py" "optional-py"
      , golden "optional-cpp" "optional-cpp"
      , golden "optional-r" "optional-r"
      , golden "optional-json" "optional-json"
      , golden "optional-interop-cp" "optional-interop-cp"
      , golden "optional-interop-pr" "optional-interop-pr"
      , golden "optional-interop-rc" "optional-interop-rc"
      , golden "optional-records-py" "optional-records-py"
      , golden "optional-records-cpp" "optional-records-cpp"
      , golden "optional-records-r" "optional-records-r"
      , -- literal Null inside lists / tuples / records / nested optionals
        golden "optional-null-literal-py" "optional-null-literal-py"
      , -- optional coercion tests (a -> ?a)
        golden "optional-coerce-py" "optional-coerce-py"
      , golden "optional-coerce-cpp" "optional-coerce-cpp"
      , golden "optional-coerce-interop" "optional-coerce-interop"
      , golden "optional-coerce-return-py" "optional-coerce-return-py"
      , golden "optional-coerce-return-cpp" "optional-coerce-return-cpp"
      , -- eval-sugar ('!' prefix) end-to-end: '!' form must produce the
        -- same output as the equivalent explicit do-block form.
        golden "eval-sugar-py" "eval-sugar-py"
      , -- multi-label and subtyping effect tests
        golden "effect-multi-label-py" "effect-multi-label-py"
      , golden "effect-subtype-py" "effect-subtype-py"
      , golden "effect-error-cpp" "effect-error-cpp"
      , golden "effect-accumulate-py" "effect-accumulate-py"
      , golden "effect-escapable-py" "effect-escapable-py"
      , -- intrinsic tests
        golden "intrinsic-agnostic" "intrinsic-agnostic"
      , golden "intrinsic-hash" "intrinsic-hash"
      , golden "intrinsic-constants" "intrinsic-constants"
      , golden "intrinsic-show-read" "intrinsic-show-read"
      , golden "intrinsic-show-read-nexus" "intrinsic-show-read-nexus"
      , golden "intrinsic-show-ho-r" "intrinsic-show-ho-r"
      , golden "intrinsic-load-tail-position" "intrinsic-load-tail-position"
      , golden "intrinsic-load-nexus-only" "intrinsic-load-nexus-only"
      , -- parser stress test: precedence, parentheses, negatives, numeric literals, getters
        golden "parser-stress" "parser-stress"
      , -- stdout flush test: verify Python pool stdout is flushed before shutdown
        golden "stdout-flush-py" "stdout-flush-py"
      , -- namespace import tests
        golden "namespace-basic" "namespace-basic"
      , golden "namespace-selective" "namespace-selective"
      , golden "namespace-separate-impls" "namespace-separate-impls"
      , golden "namespace-disambiguation" "namespace-disambiguation"
      , golden "namespace-ns-composition" "namespace-ns-composition"
      , golden "namespace-ns-hof" "namespace-ns-hof"
      , golden "namespace-ns-let" "namespace-ns-let"
      , golden "namespace-ns-shadow" "namespace-ns-shadow"
      , golden "namespace-ns-multi" "namespace-ns-multi"
      , golden "namespace-ns-same-func-name" "namespace-ns-same-func-name"
      , golden "namespace-ns-unqualified" "namespace-ns-unqualified"
      , golden "namespace-ns-exported" "namespace-ns-exported"
      , golden "namespace-ns-reexport" "namespace-ns-reexport"
      , golden "namespace-ns-nested-getter" "namespace-ns-nested-getter"
      , golden "namespace-ns-guard" "namespace-ns-guard"
      , golden "namespace-ns-double-import" "namespace-ns-double-import"
      , -- %inline pragma tests
        golden "inline-op-py" "inline-op-py"
      , golden "inline-func-py" "inline-func-py"
      , golden "inline-typeclass-py" "inline-typeclass-py"
      , golden "inline-ho-py" "inline-ho-py"
      , golden "inline-op-ho-py" "inline-op-ho-py"
      , golden "inline-deep-py" "inline-deep-py"
      , golden "inline-mixed-py" "inline-mixed-py"
      , golden "inline-block-py" "inline-block-py"
      , golden "inline-old-style-py" "inline-old-style-py"
      , golden "inline-cross-lang" "inline-cross-lang"
      , -- bare operators in old-style source declarations
        golden "source-old-op-py" "source-old-op-py"
      , -- eval mode restriction tests
        golden "eval-restrict-source" "eval-restrict-source"
      , -- memory alignment tests (document misalignment bugs in voidstar format)
        golden "memory-optional-double-cpp" "memory-optional-double-cpp"
      , golden "memory-optional-double-py" "memory-optional-double-py"
      , golden "memory-record-pack-cpp" "memory-record-pack-cpp"
      , golden "memory-record-pack-py" "memory-record-pack-py"
      , golden "memory-interop-misalign-cp" "memory-interop-misalign-cp"
      , golden "memory-nested-misalign-cpp" "memory-nested-misalign-cpp"
      , golden "memory-nested-misalign-py" "memory-nested-misalign-py"
      , golden "memory-split-block-cpp" "memory-split-block-cpp"
      , -- Consolidated `tables-*` suite. The 25 original directories were
        --   merged into 8 by import set / coverage axis (see commit log).
        --   Each merged module exports one function per original test and
        --   is driven by a Makefile that invokes them in sequence with
        --   banner separators in obs.txt.
        --
        -- tables-typecheck:    typecheck-only typed Table algebra
        --                      (Nat+Rec arithmetic, add/drop f:Str lifting).
        -- tables-py-build:     basic Table construction + head under three
        --                      row-type shapes (kindless r, typed Rec,
        --                      bare-T polymorphic).
        -- tables-py-ops:       runtime stdlib ops through the python pool
        --                      -- sliceRows, arrange, select, reverseRows,
        --                      rbind, cbind, add/drop, selectLit/dropMany.
        -- tables-py-formats:   CSV/JSON/Parquet/Arrow-IPC input round-trip
        --                      and CSV output via --output-form csv.
        -- tables-py-schema-inference:
        --                      open-table CSV/JSON merge, CSV sniff-window
        --                      promotion, bare-T JSON schema inference.
        -- tables-r:            R-pool parity for the python ops coverage.
        -- tables-pipeline-py:  end-to-end chained integration test
        --                      (read CSV -> head -> rbind -> lookupRow).
        -- tables-mvp-pc:       cross-pool py->cpp Table identity wire path.
        golden "tables-typecheck" "tables-typecheck"
      , golden "tables-py-build" "tables-py-build"
      , golden "tables-py-ops" "tables-py-ops"
      , golden "tables-py-formats" "tables-py-formats"
      , golden "tables-py-schema-inference" "tables-py-schema-inference"
      , golden "tables-r" "tables-r"
      , golden "tables-mvp-pc" "tables-mvp-pc"
      , -- Stage 4: legacy `table Stats = Stats { ... }` migrated to typed
        --   `Table n {idx=Int, value=Real}`. Same Python/R/C++ sources,
        --   same runtime behaviour as arrow-immutable-{pr,rp,pc,cp},
        --   but using the new stage-3 Rec kind syntax. Proves users can
        --   migrate cleanly across all language combinations.
        golden "arrow-immutable-pr-typed" "arrow-immutable-pr-typed"
      , golden "arrow-immutable-rp-typed" "arrow-immutable-rp-typed"
      , golden "arrow-immutable-pc-typed" "arrow-immutable-pc-typed"
      , golden "arrow-immutable-cp-typed" "arrow-immutable-cp-typed"
      , -- Stage 4: arrow-nexus migrations. Single-pool table generators
        --   exposed through the nexus; output is row-oriented JSON via
        --   print_arrow_as_json. Typed Table form is a 1-line declaration
        --   instead of the 3-line legacy table syntax.
        golden "arrow-nexus-py-typed" "arrow-nexus-py-typed"
      , golden "arrow-nexus-cpp-typed" "arrow-nexus-cpp-typed"
      , -- Stage 4: end-to-end pipeline demo. Chains readData -> head 5 ->
        --   rbind self -> lookupRow i. Demonstrates schema preservation
        --   through multiple ops and runtime correctness for the doubled
        --   row count (lookupRow 7 of doubled-from-head-5 -> value at
        --   original index 2 = 1.0).
        golden "tables-pipeline-py" "tables-pipeline-py"
      , -- Stage 2 Str kind. A typedef parameter declared (s :: Str) marks
        --   its slot as Str-kinded; refineKinds promotes corresponding
        --   variables to StrVarU; the StrSolver unifies them with literal
        --   StrLitU values at call sites. End-to-end check that the
        --   parser-promotion-solver path is wired correctly.
        golden "str-kind-typecheck" "str-kind-typecheck"
      , -- Stage 3 Rec kind. A typedef parameter declared (r :: Rec) marks
        --   its slot as Rec-kinded; refineKinds promotes corresponding
        --   variables to RecVarU; the RecSolver leaves free row variables
        --   in place (row polymorphism). End-to-end check that the
        --   parser-promotion-solver path is wired for Rec.
        golden "rec-kind-typecheck" "rec-kind-typecheck"
      , -- Stage 10 instantiation-time constraint discharge. An explicit
        --   `=>` clause carrying primitive constraints (Disjoint, Subset)
        --   over the cross-kind algebra (Keys, ListToSet) gets renamed,
        --   queued at the call site, reduced under the call-site row
        --   substitution, and discharged. Verifies the success paths;
        --   the contradiction path is locked in by RecSolver overlap.
        golden "constraint-primitive-typecheck" "constraint-primitive-typecheck"
      , -- Stage 10 constraint propagation. A function whose body uses
        --   another function carrying a primitive constraint over a
        --   row variable cannot discharge that obligation locally
        --   (the row variable belongs to the *caller's* signature).
        --   Propagation: the caller declares the same constraint, the
        --   RecSolver unifies the inner-call's renamed row variable
        --   with the caller's renamed row variable, and the
        --   subsumption check in @dischargeConstraints@ recognises
        --   the obligation as satisfied by the caller's promise.
        golden "constraint-propagation" "constraint-propagation"
      , -- Negative path of constraint propagation. A caller invokes a
        --   function with a primitive constraint over a row variable
        --   but does NOT re-declare the constraint; the typechecker
        --   reports it as unsolved with a copy-pasteable form
        --   naming the offending function.
        golden "constraint-missing-error" "constraint-missing-error"
      , -- Error-quality regressions: locked-in diagnostics for the
        --   seven bug reports C5/C10/C14/C20/C24/C26/D4. Each test
        --   pinpoints the specific user-visible improvement (clear
        --   diagnostic text, correct caret line, no OOM, etc.) so a
        --   future refactor cannot silently regress the wording or
        --   the failure point.
        golden "self-import" "self-import"
      , golden "caret-position-typeerror" "caret-position-typeerror"
      , golden "tuple-getter-out-of-bounds" "tuple-getter-out-of-bounds"
      , golden "getter-on-list-rejected" "getter-on-list-rejected"
      , golden "source-missing-file" "source-missing-file"
      , golden "valuecheck-literal-mismatch" "valuecheck-literal-mismatch"
      , -- Stage 9.5 singleton-Str lifting. An f:Str signature label
        --   introduces `f` as a Str-kinded type-level variable. At the
        --   call site a Str literal gets lifted into the type, driving
        --   reduction of (r - f), Singleton f a, etc. End-to-end check
        --   that the parser-promotion-substitution-reduction chain
        --   delivers a flat ground Rec for the canonical "set or
        --   replace" extension pattern.
        golden "singleton-str-lifting" "singleton-str-lifting"
      , -- dense tensor tests
        golden "tensor-comprehensive-cpp" "tensor-comprehensive-cpp"
      , golden "tensor-comprehensive-cross" "tensor-comprehensive-cross"
      , golden "tensor-dimensions" "tensor-dimensions"
      , -- comprehensive vector + tensor coverage including file IO,
        -- element-type, zero-copy assertions, and permutation probes
        golden "vector-comprehensive" "vector-comprehensive"
      , golden "tensor-comprehensive" "tensor-comprehensive"
      , -- nat-parameterized type tests
        golden "nat-typecheck" "nat-typecheck"
      -- , golden "nat-dim-runtime-pure" "nat-dim-runtime-pure"
      , golden "nat-dim-runtime-remote" "nat-dim-runtime-remote"

      , -- big integer tests
        golden "numeric-literals" "numeric-literals"
      , golden "bigint-factorial-py" "bigint-factorial-py"
      , golden "bigint-overflow-cpp" "bigint-overflow-cpp"
      , golden "bigint-overflow-r" "bigint-overflow-r"
      , golden "int-overflow" "int-overflow"
      , golden "real-overflow" "real-overflow"
      , -- unary minus / negate operator
        golden "negate-unary-operator" "negate-unary-operator"
        -- an annoyingly slow one
      , golden "shm-volume-growth-py" "shm-volume-growth-py"
      ]
