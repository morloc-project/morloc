import Test.Tasty
import qualified System.Directory as SD

import PropertyTests (propertyTests)
import UnitTypeTests
import GoldenMakefileTests (goldenMakefileTest)

main = do
  wd <- SD.getCurrentDirectory >>= SD.makeAbsolute
  let golden = \msg f -> goldenMakefileTest msg (wd ++ "/test-suite/golden-tests/" ++ f)
  defaultMain $
    testGroup
      "Morloc tests"
      [ packerTests
      , unitTypeTests
      , typeOrderTests
      , typeAliasTests
      , propertyTests
      , jsontype2jsonTests
      , recordAccessTests

      , golden "import-1" "import-1"

      , golden "argument-form-1-c" "argument-form-1-c"
      , golden "argument-form-1-py" "argument-form-1-py"
      , golden "argument-form-1-r" "argument-form-1-r"

      , golden "argument-form-2-c" "argument-form-2-c"
      , golden "argument-form-2-py" "argument-form-2-py"
      , golden "argument-form-2-r" "argument-form-2-r"

      -- see github issue #7
      , golden "argument-form-3-c" "argument-form-3-c"
      , golden "argument-form-3-py" "argument-form-3-py"
      , golden "argument-form-3-r" "argument-form-3-r"

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

      , golden "defaults-1-py" "defaults-1-py"

      , golden "interop-1-py" "interop-1-py"
      , golden "interop-1-r" "interop-1-r"
      , golden "interop-2" "interop-2"

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

      -- see github issue #9
      , golden "manifold-form-7_c" "manifold-form-7_c"
      , golden "manifold-form-7_py" "manifold-form-7_py"
      , golden "manifold-form-7_r" "manifold-form-7_r"

      , golden "records-1-py" "records-1-py"
      , golden "records-1-r" "records-1-r"
      -- see github issue #8
      , golden "records-1-c" "records-1-c"

      , golden "selection-1" "selection-1"
      , golden "selection-2" "selection-2"
      , golden "selection-3" "selection-3"
      , golden "selection-4" "selection-4"

      -- import two instances in one languages for a function
      -- this is also a test of a function that is defind in a local file
      , golden "multiple-instances-1-c" "multiple-instances-1-c"
      , golden "multiple-instances-1-py" "multiple-instances-1-py"
      , golden "multiple-instances-1-r" "multiple-instances-1-r"
      -- multiple sources and a declaration
      , golden "multiple-instances-2-c" "multiple-instances-2-c"
      , golden "multiple-instances-2-py" "multiple-instances-2-py"
      , golden "multiple-instances-2-r" "multiple-instances-2-r"
      -- tests of module forms
      -- where *-sid
      --   s - number of sourced instances
      --   i - number of imported instances
      --   d - number of declared instances
      , golden "module-form-00n" "module-form-00n"
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

      -- tests of serialization
      -- , golden "c  S" "serial-form-1-c"
      -- , golden "py S" "serial-form-1-py"
      -- , golden "r  S" "serial-form-1-r"
      , golden "C serial-form-2-c" "serial-form-2-c"
      , golden "C serial-form-2-py" "serial-form-2-py"
      , golden "C serial-form-2-r" "serial-form-2-r"
      -- , golden "c  R" "serial-form-3-c"
      -- , golden "py R" "serial-form-3-py"
      -- , golden "r  R" "serial-form-3-r"
      -- outer simple type
      , golden "S(S) serial-form-4-c" "serial-form-4-c"
      , golden "S(S) serial-form-4-py" "serial-form-4-py"
      , golden "S(S) serial-form-4-r" "serial-form-4-r"
      , golden "S(C) serial-form-5-c" "serial-form-5-c"
      , golden "S(C) serial-form-5-py" "serial-form-5-py"
      , golden "S(C) serial-form-5-r" "serial-form-5-r"
      , golden "S(R) serial-form-6-c" "serial-form-6-c"
      , golden "S(R) serial-form-6-py" "serial-form-6-py"
      , golden "S(R) serial-form-6-r" "serial-form-6-r"
      -- outer constructed type
      , golden "C(S) serial-form-7-c" "serial-form-7-c"
      , golden "C(S) serial-form-7-py" "serial-form-7-py"
      , golden "C(S) serial-form-7-r" "serial-form-7-r"
      , golden "C(C) serial-form-8-c" "serial-form-8-c"
      , golden "C(C) serial-form-8-rs" "serial-form-8-rs"
      , golden "C(C) serial-form-8-py" "serial-form-8-py"
      , golden "C(C) serial-form-8-r" "serial-form-8-r"
      , golden "C(R) serial-form-9-c" "serial-form-9-c"
      , golden "C(R) serial-form-9-rs" "serial-form-9-rs"
      , golden "C(R) serial-form-9-py" "serial-form-9-py"
      , golden "C(R) serial-form-9-r" "serial-form-9-r"
      -- outer record type
      , golden "R(S) serial-form-10-c" "serial-form-10-c"
      , golden "R(S) serial-form-10-rs" "serial-form-10-rs"
      , golden "R(S) serial-form-10-py" "serial-form-10-py"
      , golden "R(S) serial-form-10-r" "serial-form-10-r"
      , golden "R(C) serial-form-11-c" "serial-form-11-c"
      , golden "R(C) serial-form-11-rs" "serial-form-11-rs"
      , golden "R(C) serial-form-11-py" "serial-form-11-py"
      , golden "R(C) serial-form-11-r" "serial-form-11-r"
      , golden "R(R) serial-form-12-c" "serial-form-12-c"
      , golden "R(R) serial-form-12-rs" "serial-form-12-rs"
      , golden "R(R) serial-form-12-py" "serial-form-12-py"
      , golden "R(R) serial-form-12-r" "serial-form-12-r"
      -- table handling
      , golden "C++ table default" "table-1-c"
      , golden "Rust table default" "table-1-rs"
      , golden "py3 table default" "table-1-py"
      , golden "R table default" "table-1-r"
      , golden "C++ table object" "table-2-c"
      , golden "Rust table object" "table-2-rs"
      , golden "py3 table object" "table-2-py"
      , golden "R table object" "table-2-r"
      -- object handling
      , golden "C++ object handling" "object-1-c"
      , golden "Rust object handling" "object-1-rs"
      , golden "py3 object handling" "object-1-py"
      , golden "R object handling" "object-1-r"
      -- record access
      , golden "record-access-gen" "record-access-gen"
      , golden "record-access-c" "record-access-c"
      , golden "record-access-rs" "record-access-rs"
      , golden "record-access-py" "record-access-py"
      , golden "record-access-r" "record-access-r"
      -- type identities
      , golden "type-identities-c" "type-identities-c"
      , golden "type-identities-rs" "type-identities-rs"
      -- where-scope
      , golden "where-scoping" "where-scoping"

      -- , golden "argument-form-1-rs" "argument-form-1-rs"
      -- , golden "argument-form-2-rs" "argument-form-2-rs"
      -- , golden "argument-form-3-rs" "argument-form-3-rs"
      -- , golden "argument-form-4-rs" "argument-form-4-rs"
      -- , golden "argument-form-6-rs" "argument-form-6-rs"
      -- , golden "argument-form-7-rs" "argument-form-7-rs"
      -- , golden "argument-form-8-rs" "argument-form-8-rs"
      -- , golden "argument-form-5-rs" "argument-form-5-rs"
      -- , golden "records-1-rs" "records-1-rs"
      -- , golden "multiple-instances-1-rs" "multiple-instances-1-rs"
      -- , golden "multiple-instances-2-rs" "multiple-instances-2-rs"
      -- , golden "C serial-form-2-rs" "serial-form-2-rs"
      -- , golden "S(S) serial-form-4-rs" "serial-form-4-rs"
      -- , golden "S(C) serial-form-5-rs" "serial-form-5-rs"
      -- , golden "S(R) serial-form-6-rs" "serial-form-6-rs"
      -- , golden "C(S) serial-form-7-rs" "serial-form-7-rs"
      -- , golden "C(C) serial-form-8-rs" "serial-form-8-rs"
      -- , golden "C(R) serial-form-9-rs" "serial-form-9-rs"
      -- , golden "R(S) serial-form-10-rs" "serial-form-10-rs"
      -- , golden "R(C) serial-form-11-rs" "serial-form-11-rs"
      -- , golden "R(R) serial-form-12-rs" "serial-form-12-rs"
      -- , golden "Rust table default" "table-1-rs"
      -- , golden "Rust table object" "table-2-rs"
      -- , golden "Rust object handling" "object-1-rs"
      -- , golden "record-access-rs" "record-access-rs"
      -- , golden "type-identities-rs" "type-identities-rs"

      ]
