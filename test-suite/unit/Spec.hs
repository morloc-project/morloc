{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Spec
-- Description : Unit tests cited by the language spec
--
-- Each test is named by its spec ID, spec-<prefix>-<rule>-<n>, and is cited
-- by the `Tests:` line of that rule. One module per spec chapter.
module Spec (specTests) where

import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Spec.Common (modelCheck)
import Spec.Annotations
import Spec.Modules
import Spec.Aliases
import Spec.Kinds
import Spec.Dimensions
import Spec.TypeEquality
import Spec.Typeclasses
import Spec.Intrinsics
import Spec.Diagnostics
import Spec.Application
import Spec.Effects
import Spec.Deriving
import Spec.Newtypes
import Spec.Interfaces
import Spec.HostCalls
import Spec.Bindings
import Spec.Patterns
import Spec.Records
import Spec.Collections
import Spec.SumTypes
import Spec.BaseTypes
import Spec.Signatures
import Spec.Lexical
import Spec.Streams
import Spec.GeneratedTypes
import Spec.Polymorphism

specTests :: TestTree
specTests =
  testGroup "spec"
    [ localOption (mkTimeout 2000000) modTests
    , localOption (mkTimeout 5000000) aliasTests
    , localOption (mkTimeout 2000000) kindTests
    , localOption (mkTimeout 2000000) teqTests
    , localOption (mkTimeout 2000000) newtTests
    , localOption (mkTimeout 2000000) derivTests
    , localOption (mkTimeout 2000000) instanceAnnotationTests
    , localOption (mkTimeout 5000000) dimTests
    , localOption (mkTimeout 2000000) dim7Tests
    , localOption (mkTimeout 2000000) cls5Tests
    , localOption (mkTimeout 2000000) cls16Tests
    , gen14Tests
    , localOption (mkTimeout 2000000) cls17Tests
    , localOption (mkTimeout 2000000) cls18Tests
    , localOption (mkTimeout 5000000) cls2OwnInstanceTests
    , localOption (mkTimeout 2000000) cls20Tests
    , localOption (mkTimeout 5000000) cls23Tests
    , localOption (mkTimeout 5000000) cls5ChainTests
    , localOption (mkTimeout 5000000) poly1Tests
    , localOption (mkTimeout 5000000) poly7Tests
    , localOption (mkTimeout 5000000) poly8Tests
    , localOption (mkTimeout 5000000) checkOnceTests
    , localOption (mkTimeout 20000000) patternConformanceTests
    , localOption (mkTimeout 2000000) bindTests
    , localOption (mkTimeout 2000000) hostTests
    , localOption (mkTimeout 2000000) effTests
    , localOption (mkTimeout 2000000) appTests
    , localOption (mkTimeout 2000000) cls3Tests
    , localOption (mkTimeout 2000000) diagTests
    , localOption (mkTimeout 2000000) intrTests
    , localOption (mkTimeout 2000000) patTests
    , localOption (mkTimeout 2000000) recTests
    , localOption (mkTimeout 2000000) collTests
    , localOption (mkTimeout 2000000) strmTests
    , localOption (mkTimeout 2000000) sumTests
    , localOption (mkTimeout 2000000) lexTests
    , localOption (mkTimeout 2000000) sigTests
    , localOption (mkTimeout 2000000) baseTests
    , localOption (mkTimeout 2000000) ifaceTests
    , localOption (mkTimeout 20000000) annTests
    , modelCheck
    ]
