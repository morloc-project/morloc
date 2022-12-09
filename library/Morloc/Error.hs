{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Error
Description : Prepare error messages from MorlocError types
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental

MorlocError is the type used within morloc to store data related to any errors
that are encountered. Data constructors in the MorlocError type may associates
data with the error. This data may be an arbitrary message or any other type.
The @errmsg@ function in this module defines how these errors will be printed
to the user.
-}
module Morloc.Error () where

import Morloc.Namespace
import Morloc.Data.Doc
import Morloc.Pretty ()
import qualified Morloc.Data.Text as MT
import Text.Megaparsec.Error (errorBundlePretty)

instance Show MorlocError where
  show = MT.unpack . render . pretty

instance Show TypeError where
  show = MT.unpack . render . pretty

instance Pretty MorlocError where
  pretty (IndexedError i e) = "At index" <+> pretty i <> ":" <+> pretty e
  pretty (NotImplemented msg) = "Not yet implemented: " <> pretty msg
  pretty (NotSupported msg) = "NotSupported: " <> pretty msg
  pretty (UnknownLanguage lang) =
    "'" <> pretty lang <> "' is not recognized as a supported language"
  pretty (SyntaxError err') = "SyntaxError: " <> pretty (errorBundlePretty err')
  pretty (SerializationError t) = "SerializationError: " <> pretty t
  pretty (CannotLoadModule t) = "CannotLoadModule: " <> pretty t
  pretty (SystemCallError cmd loc msg) =
    "System call failed at (" <>
    pretty loc <> "):\n" <> " cmd> " <> pretty cmd <> "\n" <> " msg>\n" <> pretty msg
  pretty (PoolBuildError msg) = "PoolBuildError: " <> pretty msg
  pretty (SelfRecursiveTypeAlias v) = "SelfRecursiveTypeAlias: " <> pretty v
  pretty (MutuallyRecursiveTypeAlias vs) = "MutuallyRecursiveTypeAlias: " <> tupled (map pretty vs)
  pretty (BadTypeAliasParameters (TV _ v) exp' obs)
    =  "BadTypeAliasParameters: for type alias " <> pretty v
    <> " expected " <> pretty exp'
    <> " parameters but found " <> pretty obs
  pretty (ConflictingTypeAliases t1 t2)
    = "ConflictingTypeAliases: (" <> pretty t1 <> ", " <> pretty t2 <> ")" 
  pretty (CallTheMonkeys msg) =
    "There is a bug in the code, send this message to the maintainer: " <> pretty msg
  pretty (GeneratorError msg) = "GeneratorError: " <> pretty msg
  pretty (ConcreteTypeError err') = "Concrete type error: " <> pretty err'
  pretty (GeneralTypeError err') = "General type error: " <> pretty err'
  pretty ToplevelRedefinition = "ToplevelRedefinition"
  pretty (OtherError msg) = "OtherError: " <> pretty msg
  -- TODO: this will be a common class of errors and needs an informative message
  pretty (IncompatibleGeneralType _ _) = "Incompatible general types"
  -- container errors
  pretty EmptyTuple = "EmptyTuple"
  pretty TupleSingleton = "TupleSingleton"
  pretty EmptyRecord = "EmptyRecord"
  -- module errors
  pretty (MultipleModuleDeclarations mv) = "MultipleModuleDeclarations: " <> tupled (map pretty mv) 
  pretty (NestedModule name') = "Nested modules are currently illegal: " <> pretty name'
  pretty (BadImport mv ev) = "BadImport: " <> pretty mv <> "::" <> pretty ev
  pretty (BadExport mv ev) = "BadExport: " <> pretty mv <> "::" <> pretty ev
  pretty (CannotFindModule name') = "Cannot find morloc module '" <> pretty name' <> "'"
  pretty CyclicDependency = "CyclicDependency"
  pretty (SelfImport _) = "SelfImport"
  pretty BadRealization = "BadRealization"
  pretty MissingSource = "MissingSource"
  -- serialization errors
  pretty (MissingPacker place t)
    = "SerializationError: no packer found for type ("
    <> pretty t <> ") at " <> pretty place 
  pretty (MissingUnpacker place t)
    = "SerializationError: no unpacker found for type ("
    <> pretty t <> ") at " <> pretty place
  pretty (CyclicPacker _) = "CyclicPacker"
  -- type extension errors
  pretty (AmbiguousPacker _) = "AmbiguousPacker"
  pretty (AmbiguousUnpacker _) = "AmbiguousUnpacker"
  pretty (AmbiguousCast _ _) = "AmbiguousCast"
  pretty (IllegalPacker t) = "IllegalPacker:" <+> pretty t
  pretty (IncompatibleRealization _) = "IncompatibleRealization"
  pretty MissingAbstractType = "MissingAbstractType"
  pretty ExpectedAbstractType = "ExpectedAbstractType"
  pretty CannotInferConcretePrimitiveType = "CannotInferConcretePrimitiveType"
  pretty ToplevelStatementsHaveNoLanguage = "ToplevelStatementsHaveNoLanguage"
  pretty InconsistentWithinTypeLanguage = "InconsistentWithinTypeLanguage"
  pretty CannotInferLanguageOfEmptyRecord = "CannotInferLanguageOfEmptyRecord"
  pretty ConflictingSignatures = "ConflictingSignatures: currently a given term can have only one type per language"
  pretty CompositionsMustBeGeneral = "CompositionsMustBeGeneral"
  pretty IllegalConcreteAnnotation = "IllegalConcreteAnnotation"
  pretty (DagMissingKey msg) = "DagMissingKey: " <> pretty msg
  pretty TooManyRealizations = "TooManyRealizations"

instance Pretty TypeError where
  pretty (SubtypeError t1 t2 msg)
    = "SubtypeError:" <+> pretty msg <> "\n  "
    <> "(" <> pretty t1 <+> "<:" <+> pretty t2 <> ")"
  pretty (InstantiationError t1 t2 msg)
    = "InstantiationError:" <+> pretty msg <> "\n  "
    <> "(" <> pretty t1 <+> "<:=" <+> pretty t2 <> ")"
  pretty (EmptyCut gi) = "EmptyCut:" <+> pretty gi
  pretty OccursCheckFail {} = "OccursCheckFail"
  pretty (Mismatch t1 t2 msg)
    = "Mismatch"
    <+> tupled ["t1=" <> pretty t1, "t2=" <> pretty t2]
    <+> pretty msg
  pretty (UnboundVariable v) = "UnboundVariable:" <+> pretty v
  pretty (KeyError k t) = "KeyError:" <+> dquotes (pretty k) <+> "not found in record" <+> pretty t
  pretty (MissingConcreteSignature src) = "MissingConcreteSignature for" <+> pretty src 
  pretty (MissingGeneralSignature src) = "MissingGeneralSignature for" <+> pretty src
  pretty ApplicationOfNonFunction = "ApplicationOfNonFunction"
  pretty TooManyArguments = "TooManyArguments"
  pretty (MissingFeature msg) = "MissingFeature: " <> pretty msg
  pretty EmptyExpression = "EmptyExpression"
