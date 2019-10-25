{-|
Module      : Morloc.Error
Description : Prepare error messages from MorlocError types
Copyright   : (c) Zebulun Arendsee, 2018
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
import qualified Morloc.Data.Text as MT
import qualified Morloc.Language as ML

-- TODO: fix this orphan instance
instance Show MorlocError where
  show = MT.unpack . errmsg

errmsg :: MorlocError -> MT.Text
errmsg UnknownError = "UnknownError"
errmsg (InvalidRDF msg) = "Invalid RDF: " <> msg
errmsg (NotImplemented msg) = "Not yet implemented: " <> msg
errmsg (NotSupported msg) = "NotSupported: " <> msg
errmsg (UnknownLanguage lang) =
  "'" <> lang <> "' is not recognized as a supported language"
errmsg (SyntaxError err) = "SyntaxError: " <> MT.show' err
errmsg (SerializationError t) = "SerializationError: " <> t
errmsg (TypeConflict t1 t2) = "TypeConflict: cannot cast " <> t1 <> " as " <> t2
errmsg (TypeError t) = "TypeError: " <> t
errmsg (CannotLoadModule t) = "CannotLoadModule: " <> t
errmsg (SystemCallError cmd loc msg) =
  "System call failed at (" <>
  loc <> "):\n" <> " cmd> " <> cmd <> "\n" <> " msg>\n" <> msg
errmsg (GeneratorError t) = "GeneratorError: " <> t
errmsg (DependencyError (ModuleDependency name path lang)) =
  "DependencyError: could not find module " <>
  name <> "(" <> ML.showLangName lang <> ") at " <> path
errmsg (DependencyError (ExecutableDependency name path)) =
  "DependencyError: could not find executable " <> name <> " at " <> path
errmsg (DependencyError (SourceCodeDependency moduleName path lang)) =
  "DependencyError: could not find source code '" <>
  path <>
  "' (" <>
  ML.showLangName lang <> ")" <> " imported by Morloc module " <> moduleName
errmsg (PoolBuildError _ msg) = "PoolBuildError: " <> msg
errmsg NoBenefits =
  "Manifolds in this context need to be fully resolved. " <>
  "This is probably due to a bug in the code."
errmsg (CallTheMonkeys msg) =
  "There is a bug in the code, send this message to the maintainer: " <> msg
errmsg MissingGeneralType = "MissingGeneralType"
errmsg AmbiguousGeneralType = "AmbiguousGeneralType"
errmsg (SubtypeError t1 t2) = "SubtypeError: (" <> MT.show' t1 <> ") <: (" <> MT.show' t2 <> ")"
errmsg ExistentialError = "ExistentialError"
errmsg BadExistentialCast = "BadExistentialCast"
errmsg (AccessError msg) = "AccessError"
errmsg NonFunctionDerive = "NonFunctionDerive"
errmsg (UnboundVariable ev) = "UnboundVariable"
errmsg OccursCheckFail = "OccursCheckFail"
errmsg EmptyCut = "EmptyCut"
errmsg TypeMismatch = "TypeMismatch"
errmsg (UnexpectedPattern e t) = "UnexpectedPattern"
errmsg ToplevelRedefinition = "ToplevelRedefinition"
errmsg NoAnnotationFound = "NoAnnotationFound"
errmsg (OtherError msg) = "OtherError: " <> msg
-- container errors
errmsg EmptyTuple = "EmptyTuple"
errmsg TupleSingleton = "TupleSingleton"
errmsg EmptyRecord = "EmptyRecord"
-- module errors
errmsg (MultipleModuleDeclarations mv) = "MultipleModuleDeclarations"
errmsg (BadImport mv ev) = "BadImport"
errmsg (CannotFindModule mv) = "CannotFindModule"
errmsg CyclicDependency = "CyclicDependency"
errmsg CannotImportMain = "CannotImportMain"
errmsg (SelfImport mv) = "SelfImport"
errmsg BadRealization = "BadRealization"
errmsg TooManyRealizations = "TooManyRealizations"
errmsg MissingSource = "MissingSource"
-- type extension errors
errmsg (AmbiguousPacker tv) = "AmbiguousPacker"
errmsg (AmbiguousUnpacker tv) = "AmbiguousUnpacker"
errmsg (AmbiguousCast tv1 tv2) = "AmbiguousCast"
errmsg (IncompatibleRealization mv) = "IncompatibleRealization"
errmsg MissingAbstractType = "MissingAbstractType"
errmsg ExpectedAbstractType = "ExpectedAbstractType"
errmsg CannotInferConcretePrimitiveType = "CannotInferConcretePrimitiveType"
errmsg ToplevelStatementsHaveNoLanguage = "ToplevelStatementsHaveNoLanguage"
errmsg InconsistentWithinTypeLanguage = "InconsistentWithinTypeLanguage"
errmsg CannotInferLanguageOfEmptyRecord = "CannotInferLanguageOfEmptyRecord"
errmsg ConflictingSignatures = "ConflictingSignatures: currently a given term can have only one type per language"
errmsg CompositionsMustBeGeneral = "CompositionsMustBeGeneral"
errmsg IllegalConcreteAnnotation = "IllegalConcreteAnnotation"
