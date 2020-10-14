{-|
Module      : Morloc.Error
Description : Prepare error messages from MorlocError types
Copyright   : (c) Zebulun Arendsee, 2020
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
import Morloc.Pretty (prettyType)
import Morloc.Data.Doc (render)
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
errmsg (TypeError msg) = "TypeError: " <> msg
errmsg (CannotLoadModule t) = "CannotLoadModule: " <> t
errmsg (SystemCallError cmd loc msg) =
  "System call failed at (" <>
  loc <> "):\n" <> " cmd> " <> cmd <> "\n" <> " msg>\n" <> msg
errmsg (PoolBuildError _ msg) = "PoolBuildError: " <> msg
errmsg (SelfRecursiveTypeAlias v) = "SelfRecursiveTypeAlias: " <> MT.show' v
errmsg (MutuallyRecursiveTypeAlias vs) = "MutuallyRecursiveTypeAlias: " <> MT.unwords (map MT.show' vs)
errmsg (BadTypeAliasParameters (TV _ v) exp obs)
  =  "BadTypeAliasParameters: for type alias " <> MT.show' v
  <> " expected " <> MT.show' exp
  <> " parameters but found " <> MT.show' obs
errmsg (ConflictingTypeAliases t1 t2)
  = "ConflictingTypeAliases: (" <> MT.show' t1 <> ", " <> MT.show' t2 <> ")" 
errmsg NoBenefits =
  "Manifolds in this context need to be fully resolved. " <>
  "This is probably due to a bug in the code."
errmsg (CallTheMonkeys msg) =
  "There is a bug in the code, send this message to the maintainer: " <> msg
errmsg (GeneratorError msg) = "GeneratorError: " <> msg
errmsg MissingGeneralType = "MissingGeneralType"
errmsg AmbiguousGeneralType = "AmbiguousGeneralType"
errmsg (SubtypeError t1 t2) = "SubtypeError: (" <> MT.show' t1 <> ") <: (" <> MT.show' t2 <> ")"
errmsg ExistentialError = "ExistentialError"
errmsg UnsolvedExistentialTerm = "UnsolvedExistentialTerm"
errmsg BadExistentialCast = "BadExistentialCast"
errmsg (AccessError msg) = "AccessError"
errmsg NonFunctionDerive = "NonFunctionDerive"
errmsg (UnboundVariable v) = "UnboundVariable: " <> unEVar v
errmsg OccursCheckFail = "OccursCheckFail"
errmsg EmptyCut = "EmptyCut"
errmsg TypeMismatch = "TypeMismatch"
errmsg ToplevelRedefinition = "ToplevelRedefinition"
errmsg NoAnnotationFound = "NoAnnotationFound"
errmsg (OtherError msg) = "OtherError: " <> msg
-- container errors
errmsg EmptyTuple = "EmptyTuple"
errmsg TupleSingleton = "TupleSingleton"
errmsg EmptyRecord = "EmptyRecord"
-- module errors
errmsg (MultipleModuleDeclarations mv) = "MultipleModuleDeclarations: " <> MT.unwords (map unMVar mv) 
errmsg (BadImport mv ev) = "BadImport: " <> unMVar mv <> "::" <> unEVar ev
errmsg (CannotFindModule name) = "Cannot find morloc module '" <> unMVar name <> "'"
errmsg CyclicDependency = "CyclicDependency"
errmsg (SelfImport mv) = "SelfImport"
errmsg BadRealization = "BadRealization"
errmsg MissingSource = "MissingSource"
-- serialization errors
errmsg (MissingPacker place t)
  = "SerializationError: no packer found for type ("
  <> render (prettyType (unCType t)) <> ") at " <> place 
errmsg (MissingUnpacker place t)
  = "SerializationError: no unpacker found for type ("
  <> render (prettyType (unCType t)) <> ") at " <> place
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
errmsg (DagMissingKey msg) = "DagMissingKey: " <> msg
