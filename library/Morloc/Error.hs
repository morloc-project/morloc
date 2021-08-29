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
import Morloc.Pretty (prettyType)
import Morloc.Data.Doc (render)
import qualified Morloc.Data.Text as MT
import Text.Megaparsec.Error (errorBundlePretty)

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
errmsg (SyntaxError err) = "SyntaxError: " <> MT.pack (errorBundlePretty err)
errmsg (SerializationError t) = "SerializationError: " <> t
errmsg (CannotLoadModule t) = "CannotLoadModule: " <> t
errmsg (SystemCallError cmd loc msg) =
  "System call failed at (" <>
  loc <> "):\n" <> " cmd> " <> cmd <> "\n" <> " msg>\n" <> msg
errmsg (PoolBuildError msg) = "PoolBuildError: " <> msg
errmsg (SelfRecursiveTypeAlias v) = "SelfRecursiveTypeAlias: " <> MT.show' v
errmsg (MutuallyRecursiveTypeAlias vs) = "MutuallyRecursiveTypeAlias: " <> MT.unwords (map MT.show' vs)
errmsg (BadTypeAliasParameters (TV _ v) exp' obs)
  =  "BadTypeAliasParameters: for type alias " <> MT.show' v
  <> " expected " <> MT.show' exp'
  <> " parameters but found " <> MT.show' obs
errmsg (ConflictingTypeAliases t1 t2)
  = "ConflictingTypeAliases: (" <> MT.show' t1 <> ", " <> MT.show' t2 <> ")" 
errmsg NoBenefits =
  "Manifolds in this context need to be fully resolved. " <>
  "This is probably due to a bug in the code."
errmsg (CallTheMonkeys msg) =
  "There is a bug in the code, send this message to the maintainer: " <> msg
errmsg (GeneratorError msg) = "GeneratorError: " <> msg
errmsg (ConcreteTypeError _) = "ConcreteTypeError"
errmsg (GeneralTypeError _) = "ConcreteTypeError"
errmsg ToplevelRedefinition = "ToplevelRedefinition"
errmsg (OtherError msg) = "OtherError: " <> msg
-- TODO: this will be a common class of errors and needs an informative message
errmsg (IncompatibleGeneralType _ _) = "Incompatible general types"
-- container errors
errmsg EmptyTuple = "EmptyTuple"
errmsg TupleSingleton = "TupleSingleton"
errmsg EmptyRecord = "EmptyRecord"
-- module errors
errmsg (MultipleModuleDeclarations mv) = "MultipleModuleDeclarations: " <> MT.unwords (map unMVar mv) 
errmsg (BadImport mv (EV v)) = "BadImport: " <> unMVar mv <> "::" <> v
errmsg (CannotFindModule name) = "Cannot find morloc module '" <> unMVar name <> "'"
errmsg CyclicDependency = "CyclicDependency"
errmsg (SelfImport _) = "SelfImport"
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
errmsg (AmbiguousPacker _) = "AmbiguousPacker"
errmsg (AmbiguousUnpacker _) = "AmbiguousUnpacker"
errmsg (AmbiguousCast _ _) = "AmbiguousCast"
errmsg (IncompatibleRealization _) = "IncompatibleRealization"
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
errmsg TooManyRealizations = "TooManyRealizations"
