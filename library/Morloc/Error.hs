{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Error
Description : Error handling
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental

The MorlocError type is used to handle all forms of errors across the entire
program. New entries can be added to describe new types of error.
-}

module Morloc.Error () where

import Morloc.Types
import Morloc.Operators
import qualified Morloc.Data.Text as MT

-- TODO: fix this orphan instance
instance Show MorlocError where
  show = MT.unpack . errmsg

errmsg :: MorlocError -> MT.Text
errmsg UnknownError = "UnknownError"
errmsg (InvalidRDF msg) = "Invalid RDF: " <> MT.show' msg
errmsg (NotImplemented msg) = "Not yet implemented: " <> MT.show' msg
errmsg (NotSupported msg) = "NotSupported: " <> MT.show' msg
errmsg (SyntaxError err) = "SyntaxError: " <> MT.show' err
errmsg (SerializationError t) = "SerializationError: " <> t
errmsg (TypeConflict t1 t2) = "TypeConflict: cannot cast " <> t1 <> " as " <> t2
errmsg (TypeError t) = "TypeError: " <> t
errmsg (SparqlFail t) = "SparqlFail: " <> t
errmsg (CannotLoadModule t) = "CannotLoadModule: " <> t
errmsg (SystemCallError cmd loc msg) =  "System call failed at (" <> loc <> "):\n"
                                     <> " cmd> " <> cmd <> "\n"
                                     <> " msg>\n" <> msg
errmsg (GeneratorError t) = "GeneratorError: " <> t
errmsg TrulyWeird = "Find the code monkeys 'cause you broke it good"
