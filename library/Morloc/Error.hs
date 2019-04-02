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

module Morloc.Error
(
    error'
  , errmsg
) where

import Morloc.Types
import Morloc.Operators
import qualified Morloc.Data.Text as MT

instance Show MorlocError where
  show = MT.unpack . errmsg

errmsg :: MorlocError -> MT.Text
errmsg UnknownError         = "UnknownError"
errmsg (InvalidRDF msg)     = "Invalid RDF: " <> MT.show' msg
errmsg (NotImplemented msg) = "Not yet implemented: " <> MT.show' msg
errmsg (NotSupported msg)   = "NotSupported: " <> MT.show' msg
errmsg (SyntaxError err)    = "SyntaxError: " <> MT.show' err
errmsg (TypeConflict t1 t2) = "TypeConflict: cannot cast " <> t1 <> " as " <> t2
errmsg (SparqlFail t)       = "SparqlFail: " <> t
errmsg (CannotLoadModule t) = "CannotLoadModule: " <> t
errmsg (SystemCallError t)  = "System call failed:\n" <> t

error' :: MT.Text -> a
error' x = error $ MT.unpack x
