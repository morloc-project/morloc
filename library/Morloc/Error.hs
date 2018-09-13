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
    MorlocError(..)
  , ThrowsError
  , error'
) where

import Morloc.Operators
import qualified Morloc.Data.Text as MT

import qualified Text.Megaparsec.Error as PE
import Data.Void

type ThrowsError = Either MorlocError

data MorlocError
  -- | Raised when assumptions about the input RDF are broken. This should not
  -- occur for RDF that has been validated.
  = InvalidRDF MT.Text
  -- | Raised for calls to unimplemented features
  | NotImplemented MT.Text
  -- | Raised for unsupported features (such as specific languages)
  | NotSupported MT.Text
  -- | Raised by parsec on parse errors
  | SyntaxError (PE.ParseError Char Void)
  -- | Raised when someone didn't customize their error messages
  | UnknownError
  -- | Raised when parent and child types conflict
  | TypeConflict MT.Text MT.Text
  deriving(Eq)

instance Show MorlocError where
  show = MT.unpack . errmsg

errmsg :: MorlocError -> MT.Text
errmsg  UnknownError         = "UnknownError"
errmsg (InvalidRDF msg)      = "Invalid RDF: " <> MT.show' msg
errmsg (NotImplemented msg)  = "Not yet implemented: " <> MT.show' msg
errmsg (NotSupported msg)    = "NotSupported: " <> MT.show' msg
errmsg (SyntaxError err)     = "SyntaxError: " <> MT.show' err
errmsg (TypeConflict t1 t2)  = 
  "TypeConflict: cannot cast " <> t1 <> " as " <> t2 <> "\n"

error' :: MT.Text -> a
error' x = error $ MT.unpack x
