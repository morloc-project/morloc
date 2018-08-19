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
) where

import qualified Text.Megaparsec.Error as PE
import Data.Void

type ThrowsError = Either MorlocError

data MorlocError
  -- | Raised when assumptions about the input RDF are broken. This should not
  -- occur for RDF that has been validated.
  = InvalidRDF String
  -- | Raised for calls to unimplemented features
  | NotImplemented String
  -- | Raised for unsupported features (such as specific languages)
  | NotSupported String
  -- | Raised by parsec on parse errors
  | SyntaxError (PE.ParseError Char Void)
  -- | Raised when someone didn't customize their error messages
  | UnknownError
  deriving(Eq)

instance Show MorlocError where
  show = morlocShow

morlocShow :: MorlocError -> String
morlocShow  UnknownError         = "UnknownError"
morlocShow (InvalidRDF msg)      = "Invalid RDF: " ++ show msg
morlocShow (NotImplemented msg)  = "Not yet implemented: " ++ show msg
morlocShow (NotSupported msg)    = "NotSupported: "        ++ show msg
morlocShow (SyntaxError    err)  = "SyntaxError: "         ++ show err
