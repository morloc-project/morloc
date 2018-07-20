module Morloc.Error
(
    MorlocError(..)
  , ThrowsError
) where

import Text.Parsec (ParseError)
import Data.List (intercalate)

-- TODO add declaration errors
data MorlocError
  = BadApplication   String
  | BadComposition   String
  | SyntaxError      ParseError
  | BadArray         String
  | NotImplemented   String
  | NotSupported     String
  | CouldNotFind     String
  | NameConflict     String [String]
  | TypeError        String
  | TypeMismatch     String String String -- name, obs type, exp type
  | VeryBadBug       String
  | InvalidRDF       String
  | UnknownError
  deriving(Eq)


instance Show MorlocError
  where
    show = morlocShow

morlocShow :: MorlocError -> String
morlocShow (BadApplication msg)  = "BadApplication: "      ++ show msg 
morlocShow (BadComposition msg)  = "BadComposition: "      ++ show msg
morlocShow (SyntaxError    err)  = "SyntaxError: "         ++ show err
morlocShow (BadArray       err)  = "BadArray: "            ++ show err
morlocShow (NotImplemented msg)  = "Not yet implemented: " ++ show msg
morlocShow (NotSupported msg)    = "NotSupported: "        ++ show msg
morlocShow (CouldNotFind x)      = "Could not find " ++ q x ++ ", missing import?" 
morlocShow (NameConflict x pkgs) = "NameConflict: "  ++ q x ++ " is imported from " ++ l pkgs
morlocShow (TypeError s)         = "TypeError: " ++ s
morlocShow (TypeMismatch n o e)  = "Type mismatch in '" ++ n ++ "':\n" 
                                   ++ "  Expected type: '" ++ e ++ "'\n"
                                   ++ "  Observed type: '" ++ o ++ "'"
morlocShow (VeryBadBug msg)      = "BUG IN MORLOC CORE: " ++ show msg
morlocShow (InvalidRDF msg)      = "Invalid RDF: " ++ show msg
morlocShow  UnknownError         = "Damn, you broke it good"

-- show a list as: [<item>, <item>, ...]
l :: [String] -> String
l xs = "[" ++ (intercalate ", ") xs ++ "]"

-- quote a string
q :: Show a => a -> String
q x = "'" ++ show x ++ "'"

type ThrowsError = Either MorlocError
