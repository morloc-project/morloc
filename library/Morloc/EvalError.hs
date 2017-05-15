module Morloc.EvalError
(
    MorlocError(..)
  , ThrowsError
) where

import Text.Parsec

data MorlocError
  = BadApplication String
  | BadComposition String
  | SyntaxError    ParseError
  | UnknownError

instance Show MorlocError where show = morlocShow

morlocShow :: MorlocError -> String
morlocShow (BadApplication msg) = "BadApplication: " ++ msg 
morlocShow (BadComposition msg) = "BadComposition: " ++ msg
morlocShow (SyntaxError    err) = "Syntax error: "   ++ show err
morlocShow (UnknownError      ) = "Damn, you broke it good"

type ThrowsError = Either MorlocError
