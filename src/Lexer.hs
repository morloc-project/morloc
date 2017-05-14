module Lexer where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language
import qualified Text.Parsec.Token as T

lexer :: T.TokenParser ()
lexer = T.makeTokenParser style
  where
  style = emptyDef {
            T.commentLine     = "#"
          , T.commentStart    = ""
          , T.commentEnd      = ""
          , T.nestedComments  = False
          , T.identStart      = letter <|> char '_'
          , T.identLetter     = alphaNum <|> oneOf "_'"
          , T.opStart         = T.opLetter emptyDef
          , T.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
          , T.reservedOpNames = ["."]
          , T.reservedNames   = []
          , T.caseSensitive   = True
          }

-- Below we build all the base combinators

integer' :: Parser Integer
integer' = T.integer lexer

float' :: Parser Double
float' = T.float lexer

string' :: Parser String
string' = do
  _ <- char '"'
  s <- many ((char '\\' >> char '"' ) <|> noneOf "\"")
  _ <- char '"'
  return s

identifier' :: Parser String
identifier' = T.identifier lexer

reserved' :: String -> Parser ()
reserved' = T.reserved lexer

reservedOp' :: String -> Parser ()
reservedOp' = T.reservedOp lexer
