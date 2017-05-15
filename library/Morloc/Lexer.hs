module Morloc.Lexer where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language
import Text.Parsec.Token as Token

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
  style = emptyDef {
            Token.commentLine     = "#"
          , Token.commentStart    = ""
          , Token.commentEnd      = ""
          , Token.nestedComments  = False
          , Token.identStart      = letter <|> char '_'
          , Token.identLetter     = alphaNum <|> oneOf "_'"
          , Token.opStart         = Token.opLetter emptyDef
          , Token.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
          , Token.reservedOpNames = ["."]
          , Token.reservedNames   = []
          , Token.caseSensitive   = True
          }

-- Below we build all the base combinators

integer' :: Parser Integer
integer' = Token.integer lexer

float' :: Parser Double
float' = Token.float lexer

string' :: Parser String
string' = do
  _ <- char '"'
  s <- many ((char '\\' >> char '"' ) <|> noneOf "\"")
  _ <- char '"'
  return s

identifier' :: Parser String
identifier' = Token.identifier lexer

reserved' :: String -> Parser ()
reserved' = Token.reserved lexer

reservedOp' :: String -> Parser ()
reservedOp' = Token.reservedOp lexer
