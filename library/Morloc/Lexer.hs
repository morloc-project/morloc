module Morloc.Lexer (
    lexer
  , parseInteger
  , parseFloat
  , parseString
  , parseBoolean
  , parseIdentifier
  , parseReserved
  , parseReservedOp
) where

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
          , Token.identLetter     = alphaNum <|> oneOf "_.'"
          , Token.opStart         = Token.opLetter emptyDef
          , Token.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
          , Token.reservedOpNames = ["."]
          , Token.reservedNames   = []
          , Token.caseSensitive   = True
          }

parseInteger :: Parser Integer
parseInteger = Token.integer lexer

parseFloat :: Parser Double
parseFloat = Token.float lexer

parseString :: Parser String
parseString = do
  _ <- char '"'
  s <- many ((char '\\' >> char '"' ) <|> noneOf "\"")
  _ <- char '"'
  return s

parseIdentifier :: Parser String
parseIdentifier = Token.identifier lexer

parseBoolean :: Parser Bool
parseBoolean = do
  s <- string "True" <|> string "False"
  return (read s)

parseReserved :: String -> Parser ()
parseReserved = Token.reserved lexer

parseReservedOp :: String -> Parser ()
parseReservedOp = Token.reservedOp lexer
