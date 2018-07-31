{-|
Module      : Morloc.Lexer
Description : Lexer for Morloc script
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Lexer (
    integer
  , float
  , stringLiteral
  , boolean
  , whiteSpace
  , op
  , reserved
  , name
  , tag
  , specificType
  , genericType
  , path
  , comma
  , parens
  , braces
  , brackets
  , relativeBinOp
  , logicalBinOp
  , arithmeticBinOp
) where

import Text.Parsec hiding (State)
import qualified Data.Char as DC
import qualified Text.Parsec.Language as Lang
import qualified Text.Parsec.Token as Token

import Morloc.State

lexer :: Token.TokenParser ParserState
lexer = Token.makeTokenParser style
  where
  style = Lang.emptyDef {
            Token.commentLine     = "#"
          , Token.commentStart    = ""
          , Token.commentEnd      = ""
          , Token.nestedComments  = False
          , Token.caseSensitive   = True
          , Token.identStart      = letter <|> char '_'
          , Token.identLetter     = alphaNum <|> oneOf "_'"
          , Token.opStart         = Token.opLetter Lang.emptyDef
          , Token.opLetter        = oneOf ":!$%&*+./<=>?@\\^|-~"
          , Token.reservedOpNames = [
                "=", "::", ":", "+", "-", "^", "/", "//", "%", "->", ";",
                "(", ")", "{", "}",
                "<", ">", "==", "<=", ">=", "!=",
                "."
              ]
          , Token.reservedNames = [
                "where"
              , "import"
              , "from"
              , "as"
              , "source"
              , "export"
              , "True"
              , "False"
              , "and"
              , "or"
              , "xor"
              , "nand"
              , "not"
            ]
          }

parens :: Parser a -> Parser a
parens = Token.parens lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

integer    :: Parser Integer
float      :: Parser Double
whiteSpace :: Parser ()
op         :: String -> Parser ()
reserved   :: String -> Parser ()
comma      :: Parser String
name       :: Parser String

integer    = Token.integer    lexer
float      = Token.float      lexer
whiteSpace = Token.whiteSpace lexer
op         = Token.reservedOp lexer
reserved   = Token.reserved   lexer
comma      = Token.comma      lexer
name       = Token.identifier lexer

-- | match an optional tag that precedes some construction
tag :: Parser a -> Parser (Maybe String)
tag p =
  optionMaybe (try tag')
  where
    tag' = do
      l <- many1 alphaNum
      whiteSpace
      op ":"
      _ <- lookAhead p
      return l

-- | match a double-quoted literal string
stringLiteral :: Parser String
stringLiteral = do
  _ <- char '"'
  s <- many ((char '\\' >> char '"' ) <|> noneOf "\"")
  _ <- char '"'
  whiteSpace
  return s

-- | match a boolean written as "True" or "False"
boolean :: Parser Bool 
boolean = fmap read ((string "True" <|> string "False") <* whiteSpace)

-- | match a non-generic type (alphanumeric with initial uppercase character)
specificType :: Parser String
specificType = do
  s <- satisfy DC.isUpper
  ss <- many alphaNum
  whiteSpace
  return (s : ss)

-- | match a generic type (alphanumeric with initial lowercase character)
genericType :: Parser String
genericType = do
  s <- satisfy DC.isLower
  ss <- many alphaNum
  whiteSpace
  return (s : ss)

-- | match a UNIX style path ('/' delimited)
path :: Parser [String]
path = do
  path' <- sepBy name (char '/')
  whiteSpace
  return path'

relativeBinOp :: Parser String
relativeBinOp = do
  op' <-  (string "==")
      <|> try (string "<=")
      <|> try (string ">=")
      <|> (string "<")
      <|> (string ">")
      <|> (string "!=")
      <?> "a numeric comparison operator" 
  whiteSpace
  return op' 

logicalBinOp :: Parser String
logicalBinOp = do
  op' <-  (string "and")
      <|> (string "or")
      <|> (string "xor")
      <|> (string "nand")
      <|> (string "not")
      <?> "a logical operator" 
  whiteSpace
  return op'

arithmeticBinOp :: Parser String
arithmeticBinOp = do
  op' <-  (string "+")
      <|> (string "-")
      <|> (string "*")
      <|> (string "^")
      <|> (string "%")
      <|> try (string "//")
      <|> (string "/")
      <?> "a numeric operator" 
  whiteSpace
  return op' 
