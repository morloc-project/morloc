{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Lexer
Description : Lexer for Morloc script
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Lexer (
    number
  , stringLiteral
  , boolean
  , sc
  , op
  , reserved
  , name
  , tag
  , specificType
  , genericType
  , comma
  , parens
  , braces
  , brackets
  , relativeBinOp
  , logicalBinOp
  , arithmeticBinOp
) where

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Scientific as DS
import qualified Data.Text as DT

import Morloc.State

-- sc stands for space consumer
sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment  = L.skipLineComment "#"
    blockComment = empty -- no block comments

-- A lexer where space is consumed after every token (but not before)
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Parse and return a fixed string (and following space
symbol :: DT.Text -> Parser DT.Text
symbol = L.symbol sc

reservedWords :: [DT.Text]
reservedWords = ["where", "import", "from", "as", "source", "export",
                 "True", "False",
                 "and", "or", "xor", "nand", "not"]

operatorChars :: String
operatorChars = ":!$%&*+./<=>?@\\^|-~"

reserved :: DT.Text -> Parser DT.Text
reserved w = (lexeme . try) (string w <* notFollowedBy alphaNumChar)

-- TODO: should name this "identifier", that is more conventional
name :: Parser DT.Text
name = (lexeme . try) (p >>= check)
  where
    p       = fmap DT.pack $ (:) <$> letterChar <*> many alphaNumChar
    check x = if elem x reservedWords
                then error $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

op :: DT.Text -> Parser DT.Text
op o = (lexeme . try) (string o <* notFollowedBy (oneOf operatorChars))

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

number :: Parser DS.Scientific
number = L.signed empty L.scientific

comma :: Parser ()
comma = symbol "," >> return ()

-- | match a double-quoted literal string
stringLiteral :: Parser DT.Text
stringLiteral = fmap DT.pack $ char '"' >> manyTill L.charLiteral (char '"')


-- | match an optional tag that precedes some construction
tag :: Parser a -> Parser DT.Text
tag p =
  option "" (try tag')
  where
    tag' = do
      l <- name
      _ <- op ":"
      _ <- lookAhead p
      return l

-- | match a boolean written as "True" or "False"
boolean :: Parser Bool
boolean = fmap (read . DT.unpack) (reserved "True" <|> reserved "False") where

-- | match a non-generic type (alphanumeric with initial uppercase character)
specificType :: Parser DT.Text
specificType = lexeme (fmap DT.pack $ (:) <$> upperChar <*> many alphaNumChar)

-- | match a generic type (alphanumeric with initial lowercase character)
genericType :: Parser DT.Text
genericType = lexeme (fmap DT.pack $ (:) <$> lowerChar <*> many alphaNumChar)

relativeBinOp :: Parser DT.Text
relativeBinOp =
      op "=="
  <|> try (op "<=")
  <|> try (op ">=")
  <|> op "<"
  <|> op ">"
  <|> op "!="
  <?> "a numeric comparison operator"

logicalBinOp :: Parser DT.Text
logicalBinOp =
          (reserved "and")
  <|>     (reserved "or")
  <|>     (reserved "xor")
  <|> try (reserved "nand")
  <|> try (reserved "not")
  <?> "a logical operator"

arithmeticBinOp :: Parser DT.Text
arithmeticBinOp =
          (op "+")
  <|>     (op "-")
  <|>     (op "*")
  <|>     (op "^")
  <|>     (op "%")
  <|> try (op "/")
  <|>     (op "//")
  <?> "a numeric operator"


-- lexer :: Token.TokenParser ParserState
-- lexer = Token.makeTokenParser style
--   where
--   style = Lang.emptyDef {
--             Token.commentLine     = "#"
--           , Token.commentStart    = ""
--           , Token.commentEnd      = ""
--           , Token.nestedComments  = False
--           , Token.caseSensitive   = True
--           , Token.identStart      = letter <|> char '_'
--           , Token.identLetter     = alphaNum <|> oneOf "_'"
--           , Token.opStart         = Token.opLetter Lang.emptyDef
--           , Token.opLetter        = oneOf ":!$%&*+./<=>?@\\^|-~"
--           , Token.reservedOpNames = [
--                 "=", "::", ":", "+", "-", "^", "/", "//", "%", "->", ";",
--                 "(", ")", "{", "}",
--                 "<", ">", "==", "<=", ">=", "!=",
--                 "."
--               ]
--           , Token.reservedNames = [
--                 "where"
--               , "import"
--               , "from"
--               , "as"
--               , "source"
--               , "export"
--               , "True"
--               , "False"
--               , "and"
--               , "or"
--               , "xor"
--               , "nand"
--               , "not"
--             ]
--           }
--
-- parens :: Parser a -> Parser a
-- parens = Token.parens lexer
--
-- braces :: Parser a -> Parser a
-- braces = Token.braces lexer
--
-- brackets :: Parser a -> Parser a
-- brackets = Token.brackets lexer
--
-- integer    :: Parser Integer
-- float      :: Parser Double
-- whiteSpace :: Parser ()
-- op         :: String -> Parser ()
-- reserved   :: String -> Parser ()
-- comma      :: Parser String
-- name       :: Parser String
--
-- integer    = Token.integer    lexer
-- float      = Token.float      lexer
-- whiteSpace = Token.whiteSpace lexer
-- op         = Token.reservedOp lexer
-- reserved   = Token.reserved   lexer
-- comma      = Token.comma      lexer
-- name       = Token.identifier lexer
--
-- -- | match an optional tag that precedes some construction
-- tag :: Parser a -> Parser (Maybe String)
-- tag p =
--   optionMaybe (try tag')
--   where
--     tag' = do
--       l <- many1 alphaNum
--       whiteSpace
--       op ":"
--       _ <- lookAhead p
--       return l
--
-- -- | match a double-quoted literal string
-- stringLiteral :: Parser String
-- stringLiteral = do
--   _ <- char '"'
--   s <- many ((char '\\' >> char '"' ) <|> noneOf "\"")
--   _ <- char '"'
--   whiteSpace
--   return s
--
-- -- | match a boolean written as "True" or "False"
-- boolean :: Parser Bool
-- boolean = fmap read ((string "True" <|> string "False") <* whiteSpace)
--
-- -- | match a non-generic type (alphanumeric with initial uppercase character)
-- specificType :: Parser String
-- specificType = do
--   s <- satisfy DC.isUpper
--   ss <- many alphaNum
--   whiteSpace
--   return (s : ss)
--
-- -- | match a generic type (alphanumeric with initial lowercase character)
-- genericType :: Parser String
-- genericType = do
--   s <- satisfy DC.isLower
--   ss <- many alphaNum
--   whiteSpace
--   return (s : ss)
--
-- -- | match a UNIX style path ('/' delimited)
-- path :: Parser [String]
-- path = do
--   path' <- sepBy name (char '/')
--   whiteSpace
--   return path'
--
-- relativeBinOp :: Parser String
-- relativeBinOp = do
--   op' <-  (string "==")
--       <|> try (string "<=")
--       <|> try (string ">=")
--       <|> (string "<")
--       <|> (string ">")
--       <|> (string "!=")
--       <?> "a numeric comparison operator"
--   whiteSpace
--   return op'
--
-- logicalBinOp :: Parser String
-- logicalBinOp = do
--   op' <-  (string "and")
--       <|> (string "or")
--       <|> (string "xor")
--       <|> (string "nand")
--       <|> (string "not")
--       <?> "a logical operator"
--   whiteSpace
--   return op'
--
-- arithmeticBinOp :: Parser String
-- arithmeticBinOp = do
--   op' <-  (string "+")
--       <|> (string "-")
--       <|> (string "*")
--       <|> (string "^")
--       <|> (string "%")
--       <|> try (string "//")
--       <|> (string "/")
--       <?> "a numeric operator"
--   whiteSpace
--   return op'
