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

import qualified Morloc.Data.Text as MT

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Scientific as DS
import qualified Data.Set as DS

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
symbol :: MT.Text -> Parser MT.Text
symbol = L.symbol sc

reservedWords :: [MT.Text]
reservedWords = ["where", "import", "from", "as", "source", "export",
                 "True", "False",
                 "and", "or", "xor", "nand", "not"]

operatorChars :: String
operatorChars = ":!$%&*+./<=>?@\\^|-~"

reserved :: MT.Text -> Parser MT.Text
reserved w = (lexeme . try) (string w <* notFollowedBy alphaNumChar)

-- TODO: should name this "identifier", that is more conventional
name :: Parser MT.Text
name = (lexeme . try) (p >>= check)
  where
    p       = fmap MT.pack $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
    check x = if elem x reservedWords
                then failure Nothing DS.empty -- TODO: error message
                else return x

op :: MT.Text -> Parser MT.Text
op o = (lexeme . try) (string o <* notFollowedBy (oneOf operatorChars))

parens :: Parser a -> Parser a
parens p = lexeme $ between (symbol "(") (symbol ")") p

brackets :: Parser a -> Parser a
brackets p = lexeme $ between (symbol "[") (symbol "]") p

braces :: Parser a -> Parser a
braces p = lexeme $ between (symbol "{") (symbol "}") p

number :: Parser DS.Scientific
number = lexeme $ L.signed sc L.scientific -- `empty` because no space is allowed

comma :: Parser ()
comma = symbol "," >> return ()

-- | match a double-quoted literal string
stringLiteral :: Parser MT.Text
stringLiteral = lexeme $ fmap MT.pack $ char '"' >> manyTill L.charLiteral (char '"')


-- | match an optional tag that precedes some construction
tag :: Parser a -> Parser (Maybe MT.Text)
tag p =
  optional (try tag')
  where
    tag' = do
      l <- name
      _ <- op ":"
      _ <- lookAhead p
      return l

-- | match a boolean written as "True" or "False"
boolean :: Parser Bool
boolean = fmap MT.read' (reserved "True" <|> reserved "False") where

-- | match a non-generic type (alphanumeric with initial uppercase character)
specificType :: Parser MT.Text
specificType = lexeme (fmap MT.pack $ (:) <$> upperChar <*> many alphaNumChar)

-- | match a generic type (alphanumeric with initial lowercase character)
genericType :: Parser MT.Text
genericType = lexeme (fmap MT.pack $ (:) <$> lowerChar <*> many alphaNumChar)

relativeBinOp :: Parser MT.Text
relativeBinOp =
      op "=="
  <|> try (op "<=")
  <|> try (op ">=")
  <|> op "<"
  <|> op ">"
  <|> op "!="
  <?> "a numeric comparison operator"

logicalBinOp :: Parser MT.Text
logicalBinOp =
          (reserved "and")
  <|>     (reserved "or")
  <|>     (reserved "xor")
  <|> try (reserved "nand")
  <|> try (reserved "not")
  <?> "a logical operator"

arithmeticBinOp :: Parser MT.Text
arithmeticBinOp =
          (op "+")
  <|>     (op "-")
  <|>     (op "*")
  <|>     (op "^")
  <|>     (op "%")
  <|> try (op "/")
  <|>     (op "//")
  <?> "a numeric operator"
