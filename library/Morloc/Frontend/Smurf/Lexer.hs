module Smurf.Lexer
    ( Parser
    , integer
    , float
    , stringLiteral
    , boolean

    , integerP
    , floatP
    , stringLiteralP
    , booleanP

    , op
    , reserved
    , name
    , tag
    , specificType
    , genericType
    , nonSpace
    , lexeme
    , whiteSpace
    , whiteSpaceNewline
    , eol
    , indent
    , block
    , forceBlock
    , spaces
    , path
    , comma
    , parens
    , braces
    , brackets
    ) where

import Text.Megaparsec
import Text.Megaparsec.Char hiding (eol)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Char as DC
import Data.Void
import Control.Monad

import qualified Smurf.Data as D
type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whiteSpace

comments :: Parser ()
comments =  L.skipLineComment "--"
        <|> L.skipBlockCommentNested "{-" "-}"
        <?> "comment"

spaces :: Parser ()
spaces = void $ oneOf " \t\r\v"

whiteSpace :: Parser ()
whiteSpace = L.space spaces comments empty

eol :: Parser ()
eol = (char '\n' >> whiteSpaceNewline) <|> eof

indent :: Ordering -> Pos -> Parser Pos
indent ord pos =
    -- indentGuard doesn't work for me :/
    do
        many $ oneOf " \t"
        level <- L.indentLevel
        let correct = case ord of
                EQ -> pos == level
                GT -> pos < level
                LT -> pos > level
        if correct then
            return level
        else
            L.incorrectIndent ord pos level

forceBlock :: Pos -> Parser Pos
forceBlock level = eol >> indent GT level >> L.indentLevel

block :: Pos -> Parser (Maybe Pos)
block level = optional $ forceBlock level

whiteSpaceNewline :: Parser ()
whiteSpaceNewline =  skipMany
                  $  void (char '\n')
                 <|> spaces
                 <|> comments

symbol :: String -> Parser String
symbol = L.symbol whiteSpace

surround :: Parser l -> Parser r -> Pos -> Parser a -> Parser a
surround l r level v =
    do
        l
        block level
        v <- v
        block level
        r
        return v

brackets :: Pos -> Parser a -> Parser a
brackets = surround (symbol "[") (symbol "]")

parens :: Pos -> Parser a -> Parser a
parens = surround (symbol "(") (symbol ")")

braces :: Pos -> Parser a -> Parser a
braces = surround (symbol "{") (symbol "}")

op :: String -> Parser ()
op s = lexeme $ void $ string s

reserved :: String -> Parser ()
reserved s = lexeme $ void $ string s

reservedNames :: [String]
reservedNames = [
                "where"
              , "import"
              , "from"
              , "as"
              , "source"
              , "True"
              , "False"
              , "and"
              , "or"
              , "xor"
              , "nand"
              , "not"
            ]

name :: Parser String
name = lexeme $
    do
        head <- letterChar <|> char '_'
        tail <- many $ alphaNumChar <|> oneOf "_.'"
        let v = head : tail
        if v `elem` reservedNames then
            fail "used reserved word as an identifier"
        else
            return v

comma :: Parser ()
comma = lexeme $ void $ char ','

integer :: Parser Integer
integer = lexeme $
    do
        num <- some digitChar
        return $ read num

float :: Parser Double
float = lexeme L.float

tag p =
  option "" (try tag')
  where
    tag' = do
        l <- lexeme $ some alphaNumChar
        lexeme $ op ":"
        lookAhead $ lexeme p
        return l

stringLiteral :: Parser String
stringLiteral = lexeme $ do
  _ <- char '"'
  s <- many ((char '\\' >> char '"' ) <|> noneOf "\"")
  _ <- char '"'
  return s

boolean :: Parser Bool 
boolean = lexeme $ do
  s <- string "True" <|> string "False"
  return $ read s

integerP :: Parser D.Primitive
integerP = lexeme $ D.PrimitiveInt <$> integer

floatP :: Parser D.Primitive
floatP = lexeme $ D.PrimitiveReal <$> float

stringLiteralP :: Parser D.Primitive 
stringLiteralP = lexeme $ D.PrimitiveString <$> stringLiteral

booleanP :: Parser D.Primitive
booleanP = lexeme $ D.PrimitiveBool <$> boolean

-- | a legal non-generic type name
specificType :: Parser String
specificType = lexeme $ do
  s <- satisfy DC.isUpper
  ss <- many alphaNumChar
  return $ s : ss

genericType :: Parser String
genericType = lexeme $
    do
        head <- satisfy (\c -> ('a' <= c && c <= 'z') || c == '_')
        tail <- many $ alphaNumChar <|> oneOf "_.'"
        return $ head : tail

-- | match any non-space character
nonSpace :: Parser Char
nonSpace = noneOf " \n\t\r\v"

path :: Parser [String]
path = lexeme $ sepBy1 name (symbol "/")

