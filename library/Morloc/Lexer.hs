module Morloc.Lexer (
    Parser
  , integer
  , float
  , stringLiteral
  , boolean
  , whiteSpace
  , op
  , reserved
  , name
  , mdata
  , tag
  , specificType
  , genericType
  , nonSpace
  , path
  , comma
  , chop
  , space'
  , line
  , parens
  , braces
  , brackets
  , relativeBinOp
  , logicalBinOp
  , arithmeticBinOp
) where

import Text.Parsec hiding (State)
import Control.Monad.State
import qualified Data.Char as DC
import qualified Text.Parsec.Language as Lang
import qualified Text.Parsec.Token as Token

import qualified Morloc.Syntax as MS

-- For now, the passed parser state is just an counter
type ParserState = Integer

-- data ParsecT s u m a
-- where
--   s := stream type
--   u := user state type
--   m := underlying monad
--   a := return type
--
-- type Parsec s u = ParsecT s u Identity
-- type Parser = Parsec String ()
-- 
-- Here, MorlocParser deviates from Parser (defined in Text.Parsec.String) by
-- passing Integer state.
type Parser = Parsec String ParserState

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

parens = Token.parens lexer
braces = Token.braces lexer
brackets = Token.brackets lexer

integer    :: Parser Integer
float      :: Parser Double
whiteSpace :: Parser ()
op         :: String -> Parser ()
reserved   :: String -> Parser ()
name       :: Parser String
comma      :: Parser String

integer    = Token.integer lexer
float      = Token.float lexer
whiteSpace = Token.whiteSpace lexer
op         = Token.reservedOp lexer
reserved   = Token.reserved   lexer
name       = Token.identifier lexer
comma      = Token.comma      lexer

tag p =
  option "" (try tag')
  where
    tag' = do
      l <- many1 alphaNum
      whiteSpace
      op ":"
      lookAhead p
      return l

stringLiteral :: Parser String
stringLiteral = do
  _ <- char '"'
  s <- many ((char '\\' >> char '"' ) <|> noneOf "\"")
  _ <- char '"'
  whiteSpace
  return s

boolean :: Parser Bool 
boolean = fmap read ((string "True" <|> string "False") <* whiteSpace)

mdata :: Parser MS.MData
mdata = do
      try boolean'       -- True | False
  <|> try float'         -- 1.1
  <|> try integer'       -- 1
  <|> try stringLiteral' -- "yolo"
  <|> try list'          -- [ ...
  <|> try tuple'         -- ( ...
  <|> try record'        -- { ...
  <?> "literal data"
  where

    integer'       = fmap MS.MInt integer
    float'         = fmap MS.MNum float
    stringLiteral' = fmap MS.MStr stringLiteral
    boolean'       = fmap MS.MLog boolean
    list'          = fmap MS.MLst (brackets (sepBy mdata comma))
    tuple'         = fmap MS.MTup (parens tuple'')
    record'        = fmap MS.MRec (braces (sepBy1 recordEntry' comma))

    -- must have at least two elements
    tuple'' = do
      x <- mdata
      comma
      xs <- sepBy1 mdata comma
      return $ x:xs

    -- parse a tag/value pair
    recordEntry' = do
      n <- name
      op "="
      t <- mdata
      return (n, t)


-- | a legal non-generic type name
specificType :: Parser String
specificType = do
  s <- satisfy DC.isUpper
  ss <- many alphaNum
  whiteSpace
  return (s : ss)

genericType :: Parser String
genericType = Token.identifier lexer 

-- | match any non-space character
nonSpace :: Parser Char
nonSpace = noneOf " \n\t\r\v"

path :: Parser [String]
path = do
  path <- sepBy name (char '/')
  whiteSpace
  return path

-- | matches all trailing space
chop :: Parser String
chop = do
  ss <- many space'
  optional newline
  return ss

-- | non-newline space
space' :: Parser Char
space' = char ' ' <|> char '\t'

-- | a raw line with spaces
line :: Parser String
line = do
  s <- many1 space'
  l <- many (noneOf "\n")
  newline
  return $ (s ++ l)

relativeBinOp :: Parser String
relativeBinOp = do
  op <-  (string "==")
     <|> try (string "<=")
     <|> try (string ">=")
     <|> (string "<")
     <|> (string ">")
     <|> (string "!=")
     <?> "a numeric comparison operator" 
  whiteSpace
  return op 

logicalBinOp :: Parser String
logicalBinOp = do
  op <-  (string "and")
     <|> (string "or")
     <|> (string "xor")
     <|> (string "nand")
     <|> (string "not")
     <?> "a logical operator" 
  whiteSpace
  return op 

arithmeticBinOp :: Parser String
arithmeticBinOp = do
  op <-  (string "+")
     <|> (string "-")
     <|> (string "*")
     <|> (string "^")
     <|> (string "%")
     <|> try (string "//")
     <|> (string "/")
     <?> "a numeric operator" 
  whiteSpace
  return op 
