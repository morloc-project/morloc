module Morloc.Parser (parseExpr) where

import Text.Parsec
import qualified Text.Parsec.Combinator as C
import Text.Parsec.String (Parser)

import Text.Parsec.Expr (Operator(..), Assoc(..), buildExpressionParser)
import Text.Parsec.Token (whiteSpace)
import Control.Monad.Except (throwError)

import Morloc.Lexer
import Morloc.Syntax
import Morloc.Data
import Morloc.EvalError (ThrowsError, MorlocError(..))

-- | Parse a string of Morloc text into an expression that may be passed to
-- Morloc.Evaluator.eval. Catch lexical syntax errors.
parseExpr :: String -> ThrowsError Expr
parseExpr s =
  case parse (contents expr) "<stdin>" s of
    Left err  -> throwError $ SyntaxError err
    Right val -> return val
  where
  contents :: Parser a -> Parser a
  contents p = do
    whiteSpace lexer
    r <- p
    eof
    return r


node :: Parser Expr
node = fmap ( Value . MFunc ) parseIdentifier

num :: Parser Expr
num = fmap ( Value . MNum ) parseFloat

int :: Parser Expr
int = fmap ( Value . MInt ) parseInteger

str :: Parser Expr
str = fmap ( Value . MString ) parseString

bool :: Parser Expr
bool = fmap ( Value . MBool ) parseBoolean

-- Parsers for heterogenous arrays
-- The evaluator will trim the possibilities. Currently only homogenous arrays
-- of primitives are allows.
array :: Parser Expr
array = do
  _ <- char '['
  _ <- whiteSpace lexer
  m <- C.sepBy element (char ',')
  _ <- whiteSpace lexer
  _ <- char ']'
  return $ Array m

element :: Parser Expr
element = do
  _ <- whiteSpace lexer
  p <-  try bool
    <|> try num
    <|> try int
    <|> try str
  _ <- whiteSpace lexer
  return p

factor :: Parser Expr
factor =
        try array
    <|> try bool
    <|> try num -- num before int, else "." parsed as COMPOSE
    <|> try int
    <|> try str
    <|> try node

-- parse an expression, handles precedence and associativity
expr :: Parser Expr
expr = buildExpressionParser table (try apply <|> factor)
  where
  -- binary operators, listed in order of precedence
  table =
    [[binary "." Dot AssocRight]]
    where
    binary s f = Infix $ parseReservedOp s >> return (BinOp f)

apply :: Parser Expr
apply = do
  name <- factor -- NOTE: I'll allow anything to compose here,
                 -- I'll catch the errors in the evaluator 
  args <- many1 factor 
  return $ Apply name args
