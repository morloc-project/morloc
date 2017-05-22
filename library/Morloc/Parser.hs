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
num  = fmap ( Value . MNum    ) parseFloat

int :: Parser Expr
int  = fmap ( Value . MInt    ) parseInteger

str :: Parser Expr
str  = fmap ( Value . MString ) parseString

bool :: Parser Expr
bool = fmap ( Value . MBool   ) parseBoolean

-- parsers for homogenous arrays
parseArray :: Parser a -> ([a] -> MData) -> Parser Expr
parseArray p f = do
  _ <- char '['
  m <- C.sepBy p (char ',')
  _ <- char ']'
  return $ Value $ f m

narray :: Parser Expr
narray = parseArray parseFloat    MNums

iarray :: Parser Expr
iarray = parseArray parseInteger  MInts

sarray :: Parser Expr
sarray = parseArray parseString   MStrings

barray :: Parser Expr
barray = parseArray parseBoolean  MBools

factor :: Parser Expr
factor = 
        try narray
    <|> try iarray
    <|> try sarray
    <|> try barray
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
