module Morloc.Parser (parseExpr) where

import Text.Parsec
import Text.Parsec.String (Parser)

import Text.Parsec.Expr (Operator(..), Assoc(..), buildExpressionParser)
import Text.Parsec.Token (whiteSpace)
import Control.Monad.Except (throwError)

import Morloc.Lexer
import Morloc.Syntax
import Morloc.EvalError (ThrowsError, MorlocError(..))

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


-- parse an expression, handles precedence and associativity
expr :: Parser Expr
expr = buildExpressionParser table factor
  where
  -- all expressions that evaluate to a legal element in an expression
  factor :: Parser Expr
  factor =
        try apply
    <|> try node
    <|> try num
    <|> try int
    <|>     str
  -- binary operators, listed in order of precedence
  table =
    [[binary "." Dot AssocRight]]
    where
    binary s f = Infix $ parseReservedOp s >> return (BinOp f)

num :: Parser Expr
num = fmap Float parseFloat

int :: Parser Expr
int = fmap Integer parseInteger

str :: Parser Expr
str = fmap String parseString

node :: Parser Expr
node = fmap Node parseIdentifier

apply :: Parser Expr
apply = do
  name <- composon -- NOTE: I'll allow anything to compose here,
                   -- I'll catch the errors in the evaluator 
  args <- many1 composon
  return $ Apply name args
  where
  composon =
        try node
    <|> try str
    -- num before int, else "." is parsed as COMPOSE
    <|> try num
    <|>     int
