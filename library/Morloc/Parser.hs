module Morloc.Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import Text.Parsec.Expr (Operator(..), Assoc(..), buildExpressionParser)
import Text.Parsec.Token (whiteSpace)
import Control.Monad.Except (throwError)

import Morloc.Lexer
import Morloc.Syntax
import Morloc.EvalError (ThrowsError, MorlocError(..))

-- parses the entire given program, returns a list of expressions on success,
-- or a error statement on failure.
-- * I assume "<stdin>" means we are reading from STDIN. But what exactly is it
--   that we are reading from STDIN? I suppose this could also be a file name?
-- * I don't know what `s` means here
parseToplevel :: String -> ThrowsError [Expr]
parseToplevel s =
  case parse (contents toplevel) "<stdin>" s of
    Left err  -> throwError $ SyntaxError err
    Right val -> return val
  where
  -- parse a list of semi-colon delimited expressions
  toplevel :: Parser [Expr]
  toplevel = many $ do
    def <- expr
    return def

-- this is needed to parse individual Expr, but it doesn't seem to be called by
-- name anywhere. I don't know how it is connected to everything else.
parseExpr :: String -> ThrowsError Expr
parseExpr s =
  case parse (contents expr) "<stdin>" s of
    Left err  -> throwError $ SyntaxError err
    Right val -> return val


-- conents is passed a parser (e.g. toplevel, as above), removes leading
-- whitespace and the trailing EOF, if present. Then it returns whatever the
-- given parser makes of the interior content. 
contents :: Parser a -> Parser a
contents p = do
  -- `lexer` here, is defined in Lexer.hs.  What does the inclusion of this
  -- lexer do for us?  I suppose this is removing space, but it seems like
  -- something else is going on ...
  whiteSpace lexer
  -- next we take everything in the input
  r <- p
  -- up until the end of file
  eof
  -- return the stuff inside
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
    binary s f assoc = Infix (reservedOp' s >> return (BinOp f)) assoc

num :: Parser Expr
num = float' >>= return . Float 

int :: Parser Expr
int = integer' >>= return . Integer

str :: Parser Expr
str = string' >>= return . String

node :: Parser Expr
node = identifier' >>= return . Node

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
