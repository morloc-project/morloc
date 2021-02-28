module Smurf.Parser (smurf) where

import Control.Monad.State
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Maybe

import Smurf.Data
import qualified Smurf.Lexer as Tok

smurf :: Tok.Parser [Top]
smurf = Tok.whiteSpaceNewline >>
    do
        result <- many top
        Tok.lexeme eof
        return result

top :: Tok.Parser Top
top = do
    Tok.indent EQ (mkPos 1)
    s <- statement <?> "import, export, signature, or declaration"
    Tok.eol <?> "newline"
    return $ TopStatement s

statement :: Tok.Parser Statement
statement =  importy
         <|> exporty
         <|> try signature
         <|> declaration

importy :: Tok.Parser Statement
importy =
    do
        level <- L.indentLevel
        Tok.reserved "import"
        mod <- Tok.path
        imports <- optional $ try $ Tok.block level >> Tok.parens level (sepBy Tok.name (Tok.comma >> Tok.whiteSpaceNewline))
        if isNothing imports then do
            alias <- optional $ Tok.reserved "as" >> Tok.name
            return $ ImportQualified mod alias
        else
            let imports' = fromMaybe [] imports in
            return $ Import mod imports'

exporty :: Tok.Parser Statement
exporty =
    do
        Tok.reserved "export"
        Export <$> Tok.name

declaration :: Tok.Parser Statement
declaration =
    do
        level <- L.indentLevel
        varname <- Tok.name
        bndvars <- many Tok.name
        Tok.block level
        Tok.op "="
        Tok.block level
        Declaration varname bndvars <$> expression

expression :: Tok.Parser Expression
expression =
      -- currently this just handles "."
  term'
  <?> "an expression"
  where
    term' = do
        level <- L.indentLevel
        try application
          <|> try (Tok.parens level expression)
          <|> try primitiveExpr
          <|> ExprName <$> Tok.name

primitiveExpr :: Tok.Parser Expression
primitiveExpr = ExprPrimitive <$> primitive

primitive :: Tok.Parser Primitive
primitive =
      try Tok.floatP -- this must go before integer
  <|> try Tok.integerP
  <|> try Tok.booleanP
  <|> try Tok.stringLiteralP
  <?> "a primitive"

application :: Tok.Parser Expression
application = do
    level <- L.indentLevel
    function <- Tok.name
    arguments <- many $ try $ Tok.block level >> (
            Tok.parens level application
        <|> (ExprName <$> Tok.name)
        <|> primitiveExpr
        )
    return $ ExprApplication function arguments

-- | function :: [input] -> output constraints
signature :: Tok.Parser Statement
signature =
    do
        level <- L.indentLevel
        function <- Tok.name
        Tok.block level
        Tok.op "::"
        head <- mtype
        tail <- many $ try $ do
            Tok.block level
            Tok.op "->"
            mtype
        let output = if null tail then
                Nothing
            else
                Just $ last tail
        let inputs = if null tail then
                [head]
            else
                head : init tail
        Signature function inputs output <$> wherey level

wherey :: Pos -> Tok.Parser (Maybe [Expression])
wherey level = optional $ try $ do
    level' <- Tok.block level
    let level'' = fromMaybe level level'
    Tok.reserved "where"
    Tok.block level''
    sepBy1 expression $ try $ Tok.forceBlock level''

mtype :: Tok.Parser MType
mtype =
      list'       -- [a]
  <|> paren'      -- () | (a) | (a,b,...)
  <|> try record' -- Foo {a :: t, ...}
  <|> specific'   -- Foo
  <|> generic'    -- foo
  <?> "type"
  where
    -- [ <type> ]
    list' :: Tok.Parser MType
    list' = do
      level <- L.indentLevel
      l <- Tok.tag (char '[')
      s <- Tok.brackets level mtype
      return $ MList s l

    -- ( <type>, <type>, ... )
    paren' :: Tok.Parser MType
    paren' = do
      level <- L.indentLevel
      l <- Tok.tag (char '(')
      s <- Tok.parens level (sepBy mtype Tok.comma)
      return $ case s of
        []  -> MEmpty
        [x] -> x
        xs  -> MTuple xs l

    -- <name> <type> <type> ...
    specific' :: Tok.Parser MType
    specific' = do
      l <- Tok.tag Tok.specificType
      s <- Tok.specificType
      ss <- many mtype 
      return $ MSpecific s ss l

    -- <name> <type> <type> ...
    generic' :: Tok.Parser MType
    generic' = do
      -- TODO - the genericType should automatically fail on keyword conflict
      notFollowedBy (Tok.reserved "where")
      l <- Tok.tag Tok.genericType
      s <- Tok.genericType
      ss <- many mtype 
      return $ MGeneric s ss l

    -- <name> { <name> :: <type>, <name> :: <type>, ... }
    record' :: Tok.Parser MType
    record' = do
      level <- L.indentLevel
      l <- Tok.tag Tok.specificType
      n <- Tok.specificType
      xs <- Tok.braces level (sepBy1 recordEntry' Tok.comma)
      return $ MRecord n xs l

    -- (<name> = <type>)
    recordEntry' :: Tok.Parser (Name, MType)
    recordEntry' = do
      n <- Tok.name
      Tok.op "::"
      t <- mtype
      return (n, t)

