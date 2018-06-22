module Morloc.Parser (morlocScript) where

import Text.Parsec hiding (State)
import qualified Text.Parsec.Expr as TPE
import Text.Parsec.String (Parser)
import Control.Monad.State
import Control.Monad.Except (throwError)

import Morloc.Error
import Morloc.Syntax
import qualified Morloc.Lexer as Tok

-- | Parse a string of Morloc text into an AST. Catch lexical syntax errors.
morlocScript :: String -> ThrowsError [Top]
morlocScript s =
  case parse contents "<stdin>" s of
    Left err  -> throwError $ SyntaxError err
    Right val -> return val

contents :: Parser [Top]
contents = do
  Tok.whiteSpace
  result <- many top
  eof
  return result

top :: Parser Top
top =
      try topSource 
  <|> try topStatement
  <|> try topImport
  <?> "Top. Maybe you are missing a semicolon?"

topSource :: Parser Top
topSource = do
  s <- source
  optional (Tok.op ";")
  return $ TopSource s

topStatement :: Parser Top
topStatement = do
  s <- statement
  return $ TopStatement s

topImport :: Parser Top
topImport = do
  i <-  try restrictedImport
    <|> try simpleImport
  optional (Tok.op ";")
  return $ TopImport i

statement :: Parser Statement
statement = do
  s <-  try signature
    <|> try declaration
  Tok.op ";"
  return $ s

simpleImport :: Parser Import
simpleImport = do
  Tok.reserved "import" 
  path <- Tok.path
  qual <- optionMaybe (Tok.op "as" >> Tok.name)
  return $ Import path qual Nothing

restrictedImport :: Parser Import
restrictedImport = do
  Tok.reserved "from"
  path <- Tok.path
  Tok.reserved "import"
  -- TODO: I am also importing ontologies, how should that be handled?
  -- TODO: at very least, I am also importing types
  functions <- Tok.parens (sepBy1 Tok.name Tok.comma)
  return $ Import path Nothing (Just functions)

-- | parses a 'source' header, returning the language
source :: Parser Source
source = do
  Tok.reserved "source"
  -- get the language of the imported source
  lang <- Tok.stringLiteral
  -- get the path to the srouce file, if Nothing, then assume "vanilla"
  path <- optionMaybe (Tok.reserved "from" >> Tok.path)
  -- get the function imports with with optional aliases
  fs <- Tok.parens (sepBy importAs' Tok.comma)
  return $ case path of
    (Just p) -> SourceFile lang p fs
    Nothing  -> SourceLang lang fs
  where
    importAs' :: Parser (String, Maybe String)
    importAs' = do
      -- quoting the function names allows them to have more arbitrary values,
      -- perhaps even including expressions that return functions (though this
      -- is probably bad practice).
      func <- Tok.stringLiteral
      -- the alias is especially important when the native function name is not
      -- legal Morloc syntax, for example an R function with a '.' in the name.
      alias <- optionMaybe (Tok.reserved "as" >> Tok.name)
      return $ (func, alias)

declaration :: Parser Statement
declaration = do
  varname <- Tok.name
  bndvars <- many Tok.name
  Tok.op "="
  value <- expression
  return $ Declaration varname bndvars value 

expression :: Parser Expression
expression =
      -- currently this just handles "."
      try (TPE.buildExpressionParser functionTable term')
  <|> term'
  <?> "an expression"
  where
    term' =
          try (Tok.parens expression)
      <|> try application
      <|> try primitiveExpr'

    primitiveExpr' :: Parser Expression
    primitiveExpr' = do
      x <- try Tok.mdata 
      return $ ExprData x

application :: Parser Expression
application = do
  tag' <- Tok.tag Tok.name
  function <- Tok.name
  arguments <- sepBy term' Tok.whiteSpace
  return $ ExprApplication function tag' arguments
  where
    term' = do 
          try (Tok.parens expression)
      <|> try var'
      <|> try dat'

    var' = do
      x <- Tok.name
      return $ ExprVariable x

    dat' = do
      x <- try Tok.mdata
      return $ ExprData x

-- | function :: [input] -> output constraints 
signature :: Parser Statement
signature = do
  function <- Tok.name
  Tok.op "::"
  inputs <- sepBy1 mtype Tok.comma
  output <- optionMaybe (
      Tok.op "->" >>
      mtype
    )
  constraints <- option [] (
      Tok.reserved "where" >>
      Tok.parens (sepBy1 booleanExpr Tok.comma)
    )
  return $ Signature function inputs output constraints

mtype :: Parser MType
mtype =
      list'       -- [a]
  <|> paren'      -- () | (a) | (a,b,...)
  <|> try record' -- Foo {a :: t, ...}
  <|> specific'   -- Foo
  <|> generic'    -- foo
  <?> "type"
  where
    -- [ <type> ]
    list' :: Parser MType
    list' = do
      l <- Tok.tag (char '[')
      s <- Tok.brackets mtype
      return $ MList s l

    -- ( <type>, <type>, ... )
    paren' :: Parser MType
    paren' = do
      l <- Tok.tag (char '(')
      s <- Tok.parens (sepBy mtype (Tok.comma)) 
      return $ case s of
        []  -> MEmpty
        [x] -> x
        xs  -> MTuple xs l

    -- <name> <type> <type> ...
    specific' :: Parser MType
    specific' = do
      l <- Tok.tag Tok.specificType
      s <- Tok.specificType
      ss <- many mtype 
      return $ MSpecific s ss l

    -- <name> <type> <type> ...
    generic' :: Parser MType
    generic' = do
      -- TODO - the genericType should automatically fail on keyword conflict
      notFollowedBy (Tok.reserved "where")
      l <- Tok.tag Tok.genericType
      s <- Tok.genericType
      ss <- many mtype 
      return $ MGeneric s ss l

    -- <name> { <name> :: <type>, <name> :: <type>, ... }
    record' :: Parser MType
    record' = do
      l <- Tok.tag Tok.specificType
      n <- Tok.specificType
      xs <- Tok.braces (sepBy1 recordEntry' Tok.comma)
      return $ MRecord n xs l

    -- (<name> = <type>)
    recordEntry' :: Parser (Name, MType)
    recordEntry' = do
      n <- Tok.name
      Tok.op "::"
      t <- mtype
      return (n, t)


booleanExpr :: Parser BExpr
booleanExpr =
      try booleanBinOp
  <|> try relativeExpr
  <|> try not'
  <|> try (Tok.parens booleanExpr)
  <|> try application'
  <?> "an expression that reduces to True/False"
  where
    not' = do
      Tok.reserved "not"
      e <- booleanExpr
      return $ NOT e

    application' = do
      n <- Tok.name
      ns <- many Tok.name
      return $ BExprFunc n ns

booleanBinOp :: Parser BExpr
booleanBinOp = do
  a <- bterm'
  op <- Tok.logicalBinOp
  b <- bterm'
  return $ binop' op a b
  where
    bterm' = do
      s <-  application'
        <|> bool'
        <|> Tok.parens booleanExpr
        <?> "boolean expression"
      return s

    application' = do
      n <- Tok.name
      ns <- many Tok.name
      return $ BExprFunc n ns

    bool' = do
      s <- Tok.boolean
      return $ BExprBool s

    binop' op a b
      | op == "and" = AND a b
      | op == "or"  = OR  a b

relativeExpr :: Parser BExpr
relativeExpr = do
  a <- arithmeticExpr
  op <- Tok.relativeBinOp
  b <- arithmeticExpr
  return $ relop' op a b
  where
    relop' op a b
      | op == "==" = EQ' a b
      | op == "!=" = NE' a b
      | op == ">"  = GT' a b
      | op == "<"  = LT' a b
      | op == ">=" = GE' a b
      | op == "<=" = LE' a b

arithmeticExpr
  = TPE.buildExpressionParser arithmeticTable arithmeticTerm
  <?> "expression"

arithmeticTerm
  = do
      Tok.parens arithmeticExpr
  <|> try access'
  <|> val'
  <|> var'
  <?> "simple expression. Currently only integers are allowed"
  where
    val' = do
      x <- Tok.mdata
      return $ toExpr' x

    var' = do
      x <- Tok.name
      xs <- option [] (many Tok.name)
      return $ AExprFunc x xs

    access' = do
      x <- Tok.name
      ids <- Tok.brackets (sepBy1 arithmeticExpr Tok.comma)
      return $ AExprAccess x ids

    toExpr' :: MData -> AExpr
    toExpr' (MInt x) = AExprInt x
    toExpr' (MNum x) = AExprReal x
    toExpr' _ = undefined

arithmeticTable
  = [
      [ prefix "-" Neg
      , prefix "+" Pos
      ]             
    , [ binary "^"  Pow TPE.AssocRight
      ]
    , [ binary "*"  Mul TPE.AssocLeft
      , binary "/"  Div TPE.AssocLeft
      , binary "%"  Mod TPE.AssocLeft
      , binary "//" Quo TPE.AssocLeft
      ]
    , [ binary "+"  Add TPE.AssocLeft
      , binary "-"  Sub TPE.AssocLeft
      ]
  ]

functionTable = [[ binary "."  ExprComposition TPE.AssocRight]]

binary name fun assoc = TPE.Infix  (do{ Tok.op name; return fun }) assoc
prefix name fun       = TPE.Prefix (do{ Tok.op name; return fun })
