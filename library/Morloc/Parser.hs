module Morloc.Parser (morlocScript) where

import Text.Parsec hiding (State, Parser)
import qualified Text.Parsec.Expr as TPE
import Control.Monad.State
import Control.Monad.Except (throwError)

import Morloc.Error
import Morloc.Syntax
import Morloc.State
import Morloc.Triple
import qualified Morloc.Lexer as Tok

-- | Parse a string of Morloc text into an AST. Catch lexical syntax errors.
morlocScript :: String -> ThrowsError [Triple]
morlocScript s =
  case runParser contents parserStateEmpty "<stdin>" s of
    Left err  -> throwError $ SyntaxError err
    Right val -> return val

-- (>>) :: f a -> f b -> f a
-- (<*) :: f a -> (a -> f b) -> f a
contents :: Parser [Triple]
contents = fmap concat (Tok.whiteSpace >> many top <* eof)

top :: Parser [Triple]
top =
      try (source'    <* optional (Tok.op ";") )
  <|> try (statement' <*           Tok.op ";"  )
  <|> try (import'    <* optional (Tok.op ";") )
  <?> "Top. Maybe you are missing a semicolon?"

-- | parses a 'source' header, returning the language
source' :: Parser [Triple]
source' = do
  Tok.reserved "source"
  -- get the language of the imported source
  lang <- Tok.stringLiteral
  -- get the path to the srouce file, if Nothing, then assume "vanilla"
  path <- optionMaybe (Tok.reserved "from" >> Tok.path)
  -- get the function imports with with optional aliases
  fs <- Tok.parens (sepBy importAs' Tok.comma)

  -- the statement is unambiguous even without a semicolon
  optional (Tok.op ";")

  return $ [(42, ":source", Id' 666)]

  -- return $ case path of
  --   (Just p) -> SourceFile lang p fs
  --   Nothing  -> SourceLang lang fs
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

statement' :: Parser [Triple]
statement' =
      try signature
  <|> try declaration

import' :: Parser [Triple]
import' =
      try restrictedImport
  <|> try simpleImport

simpleImport :: Parser [Triple]
simpleImport = do
  Tok.reserved "import"
  path <- Tok.path
  qual <- optionMaybe (Tok.op "as" >> Tok.name)

  return $ [(42, ":simpleImport", Id' 666)]
  -- return $ Import path qual Nothing

restrictedImport :: Parser [Triple]
restrictedImport = do
  Tok.reserved "from"
  path <- Tok.path
  Tok.reserved "import"
  -- TODO: I am also importing ontologies, how should that be handled?
  -- TODO: at very least, I am also importing types
  functions <- Tok.parens (sepBy1 Tok.name Tok.comma)

  return $ [(42, ":restrictedImport", Id' 666)]
  -- return $ Import path Nothing (Just functions)

declaration :: Parser [Triple]
declaration = do
  varname <- Tok.name
  bndvars <- many Tok.name
  Tok.op "="
  value <- expression

  return $ [(42, ":declaration", Id' 666)]
  -- return $ Declaration varname bndvars value

-- | function :: [input] -> output constraints
signature :: Parser [Triple]
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

  -- -- Record this function signature
  -- pushTriple      function (Triple.IsA' Triple.Signature')
  -- pushTriple      function (Triple.Name' function)
  -- pushTriple      function (Triple.Params' constraints)

  return $ [(42, ":signature", Id' 666)]
  -- return $ Signature function inputs output constraints


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
      x <- try mdata
      return $ ExprData x

application :: Parser Expression
application = do
  -- this should be either a function or a composition
  function <- Tok.parens expression <|> var'
  arguments <- sepBy term' Tok.whiteSpace
  return $ ExprApplication function arguments
  where
    term' =
          try (Tok.parens expression)
      <|> try var'
      <|> try dat'

    var' = do
      x    <- Tok.name
      tag' <- Tok.tag Tok.name
      return $ ExprVariable x tag'

    dat' = fmap ExprData (try mdata)

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

mdata :: Parser MData
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

    integer'       = fmap MInt Tok.integer
    float'         = fmap MNum Tok.float
    stringLiteral' = fmap MStr Tok.stringLiteral
    boolean'       = fmap MLog Tok.boolean
    list'          = fmap MLst (Tok.brackets (sepBy mdata Tok.comma))
    tuple'         = fmap MTup (Tok.parens tuple'')
    record'        = fmap MRec (Tok.braces (sepBy1 recordEntry' Tok.comma))

    -- must have at least two elements
    tuple'' = do
      x <- mdata
      Tok.comma
      xs <- sepBy1 mdata Tok.comma
      return $ x:xs

    -- parse a tag/value pair
    recordEntry' = do
      n <- Tok.name
      Tok.op "="
      t <- mdata
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
    not' = fmap NOT (Tok.reserved "not" >> booleanExpr)
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
    bterm' =
            application'
        <|> bool'
        <|> Tok.parens booleanExpr
        <?> "boolean expression"

    application' = do
      n <- Tok.name
      ns <- many Tok.name
      return $ BExprFunc n ns

    bool' = fmap BExprBool Tok.boolean

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
  =
      Tok.parens arithmeticExpr
  <|> try access'
  <|> val'
  <|> var'
  <?> "simple expression. Currently only integers are allowed"
  where
    val' = fmap toExpr' mdata

    var' = do
      x <- Tok.name
      xs <- option [] (many arithmeticTerm)
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


concatOrderedTriples :: [[Triple]] -> [Triple]
concatOrderedTriples tss
  =  concat tss
  ++ map (\((i,_,_):_,j) -> (i, ":position", Id' j)) (zip tss [0..])

triplePrimitive :: String -> Parser a -> (a -> Object) -> Parser [Triple]
triplePrimitive isa p f = do
  i <- getId
  n <- p
  s <- getState
  return $ [
        (i, ":isa", Str' isa)
      , (i, ":value", f n)
      , (i, ":parent", Id' (stateScope s))
    ]

tripleInteger       :: Parser [Triple]
tripleFloat         :: Parser [Triple]
tripleName          :: Parser [Triple]
tripleStringLiteral :: Parser [Triple]
tripleBool          :: Parser [Triple]

tripleInteger       = triplePrimitive ":integer" Tok.integer       Int'
tripleFloat         = triplePrimitive ":number"  Tok.float         Num'
tripleName          = triplePrimitive ":name"    Tok.name          Str'
tripleStringLiteral = triplePrimitive ":string"  Tok.stringLiteral Str'
tripleBool          = triplePrimitive ":boolean" Tok.boolean       Log'
