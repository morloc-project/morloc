module Morloc.Parser (morlocScript) where

import Text.Parsec hiding (State, Parser)
import qualified Text.Parsec.Expr as TPE
import Control.Monad.State
import Control.Monad.Except (throwError)
import Data.List (intercalate)

import Morloc.Error
import Morloc.Syntax
import Morloc.State
import Morloc.Triple
import qualified Morloc.Lexer as Tok

adopt :: Subject -> [RDF] -> [Triple]
adopt = adoptAs ":child"

-- | Parse a string of Morloc text into an AST. Catch lexical syntax errors.
morlocScript :: String -> ThrowsError RDF
morlocScript s =
  case runParser contents parserStateEmpty "<stdin>" s of
    Left err  -> throwError $ SyntaxError err
    Right val -> return val

-- (>>) :: f a -> f b -> f a
-- (<*) :: f a -> (a -> f b) -> f a
contents :: Parser RDF
contents = do
  i <- getId
  xs <- Tok.whiteSpace >> many top <* eof
  return $ RDF i (
         [(i, ":isa", Str' ":script")]
      ++ adopt i xs
    )

top :: Parser RDF 
top =
      try (source'    <* optional (Tok.op ";") )
  <|> try (statement' <*           Tok.op ";"  )
  <|> try (import'    <* optional (Tok.op ";") )
  <|> try (expression <*           Tok.op ";"  )
  <?> "Top. Maybe you are missing a semicolon?"

-- | parses a 'source' header, returning the language
source' :: Parser RDF
source' = do
  -- id for this source
  Tok.reserved "source"
  -- get the language of the imported source
  lang <- Tok.stringLiteral
  -- get the path to the source file, if Nothing, then assume "vanilla"
  i <- getId
  path <- optionMaybe (Tok.reserved "from" >> Tok.stringLiteral)
  -- get the function imports with with optional aliases
  fs <- Tok.parens (sepBy importAs' Tok.comma)
  -- the statement is unambiguous even without a semicolon
  optional (Tok.op ";")

  return $ RDF i ([
          (i, ":isa",  Str' ":source")
        , (i, ":lang", Str' lang)
      ] ++
        adopt i fs ++
        maybe [] (\p -> [(i, ":path", Str' p)]) path
    )

  where
    importAs' :: Parser RDF
    importAs' = do
      i <- getId
      -- quoting the function names allows them to have more arbitrary values,
      -- perhaps even including expressions that return functions (though this
      -- is probably bad practice).
      func <- Tok.stringLiteral
      -- the alias is especially important when the native function name is not
      -- legal Morloc syntax, for example an R function with a '.' in the name.
      alias <- optionMaybe (Tok.reserved "as" >> Tok.name)
      return $ RDF i (
           [(i, ":name", Str' func)] ++
            maybe [] (\x -> [(i, ":alias", Str' x)]) alias
        )

statement' :: Parser RDF
statement' =
      try typeDeclaration
  <|> try declaration

import' :: Parser RDF
import' =
      try restrictedImport
  <|> try simpleImport

simpleImport :: Parser RDF
simpleImport = do
  i <- getId
  Tok.reserved "import"
  path <- Tok.path
  qual <- optionMaybe (Tok.op "as" >> Tok.name)
  return $ RDF i (
      [
        (i, ":isa", Str' ":import")
      , (i, ":name", Str' (intercalate "." path))
      ] ++ maybe [] (\q -> [(i, ":namespace", Str' q)]) qual
    )

restrictedImport :: Parser RDF
restrictedImport = do
  i <- getId
  Tok.reserved "from"
  path <- Tok.path
  Tok.reserved "import"
  functions <- Tok.parens (sepBy1 tripleName Tok.comma)
  return $ RDF i (
      [
        (i, ":isa", Str' ":restricted_import")
      , (i, ":name", Str' (intercalate "." path))
      ] ++ adopt i functions
    )

declaration :: Parser RDF
declaration = do
  i <- getId
  lhs <- tripleName 
  bndvars <- many tripleName
  Tok.op "="
  rhs <- expression
  return $ RDF i (
         [(i, ":isa", Str' ":declaration")]
      ++ adoptAs ":lhs"       i [lhs]
      ++ adoptAs ":parameter" i bndvars
      ++ adoptAs ":rhs"       i [rhs]
    )

-- | function :: [input] -> output constraints
typeDeclaration :: Parser RDF
typeDeclaration = do
  i <- getId
  lhs <- tripleName
  Tok.op "::"
  rhs <- mtype
  constraints <- option [] (
      Tok.reserved "where" >>
      Tok.parens (sepBy1 booleanExpr Tok.comma)
    )
  return $ RDF i (
         [(i, ":isa", Str' ":typeDeclaration")]
      ++ adoptAs ":lhs" i [lhs]
      ++ adoptAs ":rhs" i [rhs]
      ++ adoptAs ":constraint" (rdfId rhs) constraints
    )

listTag :: Subject -> Maybe String -> [(Subject, Relation, Object)] 
listTag i tag = maybe [] (\t -> [(i, ":label", Str' t)]) tag

mtype :: Parser RDF
mtype =
        try function' -- a [, *] -> b [where (*)]
    <|> try specific' -- A ...
    <|> try generic'  -- a ...
    <|> try record'   -- A { ... }
    <|> try unambiguous'
    <?> "type"
  where

    unambiguous' :: Parser RDF
    unambiguous' =
            try empty' -- ()
        <|> try paren' -- (a)
        <|> try tuple' -- (a, ...)
        <|> specific1  -- A
        <|> generic1   -- a
        <|> list'      -- [a]

    -- <name> <type> <type> ...
    specific' :: Parser RDF
    specific' = do
      l <- Tok.tag Tok.specificType
      n <- Tok.specificType
      i <- getId
      ns <- many1 unambiguous'
      return $ RDF i (
             [(i, ":isa", Str' ":type"), (i, ":value", Str' n)]
          ++ listTag i l
          ++ adopt i ns
        )

    -- Does parameterized generic even make sense?  Yes, say `f Int` where `f`
    -- is a generic collection of integers. Then you can map across it with any
    -- function of an Int.
    --
    -- <name> <type> <type> ...
    generic' :: Parser RDF
    generic' = do
      -- TODO - the genericType should automatically fail on keyword conflict
      notFollowedBy (Tok.reserved "where")
      l <- Tok.tag Tok.genericType
      n <- Tok.genericType
      i <- getId
      ns <- many1 unambiguous'
      return $ RDF i (
             [(i, ":isa", Str' ":generic"), (i, ":value", Str' n)]
          ++ (listTag i l)
          ++ (adopt i ns)
        )

    -- <name> <type> <type> ...
    specific1 :: Parser RDF
    specific1 = do
      l <- Tok.tag Tok.specificType
      n <- Tok.specificType
      i <- getId
      return $ RDF i (
             [(i, ":isa", Str' ":type"), (i, ":value", Str' n)]
          ++ listTag i l
        )

    -- <name> <type> <type> ...
    generic1 :: Parser RDF
    generic1 = do
      -- TODO - the genericType should automatically fail on keyword conflict
      notFollowedBy (Tok.reserved "where")
      l <- Tok.tag Tok.genericType
      n <- Tok.genericType
      i <- getId
      return $ RDF i (
             [(i, ":isa", Str' ":generic"), (i, ":value", Str' n)]
          ++ (listTag i l)
        )

    empty' :: Parser RDF
    empty' = do
      Tok.op "("
      Tok.op ")"
      i <- getId
      return $ RDF i [(i, ":isa", Str' ":empty")]

    paren' :: Parser RDF 
    paren' = Tok.parens mtype

    tuple' :: Parser RDF
    tuple' = Tok.parens $ do
      i <- getId
      l <- Tok.tag (char '(')
      x <- mtype
      Tok.op ","
      xs <- sepBy1 mtype Tok.comma
      return $ RDF i (
             [(i, ":isa", Str' ":tuple")]
          ++ listTag i l
          ++ adoptAs ":contains" i (x:xs)
        )

    -- [ <type> ]
    list' :: Parser RDF
    list' = do
      i <- getId
      l <- Tok.tag (char '[')
      s <- Tok.brackets mtype
      return $ RDF i (
             [(i, ":isa", Str' ":list")]
          ++ listTag i l
          ++ adoptAs ":contains" i [s]
        )

    -- <name> { <name> :: <type>, <name> :: <type>, ... }
    record' :: Parser RDF
    record' = do
      l <- Tok.tag Tok.specificType
      n <- Tok.specificType
      i <- getId
      ns <- Tok.braces $ sepBy1 recordEntry' Tok.comma
      return $ RDF i (
             [ (i, ":isa", Str' ":record"), (i, ":name", Str' n)]
          ++ listTag i l
          ++ adoptAs ":contains" i ns
        )

    -- (<name> = <type>)
    recordEntry' :: Parser RDF
    recordEntry' = do
      i <- getId
      n <- Tok.name
      Tok.op "::"
      t <- mtype
      return $ RDF i (
          [ (i, ":isa", Str' ":recordEntry")
          , (i, ":lhs", Str' n)
          ] ++ adoptAs ":rhs" i [t]
        )

    function' :: Parser RDF
    function' = do
      i <- getId
      inputs <- sepBy1 unambiguous' Tok.comma
      Tok.op "->"
      output <- unambiguous'
      return $ RDF i (
             [(i, ":isa", Str' ":function")]
          ++ adoptAs ":input" i inputs
          ++ adoptAs ":output" i [output]
        )


mdata :: Parser RDF
mdata =  do
        try tripleBool          -- True | False
    <|> try tripleFloat         -- 1.1
    <|> try tripleInteger       -- 1
    <|> try tripleStringLiteral -- "yolo"
    <|> try list'               -- [ ...
    <|> try tuple'              -- ( ...
    <|> try record'             -- { ...
    <?> "literal data"
    where

      list' :: Parser RDF
      list' = do
        i <- getId
        xs <- Tok.brackets (sepBy mdata Tok.comma)
        return $ RDF i (
               [(i, ":isa", Str' ":list")]
            ++ adoptAs ":contains" i xs
          )

      tuple' = do
        i <- getId
        xs <- Tok.parens tuple''
        return $ RDF i (
               [(i, ":isa", Str' ":tuple")]
            ++ adoptAs ":contains" i xs
          )

      record' = do
        i <- getId
        xs <- Tok.braces (sepBy1 recordEntry' Tok.comma) 
        return $ RDF i (
               [(i, ":isa", Str' ":record")]
            ++ adoptAs ":contains" i xs
          )

      -- must have at least two elements
      tuple'' = do
        x <- mdata
        Tok.comma
        xs <- sepBy1 mdata Tok.comma
        return $ x:xs

      -- parse a tag/value pair
      recordEntry' = do
        i <- getId
        n <- Tok.name
        Tok.op "="
        t <- mdata
        return $ RDF i (
            [ (i, ":isa", Str' "recordEntry") 
            , (i, ":lhs", Str' n)
            ] ++ adoptAs ":rhs" i [t]
          )

expression :: Parser RDF
expression =
  -- currently this just handles "."
      try (TPE.buildExpressionParser functionTable term')
  <|> term'
  <?> "an expression"
  where
    term' :: Parser RDF
    term' =
          try (Tok.parens expression)
      <|> try application
      <|> try mdata
      <|> try tripleName

application :: Parser RDF
application = do
  i <- getId
  function <- Tok.parens expression <|> identifier'
  arguments <- many1 term'
  return $ RDF i (
         [ (i, ":isa", Str' ":application")]
      ++ adoptAs ":function" i [function]
      ++ adoptAs ":argument" i arguments
    )
  where
    term' =
          try (Tok.parens expression)
      <|> try identifier'
      <|> try mdata

    identifier' = do
      i    <- getId
      x    <- Tok.name
      tag' <- Tok.tag Tok.name
      return $ RDF i (
          [ (i, ":isa", Str' ":name")
          , (i, ":value", Str' x)
          ] ++ listTag i tag')

-- application of only simple named elements (TODO: am I nuts?) 
simpleApplication :: Parser RDF
simpleApplication = do
  i <- getId
  n <- Tok.name
  ns <- many tripleName
  return $ RDF i (
         [(i, ":isa", Str' ":application"), (i, ":name", Str' n)]
      ++ adopt i ns
    )

booleanExpr :: Parser RDF
booleanExpr = do
      try booleanBinOp
  <|> try relativeExpr
  <|> try not'
  <|> try (Tok.parens booleanExpr)
  <|> try simpleApplication 
  <?> "an expression that reduces to True/False"
  where
    not' :: Parser RDF
    not' = do
      Tok.reserved "not"
      i <- getId
      e <- booleanExpr
      return $ RDF i (adoptAs ":not" i [e]) 

booleanBinOp :: Parser RDF
booleanBinOp = do
  i <- getId
  a <- bterm'
  op <- Tok.logicalBinOp
  b <- bterm'
  return $ binOp op i a b
  where
    bterm' =
            simpleApplication
        <|> tripleBool
        <|> Tok.parens booleanExpr
        <?> "boolean expression"

relativeExpr :: Parser RDF
relativeExpr = do
  i <- getId
  a <- arithmeticExpr
  op <- Tok.relativeBinOp
  b <- arithmeticExpr
  return $ relop' i op a b
  where
    relop' i op a b
      | op == "==" = (binOp "EQ" i a b)
      | op == "!=" = (binOp "NE" i a b)
      | op == ">"  = (binOp "GT" i a b)
      | op == "<"  = (binOp "LT" i a b)
      | op == ">=" = (binOp "GE" i a b)
      | op == "<=" = (binOp "LE" i a b)

arithmeticExpr :: Parser RDF
arithmeticExpr
  =   TPE.buildExpressionParser arithmeticTable arithmeticTerm
  <?> "expression"

arithmeticTerm :: Parser RDF
arithmeticTerm = do
      Tok.parens arithmeticExpr
  <|> try access'
  <|> try mdata
  <|> try call'
  <|> tripleName
  <?> "simple expression. Currently only integers are allowed"
  where

    call' = do
      i <- getId
      x <- Tok.name
      xs <- many1 arithmeticTerm
      return $ RDF i (
          [ (i, ":isa", Str' ":call")
          , (i, ":name", Str' x)
          ] ++ adopt i xs
        )

    access' = do
      i <- getId
      x <- Tok.name
      ids <- Tok.brackets (sepBy1 arithmeticExpr Tok.comma)
      return $ RDF i (
          [ (i, ":isa", Str' ":access")
          , (i, ":name", Str' x)
          ] ++ adopt i ids
        )

arithmeticTable
  = [
      [ binary "^"  (binOp "Pow") TPE.AssocRight
      ]
    , [ binary "*"  (binOp "Mul") TPE.AssocLeft
      , binary "/"  (binOp "Div") TPE.AssocLeft
      , binary "%"  (binOp "Mod") TPE.AssocLeft
      , binary "//" (binOp "Quo") TPE.AssocLeft
      ]                     
    , [ binary "+"  (binOp "Add") TPE.AssocLeft
      , binary "-"  (binOp "Sub") TPE.AssocLeft
      ]
  ]

binOp :: String -> Subject -> RDF -> RDF -> RDF
binOp s i (RDF j xs) (RDF k ys) = RDF i (
     [ (i, ":isa", Str' ":binop") 
     , (i, ":value", Str' s)
     , (i, ":lhs", Id' j)
     , (i, ":rhs", Id' k)
     ] ++ xs ++ ys
  )

functionTable = [[ binary "."  exprComposition TPE.AssocRight ]]

exprComposition :: Subject -> RDF -> RDF -> RDF
exprComposition i (RDF j xs) (RDF k ys) = RDF i (
     [ (i, ":isa", Str' ":composition") 
     , (i, ":lhs", Id' j)
     , (i, ":rhs", Id' k)
     ] ++ xs ++ ys
  )

binary name fun assoc = TPE.Infix (do {
    Tok.op name;
    i <- getId;
    return (fun i);
  }) assoc

prefix name fun = TPE.Prefix (do {
    Tok.op name;
    i <- getId;
    return (fun i);
  })

triplePrimitive :: String -> Parser a -> (a -> Object) -> Parser RDF
triplePrimitive isa p f = do
  i <- getId
  n <- p
  return $ RDF i [(i, ":isa", Str' isa), (i, ":value", f n)]

tripleInteger       :: Parser RDF
tripleFloat         :: Parser RDF
tripleName          :: Parser RDF
tripleStringLiteral :: Parser RDF
tripleBool          :: Parser RDF

tripleInteger       = triplePrimitive ":integer" Tok.integer       Int'
tripleFloat         = triplePrimitive ":number"  Tok.float         Num'
tripleName          = triplePrimitive ":name"    Tok.name          Str'
tripleStringLiteral = triplePrimitive ":string"  Tok.stringLiteral Str'
tripleBool          = triplePrimitive ":boolean" Tok.boolean       Log'
