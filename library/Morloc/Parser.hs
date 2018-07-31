{-# LANGUAGE OverloadedStrings #-}

module Morloc.Parser (morlocScript) where

import Text.Parsec hiding (State)
import qualified Data.RDF as DR
import qualified Text.Parsec.Expr as TPE
import qualified Control.Monad.Except as CME
import qualified Data.List as DL

import qualified Morloc.Error as ME
import qualified Morloc.State as MS
import qualified Morloc.Triple as M3
import qualified Morloc.Lexer as Tok

-- | Parse a string of Morloc text into an AST. Catch lexical syntax errors.
morlocScript :: String -> ME.ThrowsError M3.RDF
morlocScript s =
  case runParser contents MS.parserStateEmpty "<stdin>" s of
    Left err  -> CME.throwError $ ME.SyntaxError err
    Right (M3.TopRDF _ val) -> return val

-- (>>) :: f a -> f b -> f a
-- (<*) :: f a -> (a -> f b) -> f a
contents :: MS.Parser M3.TopRDF
contents = do
  i <- MS.getId
  xs <- Tok.whiteSpace >> many top <* eof
  return $ M3.makeTopRDF i (
         [M3.uss i "rdf:type" "morloc:script"]
      ++ M3.adopt i xs
    )

top :: MS.Parser M3.TopRDF 
top =
      try (export'    <* optional (Tok.op ";") )
  <|> try (source'    <* optional (Tok.op ";") )
  <|> try (statement' <*           Tok.op ";"  )
  <|> try (import'    <* optional (Tok.op ";") )
  <|> try (expression <*           Tok.op ";"  )
  <?> "Top. Maybe you are missing a semicolon?"

export' :: MS.Parser M3.TopRDF
export' = do
  Tok.reserved "export"
  i <- MS.getId
  n <- Tok.name
  return $ M3.makeTopRDF i ([
        M3.uss i "rdf:type" "morloc:export"
      , M3.ust i "rdf:value" "morloc:string" n
    ])

-- | parses a 'source' header, returning the language
source' :: MS.Parser M3.TopRDF
source' = do
  -- id for this source
  Tok.reserved "source"
  -- get the language of the imported source
  lang <- Tok.stringLiteral
  -- get the path to the source file, if Nothing, then assume "vanilla"
  i <- MS.getId
  path <- optionMaybe (Tok.reserved "from" >> Tok.stringLiteral)
  -- get the function imports with with optional aliases
  fs <- Tok.parens (sepBy importAs' Tok.comma)
  -- the statement is unambiguous even without a semicolon
  optional (Tok.op ";")

  return $ M3.makeTopRDF i ([
          (M3.uss i "rdf:type" "morloc:source")
        , (M3.ust i "morloc:lang" "morloc:string" lang)
      ] ++
        M3.adoptAs "morloc:import" i fs ++
        maybe [] (\p -> [M3.ust i "morloc:path" "morloc:string" p]) path
    )

  where
    importAs' :: MS.Parser M3.TopRDF
    importAs' = do
      i <- MS.getId
      -- quoting the function names allows them to have more arbitrary values,
      -- perhaps even including expressions that return functions (though this
      -- is probably bad practice).
      func <- Tok.stringLiteral
      -- the alias is especially important when the native function name is not
      -- legal Morloc syntax, for example an R function with a '.' in the name.
      -- If no alias is given, the alias is set to be the same as the name.
      alias <- option func (Tok.reserved "as" >> Tok.name)
      return $ M3.makeTopRDF i (
           [ M3.ust i "morloc:name" "morloc:string" func
           , M3.ust i "morloc:alias" "morloc:string" alias
           ]
        )

statement' :: MS.Parser M3.TopRDF
statement' =
      try typeDeclaration
  <|> try dataDeclaration

import' :: MS.Parser M3.TopRDF
import' =
      try restrictedImport
  <|> try simpleImport

simpleImport :: MS.Parser M3.TopRDF
simpleImport = do
  i <- MS.getId
  Tok.reserved "import"
  path <- Tok.path
  qual <- optionMaybe (Tok.op "as" >> Tok.name)
  return $ M3.makeTopRDF i (
      [
        (M3.uss i "rdf:type" "morloc:import")
      , (M3.ust i "morloc:name" "morloc:string" (DL.intercalate "." path))
      ] ++ maybe [] (\q -> [M3.ust i "morloc:namespace" "morloc:string" q]) qual
    )

restrictedImport :: MS.Parser M3.TopRDF
restrictedImport = do
  i <- MS.getId
  Tok.reserved "from"
  path <- Tok.path
  Tok.reserved "import"
  functions <- Tok.parens (sepBy1 tripleName Tok.comma)
  return $ M3.makeTopRDF i (
      [
        M3.uss i "rdf:type" "morloc:restricted_import"
      , M3.ust i "morloc:name" "morloc:string" (DL.intercalate "." path)
      ] ++ M3.adoptAs "morloc:import" i functions
    )

dataDeclaration :: MS.Parser M3.TopRDF
dataDeclaration = do
  i <- MS.getId
  lhs <- tripleName 
  bndvars <- many tripleName
  Tok.op "="
  rhs <- expression
  return $ M3.makeTopRDF i (
         [M3.uss i "rdf:type" "morloc:dataDeclaration"]
      ++ M3.adoptAs "morloc:lhs" i [lhs]
      ++ M3.adopt i bndvars
      ++ M3.adoptAs "morloc:rhs" i [rhs]
    )

-- | function :: [input] -> output constraints
typeDeclaration :: MS.Parser M3.TopRDF
typeDeclaration = do
  i <- MS.getId
  lhs <- tripleName
  Tok.op "::"
  rhs <- mtype
  constraints <- option [] (
      Tok.reserved "where" >>
      Tok.parens (sepBy1 booleanExpr Tok.comma)
    )
  return $ M3.makeTopRDF i (
         [M3.uss i "rdf:type" "morloc:typeDeclaration"]
      ++ M3.adoptAs "morloc:lhs" i [lhs]
      ++ M3.adoptAs "morloc:rhs" i [rhs]
      ++ M3.adoptAs "morloc:constraint" (M3.rdfId rhs) constraints
    )

listTag :: DR.Node -> Maybe String -> [M3.Triple]
listTag i tag = maybe [] (\t -> [M3.ust i "morloc:label" "morloc:name" t]) tag

mtype :: MS.Parser M3.TopRDF
mtype =
        try function' -- a [, *] -> b [where (*)]
    <|> try specific' -- A ...
    <|> try generic'  -- a ...
    <|> try record'   -- A { ... }
    <|> try unambiguous'
    <?> "type"
  where

    -- any type other than a naked function
    notFunction' :: MS.Parser M3.TopRDF
    notFunction' =
          try specific' -- A ...
      <|> try generic'  -- a ...
      <|> try record'   -- A { ... }
      <|> try unambiguous'
      <?> "type"

    unambiguous' :: MS.Parser M3.TopRDF
    unambiguous' =
            try empty' -- ()
        <|> try paren' -- (a)
        <|> try tuple' -- (a, ...)
        <|> list'      -- [a]
        <|> specific1  -- A
        <|> generic1   -- a

    -- <name> <type> <type> ...
    specific' :: MS.Parser M3.TopRDF
    specific' = do
      l <- Tok.tag Tok.genericType
      n <- Tok.specificType
      i <- MS.getId
      ns <- many1 unambiguous'
      return $ M3.makeTopRDF i (
             [ M3.ust i "rdf:type" "morloc:parameterizedType" n ]
          ++ (listTag i l)
          ++ M3.adopt i ns
        )

    -- Does parameterized generic even make sense?  Yes, say `f Int` where `f`
    -- is a generic collection of integers. Then you can map across it with any
    -- function of an Int.
    --
    -- <name> <type> <type> ...
    generic' :: MS.Parser M3.TopRDF
    generic' = do
      notFollowedBy (Tok.reserved "where")
      l <- Tok.tag Tok.genericType
      n <- Tok.genericType
      i <- MS.getId
      ns <- many1 unambiguous'
      return $ M3.makeTopRDF i (
             [ M3.uss i "rdf:type" "morloc:parameterizedGeneric"
             , M3.ust i "morloc:value" "morloc:string" n
             ]
          ++ listTag i l
          ++ M3.adopt i ns
        )

    -- <name> <type> <type> ...
    specific1 :: MS.Parser M3.TopRDF
    specific1 = do
      l <- Tok.tag Tok.specificType
      n <- Tok.specificType
      i <- MS.getId
      return $ M3.makeTopRDF i (
             [ M3.ust i "rdf:type" "morloc:atomicType" n ]
          ++ listTag i l
        )

    -- <name> <type> <type> ...
    generic1 :: MS.Parser M3.TopRDF
    generic1 = do
      notFollowedBy (Tok.reserved "where")
      l <- Tok.tag Tok.genericType
      n <- Tok.genericType
      i <- MS.getId
      return $ M3.makeTopRDF i (
             [ M3.ust i "rdf:type" "morloc:atomicGeneric" n
             ]
          ++ (listTag i l)
        )

    empty' :: MS.Parser M3.TopRDF
    empty' = do
      Tok.op "("
      Tok.op ")"
      i <- MS.getId
      return $ M3.makeTopRDF i [M3.uss i "rdf:type" "morloc:empty"]

    paren' :: MS.Parser M3.TopRDF 
    paren' = Tok.parens mtype

    tuple' :: MS.Parser M3.TopRDF
    tuple' = Tok.parens $ do
      i <- MS.getId
      l <- Tok.tag (char '(')
      x <- mtype
      Tok.op ","
      xs <- sepBy1 mtype Tok.comma
      return $ M3.makeTopRDF i (
             [ M3.ust i "rdf:type" "morloc:parameterizedType" "Tuple" ]
          ++ listTag i l
          ++ M3.adopt i (x:xs)
        )

    -- [ <type> ]
    list' :: MS.Parser M3.TopRDF
    list' = do
      i <- MS.getId
      l <- Tok.tag (char '[')
      s <- Tok.brackets mtype
      return $ M3.makeTopRDF i (
             [ M3.ust i "rdf:type" "morloc:parameterizedType" "List" ]
          ++ listTag i l
          ++ M3.adopt i [s]
        )

    -- { <name> :: <type>, <name> :: <type>, ... }
    record' :: MS.Parser M3.TopRDF
    record' = do
      i <- MS.getId
      l <- Tok.tag (char '{')
      ns <- Tok.braces $ sepBy1 recordEntry' Tok.comma
      return $ M3.makeTopRDF i (
             [ M3.ust i "rdf:type" "morloc:parameterizedType" "Record" ]
          ++ listTag i l
          ++ M3.adopt i ns
        )

    -- (<name> = <type>)
    recordEntry' :: MS.Parser M3.TopRDF
    recordEntry' = do
      i <- MS.getId
      n <- Tok.name
      Tok.op "::"
      t <- mtype
      return $ M3.makeTopRDF i (
          [ M3.uss i "rdf:type" "morloc:namedType"
          , M3.ust i "morloc:key" "morloc:name" n
          ] ++ M3.adoptAs "morloc:value" i [t]
        )

    function' :: MS.Parser M3.TopRDF
    function' = do
      i <- MS.getId
      inputs <- sepBy1 notFunction' Tok.comma
      Tok.op "->"
      output <- notFunction'
      return $ M3.makeTopRDF i (
             [M3.uss i "rdf:type" "morloc:functionType"]
          ++ M3.adopt i inputs
          ++ M3.adoptAs "morloc:output" i [output]
        )


mdata :: MS.Parser M3.TopRDF
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

      list' :: MS.Parser M3.TopRDF
      list' = do
        i <- MS.getId
        xs <- Tok.brackets (sepBy mdata Tok.comma)
        return $ M3.makeTopRDF i (
               [M3.uss i "rdf:type" "morloc:list"]
            ++ M3.adopt i xs
          )

      tuple' = do
        i <- MS.getId
        xs <- Tok.parens tuple''
        return $ M3.makeTopRDF i (
               [M3.uss i "rdf:type" "morloc:tuple"]
            ++ M3.adopt i xs
          )

      record' = do
        i <- MS.getId
        xs <- Tok.braces (sepBy1 recordEntry' Tok.comma) 
        return $ M3.makeTopRDF i (
               [M3.uss i "rdf:type" "morloc:record"]
            ++ M3.adopt i xs
          )

      -- must have at least two elements
      tuple'' = do
        x <- mdata
        _ <- Tok.comma
        xs <- sepBy1 mdata Tok.comma
        return $ x:xs

      -- parse a tag/value pair
      recordEntry' = do
        i <- MS.getId
        n <- Tok.name
        Tok.op "="
        t <- mdata
        return $ M3.makeTopRDF i (
            [ M3.uss i "rdf:type" "morloc:recordEntry"
            , M3.ust i "morloc:lhs" "morloc:name" n
            ] ++ M3.adoptAs "morloc:rhs" i [t]
          )

expression :: MS.Parser M3.TopRDF
expression =
  -- currently this just handles "."
      try (TPE.buildExpressionParser functionTable term')
  <|> term'
  <?> "an expression"
  where
    term' :: MS.Parser M3.TopRDF
    term' =
          try (Tok.parens expression)
      <|> try application
      <|> try mdata
      <|> try tripleName

application :: MS.Parser M3.TopRDF
application = do
  i <- MS.getId
  function <- Tok.parens expression <|> identifier'
  arguments <- many1 term'
  return $ M3.makeTopRDF i (
         [M3.uss i "rdf:type" "morloc:call"]
      ++ M3.adoptAs  "morloc:value" i [function]
      ++ M3.adopt i arguments
    )
  where
    term' =
          try (Tok.parens expression)
      <|> try identifier'
      <|> try mdata

    identifier' = do
      i    <- MS.getId
      x    <- Tok.name
      tag' <- Tok.tag Tok.name
      return $ M3.makeTopRDF i (
          [ M3.ust i "rdf:type" "morloc:name" x
          ] ++ listTag i tag')

booleanExpr :: MS.Parser M3.TopRDF
booleanExpr = do
      try booleanBinOp
  <|> try relativeExpr
  <|> try not'
  <|> try (Tok.parens booleanExpr)
  <|> try call'
  <?> "an expression that reduces to True/False"
  where
    not' :: MS.Parser M3.TopRDF
    not' = do
      Tok.reserved "not"
      i <- MS.getId
      e <- booleanExpr
      return $ M3.makeTopRDF i (M3.adoptAs "morloc:not" i [e]) 

    call' :: MS.Parser M3.TopRDF
    call' = do
      i <- MS.getId
      f <- Tok.parens expression <|> identifier'
      ns <- many1 argument'
      return $ M3.makeTopRDF i (
             [ M3.uss i "rdf:type" "morloc:call" ]
          ++ M3.adoptAs  "morloc:value" i [f]
          ++ M3.adopt i ns
        )

    identifier' :: MS.Parser M3.TopRDF
    identifier' = do
      i <- MS.getId
      n <- Tok.name
      return $ M3.makeTopRDF i [M3.ust i "rdf:type" "morloc:name" n]

    argument' :: MS.Parser M3.TopRDF
    argument' =
          (Tok.parens booleanExpr)
      <|> try tripleBool
      <|> tripleName
      <?> "expect an argument"

booleanBinOp :: MS.Parser M3.TopRDF
booleanBinOp = do
  i <- MS.getId
  a <- bterm'
  op <- Tok.logicalBinOp
  b <- bterm'
  return $ binOp op i a b
  where
    bterm' =
            tripleBool
        <|> tripleName
        <|> Tok.parens booleanExpr
        <?> "boolean expression"

relativeExpr :: MS.Parser M3.TopRDF
relativeExpr = do
  i <- MS.getId
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
      | otherwise = error "Unsupported operator" 

arithmeticExpr :: MS.Parser M3.TopRDF
arithmeticExpr
  =   TPE.buildExpressionParser arithmeticTable arithmeticTerm
  <?> "expression"

arithmeticTerm :: MS.Parser M3.TopRDF
arithmeticTerm = do
      Tok.parens arithmeticExpr
  <|> try access'
  <|> try mdata
  <|> try call'
  <|> tripleName
  <?> "simple expression. Currently only integers are allowed"
  where

    call' = do
      i <- MS.getId
      f <- Tok.parens expression <|> identifier'
      args <- many1 argument'
      return $ M3.makeTopRDF i (
             [M3.uss i "rdf:type" "morloc:call"]
          ++ M3.adoptAs "morloc:value" i [f]
          ++ M3.adopt i args
        )

    identifier' :: MS.Parser M3.TopRDF
    identifier' = do
      i <- MS.getId
      n <- Tok.name
      return $ M3.makeTopRDF i [M3.ust i "rdf:type" "morloc:name" n]

    argument' :: MS.Parser M3.TopRDF
    argument' =
          Tok.parens arithmeticExpr
      <|> try access'
      <|> try mdata
      <|> tripleName
      <?> "a function argument"

    access' = do
      i <- MS.getId
      x <- Tok.name
      ids <- Tok.brackets (sepBy1 arithmeticExpr Tok.comma)
      return $ M3.makeTopRDF i (
          [ M3.uss i "rdf:type" "morloc:access"
          , M3.ust i "morloc:name" "morloc:string" x
          ] ++ M3.adopt i ids
        )

arithmeticTable
  = [
      [ prefix "-" (unaryOp "Neg")
      , prefix "+" (unaryOp "Pos")
      ]
    , [ binary "^"  (binOp "Pow") TPE.AssocRight
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

unaryOp :: String -> DR.Node -> M3.TopRDF -> M3.TopRDF
unaryOp s i (M3.TopRDF j xs) = M3.makeTopRDF i (
     [ M3.ust i "rdf:type" "morloc:unaryOp" s
     , M3.usu j "rdf:_0" i
     ] ++ (DR.triplesOf xs)
  )

binOp :: String -> DR.Node -> M3.TopRDF -> M3.TopRDF -> M3.TopRDF
binOp s i (M3.TopRDF j xs) (M3.TopRDF k ys) = M3.makeTopRDF i (
     [ M3.ust i "rdf:type" "morloc:binop" s
     , M3.usu i "morloc:lhs" j
     , M3.usu i "morloc:rhs" k
     ] ++ (DR.triplesOf xs) ++ (DR.triplesOf ys)
  )

functionTable = [[ binary "."  exprComposition TPE.AssocRight ]]

exprComposition :: DR.Node -> M3.TopRDF -> M3.TopRDF -> M3.TopRDF
exprComposition i (M3.TopRDF j xs) (M3.TopRDF k ys) = M3.makeTopRDF i (
     [ M3.uss i "rdf:type" "morloc:composition"
     , M3.usu i "morloc:lhs" j
     , M3.usu i "morloc:rhs" k
     ] ++ (DR.triplesOf xs) ++ (DR.triplesOf ys)
  )

binary name fun assoc = TPE.Infix (do {
    Tok.op name;
    i <- MS.getId;
    return (fun i);
  }) assoc

prefix name fun = TPE.Prefix (do {
    Tok.op name;
    i <- MS.getId;
    return (fun i);
  })

triplePrimitive :: String -> MS.Parser a -> (a -> String) -> MS.Parser M3.TopRDF
triplePrimitive isa p show' = do
  i <- MS.getId
  n <- p
  -- TODO: the separation of "isa" and "value" is now redundant, since the
  -- primitives are typed
  return $ M3.makeTopRDF i [M3.ust i "rdf:type" isa (show' n)]

tripleInteger       :: MS.Parser M3.TopRDF
tripleFloat         :: MS.Parser M3.TopRDF
tripleName          :: MS.Parser M3.TopRDF
tripleStringLiteral :: MS.Parser M3.TopRDF
tripleBool          :: MS.Parser M3.TopRDF

tripleInteger       = triplePrimitive "morloc:integer" Tok.integer       show
tripleFloat         = triplePrimitive "morloc:number"  Tok.float         show
tripleName          = triplePrimitive "morloc:name"    Tok.name          id
tripleStringLiteral = triplePrimitive "morloc:string"  Tok.stringLiteral id
tripleBool          = triplePrimitive "morloc:boolean" Tok.boolean       show
