{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Module
Description : Parses a Morloc script into RDF
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Parser (parse, parseShallow) where

import Text.Megaparsec hiding (parse, State)
import qualified Data.RDF as DR
import qualified Text.Megaparsec.Expr as TPE
import qualified Control.Monad as CM
import qualified Control.Monad.State as CMS
import qualified Control.Monad.Except as CME
import qualified Data.List as DL
import qualified Data.Text as DT
import qualified Data.Text.IO as DTO

import qualified Morloc.Error as ME
import qualified Morloc.State as MS
import qualified Morloc.Triple as M3
import qualified Morloc.Lexer as Tok

import qualified Morloc.Walker as MW

parse :: Maybe DT.Text -> DT.Text -> IO (ME.ThrowsError M3.RDF)
parse srcfile code = case (parseShallow srcfile code) of
  (Right rdf) -> joinRDF rdf (parseImports rdf)
  err         -> return err

joinRDF :: M3.RDF -> [IO (ME.ThrowsError M3.RDF)] -> IO (ME.ThrowsError M3.RDF)
joinRDF rdf xs
  = (fmap . fmap)                         -- raise to IO (ME.ThrowsError a)
      (foldl M3.rdfAppend rdf)            -- ([M3.RDF] -> M3.RDF)
      ((CM.liftM sequence . sequence) xs) -- IO (ME.ThrowsError [M3.RDF])

parseImports :: M3.RDF -> [IO (ME.ThrowsError M3.RDF)]
parseImports rdf = map morlocScriptFromFile (MW.getImportedFiles rdf)

morlocScriptFromFile :: DT.Text -> IO (ME.ThrowsError M3.RDF)
morlocScriptFromFile s = CM.join . fmap (parse (Just s)) . DTO.readFile $ DT.unpack s

-- | Parse a string of Morloc text into an AST. Catch lexical syntax errors.
parseShallow :: Maybe DT.Text -> DT.Text -> ME.ThrowsError M3.RDF
parseShallow srcfile code =
  case runParser (CMS.runStateT contents pstate) (DT.unpack input') code of
    Left err  -> CME.throwError $ ME.SyntaxError err
    Right ((M3.TopRDF _ val), s) -> return val
  where
    pstate = MS.ParserState { MS.stateCount = 0, MS.stateFile = srcfile}
    input' = case srcfile of
      Just s  -> s
      Nothing -> "<stdin>"

-- (>>) :: f a -> f b -> f a
-- (<*) :: f a -> (a -> f b) -> f a
contents :: MS.Parser M3.TopRDF
contents = do
  i <- MS.getId
  xs <- Tok.sc >> many top <* eof
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
      , M3.usp i "rdf:value" n
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
        , (M3.usp i "morloc:lang" lang)
      ] ++
        M3.adoptAs "morloc:import" i fs ++
        maybe [] (\p -> [M3.usp i "morloc:path" p]) path -- TODO: this really should be a URL
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
           [ M3.usp i "morloc:name" func
           , M3.usp i "morloc:alias" alias
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
  path <- Tok.stringLiteral
  qual <- optionMaybe (Tok.op "as" >> Tok.name)
  return $ M3.makeTopRDF i (
      [
        (M3.uss i "rdf:type" "morloc:import")
      , (M3.usp i "morloc:name" (path ++ ".loc"))
      ] ++ maybe [] (\q -> [M3.usp i "morloc:namespace" q]) qual
    )

restrictedImport :: MS.Parser M3.TopRDF
restrictedImport = do
  i <- MS.getId
  Tok.reserved "from"
  path <- Tok.stringLiteral
  Tok.reserved "import"
  functions <- Tok.parens (sepBy1 tripleName Tok.comma)
  return $ M3.makeTopRDF i (
      [
        M3.uss i "rdf:type" "morloc:restricted_import"
      , M3.usp i "morloc:name" (path ++ ".loc")
      ] ++ M3.adoptAs "morloc:import" i functions
    )

dataDeclaration :: MS.Parser M3.TopRDF
dataDeclaration = do
  i <- MS.getId
  lhs <- Tok.name
  bndvars <- many tripleName
  Tok.op "="
  rhs <- expression
  return $ M3.makeTopRDF i (
         [ M3.uss i "rdf:type" "morloc:dataDeclaration"
         , M3.usp i "morloc:lhs" lhs
         ]
      ++ M3.adoptAs "morloc:rhs" i [rhs]
      ++ M3.adopt i bndvars
    )

-- | function :: [input] -> output constraints
typeDeclaration :: MS.Parser M3.TopRDF
typeDeclaration = do
  i <- MS.getId
  lhs <- Tok.name
  lang <- option "Morloc" Tok.name
  Tok.op "::"
  properties <- option [] (try $ sepBy1 tripleName Tok.comma <* Tok.op "=>")
  rhs <- mtype
  constraints <- option [] (
      Tok.reserved "where" >>
      Tok.parens (sepBy1 booleanExpr Tok.comma)
    )
  return $ M3.makeTopRDF i (
         [ M3.uss i "rdf:type" "morloc:typeDeclaration"
         , M3.usp i "morloc:lhs" lhs
         , M3.usp i "morloc:lang" lang
         ]
      ++ M3.adoptAs "morloc:rhs" i [rhs]
      ++ M3.adoptAs "morloc:property" (M3.rdfId rhs) properties
      ++ M3.adoptAs "morloc:constraint" (M3.rdfId rhs) constraints
    )

listTag :: DR.Node -> Maybe DT.Text -> [M3.Triple]
listTag i "" = []
listTag i tag = [M3.usp i "morloc:label" t]

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
      ns <- some unambiguous'
      return $ M3.makeTopRDF i (
             [ M3.uss i "rdf:type" "morloc:parameterizedType"
             , M3.usp i "rdf:value" n]
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
      ns <- some unambiguous'
      return $ M3.makeTopRDF i (
             [ M3.uss i "rdf:type" "morloc:parameterizedGeneric"
             , M3.usp i "morloc:value" n
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
             [ M3.uss i "rdf:type" "morloc:atomicType"
             , M3.usp i "rdf:value" n]
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
             [ M3.uss i "rdf:type" "morloc:atomicGeneric"
             , M3.usp i "rdf:value" n]
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
             [ M3.uss i "rdf:type" "morloc:parameterizedType"
             , M3.usp i "rdf:value" "Tuple"]
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
             [ M3.uss i "rdf:type" "morloc:parameterizedType"
             , M3.usp i "rdf:value" "List"]
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
             [ M3.uss i "rdf:type" "morloc:parameterizedType"
             , M3.usp i "rdf:value" "Record"]
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
          , M3.usp i "morloc:key" n
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
    <|> try tripleNumber        -- 1.1, 42, -1e-100
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
            , M3.usp i "morloc:lhs" n
            ] ++ M3.adoptAs "morloc:rhs" i [t]
          )

expression :: MS.Parser M3.TopRDF
expression =
  -- currently this just handles "."
      try (TPE.makeExprParser functionTable term')
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
  arguments <- some term'
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
          [ M3.uss i "rdf:type" "morloc:name"
          , M3.usp i "rdf:value" x
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
      ns <- some argument'
      return $ M3.makeTopRDF i (
             [ M3.uss i "rdf:type" "morloc:call" ]
          ++ M3.adoptAs  "morloc:value" i [f]
          ++ M3.adopt i ns
        )

    identifier' :: MS.Parser M3.TopRDF
    identifier' = do
      i <- MS.getId
      n <- Tok.name
      return $ M3.makeTopRDF i
        [ M3.uss i "rdf:type" "morloc:name"
        , M3.usp i "rdf:value" n]

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
  =   TPE.makeExprParser arithmeticTable arithmeticTerm
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
      args <- some argument'
      return $ M3.makeTopRDF i (
             [M3.uss i "rdf:type" "morloc:call"]
          ++ M3.adoptAs "morloc:value" i [f]
          ++ M3.adopt i args
        )

    identifier' :: MS.Parser M3.TopRDF
    identifier' = do
      i <- MS.getId
      n <- Tok.name
      return $ M3.makeTopRDF i
        [ M3.uss i "rdf:type" "morloc:name"
        , M3.usp i "rdf:value" n]

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
          , M3.usp i "morloc:name" x
          ] ++ M3.adopt i ids
        )

arithmeticTable
  = [
      [ prefix "-" (unaryOp "Neg")
      , prefix "+" (unaryOp "Pos")
      ]
    , [ binaryR "^"  (binOp "Pow")
      ]
    , [ binaryL "*"  (binOp "Mul")
      , binaryL "/"  (binOp "Div")
      , binaryL "%"  (binOp "Mod")
      , binaryL "//" (binOp "Quo")
      ]
    , [ binaryL "+"  (binOp "Add")
      , binaryL "-"  (binOp "Sub")
      ]
  ]

unaryOp :: DT.Text -> DR.Node -> M3.TopRDF -> M3.TopRDF
unaryOp s i (M3.TopRDF j xs) = M3.makeTopRDF i (
     [ M3.uss i "rdf:type" "morloc:unaryOp"
     , M3.usp i "rdf:value" s
     , M3.usu j "rdf:_0" i
     ] ++ (DR.triplesOf xs)
  )

binOp :: DT.Text -> DR.Node -> M3.TopRDF -> M3.TopRDF -> M3.TopRDF
binOp s i (M3.TopRDF j xs) (M3.TopRDF k ys) = M3.makeTopRDF i (
     [ M3.uss i "rdf:type" "morloc:binop"
     , M3.usp i "rdf:value" s
     , M3.usu i "morloc:lhs" j
     , M3.usu i "morloc:rhs" k
     ] ++ (DR.triplesOf xs) ++ (DR.triplesOf ys)
  )

functionTable = [[ binaryR "."  exprComposition ]]

exprComposition :: DR.Node -> M3.TopRDF -> M3.TopRDF -> M3.TopRDF
exprComposition i (M3.TopRDF j xs) (M3.TopRDF k ys) = M3.makeTopRDF i (
     [ M3.uss i "rdf:type" "morloc:composition"
     , M3.usu i "morloc:lhs" j
     , M3.usu i "morloc:rhs" k
     ] ++ (DR.triplesOf xs) ++ (DR.triplesOf ys)
  )

binaryR name fun = TPE.InfixR (do {
    Tok.op name;
    i <- MS.getId;
    return (fun i);
  })

binaryL name fun = TPE.InfixL (do {
    Tok.op name;
    i <- MS.getId;
    return (fun i);
  })

prefix name fun = TPE.Prefix (do {
    Tok.op name;
    i <- MS.getId;
    return (fun i);
  })

triplePrimitive :: (a -> DR.Node) -> DT.Text -> MS.Parser a -> MS.Parser M3.TopRDF
triplePrimitive cast t p = do
  i <- MS.getId
  n <- p
  return $ M3.makeTopRDF i
    [ M3.uss i "rdf:type" t
    , DR.triple i (DR.UNode "rdf:value") (cast n)
    ]

tripleNumber        :: MS.Parser M3.TopRDF
tripleStringLiteral :: MS.Parser M3.TopRDF
tripleBool          :: MS.Parser M3.TopRDF
tripleName          :: MS.Parser M3.TopRDF

tnode :: DT.Text -> DT.Text -> DR.Node
tnode n t = DR.LNode (DR.TypedL n t)

tripleNumber = triplePrimitive
  (\n -> tnode (DT.pack . show $ n) "xsd:decimal")
  "morloc:number"
  Tok.number

tripleStringLiteral = triplePrimitive
  (\n -> tnode n  "xsd:string")
  "morloc:string"
  Tok.stringLiteral

tripleBool = triplePrimitive
  (\n -> tnode (DT.pack . show $ n) "xsd:boolean")
  "morloc:boolean"
  Tok.boolean

tripleName = triplePrimitive
  (\n -> DR.LNode . DR.PlainL $ n)
  "morloc:name"
  Tok.name
