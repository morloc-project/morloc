{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Module
Description : Parses a Morloc script into RDF
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Parser (parse) where

import Text.Megaparsec hiding (parse, State)
import qualified Text.Megaparsec.Expr as TPE
import qualified Text.Megaparsec.Char as TMC
import qualified Control.Monad as CM
import qualified Control.Monad.State as CMS

import Morloc.Global
import qualified Morloc.Language as ML
import qualified Morloc.Config as MC
import qualified Morloc.Monad as M
import qualified Morloc.Data.Text as MT
import qualified Morloc.State as MS
import qualified Morloc.Data.RDF as MR
import qualified Morloc.Lexer as Tok
import qualified Morloc.Module as MM
import Morloc.Operators

-- | Parse a Morloc source file into an AST stored as an RDF, recursively parse
-- all imported Morloc files. Catch lexical syntax errors.
parse
  :: Maybe Path -- ^ A filename associated with the source code (for debugging messages)
  -> MT.Text    -- ^ The Morloc source code
  -> MorlocMonad MR.RDF
parse srcfile code = do
  top' <- parseShallow srcfile code
  imports <- parseImports top'
  return $ (foldl MR.rdfAppend top') imports

parseShallow
  :: Maybe MT.Text -- ^ Source code file name
  -> MT.Text       -- ^ Source code
  -> MorlocMonad MR.RDF
parseShallow srcfile code = do
  source_str <- MC.makeLibSourceString srcfile
  let pstate = MS.ParserState {
                   MS.stateCount      = 0
                 , MS.stateSourceUri  = source_str
                 , MS.stateModulePath = srcfile
               }
  case runParser (CMS.runStateT contents pstate) (MT.unpack input') code of
    Left err  -> M.throwError $ SyntaxError err
    Right ((MR.TopRDF _ val), _) -> return val
  where
    input' = case srcfile of
      Just s  -> s
      Nothing -> "<stdin>"

parseImports :: MR.RDF -> MorlocMonad [MR.RDF]
parseImports rdf = do
  let importNames = MR.getImports rdf
  CM.mapM MM.findModule importNames >>= CM.mapM parseFile
  where
    parseFile :: Path -> MorlocMonad MR.RDF
    parseFile p = do
      code <- M.liftIO . MT.readFile . MT.unpack $ p
      parse (Just p) code

-- (>>) :: f a -> f b -> f a
-- (<*) :: f a -> (a -> f b) -> f a
contents :: MS.Parser MR.TopRDF
contents = do
  i <- MS.getId
  modulePath <- MS.getModulePath
  xs <- Tok.sc >> many top <* eof
  return $ MR.makeTopRDF i (
         [ MR.mtriple i PType OScript
         , MR.mtriple i PValue modulePath
         ]
      ++ MR.adopt i xs
    )

top :: MS.Parser MR.TopRDF 
top =
      try (export'    <* optional (Tok.op ";") )
  <|> try (source'    <* optional (Tok.op ";") )
  <|> try (statement' <*           Tok.op ";"  )
  <|> try (import'    <* optional (Tok.op ";") )
  <|> try (expression <*           Tok.op ";"  )
  <?> "Top. Maybe you are missing a semicolon?"

export' :: MS.Parser MR.TopRDF
export' = do
  Tok.reserved "export"
  i <- MS.getId
  n <- Tok.name
  return $ MR.makeTopRDF i ([
        MR.mtriple i PType OExport
      , MR.mtriple i PValue n
    ])

-- | parses a 'source' header, returning the language
source' :: MS.Parser MR.TopRDF
source' = do
  -- id for this source
  Tok.reserved "source"
  -- get the language of the imported source
  langStr <- Tok.stringLiteral
  -- get the path to the source file, if Nothing, then assume "vanilla"
  i <- MS.getId
  path <- optional (Tok.reserved "from" >> Tok.stringLiteral)
  -- get the function imports with with optional aliases
  fs <- Tok.parens (sepBy importAs' Tok.comma)
  -- the statement is unambiguous even without a semicolon
  optional (Tok.op ";")

  case ML.standardizeLangName langStr of
    (Just lang) ->
      return $ MR.makeTopRDF i ([
            MR.mtriple i PType OSource
          , MR.mtriple i PLang lang
        ] ++
          MR.adoptAs OImport i fs ++
          maybe [] (\p -> [MR.mtriple i PPath p]) path
      )
    Nothing -> fail ("Encountered unknown language: '" ++ MT.unpack langStr ++ "'")

  where
    importAs' :: MS.Parser MR.TopRDF
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
      return $ MR.makeTopRDF i (
           [ MR.mtriple i PName func
           , MR.mtriple i PAlias alias
           ]
        )

statement' :: MS.Parser MR.TopRDF
statement' =
      try typeDeclaration
  <|> try dataDeclaration

import' :: MS.Parser MR.TopRDF
import' =
      try restrictedImport
  <|> try simpleImport

simpleImport :: MS.Parser MR.TopRDF
simpleImport = do
  i <- MS.getId
  Tok.reserved "import"
  path <- Tok.stringLiteral
  qual <- optional (Tok.op "as" >> Tok.name)
  return $ MR.makeTopRDF i (
      [
        MR.mtriple i PType OImport
      , MR.mtriple i PName path
      ] ++ maybe [] (\q -> [MR.mtriple i PNamespace q]) qual
    )

restrictedImport :: MS.Parser MR.TopRDF
restrictedImport = do
  i <- MS.getId
  Tok.reserved "from"
  path <- Tok.stringLiteral
  Tok.reserved "import"
  functions <- Tok.parens (sepBy1 tripleName Tok.comma)
  return $ MR.makeTopRDF i (
      [
        MR.mtriple i PType ORestrictedImport
      , MR.mtriple i PName path
      ] ++ MR.adoptAs PImport i functions
    )

dataDeclaration :: MS.Parser MR.TopRDF
dataDeclaration = do
  i <- MS.getId
  lhs <- Tok.name
  bndvars <- many tripleName
  Tok.op "="
  rhs <- expression
  return $ MR.makeTopRDF i (
         [ MR.mtriple i PType ODataDeclaration
         , MR.mtriple i PLeft lhs
         ]
      ++ MR.adoptAs PRight i [rhs]
      ++ MR.adopt i bndvars
    )

-- | function :: [input] -> output constraints
typeDeclaration :: MS.Parser MR.TopRDF
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
  return $ MR.makeTopRDF i (
         [ MR.mtriple i PType OTypeDeclaration
         , MR.mtriple i PLeft lhs
         , MR.mtriple i PLang lang
         ]
      ++ MR.adoptAs PRight i [rhs]
      ++ MR.adoptAs PProperty (MR.rdfId rhs) properties
      ++ MR.adoptAs PConstraint (MR.rdfId rhs) constraints
    )

listTag :: MR.Node -> Maybe MT.Text -> [MR.Triple]
listTag _ Nothing    = []
listTag i (Just tag) = [MR.mtriple i PLabel tag]

mtype :: MS.Parser MR.TopRDF
mtype =
        try function' -- a [, *] -> b [where (*)]
    <|> try specific' -- A ...
    <|> try generic'  -- a ...
    <|> try record'   -- A { ... }
    <|> try unambiguous'
    <?> "type"
  where

    -- any type other than a naked function
    notFunction' :: MS.Parser MR.TopRDF
    notFunction' =
          try specific' -- A ...
      <|> try generic'  -- a ...
      <|> try record'   -- A { ... }
      <|> try unambiguous'
      <?> "type"

    unambiguous' :: MS.Parser MR.TopRDF
    unambiguous' =
            try empty' -- ()
        <|> try paren' -- (a)
        <|> try tuple' -- (a, ...)
        <|> list'      -- [a]
        <|> specific1  -- A
        <|> generic1   -- a

    -- <name> <type> <type> ...
    specific' :: MS.Parser MR.TopRDF
    specific' = do
      l <- Tok.tag Tok.genericType
      n <- Tok.specificType
      i <- MS.getId
      ns <- some unambiguous'
      return $ MR.makeTopRDF i (
             [ MR.mtriple i PType OParameterizedType
             , MR.mtriple i PType OType
             , MR.mtriple i PValue n]
          ++ (listTag i l)
          ++ MR.adopt i ns
        )

    -- Does parameterized generic even make sense?  Yes, say `f Int` where `f`
    -- is a generic collection of integers. Then you can map across it with any
    -- function of an Int.
    --
    -- <name> <type> <type> ...
    generic' :: MS.Parser MR.TopRDF
    generic' = do
      notFollowedBy (Tok.reserved "where")
      l <- Tok.tag Tok.genericType
      n <- Tok.genericType
      i <- MS.getId
      ns <- some unambiguous'
      return $ MR.makeTopRDF i (
             [ MR.mtriple i PType OParameterizedGenericType
             , MR.mtriple i PType OType
             , MR.mtriple i PValue n
             ]
          ++ listTag i l
          ++ MR.adopt i ns
        )

    -- <name> <type> <type> ...
    specific1 :: MS.Parser MR.TopRDF
    specific1 = do
      l <- Tok.tag Tok.specificType
      n <- Tok.specificType
      i <- MS.getId
      return $ MR.makeTopRDF i (
             [ MR.mtriple i PType OAtomicType
             , MR.mtriple i PType OType
             , MR.mtriple i PValue n
             ]
          ++ listTag i l
        )

    -- <name> <type> <type> ...
    generic1 :: MS.Parser MR.TopRDF
    generic1 = do
      notFollowedBy (Tok.reserved "where")
      l <- Tok.tag Tok.genericType
      n <- Tok.genericType
      i <- MS.getId
      return $ MR.makeTopRDF i (
             [ MR.mtriple i PType OAtomicGenericType
             , MR.mtriple i PType OType
             , MR.mtriple i PValue n]
          ++ (listTag i l)
        )

    empty' :: MS.Parser MR.TopRDF
    empty' = do
      Tok.op "("
      Tok.op ")"
      i <- MS.getId
      return $ MR.makeTopRDF i
        [ MR.mtriple i PType OEmptyType
        , MR.mtriple i PType OType
        ]

    paren' :: MS.Parser MR.TopRDF 
    paren' = Tok.parens mtype

    tuple' :: MS.Parser MR.TopRDF
    tuple' = Tok.parens $ do
      i <- MS.getId
      l <- Tok.tag (TMC.char '(')
      x <- mtype
      Tok.op ","
      xs <- sepBy1 mtype Tok.comma
      return $ MR.makeTopRDF i (
             [ MR.mtriple i PType OParameterizedType
             , MR.mtriple i PType OType
             , MR.mtriple i PValue ("Tuple" :: MT.Text)]
          ++ listTag i l
          ++ MR.adopt i (x:xs)
        )

    -- [ <type> ]
    list' :: MS.Parser MR.TopRDF
    list' = do
      i <- MS.getId
      l <- Tok.tag (TMC.char '[')
      s <- Tok.brackets mtype
      return $ MR.makeTopRDF i (
             [ MR.mtriple i PType OParameterizedType
             , MR.mtriple i PType OType
             , MR.mtriple i PValue ("List" :: MT.Text)]
          ++ listTag i l
          ++ MR.adopt i [s]
        )

    -- { <name> :: <type>, <name> :: <type>, ... }
    record' :: MS.Parser MR.TopRDF
    record' = do
      i <- MS.getId
      l <- Tok.tag (TMC.char '{')
      ns <- Tok.braces $ sepBy1 recordEntry' Tok.comma
      return $ MR.makeTopRDF i (
             [ MR.mtriple i PType OParameterizedType
             , MR.mtriple i PType OType
             , MR.mtriple i PValue ("Record" :: MT.Text)]
          ++ listTag i l
          ++ MR.adopt i ns
        )

    -- (<name> = <type>)
    recordEntry' :: MS.Parser MR.TopRDF
    recordEntry' = do
      i <- MS.getId
      n <- Tok.name
      Tok.op "::"
      t <- mtype
      return $ MR.makeTopRDF i (
          [ MR.mtriple i PType ONamedType
          , MR.mtriple i PKey n
          ] ++ MR.adoptAs PValue i [t]
        )

    function' :: MS.Parser MR.TopRDF
    function' = do
      i <- MS.getId
      inputs <- sepBy1 notFunction' Tok.comma
      Tok.op "->"
      output <- notFunction'
      return $ MR.makeTopRDF i (
             [ MR.mtriple i PType OFunctionType
             , MR.mtriple i PType OType
             ]
          ++ MR.adopt i inputs
          ++ MR.adoptAs POutput i [output]
        )


mdata :: MS.Parser MR.TopRDF
mdata =  do
        try tripleBool          -- True | False
    <|> try tripleNumber        -- scientific number of arbitrary precision
    <|> try tripleStringLiteral -- "yolo"
    <|> try list'               -- [ ...
    <|> try tuple'              -- ( ...
    <|> try record'             -- { ...
    <?> "literal data"
    where

      list' :: MS.Parser MR.TopRDF
      list' = do
        i <- MS.getId
        xs <- Tok.brackets (sepBy mdata Tok.comma)
        return $ MR.makeTopRDF i (
               [ MR.mtriple i PType OList 
               , MR.mtriple i PType OData]
            ++ MR.adopt i xs
          )

      tuple' = do
        i <- MS.getId
        xs <- Tok.parens tuple''
        return $ MR.makeTopRDF i (
               [ MR.mtriple i PType OTuple
               , MR.mtriple i PType OData]
            ++ MR.adopt i xs
          )

      record' = do
        i <- MS.getId
        xs <- Tok.braces (sepBy1 recordEntry' Tok.comma) 
        return $ MR.makeTopRDF i (
               [ MR.mtriple i PType ORecord
               , MR.mtriple i PType OData]
            ++ MR.adopt i xs
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
        return $ MR.makeTopRDF i (
            [ MR.mtriple i PType ORecordEntry
            , MR.mtriple i PLeft n
            ] ++ MR.adoptAs PRight i [t]
          )

expression :: MS.Parser MR.TopRDF
expression =
          try application -- must go first to allow, e.g. `(g . f) x`
      <|> try (Tok.parens expression)
      <|> try mdata
      <|> try tripleName

application :: MS.Parser MR.TopRDF
application = do
  i <- MS.getId
  function <- Tok.parens expression <|> identifier'
  arguments <- some term'
  return $ MR.makeTopRDF i (
         [MR.mtriple i PType OCall]
      ++ MR.adoptAs  PValue i [function]
      ++ MR.adopt i arguments
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
      return $ MR.makeTopRDF i (
          [ MR.mtriple i PType OName
          , MR.mtriple i PValue x
          ] ++ listTag i tag')

booleanExpr :: MS.Parser MR.TopRDF
booleanExpr = do
      try booleanBinOp
  <|> try relativeExpr
  <|> try not'
  <|> try (Tok.parens booleanExpr)
  <|> try call'
  <?> "an expression that reduces to True/False"
  where
    not' :: MS.Parser MR.TopRDF
    not' = do
      Tok.reserved "not"
      i <- MS.getId
      e <- booleanExpr
      return $ MR.makeTopRDF i (MR.adoptAs PNot i [e]) 

    call' :: MS.Parser MR.TopRDF
    call' = do
      i <- MS.getId
      f <- Tok.parens expression <|> identifier'
      ns <- some argument'
      return $ MR.makeTopRDF i (
             [ MR.mtriple i PType OCall ]
          ++ MR.adoptAs  PValue i [f]
          ++ MR.adopt i ns
        )

    identifier' :: MS.Parser MR.TopRDF
    identifier' = do
      i <- MS.getId
      n <- Tok.name
      return $ MR.makeTopRDF i
        [ MR.mtriple i PType OName
        , MR.mtriple i PValue n]

    argument' :: MS.Parser MR.TopRDF
    argument' =
          (Tok.parens booleanExpr)
      <|> try tripleBool
      <|> tripleName
      <?> "expect an argument"

booleanBinOp :: MS.Parser MR.TopRDF
booleanBinOp = do
  i <- MS.getId
  a <- bterm'
  op <- Tok.logicalBinOp
  b <- bterm'
  return $ binOp op i a b
  where
    bterm' =
            try tripleBool
        <|> tripleName
        <|> Tok.parens booleanExpr
        <?> "boolean expression"

relativeExpr :: MS.Parser MR.TopRDF
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

arithmeticExpr :: MS.Parser MR.TopRDF
arithmeticExpr
  =   TPE.makeExprParser arithmeticTerm arithmeticTable
  <?> "expression"

arithmeticTerm :: MS.Parser MR.TopRDF
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
      return $ MR.makeTopRDF i (
             [MR.mtriple i PType OCall]
          ++ MR.adoptAs PValue i [f]
          ++ MR.adopt i args
        )

    identifier' :: MS.Parser MR.TopRDF
    identifier' = do
      i <- MS.getId
      n <- Tok.name
      return $ MR.makeTopRDF i
        [ MR.mtriple i PType OName
        , MR.mtriple i PValue n]

    argument' :: MS.Parser MR.TopRDF
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
      return $ MR.makeTopRDF i (
          [ MR.mtriple i PType OAccess
          , MR.mtriple i PName x
          ] ++ MR.adopt i ids
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

unaryOp :: MT.Text -> MR.Node -> MR.TopRDF -> MR.TopRDF
unaryOp s i (MR.TopRDF j xs) = MR.makeTopRDF i (
     [ MR.mtriple i PType OUnaryOp
     , MR.mtriple i PValue s
     , MR.mtriple i (PElem 0) j
     ] ++ (MR.triplesOf xs)
  )

binOp :: MT.Text -> MR.Node -> MR.TopRDF -> MR.TopRDF -> MR.TopRDF
binOp s i (MR.TopRDF j xs) (MR.TopRDF k ys) = MR.makeTopRDF i (
     -- TODO: The `binop` classification should not appear in the Morloc.Data.RDF.
     -- These binary expression should resolve into perfectly normal functions.
     -- No special handling should be needed (except to integrate them into the
     -- manifolds).
     [ MR.mtriple i PType OBinOp
     , MR.mtriple i PValue s
     , MR.mtriple i PLeft j
     , MR.mtriple i PRight k
     ] ++ (MR.triplesOf xs) ++ (MR.triplesOf ys)
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

tripleName :: MS.Parser MR.TopRDF
tripleName = do
  i <- MS.getId
  n <- Tok.name
  return $ MR.makeTopRDF i
    [ MR.mtriple i PType OName
    , MR.mtriple i PValue n
    ]

triplePrimitive :: (a -> MR.Node) -> GraphObject -> MS.Parser a -> MS.Parser MR.TopRDF
triplePrimitive cast t p = do
  i <- MS.getId
  n <- p
  return $ MR.makeTopRDF i
    [ MR.mtriple i PType t
    , MR.mtriple i PType OData
    , MR.mtriple i PValue (cast n)
    ]

tripleNumber        :: MS.Parser MR.TopRDF
tripleStringLiteral :: MS.Parser MR.TopRDF
tripleBool          :: MS.Parser MR.TopRDF

tripleNumber = triplePrimitive
  asRdfNode
  ONumber
  Tok.number

tripleStringLiteral = triplePrimitive
  (\n -> MR.LNode (MR.TypedL n (MR.xsdPre <> "string")))
  OString
  Tok.stringLiteral

tripleBool = triplePrimitive
  asRdfNode
  OBoolean
  Tok.boolean
