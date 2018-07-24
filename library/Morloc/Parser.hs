module Morloc.Parser (morlocScript) where

import Text.Parsec hiding (State, Parser)
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
    Right val -> return val

-- (>>) :: f a -> f b -> f a
-- (<*) :: f a -> (a -> f b) -> f a
contents :: MS.Parser M3.RDF
contents = do
  let i = 0
  xs <- Tok.whiteSpace >> many top <* eof
  return $ M3.RDF 0 (
         [(0, ":isa", M3.Str' ":script")]
      ++ M3.adoptAs ":child" 0 xs
    )

top :: MS.Parser M3.RDF 
top =
      try (source'    <* optional (Tok.op ";") )
  <|> try (statement' <*           Tok.op ";"  )
  <|> try (import'    <* optional (Tok.op ";") )
  <|> try (expression <*           Tok.op ";"  )
  <?> "Top. Maybe you are missing a semicolon?"

-- | parses a 'source' header, returning the language
source' :: MS.Parser M3.RDF
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

  return $ M3.RDF i ([
          (i, ":isa",  M3.Str' ":source")
        , (i, ":lang", M3.Str' lang)
      ] ++
        M3.adoptAs ":import" i fs ++
        maybe [] (\p -> [(i, ":path", M3.Str' p)]) path
    )

  where
    importAs' :: MS.Parser M3.RDF
    importAs' = do
      i <- MS.getId
      -- quoting the function names allows them to have more arbitrary values,
      -- perhaps even including expressions that return functions (though this
      -- is probably bad practice).
      func <- Tok.stringLiteral
      -- the alias is especially important when the native function name is not
      -- legal Morloc syntax, for example an R function with a '.' in the name.
      alias <- optionMaybe (Tok.reserved "as" >> Tok.name)
      return $ M3.RDF i (
           [(i, ":name", M3.Str' func)] ++
            maybe [] (\x -> [(i, ":alias", M3.Str' x)]) alias
        )

statement' :: MS.Parser M3.RDF
statement' =
      try typeDeclaration
  <|> try dataDeclaration

import' :: MS.Parser M3.RDF
import' =
      try restrictedImport
  <|> try simpleImport

simpleImport :: MS.Parser M3.RDF
simpleImport = do
  i <- MS.getId
  Tok.reserved "import"
  path <- Tok.path
  qual <- optionMaybe (Tok.op "as" >> Tok.name)
  return $ M3.RDF i (
      [
        (i, ":isa", M3.Str' ":import")
      , (i, ":name", M3.Str' (DL.intercalate "." path))
      ] ++ maybe [] (\q -> [(i, ":namespace", M3.Str' q)]) qual
    )

restrictedImport :: MS.Parser M3.RDF
restrictedImport = do
  i <- MS.getId
  Tok.reserved "from"
  path <- Tok.path
  Tok.reserved "import"
  functions <- Tok.parens (sepBy1 tripleName Tok.comma)
  return $ M3.RDF i (
      [
        (i, ":isa", M3.Str' ":restricted_import")
      , (i, ":name", M3.Str' (DL.intercalate "." path))
      ] ++ M3.adoptAs ":import" i functions
    )

dataDeclaration :: MS.Parser M3.RDF
dataDeclaration = do
  i <- MS.getId
  lhs <- tripleName 
  bndvars <- many tripleName
  Tok.op "="
  rhs <- expression
  return $ M3.RDF i (
         [(i, ":isa", M3.Str' ":dataDeclaration")]
      ++ M3.adoptAs ":lhs"       i [lhs]
      ++ M3.adoptAs ":parameter" i bndvars
      ++ M3.adoptAs ":rhs"       i [rhs]
    )

-- | function :: [input] -> output constraints
typeDeclaration :: MS.Parser M3.RDF
typeDeclaration = do
  i <- MS.getId
  lhs <- tripleName
  Tok.op "::"
  rhs <- mtype
  constraints <- option [] (
      Tok.reserved "where" >>
      Tok.parens (sepBy1 booleanExpr Tok.comma)
    )
  return $ M3.RDF i (
         [(i, ":isa", M3.Str' ":typeDeclaration")]
      ++ M3.adoptAs ":lhs" i [lhs]
      ++ M3.adoptAs ":rhs" i [rhs]
      ++ M3.adoptAs ":constraint" (M3.rdfId rhs) constraints
    )

listTag :: M3.Subject -> Maybe String -> [(M3.Subject, M3.Relation, M3.Object)] 
listTag i tag = maybe [] (\t -> [(i, ":label", M3.Str' t)]) tag

mtype :: MS.Parser M3.RDF
mtype =
        try function' -- a [, *] -> b [where (*)]
    <|> try specific' -- A ...
    <|> try generic'  -- a ...
    <|> try record'   -- A { ... }
    <|> try unambiguous'
    <?> "type"
  where

    -- any type other than a naked function
    notFunction' :: MS.Parser M3.RDF
    notFunction' =
          try specific' -- A ...
      <|> try generic'  -- a ...
      <|> try record'   -- A { ... }
      <|> try unambiguous'
      <?> "type"

    unambiguous' :: MS.Parser M3.RDF
    unambiguous' =
            try empty' -- ()
        <|> try paren' -- (a)
        <|> try tuple' -- (a, ...)
        <|> list'      -- [a]
        <|> specific1  -- A
        <|> generic1   -- a

    -- <name> <type> <type> ...
    specific' :: MS.Parser M3.RDF
    specific' = do
      l <- Tok.tag Tok.genericType
      n <- Tok.specificType
      i <- MS.getId
      ns <- many1 unambiguous'
      return $ M3.RDF i (
             [(i, ":isa", M3.Str' ":parameterizedType"), (i, ":value", M3.Str' n)]
          ++ (listTag i l)
          ++ M3.adoptAs ":parameter" i ns
        )

    -- Does parameterized generic even make sense?  Yes, say `f Int` where `f`
    -- is a generic collection of integers. Then you can map across it with any
    -- function of an Int.
    --
    -- <name> <type> <type> ...
    generic' :: MS.Parser M3.RDF
    generic' = do
      notFollowedBy (Tok.reserved "where")
      l <- Tok.tag Tok.genericType
      n <- Tok.genericType
      i <- MS.getId
      ns <- many1 unambiguous'
      return $ M3.RDF i (
             [(i, ":isa", M3.Str' ":parameterizedGeneric"), (i, ":value", M3.Str' n)]
          ++ (listTag i l)
          ++ (M3.adoptAs ":parameter" i ns)
        )

    -- <name> <type> <type> ...
    specific1 :: MS.Parser M3.RDF
    specific1 = do
      l <- Tok.tag Tok.specificType
      n <- Tok.specificType
      i <- MS.getId
      return $ M3.RDF i (
             [(i, ":isa", M3.Str' ":atomicType"), (i, ":value", M3.Str' n)]
          ++ listTag i l
        )

    -- <name> <type> <type> ...
    generic1 :: MS.Parser M3.RDF
    generic1 = do
      notFollowedBy (Tok.reserved "where")
      l <- Tok.tag Tok.genericType
      n <- Tok.genericType
      i <- MS.getId
      return $ M3.RDF i (
             [(i, ":isa", M3.Str' ":atomicGeneric"), (i, ":value", M3.Str' n)]
          ++ (listTag i l)
        )

    empty' :: MS.Parser M3.RDF
    empty' = do
      Tok.op "("
      Tok.op ")"
      i <- MS.getId
      return $ M3.RDF i [(i, ":isa", M3.Str' ":empty")]

    paren' :: MS.Parser M3.RDF 
    paren' = Tok.parens mtype

    tuple' :: MS.Parser M3.RDF
    tuple' = Tok.parens $ do
      i <- MS.getId
      l <- Tok.tag (char '(')
      x <- mtype
      Tok.op ","
      xs <- sepBy1 mtype Tok.comma
      return $ M3.RDF i (
             [(i, ":isa", M3.Str' ":parameterizedType"), (i, ":value", M3.Str' "Tuple")]
          ++ listTag i l
          ++ M3.adoptAs ":parameter" i (x:xs)
        )

    -- [ <type> ]
    list' :: MS.Parser M3.RDF
    list' = do
      i <- MS.getId
      l <- Tok.tag (char '[')
      s <- Tok.brackets mtype
      return $ M3.RDF i (
             [(i, ":isa", M3.Str' ":parameterizedType"), (i, ":value", M3.Str' "List")]
          ++ listTag i l
          ++ M3.adoptAs ":parameter" i [s]
        )

    -- { <name> :: <type>, <name> :: <type>, ... }
    record' :: MS.Parser M3.RDF
    record' = do
      i <- MS.getId
      l <- Tok.tag (char '{')
      ns <- Tok.braces $ sepBy1 recordEntry' Tok.comma
      return $ M3.RDF i (
             [ (i, ":isa", M3.Str' ":parameterizedType"), (i, ":value", M3.Str' "Record")]
          ++ listTag i l
          ++ M3.adoptAs ":parameter" i ns
        )

    -- (<name> = <type>)
    recordEntry' :: MS.Parser M3.RDF
    recordEntry' = do
      i <- MS.getId
      n <- Tok.name
      Tok.op "::"
      t <- mtype
      return $ M3.RDF i (
          [ (i, ":isa", M3.Str' ":namedType")
          , (i, ":name", M3.Str' n)
          ] ++ M3.adoptAs ":value" i [t]
        )

    function' :: MS.Parser M3.RDF
    function' = do
      i <- MS.getId
      inputs <- sepBy1 notFunction' Tok.comma
      Tok.op "->"
      output <- notFunction'
      return $ M3.RDF i (
             [(i, ":isa", M3.Str' ":functionType")]
          ++ M3.adoptAs ":input" i inputs
          ++ M3.adoptAs ":output" i [output]
        )


mdata :: MS.Parser M3.RDF
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

      list' :: MS.Parser M3.RDF
      list' = do
        i <- MS.getId
        xs <- Tok.brackets (sepBy mdata Tok.comma)
        return $ M3.RDF i (
               [(i, ":isa", M3.Str' ":list")]
            ++ M3.adoptAs ":contains" i xs
          )

      tuple' = do
        i <- MS.getId
        xs <- Tok.parens tuple''
        return $ M3.RDF i (
               [(i, ":isa", M3.Str' ":tuple")]
            ++ M3.adoptAs ":contains" i xs
          )

      record' = do
        i <- MS.getId
        xs <- Tok.braces (sepBy1 recordEntry' Tok.comma) 
        return $ M3.RDF i (
               [(i, ":isa", M3.Str' ":record")]
            ++ M3.adoptAs ":contains" i xs
          )

      -- must have at least two elements
      tuple'' = do
        x <- mdata
        Tok.comma
        xs <- sepBy1 mdata Tok.comma
        return $ x:xs

      -- parse a tag/value pair
      recordEntry' = do
        i <- MS.getId
        n <- Tok.name
        Tok.op "="
        t <- mdata
        return $ M3.RDF i (
            [ (i, ":isa", M3.Str' "recordEntry") 
            , (i, ":lhs", M3.Str' n)
            ] ++ M3.adoptAs ":rhs" i [t]
          )

expression :: MS.Parser M3.RDF
expression =
  -- currently this just handles "."
      try (TPE.buildExpressionParser functionTable term')
  <|> term'
  <?> "an expression"
  where
    term' :: MS.Parser M3.RDF
    term' =
          try (Tok.parens expression)
      <|> try application
      <|> try mdata
      <|> try tripleName

application :: MS.Parser M3.RDF
application = do
  i <- MS.getId
  function <- Tok.parens expression <|> identifier'
  arguments <- many1 term'
  return $ M3.RDF i (
         [ (i, ":isa", M3.Str' ":call")]
      ++ M3.adoptAs ":value" i [function]
      ++ M3.adoptAs ":argument" i arguments
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
      return $ M3.RDF i (
          [ (i, ":isa", M3.Str' ":name")
          , (i, ":value", M3.Str' x)
          ] ++ listTag i tag')

booleanExpr :: MS.Parser M3.RDF
booleanExpr = do
      try booleanBinOp
  <|> try relativeExpr
  <|> try not'
  <|> try (Tok.parens booleanExpr)
  <|> try call'
  <?> "an expression that reduces to True/False"
  where
    not' :: MS.Parser M3.RDF
    not' = do
      Tok.reserved "not"
      i <- MS.getId
      e <- booleanExpr
      return $ M3.RDF i (M3.adoptAs ":not" i [e]) 

    call' :: MS.Parser M3.RDF
    call' = do
      i <- MS.getId
      n <- Tok.name
      ns <- many1 argument'
      return $ M3.RDF i (
             [(i, ":isa", M3.Str' ":call"), (i, ":name", M3.Str' n)]
          ++ M3.adoptAs ":argument" i ns
        )

    argument' :: MS.Parser M3.RDF
    argument' =
          (Tok.parens booleanExpr)
      <|> try tripleBool
      <|> tripleName
      <?> "expect an argument"

booleanBinOp :: MS.Parser M3.RDF
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

relativeExpr :: MS.Parser M3.RDF
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

arithmeticExpr :: MS.Parser M3.RDF
arithmeticExpr
  =   TPE.buildExpressionParser arithmeticTable arithmeticTerm
  <?> "expression"

arithmeticTerm :: MS.Parser M3.RDF
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
      x <- Tok.name
      xs <- many1 argument'
      return $ M3.RDF i (
          [ (i, ":isa", M3.Str' ":call")
          , (i, ":name", M3.Str' x)
          ] ++ M3.adoptAs ":argument" i xs
        )

    argument' :: MS.Parser M3.RDF
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
      return $ M3.RDF i (
          [ (i, ":isa", M3.Str' ":access")
          , (i, ":name", M3.Str' x)
          ] ++ M3.adoptAs ":child" i ids
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

unaryOp :: String -> M3.Subject -> M3.RDF -> M3.RDF
unaryOp s i (M3.RDF j xs) = M3.RDF i (
     [ (i, ":isa", M3.Str' s) 
     , (i, ":contains", M3.Id' j)
     ] ++ xs
  )

binOp :: String -> M3.Subject -> M3.RDF -> M3.RDF -> M3.RDF
binOp s i (M3.RDF j xs) (M3.RDF k ys) = M3.RDF i (
     [ (i, ":isa", M3.Str' ":binop") 
     , (i, ":value", M3.Str' s)
     , (i, ":lhs", M3.Id' j)
     , (i, ":rhs", M3.Id' k)
     ] ++ xs ++ ys
  )

functionTable = [[ binary "."  exprComposition TPE.AssocRight ]]

exprComposition :: M3.Subject -> M3.RDF -> M3.RDF -> M3.RDF
exprComposition i (M3.RDF j xs) (M3.RDF k ys) = M3.RDF i (
     [ (i, ":isa", M3.Str' ":composition") 
     , (i, ":lhs", M3.Id' j)
     , (i, ":rhs", M3.Id' k)
     ] ++ xs ++ ys
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

triplePrimitive :: String -> MS.Parser a -> (a -> M3.Object) -> MS.Parser M3.RDF
triplePrimitive isa p f = do
  i <- MS.getId
  n <- p
  return $ M3.RDF i [(i, ":isa", M3.Str' isa), (i, ":value", f n)]

tripleInteger       :: MS.Parser M3.RDF
tripleFloat         :: MS.Parser M3.RDF
tripleName          :: MS.Parser M3.RDF
tripleStringLiteral :: MS.Parser M3.RDF
tripleBool          :: MS.Parser M3.RDF

tripleInteger       = triplePrimitive ":integer" Tok.integer       M3.Int'
tripleFloat         = triplePrimitive ":number"  Tok.float         M3.Num'
tripleName          = triplePrimitive ":name"    Tok.name          M3.Str'
tripleStringLiteral = triplePrimitive ":string"  Tok.stringLiteral M3.Str'
tripleBool          = triplePrimitive ":boolean" Tok.boolean       M3.Log'
