{-|
Module      : Morloc.Parser.Parser
Description : Full parser for Morloc/Xi
Copyright   : (c) Zebulun Arendsee, 2019
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.Parser.Parser
  ( readProgram
  , readType
  ) where

import Data.Void (Void)
import Morloc.Namespace
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Control.Monad.State as CMS
import qualified Data.Map as Map
import qualified Data.Scientific as DS
import qualified Data.Set as Set
import qualified Morloc.Data.Text as MT
import qualified Morloc.Language as ML
import qualified Morloc.System as MS
import qualified Text.Megaparsec.Char.Lexer as L

type Parser a = CMS.StateT ParserState (Parsec Void MT.Text) a

data ParserState = ParserState {
    stateLang :: Maybe Lang
  , stateModulePath :: Maybe MT.Text
}

emptyState :: ParserState
emptyState = ParserState {stateLang = Nothing , stateModulePath = Nothing}

setLang :: Maybe Lang -> Parser ()
setLang lang = do
  s <- CMS.get
  CMS.put (s { stateLang = lang })

readProgram :: Maybe Path -> MT.Text -> [Module]
readProgram f sourceCode =
  case runParser
         (CMS.runStateT (sc >> pProgram <* eof) pstate)
         (maybe "<expr>" MT.unpack f)
         sourceCode of
    Left err -> error (show err)
    Right (es, _) -> es
  where
    pstate = ParserState {stateLang = Nothing , stateModulePath = f}

readType :: MT.Text -> Type
readType typeStr =
  case runParser (CMS.runStateT (pType <* eof) emptyState) "" typeStr of
    Left err -> error (show err)
    Right (es, _) -> es

many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  return (x : xs)

-- sc stands for space consumer
sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment = L.skipLineComment "--"
    blockComment = L.skipBlockComment "{-" "-}"

symbol = L.symbol sc

-- A lexer where space is consumed after every token (but not before)
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

number :: Parser DS.Scientific
number = lexeme $ L.signed sc L.scientific -- `empty` because no space is allowed

parens :: Parser a -> Parser a
parens p = lexeme $ between (symbol "(") (symbol ")") p

brackets :: Parser a -> Parser a
brackets p = lexeme $ between (symbol "[") (symbol "]") p

braces :: Parser a -> Parser a
braces p = lexeme $ between (symbol "{") (symbol "}") p

angles :: Parser a -> Parser a
angles p = lexeme $ between (symbol "<") (symbol ">") p

reservedWords :: [MT.Text]
reservedWords =
  [ "module"
  , "source"
  , "from"
  , "where"
  , "import"
  , "export"
  , "as"
  , "True"
  , "False"
  , "forall"
  ]

operatorChars :: String
operatorChars = ":!$%&*+./<=>?@\\^|-~#"

delimiter :: Parser ()
delimiter = many1 (symbol ";") >> return ()

op :: MT.Text -> Parser MT.Text
op o = (lexeme . try) (symbol o <* notFollowedBy (oneOf operatorChars))

reserved :: MT.Text -> Parser MT.Text
reserved w = try (symbol w)

stringLiteral :: Parser MT.Text
stringLiteral = do
  _ <- symbol "\""
  s <- many (noneOf ['"'])
  _ <- symbol "\""
  return $ MT.pack s

name :: Parser MT.Text
name = (lexeme . try) (p >>= check)
  where
    p = fmap MT.pack $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
    check x =
      if elem x reservedWords
        then failure Nothing Set.empty -- TODO: error message
        else return x

data Toplevel
  = TModule Module
  | TModuleBody ModuleBody

data ModuleBody
  = MBImport Import
  -- ^ module name, function name and optional alias
  | MBExport EVar
  | MBBody Expr

pProgram :: Parser [Module]
pProgram = do
  f <- CMS.gets stateModulePath
  -- allow ';' at the beginning (if you're into that sort of thing)
  optional delimiter
  es <- many pToplevel
  let mods = [m | (TModule m) <- es]
  case [e | (TModuleBody e) <- es] of
    [] -> return mods
    es' -> return $ makeModule f (MV "Main") es' : mods

pToplevel :: Parser Toplevel
pToplevel =
  try (fmap TModule pModule <* optional delimiter) <|>
  fmap TModuleBody (pModuleBody <* optional delimiter)

pModule :: Parser Module
pModule = do
  f <- CMS.gets stateModulePath
  _ <- reserved "module"
  moduleName' <- name
  mes <- braces (optional delimiter >> many1 pModuleBody)
  return $ makeModule f (MV moduleName') mes

makeModule :: Maybe Path -> MVar -> [ModuleBody] -> Module
makeModule f n mes =
  Module
    { moduleName = n
    , modulePath = f
    , moduleImports = imports'
    , moduleExports = exports'
    , moduleBody = body'
    , moduleTypeMap = Map.empty
    }
  where
    imports' = [x | (MBImport x) <- mes]
    exports' = [x | (MBExport x) <- mes]
    body' = [x | (MBBody x) <- mes]

pModuleBody :: Parser ModuleBody
pModuleBody =
        try pImport <* optional delimiter 
    <|> try pExport <* optional delimiter 
    <|> try pStatement' <* optional delimiter
    <|> pExpr' <* optional delimiter
  where
    pStatement' = fmap MBBody pStatement
    pExpr' = fmap MBBody pExpr

pImport :: Parser ModuleBody
pImport = do
  _ <- reserved "import"
  n <- name
  imports <-
    optional $
    parens (sepBy pImportTerm (symbol ",")) <|> fmap (\x -> [(EV x, EV x)]) name
  return . MBImport $
    Import
      { importModuleName = MV n
      , importInclude = imports
      , importExclude = []
      , importNamespace = Nothing
      }

pImportTerm :: Parser (EVar, EVar)
pImportTerm = do
  n <- name
  a <- option n (reserved "as" >> name)
  return (EV n, EV a)

pExport :: Parser ModuleBody
pExport = fmap (MBExport . EV) $ reserved "export" >> name

pStatement :: Parser Expr
pStatement = try pDeclaration <|> pSignature

pDeclaration :: Parser Expr
pDeclaration = try pFunctionDeclaration <|> pDataDeclaration

pDataDeclaration :: Parser Expr
pDataDeclaration = do
  v <- name
  _ <- symbol "="
  e <- pExpr
  return (Declaration (EV v) e)

pFunctionDeclaration :: Parser Expr
pFunctionDeclaration = do
  v <- name
  args <- many1 name
  _ <- op "="
  e <- pExpr
  return $ Declaration (EV v) (curryLamE (map EV args) e)
  where
    curryLamE [] e' = e'
    curryLamE (v:vs') e' = LamE v (curryLamE vs' e')

pSignature :: Parser Expr
pSignature = do
  v <- name
  lang <- optional pLang
  setLang lang
  _ <- op "::"
  props <- option [] (try pPropertyList)
  t <- pType
  constraints <-
    option [] $ reserved "where" >> braces (sepBy pConstraint (symbol ";"))
  setLang Nothing
  return $
    Signature
      (EV v)
      (EType
         { etype = t
         , eprop = Set.fromList props
         , econs = Set.fromList constraints
         , esource = Nothing
         })

pLang :: Parser Lang
pLang = do
  langStr <- name
  case ML.readLangName langStr of
    (Just lang) -> return lang
    Nothing -> fancyFailure . Set.singleton . ErrorFail
      $ "Langage '" <> MT.unpack langStr <> "' is not supported"
 
-- | match an optional tag that precedes some construction
tag :: Parser a -> Parser (Maybe MT.Text)
tag p = optional (try tag')
  where
    tag' = do
      l <- name
      _ <- op ":"
      _ <- lookAhead p
      return l

pPropertyList :: Parser [Property]
pPropertyList =
  (parens (sepBy1 pProperty (symbol ",")) <|> sepBy1 pProperty (symbol ",")) <*
  op "=>"

pProperty :: Parser Property
pProperty = do
  ps <- many1 name
  case ps of
    ["packs"] -> return Pack
    ["unpacks"] -> return Unpack
    ["casts"] -> return Cast
    _ -> return (GeneralProperty ps)

pConstraint :: Parser Constraint
pConstraint = fmap (Con . MT.pack) (many (noneOf ['{', '}']))

pExpr :: Parser Expr
pExpr =
      try pRecordE
  <|> try pTuple
  <|> try pUni
  <|> try pAnn
  <|> try pApp
  <|> try pStrE
  <|> try pLogE
  <|> try pNumE
  <|> try pSrcE
  <|> pListE
  <|> parens pExpr
  <|> pLam
  <|> pVar

-- source "c" from "foo.c" ("Foo" as foo, "bar")
-- source "R" ("sqrt", "t.test" as t_test)
pSrcE :: Parser Expr
pSrcE = do
  f <- CMS.gets stateModulePath
  reserved "source"
  language <- pLang
  srcfile <- optional (reserved "from" >> stringLiteral)
  rs <- parens (sepBy1 pImportSourceTerm (symbol ","))
  return $ SrcE language (MS.combine <$> fmap MS.takeDirectory f <*> srcfile) rs

pImportSourceTerm :: Parser (EVar, EVar)
pImportSourceTerm = do
  n <- stringLiteral
  a <- option n (reserved "as" >> name)
  return (EV n, EV a)

pRecordE :: Parser Expr
pRecordE = fmap RecE $ braces (sepBy1 pRecordEntryE (symbol ","))

pRecordEntryE :: Parser (EVar, Expr)
pRecordEntryE = do
  n <- name
  _ <- symbol "="
  e <- pExpr
  return (EV n, e)

pListE :: Parser Expr
pListE = fmap ListE $ brackets (sepBy pExpr (symbol ","))

pTuple :: Parser Expr
pTuple = do
  _ <- op "("
  e <- pExpr
  _ <- op ","
  es <- sepBy1 pExpr (op ",")
  _ <- op ")"
  return (TupleE (e : es))

pUni :: Parser Expr
pUni = symbol "Null" >> return UniE

pAnn :: Parser Expr
pAnn = do
  e <-
    parens pExpr <|> pVar <|> pListE <|> try pNumE <|> pLogE <|> pStrE
  _ <- op "::"
  t <- pType
  return $ AnnE e [t]

pApp :: Parser Expr
pApp = do
  f <- parens pExpr <|> pVar
  (e:es) <- many1 s
  return $ foldl AppE (AppE f e) es
  where
    s =   try pAnn
      <|> try (parens pExpr)
      <|> try pUni
      <|> try pStrE
      <|> try pLogE
      <|> try pNumE
      <|> pListE
      <|> pTuple
      <|> pRecordE
      <|> pVar

pLogE :: Parser Expr
pLogE = pTrue <|> pFalse
  where
    pTrue = reserved "True" >> return (LogE True)
    pFalse = reserved "False" >> return (LogE False)

pStrE :: Parser Expr
pStrE = fmap StrE stringLiteral

pNumE :: Parser Expr
pNumE = fmap NumE number

pLam :: Parser Expr
pLam = do
  _ <- symbol "\\"
  vs <- many1 pEVar
  _ <- symbol "->"
  e <- pExpr
  return (curryLamE vs e)
  where
    curryLamE [] e' = e'
    curryLamE (v:vs') e' = LamE v (curryLamE vs' e')

pVar :: Parser Expr
pVar = fmap VarE pEVar

pEVar :: Parser EVar
pEVar = fmap EV name

pType :: Parser Type
pType =
      pExistential
  <|> try pUniT
  <|> try pRecordT
  <|> try pForAllT
  <|> try pFunT
  <|> try pArrT
  <|> try parensType
  <|> pListT
  <|> pTupleT
  <|> pVarT

pUniT :: Parser Type
pUniT = do
  lang <- CMS.gets stateLang
  _ <- symbol "("
  _ <- symbol ")"
  return (VarT (TV lang "Unit"))

parensType :: Parser Type
parensType = do
  _ <- tag (symbol "(")
  t <- parens pType
  return t

pTupleT :: Parser Type
pTupleT = do
  _ <- tag (symbol "(")
  ts <- parens (sepBy1 pType (symbol ","))
  let v = TV Nothing . MT.pack $ "Tuple" ++ (show (length ts))
  return $ ArrT v ts

pRecordT :: Parser Type
pRecordT = do
  _ <- tag (symbol "{")
  record <- braces (sepBy1 pRecordEntryT (symbol ","))
  return $ RecT record

pRecordEntryT :: Parser (TVar, Type)
pRecordEntryT = do
  n <- name
  _ <- op "::"
  t <- pType
  return (TV Nothing n, t)

pExistential :: Parser Type
pExistential = do
  v <- angles name
  return (ExistT (TV Nothing v) [])

pArrT :: Parser Type
pArrT = do
  lang <- CMS.gets stateLang
  _ <- tag (name <|> stringLiteral)
  n <- name <|> stringLiteral
  args <- many1 pType'
  return $ ArrT (TV lang n) args
  where
    pType' = try parensType <|> try pUniT <|> pVarT <|> pListT <|> pTupleT <|> pRecordT

pFunT :: Parser Type
pFunT = do
  t <- pType'
  _ <- op "->"
  ts <- sepBy1 pType' (op "->")
  return $ foldr1 FunT (t : ts)
  where
    pType' = try parensType <|> try pUniT <|> try pArrT <|> pVarT <|> pListT <|> pTupleT <|> pRecordT

pListT :: Parser Type
pListT = do
  _ <- tag (symbol "[")
  t <- brackets pType
  return $ ArrT (TV Nothing "List") [t]

pVarT :: Parser Type
pVarT = do
  lang <- CMS.gets stateLang
  _ <- tag (name <|> stringLiteral)
  n <- name <|> stringLiteral
  return $ VarT (TV lang n)

pForAllT :: Parser Type
pForAllT = do
  lang <- CMS.gets stateLang
  _ <- reserved "forall"
  vs <- many1 name
  _ <- op "."
  t <- pType
  return (curryForall lang vs t)
  where
    curryForall _ [] e' = e'
    curryForall lang' (v:vs') e' = Forall (TV lang' v) (curryForall lang' vs' e')
