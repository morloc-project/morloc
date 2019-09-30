{-|
Module      : Morloc.Parser.Parser
Description : Full parser for Morloc/Xi
Copyright   : (c) Zebulun Arendsee, 2019
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Parser.Parser (readProgram, readType) where

import Morloc.Global (Path)
import Morloc.Operators
import Morloc.TypeChecker.Namespace
import qualified Morloc.Data.Text as MT

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Map as Map
import qualified Data.Scientific as DS
import qualified Data.Set as Set
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void MT.Text

readProgram :: Maybe Path -> MT.Text -> [Module]
readProgram f s = case parse (sc >> pProgram f <* eof) (maybe "<expr>" MT.unpack f) s of 
      Left err -> error (show err)
      Right es -> es

many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  return (x:xs)

-- sc stands for space consumer
sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment  = L.skipLineComment "--"
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
reservedWords = ["module", "source", "from", "where", "import",
                 "export", "as", "True", "False"]

operatorChars :: String
operatorChars = ":!$%&*+./<=>?@\\^|-~#"

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
name = (lexeme . try) (p >>= check) where
  p       = fmap MT.pack $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
  check x = if elem x reservedWords
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

pProgram :: Maybe Path -> Parser [Module]
pProgram f = do
  es <- many (pToplevel f)
  let mods = [m | (TModule m) <- es]
  case [e | (TModuleBody e) <- es] of
    [] -> return mods
    es' -> return $ makeModule f (MV "Main") es' : mods

pToplevel :: Maybe Path -> Parser Toplevel
pToplevel f =   try (fmap TModule (pModule f) <* optional (symbol ";"))
            <|> fmap TModuleBody (pModuleBody <* optional (symbol ";"))

pModule :: Maybe Path -> Parser Module
pModule f = do
  _ <- reserved "module"
  moduleName' <- name
  mes <- braces ( many1 pModuleBody)
  return $ makeModule f (MV moduleName') mes

makeModule :: Maybe Path -> MVar -> [ModuleBody] -> Module
makeModule f n mes = Module {
      moduleName = n
    , modulePath = f
    , moduleImports = imports'
    , moduleExports = exports'
    , moduleBody = body'
  } where
  imports' = [x | (MBImport x) <- mes]
  exports' = [x | (MBExport x) <- mes]
  body' = [x | (MBBody x) <- mes]

pModuleBody :: Parser ModuleBody
pModuleBody
  =   try pImport <* optional (symbol ";")
  <|> try pExport <* optional (symbol ";")
  <|> try pStatement' <* optional (symbol ";")
  <|> pExpr' <* optional (symbol ";")
  where
    pStatement' = fmap MBBody pStatement
    pExpr' = fmap MBBody pExpr

pImport :: Parser ModuleBody
pImport = do
  _ <- reserved "import"
  n <- name
  imports <- optional $ parens (sepBy pImportTerm (symbol ","))
          <|> fmap (\x -> [(EV x, EV x)]) name
  return . MBImport $ Import
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
  _ <- symbol "="
  e <- pExpr
  return $ Declaration (EV v) (curryLamE (map EV args) e)
  where
    curryLamE [] e' = e'
    curryLamE (v:vs') e' = LamE v (curryLamE vs' e') 

pSignature :: Parser Expr
pSignature = do
  v <- name
  lang <- optional (name)
  _ <- symbol "::"
  props <- option [] (try $ sepBy1 pProperty (symbol ",") <* symbol "=>")
  t <- pType
  constraints <- option [] $ reserved "where" >> parens (sepBy1 pConstraint (symbol ","))
  return $ Signature (EV v) (EType
    { etype = t
    , elang = lang
    , eprop = Set.fromList props
    , econs = Set.fromList constraints
    , esource = Nothing
    })

-- | match an optional tag that precedes some construction
tag :: Parser a -> Parser (Maybe MT.Text)
tag p =
  optional (try tag')
  where
    tag' = do
      l <- name
      _ <- op ":"
      _ <- lookAhead p
      return l

pProperty :: Parser Property
pProperty = do 
  ps <- many1 name
  case ps of
    ["pack"] -> return Pack
    ["unpack"] -> return Unpack
    ["cast"] -> return Cast
    _ -> return (GeneralProperty ps)

pConstraint :: Parser Constraint
pConstraint = fmap (Con . MT.pack) (many1 (noneOf [',']))

readType :: MT.Text -> Type
readType s = case parse (pType <* eof) "" s of 
  Left err -> error (show err)
  Right t -> t 

pExpr :: Parser Expr
pExpr
  =   try pRecordE
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
  reserved "source"
  language <- stringLiteral 
  srcfile <- optional (reserved "from" >> stringLiteral)
  rs <- parens (sepBy1 pImportSourceTerm (symbol ","))
  return $ SrcE language srcfile rs

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
  _ <- symbol "("
  e <- pExpr
  _ <- symbol ","
  es <- sepBy1 pExpr (symbol ",")
  _ <- symbol ")"
  return (TupleE (e:es))

pUni :: Parser Expr
pUni = symbol "UNIT" >> return UniE

pAnn :: Parser Expr
pAnn = do
  e <- parens pExpr <|> pVar <|> pListE <|> try pNumE <|> pLogE <|> pStrE
  _ <- symbol "::"
  t <- pType
  return $ AnnE e t

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
pType
  =   pExistential
  <|> try pUniT
  <|> try pRecordT
  <|> try pForAllT
  <|> try pArrT
  <|> try pFunT
  <|> try (parens pType)
  <|> pListT
  <|> pTupleT
  <|> pVarT

pUniT :: Parser Type
pUniT = do
  _ <- symbol "("
  _ <- symbol ")"
  return UniT

pTupleT :: Parser Type
pTupleT = do
  _ <- tag (symbol "(")
  ts <- parens (sepBy1 pType (symbol ","))
  let v = TV . MT.pack $ "Tuple" ++ (show (length ts))
  return $ ArrT v ts

pRecordT :: Parser Type
pRecordT = do
  _ <- tag (symbol "{")
  record <- braces (sepBy1 pRecordEntryT (symbol ","))
  return $ RecT record

pRecordEntryT :: Parser (TVar, Type)
pRecordEntryT = do
  n <- name
  _ <- symbol "::"
  t <- pType
  return (TV n, t)

pExistential :: Parser Type
pExistential = do
  v <- angles name
  return (ExistT (TV v))

pArrT :: Parser Type
pArrT = do
  v <- name
  args <- many1 pType'
  return $ ArrT (TV v) args
  where
    pType' = parens pType <|> pVarT <|> pListT

pFunT :: Parser Type
pFunT = do
  t <- pType'
  _ <- symbol "->"
  ts <- sepBy1 pType' (symbol "->")
  return $ foldr1 FunT (t:ts)
  where
    pType' = parens pType <|> try pArrT <|> pVarT <|> pListT

pListT :: Parser Type
pListT = do
  _ <- tag (symbol "[")
  t <- brackets pType
  return $ ArrT (TV "List") [t]

pVarT :: Parser Type
pVarT = do
  _ <- tag (name <|> stringLiteral)
  n <- name <|> stringLiteral
  return $ VarT (TV n)

pForAllT :: Parser Type
pForAllT = do
  _ <- symbol "forall"
  vs <- many1 name
  _ <- symbol "."
  t <- pType
  return (curryForall vs t)
  where
    curryForall [] e' = e'
    curryForall (v:vs') e' =  Forall (TV v) (curryForall vs' e') 
