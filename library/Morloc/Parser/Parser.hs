{-|
Module      : Morloc.Parser.Parser
Description : Full parser for Morloc
Copyright   : (c) Zebulun Arendsee, 2020
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
import qualified Morloc.Lang.DefaultTypes as MLD
import qualified Control.Monad.State as CMS
import qualified Data.Map as Map
import qualified Data.Scientific as DS
import qualified Data.Set as Set
import qualified Data.Char as DC
import qualified Morloc.Data.Text as MT
import qualified Morloc.Language as ML
import qualified Morloc.System as MS
import qualified Text.Megaparsec.Char.Lexer as L

type Parser a = CMS.StateT ParserState (Parsec Void MT.Text) a

data ParserState = ParserState {
    stateLang :: Maybe Lang
  , stateModulePath :: Maybe Path
  , stateIndex :: Int
  , stateGenerics :: [TVar] -- store the observed generic variables in the current type
                            -- you should reset the field before parsing a new type 
}

emptyState :: ParserState
emptyState = ParserState {
    stateLang = Nothing
  , stateModulePath = Nothing
  , stateIndex = 1
  , stateGenerics = []
}

newvar :: Maybe Lang -> Parser TVar
newvar lang = do
  s <- CMS.get
  let i = stateIndex s 
  CMS.put (s {stateIndex = i + 1}) 
  return (TV lang ("p" <> MT.show' i))

setLang :: Maybe Lang -> Parser ()
setLang lang = do
  s <- CMS.get
  CMS.put (s { stateLang = lang })

resetGenerics :: Parser ()
resetGenerics = do
  s <- CMS.get
  CMS.put (s { stateGenerics = [] })

appendGenerics :: TVar -> Parser ()
appendGenerics v@(TV _ vstr) = do
  s <- CMS.get
  let isGeneric = maybe False (DC.isLower . fst) (MT.uncons vstr)
      gs = stateGenerics s
      gs' = if isGeneric then v : gs else gs
  CMS.put (s {stateGenerics = gs'})

readProgram
  :: Maybe Path
  -> MT.Text
  -> DAG MVar Import ParserNode
  -> DAG MVar Import ParserNode
readProgram f sourceCode p =
  case runParser
         (CMS.runStateT (sc >> pProgram <* eof) pstate)
         (maybe "<expr>" (MT.unpack . unPath) f)
         sourceCode of
    Left err -> error (show err)
    Right (es, _) -> foldl (\d (k,xs,n) -> Map.insert k (n,xs) d) p es 
  where
    pstate = emptyState { stateModulePath = f }

readType :: MT.Text -> Type
readType typeStr =
  case runParser (CMS.runStateT (pTypeGen <* eof) emptyState) "" typeStr of
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
  , "type"
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
  = TModule (MVar, [(MVar, Import)], ParserNode)
  | TModuleBody ModuleBody

data ModuleBody
  = MBImport Import
  -- ^ module name, function name and optional alias
  | MBExport EVar
  | MBTypeDef TVar [TVar] Type
  | MBBody Expr

pProgram :: Parser [(MVar, [(MVar, Import)], ParserNode)]
pProgram = do
  f <- CMS.gets stateModulePath
  -- allow ';' at the beginning (if you're into that sort of thing)
  optional delimiter
  es <- many pToplevel
  let mods = [m | (TModule m) <- es]
  case [e | (TModuleBody e) <- es] of
    [] -> return mods
    es' -> return $ makeModule f (MVar "Main") es' : mods

pToplevel :: Parser Toplevel
pToplevel =
  try (fmap TModule pModule <* optional delimiter) <|>
  fmap TModuleBody (pModuleBody <* optional delimiter)

pModule :: Parser (MVar, [(MVar, Import)], ParserNode)
pModule = do
  f <- CMS.gets stateModulePath
  _ <- reserved "module"
  moduleName' <- name
  mes <- braces (optional delimiter >> many1 pModuleBody)
  return $ makeModule f (MVar moduleName') mes

makeModule :: Maybe Path -> MVar -> [ModuleBody] -> (MVar, [(MVar, Import)], ParserNode)
makeModule f n mes = (n, edges, node) where
  imports' = [x | (MBImport x) <- mes]
  exports' = Set.fromList [x | (MBExport x) <- mes]
  body' = [x | (MBBody x) <- mes]
  typedefmap = Map.fromList [(v, (t, vs)) | MBTypeDef v vs t <- mes]
  srcMap = (Map.fromList . concat)
           [[((srcAlias s, srcLang s), s) | s <- ss ] | (SrcE ss) <- body']
  edges = [(importModuleName i, i) | i <- imports']
  node = ParserNode
    { parserNodePath = f
    , parserNodeBody = body'
    , parserNodeSourceMap = srcMap
    , parserNodeExports = exports'
    , parserNodeTypedefs = typedefmap
    }

pModuleBody :: Parser ModuleBody
pModuleBody =
        try pTypedef <* optional delimiter
    <|> try pImport <* optional delimiter 
    <|> try pExport <* optional delimiter 
    <|> try pStatement' <* optional delimiter
    <|> pExpr' <* optional delimiter
  where
    pStatement' = fmap MBBody pStatement
    pExpr' = fmap MBBody pExpr

pTypedef :: Parser ModuleBody
pTypedef = do
  _ <- reserved "type"
  lang <- optional (try pLang)
  setLang lang
  (v, vs) <- pTypedefTermUnpar <|> pTypedefTermPar
  _ <- symbol "="
  t <- pType
  setLang Nothing
  return (MBTypeDef v vs t)

pTypedefTermUnpar :: Parser (TVar, [TVar])
pTypedefTermUnpar = do
  v <- name
  lang <- CMS.gets stateLang
  return (TV lang v, [])

pTypedefTermPar :: Parser (TVar, [TVar])
pTypedefTermPar = do
  vs <- parens (many1 name)
  lang <- CMS.gets stateLang
  return (TV lang (head vs), map (TV lang) (tail vs))

pImport :: Parser ModuleBody
pImport = do
  _ <- reserved "import"
  n <- name
  imports <-
    optional $
    parens (sepBy pImportTerm (symbol ",")) <|> fmap (\x -> [(EVar x, EVar x)]) name
  return . MBImport $
    Import
      { importModuleName = MVar n
      , importInclude = imports
      , importExclude = []
      , importNamespace = Nothing
      }

pImportTerm :: Parser (EVar, EVar)
pImportTerm = do
  n <- name
  a <- option n (reserved "as" >> name)
  return (EVar n, EVar a)

pExport :: Parser ModuleBody
pExport = fmap (MBExport . EVar) $ reserved "export" >> name

pStatement :: Parser Expr
pStatement = try pDeclaration <|> pSignature

pDeclaration :: Parser Expr
pDeclaration = try pFunctionDeclaration <|> pDataDeclaration

pDataDeclaration :: Parser Expr
pDataDeclaration = do
  v <- name
  _ <- symbol "="
  e <- pExpr
  return (Declaration (EVar v) e)

pFunctionDeclaration :: Parser Expr
pFunctionDeclaration = do
  v <- name
  args <- many1 name
  _ <- op "="
  e <- pExpr
  return $ Declaration (EVar v) (curryLamE (map EVar args) e)
  where
    curryLamE [] e' = e'
    curryLamE (v:vs') e' = LamE v (curryLamE vs' e')

pSignature :: Parser Expr
pSignature = do
  v <- name
  lang <- optional (try pLang)
  setLang lang
  _ <- op "::"
  props <- option [] (try pPropertyList)
  t <- pTypeGen
  constraints <-
    option [] $ reserved "where" >> braces (sepBy pConstraint (symbol ";"))
  setLang Nothing
  return $
    Signature
      (EVar v)
      (EType
         { etype = t
         , eprop = Set.fromList props
         , econs = Set.fromList constraints
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
      try pNamE
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
  srcfile <- optional (reserved "from" >> stringLiteral |>> Path)
  rs <- parens (sepBy1 pImportSourceTerm (symbol ","))
  let path = MS.combine <$> fmap MS.takeDirectory f <*> srcfile
  return $ SrcE [Source { srcName = name
                        , srcLang = language
                        , srcPath = path
                        , srcAlias = alias
                        } | (name, alias) <- rs]

pImportSourceTerm :: Parser (Name, EVar)
pImportSourceTerm = do
  n <- stringLiteral
  a <- option n (reserved "as" >> name)
  return (Name n, EVar a)

pNamE :: Parser Expr
pNamE = fmap RecE $ braces (sepBy1 pNamEntryE (symbol ","))

pNamEntryE :: Parser (EVar, Expr)
pNamEntryE = do
  n <- name
  _ <- symbol "="
  e <- pExpr
  return (EVar n, e)

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
  t <- pTypeGen
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
      <|> pNamE
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
pEVar = fmap EVar name

pTypeGen :: Parser Type
pTypeGen = do
  resetGenerics
  t <- pType
  s <- CMS.get
  return $ forallWrap (unique (reverse (stateGenerics s))) t
  where
    forallWrap :: [TVar] -> Type -> Type
    forallWrap [] t = t
    forallWrap (v:vs) t = Forall v (forallWrap vs t)

pType :: Parser Type
pType =
      pExistential
  <|> try pFunT
  <|> try pUniT
  <|> try pNamT
  <|> try pArrT
  <|> try parensType
  <|> pListT
  <|> pTupleT
  <|> pVarT

pUniT :: Parser Type
pUniT = do
  _ <- symbol "("
  _ <- symbol ")"
  lang <- CMS.gets stateLang
  v <- newvar lang
  return (ExistT v [] (MLD.defaultNull lang))

parensType :: Parser Type
parensType = do
  _ <- tag (symbol "(")
  t <- parens pType
  return t

pTupleT :: Parser Type
pTupleT = do
  lang <- CMS.gets stateLang
  _ <- tag (symbol "(")
  ts <- parens (sepBy1 pType (symbol ","))
  v <- newvar lang
  let dts = MLD.defaultTuple lang ts
  return $
    if lang == Nothing
    then head (map unDefaultType dts)
    else ExistT v ts dts

pNamT :: Parser Type
pNamT = do
  _ <- tag (symbol "{")
  entries <- braces (sepBy1 pNamEntryT (symbol ","))
  lang <- CMS.gets stateLang
  v <- newvar lang
  let dts = MLD.defaultRecord lang entries
  return $
    if lang == Nothing
    then head (map unDefaultType dts)
    else ExistT v [NamT (TV lang "__RECORD__") entries] dts -- see entry in Infer.hs

pNamEntryT :: Parser (MT.Text, Type)
pNamEntryT = do
  n <- name
  _ <- op "::"
  t <- pType
  return (n, t)

pExistential :: Parser Type
pExistential = do
  v <- angles name
  return (ExistT (TV Nothing v) [] [])

pArrT :: Parser Type
pArrT = do
  lang <- CMS.gets stateLang
  _ <- tag (name <|> stringLiteral)
  n <- name <|> stringLiteral
  args <- many1 pType'
  return $ ArrT (TV lang n) args
  where
    pType' = try pUniT <|> try parensType <|> pVarT <|> pListT <|> pTupleT <|> pNamT

pFunT :: Parser Type
pFunT = do
  t <- pType'
  _ <- op "->"
  ts <- sepBy1 pType' (op "->")
  return $ foldr1 FunT (t : ts)
  where
    pType' = try pUniT <|> try parensType <|> try pArrT <|> pVarT <|> pListT <|> pTupleT <|> pNamT

pListT :: Parser Type
pListT = do
  _ <- tag (symbol "[")
  t <- brackets pType
  lang <- CMS.gets stateLang
  v <- newvar lang
  let dts = MLD.defaultList lang t
  return $
    if lang == Nothing
    then head (map unDefaultType dts)
    else ExistT v [t] dts

pVarT :: Parser Type
pVarT = try pVarConT <|> pVarGenT

pVarConT :: Parser Type
pVarConT = do
  lang <- CMS.gets stateLang
  _ <- tag stringLiteral
  n <- stringLiteral
  return $ VarT (TV lang n)

pVarGenT :: Parser Type
pVarGenT = do
  lang <- CMS.gets stateLang
  _ <- tag name
  n <- name
  let v = TV lang n
  appendGenerics v  -- add the term to the generic list IF generic
  return $ VarT v
