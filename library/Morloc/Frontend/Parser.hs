{-|
Module      : Morloc.Frontend.Parser
Description : Full parser for Morloc
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.Frontend.Parser
  ( readProgram
  , readType
  ) where

import Data.Void (Void)
import Morloc.Frontend.Namespace
import Text.Megaparsec
import Text.Megaparsec.Char hiding (eol)
import qualified Morloc.Frontend.Lang.DefaultTypes as MLD
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
  , stateMinPos :: Pos
  , stateAccepting :: Bool
  , stateNamespace :: [MT.Text]
} deriving(Show)

emptyState :: ParserState
emptyState = ParserState {
    stateLang = Nothing
  , stateModulePath = Nothing
  , stateIndex = 1
  , stateGenerics = []
  , stateMinPos = mkPos 1
  , stateAccepting = False
  , stateNamespace = []
}

evar :: MT.Text -> Parser EVar
evar v = do
  namespace <- CMS.gets stateNamespace
  return $ EV namespace v

tvar :: MT.Text -> Parser TVar
tvar v = do
  lang <- CMS.gets stateLang
  return $ TV lang v

newvar :: Maybe Lang -> Parser TVar
newvar lang = do
  s <- CMS.get
  let i = stateIndex s 
      n = "p" <> MT.show' i
  CMS.put (s {stateIndex = i + 1}) 
  tvar n

setLang :: Maybe Lang -> Parser ()
setLang lang = do
  s <- CMS.get
  CMS.put (s { stateLang = lang })

incNamespace :: MT.Text -> Parser ()
incNamespace v = do
  s <- CMS.get
  CMS.put (s {stateNamespace = stateNamespace s ++ [v]})

decNamespace :: Parser ()
decNamespace = do
  s <- CMS.get
  CMS.put (s {stateNamespace = tail (stateNamespace s)})

setMinPos :: Parser ()
setMinPos = do
  s <- CMS.get
  level <- L.indentLevel
  CMS.put (s { stateMinPos = level })

-- Require elements all start on the same line as the first element
align :: Parser a -> Parser [a] 
align p = do
  s <- CMS.get
  let minPos = stateMinPos s 
      accept = stateAccepting s
  curPos <- L.indentLevel
  xs <- many (CMS.put (s {stateMinPos = curPos, stateAccepting = True}) >> p)
  -- put everything back the way it was
  CMS.put (s {stateMinPos = minPos, stateAccepting = accept})
  return xs

alignInset :: Parser a -> Parser [a]
alignInset p = isInset >> align p

isInset :: Parser ()
isInset = do
  minPos <- CMS.gets stateMinPos
  curPos <- L.indentLevel
  if curPos <= minPos
    then
      L.incorrectIndent GT minPos curPos
    else
      return ()

sc = L.space space1 comments empty

symbol = lexeme . L.symbol sc

lexemeBase :: Parser a -> Parser a
lexemeBase = L.lexeme sc

lexeme :: Parser a -> Parser a
lexeme p = do
  minPos <- CMS.gets stateMinPos
  accepting <- CMS.gets stateAccepting
  s <- CMS.get
  curPos <- L.indentLevel
  if accepting
    then
      if curPos == minPos
        then
          CMS.put (s { stateAccepting = False }) >> lexemeBase p
        else
          L.incorrectIndent GT minPos curPos
    else
      if minPos < curPos
        then
          lexemeBase p
        else 
          L.incorrectIndent GT minPos curPos


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
  -> Either (ParseErrorBundle MT.Text Void) (DAG MVar Import ParserNode)
readProgram f sourceCode p =
  case runParser
         (CMS.runStateT (pProgram <* eof) pstate)
         (maybe "<expr>" id f)
         sourceCode of
    Left err -> Left err
    Right (es, _) -> Right $ foldl (\d (k,xs,n) -> Map.insert k (n,xs) d) p es 
  where
    pstate = emptyState { stateModulePath = f }

readType :: MT.Text -> Either (ParseErrorBundle MT.Text Void) UnresolvedType
readType typeStr =
  case runParser (CMS.runStateT (pTypeGen <* eof) state) "" typeStr of
    Left err -> Left err
    Right (es, _) -> Right es
  where
    state = emptyState {stateMinPos = mkPos 1, stateAccepting = True}


many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  return (x : xs)

comments :: Parser ()
comments =  L.skipLineComment "--"
        <|> L.skipBlockCommentNested "{-" "-}"
        <?> "comment"

number :: Parser DS.Scientific
number = lexeme $ L.signed sc L.scientific

surround :: Parser l -> Parser r -> Parser a -> Parser a
surround l r v = do
  l
  v' <- v
  r
  return v'

brackets :: Parser a -> Parser a
brackets = surround (symbol "[") (symbol "]")

parens :: Parser a -> Parser a
parens = surround (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = surround (symbol "{") (symbol "}")

angles :: Parser a -> Parser a
angles = surround (symbol "<") (symbol ">")

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

op :: MT.Text -> Parser MT.Text
op o = (lexeme . try) (symbol o <* notFollowedBy (oneOf operatorChars))

reserved :: MT.Text -> Parser MT.Text
reserved w = try (symbol w)

stringLiteral :: Parser MT.Text
stringLiteral = lexeme $ do
  _ <- char '\"'
  s <- many (noneOf ['"'])
  _ <- char '\"'
  return $ MT.pack s

freename :: Parser MT.Text
freename = (lexeme . try) (p >>= check)
  where
    p = fmap MT.pack $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
    check x =
      if elem x reservedWords
        then failure Nothing Set.empty -- TODO: error message
        else return x

data Toplevel
  = TModule (MVar, [(MVar, Import)], ParserNode)
  | TModuleBody [ModuleBody]

data ModuleBody
  = MBImport Import
  -- ^ module name, function name and optional alias
  | MBExport EVar
  | MBTypeDef TVar [TVar] UnresolvedType
  | MBBody Expr


pProgram :: Parser [(MVar, [(MVar, Import)], ParserNode)]
pProgram = do
  f <- CMS.gets stateModulePath
  L.space space1 comments empty
  setMinPos
  es <- align pToplevel
  let mods = [m | (TModule m) <- es]
  case concat [e | (TModuleBody e) <- es] of
    [] -> return mods
    es' -> return $ makeModule f (MVar "Main") es' : mods

pToplevel :: Parser Toplevel
pToplevel = try (fmap TModule pModule) <|> fmap TModuleBody pModuleBody

pModule :: Parser (MVar, [(MVar, Import)], ParserNode)
pModule = do
  f <- CMS.gets stateModulePath
  _ <- reserved "module"
  moduleName' <- freename
  mess <- align pModuleBody
  return $ makeModule f (MVar moduleName') (concat mess)

makeModule :: Maybe Path -> MVar -> [ModuleBody] -> (MVar, [(MVar, Import)], ParserNode)
makeModule f n mes = (n, edges, node) where
  imports' = [x | (MBImport x) <- mes]
  exports' = Set.fromList [x | (MBExport x) <- mes]
  body' = [x | (MBBody x) <- mes]
  srcMap = (Map.fromList . concat)
           [[((srcAlias s, srcLang s), s) | s <- ss ] | (SrcE ss) <- body']
  typedefmap = Map.fromList [(v, (t, vs)) | MBTypeDef v vs t <- mes]
  edges = [(importModuleName i, i) | i <- imports']
  node = ParserNode
    { parserNodePath = f
    , parserNodeBody = body'
    , parserNodeSourceMap = srcMap
    , parserNodeExports = exports'
    , parserNodeTypedefs = typedefmap
    }

pModuleBody :: Parser [ModuleBody]
pModuleBody =
        try (fmap return pTypedef)
    <|> try (fmap return pImport)
    <|> try (fmap return pExport)
    <|> try pStatement'
    <|> pExpr'
  where
    pStatement' = fmap (map MBBody) pStatement
    pExpr' = fmap (return . MBBody) pExpr

pTypedef :: Parser ModuleBody
pTypedef = pTypedefType <|> pTypedefObject

pTypedefType :: Parser ModuleBody
pTypedefType = do
  _ <- reserved "type"
  lang <- optional (try pLang)
  setLang lang
  (v, vs) <- pTypedefTermUnpar <|> pTypedefTermPar
  _ <- symbol "="
  t <- pType
  setLang Nothing
  return (MBTypeDef v vs t)

pTypedefObject :: Parser ModuleBody
pTypedefObject = do
  r <- pNamType
  lang <- optional (try pLang)
  setLang lang
  (v, vs) <- pTypedefTermUnpar <|> pTypedefTermPar
  _ <- symbol "="
  constructor <- freename <|> stringLiteral
  entries <- braces (sepBy1 pNamEntryU (symbol ",")) >>= mapM (desugarTableEntries lang r)
  let t = NamU r (TV lang constructor) (map VarU vs) entries
  setLang Nothing
  return $ MBTypeDef v vs t

desugarTableEntries
  :: Maybe Lang
  -> NamType
  -> (MT.Text, UnresolvedType)
  -> Parser (MT.Text, UnresolvedType)
desugarTableEntries _ NamRecord entry = return entry
desugarTableEntries _ NamObject entry = return entry
desugarTableEntries lang NamTable (k0, t0) = (,) k0 <$> f t0 where
  f :: UnresolvedType -> Parser UnresolvedType
  f (ForallU v t) = ForallU v <$> f t
  f t = return $ head (MLD.defaultList lang t)

pNamType :: Parser NamType
pNamType = choice [pNamObject, pNamTable, pNamRecord] 

pNamObject :: Parser NamType
pNamObject = do
  _ <- reserved "object" 
  return NamObject

pNamTable :: Parser NamType
pNamTable = do
  _ <- reserved "table" 
  return NamTable

pNamRecord :: Parser NamType
pNamRecord = do
  _ <- reserved "record" 
  return NamRecord

pTypedefTermUnpar :: Parser (TVar, [TVar])
pTypedefTermUnpar = do
  v <- freename
  t <- tvar v
  return (t, [])

pTypedefTermPar :: Parser (TVar, [TVar])
pTypedefTermPar = do
  vs <- parens (many1 freename)
  t <- tvar (head vs)
  ts <- mapM tvar (tail vs)
  return (t, ts)

pImport :: Parser ModuleBody
pImport = do
  _ <- reserved "import"
  n <- freename
  imports <-
    optional $
    parens (sepBy pImportTerm (symbol ",")) <|> fmap (\x -> [(x, x)]) pEVar
  return . MBImport $
    Import
      { importModuleName = MVar n
      , importInclude = imports
      , importExclude = []
      , importNamespace = Nothing
      }

pImportTerm :: Parser (EVar, EVar)
pImportTerm = do
  n <- pEVar
  a <- option n (reserved "as" >> pEVar)
  return (n, a)

pExport :: Parser ModuleBody
pExport = do
  reserved "export"
  v <- pEVar
  return $ MBExport v

pStatement :: Parser [Expr]
pStatement = try pDeclaration <|> fmap return pSignature

pDeclaration :: Parser [Expr]
pDeclaration = try pFunctionDeclaration <|> pDataDeclaration

pDataDeclaration :: Parser [Expr]
pDataDeclaration = do
  v <- freename
  v' <- evar v
  _ <- symbol "="
  -- enter data declaration scope
  incNamespace v
  e <- pExpr
  subExpressions <- option [] $ reserved "where" >> alignInset whereTerm |>> concat
  decNamespace
  -- exit scope
  return $ Declaration v' e : subExpressions

pFunctionDeclaration :: Parser [Expr]
pFunctionDeclaration = do
  v <- freename
  v' <- evar v
  -- enter function scope
  incNamespace v
  args <- many1 freename
  args' <- mapM evar args
  _ <- symbol "="
  e <- pExpr
  subExpressions <- option [] $ reserved "where" >> alignInset whereTerm |>> concat
  decNamespace
  -- exit scope
  return $ Declaration v' (curryLamE args' e) : subExpressions
  where
    curryLamE [] e' = e'
    curryLamE (v:vs') e' = LamE v (curryLamE vs' e')

whereTerm :: Parser [Expr]
whereTerm = try (fmap return pSignature) <|> pDeclaration

pSignature :: Parser Expr
pSignature = do
  v <- freename
  v' <- evar v
  lang <- optional (try pLang)
  setLang lang
  _ <- op "::"
  props <- option [] (try pPropertyList)
  t <- pTypeGen
  constraints <- option [] pConstraints
  setLang Nothing
  return $
    Signature
      v'
      (EType
         { etype = t
         , eprop = Set.fromList props
         , econs = Set.fromList constraints
         })

pLang :: Parser Lang
pLang = do
  langStr <- freename
  case ML.readLangName langStr of
    (Just lang) -> return lang
    Nothing -> fancyFailure . Set.singleton . ErrorFail
      $ "Langage '" <> MT.unpack langStr <> "' is not supported"
 
-- | match an optional tag that precedes some construction
tag :: Parser a -> Parser (Maybe MT.Text)
tag p = optional (try tag')
  where
    tag' = do
      l <- freename
      _ <- op ":"
      _ <- lookAhead p
      return l

pPropertyList :: Parser [Property]
pPropertyList = do
  ps <-  parens (sepBy1 pProperty (symbol ","))
     <|> sepBy1 pProperty (symbol ",")
  _ <- op "=>"
  return ps

pProperty :: Parser Property
pProperty = do
  ps <- many1 freename
  case ps of
    ["pack"] -> return Pack
    ["unpack"] -> return Unpack
    ["cast"] -> return Cast
    _ -> return (GeneralProperty ps)

pConstraints :: Parser [Constraint]
pConstraints = reserved "where" >> alignInset pConstraint

-- FIXME: this is a stub
pConstraint :: Parser Constraint
pConstraint = fmap (Con . MT.unwords) (many1 pWord) where
  pWord :: Parser MT.Text
  pWord =  MT.pack <$> lexeme (many1 alphaNumChar)


pExpr :: Parser Expr
pExpr =
      try pAcc
  <|> try pNamE
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

pSrcE :: Parser Expr
pSrcE = do
  modulePath <- CMS.gets stateModulePath
  reserved "source"
  language <- pLang
  srcfile <- optional (reserved "from" >> stringLiteral |>> MT.unpack)
  rs <- parens (sepBy1 pImportSourceTerm (symbol ","))
  srcFile <- case (modulePath, srcfile) of
    -- build a path to the source file by searching
    -- > source "R" from "foo.R" ("Foo" as foo, "bar")
    (Just f, Just srcfile') -> return . Just $ MS.combine (MS.takeDirectory f) srcfile'
    -- we are sourcing from the language base
    -- > source "R" ("sqrt", "t.test" as t_test)
    (Just _, Nothing) -> return Nothing
    -- this case SHOULD only occur in testing where the source file does not exist
    -- file non-existence will be caught later
    (Nothing, s) -> return s 
  return $ SrcE [Source { srcName = srcVar
                        , srcLang = language
                        , srcPath = srcFile
                        , srcAlias = aliasVar
                        } | (srcVar, aliasVar) <- rs]

pImportSourceTerm :: Parser (Name, EVar)
pImportSourceTerm = do
  n <- stringLiteral
  v <- evar n
  a <- option v (reserved "as" >> freename >>= evar)
  return (Name n, a)

pNamE :: Parser Expr
pNamE = RecE <$> braces (sepBy1 pNamEntryE (symbol ","))

pNamEntryE :: Parser (MT.Text, Expr)
pNamEntryE = do
  n <- freename
  _ <- symbol "="
  e <- pExpr
  return (n, e)

pListE :: Parser Expr
pListE = ListE <$> brackets (sepBy pExpr (symbol ","))

pTuple :: Parser Expr
pTuple = do
  _ <- symbol "("
  e <- pExpr
  _ <- symbol ","
  es <- sepBy1 pExpr (symbol ",")
  _ <- symbol ")"
  return (TupleE (e : es))

pUni :: Parser Expr
pUni = symbol "Null" >> return UniE

pAcc :: Parser Expr
pAcc = do
  e <- parens pExpr <|> pNamE <|> pVar
  _ <- symbol "@"
  f <- freename
  return $ AccE e f

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
pEVar = freename >>= evar

pTypeGen :: Parser UnresolvedType
pTypeGen = do
  resetGenerics
  t <- pType
  s <- CMS.get
  return $ forallWrap (unique (reverse (stateGenerics s))) t
  where
    forallWrap :: [TVar] -> UnresolvedType -> UnresolvedType
    forallWrap [] t = t
    forallWrap (v:vs) t = ForallU v (forallWrap vs t)

pType :: Parser UnresolvedType
pType =
      pExistential
  <|> try pFunU
  <|> try pUniU
  <|> try pNamU
  <|> try pArrU
  <|> try parensType
  <|> pListU
  <|> pTupleU
  <|> pVarU

pUniU :: Parser UnresolvedType
pUniU = do
  _ <- symbol "("
  _ <- symbol ")"
  lang <- CMS.gets stateLang
  v <- newvar lang
  return (ExistU v [] (MLD.defaultNull lang))

parensType :: Parser UnresolvedType
parensType = do
  _ <- tag (symbol "(")
  t <- parens pType
  return t

pTupleU :: Parser UnresolvedType
pTupleU = do
  lang <- CMS.gets stateLang
  _ <- tag (symbol "(")
  ts <- parens (sepBy1 pType (symbol ","))
  return $ head (MLD.defaultTuple lang ts)

pNamU :: Parser UnresolvedType
pNamU = do
  _ <- tag (symbol "{")
  entries <- braces (sepBy1 pNamEntryU (symbol ","))
  lang <- CMS.gets stateLang
  return $ head (MLD.defaultRecord lang entries)

pNamEntryU :: Parser (MT.Text, UnresolvedType)
pNamEntryU = do
  n <- freename
  _ <- op "::"
  t <- pType
  return (n, t)

pExistential :: Parser UnresolvedType
pExistential = do
  v <- angles freename
  return (ExistU (TV Nothing v) [] [])

pArrU :: Parser UnresolvedType
pArrU = do
  _ <- tag (freename <|> stringLiteral)
  n <- freename <|> stringLiteral
  t <- tvar n
  args <- many1 pType'
  return $ ArrU t args
  where
    pType' = try pUniU <|> try parensType <|> pVarU <|> pListU <|> pTupleU <|> pNamU

pFunU :: Parser UnresolvedType
pFunU = do
  t <- pType'
  _ <- op "->"
  ts <- sepBy1 (pType') (op "->")
  return $ foldr1 FunU (t : ts)
  where
    pType' = try pUniU <|> try parensType <|> try pArrU <|> pVarU <|> pListU <|> pTupleU <|> pNamU

pListU :: Parser UnresolvedType
pListU = do
  _ <- tag (symbol "[")
  t <- brackets pType
  lang <- CMS.gets stateLang
  return $ head (MLD.defaultList lang t)

pVarU :: Parser UnresolvedType
pVarU = try pVarConU <|> pVarGenU

pVarConU :: Parser UnresolvedType
pVarConU = do
  _ <- tag stringLiteral
  n <- stringLiteral
  t <- tvar n
  return $ VarU t

pVarGenU :: Parser UnresolvedType
pVarGenU = do
  _ <- tag freename
  n <- freename
  t <- tvar n
  appendGenerics t  -- add the term to the generic list IF generic
  return $ VarU t
