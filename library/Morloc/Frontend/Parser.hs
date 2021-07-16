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

import Morloc.Frontend.Lexer
import qualified Morloc.Frontend.AST as AST
import Data.Void (Void)
import Morloc.Frontend.Namespace
import Text.Megaparsec
import Text.Megaparsec.Char hiding (eol)
import qualified Morloc.Frontend.Lang.DefaultTypes as MLD
import qualified Control.Monad.State as CMS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Morloc.Data.Text as MT
import qualified Morloc.System as MS
import qualified Text.Megaparsec.Char.Lexer as L

-- | Parse a single file or string that may contain multiple modules. Each
-- module is written written into the DAG of previously observed modules.
readProgram
  :: Maybe Path
  -- ^ An optional path to the file the source code was read from. If no path
  -- is given, then the source code was provided as a string.
  -> MT.Text -- ^ Source code
  -> DAG MVar Import Expr -- ^ Possibly empty directed graph of previously observed modules
  -> Either (ParseErrorBundle MT.Text Void) (DAG MVar Import Expr)
readProgram f sourceCode p =
  case runParser
         (CMS.runStateT (pProgram <* eof) pstate)
         (maybe "<expr>" id f)
         sourceCode of
    Left err -> Left err
    Right (es, _) -> Right
      $ foldl (\d (k,xs,n) -> Map.insert k (n,xs) d)
              p
              (map AST.findEdges es) -- [(MVar, [(MVar, Import)], Expr)]
  where
    pstate = emptyState { stateModulePath = f }

-- | Parse a single type. This function used only in debugging in command line
-- calls such as: `morloc typecheck -te "A -> B"`.
readType :: MT.Text -> Either (ParseErrorBundle MT.Text Void) UnresolvedType
readType typeStr =
  case runParser (CMS.runStateT (pTypeGen <* eof) state) "" typeStr of
    Left err -> Left err
    Right (es, _) -> Right es
  where
    state = emptyState {stateMinPos = mkPos 1, stateAccepting = True}

-- | The output is rolled into the final DAG of modules
pProgram :: Parser [Expr]
pProgram = do
  f <- CMS.gets stateModulePath
  L.space space1 comments empty
  setMinPos
  return $ align (many pToplevel)

pToplevel :: Parser Expr
pToplevel = try pModule <|> pMain

-- | match a named module
pModule :: Parser Expr
pModule = do
  f <- CMS.gets stateModulePath
  _ <- reserved "module"
  es <- align pExpr
  moduleName' <- freename
  return $ ModE freename es

-- | match an implicit "main" module
pMain :: Parser Expr
pMain = ModE (EV "Main") <*> many pExpr

pExpr :: Parser Expr
pExpr =
      try pImport
  <|> try pExport
  <|> try pTypedef
  <|> try pDeclaration
  <|> try pSignature
  <|> try pAcc
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


pImport :: Parser Expr
pImport = do
  _ <- reserved "import"
  n <- freename
  imports <-
    optional $
    parens (sepBy pImportTerm (symbol ",")) <|> fmap (\x -> [(x, x)]) pEVar
  return . ImpE $
    Import
      { importModuleName = MVar n
      , importInclude = imports
      , importExclude = []
      , importNamespace = Nothing
      }
  where

  pImportTerm :: Parser (EVar, EVar)
  pImportTerm = do
    n <- pEVar
    a <- option n (reserved "as" >> pEVar)
    return (n, a)

pExport :: Parser Expr
pExport = do
  reserved "export"
  v <- pEVar
  return $ ExpE v


pTypedef :: Parser Expr
pTypedef = try pTypedefType <|> pTypedefObject where

  pTypedefType :: Parser Expr
  pTypedefType = do
    _ <- reserved "type"
    lang <- optional (try pLang)
    setLang lang
    (v, vs) <- pTypedefTermUnpar <|> pTypedefTermPar
    _ <- symbol "="
    t <- pType
    setLang Nothing
    return $ TypE v vs t)

  pTypedefObject :: Parser Expr
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
    return $ TypE v vs t

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


pDeclaration :: Parser Expr
pDeclaration = try pFunctionDeclaration <|> pDataDeclaration
  where

  pDataDeclaration :: Parser Expr
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
    return $ Declaration v' e subExpressions

  pFunctionDeclaration :: Parser Expr
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
    return $ Declaration v' (curryLamE args' e) subExpressions
    where
      curryLamE [] e' = e'
      curryLamE (v:vs') e' = LamE v (curryLamE vs' e')

  -- | For now, only type signatures and declarations are allowed in function
  -- where statements. There is no particularly reason why source and imports
  -- could not be here. Exports probably should NOT be allowed since they would
  -- break scope.
  whereTerm :: Parser Expr
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
  where

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
  pConstraints = reserved "where" >> alignInset pConstraint where

    -- FIXME: this is a stub
    pConstraint :: Parser Constraint
    pConstraint = fmap (Con . MT.unwords) (many1 pWord)

    pWord :: Parser MT.Text
    pWord =  MT.pack <$> lexeme (many1 alphaNumChar)


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
  where

  pImportSourceTerm :: Parser (Name, EVar)
  pImportSourceTerm = do
    n <- stringLiteral
    v <- evar n
    a <- option v (reserved "as" >> freename >>= evar)
    return (Name n, a)

pNamE :: Parser Expr
pNamE = RecE <$> braces (sepBy1 pNamEntryE (symbol ","))
  where

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
pVarU = try pVarConU <|> pVarGenU where
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
