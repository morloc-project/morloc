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
  -> DAG MVar Import ExprI -- ^ Possibly empty directed graph of previously observed modules
  -> Either (ParseErrorBundle MT.Text Void) (DAG MVar Import ExprI)
readProgram f sourceCode p =
  case runParser
         (CMS.runStateT (pProgram <* eof) pstate)
         (maybe "<expr>" id f)
         sourceCode of
    Left err -> Left err
    Right (es, _) -> Right
      $ foldl (\d (k,xs,n) -> Map.insert k (n,xs) d)
              p
              (map AST.findEdges es) -- [(MVar, [(MVar, Import)], ExprI)]
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
pProgram :: Parser [ExprI]
pProgram = do
  f <- CMS.gets stateModulePath
  L.space space1 comments empty
  setMinPos
  many pToplevel

pToplevel :: Parser ExprI
pToplevel = try pModule <|> pMain

-- | match a named module
pModule :: Parser ExprI
pModule = do
  f <- CMS.gets stateModulePath
  _ <- reserved "module"
  ess <- align pTopExpr
  moduleName <- freename
  exprI $ ModE (MVar moduleName) (concat ess)

-- | match an implicit "main" module
pMain :: Parser ExprI
pMain = (ModE (MVar "Main") <$> fmap concat (many pTopExpr)) >>= exprI

-- | Expressions including ones that are allowed only at the top-level of a scope
pTopExpr :: Parser [ExprI]
pTopExpr = 
      try (plural pImport) -- TODO simplify Import expression to be singular
  <|> try (plural pExport) -- TODO allow many exports from one statement
  <|> try (plural pTypedef)
  <|> try (plural pDeclaration)
  <|> try (plural pSignature)
  <|> try pSrcE
  <|> try (plural pExpr)
  where
    plural :: Functor m => m a -> m [a]
    plural = fmap return 

-- | Expressions that are allowed in function or data declarations
pExpr :: Parser ExprI
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
  <|> pListE
  <|> parens pExpr
  <|> pLam
  <|> pVar


pImport :: Parser ExprI
pImport = do
  _ <- reserved "import"
  n <- freename
  imports <-
    optional $
    parens (sepBy pImportTerm (symbol ",")) <|> fmap (\x -> [(x, x)]) pEVar
  exprI . ImpE $
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

pExport :: Parser ExprI
pExport = do
  reserved "export"
  v <- pEVar
  exprI $ ExpE v


pTypedef :: Parser ExprI
pTypedef = try pTypedefType <|> pTypedefObject where

  pTypedefType :: Parser ExprI
  pTypedefType = do
    _ <- reserved "type"
    lang <- optional (try pLang)
    setLang lang
    (v, vs) <- pTypedefTermUnpar <|> pTypedefTermPar
    _ <- symbol "="
    t <- pType
    setLang Nothing
    exprI (TypE v vs t)

  pTypedefObject :: Parser ExprI
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
    exprI (TypE v vs t)

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


pDeclaration :: Parser ExprI
pDeclaration = try pFunctionDeclaration <|> pDataDeclaration
  where

  pDataDeclaration :: Parser ExprI
  pDataDeclaration = do
    v <- pEVar
    _ <- symbol "="
    e <- pExpr
    subExpressions <- option [] $ reserved "where" >> alignInset whereTerm
    exprI $ Declaration v e subExpressions

  pFunctionDeclaration :: Parser ExprI
  pFunctionDeclaration = do
    v <- pEVar
    args <- many1 pEVar
    _ <- symbol "="
    e <- pExpr
    subExpressions <- option [] $ reserved "where" >> alignInset whereTerm
    f <- exprI (LamE args e)
    exprI $ Declaration v f subExpressions

  -- | For now, only type signatures and declarations are allowed in function
  -- where statements. There is no particularly reason why source and imports
  -- could not be here. Exports probably should NOT be allowed since they would
  -- break scope.
  whereTerm :: Parser ExprI
  whereTerm = try pSignature <|> pDeclaration


pSignature :: Parser ExprI
pSignature = do
  label <- tag freename
  v <- freename
  lang <- optional (try pLang)
  setLang lang
  _ <- op "::"
  props <- option [] (try pPropertyList)
  t <- pTypeGen
  constraints <- option [] pConstraints
  setLang Nothing
  exprI $
    Signature
      (EV v)
      label
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


pSrcE :: Parser [ExprI]
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
  mapM exprI [SrcE $ Source { srcName = srcVar
                            , srcLang = language
                            , srcPath = srcFile
                            , srcAlias = aliasVar
                            , srcLabel = label
                            } | (srcVar, aliasVar, label) <- rs]
  where

  pImportSourceTerm :: Parser (Name, EVar, Maybe MT.Text)
  pImportSourceTerm = do
    t <- tag stringLiteral
    n <- stringLiteral
    a <- option n (reserved "as" >> freename)
    return (Name n, EV a, t)

pNamE :: Parser ExprI
pNamE = RecE <$> braces (sepBy1 pNamEntryE (symbol ",")) >>= exprI
  where

  pNamEntryE :: Parser (MT.Text, ExprI)
  pNamEntryE = do
    n <- freename
    _ <- symbol "="
    e <- pExpr
    return (n, e)

pListE :: Parser ExprI
pListE = do
  e <- ListE <$> brackets (sepBy pExpr (symbol ","))
  exprI e

pTuple :: Parser ExprI
pTuple = do
  _ <- symbol "("
  e <- pExpr
  _ <- symbol ","
  es <- sepBy1 pExpr (symbol ",")
  _ <- symbol ")"
  exprI $ TupleE (e : es)

pUni :: Parser ExprI
pUni = symbol "Null" >> exprI UniE

pAcc :: Parser ExprI
pAcc = do
  e <- parens pExpr <|> pNamE <|> pVar
  _ <- symbol "@"
  f <- freename
  exprI $ AccE e f

pAnn :: Parser ExprI
pAnn = do
  e <-
    parens pExpr <|> pVar <|> pListE <|> try pNumE <|> pLogE <|> pStrE
  _ <- op "::"
  t <- pTypeGen
  exprI $ AnnE e [t]

pApp :: Parser ExprI
pApp = do
  f <- parens pExpr <|> pVar
  es <- many1 s
  exprI $ AppE f es
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


pLogE :: Parser ExprI
pLogE = do
  e <- pTrue <|> pFalse
  exprI e
  where
    pTrue = reserved "True" >> return (LogE True)
    pFalse = reserved "False" >> return (LogE False)

pStrE :: Parser ExprI
pStrE = fmap StrE stringLiteral >>= exprI

pNumE :: Parser ExprI
pNumE = fmap NumE number >>= exprI

pLam :: Parser ExprI
pLam = do
  _ <- symbol "\\"
  vs <- many1 pEVar
  _ <- symbol "->"
  e <- pExpr
  exprI $ LamE vs e

pVar :: Parser ExprI
pVar = fmap VarE pEVar >>= exprI


pEVar :: Parser EVar
pEVar = fmap EV freename

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
