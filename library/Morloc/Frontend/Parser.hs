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
    Left err' -> Left err'
    -- all will be ModE expressions, since pTopLevel can return only these
    Right (es, _) -> Right
      $ foldl (\d (k,xs,n) -> Map.insert k (n,xs) d)
              p
              (map AST.findEdges es) -- [(MVar, [(MVar, Import)], ExprI)]
  where
    pstate = emptyState { stateModulePath = f, stateAccepting = True }

-- | Parse a single type. This function used only in debugging in command line
-- calls such as: `morloc typecheck -te "A -> B"`.
readType :: MT.Text -> Either (ParseErrorBundle MT.Text Void) TypeU
readType typeStr =
  case runParser (CMS.runStateT (pTypeGen <* eof) state) "" typeStr of
    Left err' -> Left err'
    Right (es, _) -> Right es
  where
    state = emptyState {stateMinPos = mkPos 1, stateAccepting = True}

-- | The output is rolled into the final DAG of modules
pProgram :: Parser [ExprI]
pProgram = do
  L.space space1 comments empty
  setMinPos
  many1 pToplevel

pToplevel :: Parser ExprI
pToplevel = try pModule <|> pMain

-- | match a named module
pModule :: Parser ExprI
pModule = do
  _ <- reserved "module"
  ess <- align pTopExpr
  moduleName <- freename
  exprI $ ModE (MV moduleName) (concat ess)

-- | match an implicit "main" module
pMain :: Parser ExprI
pMain = do
  ess <- align pTopExpr -- FYI - using `many` rather than `many1` makes infinite loop
  exprI $ ModE (MV "Main") (concat ess)

-- | Expressions including ones that are allowed only at the top-level of a scope
pTopExpr :: Parser [ExprI]
pTopExpr = 
      try (plural pImport)
  <|> try (plural pExport) -- TODO allow many exports from one statement
  <|> try (plural pTypedef)
  <|> try (plural pAssE)
  <|> try (plural pSigE)
  <|> try pSrcE
  <|> (plural pExpr)
  where
    plural :: Functor m => m a -> m [a]
    plural = fmap return 

-- | Expressions that are allowed in function or data declarations
pExpr :: Parser ExprI
pExpr =
      try pAcc
  <|> try pNamE
  <|> try pTupE
  <|> try pUni
  <|> try pAnn
  <|> try pApp
  <|> try pStrE
  <|> try pLogE
  <|> try pNumE
  <|> pLstE
  <|> parens pExpr
  <|> pLam
  <|> pVar
  <?> "Bad expression"


pImport :: Parser ExprI
pImport = do
  _ <- reserved "import"
  n <- freename
  imports <-
    optional $
    parens (sepBy pImportTerm (symbol ",")) <|> fmap (\x -> [(x, x)]) pEVar
  exprI . ImpE $
    Import
      { importModuleName = MV n
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
    o <- pNamType
    lang <- optional (try pLang)
    setLang lang
    (v, vs) <- pTypedefTermUnpar <|> pTypedefTermPar
    _ <- symbol "="
    constructor <- freename <|> stringLiteral
    entries <- braces (sepBy1 pNamEntryU (symbol ",")) >>= mapM (desugarTableEntries lang o)
    let t = NamU o (TV lang constructor) vs entries
    setLang Nothing
    exprI (TypE v vs t)

  desugarTableEntries
    :: Maybe Lang
    -> NamType
    -> (MT.Text, TypeU)
    -> Parser (MT.Text, TypeU)
  desugarTableEntries _ NamRecord entry = return entry
  desugarTableEntries _ NamObject entry = return entry
  desugarTableEntries lang NamTable (k0, t0) = (,) k0 <$> f t0 where
    f :: TypeU -> Parser TypeU
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


pAssE :: Parser ExprI
pAssE = try pFunctionAssE <|> pDataAssE
  where

  pDataAssE :: Parser ExprI
  pDataAssE = do
    v <- pEVar
    _ <- symbol "="
    e <- pExpr
    subExpressions <- option [] $ reserved "where" >> alignInset whereTerm
    exprI $ AssE v e subExpressions

  pFunctionAssE :: Parser ExprI
  pFunctionAssE = do
    v <- pEVar
    args <- many1 pEVar
    _ <- symbol "="
    e <- pExpr
    subExpressions <- option [] $ reserved "where" >> alignInset whereTerm
    f <- exprI (LamE args e)
    exprI $ AssE v f subExpressions

  -- | For now, only type signatures and declarations are allowed in function
  -- where statements. There is no particularly reason why source and imports
  -- could not be here. Exports probably should NOT be allowed since they would
  -- break scope.
  whereTerm :: Parser ExprI
  whereTerm = try pSigE <|> pAssE


pSigE :: Parser ExprI
pSigE = do
  label' <- tag freename
  v <- freename
  lang <- optional (try pLang)
  setLang lang
  _ <- op "::"
  props <- option [] (try pPropertyList)
  t <- pTypeGen
  constraints <- option [] pConstraints
  setLang Nothing
  exprI $
    SigE
      (EV v)
      label'
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
                            , srcLabel = label'
                            } | (srcVar, aliasVar, label') <- rs]
  where

  pImportSourceTerm :: Parser (Name, EVar, Maybe MT.Text)
  pImportSourceTerm = do
    t <- tag stringLiteral
    n <- stringLiteral
    a <- option n (reserved "as" >> freename)
    return (Name n, EV a, t)


pLstE :: Parser ExprI
pLstE = (LstE <$> brackets (sepBy pExpr (symbol ","))) >>= exprI


pTupE :: Parser ExprI
pTupE = do
  _ <- symbol "("
  e <- pExpr
  _ <- symbol ","
  es <- sepBy1 pExpr (symbol ",")
  _ <- symbol ")"
  exprI $ TupE (e:es)


pNamE :: Parser ExprI
pNamE = do
  rs <- braces (sepBy1 pNamEntryE (symbol ","))
  -- FIXME - making records without constructors is a bit sketch, for now it is
  -- allowed (and heavily tested) and I will leave it for the moment. But
  -- eventually the syntax should be `Person {Age = 5, Name = "Juicebox"}` or
  -- whatever.
  exprI $ NamE rs

pNamEntryE :: Parser (MT.Text, ExprI)
pNamEntryE = do
  n <- freename
  _ <- symbol "="
  e <- pExpr
  return (n, e)


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
    parens pExpr <|> pVar <|> pLstE <|> try pNumE <|> pLogE <|> pStrE
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
      <|> pLstE
      <|> pTupE
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

pTypeGen :: Parser TypeU
pTypeGen = do
  resetGenerics
  t <- pType
  s <- CMS.get
  return $ forallWrap (unique (reverse (stateGenerics s))) t
  where
    forallWrap :: [TVar] -> TypeU -> TypeU
    forallWrap [] t = t
    forallWrap (v:vs) t = ForallU v (forallWrap vs t)


pType :: Parser TypeU
pType =
      pExistential
  <|> try pFunU
  <|> try pUniU
  <|> try pNamU
  <|> try pAppU
  <|> try parensType
  <|> pListU
  <|> pTupleU
  <|> pVarU

pUniU :: Parser TypeU
pUniU = do
  _ <- symbol "("
  _ <- symbol ")"
  lang <- CMS.gets stateLang
  v <- newvar lang
  return (ExistU v [] (MLD.defaultNull lang))

parensType :: Parser TypeU
parensType = do
  _ <- tag (symbol "(")
  t <- parens pType
  return t

pTupleU :: Parser TypeU
pTupleU = do
  lang <- CMS.gets stateLang
  _ <- tag (symbol "(")
  ts <- parens (sepBy1 pType (symbol ","))
  return $ head (MLD.defaultTuple lang ts)

-- A naked record with default constructor.  Currently this is legal in a
-- signature, but it probably shouldn't be (it isn't in haskell), instead it
-- should only be used in type definitions (pTypeDef).
pNamU :: Parser TypeU
pNamU = do
  _ <- tag (symbol "{")
  entries <- braces (sepBy1 pNamEntryU (symbol ","))
  lang <- CMS.gets stateLang
  return $ head (MLD.defaultRecord lang entries)



pNamEntryU :: Parser (MT.Text, TypeU)
pNamEntryU = do
  n <- freename
  _ <- op "::"
  t <- pType
  return (n, t)

pExistential :: Parser TypeU
pExistential = do
  v <- angles freename
  return (ExistU (TV Nothing v) [] [])

pAppU :: Parser TypeU
pAppU = do
  _ <- tag (freename <|> stringLiteral)
  n <- freename <|> stringLiteral
  t <- tvar n
  args <- many1 pType'
  return $ AppU t args
  where
    pType' = try pUniU <|> try parensType <|> pVarU <|> pListU <|> pTupleU <|> pNamU

pFunU :: Parser TypeU
pFunU = do
  t <- pType'
  _ <- op "->"
  ts <- sepBy1 (pType') (op "->")
  return $ FunU ts t
  where
    pType' = try pUniU <|> try parensType <|> try pAppU <|> pVarU <|> pListU <|> pTupleU <|> pNamU

pListU :: Parser TypeU
pListU = do
  _ <- tag (symbol "[")
  t <- brackets pType
  lang <- CMS.gets stateLang
  return $ head (MLD.defaultList lang t)

pVarU :: Parser TypeU
pVarU = try pVarConU <|> pVarGenU where
  pVarConU :: Parser TypeU
  pVarConU = do
    _ <- tag stringLiteral
    n <- stringLiteral
    t <- tvar n
    return $ VarU t

  pVarGenU :: Parser TypeU
  pVarGenU = do
    _ <- tag freename
    n <- freename
    t <- tvar n
    appendGenerics t  -- add the term to the generic list IF generic
    return $ VarU t
