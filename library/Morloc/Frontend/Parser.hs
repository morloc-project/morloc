{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Frontend.Parser
Description : Full parser for Morloc
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc.Frontend.Parser
  ( readProgram
  , readType
  ) where

import qualified Control.Monad.State as CMS
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Void (Void)
import qualified Morloc.BaseTypes as BT
import qualified Morloc.Data.Map as Map
import qualified Morloc.Data.Text as MT
import qualified Morloc.Frontend.AST as AST
import Morloc.Frontend.Lexer
import Morloc.Frontend.Namespace
import qualified Morloc.System as MS
import Text.Megaparsec hiding (Label)
import Text.Megaparsec.Char hiding (eol)
import qualified Text.Megaparsec.Char.Lexer as L

-- | Internal type for do-block statement parsing
data DoStmt = DoBind EVar ExprI | DoBare ExprI | DoReturn ExprI

{- | Parse a single file or string that may contain multiple modules. Each
module is written written into the DAG of previously observed modules.
-}
readProgram ::
  -- | The expected module name,
  Maybe MVar ->
  -- | An optional path to the file the source code was read from. If no path
  -- is given, then the source code was provided as a string.
  Maybe Path ->
  -- | Source code
  Text ->
  ParserState ->
  -- | Possibly empty directed graph of previously observed modules
  DAG MVar Import ExprI ->
  Either (ParseErrorBundle Text Void) (DAG MVar Import ExprI, ParserState)
readProgram moduleName modulePath sourceCode pstate p =
  case runParser
    (CMS.runStateT (sc >> pProgram moduleName <* eof) (reenter modulePath pstate))
    (fromMaybe "<expr>" modulePath)
    sourceCode of
    (Left err') -> Left err'
    -- all will be ModE expressions, since pTopLevel can return only these
    (Right (es, s)) ->
      let dag = foldl (\d (k, xs, n) -> Map.insert k (n, xs) d) p (map AST.findEdges es)
       in Right (dag, s)

{- | Parse a single type. This function used only in debugging in command line
calls such as: `morloc typecheck -te "A -> B"`.
-}
readType :: Text -> Either (ParseErrorBundle Text Void) TypeU
readType typeStr =
  case runParser (CMS.runStateT (sc >> pTypeGen <* eof) (reenter Nothing emptyState)) "" typeStr of
    Left err' -> Left err'
    Right (es, _) -> Right es

-- prepare the state for reading of a new file (keeping past counters)
reenter :: Maybe Path -> ParserState -> ParserState
reenter f p = p {stateMinPos = mkPos 1, stateAccepting = True, stateModulePath = f}

{- | The output will be rolled into the final DAG of modules. There may be
EITHER one implicit main module OR one or more named modules.
-}
pProgram ::
  -- | The expected module path (fail if it doesn't match)
  Maybe MVar ->
  Parser [ExprI]
pProgram m = try (align (pModule m)) <|> plural pMain

-- | match a named module
pModule ::
  -- | The expected module path
  Maybe MVar ->
  Parser ExprI
pModule expModuleName = do
  pos <- getSourcePos
  _ <- reserved "module"

  moduleName <- case expModuleName of
    Nothing -> MT.intercalate "." <$> sepBy moduleComponent (symbol ".")
    (Just (MV n)) -> symbol n

  export <-
    parens
      ( (char '*' >> return ExportAll)
          <|> (sepBy pIndexedSymbol (symbol ",") |>> ExportMany . Set.fromList)
      )

  expE <- exprIAt pos (ExpE export)

  es <- align pTopExpr |>> concat

  exprIAt pos $ ModE (MV moduleName) (expE : es)

pIndexedSymbol :: Parser (Int, Symbol)
pIndexedSymbol = do
  pos <- getSourcePos
  sym <- pSymbol
  i <- exprIdAt pos
  return (i, sym)

-- | match an implicit Main module
pMain :: Parser ExprI
pMain = do
  pos <- getSourcePos
  setMinPos
  es <- align pTopExpr |>> concat >>= createMainFunction
  exprIAt pos $ ModE (MV "main") es

plural :: (Functor m) => m a -> m [a]
plural = fmap return

createMainFunction :: [ExprI] -> Parser [ExprI]
createMainFunction es = case (init es, last es) of
  (_, ExprI _ (ModE _ _)) -> return es
  (_, ExprI _ TypE {}) -> return es
  (_, ExprI _ (ImpE _)) -> return es
  (_, ExprI _ (SrcE _)) -> return es
  (_, ExprI _ (SigE _)) -> return es
  (_, ExprI _ AssE {}) -> return es
  (_, ExprI _ (ExpE _)) -> return es
  (rs, terminalExpr) -> do
    i <- exprId
    expMain <- exprI $ ExpE (ExportMany $ Set.singleton (i, TermSymbol (EV "__main__")))
    assMain <- exprI $ AssE (EV "__main__") terminalExpr []
    return $ expMain : (assMain : rs)

-- | Expressions including ones that are allowed only at the top-level of a scope
pTopExpr :: Parser [ExprI]
pTopExpr =
  try (plural pImport)
    <|> try (plural pTypedef)
    <|> try (plural pTypeclass)
    <|> try (plural pInstance)
    <|> try (plural pFixity)
    <|> try (plural pAssE)
    <|> try (plural pSigE)
    <|> try pSourceLegacy
    <|> try pSourceNew
    <|> plural pExpr
    <?> "statement"

-- | Expressions that are allowed in function or data declarations
pExpr :: Parser ExprI
pExpr = (do
  pos <- getSourcePos
  e <- try pLetE <|> try pLam <|> pInfixExpr
  -- optional type annotation suffix: e :: Type
  mayAnn <- optional (op "::")
  case mayAnn of
    Just _ -> do
      t <- pTypeGen
      exprIAt pos $ AnnE e t
    Nothing -> return e
  ) <?> "expression"

-- Either a lowercase term name or an uppercase type/class name
pSymbol :: Parser Symbol
pSymbol =
  try (TermSymbol <$> parenOperator)  -- Operators in parens: (+), (*), etc.
    <|> (TermSymbol . EV <$> freenameL)  -- Regular term names
    <|> (TypeSymbol . TV <$> freenameU)  -- Type names

pImport :: Parser ExprI
pImport = do
  pos <- getSourcePos
  _ <- reserved "import"
  -- There may be '.' in import names, these represent folders/namespaces of modules
  n <- MT.intercalate "." <$> sepBy moduleComponent (symbol ".")
  imports <-
    optional $
      parens (sepBy pImportItem (symbol ","))
  exprIAt pos . ImpE $
    Import
      { importModuleName = MV n
      , importInclude = imports
      , importExclude = []
      , importNamespace = Nothing
      }
  where
    pImportItem :: Parser AliasedSymbol
    pImportItem = pImportType <|> pImportTerm

    pImportTerm :: Parser AliasedSymbol
    pImportTerm = do
      n <- try pOpOrName
      a <- option n (reserved "as" >> pOpOrName)
      return (AliasedTerm (EV n) (EV a))

    pOpOrName = (parenOperator |>> unEVar) <|> freenameL

    pImportType :: Parser AliasedSymbol
    pImportType = do
      n <- freenameU
      a <- option n (reserved "as" >> freenameU)
      return (AliasedType (TV n) (TV a))

-- example:
--   class Show a where
--     show a :: a -> Str
pTypeclass :: Parser ExprI
pTypeclass = do
  pos <- getSourcePos
  _ <- reserved "class"
  constraints <- option [] (try pConstraintList)
  (TV v, vs) <- pTypedefTerm' <|> parens pTypedefTerm'
  sigs <- option [] (reserved "where" >> alignInset pSignature)
  exprIAt pos . ClsE $ Typeclass constraints (ClassName v) vs sigs
  where
    -- parses "Show a" from above example
    pTypedefTerm' :: Parser (TVar, [TVar])
    pTypedefTerm' = do
      t <- freenameU
      ts <- many freenameL -- The parameters must currently be generic, hence "L" for lowercase
      return (TV t, map TV ts)

pInstance :: Parser ExprI
pInstance = do
  pos <- getSourcePos
  _ <- reserved "instance"
  v <- freenameU
  ts <- many1 pTypeGen
  es <- option [] (reserved "where" >> alignInset pInstanceExpr) |>> concat
  exprIAt pos $ IstE (ClassName v) ts es
  where
    pInstanceExpr :: Parser [ExprI]
    pInstanceExpr
      =   try pSourceLegacy
      <|> try pSourceNew
      <|> (pAssE |>> return)

-- | Parse fixity declaration: infixl 6 +, -
pFixity :: Parser ExprI
pFixity = do
  pos <- getSourcePos
  assoc <- pAssociativity
  prec <- pPrecValue
  when (prec > 9) $
    fail "precedence must be between 0 and 9"
  ops <- sepBy1 pOperatorName (symbol ",")
  exprIAt pos . FixE $ Fixity assoc prec ops
  where
    pAssociativity :: Parser Associativity
    pAssociativity =
      (reserved "infixl" >> return InfixL)
        <|> (reserved "infixr" >> return InfixR)
        <|> (reserved "infix" >> return InfixN)

    pOperatorName :: Parser EVar
    pOperatorName =
      try parenOperator -- symbolic operators in parens: (+)
        <|> try (operatorName |>> EV) -- symbolic operators bare: +
        <|> (freenameL |>> EV) -- alphanumeric operators: div

    pPrecValue :: Parser Int
    pPrecValue = do
      prec <- lexeme L.decimal
      when (prec > 9) $
        fail "precedence must be between 0 and 9"
      return prec

pTypedef :: Parser ExprI
pTypedef =
  try pTypedefType
    <|> try pTypedefObjectLegacy
    <|> pTypedefObject
  where
    pConcreteType = do
      t <- pTypeCon
      return (t, True)

    pConcreteVar = do
      v <- stringLiteral
      return (v, True)

    pGeneralType = do
      (_, t) <- pType
      return (t, False)

    pGeneralVar = do
      v <- freename
      return (v, False)

    pTypedefType :: Parser ExprI
    pTypedefType = do
      pos <- getSourcePos
      doc <- parseArgDocVars |>> ArgDocAlias
      _ <- reserved "type"
      mayLang <- optional (try pLangNamespace)
      (v, vs) <- pTypedefTerm <|> parens pTypedefTerm
      case mayLang of
        (Just lang) -> do
          _ <- symbol "="
          (t, isTerminal) <- pConcreteType <|> pGeneralType
          exprIAt pos (TypE (ExprTypeE (Just (lang, isTerminal)) v vs t doc))
        Nothing -> do
          mayT <- optional (symbol "=" >> pType)
          case (vs, mayT) of
            (_, Just (_, t)) -> exprIAt pos (TypE (ExprTypeE Nothing v vs t doc))
            ([], Nothing) -> exprIAt pos (TypE (ExprTypeE Nothing v vs (VarU v) doc))
            (_, Nothing) -> exprIAt pos (TypE (ExprTypeE Nothing v vs (AppU (VarU v) (map (either (VarU) id) vs)) doc))

    pTypedefObjectLegacy :: Parser ExprI
    pTypedefObjectLegacy = do
      pos <- getSourcePos
      doc <- parseArgDocVars
      o <- pNamType
      mayLang <- optional (try pLangNamespace)
      (v, vs) <- pTypedefTerm <|> parens pTypedefTerm
      _ <- symbol "="
      (con, k) <- case mayLang of
        (Just lang) -> do
          (constructor, isTerminal) <- pConcreteVar <|> pGeneralVar
          return (constructor, Just (lang, isTerminal))
        Nothing -> do
          constructor <- freename
          return (constructor, Nothing)
      entries <-
        option [] (braces (sepBy1 pNamEntryU (symbol ",")))
          >>= mapM (secondM (desugarTableEntries o))

      -- The vs are the parameters of the object. In C++ they are the required
      -- template parameters (e.g., A and B in Obj<A,B>). I have to maintain them
      -- as an ordered list all the way to code generation.
      let t = NamU o (TV con) (map (either VarU id) vs) (map snd entries)
          objDoc = ArgDocRec doc [(fieldKey, arg) | (arg, (fieldKey, _)) <- entries]
      exprIAt pos (TypE (ExprTypeE k v vs t objDoc))

    pTypedefObject :: Parser ExprI
    pTypedefObject = do
      pos <- getSourcePos
      recDoc <- parseArgDocVars
      o <- pNamType
      (v, vs) <- pTypedefTerm <|> parens pTypedefTerm
      reserved "where"
      entries <-
        alignInset pNamEntryU
          >>= (mapM (secondM (desugarTableEntries o)))
      let docEntries = [(k, r) | (r, (k, _)) <- entries]
      let grpArg = ArgDocRec recDoc docEntries
      let t = NamU o v (map (either VarU id) vs) (map snd entries)
      exprIAt pos (TypE (ExprTypeE Nothing v vs t grpArg))

    -- TODO: is this really the right place to be doing this?
    desugarTableEntries ::
      NamType ->
      (Key, TypeU) ->
      Parser (Key, TypeU)
    desugarTableEntries NamRecord entry = return entry
    desugarTableEntries NamObject entry = return entry
    desugarTableEntries NamTable (k0, t0) = (,) k0 <$> f t0
      where
        f :: TypeU -> Parser TypeU
        f (ForallU v t) = ForallU v <$> f t
        f t = return $ BT.listU t

    pNamType :: Parser NamType
    pNamType = pNamObject <|> pNamTable <|> pNamRecord

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

    pLangNamespace :: Parser Lang
    pLangNamespace = do
      lang <- pLang
      _ <- symbol "=>"
      return lang

-- Typedef terms may be fully or partially generic.
-- examples:
--   Map a b
--   Map Str b
pTypedefTerm :: Parser (TVar, [Either TVar TypeU])
pTypedefTerm = do
  t <- freenameU
  ts <- many (fmap (Left . TV) freenameL <|> fmap Right (try pType |>> snd))
  return (TV t, ts)

pAssE :: Parser ExprI
pAssE = try pFunctionAssE <|> pDataAssE
  where
    -- The name pDataAssE is a deceptive. The right hand value is not necessarily
    -- data, it may be an unapplied function that will need to undergo eta
    -- expansion later.
    pDataAssE :: Parser ExprI
    pDataAssE = do
      pos <- getSourcePos
      v <- pEVarOrOp
      _ <- symbol "="
      e <- pExpr
      subExpressions <- option [] $ reserved "where" >> alignInset whereTerm
      exprIAt pos $ AssE v e subExpressions

    pFunctionAssE :: Parser ExprI
    pFunctionAssE = do
      pos <- getSourcePos
      v <- pEVarOrOp
      posArgs <- getSourcePos
      args <- many1 pEVar
      _ <- symbol "="
      e <- pExpr
      subExpressions <- option [] $ reserved "where" >> alignInset whereTerm
      f <- exprIAt posArgs (LamE args e)
      exprIAt pos $ AssE v f subExpressions

    -- \| For now, only type signatures and declarations are allowed in function
    -- where statements. There is no particularly reason why source and imports
    -- could not be here. Exports probably should NOT be allowed since they would
    -- break scope.
    whereTerm :: Parser ExprI
    whereTerm = try pSigE <|> pAssE

pSigE :: Parser ExprI
pSigE = do
  pos <- getSourcePos
  signature <- pSignature
  exprIAt pos . SigE $ signature

pSignature :: Parser Signature
pSignature = do
  doc <- parseArgDocVars
  label' <- optional pTag
  v <- pEVarOrOp
  vs <- many freenameL |>> map TV
  _ <- op "::"
  constraints <- option [] (try pConstraintList)

  (docs, t') <- pTypeDoc

  let cmdDoc = ArgDocSig doc (init docs) (last docs)

  let t = forallWrap vs t'
      et =
        EType
          { etype = t
          , econs = Set.fromList constraints
          , edocs = cmdDoc
          }
      sig = Signature v (Label <$> label') et

  return sig

pConstraintList :: Parser [Constraint]
pConstraintList = do
  cs <- try (parens (sepBy1 pSingleConstraint (symbol ",")))
          <|> (pSingleConstraint |>> return)
  _ <- op "=>"
  return cs

pSingleConstraint :: Parser Constraint
pSingleConstraint = do
  cls <- freenameU
  ts <- many1 pConstraintTypeArg
  return $ Constraint (ClassName cls) ts

pConstraintTypeArg :: Parser TypeU
pConstraintTypeArg = try pUniU <|> try pThunkU <|> try parensType <|> try pVarU <|> try pListU <|> pTupleU

parseArgDocVars :: Parser ArgDocVars
parseArgDocVars = indentFreeTerm $ foldMany defaultValue parseArgDocVar

parseArgDocVar :: ArgDocVars -> Parser ArgDocVars
parseArgDocVar d =
  try (parseWordDocStr "name" |>> (\x -> d {docName = Just x}))
    <|> try (parseFlagDocStr "literal" |>> (\x -> d {docLiteral = Just x}))
    <|> try (parseFlagDocStr "unroll" |>> (\x -> d {docUnroll = Just x}))
    <|> try (parseTextDocStr "default" |>> (\x -> d {docDefault = Just x}))
    <|> try (parseWordDocStr "metavar" |>> (\x -> d {docMetavar = Just x}))
    <|> try (parseArgDocStr "arg" |>> (\x -> d {docArg = Just x}))
    <|> try (parseArgDocStr "true" |>> (\x -> d {docTrue = Just x}))
    <|> try (parseArgDocStr "false" |>> (\x -> d {docFalse = Just x}))
    <|> try (parseTextDocStr "return" |>> (\x -> d {docReturn = Just x}))
    <|> (parseLineDocStr |>> (\x -> d {docLines = docLines d <> [x]}))

pSourceLegacy :: Parser [ExprI]
pSourceLegacy = do
  _ <- reserved "source"
  modulePath <- CMS.gets stateModulePath
  language <- pLang
  maySrcfile <- optional (reserved "from" >> stringLiteral |>> MT.unpack)
  rs <- parens (sepBy1 pImportSourceTerm (symbol ","))
  let srcfile = getSourceFile modulePath maySrcfile
  mapM (\ (pos, srcVar, aliasVar, label') -> exprIAt pos . SrcE $
      Source
      { srcName = srcVar
      , srcLang = language
      , srcPath = srcfile
      , srcAlias = aliasVar
      , srcLabel = Label <$> label'
      , srcRsize = []
      , srcNote = []
      }
    ) rs
  where
    pImportSourceTerm :: Parser (SourcePos, SrcName, EVar, Maybe Text)
    pImportSourceTerm = do
      t <- optional pTag
      posSrc <- getSourcePos
      n <- stringLiteral
      (posAlias, a) <- option (posSrc, n) (
        reserved "as" >>
          ( (,) <$> getSourcePos
                <*> (freename <|> (parenOperator |>> unEVar))))
      return (posAlias, SrcName n, EV a, t)

pSourceNew :: Parser [ExprI]
pSourceNew = do
  modulePath <- CMS.gets stateModulePath
  _ <- reserved "source"
  language <- pLang
  maySrcfile <- optional (reserved "from" >> stringLiteral |>> MT.unpack)
  let srcfile = getSourceFile modulePath maySrcfile
  option [] (reserved "where" >> alignInset (pImportSourceTerm language srcfile))
  where
    pImportSourceTerm :: Lang -> Maybe Path -> Parser ExprI
    pImportSourceTerm language srcfile = do
      src <-
        parseSourceDocstrs $
          Source
            { srcName = SrcName ""
            , srcLang = language
            , srcPath = srcfile
            , srcAlias = EV ""
            , srcLabel = Nothing
            , srcRsize = []
            , srcNote = []
            }
      pos <- getSourcePos
      n <- freename
      let srcname =
            if srcName src == SrcName ""
              then SrcName n
              else srcName src
      exprIAt pos . SrcE $ src {srcName = srcname, srcAlias = EV n}

    parseSourceDocstrs :: Source -> Parser Source
    parseSourceDocstrs src = indentFreeTerm $ foldMany src parseSourceDocstr

    parseSourceDocstr :: Source -> Parser Source
    parseSourceDocstr src =
      try (parseWordDocStr "name" |>> (\x -> src {srcName = SrcName x}))
        <|> try (parseIntsDocStr "rsize" |>> (\xs -> src {srcRsize = xs}))
        <|> (parseLineDocStr |>> (\x -> src {srcNote = srcNote src <> [x]}))

getSourceFile :: Maybe Path -> Maybe Path -> Maybe Path
getSourceFile modulePath srcFile =
  case (modulePath, srcFile) of
    -- build a path to the source file by searching
    -- > source "R" from "foo.R" ("Foo" as foo, "bar")
    (Just f, Just srcfile') -> Just $ MS.combine (MS.takeDirectory f) srcfile'
    -- we are sourcing from the language base
    -- > source "R" ("sqrt", "t.test" as t_test)
    (Just _, Nothing) -> Nothing
    -- this case SHOULD only occur in testing where the source file does not exist
    -- file non-existence will be caught later
    (Nothing, s) -> s

pLstE :: Parser ExprI
pLstE = do
  pos <- getSourcePos
  brackets (sepBy pExpr (symbol ",")) >>= exprIAt pos . LstE

pNamE :: Parser ExprI
pNamE = do
  pos <- getSourcePos
  rs <- braces (sepBy1 pNamEntryE (symbol ","))
  -- FIXME - making records without constructors is a bit sketch, for now it is
  -- allowed (and heavily tested) and I will leave it for the moment. But
  -- eventually the syntax should be `Person {Age = 5, Name = "Juicebox"}` or
  -- whatever. The hell it should fucking dimwit. Constructors are shit.
  exprIAt pos $ NamE (map (first Key) rs)

pNamEntryE :: Parser (Text, ExprI)
pNamEntryE = do
  n <- freenameL
  _ <- symbol "="
  e <- pExpr
  return (n, e)

-- | Parse any parenthesized expression: (), (e), (e, e, ...), or (op)
-- This avoids exponential backtracking by parsing ( once and then deciding.
pParenExpr :: Parser ExprI
pParenExpr = do
  pos <- getSourcePos
  _ <- symbol "("
  -- Unit: ()
  try (symbol ")" >> exprIAt pos UniE)
    -- Operator in parens: (+)
    <|> try (do
      v <- operatorName
      _ <- symbol ")"
      exprIAt pos $ VarE defaultValue (EV v))
    -- Expression, then check for tuple or grouping
    <|> do
      e <- pExpr
      -- Tuple: (e, e, ...)
      try (do
        _ <- symbol ","
        es <- sepBy1 pExpr (symbol ",")
        _ <- symbol ")"
        exprIAt pos $ TupE (e : es))
        -- Grouping: (e)
        <|> (symbol ")" >> return e)

-- | Parse an expression that may contain infix operators
-- Note that the fixity of the binops is not yet known, so they are merged into
-- a left-associated temporary structure
pInfixExpr :: Parser ExprI
pInfixExpr = do
  pos <- getSourcePos
  lhs <- pOperand  -- Try function application first, then atoms
  opPos <- getSourcePos
  mayOp <- optional pInfixOperator
  case mayOp of
    (Just binop) -> do
      rhs <- pExpr
      i <- exprIdAt opPos
      exprIAt pos $ BopE lhs i binop rhs
    Nothing -> return lhs

-- | Parse an infix operator (not in parens)
pInfixOperator :: Parser EVar
pInfixOperator = EV <$> operatorName

-- | Parse an operand
pOperand :: Parser ExprI
pOperand =
      try pApp
    <|> try pForceE
    <|> try pDoBlock
    <|> try pHolE
    <|> try pNumE
    <|> try pLogE
    <|> try pStrE
    <|> try pParenExpr -- handles (), (e), (e,e,..), (+)
    <|> try pLstE
    <|> try pSuspendE
    <|> try pNamE
    <|> try pSetter
    <|> try pGetter
    <|>     pVar
    <?> "operand"

pApp :: Parser ExprI
pApp = do
  pos <- getSourcePos
  f <- parseFun
  es <- many1 parseArg
  exprIAt pos $ AppE f es
  where
    parseFun =
            try pForceE
        <|> try pVar
        <|> try pLstE
        <|> try pSuspendE
        <|> try pNamE
        <|> try pSetter
        <|> try pGetter
        <|> try pParenExpr -- handles (), (e), (e,e,..), (+)
    parseArg =
            try pForceE
        <|> try pParenExpr -- handles (), (e), (e,e,..), (+)
        <|> try pSetter
        <|> try pGetter
        <|> try pStrE
        <|> try pLogE
        <|> try pNumE
        <|> try pLstE
        <|> try pSuspendE
        <|> try pNamE
        <|> try pVar
        <|> pHolE

pLogE :: Parser ExprI
pLogE = do
  pos <- getSourcePos
  e <- pTrue <|> pFalse
  exprIAt pos e
  where
    pTrue = reserved "True" >> return (LogE True)
    pFalse = reserved "False" >> return (LogE False)

pStrE :: Parser ExprI
pStrE = do
  pos <- getSourcePos
  -- (Either Text (Text, [(a, Text)]))
  eitherS <- stringPatterned pExpr
  case eitherS of
    (Left txt) -> exprIAt pos . StrE $ txt
    (Right (s, es)) -> do
      pattern <- exprIAt pos . PatE $ PatternText s (map snd es)
      exprIAt pos $ AppE pattern (map fst es)

pSetter :: Parser ExprI
pSetter = do
  pos <- getSourcePos
  -- parse the setter pattern
  -- for example: .(x.0 = 1, y.a = 2, z = 3)
  --  ss: the selectors, in this case the pattern .(x.0, y.a, z)
  --  es: a list of expressions: [1,2,3]
  (s, es) <- parsePatternSetter pExpr
  setter <- exprIAt pos $ PatE (PatternStruct s)

  -- fresh indices for the lambda and application expressions we'll create
  idxLam <- exprIdAt pos

  -- a unique name for the lambda-bound datastructure variable
  let v = EV (".setter_" <> MT.show' idxLam)

  -- a variable to store the datastructure that will be passed to the lambda
  vArg <- exprIAt pos $ VarE defaultValue v

  -- apply arguments to the setter
  -- first the datastructure and then all setting values
  setterApp <- exprIAt pos $ AppE setter (vArg : es)

  return $ ExprI idxLam (LamE [v] setterApp)

pGetter :: Parser ExprI
pGetter = do
  pos <- getSourcePos
  s <- parsePatternGetter
  exprIAt pos $ PatE (PatternStruct s)

pNumE :: Parser ExprI
pNumE = do
  pos <- getSourcePos
  x <- number
  exprIAt pos $ case x of
    (Left i) -> IntE i
    (Right f) -> RealE f

pLetE :: Parser ExprI
pLetE = do
  pos <- getSourcePos
  bindings <- many1 pLetBinding
  _ <- reserved "in"
  body <- pExpr
  exprIAt pos $ LetE bindings body

pLetBinding :: Parser (EVar, ExprI)
pLetBinding = do
  _ <- reserved "let"
  v <- try (hole >> return (EV "_")) <|> pEVar
  _ <- symbol "="
  e <- pExpr
  return (v, e)

pLam :: Parser ExprI
pLam = do
  pos <- getSourcePos
  _ <- symbol "\\"
  vs <- many1 pEVar
  _ <- symbol "->"
  e <- pExpr
  exprIAt pos $ LamE vs e

pVar :: Parser ExprI
pVar = do
  pos <- getSourcePos
  labels <- pTags

  s <- CMS.get

  let baseConfig = maybe defaultValue id (moduleConfigDefaultGroup (stateModuleConfig s))
      configMap = moduleConfigLabeledGroups (stateModuleConfig s)
      varConfigs = catMaybes [Map.lookup x configMap | x <- labels]
      manifoldConfig = foldl mergeConfigs baseConfig varConfigs

  v <- pEVar
  exprIAt pos $ VarE manifoldConfig v
  where
    -- The the rightmost (the newer) value
    useRight :: Maybe a -> Maybe a -> Maybe a
    useRight _ (Just x) = Just x
    useRight x _ = x

    -- Merge manifold configs, replace default values with particular values
    mergeConfigs :: ManifoldConfig -> ManifoldConfig -> ManifoldConfig
    mergeConfigs (ManifoldConfig c1 b1 r1) (ManifoldConfig c2 b2 r2) =
      ManifoldConfig (useRight c1 c2) (useRight b1 b2) (mergeResources r1 r2)

    mergeResources :: Maybe RemoteResources -> Maybe RemoteResources -> Maybe RemoteResources
    mergeResources Nothing x = x
    mergeResources x Nothing = x
    mergeResources (Just (RemoteResources r1 m1 t1 g1)) (Just (RemoteResources r2 m2 t2 g2)) =
      Just $ RemoteResources (useRight r1 r2) (useRight m1 m2) (useRight t1 t2) (useRight g1 g2)

-- | Parse a force expression: !e
-- The ! binds tighter than any infix operator (parsed at operand level).
-- Allow !! for chained force but disallow other operator chars after ! (e.g. !=).
pForceE :: Parser ExprI
pForceE = do
  pos <- getSourcePos
  _ <- try (char '!' <* notFollowedBy (oneOf (filter (/= '!') operatorChars)))
  sc  -- consume whitespace after !
  e <- pForceArg
  exprIAt pos $ ForceE e
  where
    -- force argument: same as parseArg but also allows force (for !!x)
    pForceArg =
          try pForceE
      <|> try pDoBlock
      <|> try pParenExpr
      <|> try pSetter
      <|> try pGetter
      <|> try pStrE
      <|> try pLogE
      <|> try pNumE
      <|> try pLstE
      <|> try pNamE
      <|> try pSuspendE
      <|> try pVar
      <|> pHolE

-- | Parse a suspend expression: {e}
-- Disambiguated from record expressions by checking that the content after {
-- is not "name = expr".
pSuspendE :: Parser ExprI
pSuspendE = do
  pos <- getSourcePos
  _ <- symbol "{"
  -- fail if this looks like a record: identifier followed by =
  notFollowedBy (try (freenameL >> symbol "="))
  e <- pExpr
  _ <- symbol "}"
  exprIAt pos $ SuspendE e

-- | Parse a do-block, desugared to suspend + let + force
-- do
--   print "start"       -- bare statement: let _ = !(print "start")
--   x <- rnorm 0 1      -- bind: let x = !(rnorm 0 1)
--   return (x + 1)       -- return: x + 1 (no force, becomes body)
-- The whole block is wrapped in SuspendE.
pDoBlock :: Parser ExprI
pDoBlock = do
  pos <- getSourcePos
  _ <- reserved "do"
  stmts <- alignInset pDoStmt
  case stmts of
    [] -> fail "empty do block"
    _ -> do
      body <- desugarDo pos stmts
      exprIAt pos $ SuspendE body
  where
    pDoStmt :: Parser DoStmt
    pDoStmt =
          try pDoBind
      <|> try pDoReturn
      <|> pDoBare

    pDoBind :: Parser DoStmt
    pDoBind = do
      v <- pEVar
      _ <- symbol "<-"
      e <- pExpr
      return $ DoBind v e

    pDoReturn :: Parser DoStmt
    pDoReturn = do
      _ <- reserved "return"
      e <- pExpr
      return $ DoReturn e

    pDoBare :: Parser DoStmt
    pDoBare = DoBare <$> pExpr

    desugarDo :: SourcePos -> [DoStmt] -> Parser ExprI
    -- final bare expression: force it
    desugarDo p [DoBare e] = exprIAt p $ ForceE e
    -- final return expression: no force
    desugarDo _ [DoReturn e] = return e
    -- final bind: error
    desugarDo _ [DoBind _ _] = fail "do block cannot end with a bind (<-)"
    -- non-final bind: let x = !e in rest
    desugarDo p (DoBind v e : rest) = do
      forceE <- exprIAt p $ ForceE e
      restE <- desugarDo p rest
      exprIAt p $ LetE [(v, forceE)] restE
    -- non-final bare: let _ = !e in rest (use fresh discard name)
    desugarDo p (DoBare e : rest) = do
      idx <- exprIdAt p
      let discardVar = EV ("_do_" <> MT.show' idx)
      forceE <- exprIAt p $ ForceE e
      restE <- desugarDo p rest
      exprIAt p $ LetE [(discardVar, forceE)] restE
    -- non-final return: error
    desugarDo _ (DoReturn _ : _) = fail "return must be the last statement in a do block"
    desugarDo _ [] = fail "empty do block"

pHolE :: Parser ExprI
pHolE = do
  pos <- getSourcePos
  hole
  exprIAt pos HolE

pEVarOrOp :: Parser EVar
pEVarOrOp = try parenOperator <|> pEVar

pEVar :: Parser EVar
pEVar = fmap EV freenameL

pTypeGen :: Parser TypeU
pTypeGen = do
  resetGenerics
  (_, t) <- pType
  s <- CMS.get
  return $ forallWrap (unique (reverse (stateGenerics s))) t

forallWrap :: [TVar] -> TypeU -> TypeU
forallWrap [] t = t
forallWrap (v : vs) t = ForallU v (forallWrap vs t)

pTypeCon :: Parser TypeU
pTypeCon = try pAppUCon <|> pVarUCon

pAppUCon :: Parser TypeU
pAppUCon = do
  t <- pVarUCon
  args <- many1 pType'
  return $ AppU t args
  where
    pType' = try pUniU <|> try pThunkU <|> try parensType <|> pVarU <|> pListU <|> pTupleU

pVarUCon :: Parser TypeU
pVarUCon = VarU <$> pTermCon

pTermCon :: Parser TVar
pTermCon = do
  _ <- optional pTag
  TV <$> stringLiteral

pTypeDoc :: Parser ([ArgDocVars], TypeU)
pTypeDoc = try pFunUDoc <|> (pType |>> first return)

pType :: Parser (ArgDocVars, TypeU)
pType =
  (,)
    <$> try parseArgDocVars
    <*> ( try pExistential
            <|> try (pFunUDoc |>> snd) -- discard nested function argument docs (for now)
            <|> try pUniU
            <|> try pThunkU
            <|> try pAppU
            <|> try parensType
            <|> try pListU
            <|> try pTupleU
            <|> pVarU
        )

pUniU :: Parser TypeU
pUniU = do
  _ <- symbol "("
  _ <- symbol ")"
  return BT.unitU

parensType :: Parser TypeU
parensType = do
  _ <- optional pTag
  (_, t) <- parens pType
  return t

pTupleU :: Parser TypeU
pTupleU = do
  _ <- optional pTag
  ts <- parens (sepBy1 pType (symbol ","))
  return $ BT.tupleU (map snd ts)

pNamEntryU :: Parser (ArgDocVars, (Key, TypeU))
pNamEntryU = do
  r <- parseArgDocVars
  n <- freename
  _ <- op "::"
  (_, t) <- pType
  return (r, (Key n, t))

pExistential :: Parser TypeU
pExistential = do
  v <- angles freenameL
  return (ExistU (TV v) ([], Open) ([], Open))

pAppU :: Parser TypeU
pAppU = do
  t <- pTerm -- TODO: generalize?
  args <- many1 pType'
  return $ AppU (VarU t) args
  where
    pType' = try pUniU <|> try pThunkU <|> try parensType <|> pVarU <|> pListU <|> pTupleU

pFunUDoc :: Parser ([ArgDocVars], TypeU)
pFunUDoc = do
  ts <- sepBy2 pType' (op "->")
  case (init ts, last ts) of
    (inputs, output) ->
      return $
        (map fst inputs <> [fst output], FunU (map snd inputs) (snd output))
  where
    pType' = (,) <$> parseArgDocVars <*> pFunCompatibleType

pFunCompatibleType :: Parser TypeU
pFunCompatibleType = try pUniU <|> try pThunkU <|> try parensType <|> try pAppU <|> try pVarU <|> try pListU <|> pTupleU

pListU :: Parser TypeU
pListU = do
  _ <- optional pTag
  (_, t) <- brackets pType
  return $ BT.listU t

-- | Parse a thunk type: {A}
-- Disambiguated from record types by checking that the content after { is not
-- a record field (i.e., not "name :: Type").
pThunkU :: Parser TypeU
pThunkU = try $ do
  _ <- symbol "{"
  -- fail if this looks like a record type: identifier followed by ::
  notFollowedBy (try (freename >> op "::"))
  (_, t) <- pType
  _ <- symbol "}"
  return $ ThunkU t

pVarU :: Parser TypeU
pVarU = VarU <$> pTerm

pTerm :: Parser TVar
pTerm = do
  _ <- optional pTag
  t <- TV <$> freename
  appendGenerics t -- add the term to the generic list IF generic
  return t

pTags :: Parser [Text]
pTags = many (try (freenameL <* op ":"))

pTag :: Parser Text
pTag = try (freenameL <* op ":")
