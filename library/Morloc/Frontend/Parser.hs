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
  _ <- reserved "module"

  moduleName <- case expModuleName of
    Nothing -> MT.intercalate "." <$> sepBy moduleComponent (symbol ".")
    (Just (MV n)) -> symbol n

  export <-
    parens
      ( (char '*' >> return ExportAll)
          <|> (sepBy pIndexedSymbol (symbol ",") |>> ExportMany . Set.fromList)
      )

  expE <- exprI (ExpE export)

  es <- align pTopExpr |>> concat

  exprI $ ModE (MV moduleName) (expE : es)

pIndexedSymbol :: Parser (Int, Symbol)
pIndexedSymbol = do
  sym <- pSymbol
  i <- exprId
  return (i, sym)

-- | match an implicit Main module
pMain :: Parser ExprI
pMain = do
  setMinPos
  es <- align pTopExpr |>> concat >>= createMainFunction
  exprI $ ModE (MV "main") es

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
    <|> try pSrcE
    <|> plural pExpr
    <?> "statement"

-- | Expressions that are allowed in function or data declarations
pExpr :: Parser ExprI
pExpr =
  try pAnn
    <|> try pLam
    <|> pInfixExpr -- infix-aware parser (handles all atoms and operators)
    <?> "expression"

-- Either a lowercase term name or an uppercase type/class name
pSymbol :: Parser Symbol
pSymbol =
  try (TermSymbol <$> parenOperator)  -- Operators in parens: (+), (*), etc.
    <|> (TermSymbol . EV <$> freenameL)  -- Regular term names
    <|> (TypeSymbol . TV <$> freenameU)  -- Type names

pImport :: Parser ExprI
pImport = do
  _ <- reserved "import"
  -- There may be '.' in import names, these represent folders/namespaces of modules
  n <- MT.intercalate "." <$> sepBy moduleComponent (symbol ".")
  imports <-
    optional $
      parens (sepBy pImportItem (symbol ","))
  exprI . ImpE $
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
      n <- try (parenOperator |>> unEVar) <|> freenameL
      a <- option n (reserved "as" >> freenameL)
      return (AliasedTerm (EV n) (EV a))

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
  _ <- reserved "class"
  (TV v, vs) <- pTypedefTerm' <|> parens pTypedefTerm'
  sigs <- option [] (reserved "where" >> alignInset pSignature)
  exprI . ClsE $ Typeclass (ClassName v) vs sigs
  where
    -- parses "Show a" from above example
    pTypedefTerm' :: Parser (TVar, [TVar])
    pTypedefTerm' = do
      t <- freenameU
      ts <- many freenameL -- The parameters must currently be generic, hence "L" for lowercase
      return (TV t, map TV ts)

pInstance :: Parser ExprI
pInstance = do
  _ <- reserved "instance"
  v <- freenameU
  ts <- many1 pTypeGen
  es <- option [] (reserved "where" >> alignInset pInstanceExpr) |>> concat
  exprI $ IstE (ClassName v) ts es
  where
    pInstanceExpr :: Parser [ExprI]
    pInstanceExpr =
      try (pSource >>= mapM (exprI . SrcE))
        <|> (pAssE |>> return)

-- | Parse fixity declaration: infixl 6 +, -
pFixity :: Parser ExprI
pFixity = do
  assoc <- pAssociativity
  prec <- pPrecValue
  when (prec > 9) $
    fail "precedence must be between 0 and 9"
  ops <- sepBy1 pOperatorName (symbol ",")
  exprI . FixE $ Fixity assoc prec ops
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
      doc <- parseArgDocVars |>> ArgDocAlias
      _ <- reserved "type"
      mayLang <- optional (try pLangNamespace)
      (v, vs) <- pTypedefTerm <|> parens pTypedefTerm
      case mayLang of
        (Just lang) -> do
          _ <- symbol "="
          (t, isTerminal) <- pConcreteType <|> pGeneralType
          exprI (TypE (ExprTypeE (Just (lang, isTerminal)) v vs t doc))
        Nothing -> do
          mayT <- optional (symbol "=" >> pType)
          case (vs, mayT) of
            (_, Just (_, t)) -> exprI (TypE (ExprTypeE Nothing v vs t doc))
            ([], Nothing) -> exprI (TypE (ExprTypeE Nothing v vs (VarU v) doc))
            (_, Nothing) -> exprI (TypE (ExprTypeE Nothing v vs (AppU (VarU v) (map (either (VarU) id) vs)) doc))

    pTypedefObjectLegacy :: Parser ExprI
    pTypedefObjectLegacy = do
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
      exprI (TypE (ExprTypeE k v vs t objDoc))

    pTypedefObject :: Parser ExprI
    pTypedefObject = do
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
      exprI (TypE (ExprTypeE Nothing v vs t grpArg))

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

    -- \| For now, only type signatures and declarations are allowed in function
    -- where statements. There is no particularly reason why source and imports
    -- could not be here. Exports probably should NOT be allowed since they would
    -- break scope.
    whereTerm :: Parser ExprI
    whereTerm = try pSigE <|> pAssE

pSigE :: Parser ExprI
pSigE = do
  signature <- pSignature
  exprI . SigE $ signature

pSignature :: Parser Signature
pSignature = do
  doc <- parseArgDocVars
  label' <- optional pTag
  v <- freenameL <|> (parenOperator |>> unEVar)
  vs <- many freenameL |>> map TV
  _ <- op "::"
  props <- option [] (try pPropertyList)

  (docs, t') <- pTypeDoc

  constraints <- option [] pConstraints

  let cmdDoc = ArgDocSig doc (init docs) (last docs)

  let t = forallWrap vs t'
      et =
        EType
          { etype = t
          , eprop = Set.fromList props
          , econs = Set.fromList constraints
          , edocs = cmdDoc
          }
      sig = Signature (EV v) (Label <$> label') et

  return sig
  where
    pPropertyList :: Parser [Property]
    pPropertyList = do
      ps <-
        parens (sepBy1 pProperty (symbol ","))
          <|> sepBy1 pProperty (symbol ",")
      _ <- op "=>"
      return ps

    pProperty :: Parser Property
    pProperty = Property <$> many1 freename

    pConstraints :: Parser [Constraint]
    pConstraints = reserved "where" >> alignInset pConstraint
      where
        -- FIXME: this is a stub
        pConstraint :: Parser Constraint
        pConstraint = fmap (Con . MT.unwords) (many1 pWord)

        pWord :: Parser Text
        pWord = MT.pack <$> lexeme (many1 alphaNumChar)

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

pSrcE :: Parser [ExprI]
pSrcE = do
  srcs <- pSource
  mapM (exprI . SrcE) srcs

pSource :: Parser [Source]
pSource = try pSourceLegacy <|> pSourceNew

pSourceLegacy :: Parser [Source]
pSourceLegacy = do
  _ <- reserved "source"
  modulePath <- CMS.gets stateModulePath
  language <- pLang
  maySrcfile <- optional (reserved "from" >> stringLiteral |>> MT.unpack)
  rs <- parens (sepBy1 pImportSourceTerm (symbol ","))
  let srcfile = getSourceFile modulePath maySrcfile
  return
    [ Source
      { srcName = srcVar
      , srcLang = language
      , srcPath = srcfile
      , srcAlias = aliasVar
      , srcLabel = Label <$> label'
      , srcRsize = []
      , srcNote = []
      }
    | (srcVar, aliasVar, label') <- rs
    ]
  where
    pImportSourceTerm :: Parser (SrcName, EVar, Maybe Text)
    pImportSourceTerm = do
      t <- optional pTag
      n <- stringLiteral
      a <- option n (reserved "as" >> (freename <|> (parenOperator |>> unEVar)))
      return (SrcName n, EV a, t)

pSourceNew :: Parser [Source]
pSourceNew = do
  modulePath <- CMS.gets stateModulePath
  _ <- reserved "source"
  language <- pLang
  maySrcfile <- optional (reserved "from" >> stringLiteral |>> MT.unpack)
  let srcfile = getSourceFile modulePath maySrcfile
  option [] (reserved "where" >> alignInset (pImportSourceTerm language srcfile))
  where
    pImportSourceTerm :: Lang -> Maybe Path -> Parser Source
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
      n <- freename
      let srcname =
            if srcName src == SrcName ""
              then SrcName n
              else srcName src
      return $ src {srcName = srcname, srcAlias = EV n}

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
pLstE = brackets (sepBy pExpr (symbol ",")) >>= exprI . LstE

pTupE :: Parser ExprI
pTupE = do
  _ <- symbol "("
  e <- pExpr
  _ <- symbol ","
  es <- sepBy1 pExpr (symbol ",")
  _ <- symbol ")"
  exprI $ TupE (e : es)

pNamE :: Parser ExprI
pNamE = do
  rs <- braces (sepBy1 pNamEntryE (symbol ","))
  -- FIXME - making records without constructors is a bit sketch, for now it is
  -- allowed (and heavily tested) and I will leave it for the moment. But
  -- eventually the syntax should be `Person {Age = 5, Name = "Juicebox"}` or
  -- whatever. The hell it should fucking dimwit. Constructors are shit.
  exprI $ NamE (map (first Key) rs)

pNamEntryE :: Parser (Text, ExprI)
pNamEntryE = do
  n <- freenameL
  _ <- symbol "="
  e <- pExpr
  return (n, e)

pUni :: Parser ExprI
pUni = symbol "(" >> symbol ")" >> exprI UniE

pAnn :: Parser ExprI
pAnn = do
  e <-
    try (parens pExpr)
      <|> try pTupE
      <|> pVar
      <|> pLstE
      <|> pNamE
      <|> pNumE
      <|> pLogE
      <|> pStrE
  _ <- op "::"
  t <- pTypeGen
  exprI $ AnnE e t

-- | Parse an expression that may contain infix operators
-- Note that the fixity of the binops is not yet known, so they are merged into
-- a left-associated temporary structure
pInfixExpr :: Parser ExprI
pInfixExpr = do
  lhs <- pOperand  -- Try function application first, then atoms
  mayOp <- optional pInfixOperator
  case mayOp of
    (Just binop) -> do
      rhs <- pExpr
      i <- exprId
      exprI $ BopE lhs i binop rhs 
    Nothing -> return lhs

-- | Parse an infix operator (not in parens)
pInfixOperator :: Parser EVar
pInfixOperator = EV <$> operatorName

-- | Parse an operand
pOperand :: Parser ExprI
pOperand =
  try pUni
    <|> try pApp
    <|> try pHolE
    <|> try pNumE
    <|> try pLogE
    <|> try pStrE
    <|> try (parens pExpr) -- Full expressions in parens
    <|> try pTupE
    <|> try pLstE
    <|> try pNamE
    <|> try pSetter
    <|> try pGetter
    <|> pVar
    <?> "operand"

pApp :: Parser ExprI
pApp = do
  f <- parseFun
  es <- many1 parseArg
  exprI $ AppE f es
  where
    parseFun =
      pVar
        <|> try pTupE -- only valid if wholy
        <|> try pLstE --  /
        <|> try pNamE -- /
        <|> try pSetter
        <|> try pGetter
        <|> try (parens pExpr)
    parseArg =
      try pUni
        <|> try pTupE
        <|> try (parens pExpr)
        <|> try pSetter
        <|> try pGetter
        <|> try pStrE
        <|> try pLogE
        <|> try pNumE
        <|> pHolE
        <|> pLstE
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
pStrE = do
  -- (Either Text (Text, [(a, Text)]))
  eitherS <- stringPatterned pExpr
  case eitherS of
    (Left txt) -> exprI . StrE $ txt
    (Right (s, es)) -> do
      pattern <- exprI . PatE $ PatternText s (map snd es)
      exprI $ AppE pattern (map fst es)

pSetter :: Parser ExprI
pSetter = do
  -- parse the setter pattern
  -- for example: .(x.0 = 1, y.a = 2, z = 3)
  --  ss: the selectors, in this case the pattern .(x.0, y.a, z)
  --  es: a list of expressions: [1,2,3]
  (s, es) <- parsePatternSetter pExpr
  setter <- exprI $ PatE (PatternStruct s)

  -- fresh indices for the lambda and application expressions we'll create
  idxLam <- exprId

  -- a unique name for the lambda-bound datastructure variable
  let v = EV (".setter_" <> MT.show' idxLam)

  -- a variable to store the datastructure that will be passed to the lambda
  vArg <- exprI $ VarE defaultValue v

  -- apply arguments to the setter
  -- first the datastructure and then all setting values
  setterApp <- exprI $ AppE setter (vArg : es)

  return $ ExprI idxLam (LamE [v] setterApp)

pGetter :: Parser ExprI
pGetter = do
  s <- parsePatternGetter
  exprI $ PatE (PatternStruct s)

pNumE :: Parser ExprI
pNumE = do
  x <- number
  exprI $ case x of
    (Left i) -> IntE i
    (Right f) -> RealE f

pLam :: Parser ExprI
pLam = do
  _ <- symbol "\\"
  vs <- many1 pEVar
  _ <- symbol "->"
  e <- pExpr
  exprI $ LamE vs e

pVar :: Parser ExprI
pVar = do
  labels <- pTags

  s <- CMS.get

  let baseConfig = maybe defaultValue id (moduleConfigDefaultGroup (stateModuleConfig s))
      configMap = moduleConfigLabeledGroups (stateModuleConfig s)
      varConfigs = catMaybes [Map.lookup x configMap | x <- labels]
      manifoldConfig = foldl mergeConfigs baseConfig varConfigs

  v <- pEVar
  exprI $ VarE manifoldConfig v
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

pHolE :: Parser ExprI
pHolE = hole >> exprI HolE

pEVar :: Parser EVar
pEVar = try parenOperator <|> fmap EV freenameL

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
    pType' = try pUniU <|> try parensType <|> pVarU <|> pListU <|> pTupleU

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
    pType' = try pUniU <|> try parensType <|> pVarU <|> pListU <|> pTupleU

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
pFunCompatibleType = try pUniU <|> try parensType <|> try pAppU <|> try pVarU <|> try pListU <|> pTupleU

pListU :: Parser TypeU
pListU = do
  _ <- optional pTag
  (_, t) <- brackets pType
  return $ BT.listU t

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
