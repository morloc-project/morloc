{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Frontend.Desugar
Description : Transform the concrete syntax tree (CST) into the internal ExprI AST
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Desugars CST nodes produced by the Happy parser into the indexed 'ExprI'
AST used by later compiler passes. Handles: binary operator insertion,
hole-to-lambda expansion, do-notation lowering, string interpolation,
accessor patterns, type quantification, source resolution, and implicit
main wrapping.
-}
module Morloc.Frontend.Desugar
  ( desugarProgram
  , desugarExpr
  , DState(..)
  , D
  , ParseError(..)
  , showParseError
  ) where

import qualified Control.Monad.State.Strict as State
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Morloc.Frontend.CST
import Morloc.Frontend.Token hiding (startPos)
import Morloc.Namespace.Prim
import Morloc.Namespace.Type
import Morloc.Namespace.Expr
import qualified Morloc.BaseTypes as BT
import qualified Morloc.Language as ML
import System.FilePath (takeDirectory, combine)

--------------------------------------------------------------------
-- Desugar state and monad
--------------------------------------------------------------------

data ParseError = ParseError
  { pePos      :: !Pos
  , peMsg      :: !String
  , peExpected :: ![String]
  , peSourceLines :: ![Text]
  }
  deriving (Show)

showParseError :: String -> ParseError -> String
showParseError filename (ParseError pos msg expected srcLines) =
  let ln = posLine pos
      col = posCol pos
      header = filename ++ ":" ++ show ln ++ ":" ++ show col ++ ": " ++ msg
      context = formatSourceContext srcLines ln col
      expectMsg = case cleanExpected expected of
        [] -> ""
        [x] -> "\n  expected " ++ x
        xs -> "\n  expected one of: " ++ intercalate ", " xs
  in header ++ context ++ expectMsg

formatSourceContext :: [Text] -> Int -> Int -> String
formatSourceContext srcLines ln col
  | ln < 1 || ln > length srcLines = ""
  | otherwise =
    let srcLine = srcLines !! (ln - 1)
        lineNum = show ln
        pad = replicate (length lineNum) ' '
        pointer = replicate (col - 1) ' ' ++ "^"
    in "\n  " ++ pad ++ " |\n  " ++ lineNum ++ " | " ++ T.unpack srcLine ++ "\n  " ++ pad ++ " | " ++ pointer

cleanExpected :: [String] -> [String]
cleanExpected = filter (not . isInternal) . nub . map friendlyName
  where
    isInternal s = s `elem` ["VLBRACE", "VRBRACE", "VSEMI", "EOF"]
    friendlyName "LOWER"      = "identifier"
    friendlyName "UPPER"      = "type name"
    friendlyName "OPERATOR"   = "operator"
    friendlyName "INTEGER"    = "integer"
    friendlyName "FLOAT"      = "number"
    friendlyName "STRING"     = "string"
    friendlyName "STRSTART"   = "string"
    friendlyName "STRMID"     = "string"
    friendlyName "STREND"     = "string"
    friendlyName "INTERPOPEN" = "'#{'"
    friendlyName "INTERPCLOSE" = "'}'"
    friendlyName "GDOT"       = "'.'"
    friendlyName s            = s

data DState = DState
  { dsExpIndex    :: !Int
  , dsSourceMap   :: !(Map.Map Int SrcLoc)
  , dsDocMap      :: !(Map.Map Pos [Text])
  , dsModulePath  :: !(Maybe Path)
  , dsModuleConfig :: !ModuleConfig
  , dsSourceLines :: ![Text]
  , dsLangMap :: !(Map.Map Text Lang) -- alias -> Lang for all known languages
  }
  deriving (Show)

type D a = State.StateT DState (Either ParseError) a

dfail :: Pos -> String -> D a
dfail pos msg = do
  srcLines <- State.gets dsSourceLines
  State.lift (Left (ParseError pos msg [] srcLines))

--------------------------------------------------------------------
-- ID generation with proper spans
--------------------------------------------------------------------

freshIdSpan :: Span -> D Int
freshIdSpan (Span start end) = do
  s <- State.get
  let i = dsExpIndex s
      loc = SrcLoc (Just (posFile start)) (posLine start) (posCol start) (posLine end) (posCol end)
  State.put s { dsExpIndex = i + 1
              , dsSourceMap = Map.insert i loc (dsSourceMap s) }
  return i

freshIdPos :: Pos -> D Int
freshIdPos p = freshIdSpan (Span p p)

freshExprSpan :: Span -> Expr -> D ExprI
freshExprSpan sp e = do
  i <- freshIdSpan sp
  return (ExprI i e)

noSrcLoc :: SrcLoc
noSrcLoc = SrcLoc Nothing 0 0 0 0

freshExprFrom :: ExprI -> Expr -> D ExprI
freshExprFrom (ExprI refId _) e = do
  s <- State.get
  let i = dsExpIndex s
      loc = Map.findWithDefault noSrcLoc refId (dsSourceMap s)
  State.put s { dsExpIndex = i + 1
              , dsSourceMap = Map.insert i loc (dsSourceMap s) }
  return (ExprI i e)

--------------------------------------------------------------------
-- Docstring helpers
--------------------------------------------------------------------

lookupDocsAt :: Pos -> D [Text]
lookupDocsAt pos = do
  docMap <- State.gets dsDocMap
  return (Map.findWithDefault [] pos docMap)

parseDocKV :: Text -> (Text, Text)
parseDocKV txt =
  let stripped = T.strip txt
  in case T.breakOn ":" stripped of
    (key, rest)
      | not (T.null rest) && not (T.any (== ' ') (T.strip key)) ->
        (T.strip key, T.strip (T.drop 1 rest))
    _ -> ("", stripped)

parseCliOpt :: Text -> Maybe CliOpt
parseCliOpt txt = case T.unpack (T.strip txt) of
  '-' : '-' : rest@(_:_) -> Just (CliOptLong (T.pack rest))
  '-' : c : '/' : '-' : '-' : rest@(_:_) -> Just (CliOptBoth c (T.pack rest))
  '-' : c : [] -> Just (CliOptShort c)
  _ -> Nothing

processArgDocLines :: [Text] -> ArgDocVars
processArgDocLines = foldl processLine defaultValue
  where
    processLine d line = case parseDocKV line of
      ("name", val) -> d { docName = Just val }
      ("literal", val) -> d { docLiteral = Just (val == "true" || val == "True") }
      ("unroll", val) -> d { docUnroll = Just (val == "true" || val == "True") }
      ("default", val) -> d { docDefault = Just val }
      ("metavar", val) -> d { docMetavar = Just val }
      ("arg", val) -> d { docArg = parseCliOpt val }
      ("true", val) -> d { docTrue = parseCliOpt val }
      ("false", val) -> d { docFalse = parseCliOpt val }
      ("return", val) -> d { docReturn = Just val }
      (_, val) | not (T.null val) -> d { docLines = docLines d <> [val] }
      _ -> d

applySourceDocs :: [Text] -> Source -> Source
applySourceDocs docLines' src = foldl processLine src docLines'
  where
    processLine s line = case parseDocKV line of
      ("name", val) -> s { srcName = SrcName val }
      ("rsize", val) -> s { srcRsize = mapMaybe readMaybeInt (T.words val) }
      (_, val) | not (T.null val) -> s { srcNote = srcNote s <> [val] }
      _ -> s
    readMaybeInt t = case reads (T.unpack t) of
      [(n, "")] -> Just n
      _ -> Nothing

--------------------------------------------------------------------
-- Type helpers
--------------------------------------------------------------------

forallWrap :: [TVar] -> TypeU -> TypeU
forallWrap [] t = t
forallWrap (v : vs) t = ForallU v (forallWrap vs t)

quantifyType :: TypeU -> TypeU
quantifyType t = forallWrap (nub (collectGenVars t)) t
  where
    collectGenVars :: TypeU -> [TVar]
    collectGenVars (VarU v@(TV name))
      | not (T.null name), isLower (T.head name) = [v]
      | otherwise = []
    collectGenVars (ForallU v inner) = filter (/= v) (collectGenVars inner)
    collectGenVars (AppU f args) = collectGenVars f ++ concatMap collectGenVars args
    collectGenVars (FunU args ret) = concatMap collectGenVars args ++ collectGenVars ret
    collectGenVars (NamU _ _ ts entries) = concatMap collectGenVars ts ++ concatMap (collectGenVars . snd) entries
    collectGenVars (ThunkU inner) = collectGenVars inner
    collectGenVars _ = []

parseLang :: Located -> D Lang
parseLang tok = do
  langs <- State.gets dsLangMap
  case Map.lookup (T.toLower name) langs of
    Just lang -> return lang
    Nothing -> dfail (locPos tok) ("unknown language: " ++ T.unpack name)
  where
    name = getName' tok

getName' :: Located -> Text
getName' (Located _ (TokLowerName n) _) = n
getName' (Located _ (TokUpperName n) _) = n
getName' (Located _ _ t) = t

--------------------------------------------------------------------
-- Constraint extraction
--------------------------------------------------------------------

extractConstraints :: TypeU -> D [Constraint]
extractConstraints (AppU (VarU (TV name)) args) =
  return [Constraint (ClassName name) args]
extractConstraints (VarU (TV name)) =
  return [Constraint (ClassName name) []]
extractConstraints (NamU NamRecord _ _ _) =
  dfail (Pos 0 0 "") "invalid constraint syntax"
extractConstraints t =
  case flattenTupleConstraint t of
    Just cs -> return cs
    Nothing -> dfail (Pos 0 0 "") ("invalid constraint: " ++ show t)

flattenTupleConstraint :: TypeU -> Maybe [Constraint]
flattenTupleConstraint (AppU (VarU (TV name)) args)
  | T.isPrefixOf "Tuple" name = mapM typeToConstraint args
  | otherwise = Just [Constraint (ClassName name) args]
flattenTupleConstraint (VarU (TV name)) =
  Just [Constraint (ClassName name) []]
flattenTupleConstraint _ = Nothing

typeToConstraint :: TypeU -> Maybe Constraint
typeToConstraint (AppU (VarU (TV name)) args) =
  Just (Constraint (ClassName name) args)
typeToConstraint (VarU (TV name)) =
  Just (Constraint (ClassName name) [])
typeToConstraint _ = Nothing

extractClassDef :: TypeU -> D (ClassName, [TVar])
extractClassDef (AppU (VarU (TV name)) args) = do
  tvs <- mapM typeToTVar args
  return (ClassName name, tvs)
extractClassDef (VarU (TV name)) =
  return (ClassName name, [])
extractClassDef _ = dfail (Pos 0 0 "") "invalid class head"

typeToTVar :: TypeU -> D TVar
typeToTVar (VarU tv) = return tv
typeToTVar _ = dfail (Pos 0 0 "") "expected type variable in class head"

--------------------------------------------------------------------
-- Signature and type construction
--------------------------------------------------------------------

argsToType :: [(Pos, TypeU)] -> TypeU
argsToType [] = BT.unitU
argsToType [(_, t)] = t
argsToType ts = FunU (map snd (init ts)) (snd (last ts))

desugarSigType :: Pos -> CstSigType -> D ([Constraint], [ArgDocVars], TypeU)
desugarSigType _pos (CstSigType (Just constraintArgs) args) = do
  cs <- extractConstraints (argsToType constraintArgs)
  argDocs <- mapM (\(p, _) -> lookupDocsAt p) args
  return (cs, map processArgDocLines argDocs, argsToType args)
desugarSigType _pos (CstSigType Nothing args) = do
  argDocs <- mapM (\(p, _) -> lookupDocsAt p) args
  return ([], map processArgDocLines argDocs, argsToType args)

desugarTableEntries :: NamType -> [(Key, TypeU)] -> [(Key, TypeU)]
desugarTableEntries NamTable entries = [(k, wrapList t) | (k, t) <- entries]
  where
    wrapList (ForallU v t) = ForallU v (wrapList t)
    wrapList t = BT.listU t
desugarTableEntries _ entries = entries

resolveSourceFile :: Maybe Path -> Maybe Text -> Maybe Path
resolveSourceFile modulePath srcFile =
  case (modulePath, srcFile) of
    (Just f, Just srcfile') -> Just $ combine (takeDirectory f) (T.unpack srcfile')
    (Just _, Nothing) -> Nothing
    (Nothing, s) -> fmap T.unpack s

--------------------------------------------------------------------
-- Accessor resolution
--------------------------------------------------------------------

data AccessorResult
  = ARGetter Selector
  | ARSetter Selector [ExprI]

buildAccessor :: Span -> CstAccessorBody -> D ExprI
buildAccessor sp body = do
  desBody <- desugarAccessorBody body
  result <- resolveBody desBody
  case result of
    ARGetter sel -> freshExprSpan sp (PatE (PatternStruct sel))
    ARSetter sel vals -> do
      patI <- freshExprSpan sp (PatE (PatternStruct sel))
      lamI <- freshIdSpan sp
      let v = EV (".setter_" <> T.pack (show lamI))
      vArg <- freshExprSpan sp (VarE defaultValue v)
      appI <- freshExprSpan sp (AppE patI (vArg : vals))
      return (ExprI lamI (LamE [v] appI))

-- Intermediate accessor types (with ExprI values after desugaring)
data IAccessorBody
  = IABKey Text IAccessorTail
  | IABIdx Int IAccessorTail
  | IABGroup [IAccessorBody]

data IAccessorTail
  = IATEnd
  | IATSet ExprI
  | IATChain IAccessorBody

desugarAccessorBody :: CstAccessorBody -> D IAccessorBody
desugarAccessorBody (CABKey name tail') = IABKey name <$> desugarAccessorTail tail'
desugarAccessorBody (CABIdx idx tail') = IABIdx idx <$> desugarAccessorTail tail'
desugarAccessorBody (CABGroup bodies) = IABGroup <$> mapM desugarAccessorBody bodies

desugarAccessorTail :: CstAccessorTail -> D IAccessorTail
desugarAccessorTail CATEnd = return IATEnd
desugarAccessorTail (CATSet e) = IATSet <$> desugarExpr e
desugarAccessorTail (CATChain body) = IATChain <$> desugarAccessorBody body

resolveBody :: IAccessorBody -> D AccessorResult
resolveBody (IABKey name tail') = do
  inner <- resolveTail tail'
  return (wrapKey name inner)
resolveBody (IABIdx idx tail') = do
  inner <- resolveTail tail'
  return (wrapIdx idx inner)
resolveBody (IABGroup entries) = resolveGroup entries

resolveTail :: IAccessorTail -> D AccessorResult
resolveTail IATEnd = return (ARGetter SelectorEnd)
resolveTail (IATSet expr) = return (ARSetter SelectorEnd [expr])
resolveTail (IATChain body) = resolveBody body

wrapKey :: Text -> AccessorResult -> AccessorResult
wrapKey name (ARGetter sel) = ARGetter (SelectorKey (name, sel) [])
wrapKey name (ARSetter sel vals) = ARSetter (SelectorKey (name, sel) []) vals

wrapIdx :: Int -> AccessorResult -> AccessorResult
wrapIdx idx (ARGetter sel) = ARGetter (SelectorIdx (idx, sel) [])
wrapIdx idx (ARSetter sel vals) = ARSetter (SelectorIdx (idx, sel) []) vals

resolveGroup :: [IAccessorBody] -> D AccessorResult
resolveGroup bodies = do
  results <- mapM resolveBody bodies
  let getters = [s | ARGetter s <- results]
      setterPairs = [(s, vs) | ARSetter s vs <- results]
  case (getters, setterPairs) of
    (gs, []) -> return (ARGetter (mergeSelectors gs))
    ([], ss) -> return (ARSetter (mergeSelectors (map fst ss)) (concatMap snd ss))
    _ -> dfail (Pos 0 0 "") "cannot mix getter and setter entries in .()"

mergeSelectors :: [Selector] -> Selector
mergeSelectors [] = SelectorEnd
mergeSelectors [s] = s
mergeSelectors sels =
  let idxEntries = concat [s : ss | SelectorIdx s ss <- sels]
      keyEntries = concat [s : ss | SelectorKey s ss <- sels]
  in case (idxEntries, keyEntries) of
    (is, []) -> case is of { [] -> SelectorEnd; (x:xs) -> SelectorIdx x xs }
    ([], ks) -> case ks of { [] -> SelectorEnd; (x:xs) -> SelectorKey x xs }
    _ -> error "Cannot mix key and index selectors in getter"

--------------------------------------------------------------------
-- Do-notation desugaring
--------------------------------------------------------------------

desugarDo :: Span -> [CstDoStmt] -> D ExprI
desugarDo sp [] = dfail (startPos sp) "empty do block"
desugarDo sp [CstDoBare e] = do
  e' <- desugarExpr e
  freshExprSpan sp (ForceE e')
desugarDo sp [CstDoBind _ _] = dfail (startPos sp) "do block cannot end with a bind (<-)"
desugarDo sp (CstDoBind v e : rest) = do
  e' <- desugarExpr e
  forceE <- freshExprSpan sp (ForceE e')
  restE <- desugarDo sp rest
  freshExprSpan sp (LetE [(v, forceE)] restE)
desugarDo sp (CstDoBare e : rest) = do
  idx <- freshIdSpan sp
  let discardVar = EV ("_do_" <> T.pack (show idx))
  e' <- desugarExpr e
  forceE <- freshExprSpan sp (ForceE e')
  restE <- desugarDo sp rest
  freshExprSpan sp (LetE [(discardVar, forceE)] restE)

--------------------------------------------------------------------
-- Interpolation desugaring
--------------------------------------------------------------------

mkInterpString :: Span -> Text -> [ExprI] -> [Text] -> Text -> D ExprI
mkInterpString sp startText exprs mids endText = do
  let tails = mids ++ [endText]
  patI <- freshExprSpan sp (PatE (PatternText startText tails))
  freshExprSpan sp (AppE patI exprs)

--------------------------------------------------------------------
-- Implicit main wrapping
--------------------------------------------------------------------

mkImplicitMain :: [ExprI] -> D [ExprI]
mkImplicitMain es = do
  modI <- freshIdPos (Pos 0 0 "")
  return [ExprI modI (ModE (MV "main") es)]

--------------------------------------------------------------------
-- Expression desugaring: Loc CstExpr -> D ExprI
--------------------------------------------------------------------

desugarExpr :: Loc CstExpr -> D ExprI

-- Variables and literals
desugarExpr (Loc sp (CVarE v)) = freshExprSpan sp (VarE defaultValue v)
desugarExpr (Loc sp (CIntE n)) = freshExprSpan sp (IntE n)
desugarExpr (Loc sp (CRealE n)) = freshExprSpan sp (RealE n)
desugarExpr (Loc sp (CStrE s)) = freshExprSpan sp (StrE s)
desugarExpr (Loc sp (CLogE b)) = freshExprSpan sp (LogE b)
desugarExpr (Loc sp CUniE) = freshExprSpan sp UniE
desugarExpr (Loc sp CHolE) = freshExprSpan sp HolE

-- Compound expressions
desugarExpr (Loc sp (CAppE f args)) = do
  f' <- desugarExpr f
  args' <- mapM desugarExpr args
  freshExprFrom f' (AppE f' args')

desugarExpr (Loc sp (CLamE vs body)) = do
  body' <- desugarExpr body
  freshExprSpan sp (LamE vs body')

desugarExpr (Loc sp (CLetE bindings body)) = do
  bindings' <- mapM (\(v, e) -> do { e' <- desugarExpr e; return (v, e') }) bindings
  body' <- desugarExpr body
  freshExprSpan sp (LetE bindings' body')

desugarExpr (Loc sp (CBopE lhs opTok rhs)) = do
  lhs' <- desugarExpr lhs
  rhs' <- desugarExpr rhs
  opI <- freshIdSpan (Span (locPos opTok) (locPos opTok))
  freshExprSpan (Span (locPos opTok) (locPos opTok)) (BopE lhs' opI (tokToEVar opTok) rhs')

desugarExpr (Loc sp (CLstE es)) = do
  es' <- mapM desugarExpr es
  freshExprSpan sp (LstE es')

desugarExpr (Loc sp (CTupE es)) = do
  es' <- mapM desugarExpr es
  freshExprSpan sp (TupE es')

desugarExpr (Loc sp (CNamE entries)) = do
  entries' <- mapM (\(k, e) -> do { e' <- desugarExpr e; return (k, e') }) entries
  freshExprSpan sp (NamE entries')

desugarExpr (Loc sp (CSuspendE e)) = do
  e' <- desugarExpr e
  freshExprSpan sp (SuspendE e')

desugarExpr (Loc sp (CForceE e)) = do
  e' <- desugarExpr e
  freshExprSpan sp (ForceE e')

desugarExpr (Loc sp (CAnnE e t)) = do
  e' <- desugarExpr e
  freshExprSpan sp (AnnE e' (quantifyType t))

desugarExpr (Loc sp (CDoE stmts)) = do
  body <- desugarDo sp stmts
  freshExprSpan sp (SuspendE body)

desugarExpr (Loc sp (CAccessorE body)) = buildAccessor sp body

desugarExpr (Loc sp (CInterpE startText exprs mids endText)) = do
  exprs' <- mapM desugarExpr exprs
  mkInterpString sp startText exprs' mids endText

-- Top-level declarations should not appear inside expressions
desugarExpr (Loc _ (CModE {})) = error "desugarExpr: unexpected CModE in expression position"
desugarExpr (Loc _ (CImpE {})) = error "desugarExpr: unexpected CImpE in expression position"
desugarExpr (Loc _ (CSigE {})) = error "desugarExpr: unexpected CSigE in expression position"
desugarExpr (Loc _ (CAssE {})) = error "desugarExpr: unexpected CAssE in expression position"
desugarExpr (Loc _ (CTypE {})) = error "desugarExpr: unexpected CTypE in expression position"
desugarExpr (Loc _ (CClsE {})) = error "desugarExpr: unexpected CClsE in expression position"
desugarExpr (Loc _ (CIstE {})) = error "desugarExpr: unexpected CIstE in expression position"
desugarExpr (Loc _ (CFixE {})) = error "desugarExpr: unexpected CFixE in expression position"
desugarExpr (Loc _ (CSrcOldE {})) = error "desugarExpr: unexpected CSrcOldE in expression position"
desugarExpr (Loc _ (CSrcNewE {})) = error "desugarExpr: unexpected CSrcNewE in expression position"

--------------------------------------------------------------------
-- Top-level declaration desugaring
--------------------------------------------------------------------

desugarTopLevel :: Loc CstExpr -> D [ExprI]

desugarTopLevel (Loc sp (CModE name export body)) = do
  expExprI <- desugarExport sp export
  bodyExprs <- concatMapM desugarTopLevel body
  modI <- freshIdSpan sp
  return [ExprI modI (ModE (MV name) (expExprI : bodyExprs))]

desugarTopLevel (Loc sp (CImpE imp)) = do
  e <- freshExprSpan sp (ImpE imp)
  return [e]

desugarTopLevel (Loc sp (CSigE name forallVars sigType)) = do
  docs <- lookupDocsAt (startPos sp)
  let cmdDoc = processArgDocLines docs
  (cs, argDocs, t) <- desugarSigType (startPos sp) sigType
  let t' = forallWrap (map TV forallVars) t
      doc = ArgDocSig cmdDoc (init argDocs) (last argDocs)
      et = EType t' (Set.fromList cs) doc
  e <- freshExprSpan sp (SigE (Signature name Nothing et))
  return [e]

desugarTopLevel (Loc sp (CAssE name params body whereDecls)) = do
  body' <- desugarExpr body
  whereDecls' <- concatMapM desugarTopLevel whereDecls
  e <- case params of
    [] -> freshExprSpan sp (AssE name body' whereDecls')
    vs -> do
      lam <- freshExprSpan sp (LamE (map EV vs) body')
      freshExprSpan sp (AssE name lam whereDecls')
  return [e]

desugarTopLevel (Loc sp (CTypE td)) = desugarTypeDef sp td

desugarTopLevel (Loc sp (CClsE classHead sigs)) = do
  (cs, cn, vs) <- desugarClassHead classHead
  sigs' <- mapM desugarSigItem sigs
  e <- freshExprSpan sp (ClsE (Typeclass cs cn vs sigs'))
  return [e]

desugarTopLevel (Loc sp (CIstE cn types body)) = do
  bodyExprs <- concatMapM desugarTopLevel body
  e <- freshExprSpan sp (IstE cn (map quantifyType types) bodyExprs)
  return [e]

desugarTopLevel (Loc sp (CFixE assoc prec ops)) = do
  e <- freshExprSpan sp (FixE (Fixity assoc prec ops))
  return [e]

desugarTopLevel (Loc sp (CSrcOldE langTok srcFile items)) = do
  lang <- parseLang langTok
  modPath <- State.gets dsModulePath
  let path = resolveSourceFile modPath srcFile
  mapM (mkOldSource sp lang path) items

desugarTopLevel (Loc sp (CSrcNewE langTok srcFile nameToks)) = do
  lang <- parseLang langTok
  modPath <- State.gets dsModulePath
  let path = resolveSourceFile modPath srcFile
  mapM (mkNewSource sp lang path) nameToks

-- Expression-level CST nodes should not appear at top level
desugarTopLevel node = do
  e <- desugarExpr node
  return [e]

--------------------------------------------------------------------
-- Export desugaring
--------------------------------------------------------------------

desugarExport :: Span -> CstExport -> D ExprI
desugarExport sp CstExportAll = freshExprSpan sp (ExpE ExportAll)
desugarExport sp (CstExportMany locs) = do
  items <- mapM (\tok -> do { i <- freshIdPos (locPos tok); return (i, symVal' tok) }) locs
  freshExprSpan sp (ExpE (ExportMany (Set.fromList items) []))

symVal' :: Located -> Symbol
symVal' (Located _ (TokLowerName n) _) = TermSymbol (EV n)
symVal' (Located _ (TokUpperName n) _) = TypeSymbol (TV n)
symVal' (Located _ (TokOperator n) _) = TermSymbol (EV n)
symVal' (Located _ TokMinus _) = TermSymbol (EV "-")
symVal' (Located _ TokStar _) = TermSymbol (EV "*")
symVal' (Located _ TokDot _) = TermSymbol (EV ".")
symVal' (Located _ TokLAngle _) = TermSymbol (EV "<")
symVal' (Located _ TokRAngle _) = TermSymbol (EV ">")
symVal' _ = TermSymbol (EV "?")

tokToEVar :: Located -> EVar
tokToEVar (Located _ (TokOperator n) _) = EV n
tokToEVar (Located _ TokMinus _) = EV "-"
tokToEVar (Located _ TokStar _) = EV "*"
tokToEVar (Located _ TokDot _) = EV "."
tokToEVar (Located _ TokLAngle _) = EV "<"
tokToEVar (Located _ TokRAngle _) = EV ">"
tokToEVar _ = EV "?"

--------------------------------------------------------------------
-- Type definition desugaring
--------------------------------------------------------------------

desugarTypeDef :: Span -> CstTypeDef -> D [ExprI]

desugarTypeDef sp (CstTypeAlias maybeLangTok (v, vs) (t, isTerminal)) = do
  lang <- case maybeLangTok of
    Nothing -> return Nothing
    Just tok -> do
      l <- parseLang tok
      return (Just (l, isTerminal))
  docs <- lookupDocsAt (startPos sp)
  let docVars = if null docs then defaultValue else processArgDocLines docs
  e <- freshExprSpan sp (TypE (ExprTypeE lang v vs t (ArgDocAlias docVars)))
  return [e]

desugarTypeDef sp (CstTypeAliasForward (v, vs)) = do
  let t = if null vs then VarU v else AppU (VarU v) (map (either VarU id) vs)
  e <- freshExprSpan sp (TypE (ExprTypeE Nothing v vs t (ArgDocAlias defaultValue)))
  return [e]

desugarTypeDef sp (CstNamTypeWhere nt (v, vs) locEntries) = do
  recDocs <- lookupDocsAt (startPos sp)
  let recDocVars = processArgDocLines recDocs
  fieldDocs <- mapM (\(loc, _, _) -> do { dl <- lookupDocsAt (locPos loc); return (processArgDocLines dl) }) locEntries
  let entries = [(k, t) | (_, k, t) <- locEntries]
      entries' = desugarTableEntries nt entries
      doc = ArgDocRec recDocVars (zip (map fst entries') fieldDocs)
      t = NamU nt v (map (either VarU id) vs) entries'
  e <- freshExprSpan sp (TypE (ExprTypeE Nothing v vs t doc))
  return [e]

desugarTypeDef sp (CstNamTypeLegacy maybeLangTok nt (v, vs) (conName, isTerminal) entries) = do
  lang <- case maybeLangTok of
    Nothing -> return Nothing
    Just tok -> do
      l <- parseLang tok
      return (Just (l, isTerminal))
  let con = if T.null conName then v else TV conName
      entries' = desugarTableEntries nt entries
      t = NamU nt con (map (either VarU id) vs) entries'
      doc = ArgDocRec defaultValue [(k, defaultValue) | (k, _) <- entries']
  e <- freshExprSpan sp (TypE (ExprTypeE lang v vs t doc))
  return [e]

--------------------------------------------------------------------
-- Class/instance desugaring
--------------------------------------------------------------------

desugarClassHead :: CstClassHead -> D ([Constraint], ClassName, [TVar])
desugarClassHead (CCHSimple t) = do
  (cn, vs) <- extractClassDef t
  return ([], cn, vs)
desugarClassHead (CCHConstrained constraintType headType) = do
  cs <- extractConstraints constraintType
  (cn, vs) <- extractClassDef headType
  return (cs, cn, vs)
desugarClassHead (CCHMultiConstrained cs headType) = do
  (cn, vs) <- extractClassDef headType
  return (cs, cn, vs)

desugarSigItem :: CstSigItem -> D Signature
desugarSigItem (CstSigItem name forallVars sigType) = do
  (cs, argDocs, t) <- desugarSigType (Pos 0 0 "") sigType
  let wrappedT = forallWrap (map TV forallVars) t
      doc = ArgDocSig defaultValue (init argDocs) (last argDocs)
      et = EType wrappedT (Set.fromList cs) doc
  return (Signature name Nothing et)

--------------------------------------------------------------------
-- Source desugaring
--------------------------------------------------------------------

mkOldSource :: Span -> Lang -> Maybe Path -> (Text, Maybe Text) -> D ExprI
mkOldSource sp lang path (name, mayAlias) = do
  let alias = maybe name id mayAlias
  freshExprSpan sp (SrcE Source
    { srcName = SrcName name
    , srcLang = lang
    , srcPath = path
    , srcAlias = EV alias
    , srcLabel = Nothing
    , srcRsize = []
    , srcNote = []
    })

mkNewSource :: Span -> Lang -> Maybe Path -> Located -> D ExprI
mkNewSource sp lang path nameTok = do
  docLines' <- lookupDocsAt (locPos nameTok)
  let name = getName' nameTok
      baseSrc = Source
        { srcName = SrcName name
        , srcLang = lang
        , srcPath = path
        , srcAlias = EV name
        , srcLabel = Nothing
        , srcRsize = []
        , srcNote = []
        }
      src = applySourceDocs docLines' baseSrc
  freshExprSpan sp (SrcE src)

--------------------------------------------------------------------
-- Program entry point
--------------------------------------------------------------------

-- | Desugar a list of CST nodes into ExprI nodes.
-- Handles implicit main wrapping for bare declarations.
desugarProgram :: Bool -> [Loc CstExpr] -> D [ExprI]
desugarProgram isImplicitMain cstNodes = do
  exprIs <- concatMapM desugarTopLevel cstNodes
  if isImplicitMain
    then mkImplicitMain exprIs
    else return exprIs

--------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------

-- concatMapM is imported from Morloc.Internal via Morloc.Namespace.Prim
