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
  , DState (..)
  , D
  , ParseError (..)
  , showParseError
  ) where

import qualified Control.Monad.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Morloc.BaseTypes as BT
import Morloc.Frontend.CST
import Morloc.Frontend.Token hiding (startPos)
import Morloc.Namespace.Expr
import Morloc.Namespace.Prim
import Morloc.Namespace.Type
import System.FilePath (combine, dropExtension, makeRelative, splitDirectories, takeDirectory)

--------------------------------------------------------------------
-- Desugar state and monad
--------------------------------------------------------------------

data ParseError = ParseError
  { pePos :: !Pos
  , peMsg :: !String
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
       in "\n  "
            ++ pad
            ++ " |\n  "
            ++ lineNum
            ++ " | "
            ++ T.unpack srcLine
            ++ "\n  "
            ++ pad
            ++ " | "
            ++ pointer

cleanExpected :: [String] -> [String]
cleanExpected = filter (not . isInternal) . nub . map friendlyName
  where
    isInternal s = s `elem` ["VLBRACE", "VRBRACE", "VSEMI", "EOF"]
    friendlyName "LOWER" = "identifier"
    friendlyName "UPPER" = "type name"
    friendlyName "OPERATOR" = "operator"
    friendlyName "INTEGER" = "integer"
    friendlyName "FLOAT" = "number"
    friendlyName "STRING" = "string"
    friendlyName "STRSTART" = "string"
    friendlyName "STRMID" = "string"
    friendlyName "STREND" = "string"
    friendlyName "INTERPOPEN" = "'#{'"
    friendlyName "INTERPCLOSE" = "'}'"
    friendlyName "GDOT" = "'.'"
    friendlyName s = s

data DState = DState
  { dsExpIndex :: !Int
  , dsSourceMap :: !(Map.Map Int SrcLoc)
  , dsDocMap :: !(Map.Map Pos [Text])
  , dsModulePath :: !(Maybe Path)
  , dsModuleConfig :: !ModuleConfig
  , dsSourceLines :: ![Text]
  , dsLangMap :: !(Map.Map Text Lang) -- alias -> Lang for all known languages
  , dsProjectRoot :: !(Maybe Path) -- project root (directory of entry-point file)
  , dsTermDocs :: !(Map.Map EVar [Text]) -- declaration-level docstrings
  , dsWarnings :: ![Text] -- accumulated docstring warnings, drained by the caller
  }
  deriving (Show)

type D a = State.StateT DState (Either ParseError) a

dfail :: Pos -> String -> D a
dfail pos msg = do
  srcLines <- State.gets dsSourceLines
  State.lift (Left (ParseError pos msg [] srcLines))

dwarn :: [Text] -> D ()
dwarn [] = return ()
dwarn ws = State.modify (\s -> s { dsWarnings = dsWarnings s <> ws })

--------------------------------------------------------------------
-- ID generation with proper spans
--------------------------------------------------------------------

freshIdSpan :: Span -> D Int
freshIdSpan (Span start end) = do
  s <- State.get
  let i = dsExpIndex s
      loc = SrcLoc (Just (posFile start)) (posLine start) (posCol start) (posLine end) (posCol end)
  State.put
    s
      { dsExpIndex = i + 1
      , dsSourceMap = Map.insert i loc (dsSourceMap s)
      }
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
  State.put
    s
      { dsExpIndex = i + 1
      , dsSourceMap = Map.insert i loc (dsSourceMap s)
      }
  return (ExprI i e)

--------------------------------------------------------------------
-- Docstring helpers
--------------------------------------------------------------------

lookupDocsAt :: Pos -> D [Text]
lookupDocsAt pos = do
  docMap <- State.gets dsDocMap
  return (Map.findWithDefault [] pos docMap)

-- | Capture declaration-level docstring lines, keyed by term name.
-- Extracts only free description lines (the same way processArgDocLines
-- does via docLines); key-value entries like metavar:, arg:, etc. are
-- intentionally ignored at the declaration level since those describe
-- type-signature interface details.
captureDeclDocs :: Pos -> EVar -> D ()
captureDeclDocs pos name = do
  docs <- lookupDocsAt pos
  vars <- processArgDocLinesD docs
  let descLines = docLines vars
  case descLines of
    [] -> return ()
    _ -> State.modify (\s -> s { dsTermDocs = Map.insert name descLines (dsTermDocs s) })

-- | Result of classifying a single docstring line.
data ParsedDocLine
  = DocDirective !Text !Text  -- recognized `<key>: <value>` shape (key not yet validated against an allowlist)
  | DocDesc !Text             -- free-form description line (possibly empty)
  deriving (Show)

-- | Classify one `--'` line into a directive or a description.
--
-- Rules:
--  * A single leading space (the conventional space after `--'`) is trimmed
--    from description lines so authors can indent list items with extra
--    spaces. Trailing whitespace is stripped.
--  * A leading backslash (after stripping surrounding whitespace) is an
--    escape: it is consumed and the rest of the line becomes a description
--    line with no directive parsing. Literal `\foo:` therefore needs
--    `\\foo:` (standard backslash-doubling convention).
--  * Otherwise, if the line starts with `<word>:` (no spaces in `<word>`)
--    it is returned as a DocDirective. Validation against the allowlist
--    of known directive names is done by the caller.
parseDocKV :: Text -> ParsedDocLine
parseDocKV txt =
  let stripped = T.strip txt
      descLine = T.stripEnd $ case T.uncons txt of
        Just (' ', rest) -> rest
        _ -> txt
   in case T.uncons stripped of
        Just ('\\', rest) -> DocDesc (T.stripEnd rest)
        _ -> case T.breakOn ":" stripped of
          (key, colonRest)
            | not (T.null colonRest)
            , not (T.null key)
            , not (T.any (== ' ') key) ->
                DocDirective key (T.strip (T.drop 1 colonRest))
          _ -> DocDesc descLine

parseCliOpt :: Text -> Maybe CliOpt
parseCliOpt txt = case T.unpack (T.strip txt) of
  '-' : '-' : rest@(_ : _) -> Just (CliOptLong (T.pack rest))
  '-' : c : '/' : '-' : '-' : rest@(_ : _) -> Just (CliOptBoth c (T.pack rest))
  '-' : c : [] -> Just (CliOptShort c)
  _ -> Nothing

-- Known directive keys recognized inside argument / signature docstrings.
argDocDirectiveKeys :: [Text]
argDocDirectiveKeys =
  ["name", "literal", "unroll", "default", "metavar", "arg", "true", "false", "return"]

-- Known directive keys recognized on `source` declarations.
sourceDocDirectiveKeys :: [Text]
sourceDocDirectiveKeys = ["name", "rsize"]

unknownDirectiveWarning :: [Text] -> Text -> Text
unknownDirectiveWarning knownKeys k =
  "warning: unknown docstring directive '" <> k <> "'"
  <> " (recognized: " <> T.intercalate ", " knownKeys <> "); "
  <> "if this was intended as prose, prefix the line with '\\' to suppress this warning (e.g. '\\"
  <> k <> ":')"

processArgDocLines :: [Text] -> ([Text], ArgDocVars)
processArgDocLines = foldl step ([], defaultValue)
  where
    step (ws, d) line = case parseDocKV line of
      DocDesc v
        | T.null v -> (ws, d)
        | otherwise -> (ws, d {docLines = docLines d <> [v]})
      DocDirective k v -> case k of
        "name" -> (ws, d {docName = Just v})
        "literal" -> (ws, d {docLiteral = Just (parseDocBool v)})
        "unroll" -> (ws, d {docUnroll = Just (parseDocBool v)})
        "default" -> (ws, d {docDefault = Just v})
        "metavar" -> (ws, d {docMetavar = Just v})
        "arg" -> (ws, d {docArg = parseCliOpt v})
        "true" -> (ws, d {docTrue = parseCliOpt v})
        "false" -> (ws, d {docFalse = parseCliOpt v})
        "return" -> (ws, d {docReturn = Just v})
        _ ->
          let w = unknownDirectiveWarning argDocDirectiveKeys k
              desc = k <> ": " <> v
           in (ws <> [w], d {docLines = docLines d <> [desc]})

parseDocBool :: Text -> Bool
parseDocBool v = v == "true" || v == "True"

applySourceDocs :: [Text] -> Source -> ([Text], Source)
applySourceDocs lns src = foldl step ([], src) lns
  where
    step (ws, s) line = case parseDocKV line of
      DocDesc v
        | T.null v -> (ws, s)
        | otherwise -> (ws, s {srcNote = srcNote s <> [v]})
      DocDirective k v -> case k of
        "name" -> (ws, s {srcName = SrcName v})
        "rsize" -> (ws, s {srcRsize = mapMaybe readMaybeInt (T.words v)})
        _ ->
          let w = unknownDirectiveWarning sourceDocDirectiveKeys k
              desc = k <> ": " <> v
           in (ws <> [w], s {srcNote = srcNote s <> [desc]})
    readMaybeInt t = case reads (T.unpack t) of
      [(n, "")] -> Just n
      _ -> Nothing

-- | D-monad wrapper: parse argument docstring lines and accumulate any
-- warnings into 'dsWarnings' for the caller to drain.
processArgDocLinesD :: [Text] -> D ArgDocVars
processArgDocLinesD ls = do
  let (ws, v) = processArgDocLines ls
  dwarn ws
  return v

-- | D-monad wrapper: apply `source` docstring lines and accumulate warnings.
applySourceDocsD :: [Text] -> Source -> D Source
applySourceDocsD ls src = do
  let (ws, s) = applySourceDocs ls src
  dwarn ws
  return s

--------------------------------------------------------------------
-- Type helpers
--------------------------------------------------------------------

forallWrap :: [TVar] -> TypeU -> TypeU
forallWrap [] t = t
forallWrap (v : vs) t = ForallU v (forallWrap vs t)

-- | Extract LabeledU wrappers from function argument types.
-- Returns a map from nat var name to argument position index,
-- and the type with all LabeledU stripped.
extractLabels :: TypeU -> (Map.Map TVar Int, TypeU)
extractLabels = go
  where
    go (ForallU v t) = let (labels, t') = go t in (labels, ForallU v t')
    go (FunU args ret) =
      let (labels, args') = extractFromArgs 0 args
          ret' = stripLabels ret
       in (labels, FunU args' ret')
    go t = (Map.empty, stripLabels t)

    extractFromArgs :: Int -> [TypeU] -> (Map.Map TVar Int, [TypeU])
    extractFromArgs _ [] = (Map.empty, [])
    extractFromArgs idx (LabeledU v inner : rest) =
      let (labels, rest') = extractFromArgs (idx + 1) rest
       in (Map.insert v idx labels, stripLabels inner : rest')
    extractFromArgs idx (t : rest) =
      let (labels, rest') = extractFromArgs (idx + 1) rest
       in (labels, stripLabels t : rest')

    stripLabels :: TypeU -> TypeU
    stripLabels (LabeledU _ t) = stripLabels t
    stripLabels (ForallU v t) = ForallU v (stripLabels t)
    stripLabels (FunU ts t) = FunU (map stripLabels ts) (stripLabels t)
    stripLabels (AppU t ts) = AppU (stripLabels t) (map stripLabels ts)
    stripLabels (NamU o v ps rs) = NamU o v (map stripLabels ps) [(k, stripLabels t) | (k, t) <- rs]
    stripLabels (EffectU effs t) = EffectU effs (stripLabels t)
    stripLabels (OptionalU t) = OptionalU (stripLabels t)
    stripLabels (NatAddU a b) = NatAddU (stripLabels a) (stripLabels b)
    stripLabels (NatMulU a b) = NatMulU (stripLabels a) (stripLabels b)
    stripLabels (NatSubU a b) = NatSubU (stripLabels a) (stripLabels b)
    stripLabels (NatDivU a b) = NatDivU (stripLabels a) (stripLabels b)
    stripLabels (ExistU v (ps, pc) (rs, rc)) = ExistU v (map stripLabels ps, pc) (map (second stripLabels) rs, rc)
    stripLabels t = t

quantifyType :: TypeU -> TypeU
quantifyType t =
  let natVars = collectNatVars t
      t' = promoteNatVars natVars t
      typeVars = nub (collectGenVars t')
   in forallWrap typeVars t'
  where
    -- Collect type variables (excluding NatVarU which are already promoted)
    collectGenVars :: TypeU -> [TVar]
    collectGenVars (VarU v@(TV name))
      | not (T.null name), isLower (T.head name) = [v]
      | otherwise = []
    collectGenVars (NatVarU _) = []
    collectGenVars (ForallU v inner) = filter (/= v) (collectGenVars inner)
    collectGenVars (AppU f args) = collectGenVars f ++ concatMap collectGenVars args
    collectGenVars (FunU args ret) = concatMap collectGenVars args ++ collectGenVars ret
    collectGenVars (NamU _ _ ts entries) = concatMap collectGenVars ts ++ concatMap (collectGenVars . snd) entries
    collectGenVars (EffectU _ inner) = collectGenVars inner
    collectGenVars (OptionalU inner) = collectGenVars inner
    collectGenVars (NatLitU _) = []
    collectGenVars (NatAddU a b) = collectGenVars a ++ collectGenVars b
    collectGenVars (NatMulU a b) = collectGenVars a ++ collectGenVars b
    collectGenVars (NatSubU a b) = collectGenVars a ++ collectGenVars b
    collectGenVars (NatDivU a b) = collectGenVars a ++ collectGenVars b
    collectGenVars (LabeledU _ inner) = collectGenVars inner
    collectGenVars _ = []

    -- Collect variables that appear in nat-kinded positions:
    -- Only inside NatAddU, NatMulU, NatSubU, NatDivU.
    -- Typedef-based detection (e.g., Tensor2 params) is handled by refineKinds.
    collectNatVars :: TypeU -> Set.Set TVar
    collectNatVars = go False
      where
        go inNat (VarU v@(TV name))
          | inNat, not (T.null name), isLower (T.head name) = Set.singleton v
          | otherwise = Set.empty
        go _ (NatVarU v) = Set.singleton v
        go _ (ForallU _ inner) = go False inner
        go _ (AppU f args) = go False f <> Set.unions (map (go False) args)
        go _ (FunU args ret) = Set.unions (map (go False) args) <> go False ret
        go _ (NamU _ _ ts entries) = Set.unions (map (go False) ts) <> Set.unions (map (go False . snd) entries)
        go _ (EffectU _ inner) = go False inner
        go _ (OptionalU inner) = go False inner
        go _ (NatLitU _) = Set.empty
        go _ (NatAddU a b) = go True a <> go True b
        go _ (NatMulU a b) = go True a <> go True b
        go _ (NatSubU a b) = go True a <> go True b
        go _ (NatDivU a b) = go True a <> go True b
        go inNat (LabeledU _ inner) = go inNat inner
        go _ _ = Set.empty

-- | Promote VarU to NatVarU for variables identified as nat-kinded
promoteNatVars :: Set.Set TVar -> TypeU -> TypeU
promoteNatVars natVars = go
  where
    go (VarU v)
      | Set.member v natVars = NatVarU v
      | otherwise = VarU v
    go t@(NatVarU _) = t
    go (ExistU v (ps, pc) (rs, rc)) = ExistU v (map go ps, pc) (map (second go) rs, rc)
    go (ForallU v t) = ForallU v (go t)
    go (FunU ts t) = FunU (map go ts) (go t)
    go (AppU t ts) = AppU (go t) (map go ts)
    go (NamU o n ps rs) = NamU o n (map go ps) [(k, go t) | (k, t) <- rs]
    go (EffectU effs t) = EffectU effs (go t)
    go (OptionalU t) = OptionalU (go t)
    go t@(NatLitU _) = t
    go (NatAddU a b) = NatAddU (go a) (go b)
    go (NatMulU a b) = NatMulU (go a) (go b)
    go (NatSubU a b) = NatSubU (go a) (go b)
    go (NatDivU a b) = NatDivU (go a) (go b)
    go (LabeledU n t) = LabeledU n (go t)

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
  argDocVars <- mapM processArgDocLinesD argDocs
  return (cs, argDocVars, argsToType args)
desugarSigType _pos (CstSigType Nothing args) = do
  argDocs <- mapM (\(p, _) -> lookupDocsAt p) args
  argDocVars <- mapM processArgDocLinesD argDocs
  return ([], argDocVars, argsToType args)

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
-- Intrinsic resolution
--------------------------------------------------------------------

resolveIntrinsic :: Pos -> Text -> D Intrinsic
resolveIntrinsic pos name = case parseIntrinsic name of
  Just intr -> return intr
  Nothing -> dfail pos ("unknown intrinsic: @" ++ T.unpack name)

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
        (is, []) -> case is of [] -> SelectorEnd; (x : xs) -> SelectorIdx x xs
        ([], (x : xs)) -> SelectorKey x xs
        _ -> error "Cannot mix key and index selectors in getter"

--------------------------------------------------------------------
-- Do-notation desugaring
--------------------------------------------------------------------

-- Desugar a do-block to a let-chain. Non-final bare statements and <- binds
-- are wrapped in EvalE so the typechecker sees them as forced effects (pure
-- non-finals are therefore rejected). The final bare statement is returned
-- unwrapped so synthE DoBlockS can flatten it (if effectful) or let tryCoerce
-- lift it (if pure).
desugarDo :: Span -> [CstDoStmt] -> D ExprI
desugarDo sp [] = dfail (startPos sp) "empty do block"
desugarDo _sp [CstDoBare e] = desugarExpr e
desugarDo sp [CstDoBind _ _] = dfail (startPos sp) "do block cannot end with a bind (<-)"
desugarDo sp [CstDoLet _ _] = dfail (startPos sp) "do block cannot end with a let binding"
desugarDo sp (CstDoLet v e : rest) = do
  e' <- desugarExpr e
  restE <- desugarDo sp rest
  freshExprSpan sp (LetE [(v, e')] restE)
desugarDo sp (CstDoBind v e : rest) = do
  e' <- desugarExpr e
  forceE <- freshExprSpan sp (EvalE e')
  restE <- desugarDo sp rest
  freshExprSpan sp (LetE [(v, forceE)] restE)
desugarDo sp (CstDoBare e : rest) = do
  idx <- freshIdSpan sp
  let discardVar = EV ("_do_" <> T.pack (show idx))
  e' <- desugarExpr e
  forceE <- freshExprSpan sp (EvalE e')
  restE <- desugarDo sp rest
  freshExprSpan sp (LetE [(discardVar, forceE)] restE)

--------------------------------------------------------------------
-- Interpolation desugaring
--------------------------------------------------------------------

mkInterpString :: Span -> Text -> [ExprI] -> [Text] -> Text -> D ExprI
mkInterpString sp startText exprs mids endText = do
  let suffixes = mids ++ [endText]
  patI <- freshExprSpan sp (PatE (PatternText startText suffixes))
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
desugarExpr (Loc sp (CLabeledVarE label v)) = do
  moduleConfig <- State.gets dsModuleConfig
  case Map.lookup label (moduleConfigLabeledGroups moduleConfig) of
    Just config -> freshExprSpan sp (VarE config v)
    Nothing -> dfail (startPos sp)
      ("Undefined label '" ++ T.unpack label
       ++ "': no matching entry in module config labeled-groups")
desugarExpr (Loc sp (CVarE v)) = freshExprSpan sp (VarE defaultValue v)
desugarExpr (Loc sp (CIntE n)) = freshExprSpan sp (IntE n)
desugarExpr (Loc sp (CRealE n)) = freshExprSpan sp (RealE n)
desugarExpr (Loc sp (CStrE s)) = freshExprSpan sp (StrE s)
desugarExpr (Loc sp (CLogE b)) = freshExprSpan sp (LogE b)
desugarExpr (Loc sp CUniE) = freshExprSpan sp UniE
desugarExpr (Loc sp CNullE) = freshExprSpan sp NullE
desugarExpr (Loc sp CHolE) = freshExprSpan sp HolE
-- Intrinsics: eta-expand when under-applied so they behave as first-class functions
desugarExpr (Loc sp (CIntrinsicE name)) = do
  intr <- resolveIntrinsic (startPos sp) name
  etaExpandIntrinsic sp intr []
desugarExpr (Loc sp (CAppE (Loc _ (CIntrinsicE name)) args)) = do
  intr <- resolveIntrinsic (startPos sp) name
  args' <- mapM desugarExpr args
  etaExpandIntrinsic sp intr args'
-- Compound expressions
desugarExpr (Loc _ (CAppE f args)) = do
  f' <- desugarExpr f
  args' <- mapM desugarExpr args
  freshExprFrom f' (AppE f' args')
desugarExpr (Loc sp (CLamE vs body)) = do
  body' <- desugarExpr body
  freshExprSpan sp (LamE vs body')
desugarExpr (Loc sp (CLetE bindings body)) = do
  bindings' <- mapM (\(v, e) -> do e' <- desugarExpr e; return (v, e')) bindings
  body' <- desugarExpr body
  freshExprSpan sp (LetE bindings' body')
desugarExpr (Loc sp (CParenE inner@(Loc _ CBopE{}))) = do
  inner' <- desugarExpr inner
  freshExprSpan sp (ParenE inner')
desugarExpr (Loc _ (CParenE inner)) = desugarExpr inner
desugarExpr (Loc _ (CBopE lhs opTok rhs)) = do
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
  entries' <- mapM (\(k, e) -> do e' <- desugarExpr e; return (k, e')) entries
  freshExprSpan sp (NamE entries')
desugarExpr (Loc sp (CAnnE e t)) = do
  e' <- desugarExpr e
  freshExprSpan sp (AnnE e' (quantifyType t))
desugarExpr (Loc sp (CDoE stmts)) = do
  body <- desugarDo sp stmts
  freshExprSpan sp (DoBlockE body)
desugarExpr (Loc sp (CAccessorE body)) = buildAccessor sp body
desugarExpr (Loc sp (CInterpE startText exprs mids endText)) = do
  exprs' <- mapM desugarExpr exprs
  mkInterpString sp startText exprs' mids endText
desugarExpr (Loc sp (CGuardExprE guards defaultExpr)) = desugarGuards sp guards defaultExpr
desugarExpr (Loc sp (CForceE e)) = do
  e' <- desugarExpr e
  freshExprSpan sp (EvalE e')

-- Top-level declarations should not appear inside expressions
desugarExpr (Loc _ CModE{}) = error "desugarExpr: unexpected CModE in expression position"
desugarExpr (Loc _ (CImpE {})) = error "desugarExpr: unexpected CImpE in expression position"
desugarExpr (Loc _ (CSigE {})) = error "desugarExpr: unexpected CSigE in expression position"
desugarExpr (Loc _ (CAssE {})) = error "desugarExpr: unexpected CAssE in expression position"
desugarExpr (Loc _ (CTypE {})) = error "desugarExpr: unexpected CTypE in expression position"
desugarExpr (Loc _ (CClsE {})) = error "desugarExpr: unexpected CClsE in expression position"
desugarExpr (Loc _ (CIstE {})) = error "desugarExpr: unexpected CIstE in expression position"
desugarExpr (Loc _ (CFixE {})) = error "desugarExpr: unexpected CFixE in expression position"
desugarExpr (Loc _ (CSrcOldE {})) = error "desugarExpr: unexpected CSrcOldE in expression position"
desugarExpr (Loc _ (CSrcNewE {})) = error "desugarExpr: unexpected CSrcNewE in expression position"
desugarExpr (Loc _ (CGuardedAssE {})) = error "desugarExpr: unexpected CGuardedAssE in expression position"
desugarExpr (Loc _ (CInlineE {})) = error "desugarExpr: unexpected CInlineE in expression position"

-- | Wrap an intrinsic in a lambda if it has fewer args than its arity.
-- Fully applied intrinsics pass through as IntrinsicE nodes.
etaExpandIntrinsic :: Span -> Intrinsic -> [ExprI] -> D ExprI
etaExpandIntrinsic sp intr args = do
  let arity = intrinsicArity intr
      actual = length args
  if actual >= arity
    then freshExprSpan sp (IntrinsicE intr args)
    else do
      idx <- freshIdSpan sp
      let remaining = arity - actual
          vars = [EV ("_intr_" <> T.pack (show idx) <> "_" <> T.pack (show j)) | j <- [0..remaining-1]]
      varExprs <- mapM (\v -> freshExprSpan sp (VarE defaultValue v)) vars
      intrExpr <- freshExprSpan sp (IntrinsicE intr (args ++ varExprs))
      freshExprSpan sp (LamE vars intrExpr)


--------------------------------------------------------------------
-- Top-level declaration desugaring
--------------------------------------------------------------------

-- | Infer a dot-prefixed module name from a file path relative to the project root.
-- e.g., projectRoot=/project, filePath=/project/lib/math/main.loc -> ".lib.math"
inferModuleName :: Path -> Path -> Text
inferModuleName projectRoot filePath =
  let relPath = makeRelative projectRoot filePath
      parts = splitDirectories relPath
      -- Strip .loc extension from the last component
      cleaned = case parts of
        [] -> ["main"]
        _ -> init parts ++ [dropExtension (last parts)]
      -- Strip trailing "main" for directory modules (but not if it's the only component)
      stripped = case cleaned of
        xs | length xs > 1 && last xs == "main" -> init xs
        xs -> xs
  in "." <> T.intercalate "." (map T.pack stripped)

desugarTopLevel :: Loc CstExpr -> D [ExprI]
desugarTopLevel (Loc sp (CModE maybeName export body)) = do
  name <- case maybeName of
    Just n -> return n
    Nothing -> do
      modPath <- State.gets dsModulePath
      projRoot <- State.gets dsProjectRoot
      case (modPath, projRoot) of
        (Just mp, Just pr) -> return (inferModuleName pr mp)
        _ -> dfail (startPos sp) "nameless module requires a file path and project root"
  expExprI <- desugarExport sp export
  bodyExprs <- concatMapM desugarTopLevel body
  modI <- freshIdSpan sp
  return [ExprI modI (ModE (MV name) (expExprI : bodyExprs))]
desugarTopLevel (Loc sp (CImpE imp)) = do
  e <- freshExprSpan sp (ImpE imp)
  return [e]
desugarTopLevel (Loc sp (CSigE name sigType)) = do
  docs <- lookupDocsAt (startPos sp)
  cmdDoc <- processArgDocLinesD docs
  (cs, argDocs, t) <- desugarSigType (startPos sp) sigType
  let t' = quantifyType t
      doc = ArgDocSig cmdDoc (init argDocs) (last argDocs)
      (labels, t'') = extractLabels t'
      et = EType t'' (Set.fromList cs) doc labels
  e <- freshExprSpan sp (SigE (Signature name Nothing et))
  return [e]
desugarTopLevel (Loc sp (CAssE name params body whereDecls)) = do
  captureDeclDocs (startPos sp) name
  body' <- desugarExpr body
  whereDecls' <- concatMapM desugarTopLevel whereDecls
  e <- case params of
    [] -> freshExprSpan sp (AssE name body' whereDecls')
    vs -> do
      lam <- freshExprSpan sp (LamE (map EV vs) body')
      freshExprSpan sp (AssE name lam whereDecls')
  return [e]
desugarTopLevel (Loc sp (CGuardedAssE name params guards defaultExpr whereDecls)) = do
  captureDeclDocs (startPos sp) name
  body' <- desugarGuards sp guards defaultExpr
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
desugarTopLevel (Loc sp (CSrcNewE langTok srcFile nameTuples)) = do
  lang <- parseLang langTok
  modPath <- State.gets dsModulePath
  let path = resolveSourceFile modPath srcFile
  mapM (mkNewSource sp lang path) nameTuples
desugarTopLevel (Loc _ (CInlineE inner)) = do
  exprs <- desugarTopLevel inner
  return (map markSourceInline exprs)
  where
    markSourceInline (ExprI i (SrcE src)) = ExprI i (SrcE src { srcInline = True })
    markSourceInline e = e -- %inline on non-source definitions is not yet implemented

-- Expression-level CST nodes should not appear at top level
desugarTopLevel node = do
  e <- desugarExpr node
  return [e]

--------------------------------------------------------------------
-- Guard desugaring
--------------------------------------------------------------------

-- | Desugar guard clauses with an explicit default into nested IfE expressions.
-- ? cond1 = body1 ? cond2 = body2 : defaultBody
-- becomes: IfE cond1 body1 (IfE cond2 body2 defaultBody)
desugarGuards :: Span -> [(Loc CstExpr, Loc CstExpr)] -> Loc CstExpr -> D ExprI
desugarGuards _ [] defaultExpr = desugarExpr defaultExpr
desugarGuards sp ((cond, body) : rest) defaultExpr = do
  cond' <- desugarExpr cond
  body' <- desugarExpr body
  elseE <- desugarGuards sp rest defaultExpr
  freshExprSpan sp (IfE cond' body' elseE)

--------------------------------------------------------------------
-- Export desugaring
--------------------------------------------------------------------

desugarExport :: Span -> CstExport -> D ExprI
desugarExport sp CstExportAll = freshExprSpan sp (ExpE ExportAll)
desugarExport sp (CstExportMany locs) = do
  items <- mapM (\tok -> do i <- freshIdPos (locPos tok); return (i, symVal' tok)) locs
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
  docVars <- if null docs then return defaultValue else processArgDocLinesD docs
  e <- freshExprSpan sp (TypE (ExprTypeE lang v vs t (ArgDocAlias docVars)))
  return [e]
desugarTypeDef sp (CstTypeAliasForward (v, vs)) = do
  let t = if null vs then VarU v else AppU (VarU v) (map (either (VarU . fst) id) vs)
  e <- freshExprSpan sp (TypE (ExprTypeE Nothing v vs t (ArgDocAlias defaultValue)))
  return [e]
desugarTypeDef sp (CstNamTypeWhere nt (v, vs) locEntries) = do
  recDocs <- lookupDocsAt (startPos sp)
  recDocVars <- processArgDocLinesD recDocs
  fieldDocs <-
    mapM
      (\(loc, _, _) -> do dl <- lookupDocsAt (locPos loc); processArgDocLinesD dl)
      locEntries
  let entries = [(k, ty) | (_, k, ty) <- locEntries]
      entries' = desugarTableEntries nt entries
      doc = ArgDocRec recDocVars (zip (map fst entries') fieldDocs)
      t = NamU nt v (map (either (VarU . fst) id) vs) entries'
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
      t = NamU nt con (map (either (VarU . fst) id) vs) entries'
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
desugarSigItem (CstSigItem name sigType) = do
  (cs, argDocs, t) <- desugarSigType (Pos 0 0 "") sigType
  let wrappedT = quantifyType t
      (labels, wrappedT') = extractLabels wrappedT
      doc = ArgDocSig defaultValue (init argDocs) (last argDocs)
      et = EType wrappedT' (Set.fromList cs) doc labels
  return (Signature name Nothing et)

--------------------------------------------------------------------
-- Source desugaring
--------------------------------------------------------------------

mkOldSource :: Span -> Lang -> Maybe Path -> (Text, Maybe Text) -> D ExprI
mkOldSource sp lang path (name, mayAlias) = do
  let alias = maybe name id mayAlias
  freshExprSpan
    sp
    ( SrcE
        Source
          { srcName = SrcName name
          , srcLang = lang
          , srcPath = path
          , srcAlias = EV alias
          , srcLabel = Nothing
          , srcRsize = []
          , srcNote = []
          , srcInline = False
          , srcOperator = isOperatorName name
          }
    )

mkNewSource :: Span -> Lang -> Maybe Path -> (Bool, Text, Located) -> D ExprI
mkNewSource sp lang path (isInline, name, nameTok) = do
  docLines' <- lookupDocsAt (locPos nameTok)
  let isOp = isOperatorName name
      baseSrc =
        Source
          { srcName = SrcName name
          , srcLang = lang
          , srcPath = path
          , srcAlias = EV name
          , srcLabel = Nothing
          , srcRsize = []
          , srcNote = []
          , srcInline = isInline
          , srcOperator = isOp
          }
  src <- applySourceDocsD docLines' baseSrc
  freshExprSpan sp (SrcE src)

isOperatorName :: Text -> Bool
isOperatorName t = case T.uncons t of
  Just (c, _) -> not (isLower c) && not (isUpper c) && c /= '_'
  Nothing -> False

--------------------------------------------------------------------
-- Program entry point
--------------------------------------------------------------------

{- | Desugar a list of CST nodes into ExprI nodes.
Handles implicit main wrapping for bare declarations.
-}
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
