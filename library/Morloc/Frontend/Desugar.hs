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
  , dsModuleDoc :: ![Text] -- module-level description lines
  , dsModuleEpilogues :: ![[Text]] -- epilogue blocks for top-level help
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

srcLocToSpan :: SrcLoc -> Span
srcLocToSpan loc = Span (Pos l1 c1 f) (Pos l2 c2 f)
  where
    f  = maybe "" id (srcLocPath loc)
    l1 = srcLocLine loc
    c1 = srcLocCol loc
    l2 = srcLocEndLine loc
    c2 = srcLocEndCol loc

-- | Look up the source span for a fresh-allocated ExprI id. Falls back
-- to a zero-position span if the id is not in the source map.
posOfExprI :: ExprI -> D Span
posOfExprI (ExprI ident _) = do
  sm <- State.gets dsSourceMap
  return (srcLocToSpan (Map.findWithDefault noSrcLoc ident sm))

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
-- | Register free description lines from a signature preamble under
-- the term name so lookups by name (e.g., resolving a `--' with:`
-- referenced term to its docstring) can find them. Preserves any
-- existing declaration-level entry, since declaration docs take
-- precedence over signature docs elsewhere in the pipeline.
captureSigDescLines :: EVar -> [Text] -> D ()
captureSigDescLines _ [] = return ()
captureSigDescLines name descs = State.modify $ \s ->
  s { dsTermDocs =
        Map.insertWith
          (\_new existing -> existing)  -- keep existing declaration doc
          name
          descs
          (dsTermDocs s)
    }

captureDeclDocs :: Pos -> EVar -> D ()
captureDeclDocs pos name = do
  docs <- lookupDocsAt pos
  vars <- processArgDocLinesD pos docs
  case docWith vars of
    (s : _) -> dfail pos . T.unpack $
      "`with:` requires an explicit signature above the definition. "
      <> "Add a `" <> unEVar name <> " :: <type>` line and move the "
      <> "`--' with:` atoms to that signature's docstring. "
      <> "Offending atom: `with: " <> renderWithSpec s <> "`."
    [] -> return ()
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

-- | Outcome of attempting to parse a CLI-option directive value
-- (the right-hand side of `arg:`, `true:`, `false:`).
data CliOptParse
  = CliOptOk CliOpt
  | -- | The shape matched a short-option form but the option
    -- character is not a POSIX-valid short-flag char (must be an
    -- ASCII letter). Carries the offending character so the caller
    -- can construct a precise diagnostic.
    CliOptShortCharInvalid !Char
  | -- | The text didn't match any recognized CLI-option shape.
    CliOptShapeUnknown

isShortFlagChar :: Char -> Bool
isShortFlagChar c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

parseCliOpt :: Text -> CliOptParse
parseCliOpt txt = case T.unpack (T.strip txt) of
  '-' : '-' : rest@(_ : _) -> CliOptOk (CliOptLong (T.pack rest))
  '-' : c : '/' : '-' : '-' : rest@(_ : _)
    | isShortFlagChar c -> CliOptOk (CliOptBoth c (T.pack rest))
    | otherwise -> CliOptShortCharInvalid c
  '-' : c : []
    | isShortFlagChar c -> CliOptOk (CliOptShort c)
    | otherwise -> CliOptShortCharInvalid c
  _ -> CliOptShapeUnknown

-- Known directive keys recognized inside argument / signature docstrings.
-- Includes the dotted families (`source`, `form`, `check.<kind>`,
-- `list.source`, `list.form`, `list.check.<kind>`) used by the new
-- CLI-argument shape design; the family heads are listed here for the
-- unknown-key warning even though full keys are dotted.
argDocDirectiveKeys :: [Text]
argDocDirectiveKeys =
  [ "name", "literal", "many", "unroll", "default", "metavar"
  , "arg", "true", "false", "return"
  , "source", "form", "check.<kind>"
  , "list.source", "list.form", "list.check.<kind>"
  , "with"
  ]

-- | Parse the value of a `source:` field. OR-chains (`a|b`) are
-- rejected with a targeted error so the user can find the redundant
-- syntax. `auto` was the implicit default in earlier designs and is
-- never written explicitly.
parseSourceAtom :: Text -> Either Text SourceAtom
parseSourceAtom raw = case T.strip raw of
  "inline" -> Right SourceInline
  "file" -> Right SourceFile
  s | T.isInfixOf "|" s ->
        Left $ "OR-chains in `source:` (e.g. `file|inline`) are no longer"
            <> " supported; pick a single value (inline or file)."
    | s == "auto" ->
        Left $ "`source: auto` is not a writable value; auto is the implicit"
            <> " default. Either drop the field or pick `inline` / `file`."
    | otherwise ->
        Left $ "unknown source value '" <> s
            <> "'; expected one of: inline, file."

-- | Parse the value of a `form:` field. The four meaningful atoms are
-- `list`, `bytes`, `bytes-only`, and `packet`. OR-chains are rejected.
parseFormAtom :: Text -> Either Text FormAtom
parseFormAtom raw = case T.strip raw of
  "packet" -> Right FormPacket
  "bytes" -> Right FormBytes
  "bytes-only" -> Right FormBytesOnly
  "list" -> Right FormList
  s | T.isInfixOf "|" s ->
        Left $ "OR-chains in `form:` (e.g. `packet|bytes`) are no longer"
            <> " supported; pick a single value."
    | s == "auto" ->
        Left $ "`form: auto` is not a writable value; auto is the implicit"
            <> " default. Either drop the field or pick `list`, `bytes`,"
            <> " `bytes-only`, or `packet`."
    | otherwise ->
        Left $ "unknown form value '" <> s
            <> "'; expected one of: list, bytes, bytes-only, packet."

-- | Parse a `with:` directive value: `<flag-spec>=<term-name>`.
-- `<flag-spec>` is either `--long` or `-x/--long`; short-only (`-x`)
-- is rejected because the long form is required for a stable descriptor
-- in `--help`. `<term-name>` is a plain morloc identifier.
parseWithSpec :: Text -> Either Text WithSpec
parseWithSpec raw =
  case T.breakOn "=" (T.strip raw) of
    (_, "") ->
      Left "expected `<flag-spec>=<term-name>` (missing `=`)."
    (flagPart, eqRest) ->
      let termPart = T.strip (T.drop 1 eqRest)
      in if T.null termPart
           then Left "term name is empty after `=`."
           else case parseWithFlagSpec flagPart of
             Left e -> Left e
             Right (short, long) -> Right (WithSpec short long (EV termPart))

parseWithFlagSpec :: Text -> Either Text (Maybe Char, Text)
parseWithFlagSpec txt = case parseCliOpt txt of
  CliOptOk (CliOptLong l)
    | isLongFlagName (T.unpack l) -> Right (Nothing, l)
    | otherwise -> Left (longFlagInvalidMsg (T.unpack l))
  CliOptOk (CliOptBoth c l)
    | isLongFlagName (T.unpack l) -> Right (Just c, l)
    | otherwise -> Left (longFlagInvalidMsg (T.unpack l))
  CliOptOk (CliOptShort _) -> Left
    $ "short-only flag spec is not allowed on `with:`; every terminal "
    <> "action must declare a long form (e.g. `-l/--lines=fmt_lines` "
    <> "or `--lines=fmt_lines`). The long form is what `--help` shows "
    <> "as a stable descriptor."
  CliOptShortCharInvalid c -> Left (shortCharInvalidMsg c)
  CliOptShapeUnknown -> Left
    $ "unrecognized flag spec; expected `-x/--long` or `--long`, e.g. "
    <> "`-l/--lines` or `--csv`."

isLongFlagName :: String -> Bool
isLongFlagName [] = False
isLongFlagName (h : rest) =
  isLongFlagFirst h && all isLongFlagRest rest
  where
    isLongFlagFirst c = c >= 'a' && c <= 'z'
    isLongFlagRest c =
         (c >= 'a' && c <= 'z')
      || (c >= '0' && c <= '9')
      || c == '-'

shortCharInvalidMsg :: Char -> Text
shortCharInvalidMsg c =
  "invalid short option character '" <> T.singleton c
  <> "' in `with:` spec. Short options must be a single ASCII letter "
  <> "(a-z, A-Z); digits are forbidden because they collide with "
  <> "negative-number argv values."

longFlagInvalidMsg :: String -> Text
longFlagInvalidMsg rest =
  "invalid long flag name `--" <> T.pack rest
  <> "` in `with:` spec. Long names must be lowercase-kebab: start "
  <> "with a lowercase letter, then any of [a-z0-9-]."

-- | Parse a check kind + value. Only `path` is recognized in v1.
--
-- Path modes follow the Python `open()` convention adapted to a
-- pre-open filesystem predicate:
--
--   r  = exists and readable       (input file)
--   w  = writable or creatable     (output file; create-or-overwrite)
--   x  = does not exist yet, parent writable (exclusive create)
--   rw = exists, readable, writable (in-place update)
parseCheck :: Text -> Text -> Either Text Check
parseCheck kind raw = case kind of
  "path" -> let v = T.strip raw in
            case v of
              "r"  -> Right (CheckPath (PathPerm "r"))
              "w"  -> Right (CheckPath (PathPerm "w"))
              "x"  -> Right (CheckPath (PathPerm "x"))
              "rw" -> Right (CheckPath (PathPerm "rw"))
              ""   -> Left "path check value is empty; expected one of: r, w, x, rw"
              _    -> Left $ "invalid path mode '" <> v
                          <> "': expected one of: r, w, x, rw"
  k -> Left $ "unknown check kind '" <> k <> "'; v1 supports: path"

-- Known directive keys recognized on `source` declarations.
sourceDocDirectiveKeys :: [Text]
sourceDocDirectiveKeys = ["name", "rsize"]

unknownDirectiveWarning :: [Text] -> Text -> Text
unknownDirectiveWarning knownKeys k =
  "warning: unknown docstring directive '" <> k <> "'"
  <> " (recognized: " <> T.intercalate ", " knownKeys <> "); "
  <> "if this was intended as prose, prefix the line with '\\' to suppress this warning (e.g. '\\"
  <> k <> ":')"

-- | Parse a single CLI-option directive value into the
-- [`ArgDocVars`] slot, recording an error when the value matched a
-- recognized short-flag shape but used a forbidden character.
-- A completely unrecognized shape (e.g. `arg: foobar`) is silently
-- preserved as `Nothing` -- the user's intent is ambiguous and the
-- existing pipeline treats absence as the default.
applyCliOptDirective ::
  Text -> -- directive key, used in error messages
  Text -> -- raw value text
  ([Text], Maybe CliOpt)
applyCliOptDirective k v = case parseCliOpt v of
  CliOptOk o -> ([], Just o)
  CliOptShapeUnknown -> ([], Nothing)
  CliOptShortCharInvalid c ->
    let msg =
          "invalid short option character in '" <> k <> ": " <> v <> "': "
          <> "got '" <> T.singleton c <> "'. "
          <> "Short options must use a single ASCII letter "
          <> "(a-z, A-Z); digits are forbidden because they collide "
          <> "with negative number arguments (e.g. '-5'). Use a "
          <> "letter, or the long form '--" <> k <> "'."
     in ([msg], Nothing)

processArgDocLines :: [Text] -> ([Text], [Text], ArgDocVars)
processArgDocLines = foldl step ([], [], defaultValue)
  where
    step (errs, ws, d) line = case parseDocKV line of
      DocDesc v
        | T.null v -> (errs, ws, d)
        | otherwise -> (errs, ws, d {docLines = docLines d <> [v]})
      DocDirective k v -> case T.splitOn "." k of
        ["name"] -> (errs, ws, d {docName = Just v})
        ["literal"] ->
          let parsed = parseDocBool v
              warn =
                if parsed
                  then
                    [ "warning: docstring directive `literal: true` is deprecated; "
                        <> "use `source: inline` instead. Both work today, but "
                        <> "`literal: true` will be removed in a future release."
                    ]
                  else []
           in (errs, ws <> warn, d {docLiteral = Just parsed})
        ["many"] -> (errs, ws, d {docMany = Just (parseDocBool v)})
        ["unroll"] -> (errs, ws, d {docUnroll = Just (parseDocBool v)})
        ["default"] -> (errs, ws, d {docDefault = Just v})
        ["metavar"] -> (errs, ws, d {docMetavar = Just v})
        ["arg"] ->
          let (es, o) = applyCliOptDirective k v
           in (errs <> es, ws, d {docArg = o})
        ["true"] ->
          let (es, o) = applyCliOptDirective k v
           in (errs <> es, ws, d {docTrue = o})
        ["false"] ->
          let (es, o) = applyCliOptDirective k v
           in (errs <> es, ws, d {docFalse = o})
        ["return"] -> (errs, ws, d {docReturn = Just v})
        ["source"] -> case parseSourceAtom v of
          Right a -> (errs, ws, d {docSource = Just a})
          Left e  -> (errs <> ["in `source: " <> v <> "`: " <> e], ws, d)
        ["form"] -> case parseFormAtom v of
          Right a -> (errs, ws, d {docForm = Just a})
          Left e  -> (errs <> ["in `form: " <> v <> "`: " <> e], ws, d)
        ["check", kind] -> case parseCheck kind v of
          Right c -> (errs, ws, d {docChecks = docChecks d <> [c]})
          Left e  -> (errs <> ["in `check." <> kind <> ": " <> v <> "`: " <> e], ws, d)
        ["list", "source"] -> case parseSourceAtom v of
          Right a -> (errs, ws, d {docListSource = Just a})
          Left e  -> (errs <> ["in `list.source: " <> v <> "`: " <> e], ws, d)
        ["list", "form"] -> case parseFormAtom v of
          Right a -> (errs, ws, d {docListForm = Just a})
          Left e  -> (errs <> ["in `list.form: " <> v <> "`: " <> e], ws, d)
        ["list", "check", kind] -> case parseCheck kind v of
          Right c -> (errs, ws, d {docListChecks = docListChecks d <> [c]})
          Left e  -> (errs <> ["in `list.check." <> kind <> ": " <> v <> "`: " <> e], ws, d)
        ["with"] -> case parseWithSpec v of
          Right ws' -> (errs, ws, d {docWith = docWith d <> [ws']})
          Left e -> (errs <> ["in `with: " <> v <> "`: " <> e], ws, d)
        _ ->
          let w = unknownDirectiveWarning argDocDirectiveKeys k
              desc = k <> ": " <> v
           in (errs, ws <> [w], d {docLines = docLines d <> [desc]})

parseDocBool :: Text -> Bool
parseDocBool v = v == "true" || v == "True"

processModuleDocLines :: [Text] -> ([Text], [Text], [[Text]])
processModuleDocLines = finalize . foldl step ([], Nothing, [])
  where
    step (desc, curEpi, epis) line = case parseDocKV line of
      DocDesc v -> case curEpi of
        Nothing -> (desc <> [v], Nothing, epis)
        Just epi -> (desc, Just (epi <> [v]), epis)
      DocDirective k _v -> case k of
        "epilogue" ->
          let epis' = case curEpi of
                Nothing -> epis
                Just epi -> epis <> [epi]
          in (desc, Just [], epis')
        _ ->
          let line' = k <> ": " <> _v
          in case curEpi of
            Nothing -> (desc <> [line'], Nothing, epis)
            Just epi -> (desc, Just (epi <> [line']), epis)

    finalize (desc, curEpi, epis) =
      let epis' = case curEpi of
            Nothing -> epis
            Just epi -> epis <> [epi]
      in ([], desc, epis')

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

-- | D-monad wrapper: parse argument docstring lines, accumulate
-- warnings into 'dsWarnings' for the caller to drain, and fail
-- the compile with a 'dfail' at `pos` when any directive value is
-- malformed (e.g. a short-option char that isn't a letter).
processArgDocLinesD :: Pos -> [Text] -> D ArgDocVars
processArgDocLinesD pos ls = do
  let (errs, ws, v) = processArgDocLines ls
  dwarn ws
  case errs of
    [] -> return v
    (e : _) -> dfail pos (T.unpack e)

-- | Validate signature-preamble `with:` specs: reject duplicate short/
-- long names within one export, reject collisions with reserved
-- command-scope flags (`-h`, `--help`), and reject collisions with the
-- signature's own argument-declared CLI names. The last is important
-- because the nexus registers `with:` flags on the same clap Command
-- as the arg-declared ones; a collision would abort the clap builder.
validateSigWith :: Pos -> [WithSpec] -> [ArgDocVars] -> D ()
validateSigWith pos specs argDocs = do
  let longs = [wsLong s | s <- specs]
      shorts = [c | s <- specs, Just c <- [wsShort s]]
      (argLongs, argShorts) = collectArgCliNames argDocs
      argLongSet = Set.fromList argLongs
      argShortSet = Set.fromList argShorts
  reportIfJust pos (firstDuplicate longs) $ \l ->
    "duplicate `with:` long name `--" <> l
    <> "` in this signature. Each terminal action needs a unique long form."
  reportIfJust pos (firstDuplicate shorts) $ \c ->
    "duplicate `with:` short name `-" <> T.singleton c
    <> "` in this signature. Each terminal action needs a unique short form."
  reportIfAny pos [l | l <- longs, l == "help"] $ \l ->
    "`with:` long name `--" <> l
    <> "` collides with a reserved command-scope flag. `--help` is "
    <> "always available; pick a different long name."
  reportIfAny pos [c | c <- shorts, c == 'h'] $ \c ->
    "`with:` short name `-" <> T.singleton c
    <> "` collides with a reserved command-scope flag. `-h` is always "
    <> "available; pick a different short letter."
  reportIfAny pos [l | l <- longs, Set.member l argLongSet] $ \l ->
    "`with:` long name `--" <> l
    <> "` already appears on one of this signature's own argument "
    <> "declarations (via `arg:` / `true:` / `false:`). Pick a different "
    <> "long name for the terminal action."
  reportIfAny pos [c | c <- shorts, Set.member c argShortSet] $ \c ->
    "`with:` short name `-" <> T.singleton c
    <> "` already appears on one of this signature's own argument "
    <> "declarations (via `arg:` / `true:` / `false:`). Pick a different "
    <> "short letter for the terminal action."

reportIfAny :: Pos -> [a] -> (a -> Text) -> D ()
reportIfAny _ [] _ = return ()
reportIfAny pos (x : _) f = dfail pos (T.unpack (f x))

reportIfJust :: Pos -> Maybe a -> (a -> Text) -> D ()
reportIfJust _ Nothing _ = return ()
reportIfJust pos (Just x) f = dfail pos (T.unpack (f x))

-- | Extract every long/short CLI name declared by an argument's docstring.
-- Handles the `arg:`, `true:`, `false:` fields (each of which can encode
-- a short-only / long-only / both spec).
collectArgCliNames :: [ArgDocVars] -> ([Text], [Char])
collectArgCliNames vs =
  ( concatMap longsOf slots, concatMap shortsOf slots )
  where
    slots = concatMap (\v -> [docArg v, docTrue v, docFalse v]) vs
    longsOf (Just (CliOptLong l)) = [l]
    longsOf (Just (CliOptBoth _ l)) = [l]
    longsOf _ = []
    shortsOf (Just (CliOptShort c)) = [c]
    shortsOf (Just (CliOptBoth c _)) = [c]
    shortsOf _ = []

firstDuplicate :: Ord a => [a] -> Maybe a
firstDuplicate = go Set.empty
  where
    go _ [] = Nothing
    go seen (x : xs)
      | Set.member x seen = Just x
      | otherwise = go (Set.insert x seen) xs

-- | Reject any `with:` atom that appears somewhere other than a
-- signature preamble. Used for per-argument docstrings, record-field
-- docstrings, type-alias docstrings, etc. `with:` is command-only.
rejectWithHere :: Pos -> Text -> ArgDocVars -> D ()
rejectWithHere pos ctx v =
  case docWith v of
    [] -> return ()
    (s : _) -> dfail pos . T.unpack $
      "`with:` is not allowed on " <> ctx
      <> "; it may only appear in a signature preamble (the `--'` "
      <> "lines directly above `name ::`). Offending atom: `with: "
      <> renderWithSpec s <> "`."

renderWithSpec :: WithSpec -> Text
renderWithSpec (WithSpec (Just c) l (EV t)) =
  "-" <> T.singleton c <> "/--" <> l <> "=" <> t
renderWithSpec (WithSpec Nothing l (EV t)) =
  "--" <> l <> "=" <> t

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
    stripLabels (OpU op args) = OpU op (map stripLabels args)
    stripLabels (LitU (LRec fs)) = LitU (LRec [(k, stripLabels v) | (k, v) <- fs])
    stripLabels (LitU (LList es)) = LitU (LList (map stripLabels es))
    stripLabels (LitU (LSet es)) = LitU (LSet (map stripLabels es))
    stripLabels (ExistU v (ps, pc) (rs, rc)) = ExistU v (map stripLabels ps, pc) (map (second stripLabels) rs, rc)
    stripLabels t = t

quantifyType :: TypeU -> TypeU
quantifyType t =
  let natVars = collectNatVars t
      strVars = collectStrVars t
      recVars = collectRecVars t
      t1 = promoteNatVars natVars t
      t2 = promoteStrVars strVars t1
      t3 = promoteRecVars recVars t2
      typeVars = nub (collectGenVars t3)
   in forallWrap typeVars t3
  where
    -- Collect type variables (excluding NatVarU/StrVarU which are already promoted)
    collectGenVars :: TypeU -> [TVar]
    collectGenVars (VarU v@(TV name))
      | not (T.null name), isLower (T.head name) = [v]
      | otherwise = []
    collectGenVars (ForallU v inner) = filter (/= v) (collectGenVars inner)
    collectGenVars (AppU f args) = collectGenVars f ++ concatMap collectGenVars args
    collectGenVars (FunU args ret) = concatMap collectGenVars args ++ collectGenVars ret
    collectGenVars (NamU _ _ ts entries) = concatMap collectGenVars ts ++ concatMap (collectGenVars . snd) entries
    collectGenVars (EffectU _ inner) = collectGenVars inner
    collectGenVars (OptionalU inner) = collectGenVars inner
    collectGenVars (OpU _ args) = concatMap collectGenVars args
    collectGenVars (LitU (LRec fs)) = concatMap (collectGenVars . snd) fs
    collectGenVars (LitU (LList es)) = concatMap collectGenVars es
    collectGenVars (LitU (LSet es)) = concatMap collectGenVars es
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
        go _ (StrConcatU a b) = go False a <> go False b
        go inNat (LabeledU _ inner) = go inNat inner
        go _ _ = Set.empty

    -- Collect variables that appear inside StrConcatU (str-kinded operator).
    -- Mirrors collectNatVars; typedef-based detection (e.g., labels in Table
    -- params) is handled by refineKinds. See plans/tables/04-str-solver-scope.md.
    collectStrVars :: TypeU -> Set.Set TVar
    collectStrVars = go False
      where
        go inStr (VarU v@(TV name))
          | inStr, not (T.null name), isLower (T.head name) = Set.singleton v
          | otherwise = Set.empty
        go _ (StrVarU v) = Set.singleton v
        go _ (ForallU _ inner) = go False inner
        go _ (AppU f args) = go False f <> Set.unions (map (go False) args)
        go _ (FunU args ret) = Set.unions (map (go False) args) <> go False ret
        go _ (NamU _ _ ts entries) = Set.unions (map (go False) ts) <> Set.unions (map (go False . snd) entries)
        go _ (EffectU _ inner) = go False inner
        go _ (OptionalU inner) = go False inner
        go _ (StrConcatU a b) = go True a <> go True b
        go _ (NatAddU a b) = go False a <> go False b
        go _ (NatMulU a b) = go False a <> go False b
        go _ (NatSubU a b) = go False a <> go False b
        go _ (NatDivU a b) = go False a <> go False b
        go inStr (LabeledU _ inner) = go inStr inner
        go _ _ = Set.empty

    -- Collect variables that appear inside Rec-kinded operators (RecExtendU,
    -- RecUnionU, RecDiffU, RecIntersectU). Mirrors collectStrVars; typedef-
    -- based detection (e.g. `r :: Rec` typedef param) is in refineKinds.
    collectRecVars :: TypeU -> Set.Set TVar
    collectRecVars = go False
      where
        go inRec (VarU v@(TV name))
          | inRec, not (T.null name), isLower (T.head name) = Set.singleton v
          | otherwise = Set.empty
        go _ (RecVarU v) = Set.singleton v
        go _ (ForallU _ inner) = go False inner
        go _ (AppU f args) = go False f <> Set.unions (map (go False) args)
        go _ (FunU args ret) = Set.unions (map (go False) args) <> go False ret
        go _ (NamU _ _ ts entries) = Set.unions (map (go False) ts) <> Set.unions (map (go False . snd) entries)
        go _ (EffectU _ inner) = go False inner
        go _ (OptionalU inner) = go False inner
        -- Rec operators put their tail/operand in a Rec-kinded position;
        -- the field-type slot of RecExtendU is regular Type-kinded, so we
        -- only treat the rest-of-rec position as Rec-kinded.
        go _ (RecExtendU _ a b) = go False a <> go True b
        go _ (RecUnionU a b) = go True a <> go True b
        go _ (RecDiffU a _) = go True a
        go _ (RecIntersectU a b) = go True a <> go True b
        go inRec (LabeledU _ inner) = go inRec inner
        go _ _ = Set.empty

-- | Generic VarU -> kind-specific variable promotion. The 'lift' callback
-- replaces a matching 'VarU' with the kind-specific carrier; all other
-- forms recurse uniformly.
promoteVars :: (TVar -> TypeU) -> Set.Set TVar -> TypeU -> TypeU
promoteVars lift vars = go
  where
    go (VarU v)
      | Set.member v vars = lift v
      | otherwise = VarU v
    go (ExistU v (ps, pc) (rs, rc)) = ExistU v (map go ps, pc) (map (second go) rs, rc)
    go (ForallU v t) = ForallU v (go t)
    go (FunU ts t) = FunU (map go ts) (go t)
    go (AppU t ts) = AppU (go t) (map go ts)
    go (NamU o n ps rs) = NamU o n (map go ps) [(k, go t) | (k, t) <- rs]
    go (EffectU effs t) = EffectU effs (go t)
    go (OptionalU t) = OptionalU (go t)
    go (OpU op args) = OpU op (map go args)
    go (LitU (LRec fs)) = LitU (LRec [(k, go v) | (k, v) <- fs])
    go (LitU (LList es)) = LitU (LList (map go es))
    go (LitU (LSet es)) = LitU (LSet (map go es))
    go (LabeledU n t) = LabeledU n (go t)
    go t = t  -- inert: kind-specific vars/voids, LNat, LStr

promoteNatVars :: Set.Set TVar -> TypeU -> TypeU
promoteNatVars = promoteVars NatVarU

promoteRecVars :: Set.Set TVar -> TypeU -> TypeU
promoteRecVars = promoteVars RecVarU

promoteStrVars :: Set.Set TVar -> TypeU -> TypeU
promoteStrVars = promoteVars StrVarU

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
  return [mkPrimOrClass name args]
extractConstraints (VarU (TV name)) =
  return [mkPrimOrClass name []]
extractConstraints (NamU NamRecord _ _ _) =
  dfail (Pos 0 0 "") "invalid constraint syntax"
extractConstraints t =
  case flattenTupleConstraint t of
    Just cs -> return cs
    Nothing -> dfail (Pos 0 0 "") ("invalid constraint: " ++ show t)

-- | Build a 'Constraint' from a head name and argument list, recognising
-- the primitive set-theoretic constraint forms (Member / Subset /
-- Disjoint) and routing everything else to the typeclass form. Mirrors
-- 'mkConstraint' in the parser; the two converge on the same set of
-- primitive heads so both grammar paths produce the same ADT.
mkPrimOrClass :: Text -> [TypeU] -> Constraint
mkPrimOrClass "Member" [a, s] = CMember a s
mkPrimOrClass "Subset" [a, b] = CSubset a b
mkPrimOrClass "Disjoint" [a, b] = CDisjoint a b
mkPrimOrClass name ts = Constraint (ClassName name) ts

flattenTupleConstraint :: TypeU -> Maybe [Constraint]
flattenTupleConstraint (AppU (VarU (TV name)) args)
  | T.isPrefixOf "Tuple" name = mapM typeToConstraint args
  | otherwise = Just [mkPrimOrClass name args]
flattenTupleConstraint (VarU (TV name)) =
  Just [mkPrimOrClass name []]
flattenTupleConstraint _ = Nothing

typeToConstraint :: TypeU -> Maybe Constraint
typeToConstraint (AppU (VarU (TV name)) args) =
  Just (mkPrimOrClass name args)
typeToConstraint (VarU (TV name)) =
  Just (mkPrimOrClass name [])
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
  argDocsWithPos <- mapM (\(p, _) -> (,) p <$> lookupDocsAt p) args
  argDocVars <- mapM (\(p, ls) -> processArgDocLinesD p ls) argDocsWithPos
  return (cs, argDocVars, argsToType args)
desugarSigType _pos (CstSigType Nothing args) = do
  argDocsWithPos <- mapM (\(p, _) -> (,) p <$> lookupDocsAt p) args
  argDocVars <- mapM (\(p, ls) -> processArgDocLinesD p ls) argDocsWithPos
  return ([], argDocVars, argsToType args)


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
buildAccessor sp body
  | bodyHasBracket body = buildAccessorBracket sp body
  | otherwise = do
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

-- True when any sub-component of the body is a bracket accessor. When True,
-- we use the lambda-based path below: bracket access lowers to function-call
-- chains via Indexable/Sliceable/Functor, which is structurally incompatible
-- with the Selector ADT used by PatternStruct.
bodyHasBracket :: CstAccessorBody -> Bool
bodyHasBracket (CABKey _ t) = tailHasBracket t
bodyHasBracket (CABIdx _ t) = tailHasBracket t
bodyHasBracket (CABGroup bs) = any bodyHasBracket bs
bodyHasBracket (CABBracket _ _) = True

tailHasBracket :: CstAccessorTail -> Bool
tailHasBracket CATEnd = False
tailHasBracket (CATSet _) = False
tailHasBracket (CATChain b) = bodyHasBracket b

-- Lambda-based accessor path. Produces an unapplied function (LamE) whose body
-- chains together selector PatE applications, bracket index/slice/map calls,
-- and group tuple constructions.
buildAccessorBracket :: Span -> CstAccessorBody -> D ExprI
buildAccessorBracket sp body = do
  vId <- freshIdSpan sp
  let v = EV (".bracket_arg_" <> T.pack (show vId))
  vExpr <- freshExprSpan sp (VarE defaultValue v)
  inner <- applyAccessor sp body vExpr
  lamId <- freshIdSpan sp
  return (ExprI lamId (LamE [v] inner))

-- Apply an accessor body to a concrete subject expression. Recursively builds
-- the AppE / TupE / map-LamE chain that the lambda path needs.
applyAccessor :: Span -> CstAccessorBody -> ExprI -> D ExprI
applyAccessor sp (CABKey name tail') subject = do
  applied <- applyKeyPat sp name subject
  applyTail sp tail' applied
applyAccessor sp (CABIdx idx tail') subject = do
  applied <- applyIdxPat sp idx subject
  applyTail sp tail' applied
applyAccessor sp (CABGroup bodies) subject = do
  -- First try to fold the whole group into a single unified-Selector
  -- PatCall. This keeps the pattern as ONE first-class object
  -- (instead of a TupE of separate PatCall applications), which lets
  -- IFile-aware codegen emit a single walker call and gives any
  -- future pattern-level optimisation an undisturbed AST to work on.
  --
  -- Unification falls back to the legacy TupE form when the group
  -- contains shapes that the unified Selector ADT cannot represent:
  -- mixed-kind siblings (key + index), bracket-headed siblings,
  -- nested groups, or bracket-slice followed by a chain (which
  -- lowers to IntrMap, not a Selector step).
  mUnified <- tryUnifiedGroupSelector sp bodies
  case mUnified of
    Just (sel, runtimeArgs) -> do
      patI <- freshExprSpan sp (PatE (PatternStruct sel))
      freshExprSpan sp (AppE patI (runtimeArgs ++ [subject]))
    Nothing ->
      legacyGroupTupE sp bodies subject
applyAccessor sp (CABBracket axes tail') subject =
  lowerBracket sp axes tail' subject

-- | Fall-back lowering: build a TupE of per-branch accessor
-- applications. Used when 'tryUnifiedGroupSelector' returns Nothing.
legacyGroupTupE :: Span -> [CstAccessorBody] -> ExprI -> D ExprI
legacyGroupTupE sp bodies subject =
  -- Each branch needs its own fresh VarE node referencing the subject
  -- variable. Sharing one ExprI across multiple AppE positions confuses
  -- the typechecker (every ExprI ID is a unique typed position). If
  -- the subject is already a VarE, fresh VarE references to the same
  -- name resolve via name lookup; otherwise we bind it to a let-
  -- variable first.
  case subject of
    ExprI _ (VarE _ subjName) -> do
      parts <- mapM
        (\b -> do
           vExpr <- freshExprSpan sp (VarE defaultValue subjName)
           applyAccessor sp b vExpr)
        bodies
      freshExprSpan sp (TupE parts)
    _ -> do
      vId <- freshIdSpan sp
      let v = EV (".group_subj_" <> T.pack (show vId))
      parts <- mapM
        (\b -> do
           vExpr <- freshExprSpan sp (VarE defaultValue v)
           applyAccessor sp b vExpr)
        bodies
      tupE <- freshExprSpan sp (TupE parts)
      freshExprSpan sp (LetE [(v, subject)] tupE)

-- | Try to combine a list of accessor bodies into ONE unified
-- Selector with sibling tails. Returns 'Just (selector, runtimeArgs)'
-- on success; 'Nothing' if the group can't be unified.
--
-- All siblings must share a head kind (all key, or all index). The
-- sibling tails are independent; they may contain bracket steps,
-- field chains, or any mix thereof.
tryUnifiedGroupSelector
  :: Span
  -> [CstAccessorBody]
  -> D (Maybe (Selector, [ExprI]))
tryUnifiedGroupSelector sp bodies = do
  msibs <- traverse (tryBodyAsSibling sp) bodies
  case sequence msibs of
    Nothing -> return Nothing
    Just sibs -> return (combineSiblings sibs)

-- | Per-sibling extracted shape. A sibling is the head step (field
-- key or index) plus its tail Selector and any runtime arg
-- expressions the tail brings (bracket bounds, in DFS order).
data UnifiedSibling
  = USKey Text Selector [ExprI]
  | USIdx Int Selector [ExprI]

tryBodyAsSibling :: Span -> CstAccessorBody -> D (Maybe UnifiedSibling)
tryBodyAsSibling sp (CABKey k tail') = do
  mTail <- tryTailToSelector sp tail'
  return $ case mTail of
    Just (sel, args) -> Just (USKey k sel args)
    Nothing          -> Nothing
tryBodyAsSibling sp (CABIdx i tail') = do
  mTail <- tryTailToSelector sp tail'
  return $ case mTail of
    Just (sel, args) -> Just (USIdx i sel args)
    Nothing          -> Nothing
tryBodyAsSibling _ (CABBracket _ _) = return Nothing
tryBodyAsSibling _ (CABGroup _)     = return Nothing

-- | Convert an accessor body that lives inside a chain (after a head
-- step) into a Selector + runtime args. Used by 'tryTailToSelector'.
tryBodyToChain :: Span -> CstAccessorBody -> D (Maybe (Selector, [ExprI]))
tryBodyToChain sp (CABKey k tail') = do
  mTail <- tryTailToSelector sp tail'
  return $ case mTail of
    Just (sel, args) -> Just (SelectorKey (k, sel) [], args)
    Nothing          -> Nothing
tryBodyToChain sp (CABIdx i tail') = do
  mTail <- tryTailToSelector sp tail'
  return $ case mTail of
    Just (sel, args) -> Just (SelectorIdx (i, sel) [], args)
    Nothing          -> Nothing
tryBodyToChain sp (CABBracket axes tail') = case axes of
  [BAxIdx eLoc] -> do
    iExpr <- desugarExpr eLoc
    mTail <- tryTailToSelector sp tail'
    return $ case mTail of
      Just (sel, args) -> Just (SelectorBracketIndex sel, iExpr : args)
      Nothing          -> Nothing
  [BAxSlice mStart mStop mStep] -> case tail' of
    CATEnd -> do
      sE <- desugarSliceBound sp mStart
      eE <- desugarSliceBound sp mStop
      pE <- desugarSliceBound sp mStep
      return $ Just (SelectorBracketSlice, [sE, eE, pE])
    -- bracket-slice with a chain tail lowers to IntrMap, not to a
    -- Selector step. Fall back to the legacy lambda path.
    _ -> return Nothing
  _ -> return Nothing
tryBodyToChain _ (CABGroup _) = return Nothing

tryTailToSelector :: Span -> CstAccessorTail -> D (Maybe (Selector, [ExprI]))
tryTailToSelector _ CATEnd          = return $ Just (SelectorEnd, [])
tryTailToSelector _ (CATSet _)      = return Nothing
tryTailToSelector sp (CATChain b)   = tryBodyToChain sp b

-- | Combine unified siblings into a single Selector. Same-kind
-- siblings (all keys, or all indices) compose into the existing
-- multi-sibling 'SelectorKey' / 'SelectorIdx' form. Mixed-kind
-- groups can't be represented and force the caller back to the
-- legacy TupE form.
combineSiblings :: [UnifiedSibling] -> Maybe (Selector, [ExprI])
combineSiblings [] = Nothing
combineSiblings sibs
  | all isKey sibs =
      let pairs   = [(k, s) | USKey k s _ <- sibs]
          allArgs = concat [a | USKey _ _ a <- sibs]
      in case pairs of
        []     -> Nothing
        h : tl -> Just (SelectorKey h tl, allArgs)
  | all isIdx sibs =
      let pairs   = [(i, s) | USIdx i s _ <- sibs]
          allArgs = concat [a | USIdx _ _ a <- sibs]
      in case pairs of
        []     -> Nothing
        h : tl -> Just (SelectorIdx h tl, allArgs)
  | otherwise = Nothing
  where
    isKey USKey{} = True
    isKey _       = False
    isIdx USIdx{} = True
    isIdx _       = False

applyKeyPat :: Span -> Text -> ExprI -> D ExprI
applyKeyPat sp name subject = do
  pat <- freshExprSpan sp (PatE (PatternStruct (SelectorKey (name, SelectorEnd) [])))
  freshExprSpan sp (AppE pat [subject])

applyIdxPat :: Span -> Int -> ExprI -> D ExprI
applyIdxPat sp idx subject = do
  pat <- freshExprSpan sp (PatE (PatternStruct (SelectorIdx (idx, SelectorEnd) [])))
  freshExprSpan sp (AppE pat [subject])

applyTail :: Span -> CstAccessorTail -> ExprI -> D ExprI
applyTail _ CATEnd e = return e
applyTail sp (CATSet _) _ =
  dfail (startPos sp)
    "setters are not supported on accessor chains that contain a bracket"
applyTail sp (CATChain body) e = applyAccessor sp body e

lowerBracket :: Span -> [CstBracketAxis] -> CstAccessorTail -> ExprI -> D ExprI
lowerBracket sp axes tail' subject = case axes of
  [] -> dfail (startPos sp) "empty bracket accessor"
  [BAxIdx eLoc] -> do
    iExpr <- desugarExpr eLoc
    -- Emit a PatternBracketIndex pattern applied to (index, receiver).
    -- The index expression stays at its natural type; codegen inserts
    -- any language-specific cast (e.g. __to_index__) when emitting the
    -- pool call. Keeping the desugar output free of sourced functions
    -- preserves "pure" status for bracket expressions that would
    -- otherwise be evaluable on the nexus.
    indexPat <- freshExprSpan sp (PatE PatternBracketIndex)
    callExpr <- freshExprSpan sp (AppE indexPat [iExpr, subject])
    applyTail sp tail' callExpr
  [BAxSlice mStart mStop mStep] -> do
    sliceCall <- buildSliceCall sp mStart mStop mStep subject
    case tail' of
      CATEnd -> return sliceCall
      CATSet _ ->
        dfail (startPos sp)
          "setters are not supported on slice accessors"
      CATChain bodyTail -> do
        eId <- freshIdSpan sp
        let eName = EV (".bracket_elem_" <> T.pack (show eId))
        eExpr <- freshExprSpan sp (VarE defaultValue eName)
        innerBody <- applyAccessor sp bodyTail eExpr
        lamId <- freshIdSpan sp
        let lamExpr = ExprI lamId (LamE [eName] innerBody)
        -- Emit the implicit map as the IntrMap intrinsic rather than
        -- a name reference to `map`. Going through `VarE "map"` would
        -- require the user's module to import a binding for `map`
        -- (typically Functor's class method via root) -- a silent
        -- dependency the user has no reason to expect from a bracket
        -- accessor. The intrinsic node is self-contained: the
        -- pure-runtime path lowers it to the Map evaluator branch
        -- directly, and the pool path resolves @Functor.map@ for the
        -- target language at codegen.
        freshExprSpan sp (IntrinsicE IntrMap [lamExpr, sliceCall])
  _ -> dfail (startPos sp)
         "multi-axis bracket accessors are not supported in v1 (1D lists only)"

buildSliceCall
  :: Span
  -> Maybe (Loc CstExpr)
  -> Maybe (Loc CstExpr)
  -> Maybe (Loc CstExpr)
  -> ExprI
  -> D ExprI
buildSliceCall sp mStart mStop mStep subject = do
  startE <- desugarSliceBound sp mStart
  stopE  <- desugarSliceBound sp mStop
  stepE  <- desugarSliceBound sp mStep
  -- Emit a PatternBracketSlice pattern applied to (start, stop, step,
  -- receiver). The typechecker synthesizes the result type structurally:
  -- a Nat-parameterized container has its outer Nat replaced by
  -- NatVoidU (the wildcard sentinel); otherwise the receiver type is
  -- preserved. The codegen translator emits the appropriate native
  -- slicing operation.
  slicePat <- freshExprSpan sp (PatE PatternBracketSlice)
  freshExprSpan sp (AppE slicePat [startE, stopE, stepE, subject])

-- | Lower a single slice bound. Missing positions become
-- @(Null :: ?Int64)@: annotating the synthetic Null pins every empty
-- slot to a uniform wire schema so the runtime and pool codegen don't
-- need to invent a default. User-supplied bounds stay at their natural
-- type; codegen inserts any language-specific cast (e.g.
-- @__to_index__@) at emit time.
desugarSliceBound :: Span -> Maybe (Loc CstExpr) -> D ExprI
desugarSliceBound sp Nothing = do
  nullExpr <- freshExprSpan sp NullE
  freshExprSpan sp (AnnE nullExpr (OptionalU BT.i64U))
desugarSliceBound _ (Just e) = desugarExpr e

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
desugarAccessorBody (CABBracket _ _) =
  -- buildAccessor dispatches CABBracket through buildAccessorBracket before
  -- calling this; reaching here would be a compiler-internal error.
  dfail (Pos 0 0 "") "internal error: CABBracket in selector-only path"

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
-- Irrefutable-pattern desugaring
--------------------------------------------------------------------

-- | Convert an expression appearing in a binding position into an
-- irrefutable pattern. Binding positions (lambda params, function-def
-- args, LHS of let, LHS of do-bind) are parsed as expressions to avoid
-- LALR expr/pat overlap; this converter enforces the pattern subset.
exprToIrrefPat :: Loc CstExpr -> D (Loc CstIrrefPat)
exprToIrrefPat (Loc sp (CVarE v))       = return (Loc sp (CIPatVar v))
exprToIrrefPat (Loc sp CUnderscoreE)    = return (Loc sp CIPatWild)
exprToIrrefPat (Loc sp (CAsE v inner))  = do
  inner' <- exprToIrrefPat inner
  return (Loc sp (CIPatAs v inner'))
exprToIrrefPat (Loc _ (CParenE inner))  = exprToIrrefPat inner
exprToIrrefPat (Loc sp (CTupE es))
  | length es < 2 =
      dfail (startPos sp) "tuple pattern requires at least two components"
  | otherwise = do
      ps <- mapM exprToIrrefPat es
      return (Loc sp (CIPatTup ps))
exprToIrrefPat (Loc sp (CNamE entries)) = do
  ps <- mapM (\(k, e) -> do p <- exprToIrrefPat e; return (k, p)) entries
  return (Loc sp (CIPatRec ps))
exprToIrrefPat (Loc sp _) =
  dfail (startPos sp)
    "expected a pattern (variable, '_', tuple, record, or label@pattern) in binding position"

-- | Extend an existing selector chain by one tuple-index step at the tail.
extendSelIdx :: Int -> Selector -> Selector
extendSelIdx i SelectorEnd             = SelectorIdx (i, SelectorEnd) []
extendSelIdx i (SelectorIdx (j, t) ss) = SelectorIdx (j, extendSelIdx i t) ss
extendSelIdx i (SelectorKey (k, t) ss) = SelectorKey (k, extendSelIdx i t) ss
extendSelIdx _ sel                     = sel  -- bracket selectors: not reached from irrefutable-pattern walk

-- | Extend an existing selector chain by one record-key step at the tail.
extendSelKey :: Text -> Selector -> Selector
extendSelKey k SelectorEnd              = SelectorKey (k, SelectorEnd) []
extendSelKey k (SelectorIdx (j, t) ss)  = SelectorIdx (j, extendSelKey k t) ss
extendSelKey k (SelectorKey (k', t) ss) = SelectorKey (k', extendSelKey k t) ss
extendSelKey _ sel                      = sel

-- | Walk an irrefutable pattern to its leaves, collecting
-- (bound-name, chained-selector-from-receiver) pairs. Wildcards contribute
-- no leaves; as-patterns bind the label at the current chain and continue
-- with the inner pattern.
walkIrrefPat :: Loc CstIrrefPat -> Selector -> [(EVar, Selector)]
walkIrrefPat (Loc _ (CIPatVar v))   sel = [(v, sel)]
walkIrrefPat (Loc _ CIPatWild)      _   = []
walkIrrefPat (Loc _ (CIPatAs v p))  sel = (v, sel) : walkIrrefPat p sel
walkIrrefPat (Loc _ (CIPatTup ps))  sel = concat
  [ walkIrrefPat p (extendSelIdx i sel) | (i, p) <- zip [0 ..] ps ]
walkIrrefPat (Loc _ (CIPatRec kps)) sel = concat
  [ walkIrrefPat p (extendSelKey k sel) | (Key k, p) <- kps ]

-- | Emit one LetE-style binding per leaf, projecting from 'receiver'.
-- A SelectorEnd chain (as-pattern binding the entire receiver) is
-- realized as a bare VarE reference rather than a PatE application on
-- an empty chain.
materializeIrrefLeaves :: Span -> EVar -> [(EVar, Selector)] -> D [(EVar, ExprI)]
materializeIrrefLeaves sp recv = mapM $ \(v, sel) -> do
  subj <- freshExprSpan sp (VarE defaultValue recv)
  rhs <- case sel of
    SelectorEnd -> return subj
    _ -> do
      pat <- freshExprSpan sp (PatE (PatternStruct sel))
      freshExprSpan sp (AppE pat [subj])
  return (v, rhs)

-- | If the desugared expression is a bare VarE, return its name so callers
-- can chain accessors directly on the variable instead of introducing a
-- receiver temp.
asBareVar :: ExprI -> Maybe EVar
asBareVar (ExprI _ (VarE _ v)) = Just v
asBareVar _                    = Nothing

-- | Fresh name for the receiver temp introduced when an irrefutable pattern
-- destructures a non-variable RHS. Also used for _ = rhs to keep the RHS
-- evaluated for effect.
freshIrrefVar :: Span -> D EVar
freshIrrefVar sp = do
  idx <- freshIdSpan sp
  return (EV ("_irref_" <> T.pack (show idx)))

-- | Fresh name for the lambda parameter introduced when a lambda or
-- function-arg is written with an irrefutable pattern rather than a plain
-- variable. Prefix matches the existing 'mlcp_x_' convention used for
-- implicit-main and eta-expansion parameters.
freshIrrefLamParam :: Span -> D EVar
freshIrrefLamParam sp = do
  idx <- freshIdSpan sp
  return (EV ("mlcp_x_" <> T.pack (show idx)))

-- | Extract the span of a Loc-wrapped value.
irrefPatSpan :: Loc CstIrrefPat -> Span
irrefPatSpan (Loc s _) = s

-- | Given an irrefutable pattern and its already-desugared RHS, produce a
-- flat list of (name, expr) bindings suitable for a LetE. Introduces one
-- fresh temp when the receiver is not already a bare variable. A pattern
-- whose only content is a wildcard still emits a binding so its RHS is
-- evaluated (do-block <- semantics).
desugarIrrefPat :: Loc CstIrrefPat -> ExprI -> D [(EVar, ExprI)]
-- Bare variable pattern: bind rhs directly. No temp, no aliasing, so
-- downstream codegen sees the same shape as pre-pattern let/do-bind.
desugarIrrefPat (Loc _ (CIPatVar v)) rhs = return [(v, rhs)]
desugarIrrefPat p rhs = do
  validateIrrefPat p
  let leaves = walkIrrefPat p SelectorEnd
      sp = irrefPatSpan p
  case leaves of
    [] -> do
      throwaway <- freshIrrefVar sp
      return [(throwaway, rhs)]
    _ -> case asBareVar rhs of
      Just recv -> materializeIrrefLeaves sp recv leaves
      Nothing -> do
        tmp <- freshIrrefVar sp
        body <- materializeIrrefLeaves sp tmp leaves
        return ((tmp, rhs) : body)

-- | Desugar an irrefutable pattern into a fresh lambda parameter plus
-- inner projection bindings. Returns (param-name, [(binder, projection)]).
-- The caller is expected to wrap the projection bindings in a LetE around
-- the lambda body.
--
-- The 'usedNames' set holds names free in the lambda body -- leaves whose
-- binder is not used are dropped. This is required because a lambda
-- parameter's type is existential until the lambda is applied; a
-- projection off it (e.g. @b = .1 mlcp_x_N@) picks up an unresolved
-- @_pattern_@ existential in its stored type, and nothing later closes it
-- because @b@ is dead. Codegen then fails with "cannot serialize type:
-- _pattern_N" at export.
desugarIrrefLamParam :: Set.Set EVar -> Loc CstIrrefPat -> D (EVar, [(EVar, ExprI)])
-- Bare variable pattern: use the variable name directly as the lambda
-- parameter. No fresh temp, no let-alias -- preserves the pre-pattern
-- LamE shape that downstream codegen (Express, Realize) expects.
desugarIrrefLamParam _ (Loc _ (CIPatVar v)) = return (v, [])
desugarIrrefLamParam usedNames p = do
  validateIrrefPat p
  let sp = irrefPatSpan p
  v <- freshIrrefLamParam sp
  let leaves = walkIrrefPat p SelectorEnd
      usedLeaves = filter (\(n, _) -> Set.member n usedNames) leaves
  bindings <- materializeIrrefLeaves sp v usedLeaves
  return (v, bindings)

-- | Free-variable set of an already-desugared expression. Used to decide
-- which pattern-projection let-bindings a lambda body actually needs.
-- Exhaustive on 'Expr' constructors so a new constructor forces a decision
-- here (missing one silently makes 'desugarIrrefLamParam' drop live bindings).
freeVarsE :: ExprI -> Set.Set EVar
freeVarsE (ExprI _ e) = case e of
  -- Value expressions that carry names
  VarE _ v            -> Set.singleton v
  BopE l _ v r        -> Set.unions [freeVarsE l, Set.singleton v, freeVarsE r]
  LstE es             -> Set.unions (map freeVarsE es)
  TupE es             -> Set.unions (map freeVarsE es)
  NamE kes            -> Set.unions (map (freeVarsE . snd) kes)
  AppE f xs           -> Set.union (freeVarsE f) (Set.unions (map freeVarsE xs))
  LamE params body    -> Set.difference (freeVarsE body) (Set.fromList params)
  AnnE inner _        -> freeVarsE inner
  LetE bindings body  ->
    let bound = Set.fromList (map fst bindings)
        rhsFvs = Set.unions (map (freeVarsE . snd) bindings)
    in Set.union (Set.difference (freeVarsE body) bound) rhsFvs
  IfE c t f           -> Set.unions [freeVarsE c, freeVarsE t, freeVarsE f]
  DoBlockE inner      -> freeVarsE inner
  EvalE inner         -> freeVarsE inner
  IntrinsicE _ es     -> Set.unions (map freeVarsE es)
  ParenE inner        -> freeVarsE inner
  AssE _ body wheres  -> Set.union (freeVarsE body) (Set.unions (map freeVarsE wheres))
  IstE _ _ body       -> Set.unions (map freeVarsE body)
  ModE _ body         -> Set.unions (map freeVarsE body)
  -- Leaf value expressions (no names bound or referenced)
  UniE                -> Set.empty
  NullE               -> Set.empty
  IntE _              -> Set.empty
  RealE _             -> Set.empty
  LogE _              -> Set.empty
  StrE _              -> Set.empty
  PatE _              -> Set.empty
  -- Declaration nodes: only appear at top level, not inside lambda bodies.
  -- Return empty rather than error so that a defensive caller against
  -- unusual desugarings still produces a well-defined FV set.
  ClsE _              -> Set.empty
  EffE _ _            -> Set.empty
  TypE _              -> Set.empty
  ImpE _              -> Set.empty
  ExpE _              -> Set.empty
  SrcE _              -> Set.empty
  SigE _              -> Set.empty
  FixE _              -> Set.empty

-- | Names bound by an irrefutable pattern, each paired with the source
-- position of the leaf that binds it. Wildcards contribute nothing;
-- as-patterns contribute both the label and any names in the inner pattern.
-- Positions let 'validateIrrefPat' caret at the duplicating occurrence;
-- callers who only need names take 'fst'.
irrefPatBoundNames :: Loc CstIrrefPat -> [(Text, Pos)]
irrefPatBoundNames (Loc sp (CIPatVar (EV n)))  = [(n, startPos sp)]
irrefPatBoundNames (Loc _  CIPatWild)          = []
irrefPatBoundNames (Loc sp (CIPatAs (EV n) p)) = (n, startPos sp) : irrefPatBoundNames p
irrefPatBoundNames (Loc _  (CIPatTup ps))      = concatMap irrefPatBoundNames ps
irrefPatBoundNames (Loc _  (CIPatRec kps))     = concatMap (irrefPatBoundNames . snd) kps

-- | Reject an irrefutable pattern that binds the same name twice, e.g.
-- (x, x) = e or {a=x, b=x} = r or x@(x, _) = e. Carets at the second
-- occurrence's source position.
validateIrrefPat :: Loc CstIrrefPat -> D ()
validateIrrefPat p =
  case findFirstDup Set.empty (irrefPatBoundNames p) of
    Nothing -> return ()
    Just (n, pos) -> dfail pos ("duplicate binder in pattern: " ++ T.unpack n)
  where
    findFirstDup :: Set.Set Text -> [(Text, Pos)] -> Maybe (Text, Pos)
    findFirstDup _ [] = Nothing
    findFirstDup seen ((x, pos) : xs)
      | Set.member x seen = Just (x, pos)
      | otherwise = findFirstDup (Set.insert x seen) xs

--------------------------------------------------------------------
-- Do-notation desugaring
--------------------------------------------------------------------

-- Desugar a do-block to a let-chain. Non-final bare statements and <- binds
-- are wrapped in EvalE so the typechecker sees them as forced effects (pure
-- non-finals are therefore rejected). The final bare statement is returned
-- unwrapped: synthE DoBlockS flattens it when effectful, otherwise the block
-- type is <collected-effects> (type-of-final) and an empty effect set is a
-- subtype of any expected effect set.
desugarDo :: Span -> [CstDoStmt] -> D ExprI
desugarDo sp [] = dfail (startPos sp) "empty do block"
desugarDo _sp [CstDoBare e] = desugarExpr e
desugarDo sp [CstDoBind _ _] = dfail (startPos sp) "do block cannot end with a bind (<-)"
desugarDo sp [CstDoLet _ _] = dfail (startPos sp) "do block cannot end with a let binding"
desugarDo sp (CstDoLet p e : rest) = do
  p' <- exprToIrrefPat p
  e' <- desugarExpr e
  bindings <- desugarIrrefPat p' e'
  restE <- desugarDo sp rest
  freshExprSpan sp (LetE bindings restE)
desugarDo sp (CstDoBind p e : rest) = do
  p' <- exprToIrrefPat p
  e' <- desugarExpr e
  forceE <- freshExprSpan sp (EvalE e')
  bindings <- desugarIrrefPat p' forceE
  restE <- desugarDo sp rest
  freshExprSpan sp (LetE bindings restE)
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
    Just config -> freshExprSpan sp (VarE (config { manifoldConfigLabel = Just label }) v)
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
-- Intrinsics: eta-expand when under-applied so they behave as first-class functions
desugarExpr (Loc sp (CIntrinsicE name)) = do
  intr <- resolveIntrinsic (startPos sp) name
  etaExpandIntrinsic sp intr []
desugarExpr (Loc sp (CAppE (Loc _ (CIntrinsicE name)) args)) = do
  intr <- resolveIntrinsic (startPos sp) name
  args' <- mapM desugarExpr args
  etaExpandIntrinsic sp intr args'
-- Direct application of an accessor to a subject. Without this case, the
-- general @CAppE@ path below desugars @.a.[0] e@ as @AppE (LamE [bracket_arg]
-- body) [e]@ -- introducing a fresh existential @bracket_arg@ that the
-- typechecker has to merge with @e@'s existential later. When two such
-- accessor-applied chains share a binder in a tuple (e.g.
-- @(.a.[0].0 e, .b.[0] e)@), the per-branch @bracket_arg@s accumulate
-- record-key constraints separately, and the AppU<:AppU subtype rule fails
-- to propagate @f := List@ into both branches. Inlining the application at
-- desugar time keeps a single existential -- the user's binder -- so all
-- record-key constraints accumulate in one place.
desugarExpr (Loc _ (CAppE (Loc accSp (CAccessorE body)) (firstArg : rest)))
  | bodyHasBracket body = do
      argE <- desugarExpr firstArg
      applied <- applyAccessor accSp body argE
      case rest of
        [] -> return applied
        moreArgs -> do
          rest' <- mapM desugarExpr moreArgs
          freshExprFrom applied (AppE applied rest')
-- Compound expressions
desugarExpr (Loc _ (CAppE f args)) = do
  f' <- desugarExpr f
  args' <- mapM desugarExpr args
  freshExprFrom f' (AppE f' args')
desugarExpr (Loc sp (CLamE pats body)) = do
  pats' <- mapM exprToIrrefPat pats
  body' <- desugarExpr body
  buildLamWithIrrefPats sp pats' body'
desugarExpr (Loc sp (CLetE bindings body)) = do
  bindings' <- concatMapM (\(p, e) -> do
    p' <- exprToIrrefPat p
    e' <- desugarExpr e
    desugarIrrefPat p' e') bindings
  body' <- desugarExpr body
  freshExprSpan sp (LetE bindings' body')
desugarExpr (Loc sp (CParenE inner@(Loc _ CBopE{}))) = do
  inner' <- desugarExpr inner
  freshExprSpan sp (ParenE inner')
desugarExpr (Loc _ (CParenE inner)) = desugarExpr inner
desugarExpr (Loc sp (CLeftSecE lhs opTok)) = do
  idx <- freshIdSpan sp
  let v = EV ("_sect_" <> T.pack (show idx))
  lhs' <- desugarExpr lhs
  vExpr <- freshExprSpan sp (VarE defaultValue v)
  opI <- freshIdSpan (Span (locPos opTok) (locPos opTok))
  bopExpr <- freshExprSpan sp (BopE lhs' opI (tokToEVar opTok) vExpr)
  freshExprSpan sp (LamE [v] bopExpr)
desugarExpr (Loc sp (CRightSecE opTok rhs)) = do
  idx <- freshIdSpan sp
  let v = EV ("_sect_" <> T.pack (show idx))
  rhs' <- desugarExpr rhs
  vExpr <- freshExprSpan sp (VarE defaultValue v)
  opI <- freshIdSpan (Span (locPos opTok) (locPos opTok))
  bopExpr <- freshExprSpan sp (BopE vExpr opI (tokToEVar opTok) rhs')
  freshExprSpan sp (LamE [v] bopExpr)
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

-- Top-level declarations should not appear inside expressions
desugarExpr (Loc _ CModE{}) = error "desugarExpr: unexpected CModE in expression position"
desugarExpr (Loc _ (CImpE {})) = error "desugarExpr: unexpected CImpE in expression position"
desugarExpr (Loc _ (CSigE {})) = error "desugarExpr: unexpected CSigE in expression position"
desugarExpr (Loc _ (CAssE {})) = error "desugarExpr: unexpected CAssE in expression position"
desugarExpr (Loc _ (CTypE {})) = error "desugarExpr: unexpected CTypE in expression position"
desugarExpr (Loc _ (CClsE {})) = error "desugarExpr: unexpected CClsE in expression position"
desugarExpr (Loc _ (CIstE {})) = error "desugarExpr: unexpected CIstE in expression position"
desugarExpr (Loc _ (CEffE {})) = error "desugarExpr: unexpected CEffE in expression position"
desugarExpr (Loc _ (CFixE {})) = error "desugarExpr: unexpected CFixE in expression position"
desugarExpr (Loc _ (CSrcOldE {})) = error "desugarExpr: unexpected CSrcOldE in expression position"
desugarExpr (Loc _ (CSrcNewE {})) = error "desugarExpr: unexpected CSrcNewE in expression position"
desugarExpr (Loc _ (CGuardedAssE {})) = error "desugarExpr: unexpected CGuardedAssE in expression position"
desugarExpr (Loc _ (CInlineE {})) = error "desugarExpr: unexpected CInlineE in expression position"
desugarExpr (Loc sp CUnderscoreE) =
  dfail (startPos sp) "'_' is only valid in a binding position (let / do-bind / lambda / function-def arg)"
desugarExpr (Loc sp (CAsE _ _)) =
  dfail (startPos sp) "as-pattern '@' is only valid in a binding position"

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
  -- capture module-level docstrings (--' lines before module keyword)
  docs <- lookupDocsAt (startPos sp)
  let (_warns, desc, epis) = processModuleDocLines docs
  State.modify $ \s -> s
    { dsModuleDoc = desc
    , dsModuleEpilogues = epis
    }
  expExprI <- desugarExport sp export
  bodyExprs <- concatMapM desugarTopLevel body
  modI <- freshIdSpan sp
  return [ExprI modI (ModE (MV name) (expExprI : bodyExprs))]
desugarTopLevel (Loc sp (CImpE imp)) = do
  e <- freshExprSpan sp (ImpE imp)
  return [e]
desugarTopLevel (Loc sp (CSigE name sigType)) = do
  docs <- lookupDocsAt (startPos sp)
  cmdDoc <- processArgDocLinesD (startPos sp) docs
  captureSigDescLines name (docLines cmdDoc)
  (cs, argDocs, t) <- desugarSigType (startPos sp) sigType
  validateSigWith (startPos sp) (docWith cmdDoc) argDocs
  mapM_ (rejectWithHere (startPos sp) "an argument-level docstring") argDocs
  let t' = quantifyType t
      doc = ArgDocSig cmdDoc (init argDocs) (last argDocs)
      (labels, t'') = extractLabels t'
      et = EType t'' (Set.fromList cs) doc labels
  e <- freshExprSpan sp (SigE (Signature name Nothing et))
  return [e]
desugarTopLevel (Loc sp (CAssE name params body whereDecls)) = do
  params' <- mapM exprToIrrefPat params
  checkWhereScope params' whereDecls
  captureDeclDocs (startPos sp) name
  body' <- desugarExpr body
  whereDecls' <- concatMapM desugarTopLevel whereDecls
  e <- case params' of
    [] -> freshExprSpan sp (AssE name body' whereDecls')
    ps -> do
      lam <- buildLamWithIrrefPats sp ps body'
      freshExprSpan sp (AssE name lam whereDecls')
  return [e]
desugarTopLevel (Loc sp (CGuardedAssE name params guards defaultExpr whereDecls)) = do
  params' <- mapM exprToIrrefPat params
  checkWhereScope params' whereDecls
  captureDeclDocs (startPos sp) name
  body' <- desugarGuards sp guards defaultExpr
  whereDecls' <- concatMapM desugarTopLevel whereDecls
  e <- case params' of
    [] -> freshExprSpan sp (AssE name body' whereDecls')
    ps -> do
      lam <- buildLamWithIrrefPats sp ps body'
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
desugarTopLevel (Loc sp (CEffE lbl esc)) = do
  e <- freshExprSpan sp (EffE lbl esc)
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

-- | Build a LamE from a list of irrefutable pattern parameters plus a
-- desugared body. Each pattern becomes a fresh formal, with projection
-- bindings wrapped around the body in a LetE. Unused pattern binders
-- are dropped -- see 'desugarIrrefLamParam' for why.
buildLamWithIrrefPats :: Span -> [Loc CstIrrefPat] -> ExprI -> D ExprI
buildLamWithIrrefPats sp ps body = do
  let bodyFvs = freeVarsE body
  paramResults <- mapM (desugarIrrefLamParam bodyFvs) ps
  let paramNames = map fst paramResults
      projBindings = concatMap snd paramResults
  wrapped <- case projBindings of
    [] -> return body
    bs -> freshExprSpan sp (LetE bs body)
  freshExprSpan sp (LamE paramNames wrapped)

-- Reject where-clause bindings that shadow a function parameter or
-- duplicate a sibling where-binding. Only inspects value bindings
-- (CAssE/CGuardedAssE); type signatures (CSigE) may legally repeat the
-- term name.
checkWhereScope :: [Loc CstIrrefPat] -> [Loc CstExpr] -> D ()
checkWhereScope params decls =
  go (Set.fromList (map fst (concatMap irrefPatBoundNames params))) Set.empty decls
  where
    go :: Set.Set Text -> Set.Set Text -> [Loc CstExpr] -> D ()
    go _ _ [] = return ()
    go ps seen (d : rest) = case bindingName d of
      Nothing -> go ps seen rest
      Just (n, pos)
        | Set.member n ps ->
            dfail pos ("where-clause binding shadows function parameter: " ++ T.unpack n)
        | Set.member n seen ->
            dfail pos ("duplicate binding in where-clause: " ++ T.unpack n)
        | otherwise -> go ps (Set.insert n seen) rest

    bindingName :: Loc CstExpr -> Maybe (Text, Pos)
    bindingName d@(Loc _ (CAssE (EV n) _ _ _)) = Just (n, startPos d)
    bindingName d@(Loc _ (CGuardedAssE (EV n) _ _ _ _)) = Just (n, startPos d)
    bindingName _ = Nothing

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

-- | True iff the alias body provides no information beyond a
-- self-reference, optionally wrapped in @?_@. See the call site in
-- @desugarTypeDef@ for the rationale on which shapes count as vacuous
-- and why @[X]@-guarded recursion does not.
isVacuousAlias :: TVar -> [Either (TVar, Kind) TypeU] -> TypeU -> Bool
isVacuousAlias v vs = go
  where
    paramTypes = map (either (VarU . fst) id) vs
    go (VarU v') = v == v'
    go (AppU (VarU v') ps) = v == v' && ps == paramTypes
    go (OptionalU t) = go t
    go _ = False

desugarTypeDef :: Span -> CstTypeDef -> D [ExprI]
desugarTypeDef sp (CstTypeAlias maybeLangTok (v, vs) (t, isTerminal)) = do
  -- Reject vacuous aliases: bodies that are nothing but the alias's own
  -- self-reference, optionally wrapped in @?_@. The two motivating cases
  -- are @type X = X@ (pure identity, no fixed point beyond X itself) and
  -- @type X = ?X@ (collapses to nothing under the @?(?T) == ?T@
  -- idempotence: every inhabitant is @null@). The parametric forms
  -- @type Foo a = Foo a@ and @type Foo a = ?(Foo a)@ are the same family.
  -- This check happens here -- not in @checkForSelfRecursion@ -- because
  -- by the time we reach Restructure the parser has already lowered
  -- forward declarations (@type Foo@) to the same @VarU Foo@ body, so
  -- the two cases are no longer distinguishable. The list-guarded form
  -- @type X = [X]@ is intentionally accepted: its inhabitants are
  -- nested empty lists, which is a legitimate (if niche) shape.
  when (isVacuousAlias v vs t) $
    dfail (startPos sp) $
      "Type alias '" ++ T.unpack (unTVar v) ++
      "' has a vacuous body: it reduces to a self-reference with no payload"
  lang <- case maybeLangTok of
    Nothing -> return Nothing
    Just tok -> do
      l <- parseLang tok
      return (Just (l, isTerminal))
  docs <- lookupDocsAt (startPos sp)
  docVars <- if null docs then return defaultValue else processArgDocLinesD (startPos sp) docs
  rejectWithHere (startPos sp) "a type alias" docVars
  e <- freshExprSpan sp (TypE (ExprTypeE lang v vs t (ArgDocAlias docVars) TypedefAlias))
  return [e]
desugarTypeDef sp (CstNewtype (v, vs) t) = do
  when (isVacuousAlias v vs t) $
    dfail (startPos sp) $
      "Newtype '" ++ T.unpack (unTVar v) ++
      "' has a vacuous body: it reduces to a self-reference with no payload"
  docs <- lookupDocsAt (startPos sp)
  docVars <- if null docs then return defaultValue else processArgDocLinesD (startPos sp) docs
  rejectWithHere (startPos sp) "a newtype declaration" docVars
  e <- freshExprSpan sp (TypE (ExprTypeE Nothing v vs t (ArgDocAlias docVars) TypedefNewtype))
  return [e]
desugarTypeDef sp (CstTypeAliasForward (v, vs)) = do
  -- A bare @type Foo@ (no RHS) is a built-in primitive declaration:
  -- the type is recognised by the compiler, has its own per-language
  -- forms in the root-* modules, and is opaque to reduction. The
  -- self-referential body is a structural placeholder so the alias
  -- table still has a uniform shape.
  let t = if null vs then VarU v else AppU (VarU v) (map (either (VarU . fst) id) vs)
  docs <- lookupDocsAt (startPos sp)
  docVars <- if null docs then return defaultValue else processArgDocLinesD (startPos sp) docs
  rejectWithHere (startPos sp) "a primitive type declaration" docVars
  e <- freshExprSpan sp (TypE (ExprTypeE Nothing v vs t (ArgDocAlias docVars) TypedefPrimitive))
  return [e]
desugarTypeDef sp (CstNamTypeWhere nt (v, vs) locEntries) = do
  -- A record / object / table declaration. These types are always
  -- nominal and own their per-language form; they behave structurally
  -- like newtypes (no typeclass inheritance, opaque to reduction). The
  -- general declaration goes into gscope as TypedefNewtype so it is
  -- exempt from Invariant 1 (a record may legally pair with a
  -- @record Cpp => Foo = "..."@ form). An explicit @type SpecialPerson
  -- = Person@ wrapped around a record is still TypedefAlias and would
  -- still trip Invariant 1 if also given a per-language form.
  recDocs <- lookupDocsAt (startPos sp)
  recDocVars <- processArgDocLinesD (startPos sp) recDocs
  rejectWithHere (startPos sp) "a record declaration" recDocVars
  fieldDocs <-
    mapM
      (\(loc, _, _) -> do
          let p = locPos loc
          dl <- lookupDocsAt p
          fieldDoc <- processArgDocLinesD p dl
          rejectWithHere p "a record field" fieldDoc
          return fieldDoc)
      locEntries
  let entries = [(k, ty) | (_, k, ty) <- locEntries]
      doc = ArgDocRec recDocVars (zip (map fst entries) fieldDocs)
      t = NamU nt v (map (either (VarU . fst) id) vs) entries
  e <- freshExprSpan sp (TypE (ExprTypeE Nothing v vs t doc TypedefNewtype))
  return [e]
desugarTypeDef sp (CstNamTypeLegacy maybeLangTok nt (v, vs) (conName, isTerminal, conArgs) entries) = do
  -- Legacy form covers both general @record Foo = Constructor ...@
  -- (lang=Nothing, behaves like CstNamTypeWhere -> TypedefNewtype) and
  -- per-language @record Cpp => Foo = "struct"@ (lang=Just, feeds
  -- cscope; reduction relies on TypedefAlias + isTerminal=True to
  -- terminate at the native form, so we keep TypedefAlias there).
  lang <- case maybeLangTok of
    Nothing -> return Nothing
    Just tok -> do
      l <- parseLang tok
      return (Just (l, isTerminal))
  let con = if T.null conName then v else TV conName
      -- If the user supplied explicit args after the constructor string
      -- (e.g. `"container_t<$1>" a`, parallel to the type-alias
      -- concrete_rhs syntax), use those as the body's positional args.
      -- Otherwise, default to the LHS params, which covers the common
      -- case where the macro indices map directly to the declared
      -- parameter order.
      bodyTs = if null conArgs
                 then map (either (VarU . fst) id) vs
                 else conArgs
      t = NamU nt con bodyTs entries
      doc = ArgDocRec defaultValue [(k, defaultValue) | (k, _) <- entries]
      kind = case maybeLangTok of
        Nothing -> TypedefNewtype  -- general record decl
        Just _  -> TypedefAlias     -- per-language native form (cscope)
  e <- freshExprSpan sp (TypE (ExprTypeE lang v vs t doc kind))
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
  mapM_ (rejectWithHere (Pos 0 0 "") "a class method signature") argDocs
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
  exprIs' <- if isImplicitMain
               then mkImplicitMain exprIs
               else return exprIs
  mapM injectTerminalActions exprIs'

--------------------------------------------------------------------
-- Terminal-action synthesis (`--' with:` docstring atom)
--
-- For each Signature `foo :: ... -> <E>? T` whose docstring preamble
-- carries `--' with: <flag>=<term>` atoms, synthesize an internal
-- top-level binding `mlcp_foo_<long> = \x .. -> <term> (foo x ..)`
-- (or a do-block if `foo` is effectful) and add it to the module's
-- export list. Downstream typechecking, effect merging, and pool
-- codegen handle the composed binding as an ordinary export.
--------------------------------------------------------------------

injectTerminalActions :: ExprI -> D ExprI
injectTerminalActions (ExprI i (ModE mv body)) = do
  body' <- expandWithBindings body
  return (ExprI i (ModE mv body'))
injectTerminalActions e = return e

expandWithBindings :: [ExprI] -> D [ExprI]
expandWithBindings body =
  case collectWithSpecs body of
    [] -> return body
    specs -> do
      let plan =
            [ (parent, w, mangleTerminalName parent (wsLong w), sigExprI)
            | (sigExprI, parent, _, ws) <- specs
            , w <- ws
            ]
      checkMangledCollisions body plan
      synthesized <- concat <$> mapM emitFor specs
      let mangleds = [ m | (_, _, m, _) <- plan ]
      body' <- mapM (addToExport mangleds) body
      return (body' ++ synthesized)

-- | Reject synthesized-name collisions before any AssE is emitted.
-- Two conditions:
--   1. Two `with:` declarations produce the same mangled name (e.g.
--      `foo_bar` + `--baz` collides with `foo` + `--bar-baz` after
--      hyphen -> underscore normalization).
--   2. A synthesized name matches an existing top-level user
--      identifier in the same module (any SigE or AssE binder).
-- Either would corrupt the module scope; fail with a caret on the
-- offending `--' with:` line's own signature so the user can locate it.
checkMangledCollisions ::
  [ExprI] ->
  [(EVar, WithSpec, EVar, ExprI)] ->
  D ()
checkMangledCollisions body plan = do
  case firstDuplicateBy (\(_, _, m, _) -> m) plan of
    Just ((parentA, specA, mangled, _), (parentB, specB, _, sigB)) -> do
      sp <- posOfExprI sigB
      dfail (startPos sp) . T.unpack $
        "two `--' with:` declarations produce the same synthesized "
        <> "internal name `" <> unEVar mangled <> "`: "
        <> unEVar parentA <> " " <> renderWithSpec specA
        <> " and " <> unEVar parentB <> " " <> renderWithSpec specB
        <> ". Rename one of the long flags so the mangled names differ."
    Nothing -> return ()
  let userNames = collectTopLevelBinders body
  case find (\(_, _, m, _) -> Set.member m userNames) plan of
    Just (parent, spec, mangled, sigI) -> do
      sp <- posOfExprI sigI
      dfail (startPos sp) . T.unpack $
        "synthesized internal name `" <> unEVar mangled
        <> "` (from " <> unEVar parent <> " " <> renderWithSpec spec
        <> ") collides with an existing top-level identifier in this "
        <> "module. Rename the long flag or the existing identifier."
    Nothing -> return ()

collectTopLevelBinders :: [ExprI] -> Set.Set EVar
collectTopLevelBinders = foldr pick Set.empty
  where
    pick (ExprI _ (SigE (Signature n _ _))) acc = Set.insert n acc
    pick (ExprI _ (AssE n _ _)) acc = Set.insert n acc
    pick _ acc = acc

-- | Return the first pair of entries that agree under the projection,
-- or Nothing if all projected keys are unique.
firstDuplicateBy :: Ord k => (a -> k) -> [a] -> Maybe (a, a)
firstDuplicateBy proj = go Map.empty
  where
    go _ [] = Nothing
    go seen (x : xs) =
      let k = proj x
       in case Map.lookup k seen of
            Just prior -> Just (prior, x)
            Nothing -> go (Map.insert k x seen) xs


collectWithSpecs :: [ExprI] -> [(ExprI, EVar, EType, [WithSpec])]
collectWithSpecs = foldr pick []
  where
    pick e@(ExprI _ (SigE (Signature name _ et))) rest =
      case edocs et of
        ArgDocSig cmdDoc _ _ ->
          case docWith cmdDoc of
            [] -> rest
            ws -> (e, name, et, ws) : rest
        _ -> rest
    pick _ rest = rest

emitFor :: (ExprI, EVar, EType, [WithSpec]) -> D [ExprI]
emitFor (sigExprI, parentName, parentEt, specs) = do
  sp <- posOfExprI sigExprI
  let pt = etype parentEt
      arity = sigArity pt
      isEff = returnIsEffectful pt
  mapM (synthWithBinding sp parentName arity isEff) specs

synthWithBinding :: Span -> EVar -> Int -> Bool -> WithSpec -> D ExprI
synthWithBinding sp parentName arity isEff (WithSpec _ long tTerm) = do
  let mangled = mangleTerminalName parentName long
      lamVars = [EV ("mlcp_x_" <> T.pack (show idx)) | idx <- [1..arity]]
  fooRef <- freshExprSpan sp (VarE defaultValue parentName)
  argRefs <- mapM (\v -> freshExprSpan sp (VarE defaultValue v)) lamVars
  fooApp <-
    if arity == 0
      then return fooRef
      else freshExprSpan sp (AppE fooRef argRefs)
  tRef <- freshExprSpan sp (VarE defaultValue tTerm)
  body <-
    if isEff
      then do
        let z = EV "mlcp_z"
        forceE <- freshExprSpan sp (EvalE fooApp)
        zRef <- freshExprSpan sp (VarE defaultValue z)
        tApp <- freshExprSpan sp (AppE tRef [zRef])
        letE <- freshExprSpan sp (LetE [(z, forceE)] tApp)
        freshExprSpan sp (DoBlockE letE)
      else freshExprSpan sp (AppE tRef [fooApp])
  wrapped <-
    if arity == 0
      then return body
      else freshExprSpan sp (LamE lamVars body)
  freshExprSpan sp (AssE mangled wrapped [])

-- | Add the given mangled symbols to a module's ExportMany. Fresh IDs
-- are allocated per symbol. ExportAll needs no update (it already
-- covers every top-level definition).
addToExport :: [EVar] -> ExprI -> D ExprI
addToExport mangleds (ExprI i (ExpE (ExportMany syms groups))) = do
  newItems <- mapM
    (\ev -> do fid <- freshIdPos (Pos 0 0 ""); return (fid, TermSymbol ev))
    mangleds
  let syms' = Set.union syms (Set.fromList newItems)
  return (ExprI i (ExpE (ExportMany syms' groups)))
addToExport _ e = return e

-- | Return position's effect: True iff the outermost result (after
-- stripping ForallU quantifiers) is an EffectU or the function's
-- return position is EffectU.
returnIsEffectful :: TypeU -> Bool
returnIsEffectful (ForallU _ t) = returnIsEffectful t
returnIsEffectful (FunU _ ret) = isEffectU ret
returnIsEffectful t = isEffectU t

isEffectU :: TypeU -> Bool
isEffectU (EffectU _ _) = True
isEffectU _ = False

sigArity :: TypeU -> Int
sigArity (ForallU _ t) = sigArity t
sigArity (FunU args _) = length args
sigArity _ = 0

--------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------

-- concatMapM is imported from Morloc.Internal via Morloc.Namespace.Prim
