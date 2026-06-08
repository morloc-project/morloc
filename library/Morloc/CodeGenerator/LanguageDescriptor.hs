{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.CodeGenerator.LanguageDescriptor
Description : Language descriptor for generic code generation
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Defines a descriptor format that captures the syntactic differences between
dynamically-typed interpreted languages. Used by the generic translator to
generate pool code without language-specific Haskell modules.
-}
module Morloc.CodeGenerator.LanguageDescriptor
  ( LangDescriptor (..)
  , IndexStyle (..)
  , FieldAccessStyle (..)
  , BlockStyle (..)
  , MapStyle (..)
  , ListStyle (..)
  , PatternStyle (..)
  , loadLangDescriptor
  , loadLangDescriptorFromText
  , defaultLangDescriptor
  , matchNamePattern
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml as Y
import GHC.Generics (Generic)
import qualified Text.Parsec as P

-- | How to access tuple/list elements by index
data IndexStyle
  = ZeroBracket -- e[i]     (Python, JS, Julia, Lua)
  | OneBracket -- e[i+1]   (R-like with [], 1-indexed)
  | OneDoubleBracket -- e[[i+1]] (R with [[]], 1-indexed)
  deriving (Eq, Show, Generic)

-- | How to access record fields
data FieldAccessStyle
  = DotAccess -- e.field  (Python, JS, Julia)
  | DollarAccess -- e$field  (R)
  deriving (Eq, Show, Generic)

-- | Block structure for function definitions
data BlockStyle
  = IndentBlock -- Python-style: header + indented body
  | BraceBlock -- R/JS-style: header { body }
  | EndKeywordBlock -- Julia/Ruby-style: header body end
  deriving (Eq, Show, Generic)

-- | List iteration / map style
data MapStyle
  = LoopAppend -- for x in col: result.append(...)
  | ApplyCallback -- lapply(col, function(x) ...)
  | ListComprehension -- [body for x in col]
  deriving (Eq, Show, Generic)

-- | List constructor style
data ListStyle
  = BracketList -- [a, b, c]
  | FunctionCallList -- list(a, b, c)
  | TypeDependentList -- c(a,b) for atomic types, list(a,b) for complex
  deriving (Eq, Show, Generic)

-- | String interpolation / pattern style
data PatternStyle
  = FStringPattern -- f"prefix{var}suffix"
  | ConcatCall -- paste0("prefix", var, "suffix") or string(...)
  deriving (Eq, Show, Generic)

-- | Complete language descriptor
data LangDescriptor = LangDescriptor
  { -- Identity
    ldName :: !Text
  , ldExtension :: !String
  , -- Anchored regex (subset) a sourced *identifier* must match in this
    -- language; emitted verbatim. Empty = no check.
    ldNamePattern :: !Text
  , -- Anchored regex (subset) a sourced *operator* symbol must match
    -- (e.g. "+", "//"); operator vs identifier is decided by the symbol,
    -- so "foo" and "+" are valid but "foo+" matches neither. Empty = no
    -- check.
    ldOperatorPattern :: !Text
  , -- Literals
    ldBoolTrue :: !Text
  , ldBoolFalse :: !Text
  , ldNullLiteral :: !Text
  , -- Predicate template for "the expression is not null", with {{expr}}
    -- placeholder. Used by IIfNotNull when lifting an inner transformation
    -- through an Optional. Examples: "{{expr}} is not None" (Python),
    -- "!is.null({{expr}})" (R). C++ handles its own form in CppPrinter.
    ldNullCheckTemplate :: !Text
  , -- Non-finite Real literals: idiomatic source-level expressions for
    -- +Inf, -Inf, and NaN in the target language. Used by IRealLit
    -- printing when the literal payload is non-finite.
    ldRealPosInf :: !Text
  , ldRealNegInf :: !Text
  , ldRealNaN :: !Text
  , -- Constructors
    ldListStyle :: !ListStyle
  , ldTupleConstructor :: !Text -- "tuple" or "list" or ""
  , ldRecordConstructor :: !Text -- "dict" or "OrderedDict" or "list"
  , ldRecordSeparator :: !Text -- "=" for Python/R, "=>" for Julia
  , -- Access styles
    ldIndexStyle :: !IndexStyle
  , ldKeyAccess :: !Text -- "bracket" -> e["k"], "double_bracket" -> e[["k"]]
  , ldFieldAccess :: !FieldAccessStyle
  , -- Serialize/deserialize function names
    ldSerializeFn :: !Text -- "morloc.put_value" or "morloc_put_value"
  , ldDeserializeFn :: !Text -- "morloc.get_value" or "morloc_get_value"
  , ldReleasePacketFn :: !Text -- "morloc.release_packet_shm" or equivalent
  , -- Intrinsic function prefix (for mlc_show, mlc_hash, etc.)
    ldIntrinsicPrefix :: !Text -- "morloc." or "morloc_" or "MorlocRuntime."
  , -- Foreign call template
    ldForeignCallFn :: !Text -- "morloc.foreign_call" or "morloc_foreign_call"
  , ldForeignCallIntSuffix :: !Text -- "L" for R, "" for others
  , ldIntLiteralSuffix :: !Text -- "L" for R, "" for others
  , -- Remote call
    ldRemoteCallFn :: !Text -- "morloc.remote_call" or "morloc_remote_call"
  , -- Record access
    ldDictStyleRecords :: !Bool -- True: NamRecord/dict use bracket access, others use dot (Python)
  , ldQuoteRecordKeys :: !Bool -- True: "k" => v (Julia), False: k=v (Python, R)
  , -- Import syntax
    ldQualifiedImports :: !Bool -- True: qualify source names with module path (Python)
  , ldIncludeRelToFile :: !Bool -- True if include() resolves relative to file (Julia), False for CWD (R)
  , -- Pool template
    ldPoolTemplate :: !Text -- pool template content
  , ldBreakMarker :: !Text -- "# <<<BREAK>>>"
  , ldCommentMarker :: !Text -- "#" or "//" or "--"
  , -- Execution
    ldRunCommand :: ![Text] -- command to run pool, e.g. ["python3"]
  , ldIsCompiled :: !Bool
  , -- NUL-in-Str policy. False means the morloc runtime rejects any
    -- attempt to send a Str containing an interior NUL byte across the
    -- boundary into a pool of this language. Languages whose native
    -- string type is length-prefixed (Python, C++, Julia) set this to
    -- True; languages where strings are NUL-terminated by convention
    -- (C) or whose stdlib refuses NUL strings (R) set it to False.
    ldAllowStringNull :: !Bool
  , -- External codegen (optional)
    ldCodegenCommand :: !(Maybe Text) -- e.g. "morloc-codegen-generic"
  , -- == Template fields (Layer 1 & 2) ==

    -- Assignment
    ldAssignOp :: !Text -- "=" or "<-"
  , -- Lambda
    ldLambdaTemplate :: !Text -- e.g. "lambda {{args}}: {{body}}"
  , -- Do-block (effect wrapper)
    ldDoBlockExpr :: !Text -- e.g. "(lambda: {{expr}})"
  , ldDoBlockBlock :: !Text -- e.g. "function(){\n{{body}}\n}" or "" for pass-through
  , -- Partial application
    ldPartialTemplate :: !Text -- e.g. "functools.partial({{fn_with_context}})"
  , -- Import
    ldImportTemplate :: !Text -- e.g. "{{namespace}} = importlib.import_module(\"{{module_path}}\")"
  , -- Socket path
    ldSocketPathTemplate :: !Text -- e.g. "os.path.join(global_state[\"tmpdir\"], {{socket}})"
  , -- Resource packing for remote calls
    ldResourcePackTemplate :: !Text -- e.g. "struct.pack('iiii', {{mem}}, {{time}}, {{cpus}}, {{gpus}})"
  , -- Return statement
    ldReturnTemplate :: !Text -- e.g. "return({{expr}})"
  , -- Function definition
    ldFuncDefHeader :: !Text -- e.g. "def {{name}}({{args}}):"
  , ldBlockStyle :: !BlockStyle
  , ldBlockEnd :: !Text -- "" or "end"
  , -- Error wrapping
    ldErrorWrapOpen :: !Text -- "try:" for Python, "" for others
  , ldErrorWrapClose :: ![Text] -- Except block lines with {{name}} template var
  , -- Pattern/string interpolation support
    ldPatternStyle :: !PatternStyle
  , ldConcatFn :: !Text -- For ConcatCall: "paste0", "string"
  , ldQuoteTerminator :: !Text -- String delimiter: "\"" for R/C++, "\"\"\"" for Python
  , ldQuoteTerminatorEsc :: !Text -- Escaped form: "\\\"" for R/C++, "\\\"\\\"\\\"" for Python
  , -- List constructor support
    ldAtomicTypes :: ![Text] -- For TypeDependentList: ["integer", "numeric", ...]
  , ldAtomicListFn :: !Text -- For TypeDependentList: "c"
  , ldGenericListFn :: !Text -- For FunctionCallList/TypeDependentList: "list"
  , -- Map iteration style (kept as enum - needs different code structure)
    ldMapStyle :: !MapStyle
  , -- String literal type map: concrete type -> literal prefix
    -- e.g. {"bytes": "b"} for Python (b"hello"), empty for C++/R
    ldStrLiteralMap :: !(Map.Map Text Text)
  , -- Dispatch table templates
    ldDispatchLocalHeader :: !Text -- e.g. "dispatch = {"
  , ldDispatchLocalEntry :: !Text -- e.g. "    {{mid}}: {{name}},"
  , ldDispatchLocalFooter :: !Text -- e.g. "}"
  , ldDispatchRemoteHeader :: !Text -- e.g. "remote_dispatch = {"
  , ldDispatchRemoteEntry :: !Text -- e.g. "    {{mid}}: {{name}},"
  , ldDispatchRemoteFooter :: !Text -- e.g. "}"
  , ldLogWrap :: !Text
    -- ^ Wrap a dispatch entry's callable expression for per-label logging.
    -- Substitutions: {{label}} (the label string literal value) and {{inner}}
    -- (the unwrapped callable expression, including any "_remote" suffix).
    -- Empty disables wrapping. Example for Python:
    -- @"__mlc_log(\"{{label}}\", {{inner}})"@.
  }
  deriving (Eq, Show, Generic)

-- YAML instances

instance Y.FromJSON IndexStyle where
  parseJSON = Y.withText "IndexStyle" $ \t -> case t of
    "zero_bracket" -> pure ZeroBracket
    "one_bracket" -> pure OneBracket
    "one_double_bracket" -> pure OneDoubleBracket
    _ -> fail $ "Unknown IndexStyle: " <> T.unpack t

instance Y.FromJSON FieldAccessStyle where
  parseJSON = Y.withText "FieldAccessStyle" $ \t -> case t of
    "dot" -> pure DotAccess
    "dollar" -> pure DollarAccess
    _ -> fail $ "Unknown FieldAccessStyle: " <> T.unpack t

instance Y.FromJSON BlockStyle where
  parseJSON = Y.withText "BlockStyle" $ \t -> case t of
    "indent" -> pure IndentBlock
    "braces" -> pure BraceBlock
    "end_keyword" -> pure EndKeywordBlock
    _ -> fail $ "Unknown BlockStyle: " <> T.unpack t

instance Y.FromJSON MapStyle where
  parseJSON = Y.withText "MapStyle" $ \t -> case t of
    "loop_append" -> pure LoopAppend
    "apply_callback" -> pure ApplyCallback
    "list_comprehension" -> pure ListComprehension
    _ -> fail $ "Unknown MapStyle: " <> T.unpack t

instance Y.FromJSON ListStyle where
  parseJSON = Y.withText "ListStyle" $ \t -> case t of
    "bracket" -> pure BracketList
    "function_call" -> pure FunctionCallList
    "type_dependent" -> pure TypeDependentList
    _ -> fail $ "Unknown ListStyle: " <> T.unpack t

instance Y.FromJSON PatternStyle where
  parseJSON = Y.withText "PatternStyle" $ \t -> case t of
    "fstring" -> pure FStringPattern
    "concat_call" -> pure ConcatCall
    _ -> fail $ "Unknown PatternStyle: " <> T.unpack t

-- | Custom FromJSON that injects defaults for optional fields.
instance Y.FromJSON LangDescriptor where
  parseJSON = Y.withObject "LangDescriptor" $ \obj -> do
    let ins k v = KM.insertWith (\_ old -> old) (AesonKey.fromText k) v
        -- Unconditional insert: always overwrites. Use for remapping
        -- a yaml field name (e.g. "allow_string_null") to its Haskell
        -- field name ("ldAllowStringNull"). Must run BEFORE the
        -- defaulting `ins` for the same key so the user-supplied value
        -- wins.
        setK k v = KM.insert (AesonKey.fromText k) v
        -- Map registry metadata fields to descriptor fields if not already present
        nameVal = KM.lookup (AesonKey.fromText "name") obj
        extVal = KM.lookup (AesonKey.fromText "extension") obj
        runCmdVal = KM.lookup (AesonKey.fromText "run_command") obj
        isCompiledVal = KM.lookup (AesonKey.fromText "is_compiled") obj
        allowNullVal = KM.lookup (AesonKey.fromText "allow_string_null") obj
        -- ins keeps old value if key exists, so insert specific overrides first
        withDefaults =
          maybe id (ins "ldName") nameVal
            . maybe id (ins "ldExtension") extVal
            . maybe id (ins "ldRunCommand") runCmdVal
            . maybe id (ins "ldIsCompiled") isCompiledVal
            . maybe id (setK "ldAllowStringNull") allowNullVal
            . ins "ldAllowStringNull" (Y.Bool True)
            . ins "ldCodegenCommand" Y.Null
            . ins "ldRealPosInf" (Y.String "")
            . ins "ldRealNegInf" (Y.String "")
            . ins "ldRealNaN" (Y.String "")
            . ins "ldIntLiteralSuffix" (Y.String "")
            . ins "ldIntrinsicPrefix" (Y.String "")
            . ins "ldRemoteCallFn" (Y.String "")
            . ins "ldReleasePacketFn" (Y.String "morloc.release_packet_shm")
            . ins "ldDictStyleRecords" (Y.Bool False)
            . ins "ldQuoteRecordKeys" (Y.Bool True)
            . ins "ldQualifiedImports" (Y.Bool False)
            . ins "ldRunCommand" (Y.Array mempty)
            . ins "ldIsCompiled" (Y.Bool False)
            -- Template field defaults
            . ins "ldNamePattern" (Y.String "")
            . ins "ldOperatorPattern" (Y.String "")
            . ins "ldAssignOp" (Y.String "=")
            . ins "ldLambdaTemplate" (Y.String "({{args}}) -> {{body}}")
            . ins "ldDoBlockExpr" (Y.String "(() -> {{expr}})")
            . ins "ldDoBlockBlock" (Y.String "")
            . ins "ldPartialTemplate" (Y.String "({{bound_args}}) -> {{fn}}({{all_args}})")
            . ins "ldImportTemplate" (Y.String "")
            . ins "ldSocketPathTemplate" (Y.String "")
            . ins "ldResourcePackTemplate" (Y.String "[{{mem}}, {{time}}, {{cpus}}, {{gpus}}]")
            . ins "ldReturnTemplate" (Y.String "return({{expr}})")
            . ins "ldFuncDefHeader" (Y.String "")
            . ins "ldBlockStyle" (Y.String "indent")
            . ins "ldBlockEnd" (Y.String "")
            . ins "ldErrorWrapOpen" (Y.String "")
            . ins "ldErrorWrapClose" (Y.Array mempty)
            . ins "ldPatternStyle" (Y.String "fstring")
            . ins "ldConcatFn" (Y.String "")
            . ins "ldQuoteTerminator" (Y.String "\"")
            . ins "ldQuoteTerminatorEsc" (Y.String "\\\"")
            . ins "ldAtomicTypes" (Y.Array mempty)
            . ins "ldAtomicListFn" (Y.String "")
            . ins "ldGenericListFn" (Y.String "list")
            . ins "ldStrLiteralMap" (Y.Object mempty)
            . ins "ldMapStyle" (Y.String "loop_append")
            . ins "ldDispatchLocalHeader" (Y.String "")
            . ins "ldDispatchLocalEntry" (Y.String "")
            . ins "ldDispatchLocalFooter" (Y.String "")
            . ins "ldDispatchRemoteHeader" (Y.String "")
            . ins "ldDispatchRemoteEntry" (Y.String "")
            . ins "ldDispatchRemoteFooter" (Y.String "")
            . ins "ldLogWrap" (Y.String "")
            $ obj
    Aeson.genericParseJSON Aeson.defaultOptions (Y.Object withDefaults)

-- | Load a language descriptor from a YAML file
loadLangDescriptor :: FilePath -> IO (Either String LangDescriptor)
loadLangDescriptor path = do
  result <- Y.decodeFileEither path
  return $ case result of
    Left err -> Left $ Y.prettyPrintParseException err
    Right desc -> Right desc

-- | Load a language descriptor from YAML text
loadLangDescriptorFromText :: Text -> Either String LangDescriptor
loadLangDescriptorFromText content =
  case Y.decodeEither' (TE.encodeUtf8 content) of
    Left err -> Left $ Y.prettyPrintParseException err
    Right desc -> Right desc

-- | Default descriptor
defaultLangDescriptor :: Text -> String -> LangDescriptor
defaultLangDescriptor name ext =
  LangDescriptor
    { ldName = name
    , ldExtension = ext
    , ldNamePattern = ""
    , ldOperatorPattern = ""
    , ldBoolTrue = "True"
    , ldBoolFalse = "False"
    , ldNullLiteral = "None"
    , ldNullCheckTemplate = "{{expr}} is not None"
    , ldRealPosInf = "float('inf')"
    , ldRealNegInf = "float('-inf')"
    , ldRealNaN = "float('nan')"
    , ldListStyle = BracketList
    , ldTupleConstructor = ""
    , ldRecordConstructor = "dict"
    , ldRecordSeparator = "="
    , ldIndexStyle = ZeroBracket
    , ldKeyAccess = "bracket"
    , ldFieldAccess = DotAccess
    , ldSerializeFn = "morloc.put_value"
    , ldDeserializeFn = "morloc.get_value"
    , ldReleasePacketFn = "morloc.release_packet_shm"
    , ldIntrinsicPrefix = ""
    , ldForeignCallFn = "morloc.foreign_call"
    , ldForeignCallIntSuffix = ""
    , ldIntLiteralSuffix = ""
    , ldRemoteCallFn = ""
    , ldDictStyleRecords = False
    , ldQuoteRecordKeys = True
    , ldQualifiedImports = False
    , ldIncludeRelToFile = False
    , ldPoolTemplate = ""
    , ldBreakMarker = "# <<<BREAK>>>"
    , ldCommentMarker = "#"
    , ldRunCommand = []
    , ldIsCompiled = False
    , ldAllowStringNull = True
    , ldCodegenCommand = Nothing
    , -- Template fields
      ldAssignOp = "="
    , ldLambdaTemplate = "({{args}}) -> {{body}}"
    , ldDoBlockExpr = "(() -> {{expr}})"
    , ldDoBlockBlock = ""
    , ldPartialTemplate = "({{bound_args}}) -> {{fn}}({{all_args}})"
    , ldImportTemplate = ""
    , ldSocketPathTemplate = ""
    , ldResourcePackTemplate = "[{{mem}}, {{time}}, {{cpus}}, {{gpus}}]"
    , ldReturnTemplate = "return({{expr}})"
    , ldFuncDefHeader = ""
    , ldBlockStyle = IndentBlock
    , ldBlockEnd = ""
    , ldErrorWrapOpen = ""
    , ldErrorWrapClose = []
    , ldPatternStyle = FStringPattern
    , ldConcatFn = ""
    , ldQuoteTerminator = "\""
    , ldQuoteTerminatorEsc = "\\\""
    , ldAtomicTypes = []
    , ldAtomicListFn = ""
    , ldGenericListFn = "list"
    , ldStrLiteralMap = Map.empty
    , ldMapStyle = LoopAppend
    , ldDispatchLocalHeader = ""
    , ldDispatchLocalEntry = ""
    , ldDispatchLocalFooter = ""
    , ldDispatchRemoteHeader = ""
    , ldDispatchRemoteEntry = ""
    , ldDispatchRemoteFooter = ""
    , ldLogWrap = ""
    }

-- Anchored regex-subset matcher. The pattern lives entirely in the
-- language descriptor (lang.yaml); no language-specific identifier rule
-- is hardcoded here. Supported subset: literal characters, '\'-escape,
-- '.' (any char), character classes '[...]' / '[^...]' with 'a-z'
-- ranges, and the postfix quantifiers '*', '+', '?'. The whole input
-- must match (implicitly anchored at both ends). A pattern that fails
-- to parse is treated as "no constraint" (fail open) so a malformed
-- descriptor never wrongly rejects a user's program.
data RNode
  = RChar Char
  | RAny
  | RClass Bool [(Char, Char)]
  | RStar RNode
  | RPlus RNode
  | ROpt RNode

matchNamePattern :: Text -> Text -> Bool
matchNamePattern pat input =
  case P.parse (pRegex <* P.eof) "name-pattern" (T.unpack pat) of
    Left _ -> True
    Right ns -> any null (matchNodes ns (T.unpack input))
  where
    pRegex :: P.Parsec String () [RNode]
    pRegex = P.many pQuant

    pQuant :: P.Parsec String () RNode
    pQuant = do
      a <- pAtom
      P.option
        a
        ( P.choice
            [ RStar a <$ P.char '*'
            , RPlus a <$ P.char '+'
            , ROpt a <$ P.char '?'
            ]
        )

    pAtom :: P.Parsec String () RNode
    pAtom =
      P.choice
        [ P.char '\\' >> RChar <$> P.anyChar
        , RAny <$ P.char '.'
        , pClass
        , RChar <$> P.noneOf "\\.[]*+?"
        ]

    pClass :: P.Parsec String () RNode
    pClass = do
      _ <- P.char '['
      neg <- P.option False (True <$ P.char '^')
      rs <- P.many1 pRange
      _ <- P.char ']'
      return (RClass neg rs)

    pRange :: P.Parsec String () (Char, Char)
    pRange = do
      lo <- P.noneOf "]"
      P.option
        (lo, lo)
        (P.try (P.char '-' >> P.noneOf "]" >>= \hi -> return (lo, hi)))

-- Match a node list against a string, returning every possible
-- unconsumed suffix; the overall match succeeds iff some suffix is
-- empty. Atoms always consume exactly one character, so the RStar
-- recursion strictly shrinks the input and terminates.
matchNodes :: [RNode] -> String -> [String]
matchNodes [] s = [s]
matchNodes (RStar n : rest) s =
  matchNodes rest s ++ [r | s' <- stepAtom n s, r <- matchNodes (RStar n : rest) s']
matchNodes (RPlus n : rest) s =
  [r | s' <- stepAtom n s, r <- matchNodes (RStar n : rest) s']
matchNodes (ROpt n : rest) s =
  matchNodes rest s ++ [r | s' <- stepAtom n s, r <- matchNodes rest s']
matchNodes (n : rest) s =
  [r | s' <- stepAtom n s, r <- matchNodes rest s']

-- Consume exactly one input character with a non-quantified atom.
stepAtom :: RNode -> String -> [String]
stepAtom _ [] = []
stepAtom (RChar c) (x : xs) = [xs | x == c]
stepAtom RAny (_ : xs) = [xs]
stepAtom (RClass neg ranges) (x : xs) =
  [xs | any (\(a, b) -> a <= x && x <= b) ranges /= neg]
stepAtom RStar {} _ = []
stepAtom RPlus {} _ = []
stepAtom ROpt {} _ = []
