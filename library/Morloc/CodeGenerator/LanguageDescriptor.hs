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
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml as Y
import GHC.Generics (Generic)

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
  , -- Literals
    ldBoolTrue :: !Text
  , ldBoolFalse :: !Text
  , ldNullLiteral :: !Text
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
  , -- Foreign call template
    ldForeignCallFn :: !Text -- "morloc.foreign_call" or "morloc_foreign_call"
  , ldForeignCallIntSuffix :: !Text -- "L" for R, "" for others
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
  , -- External codegen (optional)
    ldCodegenCommand :: !(Maybe Text) -- e.g. "morloc-codegen-generic"
  , -- == Template fields (Layer 1 & 2) ==

    -- Assignment
    ldAssignOp :: !Text -- "=" or "<-"
  , -- Lambda
    ldLambdaTemplate :: !Text -- e.g. "lambda {{args}}: {{body}}"
  , -- Suspend (thunk)
    ldSuspendExpr :: !Text -- e.g. "(lambda: {{expr}})"
  , ldSuspendBlock :: !Text -- e.g. "function(){\n{{body}}\n}" or "" for pass-through
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
  , -- List constructor support
    ldAtomicTypes :: ![Text] -- For TypeDependentList: ["integer", "numeric", ...]
  , ldAtomicListFn :: !Text -- For TypeDependentList: "c"
  , ldGenericListFn :: !Text -- For FunctionCallList/TypeDependentList: "list"
  , -- Map iteration style (kept as enum - needs different code structure)
    ldMapStyle :: !MapStyle
  , -- Dispatch table templates
    ldDispatchLocalHeader :: !Text -- e.g. "dispatch = {"
  , ldDispatchLocalEntry :: !Text -- e.g. "    {{mid}}: {{name}},"
  , ldDispatchLocalFooter :: !Text -- e.g. "}"
  , ldDispatchRemoteHeader :: !Text -- e.g. "remote_dispatch = {"
  , ldDispatchRemoteEntry :: !Text -- e.g. "    {{mid}}: {{name}}_remote,"
  , ldDispatchRemoteFooter :: !Text -- e.g. "}"
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
        -- Map registry metadata fields to descriptor fields if not already present
        nameVal = KM.lookup (AesonKey.fromText "name") obj
        extVal = KM.lookup (AesonKey.fromText "extension") obj
        runCmdVal = KM.lookup (AesonKey.fromText "run_command") obj
        isCompiledVal = KM.lookup (AesonKey.fromText "is_compiled") obj
        -- ins keeps old value if key exists, so insert specific overrides first
        withDefaults =
          maybe id (ins "ldName") nameVal
            . maybe id (ins "ldExtension") extVal
            . maybe id (ins "ldRunCommand") runCmdVal
            . maybe id (ins "ldIsCompiled") isCompiledVal
            . ins "ldCodegenCommand" Y.Null
            . ins "ldRemoteCallFn" (Y.String "")
            . ins "ldDictStyleRecords" (Y.Bool False)
            . ins "ldQuoteRecordKeys" (Y.Bool True)
            . ins "ldQualifiedImports" (Y.Bool False)
            . ins "ldRunCommand" (Y.Array mempty)
            . ins "ldIsCompiled" (Y.Bool False)
            -- Template field defaults
            . ins "ldAssignOp" (Y.String "=")
            . ins "ldLambdaTemplate" (Y.String "({{args}}) -> {{body}}")
            . ins "ldSuspendExpr" (Y.String "(() -> {{expr}})")
            . ins "ldSuspendBlock" (Y.String "")
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
            . ins "ldAtomicTypes" (Y.Array mempty)
            . ins "ldAtomicListFn" (Y.String "")
            . ins "ldGenericListFn" (Y.String "list")
            . ins "ldMapStyle" (Y.String "loop_append")
            . ins "ldDispatchLocalHeader" (Y.String "")
            . ins "ldDispatchLocalEntry" (Y.String "")
            . ins "ldDispatchLocalFooter" (Y.String "")
            . ins "ldDispatchRemoteHeader" (Y.String "")
            . ins "ldDispatchRemoteEntry" (Y.String "")
            . ins "ldDispatchRemoteFooter" (Y.String "")
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
    , ldBoolTrue = "True"
    , ldBoolFalse = "False"
    , ldNullLiteral = "None"
    , ldListStyle = BracketList
    , ldTupleConstructor = ""
    , ldRecordConstructor = "dict"
    , ldRecordSeparator = "="
    , ldIndexStyle = ZeroBracket
    , ldKeyAccess = "bracket"
    , ldFieldAccess = DotAccess
    , ldSerializeFn = "morloc.put_value"
    , ldDeserializeFn = "morloc.get_value"
    , ldForeignCallFn = "morloc.foreign_call"
    , ldForeignCallIntSuffix = ""
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
    , ldCodegenCommand = Nothing
    , -- Template fields
      ldAssignOp = "="
    , ldLambdaTemplate = "({{args}}) -> {{body}}"
    , ldSuspendExpr = "(() -> {{expr}})"
    , ldSuspendBlock = ""
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
    , ldAtomicTypes = []
    , ldAtomicListFn = ""
    , ldGenericListFn = "list"
    , ldMapStyle = LoopAppend
    , ldDispatchLocalHeader = ""
    , ldDispatchLocalEntry = ""
    , ldDispatchLocalFooter = ""
    , ldDispatchRemoteHeader = ""
    , ldDispatchRemoteEntry = ""
    , ldDispatchRemoteFooter = ""
    }
