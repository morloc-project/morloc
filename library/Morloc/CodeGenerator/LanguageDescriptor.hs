{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

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
  ( LangDescriptor(..)
  , IndexStyle(..)
  , FieldAccessStyle(..)
  , AssignmentStyle(..)
  , FunctionDefStyle(..)
  , LambdaStyle(..)
  , PartialAppStyle(..)
  , DispatchTableStyle(..)
  , MapListStyle(..)
  , SuspendStyle(..)
  , PatternInterpStyle(..)
  , ListConstructorStyle(..)
  , ResourcePackStyle(..)
  , loadLangDescriptor
  , defaultLangDescriptor
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml as Y
import GHC.Generics (Generic)

-- | How to access tuple/list elements by index
data IndexStyle
  = ZeroBracket       -- e[i]     (Python, JS, Julia, Lua)
  | OneBracket        -- e[i+1]   (R-like with [], 1-indexed)
  | OneDoubleBracket  -- e[[i+1]] (R with [[]], 1-indexed)
  deriving (Eq, Show, Generic)

-- | How to access record fields
data FieldAccessStyle
  = DotAccess    -- e.field  (Python, JS, Julia)
  | DollarAccess -- e$field  (R)
  deriving (Eq, Show, Generic)

-- | Assignment operator
data AssignmentStyle
  = EqualsAssign  -- x = e
  | ArrowAssign   -- x <- e
  deriving (Eq, Show, Generic)

-- | Function definition syntax
data FunctionDefStyle
  = PythonDef     -- def f(args): body
  | RAssignDef    -- f <- function(args) { body }
  | EndBlockDef   -- function f(args) body end (Julia, Lua, Ruby)
  deriving (Eq, Show, Generic)

-- | Lambda expression syntax
data LambdaStyle
  = PythonLambda  -- lambda args: body
  | RFunction     -- function(args) { body }
  | ArrowLambda   -- (args) -> body  (Julia, JS)
  deriving (Eq, Show, Generic)

-- | Partial application style
data PartialAppStyle
  = FunctoolsPartial  -- functools.partial(f, ctx_args) (Python)
  | AnonymousWrapper  -- function(bound) { f(ctx, bound) } (R)
  | ArrowWrapper      -- (bound...) -> f(ctx..., bound...) (Julia, JS)
  deriving (Eq, Show, Generic)

-- | Dispatch table construction
data DispatchTableStyle
  = PythonDictDispatch  -- dispatch = { mid: fn, ... }
  | RListDispatch       -- .dispatch <- list(); .dispatch[[i]] <- fn
  | ArrowDictDispatch   -- dispatch = Dict(mid => fn, ...) (Julia)
  deriving (Eq, Show, Generic)

-- | List iteration / map style
data MapListStyle
  = PythonForAppend  -- for x in col: result.append(...)
  | RLapply          -- lapply(col, function(x) ...)
  | Comprehension    -- [body for x in col] (Julia, Python alternative)
  deriving (Eq, Show, Generic)

-- | Suspend (thunk) style
data SuspendStyle
  = PythonSuspend  -- lambda: expr
  | RSuspend       -- function() expr
  | ArrowSuspend   -- () -> expr
  deriving (Eq, Show, Generic)

-- | String interpolation / pattern style
data PatternInterpStyle
  = PythonFString     -- f"prefix{var}suffix"
  | RPaste0           -- paste0("prefix", var, "suffix")
  | DollarInterp      -- "prefix${var}suffix" or "prefix$var suffix"
  deriving (Eq, Show, Generic)

-- | List constructor style
data ListConstructorStyle
  = BracketList    -- [a, b, c]
  | FunctionList   -- list(a, b, c)
  | RAtomicList    -- c(a,b) for atomic, list(a,b) for complex
  deriving (Eq, Show, Generic)

-- | How to pack remote call resources
data ResourcePackStyle
  = StructPackResources   -- struct.pack('iiii', m, t, c, g) (Python)
  | NamedListResources    -- list(mem=m, time=t, cpus=c, gpus=g) (R)
  | PlainListResources    -- [m, t, c, g] (default)
  deriving (Eq, Show, Generic)

-- | Complete language descriptor
data LangDescriptor = LangDescriptor
  { -- Identity
    ldName :: !Text
  , ldExtension :: !String

    -- Literals
  , ldBoolTrue :: !Text
  , ldBoolFalse :: !Text
  , ldNullLiteral :: !Text

    -- Constructors
  , ldListStyle :: !ListConstructorStyle
  , ldTupleConstructor :: !Text  -- "tuple" or "list" or ""
  , ldRecordConstructor :: !Text -- "dict" or "OrderedDict" or "list"
  , ldRecordSeparator :: !Text   -- "=" for Python/R, "=>" for Julia

    -- Access styles
  , ldIndexStyle :: !IndexStyle
  , ldKeyAccess :: !Text          -- "bracket" -> e["k"], "double_bracket" -> e[["k"]]
  , ldFieldAccess :: !FieldAccessStyle

    -- Serialize/deserialize function names
  , ldSerializeFn :: !Text     -- "morloc.put_value" or "morloc_put_value"
  , ldDeserializeFn :: !Text   -- "morloc.get_value" or "morloc_get_value"

    -- Foreign call template
  , ldForeignCallFn :: !Text   -- "morloc.foreign_call" or "morloc_foreign_call"
  , ldForeignCallSocketPath :: !Text -- how to construct socket path
  , ldForeignCallIntSuffix :: !Text  -- "L" for R, "" for others

    -- Remote call
  , ldRemoteCallFn :: !Text              -- "morloc.remote_call" or "morloc_remote_call"
  , ldResourcePackStyle :: !ResourcePackStyle -- how to format resources

    -- Syntax styles
  , ldAssignment :: !AssignmentStyle
  , ldFunctionDef :: !FunctionDefStyle
  , ldLambda :: !LambdaStyle
  , ldPartialApp :: !PartialAppStyle
  , ldSuspend :: !SuspendStyle
  , ldMapList :: !MapListStyle
  , ldPattern :: !PatternInterpStyle
  , ldDispatchTable :: !DispatchTableStyle

    -- Record access
  , ldDictStyleRecords :: !Bool   -- True: NamRecord/dict use bracket access, others use dot (Python)
  , ldQuoteRecordKeys :: !Bool    -- True: "k" => v (Julia), False: k=v (Python, R)

    -- Import syntax
  , ldQualifiedImports :: !Bool   -- True: qualify source names with module path (Python)
  , ldImportPrefix :: !Text  -- "import " or "source(" etc.
  , ldIncludeRelToFile :: !Bool  -- True if include() resolves relative to file (Julia), False for CWD (R)

    -- Pool template
  , ldPoolTemplate :: !Text    -- pool template content
  , ldBreakMarker :: !Text     -- "# <<<BREAK>>>"
  , ldCommentMarker :: !Text   -- "#" or "//" or "--"

    -- Execution
  , ldRunCommand :: ![Text]    -- command to run pool, e.g. ["python3"]
  , ldIsCompiled :: !Bool

    -- External codegen (optional)
  , ldCodegenCommand :: !(Maybe Text)  -- e.g. "morloc-codegen-generic"
  } deriving (Eq, Show, Generic)

-- YAML instances (using generic deriving + custom FromJSON)
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

instance Y.FromJSON AssignmentStyle where
  parseJSON = Y.withText "AssignmentStyle" $ \t -> case t of
    "equals" -> pure EqualsAssign
    "arrow" -> pure ArrowAssign
    _ -> fail $ "Unknown AssignmentStyle: " <> T.unpack t

instance Y.FromJSON FunctionDefStyle where
  parseJSON = Y.withText "FunctionDefStyle" $ \t -> case t of
    "python_def" -> pure PythonDef
    "r_assign" -> pure RAssignDef
    "end_block" -> pure EndBlockDef
    _ -> fail $ "Unknown FunctionDefStyle: " <> T.unpack t

instance Y.FromJSON LambdaStyle where
  parseJSON = Y.withText "LambdaStyle" $ \t -> case t of
    "python_lambda" -> pure PythonLambda
    "r_function" -> pure RFunction
    "arrow" -> pure ArrowLambda
    _ -> fail $ "Unknown LambdaStyle: " <> T.unpack t

instance Y.FromJSON PartialAppStyle where
  parseJSON = Y.withText "PartialAppStyle" $ \t -> case t of
    "functools_partial" -> pure FunctoolsPartial
    "anonymous_wrapper" -> pure AnonymousWrapper
    "arrow_wrapper" -> pure ArrowWrapper
    _ -> fail $ "Unknown PartialAppStyle: " <> T.unpack t

instance Y.FromJSON DispatchTableStyle where
  parseJSON = Y.withText "DispatchTableStyle" $ \t -> case t of
    "python_dict" -> pure PythonDictDispatch
    "r_list" -> pure RListDispatch
    "arrow_dict" -> pure ArrowDictDispatch
    _ -> fail $ "Unknown DispatchTableStyle: " <> T.unpack t

instance Y.FromJSON MapListStyle where
  parseJSON = Y.withText "MapListStyle" $ \t -> case t of
    "python_for" -> pure PythonForAppend
    "r_lapply" -> pure RLapply
    "comprehension" -> pure Comprehension
    _ -> fail $ "Unknown MapListStyle: " <> T.unpack t

instance Y.FromJSON SuspendStyle where
  parseJSON = Y.withText "SuspendStyle" $ \t -> case t of
    "python" -> pure PythonSuspend
    "r" -> pure RSuspend
    "arrow" -> pure ArrowSuspend
    _ -> fail $ "Unknown SuspendStyle: " <> T.unpack t

instance Y.FromJSON PatternInterpStyle where
  parseJSON = Y.withText "PatternInterpStyle" $ \t -> case t of
    "python_fstring" -> pure PythonFString
    "r_paste0" -> pure RPaste0
    "dollar_interp" -> pure DollarInterp
    _ -> fail $ "Unknown PatternInterpStyle: " <> T.unpack t

instance Y.FromJSON ListConstructorStyle where
  parseJSON = Y.withText "ListConstructorStyle" $ \t -> case t of
    "bracket" -> pure BracketList
    "function" -> pure FunctionList
    "r_atomic" -> pure RAtomicList
    _ -> fail $ "Unknown ListConstructorStyle: " <> T.unpack t

instance Y.FromJSON ResourcePackStyle where
  parseJSON = Y.withText "ResourcePackStyle" $ \t -> case t of
    "struct_pack" -> pure StructPackResources
    "named_list" -> pure NamedListResources
    "plain_list" -> pure PlainListResources
    _ -> fail $ "Unknown ResourcePackStyle: " <> T.unpack t

-- | Custom FromJSON that treats new optional fields as absent-safe.
-- Injects defaults for optional fields before delegating to generic parsing.
instance Y.FromJSON LangDescriptor where
  parseJSON = Y.withObject "LangDescriptor" $ \obj -> do
    let ins k v = KM.insertWith (\_ old -> old) (AesonKey.fromText k) v
        withDefaults = ins "ldCodegenCommand" Y.Null
                     . ins "ldRemoteCallFn" (Y.String "")
                     . ins "ldResourcePackStyle" (Y.String "plain_list")
                     . ins "ldDictStyleRecords" (Y.Bool False)
                     . ins "ldQuoteRecordKeys" (Y.Bool True)
                     . ins "ldQualifiedImports" (Y.Bool False)
                     $ obj
    Aeson.genericParseJSON Aeson.defaultOptions (Y.Object withDefaults)

-- | Load a language descriptor from a YAML file
loadLangDescriptor :: FilePath -> IO (Either String LangDescriptor)
loadLangDescriptor path = do
  result <- Y.decodeFileEither path
  return $ case result of
    Left err -> Left $ Y.prettyPrintParseException err
    Right desc -> Right desc

-- | Default descriptor with Python-like syntax (good base for many languages)
defaultLangDescriptor :: Text -> String -> LangDescriptor
defaultLangDescriptor name ext = LangDescriptor
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
  , ldForeignCallSocketPath = "os.path.join(global_state[\"tmpdir\"], \"{socket}\")"
  , ldForeignCallIntSuffix = ""
  , ldRemoteCallFn = ""
  , ldResourcePackStyle = PlainListResources
  , ldAssignment = EqualsAssign
  , ldFunctionDef = PythonDef
  , ldLambda = PythonLambda
  , ldPartialApp = FunctoolsPartial
  , ldSuspend = PythonSuspend
  , ldMapList = PythonForAppend
  , ldPattern = PythonFString
  , ldDispatchTable = PythonDictDispatch
  , ldDictStyleRecords = False
  , ldQuoteRecordKeys = True
  , ldQualifiedImports = False
  , ldImportPrefix = "import "
  , ldIncludeRelToFile = False
  , ldPoolTemplate = ""
  , ldBreakMarker = "# <<<BREAK>>>"
  , ldCommentMarker = "#"
  , ldRunCommand = []
  , ldIsCompiled = False
  , ldCodegenCommand = Nothing
  }
