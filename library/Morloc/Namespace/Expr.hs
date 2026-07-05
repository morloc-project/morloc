{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.Namespace.Expr
Description : Frontend AST, source/config types, post-typecheck tree
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Expression types at three stages of the compiler pipeline:

* 'Expr' \/ 'ExprI' -- parser output (untyped AST with integer indices)
* 'E' -- intermediate typed tree used during typechecking
* 'AnnoS' \/ 'ExprS' -- post-typecheck annotated tree passed to code generation

Also defines 'Source' (foreign function binding), config types
('ManifoldConfig', 'ModuleConfig', 'BuildConfig'), and mapping combinators
over the 'AnnoS' tree.
-}
module Morloc.Namespace.Expr
  ( -- * Source and config types
    Source (..)
  , RemoteResources (..)
  , ManifoldConfig (..)
  , LogTemplate (..)
  , RunLogTemplate (..)
  , EpilogueTemplate (..)
  , ModuleConfig (..)
  , BuildConfig (..)

    -- * Mostly frontend expressions
  , Symbol (..)
  , AliasedSymbol (..)
  , Signature (..)
  , Typeclass (..)
  , Selector (..)
  , ungroup
  , bracketArity
  , selectorHasBracket
  , Pattern (..)
  , Intrinsic (..)
  , intrinsicName
  , intrinsicArity
  , parseIntrinsic
  , Expr (..)
  , ExprI (..)
  , E (..)
  , Lit (..)
  , RealLit (..)
  , isFiniteRealLit
  , showRealLit
  , Import (..)
  , Export (..)
  , ExportGroup (..)
  , Fixity (..)
  , Associativity (..)

    -- * Post-typechecking tree
  , ExecutableExpr (..)
  , AnnoS (..)
  , ExprS (..)
  , Coercion (..)
  , applyCoercion
  , unapplyCoercion
  , ManyPoly (..)
  , mapAnnoSM
  , mapExprSM
  , mapAnnoS
  , mapExprS
  , mapAnnoSC
  , mapAnnoSCM
  , mapAnnoSG
  , mapAnnoSGM
  , mapExprSC
  , mapExprSCM
  , mapExprSG
  , mapExprSGM

    -- * JSON helpers
  , stripPrefixAndKebabCase
  , convertToKebabCase
  ) where

import Control.Monad.Identity (runIdentity)
import Data.Aeson (FromJSON (..), (.!=), (.:?))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Options (..), defaultOptions)
import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.Scientific (Scientific)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Morloc.Data.Doc
import Morloc.Namespace.Prim
import Morloc.Namespace.Type

---- Source and config types

data Source
  = Source
  { srcName :: SrcName
  , srcLang :: Lang
  , srcPath :: Maybe Path
  , srcAlias :: EVar
  , srcLabel :: Maybe Label
  , srcRsize :: [Int]
  , srcNote :: [Text]
  , srcInline :: !Bool
  , srcOperator :: !Bool
  }
  deriving (Ord, Eq, Show)

data RemoteResources = RemoteResources
  { remoteResourcesThreads :: Maybe Int
  , remoteResourcesMemory :: Maybe Int
  , remoteResourcesTime :: Maybe TimeInSeconds
  , remoteResourcesGpus :: Maybe Int
  }
  deriving (Show, Ord, Eq, Generic)

-- | Per-event log-message templates. Each subfield is the format string
-- for the start, pass, and fail log lines respectively. 'Nothing' for a
-- subfield means \"emit nothing for this event\". The compiler resolves
-- the effective template per labeled manifold by merging in this order:
--   per-label 'manifoldConfigLogTemplate'
--      > module-level 'moduleConfigLogTemplate'
--      > built-in default ('defaultLogTemplate')
-- per-subfield. Each subfield supports @{name}@-style placeholder
-- substitution; see 'Morloc.CodeGenerator.LogTemplate' for the
-- available variables and the rendering pipeline.
data LogTemplate = LogTemplate
  { logTemplateStart :: Maybe Text
  , logTemplatePass :: Maybe Text
  , logTemplateFail :: Maybe Text
  }
  deriving (Show, Ord, Eq, Generic)

-- | Run-scope log templates. 'runLogPrologue' fires at the nexus
-- entrypoint immediately after argument parsing; the appropriate
-- 'runLogEpilogue' subfield fires at clean exit (Ok) or via the
-- failure-path guard (Fail). 'Nothing' silences the corresponding
-- event. Lives at the top level of the program YAML alongside
-- 'log-template'; not per-label.
data RunLogTemplate = RunLogTemplate
  { runLogPrologue :: Maybe Text
  , runLogEpilogue :: Maybe EpilogueTemplate
  }
  deriving (Show, Ord, Eq, Generic)

-- | Success / failure branch of the run epilogue. Two templates so the
-- success line never has to render an empty @{error}@ field and the
-- failure line can include 'error' / 'exit_code' fields the success
-- line lacks.
data EpilogueTemplate = EpilogueTemplate
  { epilogueOk :: Maybe Text
  , epilogueFail :: Maybe Text
  }
  deriving (Show, Ord, Eq, Generic)

data ManifoldConfig = ManifoldConfig
  { manifoldConfigCache :: Maybe Bool
  , manifoldConfigBenchmark :: Maybe Bool
  , manifoldConfigRemote :: Maybe RemoteResources
  , manifoldConfigLog :: Maybe Bool
  , manifoldConfigLabel :: Maybe Text
  , manifoldConfigLogTemplate :: Maybe LogTemplate
  , manifoldConfigLabelIdx :: Maybe Int
  -- ^ The ExprI index where the label was attached. Set by 'collectTags'
  -- when a labeled VarE is registered and preserved by
  -- 'propagateManifoldLabel'. Used at codegen to look up the source
  -- position (for @{module}@/@{line}@/@{column}@) and the term name
  -- (for @{name}@). Not user-facing; absent from YAML.
  }
  deriving (Show, Ord, Eq, Generic)

data ModuleConfig = ModuleConfig
  { moduleConfigDefaultGroup :: Maybe ManifoldConfig
  , moduleConfigLabeledGroups :: Map.Map Text ManifoldConfig
  , moduleConfigLogTemplate :: Maybe LogTemplate
  , moduleConfigPrologue :: Maybe Text
  -- ^ Top-level YAML field @prologue@: a template string fired by the
  -- nexus at run start. See 'Morloc.CodeGenerator.LogTemplate' for the
  -- available placeholders.
  , moduleConfigEpilogue :: Maybe EpilogueTemplate
  -- ^ Top-level YAML field @epilogue@ with @ok@/@fail@ sub-fields. The
  -- nexus picks the branch matching the run's final disposition.
  , moduleConfigHashInclude :: Maybe [Text]
  -- ^ Top-level YAML field @hash-include@: a list of paths (glob-aware,
  -- relative to the program's YAML) whose file contents are folded into
  -- the per-pool cache key. Use this to invalidate cached results when
  -- a foreign source file or runtime data file changes. Paths follow
  -- the same scope rules as @packageInclude@ -- absolute paths and
  -- @..@ traversals are rejected.
  }
  deriving (Show, Generic)

data BuildConfig = BuildConfig
  { buildConfigSlurmSupport :: Maybe Bool
  , buildConfigSanitize :: Maybe Bool
  }
  deriving (Show, Generic)

---- Expressions

data Symbol
  = TypeSymbol TVar
  | TermSymbol EVar
  | ClassSymbol ClassName
  deriving (Show, Ord, Eq)

data ExportGroup = ExportGroup
  { exportGroupName :: !Text
  , exportGroupDesc :: [Text]
  , exportGroupMembers :: Set.Set (Int, Symbol)
  }
  deriving (Show, Ord, Eq)

data Export
  = ExportMany (Set.Set (Int, Symbol)) [ExportGroup]
  | ExportAll
  deriving (Show, Ord, Eq)

data AliasedSymbol
  = AliasedType TVar TVar
  | AliasedTerm EVar EVar
  | AliasedClass ClassName
  deriving (Show, Ord, Eq)

data Signature = Signature EVar (Maybe Label) EType
  deriving (Show, Ord, Eq)

data Typeclass a = Typeclass [Constraint] ClassName [TVar] [a]
  deriving (Show, Ord, Eq)

-- | The unified pattern descriptor. A Selector encodes the full access
-- chain a pattern performs on its subject: any mix of field-index /
-- field-key navigation, bracket-index / bracket-slice access, and
-- multi-sibling groups. Patterns are a first-class object end-to-end
-- (desugar -> typecheck -> codegen -> runtime); the codegen lowers
-- the Selector into the right runtime primitive for the receiver
-- type (mlc_ifile_walk for IFile handles; nested apply_* calls for
-- in-memory values).
--
-- Sibling semantics:
--
--   * @SelectorKey (h, s) []@ and @SelectorIdx (h, s) []@ are
--     single-step chains (no group).
--   * @SelectorKey (h, s) others@ and @SelectorIdx (h, s) others@
--     express same-kind sibling groups (all keys, or all indices)
--     branching at the current position. Each sibling has its own
--     sub-Selector tail.
--
-- Bracket steps:
--
--   * @SelectorBracketIndex@ consumes one runtime arg (the index)
--     and continues with the inner Selector applied to the
--     materialised element.
--   * @SelectorBracketSlice@ consumes three runtime args (start,
--     stop, step) and is terminal: a slice returns a list, and any
--     further per-element walk is morloc's @IntrMap@ territory, not
--     a single pattern walk.
data Selector
  = SelectorKey (Text, Selector) [(Text, Selector)]
  | SelectorIdx (Int, Selector) [(Int, Selector)]
  | SelectorBracketIndex Selector
  | SelectorBracketSlice
  | SelectorEnd
  deriving (Show, Ord, Eq)

-- | Enumerate every distinct leaf-path through a Selector. Used for
-- documentation, error messages, and the legacy non-bracketed value-
-- check path. Brackets are represented opaquely (the path "passes
-- through" a bracket step without expanding it -- a bracket has no
-- left/right axis at this level), so 'ungroup' stays a pure field-
-- chain enumerator.
ungroup :: Selector -> [[Either Int Text]]
ungroup SelectorEnd = [[]]
ungroup (SelectorBracketIndex s) = ungroup s
ungroup SelectorBracketSlice = [[]]
ungroup (SelectorKey (k, SelectorEnd) []) = [[Right k]]
ungroup (SelectorIdx (i, SelectorEnd) []) = [[Left i]]
ungroup (SelectorKey x xs) = concat [map ((:) (Right k)) (ungroup s) | (k, s) <- (x : xs)]
ungroup (SelectorIdx x xs) = concat [map ((:) (Left i)) (ungroup s) | (i, s) <- (x : xs)]

-- | DFS-count of bracket steps in a Selector. Each
-- 'SelectorBracketIndex' contributes 1 runtime arg (the index); each
-- 'SelectorBracketSlice' contributes 3 (start, stop, step). Used by
-- the typechecker to validate @App (PatCall (PatternStruct sel)) args@
-- arity: @args@ must be @bracketArity sel + 1@ (the +1 is the
-- receiver).
bracketArity :: Selector -> Int
bracketArity SelectorEnd = 0
bracketArity (SelectorBracketIndex s) = 1 + bracketArity s
bracketArity SelectorBracketSlice = 3
bracketArity (SelectorKey (_, s) others) =
  bracketArity s + sum [bracketArity sub | (_, sub) <- others]
bracketArity (SelectorIdx (_, s) others) =
  bracketArity s + sum [bracketArity sub | (_, sub) <- others]

-- | True if the selector contains any bracket step. Used at codegen
-- boundaries to decide whether the IFile walker (or the in-memory
-- bracket-aware lowering) is required.
selectorHasBracket :: Selector -> Bool
selectorHasBracket SelectorEnd = False
selectorHasBracket (SelectorBracketIndex _) = True
selectorHasBracket SelectorBracketSlice = True
selectorHasBracket (SelectorKey (_, s) others) =
  selectorHasBracket s || any (selectorHasBracket . snd) others
selectorHasBracket (SelectorIdx (_, s) others) =
  selectorHasBracket s || any (selectorHasBracket . snd) others

data Pattern
  = PatternText Text [Text]
  | PatternStruct Selector
  -- ^ Field / index accessor (record .field, tuple .0).
  | PatternBracketIndex
  -- ^ Python-style bracket index (xs[i]). Carries no payload; the index
  -- expression and receiver are passed via the outer AppS as
  -- [index, receiver]. The typechecker synthesizes the result type as
  -- the receiver's element type. The codegen translator emits the
  -- appropriate native indexing operation based on the receiver type.
  | PatternBracketSlice
  -- ^ Python-style bracket slice (xs[i:j:k]). Carries no payload; the
  -- three bound expressions (start, stop, step; each Null or an Int64
  -- after toIndex wrapping) and the receiver are passed via the outer
  -- AppS as [start, stop, step, receiver]. The typechecker synthesizes
  -- the result type structurally: a Nat-parameterized container has
  -- its outer Nat replaced by NatUnknown; otherwise the receiver type
  -- is preserved. The codegen translator emits the appropriate native
  -- slicing operation.
  deriving (Show, Ord, Eq)

-- | Compiler intrinsics: functions the compiler generates specialized code for.
data Intrinsic
  = IntrSave      -- ^ @save  :: Int -> a -> Str -> <IO>() -- voidstar packet, with zstd level 0-9
  | IntrSaveM     -- ^ @savem :: Str -> a -> <IO>()         -- raw msgpack file
  | IntrSaveJ     -- ^ @savej :: Str -> a -> <IO>()         -- raw JSON file
  | IntrLoad      -- ^ @load  :: Str -> <IO> ?a             -- auto-detect format, auto-decompress packets
  | IntrHash      -- ^ @hash   :: a -> Str           -- xxhash, hex string
  | IntrVersion   -- ^ @version :: Str               -- compiler version
  | IntrCompiled  -- ^ @compiled :: Str              -- compile timestamp
  | IntrLang      -- ^ @lang    :: Str               -- current pool language
  | IntrSchema    -- ^ @schema  :: a -> Str          -- schema string
  | IntrTypeof    -- ^ @typeof  :: a -> Str          -- concrete type name
  | IntrShow      -- ^ @show   :: a -> Str           -- serialize to JSON string
  | IntrRead      -- ^ @read   :: Str -> ?a          -- deserialize from JSON string
  | IntrDatafile  -- ^ @datafile :: Str -> Str       -- resolve installed data file path
  | IntrOpen      -- ^ @open  :: Str -> <IO> a       -- open a stream/file; `a` resolved via inline ascription to IFile/IStream/OStream
  | IntrClose     -- ^ @close :: a -> <IO> ()        -- close any stream/file handle
  | IntrFSchema   -- ^ @fschema :: Str -> <IO> Str   -- read a file's element schema without typed open
  | IntrMap       -- ^ implicit @(a -> b) -> List a -> List b@ map; emitted by
                  -- the desugar's bracket-accessor lowering when a slice is
                  -- followed by a chained accessor (e.g. @.[::-1].x pts@). NOT
                  -- a user-facing @\@map@ -- there is no entry in 'parseIntrinsic'.
                  -- The pure-runtime evaluator executes this as a direct
                  -- per-element loop over MORLOC_ARRAY; the pool path resolves
                  -- it to the language's @Functor.map@ instance.
  | IntrFLength     -- ^ @flen :: IFile a -> <IO> Int@ -- file element count.
                    -- Free from the footer's StreamDiag.element_count. Users
                    -- typically alias as @length@ via stdlib shims.
  | IntrNext        -- ^ @next :: IStream a -> <IO> [a]@ -- materialise the
                    -- current sub-packet and advance the cursor. Returns an
                    -- empty list at EOF (further calls keep returning empty).
  | IntrStream      -- ^ @stream :: IFile a -> <IO> IStream a@ -- derive a
                    -- forward-only IStream from an open IFile, bound to the
                    -- same path with an independent fd, mmap, and cursor.
  | IntrWrite       -- ^ @write :: Int -> OStream a -> [a] -> <IO> ()@ --
                    -- emit one sub-packet of element-list type. The Int is
                    -- the zstd compression level (0 = uncompressed); the
                    -- first @write fixes the level for the file's lifetime.
  | IntrAppend      -- ^ @append :: Str -> <IO> (OStream a)@ -- open an
                    -- existing stream file for append. Forward-scan recovers
                    -- the resume cursor; mismatched element schemas error
                    -- before any bytes are written.
  | IntrConcat      -- ^ @concat :: [Str] -> Str -> <IO> ()@ -- concatenate
                    -- a sequence of stream files via sendfile, exploiting
                    -- the stream-packet concat invariant.
  | IntrFlush       -- ^ @flush :: OStream a -> <IO> ()@ -- force buffered
                    -- writes to be emitted as a sub-packet immediately,
                    -- without closing the stream. No-op on an empty
                    -- buffer. Useful for tests that need deterministic
                    -- packet boundaries and for user code that wants to
                    -- make progress visible to concurrent readers.
  | IntrStdin       -- ^ @stdin :: <IO> IStream a@ -- nullary intrinsic
                    -- that opens process stdin as an IStream. The nexus is
                    -- the sole owner of fd 0; @next routes through the
                    -- pool-nexus RPC socket. At most one @stdin per nexus.
  | IntrStdout      -- ^ @stdout :: <IO> OStream a@ -- nullary; opens
                    -- process stdout as an OStream. @write routes through
                    -- the nexus. At most one @stdout per nexus.
  | IntrStderr      -- ^ @stderr :: <IO> OStream a@ -- symmetric with
                    -- @stdout for stderr.
  | IntrIFileWalk   -- ^ Unified IFile pattern walker. Synthesized by Express.hs
                    -- and Nexus.hs from any pattern application with an IFile
                    -- receiver (`.[i] f`, `.[s:e:p] f`, `.foo.bar f`, mixed
                    -- chains). The selector chain is encoded as a path string
                    -- (`.1.[]`, `.foo.[:]`, etc.); bracket steps consume
                    -- runtime args in DFS order. Lowers to a single per-
                    -- language @_mlc_ifile_walk@ call. NOT user-facing; arity
                    -- is dynamic (path + handle + 0..n bracket bounds), so
                    -- 'intrinsicArity' is intentionally undefined.
  deriving (Show, Ord, Eq)

-- | Map intrinsic to its canonical name
intrinsicName :: Intrinsic -> Text
intrinsicName IntrSave = "save"
intrinsicName IntrSaveM = "savem"
intrinsicName IntrSaveJ = "savej"
intrinsicName IntrLoad = "load"
intrinsicName IntrHash = "hash"
intrinsicName IntrVersion = "version"
intrinsicName IntrCompiled = "compiled"
intrinsicName IntrLang = "lang"
intrinsicName IntrSchema = "schema"
intrinsicName IntrTypeof = "typeof"
intrinsicName IntrShow = "show"
intrinsicName IntrRead = "read"
intrinsicName IntrDatafile = "datafile"
intrinsicName IntrOpen = "open"
intrinsicName IntrClose = "close"
intrinsicName IntrFSchema = "fschema"
intrinsicName IntrMap = "map"
intrinsicName IntrFLength = "flen"
intrinsicName IntrNext = "next"
intrinsicName IntrStream = "stream"
intrinsicName IntrWrite = "write"
intrinsicName IntrAppend = "append"
intrinsicName IntrConcat = "concat"
intrinsicName IntrFlush = "flush"
intrinsicName IntrStdin = "stdin"
intrinsicName IntrStdout = "stdout"
intrinsicName IntrStderr = "stderr"
intrinsicName IntrIFileWalk = "ifile_walk"

-- | Parse a name to an intrinsic (Nothing if not a known intrinsic)
parseIntrinsic :: Text -> Maybe Intrinsic
parseIntrinsic "save" = Just IntrSave
parseIntrinsic "savem" = Just IntrSaveM
parseIntrinsic "savej" = Just IntrSaveJ
parseIntrinsic "load" = Just IntrLoad
parseIntrinsic "hash" = Just IntrHash
parseIntrinsic "version" = Just IntrVersion
parseIntrinsic "compiled" = Just IntrCompiled
parseIntrinsic "lang" = Just IntrLang
parseIntrinsic "schema" = Just IntrSchema
parseIntrinsic "typeof" = Just IntrTypeof
parseIntrinsic "show" = Just IntrShow
parseIntrinsic "read" = Just IntrRead
parseIntrinsic "datafile" = Just IntrDatafile
parseIntrinsic "open" = Just IntrOpen
parseIntrinsic "close" = Just IntrClose
parseIntrinsic "fschema" = Just IntrFSchema
-- @flen is user-facing so users can alias: `length = @flen` in stdlib.
-- IntrIFileWalk is compiler-internal (synthesized from `.[i] f`, `.foo f`,
-- etc. by Express.hs and Nexus.hs); no entry here.
parseIntrinsic "flen" = Just IntrFLength
parseIntrinsic "next" = Just IntrNext
parseIntrinsic "stream" = Just IntrStream
parseIntrinsic "write" = Just IntrWrite
parseIntrinsic "append" = Just IntrAppend
parseIntrinsic "concat" = Just IntrConcat
parseIntrinsic "flush" = Just IntrFlush
parseIntrinsic "stdin" = Just IntrStdin
parseIntrinsic "stdout" = Just IntrStdout
parseIntrinsic "stderr" = Just IntrStderr
parseIntrinsic _ = Nothing

-- | Expected number of arguments for each intrinsic
intrinsicArity :: Intrinsic -> Int
intrinsicArity IntrSave = 3
intrinsicArity IntrSaveM = 2
intrinsicArity IntrSaveJ = 2
intrinsicArity IntrLoad = 1
intrinsicArity IntrHash = 1
intrinsicArity IntrVersion = 0
intrinsicArity IntrCompiled = 0
intrinsicArity IntrLang = 0
intrinsicArity IntrSchema = 1
intrinsicArity IntrTypeof = 1
intrinsicArity IntrShow = 1
intrinsicArity IntrRead = 1
intrinsicArity IntrDatafile = 1
intrinsicArity IntrOpen = 1
intrinsicArity IntrClose = 1
intrinsicArity IntrFSchema = 1
intrinsicArity IntrMap = 2
intrinsicArity IntrFLength = 1
intrinsicArity IntrNext = 1
intrinsicArity IntrStream = 1
intrinsicArity IntrWrite = 3
intrinsicArity IntrAppend = 1
intrinsicArity IntrConcat = 2
intrinsicArity IntrFlush = 1
intrinsicArity IntrStdin = 0
intrinsicArity IntrStdout = 0
intrinsicArity IntrStderr = 0
intrinsicArity IntrIFileWalk =
  error "intrinsicArity: IntrIFileWalk has dynamic arity (path + handle + 0..n bracket bounds) and is never eta-expanded"

data ExprI = ExprI Int Expr
  deriving (Show, Ord, Eq)

data Expr
  = ModE MVar [ExprI]
  | ClsE (Typeclass Signature)
  | IstE ClassName [TypeU] [ExprI]
  | EffE EffectLabel Bool  -- ^ effect declaration: label, isEscapable
  | TypE ExprTypeE
  | ImpE Import
  | ExpE Export
  | SrcE Source
  | SigE Signature
  | AssE EVar ExprI [ExprI]
  | FixE Fixity
  | BopE ExprI Int EVar ExprI
  | UniE
  | NullE
  | VarE ManifoldConfig EVar
  | LstE [ExprI]
  | TupE [ExprI]
  | NamE [(Key, ExprI)]
  | AppE ExprI [ExprI]
  | LamE [EVar] ExprI
  | AnnE ExprI TypeU
  | LetE [(EVar, ExprI)] ExprI
  | RealE RealLit
  | IntE Integer
  | LogE Bool
  | StrE Text
  | PatE Pattern
  | IfE ExprI ExprI ExprI
  | DoBlockE ExprI
  | EvalE ExprI
  | IntrinsicE Intrinsic [ExprI]
  | ParenE !ExprI  -- ^ transient parenthesization wrapper (eliminated by handleBinops)
  deriving (Show, Ord, Eq)

data Import
  = Import
  { importModuleName :: MVar
  , importInclude :: Maybe [AliasedSymbol]
  , importExclude :: [Symbol]
  , importNamespace :: Maybe EVar
  }
  deriving (Ord, Eq, Show)

data Associativity
  = InfixL
  | InfixR
  | InfixN
  deriving (Show, Ord, Eq, Enum)

data Fixity = Fixity
  { fixityAssoc :: Associativity
  , fixityPrecedence :: Int
  , fixityOperators :: [EVar]
  }
  deriving (Show, Ord, Eq)

data Lit
  = MNum RealLit
  | MInt Integer
  | MLog Bool
  | MStr Text
  | MUni
  | MNull
  deriving (Ord, Eq, Show)

-- | Real (IEEE 754) literal. Finite values carry a 'Scientific' to preserve
-- arbitrary precision until codegen, where they are bounds-checked against
-- the target Float32/Float64 representation. The non-finite forms are
-- introduced by the source-level keywords @Inf@ / @-Inf@ / @NaN@; they bypass
-- the bounds check (they have no finite magnitude to overflow) and are
-- emitted directly in each language's idiomatic non-finite syntax.
data RealLit
  = RealFinite !Scientific
  | RealPosInf
  | RealNegInf
  | RealNaN
  deriving (Show, Ord, Eq)

-- | True when the literal carries a finite Scientific payload.
isFiniteRealLit :: RealLit -> Bool
isFiniteRealLit (RealFinite _) = True
isFiniteRealLit _ = False

-- | Lowercase canonical text form, matching the JSON wire convention
-- ("nan"/"inf"/"-inf"). Used by Show/pretty paths and by Nexus.hs when
-- emitting the literal text into the manifest.
showRealLit :: RealLit -> String
showRealLit (RealFinite x) = show x
showRealLit RealPosInf = "inf"
showRealLit RealNegInf = "-inf"
showRealLit RealNaN = "nan"

data E
  = BndP (Indexed Type) EVar
  | VarP (Indexed Type) EVar [E]
  | AppP (Indexed Type) E [E]
  | LamP (Indexed Type) [EVar] E
  | LstP (Indexed Type) [E]
  | TupP (Indexed Type) [E]
  | NamP (Indexed Type) [(Key, E)]
  | LitP (Indexed Type) Lit
  | SrcP (Indexed Type) Source
  | PatP (Indexed Type) Selector
  -- | Bracket-index pattern at the value-check level. Applied via AppP
  -- to (index, receiver). The codegen translator emits the appropriate
  -- native indexing operation per receiver type.
  | BracketIndexP (Indexed Type)
  -- | Bracket-slice pattern at the value-check level. Applied via AppP
  -- to (start, stop, step, receiver). The codegen translator emits the
  -- appropriate native slicing operation per receiver type.
  | BracketSliceP (Indexed Type)
  | IfP (Indexed Type) E E E
  | DoBlockP (Indexed Type) E
  | EvalP (Indexed Type) E
  | CoerceP Coercion (Indexed Type) E
  | IntrinsicP (Indexed Type) Intrinsic [E]
  deriving (Ord, Eq, Show)

-- | Coercion tag for implicit type conversions inserted by the typechecker.
-- Extensible: future coercions (e.g., numeric widening) add constructors here.
data Coercion
  = CoerceToOptional
  deriving (Show, Eq, Ord)

-- | Apply a coercion to a type, returning the coerced type.
applyCoercion :: Coercion -> TypeU -> TypeU
applyCoercion CoerceToOptional t = OptionalU t

-- | Invert a coercion on a resolved Type.
unapplyCoercion :: Coercion -> Type -> Type
unapplyCoercion CoerceToOptional (OptionalT t) = t
unapplyCoercion CoerceToOptional t = t  -- defensive fallback

data ExecutableExpr = SrcCall Source | PatCall Pattern
  deriving (Ord, Eq, Show)

data AnnoS g f c = AnnoS g c (ExprS g f c)

data ExprS g f c
  = UniS
  | NullS
  | BndS EVar
  | VarS EVar (f (AnnoS g f c))
  | AppS (AnnoS g f c) [AnnoS g f c]
  | LamS [EVar] (AnnoS g f c)
  | LstS [AnnoS g f c]
  | TupS [AnnoS g f c]
  | NamS [(Key, AnnoS g f c)]
  -- Numeric literals carry an Int source-position index. The wrapping
  -- AnnoS index points at the binding/expression that owns the literal,
  -- which after term inlining can be the export reference. Keeping the
  -- literal's own index lets compile-time overflow checks (Nexus.hs)
  -- report the error at the literal's actual location.
  | RealS Int RealLit
  | IntS Int Integer
  | LogS Bool
  | StrS Text
  | ExeS ExecutableExpr
  | LetS EVar (AnnoS g f c) (AnnoS g f c)
  | LetBndS EVar
  | CallS EVar  -- recursive call back-edge
  | IfS (AnnoS g f c) (AnnoS g f c) (AnnoS g f c)
  | DoBlockS (AnnoS g f c)
  | EvalS (AnnoS g f c)
  | CoerceS Coercion (AnnoS g f c)
  | IntrinsicS Intrinsic [AnnoS g f c]

data ManyPoly a = MonomorphicExpr (Maybe EType) [a] | PolymorphicExpr ClassName EVar EType [(EType, [a])]
  deriving (Show, Eq, Ord)

---- Class instances

instance HasOneLanguage Source where
  langOf s = Just (srcLang s)
  langOf' s = srcLang s

instance Functor ManyPoly where
  fmap f (MonomorphicExpr t xs) = MonomorphicExpr t (map f xs)
  fmap f (PolymorphicExpr cls v t xs) = PolymorphicExpr cls v t (map (second (map f)) xs)

instance Traversable ManyPoly where
  traverse f (MonomorphicExpr t xs) = MonomorphicExpr t <$> traverse f xs
  traverse f (PolymorphicExpr cls v t xs) = PolymorphicExpr cls v t <$> traverse f2 xs
    where
      f2 (t', x) = (,) t' <$> traverse f x

instance Foldable ManyPoly where
  foldr f b (MonomorphicExpr _ xs) = foldr f b xs
  foldr f b (PolymorphicExpr _ _ _ (concatMap snd -> xs)) = foldr f b xs

instance Defaultable ModuleConfig where
  defaultValue =
    ModuleConfig
      { moduleConfigDefaultGroup = Nothing
      , moduleConfigLabeledGroups = Map.empty
      , moduleConfigLogTemplate = Nothing
      , moduleConfigPrologue = Nothing
      , moduleConfigEpilogue = Nothing
      , moduleConfigHashInclude = Nothing
      }

instance Defaultable LogTemplate where
  defaultValue =
    LogTemplate
      { logTemplateStart = Nothing
      , logTemplatePass = Nothing
      , logTemplateFail = Nothing
      }

instance Defaultable EpilogueTemplate where
  defaultValue =
    EpilogueTemplate
      { epilogueOk = Nothing
      , epilogueFail = Nothing
      }

instance Defaultable BuildConfig where
  defaultValue =
    BuildConfig
      { buildConfigSlurmSupport = Nothing
      , buildConfigSanitize = Nothing
      }

instance Defaultable RemoteResources where
  defaultValue =
    RemoteResources
      { remoteResourcesThreads = Nothing
      , remoteResourcesMemory = Nothing
      , remoteResourcesTime = Nothing
      , remoteResourcesGpus = Nothing
      }

instance Defaultable ManifoldConfig where
  defaultValue =
    ManifoldConfig
      { manifoldConfigCache = Just False
      , manifoldConfigBenchmark = Just False
      , manifoldConfigRemote = Nothing
      , manifoldConfigLog = Just False
      , manifoldConfigLabel = Nothing
      , manifoldConfigLogTemplate = Nothing
      , manifoldConfigLabelIdx = Nothing
      }

-- Custom parser so each field is independently optional. Generic
-- derivation would require @labeled-groups@ even when the user only
-- declares, say, @prologue@ and @epilogue@.
instance FromJSON ModuleConfig where
  parseJSON = Aeson.withObject "ModuleConfig" $ \o ->
    ModuleConfig
      <$> o .:? "default-group"
      <*> o .:? "labeled-groups" .!= Map.empty
      <*> o .:? "log-template"
      <*> o .:? "prologue"
      <*> o .:? "epilogue"
      <*> o .:? "hash-include"

instance FromJSON ManifoldConfig where
  parseJSON =
    Aeson.genericParseJSON $
      defaultOptions {fieldLabelModifier = stripPrefixAndKebabCase "manifoldConfig"}

instance FromJSON LogTemplate where
  parseJSON =
    Aeson.genericParseJSON $
      defaultOptions {fieldLabelModifier = stripPrefixAndKebabCase "logTemplate"}

instance FromJSON EpilogueTemplate where
  parseJSON =
    Aeson.genericParseJSON $
      defaultOptions {fieldLabelModifier = stripPrefixAndKebabCase "epilogue"}

instance FromJSON RemoteResources where
  parseJSON =
    Aeson.genericParseJSON $
      defaultOptions {fieldLabelModifier = stripPrefixAndKebabCase "remoteResources"}

instance FromJSON BuildConfig where
  parseJSON =
    Aeson.genericParseJSON $
      defaultOptions {fieldLabelModifier = stripPrefixAndKebabCase "buildConfig"}

---- JSON helpers

-- Helper function to strip prefixes and convert to kebab-case
stripPrefixAndKebabCase :: String -> String -> String
stripPrefixAndKebabCase prefix str =
  let stripped = drop (length prefix) str
   in case stripped of
        [] -> []
        (x : xs) -> toLower x : convertToKebabCase xs

-- Convert remaining characters to kebab-case
convertToKebabCase :: String -> String
convertToKebabCase [] = []
convertToKebabCase (x : xs)
  | isUpper x = '-' : toLower x : convertToKebabCase xs
  | otherwise = x : convertToKebabCase xs

---- Helper functions for AnnoS/ExprS

mapExprSM ::
  (Traversable f, Monad m) => (AnnoS g f c -> m (AnnoS g' f c')) -> ExprS g f c -> m (ExprS g' f c')
mapExprSM f (VarS v xs) = VarS v <$> traverse f xs
mapExprSM f (AppS x xs) = AppS <$> f x <*> mapM f xs
mapExprSM f (LamS vs x) = LamS vs <$> f x
mapExprSM f (LstS xs) = LstS <$> mapM f xs
mapExprSM f (TupS xs) = TupS <$> mapM f xs
mapExprSM f (NamS rs) = NamS <$> mapM (secondM f) rs
mapExprSM _ UniS = return UniS
mapExprSM _ NullS = return NullS
mapExprSM _ (BndS v) = return $ BndS v
mapExprSM _ (RealS i x) = return $ RealS i x
mapExprSM _ (IntS i x) = return $ IntS i x
mapExprSM _ (LogS x) = return $ LogS x
mapExprSM _ (StrS x) = return $ StrS x
mapExprSM _ (ExeS x) = return $ ExeS x
mapExprSM f (LetS v e1 e2) = LetS v <$> f e1 <*> f e2
mapExprSM _ (LetBndS v) = return $ LetBndS v
mapExprSM _ (CallS v) = return $ CallS v
mapExprSM f (IfS c t e) = IfS <$> f c <*> f t <*> f e
mapExprSM f (DoBlockS e) = DoBlockS <$> f e
mapExprSM f (EvalS e) = EvalS <$> f e
mapExprSM f (CoerceS c e) = CoerceS c <$> f e
mapExprSM f (IntrinsicS intr es) = IntrinsicS intr <$> mapM f es

mapAnnoSM ::
  (Traversable f, Monad m) =>
  (ExprS g f c -> g -> c -> m (g', c')) ->
  AnnoS g f c ->
  m (AnnoS g' f c')
mapAnnoSM fun (AnnoS g c e) = do
  e' <- mapExprSM (mapAnnoSM fun) e
  (g', c') <- fun e g c
  return (AnnoS g' c' e')

mapAnnoS :: (Traversable f) => (ExprS g f c -> g -> c -> (g', c')) -> AnnoS g f c -> AnnoS g' f c'
mapAnnoS fun = runIdentity . mapAnnoSM (\x g c -> return (fun x g c))

mapExprS :: (Traversable f) => (AnnoS g f c -> AnnoS g' f c') -> ExprS g f c -> ExprS g' f c'
mapExprS fun = runIdentity . mapExprSM (return . fun)

mapAnnoSGM :: (Traversable f, Monad m) => (g -> m g') -> AnnoS g f c -> m (AnnoS g' f c)
mapAnnoSGM f = mapAnnoSM (\_ gi ci -> (,) <$> f gi <*> pure ci)

mapAnnoSCM :: (Traversable f, Monad m) => (c -> m c') -> AnnoS g f c -> m (AnnoS g f c')
mapAnnoSCM f = mapAnnoSM (\_ gi ci -> (,) gi <$> f ci)

mapAnnoSG :: (Traversable f) => (g -> g') -> AnnoS g f c -> AnnoS g' f c
mapAnnoSG f = mapAnnoS (\_ gi ci -> (f gi, ci))

mapAnnoSC :: (Traversable f) => (c -> c') -> AnnoS g f c -> AnnoS g f c'
mapAnnoSC f = mapAnnoS (\_ gi ci -> (gi, f ci))

mapExprSGM :: (Traversable f, Monad m) => (g -> m g') -> ExprS g f c -> m (ExprS g' f c)
mapExprSGM f = mapExprSM (\(AnnoS gi ci e) -> AnnoS <$> f gi <*> pure ci <*> mapExprSGM f e)

mapExprSCM :: (Traversable f, Monad m) => (c -> m c') -> ExprS g f c -> m (ExprS g f c')
mapExprSCM f = mapExprSM (\(AnnoS gi ci e) -> AnnoS gi <$> f ci <*> mapExprSCM f e)

mapExprSG :: (Traversable f) => (g -> g') -> ExprS g f c -> ExprS g' f c
mapExprSG f = mapExprS (\(AnnoS gi ci e) -> AnnoS (f gi) ci (mapExprSG f e))

mapExprSC :: (Traversable f) => (c -> c') -> ExprS g f c -> ExprS g f c'
mapExprSC f = mapExprS (\(AnnoS gi ci e) -> AnnoS gi (f ci) (mapExprSC f e))

----- Pretty instances -------------------------------------------------------

instance Pretty Lit where
  pretty (MNum x) = pretty (showRealLit x)
  pretty (MInt x) = pretty x
  pretty (MLog x) = pretty x
  pretty (MStr x) = pretty x
  pretty MUni = "Unit"
  pretty MNull = "Null"

instance Pretty E where
  pretty (BndP _ v) = pretty v
  pretty (VarP _ v _) = pretty v
  pretty (AppP _ e es) = pretty e <+> hsep (map f es)
    where
      f x@AppP {} = parens (pretty x)
      f x@LamP {} = parens (pretty x)
      f x@SrcP {} = parens (pretty x)
      f x = pretty x
  pretty (LamP _ vs e) = "\\" <+> hsep (map pretty vs) <+> "->" <+> pretty e
  pretty (LstP _ es) = list (map pretty es)
  pretty (TupP _ es) = tupled (map pretty es)
  pretty (NamP _ rs) = encloseSep "{" "}" "," [pretty k <+> "=" <+> pretty e | (k, e) <- rs]
  pretty (LitP _ l) = pretty l
  pretty (SrcP _ src) = pretty src
  pretty (PatP _ s) = pretty (PatternStruct s)
  pretty (BracketIndexP _) = pretty PatternBracketIndex
  pretty (BracketSliceP _) = pretty PatternBracketSlice
  pretty (IfP _ c t e) = "if" <+> pretty c <+> "then" <+> pretty t <+> "else" <+> pretty e
  pretty (DoBlockP _ e) = "{" <> pretty e <> "}"
  pretty (EvalP _ e) = "!" <> pretty e
  pretty (CoerceP _ _ e) = "coerce(" <> pretty e <> ")"
  pretty (IntrinsicP _ intr args) = "@" <> pretty (intrinsicName intr) <+> hsep (map pretty args)

instance Pretty Source where
  pretty s =
    "source" <+> pretty (srcLang s)
      <> maybe "" (\path -> " from" <+> dquotes (pretty path)) (srcPath s)
        <+> dquotes (pretty (srcName s))
        <+> "as"
        <+> pretty (srcAlias s)
      <> maybe "" (\t -> ":" <> pretty t) (srcLabel s)

instance Pretty ExportGroup where
  pretty (ExportGroup name desc members) =
    "--*" <+> pretty name
      <> maybe "" (\d -> ":" <+> pretty d) (listToMaybe desc)
        <+> tupled (map pretty (Set.toList members))

instance Pretty Symbol where
  pretty (TypeSymbol x) = pretty x
  pretty (TermSymbol x) = pretty x
  pretty (ClassSymbol x) = pretty x

instance Pretty AliasedSymbol where
  pretty (AliasedType x alias)
    | x == alias = pretty x
    | otherwise = pretty x <+> "as" <+> pretty alias
  pretty (AliasedTerm x alias)
    | x == alias = pretty x
    | otherwise = pretty x <+> "as" <+> pretty alias
  pretty (AliasedClass x) = pretty x

instance Pretty ExprI where
  pretty (ExprI i e) = parens (pretty e) <> ":" <> pretty i

instance Pretty Pattern where
  pretty (PatternText s ss) = dquotes $ hcat (pretty s : ["#{}" <> pretty s' | s' <- ss])
  pretty (PatternStruct s) = pretty s
  pretty PatternBracketIndex = ".[i]"
  pretty PatternBracketSlice = ".[i:j:k]"

instance Pretty Selector where
  pretty SelectorEnd = ""
  pretty (SelectorKey (k, s) []) = "." <> pretty k <> pretty s
  pretty (SelectorIdx (i, s) []) = "." <> pretty i <> pretty s
  pretty (SelectorKey r rs) = "." <> tupled ["." <> pretty k <> pretty s | (k, s) <- (r : rs)]
  pretty (SelectorIdx r rs) = "." <> tupled ["." <> pretty i <> pretty s | (i, s) <- (r : rs)]
  pretty (SelectorBracketIndex s) = ".[i]" <> pretty s
  pretty SelectorBracketSlice = ".[i:j:k]"

instance Pretty Expr where
  pretty (PatE pat) = "pattern:" <+> pretty pat
  pretty UniE = "()"
  pretty (ModE v es) = align . vsep $ ("module" <+> pretty v) : map pretty es
  pretty (ClsE (Typeclass constraints cls vs sigs)) =
    "class" <+> consStr <> pretty cls <+> hsep (map pretty vs) <> (align . vsep . map pretty) sigs
    where
      consStr = case constraints of
        [] -> ""
        [c] -> pretty c <+> "=> "
        _ -> tupled (map pretty constraints) <+> "=> "
  pretty (IstE cls ts es) = "instance" <+> pretty cls <+> hsep (map (parens . pretty) ts) <> (align . vsep . map pretty) es
  pretty (EffE lbl esc) = (if esc then "escapable effect" else "effect") <+> pretty lbl
  pretty (TypE (ExprTypeE lang v vs t _ kind)) = case kind of
    TypedefPrimitive ->
      "type" <+> pretty lang
        <> "@"
        <> pretty v
          <+> sep (map (either pretty (parens . pretty)) vs)
    TypedefAlias -> body "type"
    TypedefNewtype -> body "newtype"
    where
      body keyword =
        keyword <+> pretty lang
          <> "@"
          <> pretty v
            <+> sep (map (either pretty (parens . pretty)) vs)
            <+> "="
            <+> pretty t
  pretty (ImpE (Import m Nothing _ _)) = "import" <+> pretty m
  pretty (ImpE (Import m (Just xs) _ _)) = "import" <+> pretty m <+> tupled (map pretty xs)
  pretty (ExpE ExportAll) = "export *"
  pretty (ExpE (ExportMany symbols groups)) =
    "export"
      <+> tupled
        ( map pretty (Set.toList symbols)
            ++ [pretty g | g <- groups]
        )
  pretty (VarE _ s) = pretty s
  pretty (LamE v e) = "\\" <+> pretty v <+> "->" <+> pretty e
  pretty (AnnE e t) = parens (pretty e <+> "::" <+> pretty t)
  pretty (LstE es) = encloseSep "[" "]" "," (map pretty es)
  pretty (TupE es) = encloseSep "[" "]" "," (map pretty es)
  pretty (AppE f es) = vsep (map pretty (f : es))
  pretty (NamE rs) = block 4 "<RECORD>" (vsep [pretty k <+> "::" <+> pretty x | (k, x) <- rs])
  pretty (RealE x) = pretty (showRealLit x)
  pretty (IntE x) = pretty (show x)
  pretty (StrE x) = dquotes (pretty x)
  pretty (LogE x) = pretty x
  pretty (LetE bindings body) = vsep [pretty v <+> "=" <+> pretty e | (v, e) <- bindings] <+> "in" <+> pretty body
  pretty (AssE v e es) = pretty v <+> "=" <+> pretty e <+> "where" <+> (align . vsep . map pretty) es
  pretty (SrcE (Source srcname lang file' alias _ rsizes _ _ _)) =
    "source"
      <+> viaShow lang
      <> maybe "" (\f -> "from" <+> pretty f) file'
        <+> "where\n"
      <> indent
        2
        ( vsep
            [ "--' srcname: " <> pretty srcname
            , "--' rsize: " <> encloseSep "" "" " " (map pretty rsizes)
            , pretty alias
            ]
        )
  pretty (SigE (Signature v _ e)) =
    pretty v <+> "::" <+> pretty e
  pretty (FixE (Fixity assoc prec ops)) =
    assocStr <+> pretty prec <+> hsep (map pretty ops)
    where
      assocStr :: Doc ann
      assocStr = case assoc of
        InfixL -> "infixl"
        InfixR -> "infixr"
        InfixN -> "infix"
  pretty (ParenE e) = parens (pretty e)
  pretty (BopE e1 _ v e2) = pretty e1 <+> pretty v <+> pretty e2
  pretty (IfE c t e) = "if" <+> pretty c <+> "then" <+> pretty t <+> "else" <+> pretty e
  pretty NullE = "Null"
  pretty (DoBlockE e) = "{" <> pretty e <> "}"
  pretty (EvalE e) = "!" <> pretty e
  pretty (IntrinsicE intr args) = "@" <> pretty (intrinsicName intr) <+> hsep (map pretty args)

instance (Foldable f) => Pretty (AnnoS a f b) where
  pretty (AnnoS _ _ e) = pretty e

instance (Foldable f) => Pretty (ExprS a f b) where
  pretty (AppS e es) = "(AppS" <+> list (map pretty (e : es)) <> ")"
  pretty (VarS v res) = "(VarS" <+> pretty v <+> "=" <+> list (map pretty (toList res)) <> ")"
  pretty (LamS vs e) = "(LamS" <+> list (map pretty vs) <+> "->" <+> pretty e <> ")"
  pretty (LstS es) = "(LstS" <+> list (map pretty es) <> ")"
  pretty (TupS es) = "(TupS" <+> list (map pretty es) <> ")"
  pretty (NamS rs) = "(NamS" <+> list [pretty k <> "=" <> pretty v | (k, v) <- rs] <> ")"
  pretty UniS = "UniS"
  pretty NullS = "NullS"
  pretty (BndS x) = "(BndS" <+> pretty x <> ")"
  pretty (RealS _ x) = pretty (showRealLit x)
  pretty (IntS _ x) = viaShow x
  pretty (LogS x) = viaShow x
  pretty (StrS x) = viaShow x
  pretty (ExeS x) = pretty x
  pretty (LetS v e1 e2) = "(LetS" <+> pretty v <+> "=" <+> pretty e1 <+> "in" <+> pretty e2 <> ")"
  pretty (LetBndS x) = "(LetBndS" <+> pretty x <> ")"
  pretty (CallS v) = "(CallS" <+> pretty v <> ")"
  pretty (IfS c t e) = "(IfS" <+> pretty c <+> pretty t <+> pretty e <> ")"
  pretty (DoBlockS e) = "(DoBlockS" <+> pretty e <> ")"
  pretty (EvalS e) = "(EvalS" <+> pretty e <> ")"
  pretty (CoerceS c e) = "(CoerceS" <+> viaShow c <+> pretty e <> ")"
  pretty (IntrinsicS intr es) = "(IntrinsicS" <+> viaShow intr <+> list (map pretty es) <> ")"

instance Pretty ExecutableExpr where
  pretty (SrcCall src) = pretty src
  pretty (PatCall pat) = pretty pat

instance Pretty Signature where
  pretty (Signature v _ e) = pretty v <+> "::" <+> pretty (etype e)
