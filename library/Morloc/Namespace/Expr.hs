{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
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
  , remoteResourceInts
  , ManifoldConfig (..)
  , LogTemplate (..)
  , BenchTemplate (..)
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
  , PatApp (..)
  , PatEvidence (..)
  , noPatEvidence
  , ArgMethod (..)
  , patAppPattern
  , applyPattern
  , patternArity
  , Pattern (..)
  , Intrinsic (..)
  , SaveFormat (..)
  , saveFormatName
  , intrinsicName
  , intrinsicArity
  , WireComparison (..)
  , derivedComparisons
  , InternalApp (..)
  , InternalView (..)
  , viewInternal
  , intrinsicIsIO
  , parseIntrinsic
  , Expr (..)
  , ExprI (..)
  , E (..)
  , PatShape (..)
  , ShapeForm (..)
  , PreTyped (..)
  , Lit (..)
  , RealLit (..)
  , MatchSite (..)
  , SitePat (..)
  , SiteLit (..)
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
  , CastDir (..)
  , InstanceOrigin (..)
  , InstanceKey (..)
  , Impl (..)
  , InstanceDecl (..)
  , ClassInfo (..)
  , InstanceEnv (..)
  , Evidence (..)
  , evidenceInstance
  , applyCoercion
  , ManyPoly (..)
  , Many (..)
  , unMany
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
  , foldExprS
  , foldAnnoS
  , annoNodes

    -- * JSON helpers
  , stripPrefixAndKebabCase
  , convertToKebabCase
  ) where

import Control.Monad.Identity (runIdentity)
import Data.Aeson (FromJSON (..), ToJSON (..), (.!=), (.:?))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Options (..), defaultOptions)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import Data.Scientific (Scientific)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Morloc.Data.Doc
import Morloc.Data.Zip (zipExact)
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
  -- | True when the source name came from a backtick-quoted form
  -- (@`and`@). Backtick names are always treated as operators and
  -- bypass the language-descriptor pattern check in Treeify; the
  -- enclosed text is emitted verbatim between the two args.
  , srcBacktick :: !Bool
  }
  deriving (Ord, Eq, Show)

data RemoteResources = RemoteResources
  { remoteResourcesThreads :: Maybe Int
  , remoteResourcesMemory :: Maybe Int
  , remoteResourcesTime :: Maybe TimeInSeconds
  , remoteResourcesGpus :: Maybe Int
  }
  deriving (Show, Ord, Eq, Generic)

-- | The four `resources_t` fields (memory, walltime seconds, cpus, gpus) with
-- the shared codegen defaults applied: -1 for a missing memory/time/cpu limit,
-- 0 for missing gpus. Single source of the SLURM default policy that every pool
-- member's remote-call codegen renders.
remoteResourceInts :: RemoteResources -> (Int, Int, Int, Int)
remoteResourceInts res =
  ( maybe (-1) id (remoteResourcesMemory res)
  , maybe (-1) unTimeInSeconds (remoteResourcesTime res)
  , maybe (-1) id (remoteResourcesThreads res)
  , maybe 0 id (remoteResourcesGpus res)
  )

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

-- | The shape of one row in the end-of-run benchmark summary. Lives at
-- the top level of the program YAML as @benchmark-template@, beside
-- @log-template@, and is program-wide rather than per-label: the nexus
-- aggregates every label's timings and renders them through one
-- template, so a per-label override would have nothing to apply to.
-- 'Nothing' means \"use the built-in default\"; an explicit null
-- @summary@ alongside @benchmark: true@ is contradictory and rejected.
newtype BenchTemplate = BenchTemplate
  { benchTemplateSummary :: Maybe Text
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
  -- ^ The index of the labeled reference, set when the label is placed
  -- (Treeify). Codegen looks up the source position (for
  -- @{module}@/@{line}@/@{column}@) and the name (for @{name}@) there. Not
  -- user-facing; absent from YAML.
  }
  deriving (Show, Ord, Eq, Generic)

data ModuleConfig = ModuleConfig
  { moduleConfigDefaultGroup :: Maybe ManifoldConfig
  , moduleConfigLabeledGroups :: Map.Map Text ManifoldConfig
  , moduleConfigLogTemplate :: Maybe LogTemplate
  , moduleConfigBenchTemplate :: Maybe BenchTemplate
  -- ^ Top-level YAML field @benchmark-template@: the row shape for the
  -- end-of-run summary emitted for labels carrying @benchmark: true@.
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
  , buildConfigLangParams :: Maybe (Map.Map Text (Map.Map Text Text))
  -- ^ Per-machine default build parameters, keyed lang -> key -> value.
  -- Written under the @lang-params@ key of the build config. Serves as the
  -- layer below the @-X lang:key=value@ command-line overrides.
  }
  deriving (Show, Generic)

---- Expressions

data Symbol
  = TypeSymbol TVar
  | TermSymbol EVar
  | ClassSymbol ClassName
  | CtorSymbol TVar EVar
  -- ^ Records that a term is a constructor of a `data` type, so the
  -- constructors can follow their type through an export or import list
  -- and through any module that re-exports it. The term itself is still a
  -- 'TermSymbol' (under whatever spelling the importer gave it, which the
  -- second field tracks); this entry is the association only, and every
  -- consumer that binds or links terms ignores it.
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
-- 'applyPattern' to check a getter's arguments: they are @bracketArity sel
-- + 1@ (the +1 is the receiver).
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
  -- ^ Python-style bracket index (xs[i]). Carries no payload; its arguments
  -- are the index and the receiver ('PatIndex').
  -- The typechecker synthesizes the result type as
  -- the receiver's element type. The codegen translator emits the
  -- appropriate native indexing operation based on the receiver type.
  | PatternBracketSlice
  -- ^ Python-style bracket slice (xs[i:j:k]). Carries no payload; the
  -- three bound expressions (start, stop, step; each Null or an Int64
  -- after toIndex wrapping) and the receiver are its arguments
  -- ('PatSlice'). The typechecker synthesizes
  -- the result type structurally: a Nat-parameterized container has
  -- its outer Nat replaced by NatUnknown; otherwise the receiver type
  -- is preserved. The codegen translator emits the appropriate native
  -- slicing operation.
  deriving (Show, Ord, Eq)

-- | A pattern with the values it is applied to, in the order they are
-- evaluated. Elsewhere a pattern used as a function is a lambda around one
-- of these.
data PatApp a
  = -- | Text, then each interpolated value and the text after it.
    PatText Text [(a, Text)]
  | -- | The bracket bounds in depth-first order, then the receiver.
    PatGet Selector [a] a
  | -- | The receiver, then a new value for each leaf of the selector.
    PatSet Selector a (NonEmpty a)
  | -- | The index, then the receiver.
    PatIndex a a
  | -- | The start, stop and step, then the receiver.
    PatSlice a a a a
  deriving (Show, Ord, Eq, Functor, Foldable, Traversable)

-- | The pattern a 'PatApp' applies.
patAppPattern :: PatApp a -> Pattern
patAppPattern app = case app of
  PatText s xs -> PatternText s (map snd xs)
  PatGet s _ _ -> PatternStruct s
  PatSet s _ _ -> PatternStruct s
  PatIndex _ _ -> PatternBracketIndex
  PatSlice {} -> PatternBracketSlice

-- | A pattern applied to arguments, if they are the arguments it takes. Any
-- argument beyond a getter's receiver is a value a setter stores.
applyPattern :: Pattern -> [a] -> Maybe (PatApp a)
applyPattern pat args = case (pat, args) of
  (PatternText s ss, _) -> PatText s <$> zipExact args ss
  (PatternStruct s, _) | selectorHasBracket s -> case splitAt (bracketArity s) args of
    (bounds, [rcv]) | length bounds == bracketArity s -> Just (PatGet s bounds rcv)
    _ -> Nothing
  (PatternStruct s, [rcv]) -> Just (PatGet s [] rcv)
  (PatternStruct s, rcv : v : vs) -> Just (PatSet s rcv (v :| vs))
  (PatternBracketIndex, [i, rcv]) -> Just (PatIndex i rcv)
  (PatternBracketSlice, [start, stop, step, rcv]) -> Just (PatSlice start stop step rcv)
  _ -> Nothing

-- | The number of arguments a pattern used as a function takes. A setter is
-- only ever written applied to its values, so a selector is a getter.
patternArity :: Pattern -> Int
patternArity (PatternText _ ss) = length ss
patternArity (PatternStruct s) = bracketArity s + 1
patternArity PatternBracketIndex = 2
patternArity PatternBracketSlice = 4

-- | The file format a save intrinsic writes.
data SaveFormat
  = SaveVoidstar -- ^ @save: a voidstar data packet
  | SaveJson     -- ^ @savej: a JSON text file
  deriving (Show, Eq, Ord)

-- | The format's name in generated code and the nexus manifest.
saveFormatName :: SaveFormat -> Text
saveFormatName SaveVoidstar = "voidstar"
saveFormatName SaveJson = "json"

-- | Compiler intrinsics: functions the compiler generates specialized code for.
data Intrinsic
  = IntrSave      -- ^ @save  :: Int -> Str -> a -> <IO> (Try Str ()) -- voidstar packet, with zstd level 0-9
  | IntrSaveJ     -- ^ @savej :: Str -> a -> <IO> (Try Str ())    -- raw JSON file
  | IntrLoad      -- ^ @load  :: Str -> <IO> (Try Str a)          -- auto-detect format, auto-decompress packets; failure is an Err arm
  | IntrHash      -- ^ @hash   :: a -> Str           -- xxhash, hex string
  | IntrEq        -- ^ @eq     :: a -> a -> Bool     -- equal wire values (INTR-7)
  | IntrLe        -- ^ @le     :: a -> a -> Bool     -- wire order (INTR-7)
  | IntrVersion   -- ^ @version :: Str               -- compiler version
  | IntrCompiled  -- ^ @compiled :: Str              -- compile timestamp
  | IntrLang      -- ^ @lang    :: Str               -- current pool language
  | IntrSchema    -- ^ @schema  :: a -> Str          -- schema string
  | IntrTypeof    -- ^ @typeof  :: a -> Str          -- concrete type name
  | IntrShow      -- ^ @show   :: a -> Str           -- serialize to JSON string
  | IntrRead      -- ^ @read   :: Str -> Try Str a   -- deserialize from JSON string; pure, so it composes in `map`
  | IntrDatafile  -- ^ @datafile :: Str -> Str       -- resolve installed data file path
  | IntrUnpack    -- ^ @unpack :: [U8] -> <IO> a     -- decode a morloc packet held as
                  -- bytes, as the pool decodes an argument. NOT user-facing: the
                  -- entries synthesized for `@parse` receive an argument that no
                  -- parser read as the packet the nexus loaded, and decode it here.
                  -- `a` is resolved by ascription.
  | IntrOpen      -- ^ @open  :: Str -> <IO> (Try Str a)  -- open a stream/file; `a` resolved via inline ascription to IFile/IStream/OStream
  | IntrClose     -- ^ @close :: a -> <IO> ()        -- close any stream/file handle
  | IntrFSchema   -- ^ @fschema :: Str -> <IO> (Try Str Str) -- read a file's element schema without typed open
  | IntrMap       -- ^ implicit @(a -> b) -> List a -> List b@ map; emitted by
                  -- the desugar's bracket-accessor lowering when a slice is
                  -- followed by a chained accessor (e.g. @.[::-1].x pts@). NOT
                  -- a user-facing @\@map@ -- there is no entry in 'parseIntrinsic'.
                  -- Instance resolution makes it a use of @Functor.map@ where
                  -- an instance implements it; otherwise the pure-runtime
                  -- evaluator executes it as a direct per-element loop over
                  -- MORLOC_ARRAY. A fused walk of an IFile keeps it.
  | IntrMapOptional -- ^ internal @(a -> b) -> ?a -> ?b@: apply a function to
                  -- an optional's value, if it has one. Emitted only by the
                  -- host adapter ('Morloc.CodeGenerator.EffectBoundary'),
                  -- which rebuilds an optional holding a function value in
                  -- the convention of the side it crosses to. NOT user-facing.
  | IntrNewtypeUnwrap -- ^ implicit @N vs -> T@ representation cast, emitted
                  -- only in derived @unpack@ bodies (NEWT-7). NOT user-facing;
                  -- the typechecker replaces it with a 'CoerceNewtype'.
  | IntrNewtypeWrap -- ^ implicit @T -> N vs@, its inverse, in derived @pack@
                  -- bodies (NEWT-8).
  | IntrTagTest   -- ^ implicit @a -> a -> Bool@ tag test, emitted only by the
                  -- desugar's constructor-pattern lowering. NOT user-facing --
                  -- there is no entry in 'parseIntrinsic', as with 'IntrMap'.
                  --
                  -- It asks "does this value carry this constructor's tag",
                  -- which is deliberately NOT equality. For an argument-free
                  -- constructor the two coincide and every backend lowers it
                  -- to its native @==@. Once constructors carry arguments they
                  -- stop coinciding: equality would compare payloads, while a
                  -- pattern must test the discriminant alone. Naming it a tag
                  -- test means that later change is confined to this lowering.
                  --
                  -- It is also why constructor patterns do not wait on `Eq`:
                  -- an internal primitive has no surface syntax, so the
                  -- wire-level derivation work can replace it with nothing to
                  -- migrate.
  | IntrCtorField -- ^ implicit field projection out of a matched `data`
                  -- value: @subject -> constructor -> index -> field@.
                  -- Emitted only by the desugar's constructor-pattern
                  -- lowering, and deliberately NOT reachable from surface
                  -- syntax -- a getter aimed at a `data` type is rejected,
                  -- because which fields exist depends on the constructor.
                  -- Here the constructor is known: the tag test guarding
                  -- this projection has already established the arm, and
                  -- short-circuiting in the emitted condition keeps the
                  -- projection from running against any other one.
  | IntrListLenTest -- ^ implicit @List a -> Int -> Bool@: does the list have
                  -- exactly this many elements. Emitted only by the
                  -- desugar's list-pattern lowering, with a literal length,
                  -- and NOT reachable from surface syntax. A primitive
                  -- rather than a call to a size function so that matching
                  -- needs no instance in scope.
  | IntrListElem  -- ^ implicit @List a -> Int -> a@: the element at a
                  -- literal index. Emitted only by the desugar's
                  -- list-pattern lowering, under a passing 'IntrListLenTest'
                  -- that puts the index in range.
  | IntrFLength     -- ^ @flen :: IFile a -> <IO> (Try Str Int)@ -- file element count.
                    -- Free from the footer's StreamDiag.element_count. Users
                    -- typically alias as @length@ via stdlib shims.
  | IntrStreamLayout -- ^ @streamLayout :: IFile [a] -> <IO> (Try Str [(U64,U64,U64)])@
                    -- -- per-sub-packet layout for parallel planning: one triple
                    -- @(elementOffset, elementCount, uncompressedSize)@ per
                    -- sub-packet, in file order. A DATA packet is the degenerate
                    -- single-chunk case (one triple @(0, count, size)@); an empty
                    -- stream yields @[]@. Derived at read time from the existing
                    -- sub-packet index + per-sub-packet headers (no wire change,
                    -- no payload decompression). A malformed packet is an Err arm.
  | IntrNext        -- ^ @next :: IStream a -> <IO> (Try Str [a])@ -- materialise the
                    -- current sub-packet and advance the cursor. Returns an
                    -- empty list at EOF (further calls keep returning empty).
                    -- A mid-stream decode failure is an Err arm.
  | IntrStream      -- ^ @stream :: IFile [a] -> <IO> IStream a@ -- derive a
                    -- forward-only IStream from an open IFile, bound to the
                    -- same path with an independent fd, mmap, and cursor.
  | IntrWrite       -- ^ @write :: Int -> OStream a -> [a] -> <IO> (Try Str ())@ --
                    -- emit one sub-packet of element-list type. The Int is
                    -- the zstd compression level (0 = uncompressed); the
                    -- first @write fixes the level for the file's lifetime.
                    -- I/O failure (disk full, broken pipe) is an Err arm.
  | IntrAppend      -- ^ @append :: Str -> <IO> (Try Str (OStream a))@ -- open an
                    -- existing stream file for append. Forward-scan recovers
                    -- the resume cursor; a mismatched element schema is an Err arm
                    -- before any bytes are written.
  | IntrConcat      -- ^ @concat :: [Str] -> Str -> <IO> (Try Str ())@ -- concatenate
                    -- a sequence of stream files via sendfile, exploiting
                    -- the stream-packet concat invariant.
  | IntrFlush       -- ^ @flush :: OStream a -> <IO> (Try Str ())@ -- force buffered
                    -- writes to be emitted as a sub-packet immediately,
                    -- without closing the stream. No-op on an empty
                    -- buffer. Useful for tests that need deterministic
                    -- packet boundaries and for user code that wants to
                    -- make progress visible to concurrent readers.
  | IntrStdin       -- ^ @stdin :: <IO> (Try Str (IStream a))@ -- nullary intrinsic
                    -- that opens process stdin as an IStream. The nexus is
                    -- the sole owner of fd 0; @next routes through the
                    -- pool-nexus RPC socket. At most one @stdin per nexus
                    -- (the second open gives an Err arm via the CAS-per-kind
                    -- guard). Read-time failures (EOF, malformed packet)
                    -- surface at @next, whose result carries them as an Err arm.
  | IntrStdout      -- ^ @stdout :: <IO> OStream a@ -- nullary; opens
                    -- process stdout as an OStream. @write routes through
                    -- the nexus. At most one @stdout per nexus.
  | IntrStderr      -- ^ @stderr :: <IO> OStream a@ -- symmetric with
                    -- @stdout for stderr.
  | IntrThrow       -- ^ @throw :: e -> a@ -- abandon the computation,
                    -- rendering the payload into the traceback. A bottom
                    -- rather than a tracked effect: it does not return, so
                    -- there is nothing for an effect row to describe. The
                    -- return type is a fresh existential so `@throw` fits
                    -- in any branch.
  | IntrTry         -- ^ @try :: <e> a -> <e> (Try Str a)@ -- run the
                    -- argument and convert an otherwise-uncaught failure
                    -- into data. The argument is always suspended, as a
                    -- property of the form rather than of its type, so a
                    -- pure body works too. The error is a rendered message
                    -- plus traceback: a body may fail in several unrelated
                    -- ways and those have no common type. The body's own
                    -- effects pass through.
  | IntrTell        -- ^ @tell :: <IO> U64@ -- the number of elements written
                    -- to the process's @stdout OStream so far (its
                    -- element_count). Used by the offset-aware `@with`/
                    -- `@render` synthesis to thread the per-batch element
                    -- offset into a handler; also user-callable.
  | IntrCollect     -- ^ @collect :: (([a] -> <IO,e> ()) -> <IO,e> ()) -> <IO,e> ()@
                    -- Provide the default stdout sink to a producer body and
                    -- manage its lifecycle. Desugar-expands to
                    -- `do { o <- @stdout; body (@write 0 o); @close o }`. The
                    -- sink type `[a] -> ...` carries the stream element type,
                    -- which the `@with`/`@render` handler synthesis reads off
                    -- to type the flag handlers.
  | IntrShape       -- ^ a destructuring pattern's shape on its receiver, as
                    -- Desugar writes it: [skeleton, receiver]. Treeify turns
                    -- it into 'ShapeS'.
  | IntrTmpfile     -- ^ @tmpfile :: <IO> Str@ -- create a fresh empty file in
                    -- the morloc tmpdir, register it for removal when the pool
                    -- call ends, and return its path. NOT user-facing;
                    -- synthesized by the whole-form `@with`/`@render` handler
                    -- to gather a stream to disk before applying the handler.
  | IntrCellNew     -- ^ @cellnew :: b -> <IO> (Cell b)@ -- create a fold
                    -- accumulator seeded with the given value. NOT
                    -- user-facing; synthesized by the `@fold` handler form.
  | IntrCellGet     -- ^ @cellget :: Cell b -> <IO> b@ -- read the calling
                    -- thread's accumulator, or the seed if that thread has
                    -- not folded yet.
  | IntrCellPut     -- ^ @cellput :: Cell b -> b -> <IO> ()@ -- replace the
                    -- calling thread's accumulator. Thread-local, so a
                    -- producer driving its sink from several threads loses
                    -- no updates and needs no lock.
  | IntrCellReduce  -- ^ @cellreduce :: (b -> b -> b) -> Cell b -> <IO> b@ --
                    -- merge every thread's accumulator with the handler's
                    -- `combine` and release the cell. Never sees an empty
                    -- cell: one that was never folded into answers with its
                    -- seed, which is what an empty stream folds to.
  | IntrReplay      -- ^ @replay :: IStream a -> ([a] -> <IO> ()) -> <IO> ()@ --
                    -- call the function on every frame (sub-packet) of the
                    -- stream, in order, empty frames included. Synthesized
                    -- only (a replayed terminal action's producer), never
                    -- written by users.
  | IntrChannel     -- ^ internal @channel :: <IO> (IStream a), synthesized by
                    -- `@parse`: a stream whose producer and readers run at
                    -- once through shared memory. The one handle is the
                    -- producer's OStream too (see IntrSpawn).
  | IntrSpawn       -- ^ internal @spawn :: IStream a -> (OStream a -> <IO> ())
                    -- -> <IO> (): start the producer on the channel as a call
                    -- of its own, without waiting for it.
  | IntrSettle      -- ^ internal @settle :: IStream a -> <IO> (): end a
                    -- channel once its readers are done; a producer failure
                    -- a reader was handed is raised again here.
  | IntrConform     -- ^ internal @conform :: a -> a, synthesized by Express.hs
                    -- around the wire form of a Packable literal: gives it
                    -- the form the pool's decoder gives a received value of
                    -- its schema (NATIVE-8). Python and R only.
  | IntrCellSlots   -- ^ internal @cellslots :: Cell b -> <IO> [b]: every
                    -- accumulator of a cell, in its wire form, and the cell
                    -- released. With IntrReduceList it is how @cellreduce
                    -- runs when a packer builds the accumulator's native form.
  | IntrReduceList  -- ^ internal @reducelist :: (b -> b -> b) -> [b] -> b,
                    -- over a non-empty list.
  | IntrIFileWalk   -- ^ Unified IFile pattern walker. Synthesized by Express.hs
                    -- and Nexus.hs from any pattern application with an IFile
                    -- receiver (`.[i] f`, `.[s:e:p] f`, `.foo.bar f`, mixed
                    -- chains). The selector chain is encoded as a path string
                    -- (`.1.[]`, `.foo.[:]`, etc.); bracket steps consume
                    -- runtime args in DFS order. Lowers to a single per-
                    -- language @_mlc_ifile_walk@ call. NOT user-facing; its
                    -- argument count varies (path + handle + 0..n bracket
                    -- bounds), so it has no fixed arity.
  deriving (Show, Ord, Eq, Enum, Bounded)

-- | A derived comparison method over its two parameters (DERIV-2).
data WireComparison
  = -- | the intrinsic applied to the parameters, swapped if True
    WireCall Intrinsic Bool
  | WireNot WireComparison
  deriving (Show, Eq)

-- | The methods a derived @Eq@ or @Ord@ instance gives.
derivedComparisons :: Text -> Maybe [(Text, WireComparison)]
derivedComparisons "Eq" = Just [("==", WireCall IntrEq False), ("!=", WireNot (WireCall IntrEq False))]
derivedComparisons "Ord" =
  Just
    [ ("<=", WireCall IntrLe False)
    , (">=", WireCall IntrLe True)
    , -- the wire order is total (DERIV-3)
      ("<", WireNot (WireCall IntrLe True))
    , (">", WireNot (WireCall IntrLe False))
    ]
derivedComparisons _ = Nothing

-- | Map intrinsic to its canonical name
-- | Everything known of an intrinsic before typechecking: its name, its
-- arity (none for one whose argument count varies), whether it performs IO (its type carries an @IO@ effect; the eval
-- sandbox forbids the IO ones written directly), and whether a program may
-- write it as @name. One equation per constructor, so a new intrinsic
-- without a row is an incomplete-pattern warning.
data IntrSig = IntrSig
  { sigName :: Text
  , sigArity :: Maybe Int
  , sigIO :: Bool
  , sigWritten :: Bool
  }

intrinsicSig :: Intrinsic -> IntrSig
intrinsicSig IntrSave = IntrSig "save" (Just 3) True True
intrinsicSig IntrSaveJ = IntrSig "savej" (Just 2) True True
intrinsicSig IntrLoad = IntrSig "load" (Just 1) True True
intrinsicSig IntrHash = IntrSig "hash" (Just 1) False True
intrinsicSig IntrEq = IntrSig "eq" (Just 2) False True
intrinsicSig IntrLe = IntrSig "le" (Just 2) False True
intrinsicSig IntrVersion = IntrSig "version" (Just 0) False True
intrinsicSig IntrCompiled = IntrSig "compiled" (Just 0) False True
intrinsicSig IntrLang = IntrSig "lang" (Just 0) False True
intrinsicSig IntrSchema = IntrSig "schema" (Just 1) False True
intrinsicSig IntrTypeof = IntrSig "typeof" (Just 1) False True
intrinsicSig IntrShow = IntrSig "show" (Just 1) False True
intrinsicSig IntrRead = IntrSig "read" (Just 1) False True
intrinsicSig IntrDatafile = IntrSig "datafile" (Just 1) False True
intrinsicSig IntrUnpack = IntrSig "unpack" (Just 1) True False
intrinsicSig IntrOpen = IntrSig "open" (Just 1) True True
intrinsicSig IntrClose = IntrSig "close" (Just 1) True True
intrinsicSig IntrFSchema = IntrSig "fschema" (Just 1) True True
intrinsicSig IntrMap = IntrSig "map" (Just 2) False False
intrinsicSig IntrMapOptional = IntrSig "mapoptional" (Just 2) False False
intrinsicSig IntrTagTest = IntrSig "tagtest" (Just 2) False False
intrinsicSig IntrNewtypeUnwrap = IntrSig "unwrap" (Just 1) False False
intrinsicSig IntrNewtypeWrap = IntrSig "wrap" (Just 1) False False
intrinsicSig IntrCtorField = IntrSig "ctorfield" (Just 3) False False
intrinsicSig IntrListLenTest = IntrSig "listlentest" (Just 2) False False
intrinsicSig IntrListElem = IntrSig "listelem" (Just 2) False False
intrinsicSig IntrFLength = IntrSig "flen" (Just 1) True True
intrinsicSig IntrStreamLayout = IntrSig "streamLayout" (Just 1) True True
intrinsicSig IntrNext = IntrSig "next" (Just 1) True True
intrinsicSig IntrStream = IntrSig "stream" (Just 1) True True
intrinsicSig IntrWrite = IntrSig "write" (Just 3) True True
intrinsicSig IntrAppend = IntrSig "append" (Just 1) True True
intrinsicSig IntrConcat = IntrSig "concat" (Just 2) True True
intrinsicSig IntrFlush = IntrSig "flush" (Just 1) True True
intrinsicSig IntrStdin = IntrSig "stdin" (Just 0) True True
intrinsicSig IntrStdout = IntrSig "stdout" (Just 0) True True
intrinsicSig IntrStderr = IntrSig "stderr" (Just 0) True True
intrinsicSig IntrThrow = IntrSig "throw" (Just 1) False True
intrinsicSig IntrTry = IntrSig "try" (Just 1) False True
intrinsicSig IntrTell = IntrSig "tell" (Just 0) True True
intrinsicSig IntrCollect = IntrSig "collect" (Just 1) True True
intrinsicSig IntrShape = IntrSig "shape" (Just 2) False False
intrinsicSig IntrTmpfile = IntrSig "tmpfile" (Just 0) True False
intrinsicSig IntrCellNew = IntrSig "cellnew" (Just 1) True False
intrinsicSig IntrCellGet = IntrSig "cellget" (Just 1) True False
intrinsicSig IntrCellPut = IntrSig "cellput" (Just 2) True False
intrinsicSig IntrCellReduce = IntrSig "cellreduce" (Just 2) True False
intrinsicSig IntrReplay = IntrSig "replay" (Just 2) True False
intrinsicSig IntrChannel = IntrSig "channel" (Just 0) True False
intrinsicSig IntrSpawn = IntrSig "spawn" (Just 2) True False
intrinsicSig IntrSettle = IntrSig "settle" (Just 1) True False
intrinsicSig IntrIFileWalk = IntrSig "ifile_walk" Nothing True False
intrinsicSig IntrConform = IntrSig "conform" (Just 1) False False
intrinsicSig IntrCellSlots = IntrSig "cellslots" (Just 1) True False
intrinsicSig IntrReduceList = IntrSig "reducelist" (Just 2) False False

intrinsicName :: Intrinsic -> Text
intrinsicName = sigName . intrinsicSig

intrinsicIsIO :: Intrinsic -> Bool
intrinsicIsIO = sigIO . intrinsicSig

intrinsicArity :: Intrinsic -> Maybe Int
intrinsicArity = sigArity . intrinsicSig

parseIntrinsic :: Text -> Maybe Intrinsic
parseIntrinsic t = lookup t [(sigName (intrinsicSig x), x) | x <- [minBound .. maxBound], sigWritten (intrinsicSig x)]

-- | An intrinsic only the compiler writes, with its fixed arguments; a
-- literal argument a consumer rebuilds is kept beside its value.
data InternalApp e
  = TagTest e e Text
  | CtorField e e Text e Int
  | ListLenTest e e Int
  | ListElem e e

-- | What an intrinsic application is, as an internal intrinsic.
data InternalView e
  = NotInternal
  | Malformed
  | Internal (InternalApp e)

-- | Read an internal intrinsic's fixed arguments.
viewInternal :: Intrinsic -> [AnnoS g f c] -> InternalView (AnnoS g f c)
viewInternal intr args = case intr of
  IntrTagTest -> case args of
    [s, n@(AnnoS _ _ (StrS t))] -> Internal (TagTest s n t)
    _ -> Malformed
  IntrCtorField -> case args of
    [s, n@(AnnoS _ _ (StrS t)), k@(AnnoS _ _ (IntS _ i))]
      | i >= 0 -> Internal (CtorField s n t k (fromIntegral i))
    _ -> Malformed
  IntrListLenTest -> case args of
    [s, k@(AnnoS _ _ (IntS _ i))] | i >= 0 -> Internal (ListLenTest s k (fromIntegral i))
    _ -> Malformed
  IntrListElem -> case args of
    [s, k@(AnnoS _ _ (IntS _ i))] | i >= 0 -> Internal (ListElem s k)
    _ -> Malformed
  _ -> NotInternal

-- | Expected number of arguments for each intrinsic

data ExprI = ExprI Int Expr
  deriving (Show, Ord, Eq)

data Expr
  = ModE MVar [ExprI]
  | ClsE (Typeclass Signature)
  | IstE InstanceOrigin ClassName [Constraint] [TypeU] [ExprI]
  -- ^ origin, class, the instance's context (constraints its body may
  -- assume), instance types, body
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
  -- | A `data` constructor applied to its arguments: the type, the
  -- constructor's name, its 0-based declaration ordinal, and the argument
  -- expressions. The ordinal is the wire tag, fixed at the declaration
  -- rather than recomputed.
  --
  -- The argument list is empty for a constructor that takes none. Whether
  -- the value is a byte or a tagged pointer is decided by the TYPE, not by
  -- this constructor's own arity: a type is an enum only when every one of
  -- its constructors is argument-free.
  | ConE TVar Text Int [ExprI]
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

-- | A pattern match: where it is written, and each clause's position with
-- one pattern per receiver, each at its own position.
data MatchSite = MatchSite
  { siteIndex :: Int
  , siteRows :: [(Int, [(Int, SitePat)])]
  , siteTotal :: Bool
  -- ^ whether every value must be matched; a refutable `<-` bind throws
  -- on a value it does not match
  }
  deriving (Show, Eq)

-- | A pattern as written: variables are 'SitePatWild'.
data SitePat
  = SitePatWild
  | SitePatLit SiteLit
  | SitePatCon Text [SitePat]
  | SitePatTuple [SitePat]
  | SitePatRecord [(Key, SitePat)]
  | SitePatList [SitePat]
  deriving (Show, Eq, Ord)

data SiteLit
  = SiteInt Integer
  | SiteReal RealLit
  | SiteStr Text
  | SiteBool Bool
  deriving (Show, Eq, Ord)

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
  -- | A `data` constructor applied to its fields.
  | ConP (Indexed Type) Text [E]
  deriving (Ord, Eq, Show)

-- | Coercion tag for implicit type conversions inserted by the typechecker.
-- Extensible: future coercions (e.g., numeric widening) add constructors here.
data Coercion
  = CoerceToOptional
  -- | Between a newtype and its representation (NEWT-7, NEWT-8), to the
  -- type it carries.
  | CoerceNewtype CastDir TypeU
  deriving (Show, Eq, Ord)

data CastDir = Unwrap | Wrap
  deriving (Show, Eq, Ord)

-- | Whether an instance was written in the program or derived by the
-- compiler from a declaration.
data InstanceOrigin = Declared | Derived
  deriving (Show, Eq, Ord)

-- | An instance's identity: its class and its head in 'canonicalHead' form,
-- so that alpha-equivalent heads are one key.
data InstanceKey = InstanceKey
  { ikClass :: ClassName
  , ikHead :: ([TVar], [TypeU])
  }
  deriving (Show, Eq, Ord)

-- | An implementation an instance gives for a method.
data Impl
  = ImplSource MVar (Indexed Source)
  | -- | The method's type in its own declaration's variable names.
    ImplBody EType ExprI
  | -- | A derived @pack@ or @unpack@, a representation cast (NEWT-7).
    ImplCast EType ExprI
  deriving (Show, Eq, Ord)

-- | One instance, merged across every declaration of its key (CLS-2).
data InstanceDecl = InstanceDecl
  { idKey :: InstanceKey
  , idContext :: [Constraint]
  -- ^ written in the key's variable names; a variable the head lacks is an
  -- unsolved existential
  , idMethods :: Map.Map EVar (EType, [Impl])
  -- ^ each method the instance declares, at the instance's types in the
  -- variable names of its latest declaration, with its implementations
  , idSites :: [Int]
  , idOrigin :: InstanceOrigin
  }
  deriving (Show, Eq, Ord)

data ClassInfo = ClassInfo
  { ciVars :: [TVar]
  , ciSupers :: [Constraint]
  , ciMethods :: Map.Map EVar EType
  , ciModule :: MVar
  -- ^ the module that declares the class
  }
  deriving (Show, Eq, Ord)

-- | How the classes a pattern calls through are met (CLS-2): the instance
-- that serves its receiver, and the instance casting each bracket bound. Each
-- is absent where nothing in the program serves it: a pool then has no call to
-- make, while the nexus may need none.
data PatEvidence = PatEvidence
  { patReceiver :: Maybe Evidence
  , patBounds :: [Maybe Evidence]
  }
  deriving (Show, Eq, Ord)

-- | A method whose class may serve a value, and the input the value takes.
data ArgMethod = ArgMethod ClassName EVar Int
  deriving (Show, Eq, Ord)

noPatEvidence :: PatEvidence
noPatEvidence = PatEvidence Nothing []

data InstanceEnv = InstanceEnv
  { ieByClass :: Map.Map ClassName [InstanceDecl]
  , ieClasses :: Map.Map ClassName ClassInfo
  , ieMethods :: Map.Map EVar (ClassName, [TVar])
  -- ^ the class each method was declared in, with that class's variables
  , ieFundeps :: Map.Map ClassName [Int]
  -- ^ the class-argument positions the other arguments determine
  }
  deriving (Show, Eq, Ord)

instance Defaultable InstanceEnv where
  defaultValue = InstanceEnv Map.empty Map.empty Map.empty Map.empty

-- | How a constraint at a use is met.
data Evidence
  = -- | The instance, the values of its key's variables, and the evidence
    -- for its context.
    EvInstance InstanceKey [TypeU] [Evidence]
  | -- | No instance: the derived comparison (DERIV-6).
    EvDerived ClassName TypeU
  | -- | A constraint of the signature being checked.
    EvGiven Constraint
  deriving (Show, Eq, Ord)

-- | The instance an evidence names, if it names one.
evidenceInstance :: Evidence -> Maybe InstanceKey
evidenceInstance (EvInstance k _ _) = Just k
evidenceInstance _ = Nothing

-- | Apply a coercion to a type, returning the coerced type.
-- CoerceToOptional widens the RESULT type; under an effect wrapper the
-- result is the inner type, so the OptionalU is placed under the EffectU
-- (yielding @<E> ?T@, never @?(<E> T)@).
applyCoercion :: Coercion -> TypeU -> TypeU
applyCoercion CoerceToOptional (EffectU e t) = EffectU e (OptionalU t)
applyCoercion CoerceToOptional t = OptionalU t
applyCoercion (CoerceNewtype _ t) _ = t


-- | A call of a sourced function carries the index the source is declared
-- under, which names the term it implements and so its signature.
data ExecutableExpr = SrcCall (Indexed Source)
  deriving (Ord, Eq, Show)

data AnnoS g f c = AnnoS g c (ExprS g f c)

data ExprS g f c
  = UniS
  | NullS
  | BndS EVar
  | VarS EVar (f (AnnoS g f c))
  | AppS (AnnoS g f c) [AnnoS g f c]
  -- | A pattern applied to its arguments. The first annotations are the
  -- pattern's own, as a function of those arguments.
  | PatS g c PatEvidence (PatApp (AnnoS g f c))
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
  -- | A `data` constructor applied to its arguments: type, name, 0-based
  -- ordinal, argument expressions (empty when it takes none).
  | ConS TVar Text Int [AnnoS g f c]
  | ExeS ExecutableExpr
  | LetS EVar (AnnoS g f c) (AnnoS g f c)
  | LetBndS EVar
  | CallS EVar  -- recursive call back-edge
  | IfS (AnnoS g f c) (AnnoS g f c) (AnnoS g f c)
  | DoBlockS (AnnoS g f c)
  | EvalS (AnnoS g f c)
  | CoerceS Coercion (AnnoS g f c)
  | IntrinsicS Intrinsic [AnnoS g f c]
  -- | A labeled function (@lab\@h@), or a labeled call once applied. The
  -- index keys the label's configuration in 'stateManifoldConfig'; it names
  -- no node, so no pass renumbers it.
  | LabelS Int (AnnoS g f c)
  -- | A destructuring pattern's shape on the value it destructures (PAT-1);
  -- the node's value is that value. Written by Treeify from 'IntrShape';
  -- typed, then erased: only a pretyped tree can hold one.
  | ShapeS !(PreTyped f) PatShape (AnnoS g f c)

-- | The shape a destructuring pattern demands of its value: anything, or the
-- form of the (sub)pattern at this index.
data PatShape
  = ShapeAny
  | Shape Int ShapeForm
  deriving (Show, Eq, Ord)

-- | A tuple of exactly these components, or a record with at least these
-- fields.
data ShapeForm
  = FormTuple [PatShape]
  | FormRecord [(Key, PatShape)]
  deriving (Show, Eq, Ord)

-- | Evidence that a tree has not yet been through instance resolution.
data PreTyped f where
  PreTyped :: PreTyped ManyPoly

-- | A use before instance resolution: a term's implementations, or a class
-- method's candidate instances, each with its method type.
data ManyPoly a = MonomorphicExpr (Maybe EType) [a] | PolymorphicExpr ClassName EVar EType [(InstanceKey, EType, [a])]
  deriving (Show, Eq, Ord)

-- | A use after instance resolution: how its class constraints are met, and
-- its implementations, each a method's with the instance it was selected
-- for (CLS-2).
data Many a = Many {manyEvidence :: [Evidence], manyAlts :: [(Maybe InstanceKey, a)]}
  deriving (Show)

unMany :: Many a -> [a]
unMany = map snd . manyAlts

instance Functor Many where
  fmap f (Many ev xs) = Many ev [(k, f x) | (k, x) <- xs]

instance Traversable Many where
  traverse f (Many ev xs) = Many ev <$> traverse (traverse f) xs

instance Foldable Many where
  foldr f b = foldr f b . unMany

instance (Pretty a) => Pretty (Many a) where
  pretty = list . map pretty . unMany

---- Class instances

instance HasOneLanguage Source where
  langOf s = Just (srcLang s)
  langOf' s = srcLang s

instance Functor ManyPoly where
  fmap f (MonomorphicExpr t xs) = MonomorphicExpr t (map f xs)
  fmap f (PolymorphicExpr cls v t xs) = PolymorphicExpr cls v t [(k, et, map f es) | (k, et, es) <- xs]

instance Traversable ManyPoly where
  traverse f (MonomorphicExpr t xs) = MonomorphicExpr t <$> traverse f xs
  traverse f (PolymorphicExpr cls v t xs) = PolymorphicExpr cls v t <$> traverse f2 xs
    where
      f2 (k, t', x) = (,,) k t' <$> traverse f x

instance Foldable ManyPoly where
  foldr f b (MonomorphicExpr _ xs) = foldr f b xs
  foldr f b (PolymorphicExpr _ _ _ xs) = foldr f b [x | (_, _, es) <- xs, x <- es]

instance Defaultable ModuleConfig where
  defaultValue =
    ModuleConfig
      { moduleConfigDefaultGroup = Nothing
      , moduleConfigLabeledGroups = Map.empty
      , moduleConfigLogTemplate = Nothing
      , moduleConfigBenchTemplate = Nothing
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
      , buildConfigLangParams = Nothing
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
      <*> o .:? "benchmark-template"
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

instance FromJSON BenchTemplate where
  parseJSON =
    Aeson.genericParseJSON $
      defaultOptions {fieldLabelModifier = stripPrefixAndKebabCase "benchTemplate"}

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

-- | Serialize the build config back to YAML/JSON. Nothing-valued fields are
-- omitted so a round-trip write only records fields that are actually set.
instance ToJSON BuildConfig where
  toJSON =
    Aeson.genericToJSON $
      defaultOptions
        { fieldLabelModifier = stripPrefixAndKebabCase "buildConfig"
        , omitNothingFields = True
        }

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
mapExprSM f (PatS g c ev app) = do
  AnnoS g' c' _ <- f (AnnoS g c UniS)
  PatS g' c' ev <$> traverse f app
mapExprSM f (LamS vs x) = LamS vs <$> f x
mapExprSM f (ShapeS w sh r) = ShapeS w sh <$> f r
mapExprSM f (LstS xs) = LstS <$> mapM f xs
mapExprSM f (TupS xs) = TupS <$> mapM f xs
mapExprSM f (NamS rs) = NamS <$> mapM (secondM f) rs
mapExprSM _ UniS = return UniS
mapExprSM _ NullS = return NullS
mapExprSM _ (BndS v) = return $ BndS v
mapExprSM f (ConS tv n i xs) = ConS tv n i <$> mapM f xs
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
mapExprSM f (LabelS k e) = LabelS k <$> f e

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

-- | Fold a monoid over every immediate child 'AnnoS' of an 'ExprS'.
foldExprS :: (Foldable f, Monoid m) => (AnnoS g f c -> m) -> ExprS g f c -> m
foldExprS f (VarS _ xs)       = foldMap f xs
foldExprS f (ShapeS _ _ r)    = f r
foldExprS f (AppS x xs)       = f x <> foldMap f xs
foldExprS f (PatS _ _ _ app)  = foldMap f app
foldExprS f (LamS _ x)        = f x
foldExprS f (LstS xs)         = foldMap f xs
foldExprS f (TupS xs)         = foldMap f xs
foldExprS f (NamS rs)         = foldMap (f . snd) rs
foldExprS f (LetS _ e1 e2)    = f e1 <> f e2
foldExprS f (IfS c t e)       = f c <> f t <> f e
foldExprS f (DoBlockS x)      = f x
foldExprS f (EvalS x)         = f x
foldExprS f (CoerceS _ x)     = f x
foldExprS f (LabelS _ x)      = f x
foldExprS f (IntrinsicS _ xs) = foldMap f xs
foldExprS f (ConS _ _ _ xs)   = foldMap f xs
foldExprS _ UniS              = mempty
foldExprS _ NullS             = mempty
foldExprS _ (BndS _)          = mempty
foldExprS _ (RealS _ _)       = mempty
foldExprS _ (IntS _ _)        = mempty
foldExprS _ (LogS _)          = mempty
foldExprS _ (StrS _)          = mempty
foldExprS _ (ExeS _)          = mempty
foldExprS _ (LetBndS _)       = mempty
foldExprS _ (CallS _)         = mempty

-- | Fold a monoid over every 'AnnoS' node in a tree (including the root).
foldAnnoS :: (Foldable f, Monoid m) => (AnnoS g f c -> m) -> AnnoS g f c -> m
foldAnnoS f a@(AnnoS _ _ e) = f a <> foldExprS (foldAnnoS f) e

-- | Every node of a tree, root first.
annoNodes :: Foldable f => AnnoS g f c -> [AnnoS g f c]
annoNodes t = appEndo (foldAnnoS (\n -> Endo (n :)) t) []

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
  pretty (ConP _ c args) = hsep (pretty c : map pretty args)

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
  pretty (CtorSymbol t c) = pretty t <> "(" <> pretty c <> ")"

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
  pretty (IstE _ cls _ ts es) = "instance" <+> pretty cls <+> hsep (map (parens . pretty) ts) <> (align . vsep . map pretty) es
  pretty (EffE lbl esc) = (if esc then "escapable effect" else "effect") <+> pretty lbl
  pretty (TypE (ExprTypeE lang v vs t _ kind)) = case kind of
    TypedefPrimitive ->
      "type" <+> pretty lang
        <> "@"
        <> pretty v
          <+> sep (map (either pretty (parens . pretty)) vs)
    TypedefAlias -> body "type"
    TypedefNewtype -> body "newtype"
    TypedefEnum -> body "data"
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
  pretty (ConE tv n _ xs) = pretty tv <> "." <> pretty n <> tupled (map pretty xs)
  pretty (LogE x) = pretty x
  pretty (LetE bindings body) = vsep [pretty v <+> "=" <+> pretty e | (v, e) <- bindings] <+> "in" <+> pretty body
  pretty (AssE v e es) = pretty v <+> "=" <+> pretty e <+> "where" <+> (align . vsep . map pretty) es
  pretty (SrcE Source {srcName = srcname, srcLang = lang, srcPath = file', srcAlias = alias, srcRsize = rsizes}) =
    "source"
      <+> viaShow lang
      <> maybe "" (\f -> "from" <+> pretty f) file'
        <+> "where\n"
      <> indent
        2
        ( vsep
            [ "--' @name " <> pretty srcname
            , "--' @rsize " <> encloseSep "" "" " " (map pretty rsizes)
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
  pretty (PatS _ _ _ app) = "(PatS" <+> pretty (patAppPattern app) <+> list (map pretty (toList app)) <> ")"
  pretty (ShapeS _ _ r) = "(ShapeS" <+> pretty r <> ")"
  pretty (VarS v res) = "(VarS" <+> pretty v <+> "=" <+> list (map pretty (toList res)) <> ")"
  pretty (LamS vs e) = "(LamS" <+> list (map pretty vs) <+> "->" <+> pretty e <> ")"
  pretty (LstS es) = "(LstS" <+> list (map pretty es) <> ")"
  pretty (TupS es) = "(TupS" <+> list (map pretty es) <> ")"
  pretty (NamS rs) = "(NamS" <+> list [pretty k <> "=" <> pretty v | (k, v) <- rs] <> ")"
  pretty UniS = "UniS"
  pretty NullS = "NullS"
  pretty (BndS x) = "(BndS" <+> pretty x <> ")"
  pretty (ConS tv n _ xs) = "(ConS" <+> pretty tv <> "." <> pretty n <+> list (map pretty xs) <> ")"
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
  pretty (LabelS k e) = "(LabelS" <+> pretty k <+> pretty e <> ")"

instance Pretty ExecutableExpr where
  pretty (SrcCall (Idx _ src)) = pretty src

instance Pretty Signature where
  pretty (Signature v _ e) = pretty v <+> "::" <+> pretty (etype e)
