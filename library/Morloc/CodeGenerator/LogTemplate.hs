{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.CodeGenerator.LogTemplate
Description : Render labeled-manifold log message templates
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

A labeled manifold may carry a 'LogTemplate' describing the text emitted
when the manifold starts, completes successfully, or throws. The
templates are user-authored YAML strings with @{varname}@ placeholders.

Resolution at the labeled manifold's midx:

  * If a per-label 'manifoldConfigLogTemplate' is present, its 'Just'
    subfields override the program-wide template field-by-field; its
    'Nothing' subfields inherit.
  * If a program-wide 'stateLogTemplate' is present, its 'Nothing'
    subfields mean \"emit nothing for this event\" and are honored --
    they do NOT fall through to the built-in default.
  * The built-in 'defaultLogTemplate' is used only when neither a
    per-label nor a program-wide template is supplied.

The compile-time substitution pass resolves the placeholders whose
values are static (@name@, @lang@, @module@, @line@, @column@, @group@,
@index@) and leaves the runtime placeholders (@date@, @runtime@, @id@)
for the per-language pool helper to fill at log emission time.

The render produces three 'Maybe Text' values per labeled midx (start,
pass, fail). A 'Nothing' means \"emit nothing for this event\" -- the
template subfield was explicitly null. Codegen elides that emission.

Unknown placeholder names are a sourced compile error citing the
labeled VarE's source position.
-}
module Morloc.CodeGenerator.LogTemplate
  ( RenderedTemplate (..)
  , RenderedRunLog (..)
  , defaultLogTemplate
  , collectRenderedTemplates
  , renderRunLogTemplate
  ) where

import Data.Binary (Binary)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Morloc.CodeGenerator.Namespace
import Morloc.Data.Doc
import qualified Morloc.Monad as MM
import qualified Morloc.Version as MV

-- | The per-labeled-midx rendered templates ready for codegen. Each
-- subfield is the template with static placeholders substituted; the
-- runtime placeholders @{date}@, @{runtime}@, @{id}@ remain for the
-- pool helper to fill. A 'Nothing' subfield means \"emit nothing for
-- this event\" (the user nulled out the subfield).
data RenderedTemplate = RenderedTemplate
  { renderedStart :: Maybe Text
  , renderedPass :: Maybe Text
  , renderedFail :: Maybe Text
  , renderedGroup :: Text
    -- ^ The label group name (e.g. @\"a\"@ for @a:foo@). Passed through
    -- to the pool's log emit helper so a per-label log file under
    -- @$MORLOC_RUN_DIR/<group>/log@ can be opened. Empty when the
    -- manifold somehow lacks a label, in which case the per-label tee
    -- is suppressed at runtime.
  }
  deriving (Show, Eq, Generic)

instance Binary RenderedTemplate

-- | The built-in default. Used to fill any subfield neither a per-label
-- nor a program-wide template supplies.
defaultLogTemplate :: LogTemplate
defaultLogTemplate =
  LogTemplate
    { logTemplateStart = Just "[{date}] {module}:{line}:{name}:{lang} start"
    , logTemplatePass = Just "[{date}] {module}:{line}:{name}:{lang} pass (time={runtime})"
    , logTemplateFail = Just "[{date}] {module}:{line}:{name}:{lang} fail (time={runtime})"
    }

-- | Walk 'stateManifoldConfig' and render templates for every labeled
-- midx that has @log: true@. Static placeholders are substituted from
-- 'stateName' (for @{name}@), 'stateSourceMap' (for @{module}@,
-- @{line}@, @{column}@), and per-call info. Returns a map keyed by
-- midx; codegen drops the result into per-language helper calls.
collectRenderedTemplates ::
  Lang ->
  -- ^ The pool language (used for @{lang}@).
  MorlocMonad (Map Int RenderedTemplate)
collectRenderedTemplates lang = do
  cfgs <- MM.gets stateManifoldConfig
  programDefault <- MM.gets stateLogTemplate
  names <- MM.gets stateName
  srcMap <- MM.gets stateSourceMap
  Map.fromList <$> mapM (renderOne lang programDefault names srcMap)
    [ (midx, cfg)
    | (midx, cfg) <- Map.toList cfgs
    , manifoldConfigLog cfg == Just True
    , isJustLabel cfg
    ]
  where
    isJustLabel cfg = case manifoldConfigLabel cfg of
      Just _ -> True
      Nothing -> False

renderOne ::
  Lang ->
  Maybe LogTemplate ->
  Map Int EVar ->
  Map Int SrcLoc ->
  (Int, ManifoldConfig) ->
  MorlocMonad (Int, RenderedTemplate)
renderOne lang programDefault names srcMap (midx, cfg) = do
  case manifoldConfigLogTemplate cfg of
    Just (LogTemplate Nothing Nothing Nothing) ->
      MM.throwSourcedError srcIdx $
        "Labeled manifold has 'log: true' but its 'log-template' nulls"
        <+> "every subfield. This combination is contradictory; either"
        <+> "set 'log: false' to silence logging, or omit the"
        <+> "'log-template' override to keep the program defaults."
    _ -> return ()
  let effective = mergeTemplates (manifoldConfigLogTemplate cfg) programDefault defaultLogTemplate
      staticVars = staticBindings lang names srcMap cfg midx
      renderField picker = traverse (\t -> renderStatic t staticVars srcIdx) (picker effective)
      groupTxt = case manifoldConfigLabel cfg of
        Just g -> g
        Nothing -> ""
  RenderedTemplate
    <$> renderField logTemplateStart
    <*> renderField logTemplatePass
    <*> renderField logTemplateFail
    <*> pure groupTxt
    >>= \r -> return (midx, r)
  where
    srcIdx = fromMaybe midx (manifoldConfigLabelIdx cfg)

-- | Resolve the effective template. The built-in default is used only
-- when neither a per-label nor a program-wide template is present; once
-- the user supplies a program-wide 'log-template', its 'Nothing'
-- subfields mean \"emit nothing\" and do not fall through to the
-- built-in. A per-label template overrides program-wide field-by-field:
-- 'Just' overrides, 'Nothing' inherits.
mergeTemplates :: Maybe LogTemplate -> Maybe LogTemplate -> LogTemplate -> LogTemplate
mergeTemplates perLabel program builtin =
  case (perLabel, program) of
    (Nothing, Nothing) -> builtin
    (Nothing, Just q)  -> q
    (Just p,  Nothing) -> mergeOver p builtin
    (Just p,  Just q)  -> mergeOver p q
  where
    mergeOver x fallback =
      LogTemplate
        { logTemplateStart = logTemplateStart x `orElse` logTemplateStart fallback
        , logTemplatePass  = logTemplatePass  x `orElse` logTemplatePass  fallback
        , logTemplateFail  = logTemplateFail  x `orElse` logTemplateFail  fallback
        }
    orElse j@(Just _) _ = j
    orElse Nothing y = y

-- | Static placeholder bindings for a labeled midx. The labeled idx
-- (the original VarE position, set by 'collectTags' in
-- 'manifoldConfigLabelIdx') is preferred over the manifold midx for
-- source-position lookups, because label propagation can have moved
-- the config to a different midx whose source position is the AppS
-- rather than the labeled reference.
staticBindings ::
  Lang -> Map Int EVar -> Map Int SrcLoc -> ManifoldConfig -> Int -> [(Text, Text)]
staticBindings lang names srcMap cfg midx =
  let labelIdx = case manifoldConfigLabelIdx cfg of
        Just i -> i
        Nothing -> midx
      nameTxt = case Map.lookup labelIdx names of
        Just (EV t) -> t
        Nothing -> ""
      srcInfo = case Map.lookup labelIdx srcMap of
        Just (SrcLoc path ln col _ _) ->
          [ ("module", maybe "" T.pack path)
          , ("line", T.pack (show ln))
          , ("column", T.pack (show col))
          ]
        Nothing -> [("module", ""), ("line", ""), ("column", "")]
      groupTxt = case manifoldConfigLabel cfg of
        Just g -> g
        Nothing -> ""
   in [ ("name", nameTxt)
      , ("lang", langName lang)
      , ("group", groupTxt)
      , ("index", T.pack (show midx))
      ] <> srcInfo
      <> colorPlaceholders

-- | All SGR placeholders. Each maps to a raw ANSI escape sequence. The
-- byte sequence survives codegen via per-language string-literal escapers
-- ('escapeStringLit' in Generic.hs, 'escapeCxxStringLit' in CppTranslator);
-- pool helpers strip these sequences when stderr is not a TTY so log
-- files never contain control bytes.
colorPlaceholders :: [(Text, Text)]
colorPlaceholders =
  [ -- attributes
    ("c:reset",        sgr  0)
  , ("c:bold",         sgr  1)
  , ("c:dim",          sgr  2)
  , ("c:italic",       sgr  3)
  , ("c:underline",    sgr  4)
  , ("c:blink",        sgr  5)
  , ("c:rapid-blink",  sgr  6)
  , ("c:reverse",      sgr  7)
  , ("c:hidden",       sgr  8)
  , ("c:strike",       sgr  9)
  , ("c:no-bold",      sgr 22)
  , ("c:no-dim",       sgr 22)
  , ("c:no-italic",    sgr 23)
  , ("c:no-underline", sgr 24)
  , ("c:no-blink",     sgr 25)
  , ("c:no-reverse",   sgr 27)
  , ("c:no-hidden",    sgr 28)
  , ("c:no-strike",    sgr 29)
    -- foreground
  , ("c:black",        sgr 30)
  , ("c:red",          sgr 31)
  , ("c:green",        sgr 32)
  , ("c:yellow",       sgr 33)
  , ("c:blue",         sgr 34)
  , ("c:magenta",      sgr 35)
  , ("c:cyan",         sgr 36)
  , ("c:white",        sgr 37)
  , ("c:default",      sgr 39)
    -- bright foreground
  , ("c:bright-black",   sgr 90)
  , ("c:gray",           sgr 90)
  , ("c:grey",           sgr 90)
  , ("c:bright-red",     sgr 91)
  , ("c:bright-green",   sgr 92)
  , ("c:bright-yellow",  sgr 93)
  , ("c:bright-blue",    sgr 94)
  , ("c:bright-magenta", sgr 95)
  , ("c:bright-cyan",    sgr 96)
  , ("c:bright-white",   sgr 97)
    -- background
  , ("c:bg-black",       sgr  40)
  , ("c:bg-red",         sgr  41)
  , ("c:bg-green",       sgr  42)
  , ("c:bg-yellow",      sgr  43)
  , ("c:bg-blue",        sgr  44)
  , ("c:bg-magenta",     sgr  45)
  , ("c:bg-cyan",        sgr  46)
  , ("c:bg-white",       sgr  47)
  , ("c:bg-default",     sgr  49)
    -- bright background
  , ("c:bg-bright-black",   sgr 100)
  , ("c:bg-bright-red",     sgr 101)
  , ("c:bg-bright-green",   sgr 102)
  , ("c:bg-bright-yellow",  sgr 103)
  , ("c:bg-bright-blue",    sgr 104)
  , ("c:bg-bright-magenta", sgr 105)
  , ("c:bg-bright-cyan",    sgr 106)
  , ("c:bg-bright-white",   sgr 107)
  ]
  where
    sgr :: Int -> Text
    sgr n = "\ESC[" <> T.pack (show n) <> "m"

-- | Substitute static placeholders into a template. Runtime placeholders
-- (@{date}@, @{runtime}@, @{id}@) are left intact -- the per-language
-- pool helper substitutes them at emission time. Any other unknown
-- placeholder is a sourced compile error.
renderStatic :: Text -> [(Text, Text)] -> Int -> MorlocMonad Text
renderStatic tmpl vars errIdx =
  renderStaticWith perLabelRuntimePlaceholders tmpl vars errIdx "log-template"

-- | Generalized static-placeholder substitution, parameterized by the
-- set of placeholders the runtime is responsible for filling. Reused by
-- both per-label 'log-template' rendering and run-scope 'prologue' /
-- 'epilogue' rendering.
renderStaticWith :: [Text] -> Text -> [(Text, Text)] -> Int -> Text -> MorlocMonad Text
renderStaticWith runtimeNames tmpl vars errIdx templateKind = do
  parts <- splitTemplate tmpl errIdx
  T.concat <$> mapM substitute parts
  where
    substitute :: TemplatePart -> MorlocMonad Text
    substitute (Literal t) = return t
    substitute (Placeholder name)
      | name `elem` runtimeNames = return ("{" <> name <> "}")
      | otherwise = case lookup name vars of
          Just v -> return v
          Nothing ->
            MM.throwSourcedError errIdx $
              "Unknown placeholder" <+> pretty (T.unpack ("{" <> name <> "}"))
              <+> "in" <+> pretty (T.unpack templateKind) <> "."
              <+> "Available static placeholders:"
              <+> pretty (T.unpack (T.intercalate ", " (map fst vars)))
              <> "; runtime placeholders:"
              <+> pretty (T.unpack (T.intercalate ", " runtimeNames)) <> "."

perLabelRuntimePlaceholders :: [Text]
perLabelRuntimePlaceholders = ["date", "runtime", "id"]

-- | Runtime placeholders for the run-scope prologue/epilogue templates.
-- The nexus substitutes these immediately before stderr emission.
-- Keep aligned with the Rust nexus's 'render' in @runlog.rs@.
runScopeRuntimePlaceholders :: [Text]
runScopeRuntimePlaceholders =
  [ "name"
  , "run_id"
  , "started_at"
  , "finished_at" -- epilogue only; empty string in prologue
  , "runtime"     -- epilogue only
  , "pid"
  , "hostname"
  , "exit_code"   -- fail epilogue only
  , "error"       -- fail epilogue only
  ]

-- | A literal text fragment or a placeholder name. Templates parse into
-- a list of these for substitution.
data TemplatePart = Literal !Text | Placeholder !Text deriving (Show)

splitTemplate :: Text -> Int -> MorlocMonad [TemplatePart]
splitTemplate input errIdx = go input
  where
    go :: Text -> MorlocMonad [TemplatePart]
    go t
      | T.null t = return []
      | otherwise = case T.breakOn "{" t of
          (lit, rest) | T.null rest -> return [Literal lit]
                      | otherwise -> do
                          let afterOpen = T.drop 1 rest
                          case T.breakOn "}" afterOpen of
                            (_, closeRest) | T.null closeRest ->
                              MM.throwSourcedError errIdx $
                                "Unmatched '{' in log-template:" <+> pretty (T.unpack input)
                            (name, closeRest) -> do
                              parts <- go (T.drop 1 closeRest)
                              return $ (if T.null lit then id else (Literal lit :))
                                     $ Placeholder name : parts

-- | Rendered run-scope log templates ready for emission by the nexus.
-- Each subfield is the template with static placeholders ({module},
-- {version}, {morloc_version}, color codes) substituted; runtime
-- placeholders remain as @{name}@-style tokens for the nexus to fill.
-- 'Nothing' means \"emit nothing for this event\".
data RenderedRunLog = RenderedRunLog
  { renderedPrologue :: Maybe Text
  , renderedEpilogueOk :: Maybe Text
  , renderedEpilogueFail :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance Binary RenderedRunLog

-- | Render the run-scope prologue and epilogue templates from the
-- entry-point module. Substitutes compile-time placeholders ({module},
-- {version}, {morloc_version}, color codes) and leaves runtime
-- placeholders for the nexus to fill. Returns 'Nothing' when the YAML
-- declares no run-scope templates (so the nexus has nothing to do).
-- Unknown placeholders are a compile error.
renderRunLogTemplate :: MorlocMonad (Maybe RenderedRunLog)
renderRunLogTemplate = do
  mRunLog <- MM.gets stateRunLog
  case mRunLog of
    Nothing -> return Nothing
    Just rl -> do
      bindings <- runScopeStaticBindings
      let renderRunScope :: Text -> MorlocMonad Text
          renderRunScope t =
            renderStaticWith
              runScopeRuntimePlaceholders
              t
              bindings
              0           -- run-scope templates have no per-source idx
              "prologue/epilogue template"
      prologue <- traverse renderRunScope (runLogPrologue rl)
      (okT, failT) <- case runLogEpilogue rl of
        Nothing -> return (Nothing, Nothing)
        Just et -> do
          mOk <- traverse renderRunScope (epilogueOk et)
          mFail <- traverse renderRunScope (epilogueFail et)
          return (mOk, mFail)
      return $
        Just
          RenderedRunLog
            { renderedPrologue = prologue
            , renderedEpilogueOk = okT
            , renderedEpilogueFail = failT
            }

-- | Static placeholder bindings for run-scope templates. Pulls the
-- entry-point module name from 'stateModuleName' and the program
-- version from the first entry of 'statePackageMeta'. Either may be
-- absent (no main module set yet, or no @package.yaml@), in which
-- case the placeholder renders as @\"?\"@.
runScopeStaticBindings :: MorlocMonad [(Text, Text)]
runScopeStaticBindings = do
  mModName <- MM.gets stateModuleName
  pkgMeta <- MM.gets statePackageMeta
  let moduleTxt = case mModName of
        Just (MV m) -> m
        Nothing -> "?"
      versionTxt = case pkgMeta of
        (m : _) | not (T.null (packageVersion m)) -> packageVersion m
        _ -> "?"
  return $
    [ ("module", moduleTxt)
    , ("version", versionTxt)
    , ("morloc_version", T.pack MV.versionStr)
    ]
      <> colorPlaceholders
