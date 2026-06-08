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

Resolution order, per subfield (start, pass, fail), at the labeled
manifold's midx:

  per-label 'manifoldConfigLogTemplate'
      > program-wide 'stateLogTemplate'
      > built-in 'defaultLogTemplate'

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
  , defaultLogTemplate
  , collectRenderedTemplates
  ) where

import Data.Binary (Binary)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (First (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Morloc.CodeGenerator.Namespace
import Morloc.Data.Doc
import qualified Morloc.Monad as MM

-- | The per-labeled-midx rendered templates ready for codegen. Each
-- subfield is the template with static placeholders substituted; the
-- runtime placeholders @{date}@, @{runtime}@, @{id}@ remain for the
-- pool helper to fill. A 'Nothing' subfield means \"emit nothing for
-- this event\" (the user nulled out the subfield).
data RenderedTemplate = RenderedTemplate
  { renderedStart :: Maybe Text
  , renderedPass :: Maybe Text
  , renderedFail :: Maybe Text
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
      render' picker = traverse (\t -> renderStatic t staticVars srcIdx) (picker effective)
  RenderedTemplate
    <$> render' logTemplateStart
    <*> render' logTemplatePass
    <*> render' logTemplateFail
    >>= \r -> return (midx, r)
  where
    srcIdx = fromMaybe midx (manifoldConfigLabelIdx cfg)

-- | Resolve each subfield with @per-label > program > built-in@ precedence.
mergeTemplates :: Maybe LogTemplate -> Maybe LogTemplate -> LogTemplate -> LogTemplate
mergeTemplates perLabel program builtin =
  LogTemplate
    { logTemplateStart = pick logTemplateStart
    , logTemplatePass  = pick logTemplatePass
    , logTemplateFail  = pick logTemplateFail
    }
  where
    pick f = getFirst $ foldMap (First . (f =<<)) [perLabel, program] <> First (f builtin)

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
renderStatic tmpl vars errIdx = do
  parts <- splitTemplate tmpl errIdx
  T.concat <$> mapM substitute parts
  where
    substitute :: TemplatePart -> MorlocMonad Text
    substitute (Literal t) = return t
    substitute (Placeholder name)
      | name `elem` runtimePlaceholders = return ("{" <> name <> "}")
      | otherwise = case lookup name vars of
          Just v -> return v
          Nothing ->
            MM.throwSourcedError errIdx $
              "Unknown placeholder" <+> pretty (T.unpack ("{" <> name <> "}"))
              <+> "in log-template. Available static placeholders:"
              <+> pretty (T.unpack (T.intercalate ", " (map fst vars)))
              <> "; runtime placeholders:"
              <+> pretty (T.unpack (T.intercalate ", " runtimePlaceholders)) <> "."

runtimePlaceholders :: [Text]
runtimePlaceholders = ["date", "runtime", "id"]

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
