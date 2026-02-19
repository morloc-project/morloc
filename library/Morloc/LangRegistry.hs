{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.LangRegistry
Description : Language registry loaded from lang.yaml and languages.yaml
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Provides metadata about all supported languages. Built from embedded
lang.yaml files at startup and extended with discovered plugin languages
at parse time.
-}
module Morloc.LangRegistry
  ( LangRegistry (..)
  , LangRegistryEntry (..)
  , emptyRegistry
  , buildDefaultRegistry
  , extendRegistry
  , lookupLang
  , lookupByAlias
  , buildLangMap
  , registryPairwiseCost
  , registryLanguageCost
  , registrySerialType
  , registryIsCompiled
  , registryRunCommand
  , registryMakeExtension
  , registryMakeExecutablePoolName
  , registryMakeSourcePoolName
  , parseLangYamlFile
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml as Y
import Data.Aeson ((.:), (.:?), (.!=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Morloc.Language (Lang(..))

data LangRegistry = LangRegistry
  { lrEntries :: Map Text LangRegistryEntry  -- keyed by canonical name
  , lrAliases :: Map Text Text               -- alias -> canonical name
  , lrSameLangCosts :: Map Text Int
  , lrCrossLangCosts :: Map Text Int
  , lrOptimizedPairs :: Map (Text, Text) Int
  , lrDefaultSameCost :: Int
  , lrDefaultCrossCost :: Int
  } deriving (Show)

data LangRegistryEntry = LangRegistryEntry
  { lreExtension :: !String
  , lreIsCompiled :: !Bool
  , lreRunCommand :: ![Text]
  , lreSerialType :: !Text
  , lreCost :: !Int
  , lrePreamble :: ![Text]
  } deriving (Show)

emptyRegistry :: LangRegistry
emptyRegistry = LangRegistry Map.empty Map.empty Map.empty Map.empty Map.empty 10 10000

-- | Build the default registry from embedded lang.yaml files and languages.yaml
buildDefaultRegistry :: [(String, Text)] -> Text -> Either String LangRegistry
buildDefaultRegistry langFiles languagesText = do
  langs <- mapM parseLangYaml langFiles
  costs <- parseLanguagesYaml languagesText
  let entries = Map.fromList [(lymName ly, entryFromYaml ly) | ly <- langs]
      aliases = Map.fromList $ concatMap (\ly -> [(a, lymName ly) | a <- lymAliases ly] ++ [(lymName ly, lymName ly)]) langs
  return costs
    { lrEntries = entries
    , lrAliases = aliases
    }

-- | Extend the registry with a new language entry (for plugins)
extendRegistry :: Text -> LangRegistryEntry -> [Text] -> LangRegistry -> LangRegistry
extendRegistry name entry newAliases reg = reg
  { lrEntries = Map.insert name entry (lrEntries reg)
  , lrAliases = Map.union (Map.fromList $ (name, name) : [(a, name) | a <- newAliases]) (lrAliases reg)
  }

lookupLang :: Text -> LangRegistry -> Maybe LangRegistryEntry
lookupLang name reg = Map.lookup name (lrEntries reg)

-- | Look up by alias, returning (canonical name, entry)
lookupByAlias :: Text -> LangRegistry -> Maybe (Text, LangRegistryEntry)
lookupByAlias alias reg = do
  canonical <- Map.lookup (T.toLower alias) (lrAliases reg)
  entry <- Map.lookup canonical (lrEntries reg)
  return (canonical, entry)

-- | Build a map from all aliases (lowercased) to Lang values.
-- Used by the parser to resolve language names.
buildLangMap :: LangRegistry -> Map Text Lang
buildLangMap reg = Map.fromList
  [ (alias, Lang canonical (lreExtension entry))
  | (alias, canonical) <- Map.toList (lrAliases reg)
  , Just entry <- [Map.lookup canonical (lrEntries reg)]
  ]

registryPairwiseCost :: LangRegistry -> Text -> Text -> Int
registryPairwiseCost reg from to
  | from == to = case Map.lookup from (lrSameLangCosts reg) of
      Just c -> c
      Nothing -> lrDefaultSameCost reg
  | otherwise = case Map.lookup (from, to) (lrOptimizedPairs reg) of
      Just c -> c
      Nothing -> case Map.lookup to (lrCrossLangCosts reg) of
        Just c -> c
        Nothing -> lrDefaultCrossCost reg

registryLanguageCost :: LangRegistry -> Text -> Int
registryLanguageCost reg name = case Map.lookup name (lrEntries reg) of
  Just entry -> lreCost entry
  Nothing -> 5

registrySerialType :: LangRegistry -> Text -> Text
registrySerialType reg name = case Map.lookup name (lrEntries reg) of
  Just entry -> lreSerialType entry
  Nothing -> "bytes"

registryIsCompiled :: LangRegistry -> Text -> Bool
registryIsCompiled reg name = case Map.lookup name (lrEntries reg) of
  Just entry -> lreIsCompiled entry
  Nothing -> False

registryRunCommand :: LangRegistry -> Text -> [Text]
registryRunCommand reg name = case Map.lookup name (lrEntries reg) of
  Just entry -> lreRunCommand entry
  Nothing -> []

registryMakeExtension :: LangRegistry -> Text -> String
registryMakeExtension reg name = case Map.lookup name (lrEntries reg) of
  Just entry -> lreExtension entry
  Nothing -> T.unpack name

registryMakeExecutablePoolName :: LangRegistry -> Text -> String -> String
registryMakeExecutablePoolName reg name base =
  if registryIsCompiled reg name
    then base <> "-" <> T.unpack name <> ".out"
    else base <> "." <> registryMakeExtension reg name

registryMakeSourcePoolName :: LangRegistry -> Text -> String -> String
registryMakeSourcePoolName reg name base =
  base <> "." <> registryMakeExtension reg name

-- internal YAML types

data LangYamlMeta = LangYamlMeta
  { lymName :: Text
  , lymExtension :: String
  , lymAliases :: [Text]
  , lymIsCompiled :: Bool
  , lymRunCommand :: [Text]
  , lymSerialType :: Text
  , lymCost :: Int
  , lymPreamble :: [Text]
  } deriving (Show)

instance Aeson.FromJSON LangYamlMeta where
  parseJSON = Aeson.withObject "LangYamlMeta" $ \o ->
    LangYamlMeta
      <$> o .: "name"
      <*> o .: "extension"
      <*> o .:? "aliases" .!= []
      <*> o .:? "is_compiled" .!= False
      <*> o .:? "run_command" .!= []
      <*> o .:? "serial_type" .!= "bytes"
      <*> o .:? "cost" .!= 5
      <*> o .:? "preamble" .!= []

data LanguagesYaml = LanguagesYaml
  { lysSameLangCosts :: Map Text Int
  , lysCrossLangCosts :: Map Text Int
  , lysOptimizedPairs :: [(Text, Text, Int)]
  , lysDefaultSame :: Int
  , lysDefaultCross :: Int
  } deriving (Show)

instance Aeson.FromJSON LanguagesYaml where
  parseJSON = Aeson.withObject "LanguagesYaml" $ \o -> do
    same <- o .:? "same_language_costs" .!= Map.empty
    cross <- o .:? "cross_language_costs" .!= Map.empty
    pairs <- o .:? "optimized_pairs" .!= []
    defSame <- o .:? "default_same_language" .!= 10
    defCross <- o .:? "default_cross_language" .!= 10000
    parsedPairs <- mapM parsePair pairs
    return $ LanguagesYaml same cross parsedPairs defSame defCross
    where
      parsePair = Aeson.withObject "OptimizedPair" $ \o ->
        (,,) <$> o .: "from" <*> o .: "to" <*> o .: "cost"

entryFromYaml :: LangYamlMeta -> LangRegistryEntry
entryFromYaml ly = LangRegistryEntry
  { lreExtension = lymExtension ly
  , lreIsCompiled = lymIsCompiled ly
  , lreRunCommand = lymRunCommand ly
  , lreSerialType = lymSerialType ly
  , lreCost = lymCost ly
  , lrePreamble = lymPreamble ly
  }

-- | Parse a lang.yaml file from the filesystem, returning (canonical name, extension)
parseLangYamlFile :: FilePath -> IO (Either String (Text, String))
parseLangYamlFile path = do
  content <- TE.decodeUtf8 <$> BS.readFile path
  case parseLangYaml ("file:" ++ path, content) of
    Left err -> return (Left err)
    Right meta -> return (Right (lymName meta, lymExtension meta))

parseLangYaml :: (String, Text) -> Either String LangYamlMeta
parseLangYaml (name, content) =
  case Y.decodeEither' (TE.encodeUtf8 content) of
    Left err -> Left $ "Failed to parse lang.yaml for " ++ name ++ ": " ++ Y.prettyPrintParseException err
    Right meta -> Right meta

parseLanguagesYaml :: Text -> Either String LangRegistry
parseLanguagesYaml content =
  case Y.decodeEither' (TE.encodeUtf8 content) of
    Left err -> Left $ "Failed to parse languages.yaml: " ++ Y.prettyPrintParseException err
    Right lys -> Right $ LangRegistry
      { lrEntries = Map.empty
      , lrAliases = Map.empty
      , lrSameLangCosts = lysSameLangCosts lys
      , lrCrossLangCosts = lysCrossLangCosts lys
      , lrOptimizedPairs = Map.fromList [((f, t), c) | (f, t, c) <- lysOptimizedPairs lys]
      , lrDefaultSameCost = lysDefaultSame lys
      , lrDefaultCrossCost = lysDefaultCross lys
      }
