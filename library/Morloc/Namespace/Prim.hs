{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Namespace.Prim
Description : Foundation types with zero dependency on other Namespace modules
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}

module Morloc.Namespace.Prim
  ( -- ** re-exports
    module Morloc.Internal

    -- ** Synonyms
  , MDoc
  , DAG

    -- ** Other functors
  , None (..)
  , One (..)
  , Or (..)
  , Many (..)

    -- ** Other classes
  , Three (..)
  , Defaultable (..)

    -- ** Indexed
  , IndexedGeneral (..)
  , Indexed

    -- ** Newtypes
  , MVar (..)
  , EVar (..)
  , TVar (..)
  , ClassName (..)
  , CVar (..)
  , Key (..)
  , Label (..)
  , SrcName (..)
  , Path
  , Code (..)
  , TimeInSeconds (..)
  , DirTree (..)
  , AnchoredDirTree (..)

    -- ** Data
  , GMap (..)
  , GMapRet (..)

    -- ** Source locations
  , SrcLoc (..)

    -- ** Typeclasses
  , HasOneLanguage (..)

    -- ** Language
  , Lang (..)
  ) where

import Data.Aeson (FromJSON (..))
import qualified Data.Aeson as Aeson
import Data.Binary (Binary)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as DT
import GHC.Generics (Generic)
import Morloc.Data.Doc
import Morloc.Internal
import Morloc.Language (Lang(..))
import System.Directory.Tree (AnchoredDirTree (..), DirTree (..))
import Text.Read (readMaybe)

---- Typeclasses

class HasOneLanguage a where
  langOf :: a -> Maybe Lang
  langOf' :: a -> Lang

  langOf x = Just (langOf' x)
  langOf' x = fromJust (langOf x)

class Defaultable a where
  defaultValue :: a

---- Type definitions

-- | no annotations for now
type MDoc = Doc ()

{- | A general purpose Directed Acyclic Graph (DAG). Technically this structure
needn't be acyclic. You can use `findCycle` to check whether a given stucture
has cycles.
-}
type DAG key edge node = Map key (node, [(key, edge)])

data GMap a b c = GMap (Map a b) (Map b c)
  deriving (Show, Ord, Eq)

data GMapRet c
  = -- | Failure on the first key
    GMapNoFst
  | -- | Failure on the internal key (possible bug)
    GMapNoSnd
  | GMapJust c
  deriving (Show, Ord, Eq)

data SrcLoc = SrcLoc
  { srcLocPath :: Maybe Path
  , srcLocLine :: Int
  , srcLocCol  :: Int
  , srcLocEndLine :: Int
  , srcLocEndCol  :: Int
  } deriving (Show, Ord, Eq)

-- ================ T Y P E C H E C K I N G  =================================

-- A module name
newtype MVar = MV {unMVar :: Text} deriving (Show, Eq, Ord)

-- A term name
newtype EVar = EV {unEVar :: Text} deriving (Show, Eq, Ord)

-- A type general name
newtype TVar = TV {unTVar :: Text} deriving (Show, Eq, Ord)

newtype ClassName = ClassName {unClassName :: Text} deriving (Show, Eq, Ord)

-- A concrete type name
newtype CVar = CV {unCVar :: Text} deriving (Show, Eq, Ord)

newtype Key = Key {unKey :: Text} deriving (Show, Eq, Ord, Generic)
instance Binary Key

newtype Label = Label {unLabel :: Text} deriving (Show, Eq, Ord)

-- The name of a source function, this is text which may be illegal in morloc
-- (such as the R function "file.exists")
newtype SrcName = SrcName {unSrcName :: Text} deriving (Show, Eq, Ord)

newtype Code = Code {unCode :: Text} deriving (Show, Eq, Ord)

newtype TimeInSeconds = TimeInSeconds {unTimeInSeconds :: Int} deriving (Show, Eq, Ord)

-- this is a string because the path libraries want strings
type Path = String

data Three a b c = A a | B b | C c
  deriving (Ord, Eq, Show)

data None = None
  deriving (Show)

newtype One a = One {unOne :: a}
  deriving (Show)

newtype Many a = Many {unMany :: [a]}
  deriving (Show)

data Or a b = L a | R b | LR a b
  deriving (Ord, Eq, Show)

type Indexed = IndexedGeneral Int

data IndexedGeneral k a = Idx k a
  deriving (Show, Ord, Eq)

---- Fundamental class instances

instance Functor (IndexedGeneral k) where
  fmap f (Idx i x) = Idx i (f x)

instance Functor One where
  fmap f (One x) = One (f x)

instance Functor Many where
  fmap f (Many x) = Many (map f x)

instance Traversable One where
  traverse f (One x) = One <$> f x

instance Traversable Many where
  traverse f (Many xs) = Many <$> traverse f xs

instance Foldable One where
  foldr f b (One a) = f a b

instance Foldable Many where
  foldr f b (Many xs) = foldr f b xs

instance Bifunctor Or where
  bimapM f _ (L a) = L <$> f a
  bimapM _ g (R a) = R <$> g a
  bimapM f g (LR a b) = LR <$> f a <*> g b

instance Bifoldable Or where
  bilistM f _ (L a) = f a |>> return
  bilistM _ g (R b) = g b |>> return
  bilistM f g (LR a b) = do
    c1 <- f a
    c2 <- g b
    return [c1, c2]

instance Annotated IndexedGeneral where
  val (Idx _ x) = x
  ann (Idx i _) = i
  annotate i x = Idx i x

-- Custom FromJSON instance for TimeInSeconds
instance FromJSON TimeInSeconds where
  parseJSON (Aeson.String t) = case parseSlurmTime (DT.unpack t) of
    Just seconds -> return $ TimeInSeconds seconds
    Nothing -> fail $ "Invalid SLURM time format: " ++ DT.unpack t
  parseJSON _ = fail "Expected a string for SLURM time"

-- Convert SLURM time string (e.g., "01-00:00:00") to seconds
parseSlurmTime :: String -> Maybe Int
parseSlurmTime str = case splitOn "-" str of
  [days, hms] -> do
    d <- readMaybe days :: Maybe Int
    s <- parseHMS hms
    return $ d * 86400 + s
  [hms] -> parseHMS hms -- No days specified
  _ -> Nothing

-- Helper to parse "HH:MM:SS" into seconds
parseHMS :: String -> Maybe Int
parseHMS hms = case splitOn ":" hms of
  [hours, minutes, seconds] -> do
    h <- readMaybe hours :: Maybe Int
    m <- readMaybe minutes :: Maybe Int
    s <- readMaybe seconds :: Maybe Int
    return $ h * 3600 + m * 60 + s
  _ -> Nothing

----- Pretty instances -------------------------------------------------------

instance Pretty SrcLoc where
  pretty (SrcLoc path ln col endLn endCol)
    | ln == endLn && col == endCol =
        maybe "<unknown>" pretty path <> ":" <> pretty ln <> ":" <> pretty col
    | ln == endLn =
        maybe "<unknown>" pretty path <> ":" <> pretty ln <> ":" <> pretty col <> "-" <> pretty endCol
    | otherwise =
        maybe "<unknown>" pretty path <> ":" <> pretty ln <> ":" <> pretty col <> "-" <> pretty endLn <> ":" <> pretty endCol

instance (Pretty a, Pretty b) => Pretty (Or a b) where
  pretty (L x) = parens ("L" <+> pretty x)
  pretty (R x) = parens ("R" <+> pretty x)
  pretty (LR x y) = parens ("LR" <+> pretty x <> "," <+> pretty y)

instance Pretty EVar where
  pretty (EV v) = pretty v

instance Pretty MVar where
  pretty = pretty . unMVar

instance Pretty TimeInSeconds where
  pretty = pretty . unTimeInSeconds

instance Pretty TVar where
  pretty (TV v) = pretty v

instance Pretty ClassName where
  pretty = pretty . unClassName

instance Pretty Key where
  pretty (Key v) = pretty v

instance Pretty CVar where
  pretty v = pretty (unCVar v)

instance Pretty Label where
  pretty (Label v) = pretty v

instance Pretty SrcName where
  pretty = pretty . unSrcName

instance Pretty Code where
  pretty = pretty . unCode

instance Pretty None where
  pretty None = "()"

instance (Pretty a) => Pretty (One a) where
  pretty (One x) = pretty x

instance (Pretty a) => Pretty (Many a) where
  pretty (Many xs) = list $ map pretty xs

instance (Pretty k, Pretty a) => Pretty (IndexedGeneral k a) where
  pretty (Idx i x) = parens (pretty i <> ":" <+> pretty x)

instance (Pretty k1, Pretty k2, Pretty v) => Pretty (GMap k1 k2 v) where
  pretty (GMap m1 m2) = "GMap" <+> (align . vsep $ [pretty (Map.toList m1), pretty (Map.toList m2)])
