{-|
Module      : Morloc.Types
Description : All types and datastructures
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Types ( 
    Script(..)
  , Manifold(..)
  , Argument(..)
  , MData(..)
  , MType(..)
  , MShow(..)
  , SerialMap(..)
  , SparqlEndPoint  
  , Text
  , Name
  , Lang
  , Path
  , Code
  , Key
  , Value
  , Element
  , ScriptGenerator
  , CodeGenerator
) where

import qualified Data.Text as DT
import qualified Data.Map.Strict as Map
import Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))

-- | Write into Morloc code
class MShow a where
  mshow :: a -> Doc

type Text = DT.Text

type Name    = Text
type Lang    = Text
type Path    = Text
type Code    = Text
type Key     = Text
type Value   = Text
type Element = Text

data Manifold = Manifold {
      mCallId      :: Key
    , mType        :: Maybe MType
    , mExported    :: Bool
    , mCalled      :: Bool
    , mSourced     :: Bool
    , mMorlocName  :: Name
    , mCallName    :: Name
    , mSourcePath  :: Maybe Path
    , mSourceName  :: Maybe Name
    , mComposition :: Maybe Name
    , mBoundVars   :: [Name]
    , mLang        :: Maybe Lang
    , mArgs        :: [Argument]
  }
  deriving(Show, Eq, Ord)

data Argument
  = ArgName Name (Maybe MType)
  -- ^ Morloc variables that are defined in scope
  | ArgCall Key (Maybe MType) (Maybe Lang)
  -- ^ A call to some function
  | ArgData MData (Maybe MType)
  -- ^ Raw data defined in one of the Morloc internal types
  | ArgPosi Int (Maybe MType)
  -- ^ A manifold positional argument (passed into a function assignment)
  deriving(Show, Eq, Ord)

-- The values are left unparsed, since they will be used as text
data MData
  = Num' Text
  | Str' Text
  | Log' Bool -- booleans are parsed, since representation depend on language
  | Lst' [MData]
  | Rec' [(Name, MData)]
  | Tup' [MData]
  deriving(Show, Eq, Ord)

data MType
  = MDataType Name [MType]
  | MFuncType [MType] MType -- TODO: add constraints
  | MDeclType Name [Name] Lang MType
  deriving(Show, Eq, Ord)

data SerialMap = SerialMap {
      serialLang :: Lang
    , serialPacker   :: Map.Map MType Name
    , serialUnpacker :: Map.Map MType Name
    , serialGenericPacker   :: Name
    , serialGenericUnpacker :: Name
    , serialSources :: [Path] -- Later, I might want to link the source files to
                              -- each function, but for now that isn't needed.
  }
  deriving(Show, Eq, Ord)

-- | Stores a URL for a SPARQL endpoint (e.g. "http://localhost:3030/morloc")
type SparqlEndPoint = String

-- | A code generator
type ScriptGenerator = SparqlEndPoint -> IO Script
type CodeGenerator = SparqlEndPoint -> IO Code

-- | Stores everything needed to build one file
data Script = Script {
      scriptBase :: String  -- ^ script basename (no extension)
    , scriptLang :: String  -- ^ script language
    , scriptCode :: Text -- ^ full script source code
  }
  deriving(Ord, Eq)
