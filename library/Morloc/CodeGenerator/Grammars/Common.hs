{-|
Module      : Morloc.CodeGenerator.Grammars.Common
Description : A common set of utility functions for language templates
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.CodeGenerator.Grammars.Common
  ( SAnno(..)
  , SExpr(..)
  , SerialMap(..)
  , Meta(..)
  , Argument(..)
  , Grammar(..)
  , TryDoc(..)
  , GeneralFunction(..)
  , GeneralAssignment(..)
  , UnpackerDoc(..)
  , ForeignCallDoc(..)
  , PoolMain(..)
  ) where

import Morloc.Data.Doc
import Morloc.Namespace
import Morloc.Pretty () -- just for mshow instances
import qualified Data.Map.Strict as Map
import qualified Morloc.Config as MC
import qualified Morloc.Data.Text as MT
import qualified Morloc.Language as ML
import qualified Morloc.Monad as MM
import qualified Morloc.System as MS

import Data.Scientific (Scientific)
import qualified Data.Set as Set

data SAnno a = SAnno (SExpr a) a deriving (Show, Ord, Eq)

data SExpr a
  = UniS
  | VarS EVar
  | ListS [SAnno a]
  | TupleS [SAnno a]
  | LamS [EVar] (SAnno a)
  | AppS (SAnno a) [SAnno a]
  | NumS Scientific
  | LogS Bool
  | StrS MT.Text
  | RecS [(EVar, SAnno a)]
  | ForeignS Int Lang
  deriving (Show, Ord, Eq)

data SerialMap = SerialMap {
    packers :: Map.Map Type (Name, Path)
  , unpackers :: Map.Map Type (Name, Path)
} deriving (Show, Ord, Eq)

data Meta = Meta {
    metaGeneralType :: Maybe Type
  , metaName :: Maybe Name
  , metaProperties :: Set.Set Property
  , metaConstraints :: Set.Set Constraint
  , metaSources :: Set.Set Source
  , metaModule :: MVar
  , metaId :: Int
  , metaArgs :: [Argument]
  , metaPacker :: Name
  , metaPackerPath :: Path
  -- -- there should be morloc source info here, for great debugging
  -- metaMorlocSource :: Path
  -- metaMorlocSourceLine :: Int
  -- metaMorlocSourceColumn :: Int
}

data Argument = Argument {
    argName :: Name
  , argType :: Type
  , argPacker :: Name
  , argPackerPath :: Path
  , argUnpacker :: Name
  , argUnpackerPath :: Path
  , argIsPacked :: Bool
} deriving (Show, Ord, Eq)

data Grammar =
  Grammar
    { gLang :: Lang
    , gSerialType :: Type
    , gAssign :: GeneralAssignment -> MDoc
    , gCall :: MDoc -> [MDoc] -> MDoc
    , gFunction :: GeneralFunction -> MDoc
    , gSignature :: GeneralFunction -> MDoc
    , gId2Function :: Integer -> MDoc
    , gCurry :: MDoc -- function name
             -> [MDoc] -- arguments that are partially applied
             -> Int -- number of remaining arguments
             -> MDoc
    , gComment :: MDoc -> MDoc
    , gReturn :: MDoc -> MDoc
    , gQuote :: MDoc -> MDoc
    , gImport :: MDoc -> MDoc -> MDoc
    , gPrepImport :: MT.Text -> MorlocMonad MDoc
    ---------------------------------------------
    , gNull :: MDoc
    , gBool :: Bool -> MDoc
    , gList :: [MDoc] -> MDoc
    , gTuple :: [MDoc] -> MDoc
    , gRecord :: [(MDoc, MDoc)] -> MDoc
    ---------------------------------------------
    , gIndent :: MDoc -> MDoc
    , gUnpacker :: UnpackerDoc -> MDoc
    , gTry :: TryDoc -> MDoc
    , gForeignCall :: ForeignCallDoc -> MDoc
    , gSwitch
        :: ((Type, Meta, Name) -> MDoc)
        -> ((Type, Meta, Name) -> MDoc)
        -> [(Type, Meta, Name)] -> MDoc
        -> MDoc
        -> MDoc
    , gCmdArgs :: [MDoc] -- ^ infinite list of main arguments (e.g. "argv[2]")
    , gShowType :: Type -> MDoc
    , gMain :: PoolMain -> MorlocMonad MDoc
    }

data GeneralAssignment =
  GeneralAssignment
    { gaType :: Maybe MDoc
    , gaName :: MDoc
    , gaValue :: MDoc
    , gaArg :: Maybe MData
    }
  deriving (Show)

data GeneralFunction =
  GeneralFunction
    { gfComments :: MDoc
    , gfReturnType :: Maybe MDoc -- ^ concrete return type
    , gfName :: MDoc -- ^ function name
    , gfArgs :: [(Maybe MDoc, MDoc)] -- ^ (variable concrete type, variable name)
    , gfBody :: MDoc
    }
  deriving (Show)

data TryDoc =
  TryDoc
    { tryCmd :: MDoc -- ^ The function we attempt to run
    , tryRet :: MDoc -- ^ A name for the returned variable?
    , tryArgs :: [MDoc] -- ^ Arguments passed to function
    , tryMid :: MDoc -- ^ The name of the calling manifold (for debugging)
    , tryFile :: MDoc -- ^ The file where the issue occurs (for debugging)
    }
  deriving (Show)

data UnpackerDoc =
  UnpackerDoc
    { udValue :: MDoc -- ^ The expression that will be unpacked
    , udUnpacker :: MDoc -- ^ The function for unpacking the value
    , udMid :: MDoc -- ^ Manifold name for debugging messages
    , udFile :: MDoc -- ^ File name for debugging messages
    }
  deriving (Show)

data ForeignCallDoc =
  ForeignCallDoc
    { fcdForeignPool :: MDoc -- ^ the name of the foreign pool (e.g., "R.pool")
    , fcdForeignExe :: MDoc -- ^ path to the foreign executable
    , fcdMid :: MDoc -- ^ the function integer identifier
    , fcdArgs :: [MDoc] -- ^ CLI arguments passed to foreign function
    , fcdCall :: [MDoc] -- ^ make a list of CLI arguments from first two
                        -- inputs -- since fcdArgs will likely be
                        -- variables, they are not included in this call.
    , fcdFile :: MDoc -- ^ for debugging
    }
  deriving (Show)

data PoolMain =
  PoolMain
    { pmSources :: [MDoc]
    , pmSignatures :: [MDoc]
    , pmPoolManifolds :: [MDoc]
    , pmDispatchManifold :: MDoc -> MDoc -> MDoc
    }
