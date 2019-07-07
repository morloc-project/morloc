{-|
Module      : Morloc.Component.Realization
Description : Map morloc names to concrete types (realizations)
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Component.Realization
(
    fromSparqlDb
  , hsparql
) where

import Morloc.Global
import Morloc.Operators
import Morloc.Sparql
import qualified Morloc.Monad as MM
import qualified Morloc.Data.Text as MT
import qualified Morloc.Language as ML

import qualified Data.List.Extra as DLE
import qualified Data.Map.Strict as Map

-- | Find all potential representations for each sourced morloc function
fromSparqlDb
  :: SparqlDatabaseLike db
  => Map.Map URI MType
  -> db
  -> MorlocMonad (Map.Map Name [Realization])
fromSparqlDb tmap ep
  =   sparqlSelect "realization" hsparql ep
  >>= mapM (realize tmap)
  |>> Map.fromList . DLE.groupSort
  >>= MM.logFileWith "realizations.txt" Map.assocs

realize :: Map.Map URI MType -> [Maybe MT.Text] -> MorlocMonad (Name, Realization)
realize
  tmap
  [ Just morlocName
  , Just langStr
  , Just sourceName
  , typeID
  , sourceFile
  , modulePath
  ] = case (ML.readLangName langStr) of
    (Just lang) ->
      return . (,) morlocName $ Realization
        { rLang         = lang
        , rName         = sourceName
        , rConcreteType = (fmap URI typeID) >>= ((flip Map.lookup) tmap)
        , rModulePath   = modulePath
        , rSourcePath   = sourceFile
        , rSourced      = True
        }
    _ -> MM.throwError $ UnknownLanguage langStr
realize _ x = MM.throwError $ SparqlFail ("Bad query: " <> MT.show' x) 

hsparql :: Query SelectQuery
hsparql = do
  -- internal IDs
  tid_ <- var
  mid_ <- var
  sid_ <- var
  iid_ <- var
  e0_  <- var
  -- returned IDs
  lang_       <- var
  morlocName_ <- var
  sourceName_ <- var
  typeID_     <- var
  sourceFile_ <- var
  modulePath_ <- var

  -- Find all sources in this language
  triple_ sid_ PType OSource
  triple_ sid_ PLang lang_

  -- Narrow to the source that imports this particular function. From this we
  -- learn the source name of the morloc function.
  triple_ sid_ PImport iid_
  triple_ iid_ PName sourceName_
  triple_ iid_ PAlias morlocName_

  -- Find the source file if given. If we are importing a built-in function
  -- from the language, then there will not be a source file.
  optional_ $ do
    triple_ sid_ PPath sourceFile_

  -- Find the module path
  optional_ $ do
    triple_ mid_ PType OScript 
    triple_ mid_ PValue modulePath_
    triple_ mid_ PElem e0_
    triple_ e0_ PValue sid_

  optional_ $ do
    -- Get the morloc name and type ID from the type declaration
    triple_ tid_ PLeft morlocName_
    triple_ tid_ PRight typeID_

    -- Find every concrete type declaration
    triple_ tid_ PType OTypeDeclaration
    triple_ tid_ PLang lang_
    filterExpr ((str lang_) .!=. (ML.showLangName MorlocLang))

  selectVars
    [ morlocName_
    , lang_
    , sourceName_
    , typeID_
    , sourceFile_
    , modulePath_
    ]
