{-|
Module      : Morloc.Component.Declaration
Description : Map morloc names to morloc composition declarations
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Component.Declaration
(
    fromSparqlDb
  , hsparql
) where

import Morloc.Global
import Morloc.Operators
import Morloc.Sparql
import qualified Morloc.Monad as MM
import qualified Morloc.Data.Text as MT
import qualified Morloc.TypeHandler as MTH

import qualified Data.Map.Strict as Map
import qualified Data.List.Extra as DLE

-- | Collect most of the info needed to build all manifolds
fromSparqlDb
  :: SparqlDatabaseLike db
  => db -> MorlocMonad (Map.Map Name FunctionDeclaration)
fromSparqlDb ep
  =   sparqlSelect "declaration" hsparql ep
  >>= mapM tuplify    -- ((Name, Key, Key), Name))
  |>> DLE.groupSort   -- ((Name, Key, Key), [Name])
  |>> map toObj       -- (Name, FunctionDeclaration)
  |>> DLE.groupSort   -- (Name, [FunctionDeclaration])
  |>> Map.fromList    -- Map Name [FunctionDeclaration]
  |>> Map.mapMaybe MTH.chooseDeclaration 
  >>= MM.logFile "declaration.txt"

tuplify :: [Maybe MT.Text] -> MorlocMonad ((Name, Key, Key, Bool), Name)
tuplify [ Just did
        , Just morlocName
        , Just cid
        , Just isExported
        , _
        , Just argName ] = return ((morlocName, did, cid, isExported == "true"), argName)
tuplify row = MM.throwError $ SparqlFail ("Bad query: " <> MT.show' row) 

toObj :: ((Name, Key, Key, Bool), [Name]) -> (Name, FunctionDeclaration)
toObj ((name, did, cid, exported), args)
  = (,) name
  $ FunctionDeclaration { fdId = did
                        , fdName = name
                        , fdCallId = cid
                        , fdExported = exported
                        , fdArgs = args
                        }

hsparql :: Query SelectQuery
hsparql = do
  did_ <- var -- data declaration URI
  cid_ <- var -- call URI
  morlocName_ <- var
  arg_ <- var
  arg_pos_ <- var
  arg_value_ <- var
  arg_name_ <- var
  export_id_ <- var
  is_exported_ <- var

  triple_ did_ PType ODataDeclaration
  triple_ did_ PLeft morlocName_

  optional_ $ do
    triple_ export_id_ PType OExport
    triple_ export_id_ PValue morlocName_

  bind (bound export_id_) is_exported_

  triple_ did_ PElem arg_
  triple_ arg_ PPosition arg_pos_
  triple_ arg_ PValue arg_value_
  triple_ arg_value_ PValue arg_name_

  triple_ did_ PRight cid_
  triple_ cid_ PType OCall

  orderNextAsc did_
  orderNextAsc arg_pos_ 

  selectVars
    [ did_
    , morlocName_
    , cid_
    , is_exported_
    , arg_pos_
    , arg_name_
    ]
