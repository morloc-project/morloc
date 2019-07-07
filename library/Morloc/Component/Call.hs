{-|
Module      : Morloc.Component.Call
Description : Map RDF URIs to their arguments and names
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Component.Call
(
    fromSparqlDb
  , hsparql
) where

import Morloc.Global
import Morloc.Operators
import Morloc.Sparql
import qualified Morloc.Monad as MM
import qualified Morloc.Data.Text as MT

import qualified Data.Map.Strict as Map
import qualified Data.List.Extra as DLE

-- | Collect most of the info needed to build all manifolds
fromSparqlDb
  :: SparqlDatabaseLike db
  => Map.Map URI MData -> db -> MorlocMonad (Map.Map URI Call)
fromSparqlDb datamap ep
  =   sparqlSelect "call" hsparql ep
  >>= mapM (tuplify datamap)
  |>> DLE.groupSort
  |>> map toObj
  |>> Map.fromList
  >>= MM.logFileWith "call.txt" Map.assocs

tuplify :: Map.Map URI MData -> [Maybe MT.Text] -> MorlocMonad ((URI, Name), Argument)
tuplify datamap [ Just cid
                , Just morlocName
                , argPos
                , argData
                , argCall
                , argName
                , isBound
                ]
  =   (,)
  <$> pure (URI cid, morlocName)
  <*> makeArgument (
        argName
      , fmap URI argCall
      , fmap URI argData >>= (flip Map.lookup) datamap
      , argPos >>= MT.readMay'
      , isBound == (Just "false") 
      )
tuplify _ row = MM.throwError $ SparqlFail ("Bad query: " <> MT.show' row) 

makeArgument
  :: ( Maybe Name    -- argument name (if it is a bound argument)
     , Maybe URI     -- argument callId (if it is a function call)
     , Maybe MData   -- argument data (if this is data)
     , Maybe Int     -- the element order
     , Bool
     )
  -> MorlocMonad Argument
makeArgument (Just x  , _       , _       , _      , True) = return $ ArgNest x
makeArgument (Just x  , _       , _       , _      , _   ) = return $ ArgName x
makeArgument (_       , Just x  , _       , _      , _   ) = return $ ArgCall x
makeArgument (_       , _       , Just x  , _      , _   ) = return $ ArgData x
makeArgument (Nothing , Nothing , Nothing , Just x , _   ) = return $ ArgPosi x
makeArgument _ = MM.throwError . InvalidRDF $ "Bad argument"

toObj :: ((URI, Name), [Argument]) -> (URI, Call)
toObj ((cid, name), args) = (cid, Call cid name args)

hsparql :: Query SelectQuery
hsparql = do
  call_id_ <- var -- call URI
  call_value_ <- var
  morloc_name_ <- var
  e_ <- var
  arg_id_ <- var
  arg_pos_ <- var
  arg_data_id_ <- var
  arg_call_id_ <- var
  arg_name_ <- var
  bound_ <- var

  triple_ call_id_ PType OCall
  triple_ call_id_ PValue call_value_ 
  triple_ call_value_ PType OName
  triple_ call_value_ PValue morloc_name_

  triple_ call_id_ PElem e_
  triple_ e_ PPosition arg_pos_
  triple_ e_ PValue arg_id_

  --  A argument must be one of the following:
  --  1. raw data
  optional_ $ do
    triple_ arg_id_ PType OData
    bind (expr arg_id_) arg_data_id_

  -- 2. a function call
  optional_ $ do
    triple_ arg_id_ PType OCall
    bind (expr arg_id_) arg_call_id_

  -- 3. a name - the name can only come from one of the bound variables
  optional_ $ do
    filterExpr (bound arg_id_)
    triple_ arg_id_ PType OName
    triple_ arg_id_ PValue arg_name_
    triple_ arg_id_ PBound bound_

  orderNextAsc call_id_
  orderNextAsc arg_pos_ 

  selectVars
    [ call_id_
    , morloc_name_
    , arg_pos_
    , arg_data_id_
    , arg_call_id_
    , arg_name_
    , bound_
    ]
