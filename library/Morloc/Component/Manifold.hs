{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Component.Manifold
Description : Build manifolds for code generation from a SPARQL endpoint.
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Component.Manifold
(
    fromSparqlDb
  , hsparql
) where

import Morloc.Global
import Morloc.Operators
import Morloc.Sparql
import qualified Morloc.System as MS
import qualified Morloc.Util as MU
import qualified Morloc.Data.RDF as MR
import qualified Morloc.Component.MType as CompMType
import qualified Morloc.Component.MData as CompMData
import qualified Morloc.Component.Call as CompCall
import qualified Morloc.Component.Realization as CompReal
import qualified Morloc.Component.Declaration as CompDecl
import qualified Morloc.Component.Util as MCU 
import qualified Morloc.Manifold as Man
import qualified Morloc.Monad as MM
import qualified Morloc.Data.Text as MT
import qualified Morloc.Language as ML

import qualified Data.Map.Strict as Map
import qualified Data.List.Extra as DLE
import qualified Data.Maybe as DM
import qualified Safe as S

-- | Collect most of the info needed to build all manifolds
fromSparqlDb
  :: SparqlDatabaseLike db
  => db -> MorlocMonad [Manifold]
fromSparqlDb ep = do
  -- map from type ID to MType
  typemap <- CompMType.fromSparqlDb ep
  -- map from data ID to data representation
  datamap <- CompMData.fromSparqlDb ep
  -- map call ID to call name and arguments
  callmap <- CompCall.fromSparqlDb datamap ep
  -- map morlocName to abstract type
  let absfmap = CompMType.makeAbstractFunctionMap typemap
  MM.logFileWith "absfmap.txt" Map.assocs absfmap
  -- map morlocName to function declaration (if given)
  declmap <- CompDecl.fromSparqlDb ep
  -- map morlocName to concrete type
  realmap <- CompReal.fromSparqlDb typemap ep
  sparqlSelect "manifold" hsparql ep
    >>= mapM (asManifold absfmap realmap callmap declmap (reverseCall callmap))
    |>> rewireExports declmap
    |>> addUniqueIds
    >>= mapM (makeReal realmap)
    >>= Man.uniqueRealization
    >>= MM.logFile "manifold.txt"

asManifold
  :: (Map.Map Name MType)  -- to mAbstractType
  -> (Map.Map Name [Realization])  -- to mRealizations
  -> (Map.Map Key Call)
  -> (Map.Map Name FunctionDeclaration)
  -> (Map.Map Key Key)
  -> [Maybe MT.Text]
  -> MorlocMonad Manifold
asManifold absfmap realmap callmap declmap revmap
           [Just mid, Just name, Just exported]
  = case ( Map.lookup name absfmap
         , maybe [] id (Map.lookup name realmap)
         , Map.lookup mid callmap
         , Map.lookup name declmap
         ) of
  (abstractType, realizations, call, decl) -> do
    let composition
          = S.headMay
          . Map.elems
          . Map.map fdName
          $ Map.filter (\m -> fdCallId m == mid) declmap
    args <- chooseArguments call decl abstractType realizations
    return $ Manifold {
        mid = 0 -- set this later
      , mCallId = mid
      , mAbstractType = abstractType
      , mRealizations = realizations
      , mMorlocName = name
      , mExported = exported == "true"
      , mCalled = DM.isJust call 
      , mDefined = DM.isJust decl
      , mComposition = composition
      , mBoundVars = chooseBoundArguments mid declmap revmap
      , mArgs = args
      }
asManifold _ _ _ _ _ x = MM.throwError . SparqlFail $ "Unexpected SPARQL row:\n" <> MT.pretty x

chooseBoundArguments
  :: Key
  -> (Map.Map Key FunctionDeclaration)
  -> (Map.Map Key Key)
  -> [Name]
chooseBoundArguments k declmap revmap
  = case filter (\d -> fdCallId d == firstCall) (Map.elems declmap) of
    [fd] -> fdArgs fd
    _ -> []
    where
      firstCall = findAncestor revmap k

findAncestor :: (Map.Map Key Key) -> Key -> Key
findAncestor m k = case Map.lookup k m of
  (Just parent) -> findAncestor m parent
  Nothing -> k

-- | Reverse the call graph, creating a map from argument ID to parent call ID.
-- This map can be used to find the top-level declaration of a particular call.
reverseCall
  :: (Map.Map Key Call)
  -- ^ a map from callID to Call object
  -> (Map.Map Key Key)
  -- ^ a map from the ArgCall IDs in Call arguments back to the callID
reverseCall
  = Map.fromList
  . concat
  . map (\(Call k _ args) -> calls' k args)
  . Map.elems
  where
    calls' :: Key -> [Argument] -> [(Key, Key)]
    calls' parent args' = [(child, parent) | (ArgCall child) <- args']

chooseArguments
  :: Maybe Call
  -> Maybe FunctionDeclaration
  -> Maybe MType
  -> [Realization]
  -> MorlocMonad [Argument]
chooseArguments (Just call) _ _ _ = return $ callArgs call
chooseArguments _ (Just decl) _ _ = return . makePosi . length . fdArgs $ decl
chooseArguments _ _ (Just ((MFuncType _ xs _))) _ = return . makePosi . length $ xs
chooseArguments _ _ _ [] = return []
chooseArguments _ _ _ rs = do
  mapM
      countArgs
      (DM.mapMaybe rConcreteType rs)
  >>= consensus |>> makePosi
  where
    countArgs :: MType -> MorlocMonad Int
    countArgs (MFuncType _ xs _) = return $ length xs
    countArgs _  = MM.throwError $ TypeError "Expected function type"

    consensus :: [Int] -> MorlocMonad Int
    consensus [] = return 0
    consensus (x:xs) = case all ((==) x) xs of
      True -> return x
      False -> MM.throwError . TypeError $
        ("Type signatures disagree on argument number" <> MT.show' (x:xs))

makePosi :: Int -> [Argument]
makePosi i
  | i <= 0 = []
  | otherwise = map ArgPosi [1..i]

-- | fix exports from compositions
rewireExports :: (Map.Map Key FunctionDeclaration) -> [Manifold] -> [Manifold]
rewireExports declmap ms = map rewireExport ms
  where
    rewireExport m =
      let d = mComposition m >>= (flip Map.lookup) declmap
      in
        case
          ( d
          , fmap fdCallId d == (Just (mCallId m)) -- is m the direct child of d
          , mDefined m
          , mExported m
          )
        of
          -- set composition head to export status of composition
          (Just d, True, _, _) -> m { mExported = fdExported d }
          -- set composition export status to false (head is exported)
          (_, _, True, True) -> m { mExported = False }
          -- if something is neither, then leave it unchanged
          _ -> m


-- | Add a unique index to each manifold. The indices will be used to link
-- manifolds in the function-call graph. They will also be used to uniquely
-- identify the generated functions in the output code.
addUniqueIds :: [Manifold] -> [Manifold]
addUniqueIds ms = [m {mid = i} | (m, i) <- zip ms [1..]]

makeReal :: Map.Map Name [Realization] -> Manifold -> MorlocMonad Manifold
makeReal rmap m = case Map.lookup (mMorlocName m) rmap of
  (Just rs) -> return $ m {mRealizations = rs}
  Nothing -> return $ m {mRealizations = []}

hsparql :: Query SelectQuery
hsparql = do
  mid_ <- var
  call_id_ <- var
  mid2_ <- var
  call_id2_ <- var
  morloc_name_ <- var
  is_exported_ <- var
  script_id_ <- var

  union_
    ( do
        triple_ mid_ PType OCall
        triple_ mid_ PValue call_id_
        triple_ call_id_ PType OName
        triple_ call_id_ PValue morloc_name_
    )
    ( do
        triple_ mid_ PType OExport
        triple_ mid_ PValue morloc_name_
        triple_ script_id_ PValue mid_
    )

  bind (bound script_id_) is_exported_

  selectVars
    [ mid_
    , morloc_name_
    , is_exported_
    ]
