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
import qualified Morloc.Component.MType as MCT 
import qualified Morloc.Component.MData as MCD 
import qualified Morloc.Component.Util as MCU 
import qualified Morloc.Monad as MM
import qualified Morloc.Data.Text as MT
import qualified Morloc.Language as ML

import qualified Data.Map.Strict as Map
import qualified Data.List.Extra as DLE

-- | Collect most of the info needed to build all manifolds
fromSparqlDb
  :: SparqlDatabaseLike db
  => db -> MorlocMonad [Manifold]
fromSparqlDb ep = do
  typemap <- MCT.fromSparqlDb ep
  datamap <- MCD.fromSparqlDb ep
  sparqlSelect "manifold" hsparql ep
    >>= mapM (asTuple typemap datamap)
    |>> propagateBoundVariables
    |>> DLE.groupSort
    |>> setLangs
    >>= setCalls
    >>= unroll
    >>= MM.logFile "manifolds.txt"

asTuple
  :: Map.Map Key MType
  -> Map.Map Key MData
  -> [Maybe MT.Text]
  -> MorlocMonad (Manifold, Either Key Argument)
asTuple typemap datamap [ Just callId'
                        , abstractTypeId'
                        , element'
                        , Just morlocName'
                        , sourceName'
                        , composition'
                        , bvars'
                        , sourceLang'
                        , sourcePath'
                        , Just called'
                        , Just sourced'
                        , Just exported'
                        , concreteTypeId'
                        , argname'
                        , argcall_id'
                        , argdata_id'
                        , modulePath'
                        ] = do
  arg <- makeArgument (
      argname'
    , argcall_id'
    , argdata_id' >>= (flip Map.lookup) datamap
    , element'
    )
  langM <- sequence . fmap MM.readLang $ sourceLang'
  let man = Manifold {
        mid           = 0 -- will set later
      , mCallId       = callId'
      , mAbstractType = abstractTypeId' >>= (flip Map.lookup) typemap
      , mConcreteType = concreteTypeId' >>= (flip Map.lookup) typemap
      , mMorlocName   = morlocName'
      , mCallName     = maybe morlocName' id sourceName'
      , mSourceName   = sourceName'
      , mComposition  = composition'
      , mCalled       = called'   == "true"
      , mExported     = exported' == "true"
      , mSourced      = sourced'  == "true"
      , mSourcePath   = sourcePath'
      , mModulePath   = modulePath'
      , mBoundVars    = maybe [] (MT.splitOn ",") bvars'
      , mLang         = langM
      , mArgs         = [] -- this will be set in the next step
      }
  return (man, arg)

asTuple _ _ x = MM.throwError . SparqlFail $ "Unexpected SPARQL row:\n" <> MT.pretty x

setCalls :: [(Manifold, [Either Key Argument])] -> MorlocMonad [Manifold]
setCalls xs = mapM setArgs xs'
  where
    -- set the IDs for each manifold
    xs' = zipWith (\i (m, x) -> (m {mid = i}, x)) [1..] xs

    hash :: Map.Map Key Manifold
    hash = Map.fromList $ zip (map (mCallId . fst) xs') (map fst xs')

    setArgs :: (Manifold, [Either Key Argument]) -> MorlocMonad Manifold 
    setArgs (m, args) = do
      args' <- mapM setArg args
      return $ m { mArgs = args' }

    setArg :: Either Key Argument -> MorlocMonad Argument
    setArg (Left key) = case Map.lookup key hash of
      (Just m) -> return $ ArgCall m
      Nothing -> MM.throwError . InvalidRDF $ "Call to non-existing manifold"
    setArg (Right a) = return a

setLangs :: [(Manifold, a)] -> [(Manifold, a)]
setLangs ms = map (setLang hash) ms
  where
    setLang :: (Map.Map Name Lang) -> (Manifold, a) -> (Manifold, a)
    setLang h (m, a) = (m { mLang = Map.lookup (mMorlocName m) h }, a)

    hash :: Map.Map Name Lang
    hash = MU.spreadAttr (map (\(m,_) -> ( mMorlocName m
                                         , mComposition m
                                         , mLang m)) ms)

makeArgument
  :: ( Maybe Name    -- argument name (if it is a bound argument)
     , Maybe Key     -- argument callId (if it is a function call)
     , Maybe MData   -- argument data (if this is data)
     , Maybe MT.Text -- the element (rdf:_<num>)
     )
  -> MorlocMonad (Either Key Argument)
makeArgument (Just x  , _       , _       , _ ) = return . Right $ ArgName x
makeArgument (_       , Just x  , _       , _ ) = return . Left $ x
makeArgument (_       , _       , Just x  , _ ) = return . Right $ ArgData x
makeArgument (Nothing , Nothing , Nothing , Just e) =
  case (MT.stripPrefix (MR.rdfPre <> "_") e) >>= MT.readMay' of
    Just i -> return . Right $ ArgPosi i
    _ -> MM.throwError . InvalidRDF $ "Unexpected value for element: " <> MT.pretty e
makeArgument _ = MM.throwError . InvalidRDF $ "Bad argument"

propagateBoundVariables :: [(Manifold, Either Key Argument)] -> [(Manifold, Either Key Argument)]
propagateBoundVariables ms = map setBoundVars ms 
  where
    setBoundVars :: (Manifold, Either Key Argument) -> (Manifold, Either Key Argument)
    setBoundVars (m, a) = (m { mBoundVars = maybe [] id (Map.lookup (mCallId m) hash) }, a)

    -- map from mCallId to mBoundVars
    hash :: Map.Map Key [Name]
    hash = MU.spreadAttr (map toTriple ms)

    toTriple :: (Manifold, Either Key Argument) -> (Key, Maybe Key, Maybe [Name])
    toTriple (m, Left k) = (mCallId m, Just k, toMaybe (mBoundVars m))
    toTriple (m, _) = (mCallId m, Nothing, toMaybe (mBoundVars m))

    toMaybe :: [a] -> Maybe [a]
    toMaybe [] = Nothing
    toMaybe xs = Just xs

-- | This function creates a tree of new manifolds to represent the call tree
-- of a called Morloc composition.
unroll :: [Manifold] -> MorlocMonad [Manifold]
unroll ms = fmap concat (mapM unroll' ms)
  where
    unroll' :: Manifold -> MorlocMonad [Manifold]
    unroll' m
      | (mCalled m) && (not $ mSourced m) =
          case
            filter (declaringManifold m) ms
          of
            [r] -> unrollPair m r
            xs  -> MM.throwError . InvalidRDF $ MT.unlines
              [ "In this manifold:"
              , MT.pretty m
              , "Expected to find one associated DataDeclaration, but found:"
              , MT.pretty xs
              ]
      | otherwise = return [m]

    unrollPair :: Manifold -> Manifold -> MorlocMonad [Manifold]
    unrollPair m r = do
      let r' = r { mExported = False }
      fmap ((:) m) (unroll' r')

    declaringManifold :: Manifold -> Manifold -> Bool
    declaringManifold m n = (Just (mMorlocName m) == mComposition n)

-- | 
hsparql :: Query SelectQuery
hsparql = do
  abstractTypeId_ <- var
  argcallId_      <- var
  argdataId_      <- var
  argname_        <- var
  bnd_            <- var
  bvars_          <- var
  mid_            <- var
  called_         <- var
  composition_    <- var
  concreteTypeId_ <- var
  element_        <- var
  exported_       <- var
  morlocName_     <- var
  sourceLang_     <- var
  sourceName_     <- var
  sourcePath_     <- var
  sourced_        <- var
  modulePath_     <- var

  subQuery_ $ do
    arg_           <- var
    bndElement_    <- var
    bndId_         <- var
    callIdType_    <- var
    datadec_       <- var
    dectypeId_     <- var
    e_             <- var
    fid_           <- var
    importId_      <- var
    langTypedec_   <- var
    sourceId_      <- var
    typedec_       <- var
    typeid_        <- var
    scriptElement_ <- var
    scriptId_      <- var
    mid'_ <- var
    mid''_ <- var

    -- Something can be exported if it is in the export list AND
    --  * Case 1: sourced and typed
    --  * Case 2: it is the name of a function declaration
    --  All other cases currently should raise errors. Old C-loc could actually
    --  export any manifold in a workflow. This was cool, since it allowed user
    --  querying of intermediate elements. In the future, I should add this
    --  handling back in, however I currently lack the syntax for it.

    union_
      -- find manifolds that are 1) exported, 2) typed, and 3) sourced
      ( do
          -- Find exported values
          triple_ mid_ PType OExport
          triple_ mid_ PValue morlocName_

          triple_ typedec_ PType OTypeDeclaration
          triple_ typedec_ PLang (ML.showLangName MorlocLang)
          triple_ typedec_ PLeft morlocName_
          triple_ typedec_ PRight typeid_

          triple_ sourceId_ PType OSource
          triple_ sourceId_ PImport importId_
          triple_ importId_ PAlias morlocName_

          triple_ scriptId_ PType OScript
          triple_ scriptId_ PValue modulePath_
          triple_ scriptId_ scriptElement_ sourceId_

          -- one return for each argument (input type)
          triple_ typeid_ element_ arg_
          MCU.isElement_ element_

          -- Keep only the values that are NOT calls (to avoid duplication)
          triple_ mid_ PType callIdType_
          filterExpr (str callIdType_ .!=. OCall)

          -- export case #1 (see note above)
          optional_ $ do
            triple_ e_ PType OExport
            triple_ e_ PValue morlocName_

      )
      -- find manifolds that are calls (this includes compositions)
      ( do
          triple_ mid_ PType OCall
          triple_ mid_ PValue fid_
          triple_ mid_ element_ arg_
          MCU.isElement_ element_

          -- -- This does not work, since there is generally not a set path
          -- -- to the call (can be nested arbitrarily deep)
          -- triple_ scriptId_ PType OScript
          -- triple_ scriptId_ PValue modulePath_
          -- triple_ scriptId_ scriptElement_ mid_
          -- MCU.isElement_ scriptElement_

          triple_ fid_ PType OName
          triple_ fid_ PValue morlocName_
      )

    -- # Find the bound variables
    optional_ $ do
      triple_ datadec_ PType ODataDeclaration
      triple_ datadec_ PLeft composition_
      triple_ datadec_ PRight mid_
      triple_ datadec_ bndElement_ bndId_
      MCU.isElement_ bndElement_

      triple_ bndId_ PType OName
      triple_ bndId_ PValue bnd_ -- bound variables

      -- export case #2 (see note above)
      optional_ $ do    
        triple_ e_ PType OExport
        triple_ e_ PValue composition_

    ------------- BAD DAMN IT ALL SUCK BALLS -----------
    -- -- # Find source name
    -- --   This case SHOULD be mutually exclusive with the next case
    -- optional_ $ do
    --   triple_ datadec_ PType ODataDeclaration
    --   triple_ datadec_ PLeft morlocName_
    --   triple_ datadec_ PRight mid'_
    --   triple_ mid'_ PType OCall
    --   triple_ mid'_ PValue mid''_
    --   triple_ mid''_ PType OName
    --   triple_ mid''_ PValue sourceName_
    ------------- BAD DAMN IT ALL SUCK BALLS -----------

    -- # Find the source language
    optional_ $ do
      triple_ sourceId_ PType OSource
      triple_ sourceId_ PLang sourceLang_
      triple_ sourceId_ PImport importId_

      triple_ importId_ PName sourceName_
      triple_ importId_ PAlias morlocName_

      optional_ $ do
        triple_ sourceId_ PPath sourcePath_

    -- Find language-specific type signature, packer, and unpacker
    optional_ $ do
      triple_ langTypedec_ PType OTypeDeclaration
      triple_ langTypedec_ PLang sourceLang_
      triple_ langTypedec_ PLeft morlocName_
      triple_ langTypedec_ PRight concreteTypeId_

      filterExpr ((str sourceLang_) .!=. (ML.showLangName MorlocLang))

    -- Find the type declaration ID
    optional_ $ do
      triple_ dectypeId_ PType OTypeDeclaration
      triple_ dectypeId_ PLang (ML.showLangName MorlocLang)
      triple_ dectypeId_ PLeft morlocName_
      triple_ dectypeId_ PRight abstractTypeId_

    --  A argument must be one of the following:
    --  1. raw data
    optional_ $ do
      triple_ arg_ PType OData
      bind (expr arg_) argdataId_

    -- 2. a function call
    optional_ $ do
      triple_ arg_ PType OCall
      bind (expr arg_) argcallId_

    -- 3. a name - the name can only come from one of the bound variables
    optional_ $ do
      filterExpr (bound arg_)
      triple_ arg_ PType OName
      triple_ arg_ PValue argname_

    bind (bound fid_) called_
    bind (bound e_) exported_
    bind (bound sourceId_) sourced_

    orderNextAsc bndElement_
    selectVars
      [ abstractTypeId_
      , argdataId_
      , argcallId_
      , argname_
      , bnd_
      , mid_
      , called_
      , composition_
      , concreteTypeId_
      , element_
      , exported_
      , morlocName_
      , sourceLang_
      , sourceName_
      , sourcePath_
      , sourced_
      , modulePath_
      ]

  groupBy abstractTypeId_
  groupBy argcallId_
  groupBy argdataId_
  groupBy argname_
  groupBy mid_
  groupBy called_
  groupBy composition_
  groupBy concreteTypeId_
  groupBy element_
  groupBy exported_
  groupBy morlocName_
  groupBy sourceLang_
  groupBy sourceName_
  groupBy sourcePath_
  groupBy sourced_
  groupBy modulePath_

  orderNextAsc mid_
  orderNextAsc element_ 

  select
    [ SelectVar  mid_
    , SelectVar  abstractTypeId_
    , SelectVar  element_
    , SelectVar  morlocName_
    , SelectVar  sourceName_
    , SelectVar  composition_
    , SelectExpr (groupConcat bnd_ ",") bvars_
    , SelectVar  sourceLang_
    , SelectVar  sourcePath_
    , SelectVar  called_
    , SelectVar  sourced_
    , SelectVar  exported_
    , SelectVar  concreteTypeId_
    , SelectVar  argname_
    , SelectVar  argcallId_
    , SelectVar  argdataId_
    , SelectVar  modulePath_
    ]
