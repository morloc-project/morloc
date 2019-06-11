{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Pools.Common
Description : A common set of utility functions for language templates
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Pools.Common
(
    Grammar(..)
  , TryDoc(..)
  , UnpackerDoc(..)
  , ForeignCallDoc(..)
  , makeGenerator
  , defaultCodeGenerator
) where

import Morloc.Global
import Morloc.Operators hiding ((<>))
import Morloc.Data.Doc hiding ((<$>))
import qualified Morloc.Language as ML
import qualified Morloc.Config as MC
import qualified Morloc.Component.Serializer as Serializer
import qualified Morloc.Component.Manifold as Manifold
import qualified Morloc.Monad as MM
import qualified Morloc.Manifold as Man
import qualified Morloc.Data.Text as MT
import qualified Data.Map.Strict as Map

data Grammar = Grammar {
      gLang        :: Lang
    , gAssign      :: Doc -> Doc -> Doc
    , gCall        :: Doc -> [Doc] -> Doc
    , gFunction    :: Doc -> [Doc] -> Doc -> Doc
    , gId2Function :: Integer -> Doc
    , gComment     :: Doc -> Doc
    , gReturn      :: Doc -> Doc
    , gQuote       :: Doc -> Doc
    , gImport      :: Doc -> Doc -> Doc
    , gList        :: [Doc] -> Doc
    , gTuple       :: [Doc] -> Doc
    , gRecord      :: [(Doc,Doc)] -> Doc
    , gTrue        :: Doc
    , gFalse       :: Doc
    , gIndent      :: Doc -> Doc
    , gUnpacker    :: UnpackerDoc -> Doc
    , gTry         :: TryDoc -> Doc
    , gForeignCall :: ForeignCallDoc -> Doc
    , gHash        :: (Manifold -> Doc) -> (Manifold -> Doc) -> [Manifold] -> Doc
    , gMain        :: [Doc] -> Doc -> Doc -> Doc -> Doc -> MorlocMonad Doc
  }

data TryDoc = TryDoc {
      tryCmd :: Doc    -- ^ The function we attempt to run
    , tryRet :: Doc    -- ^ A name for the returned variable?
    , tryArgs :: [Doc] -- ^ Arguments passed to function
    , tryMid :: Doc    -- ^ The name of the calling manifold (for debugging)
    , tryFile :: Doc   -- ^ The file where the issue occurs (for debugging)
  }

data UnpackerDoc = UnpackerDoc {
      udValue :: Doc    -- ^ The expression that will be unpacked
    , udUnpacker :: Doc -- ^ The function for unpacking the value
    , udMid :: Doc      -- ^ Manifold name for debugging messages
    , udFile :: Doc     -- ^ File name for debugging messages
  }

data ForeignCallDoc = ForeignCallDoc {
      fcdForeignPool :: Doc   -- ^ the name of the foreign pool (e.g., "R.pool")
    , fcdMid         :: Doc   -- ^ the function integer identifier
    , fcdArgs        :: [Doc] -- ^ CLI arguments passed to foreign function

    , fcdCall        :: [Doc] -- ^ make a list of CLI arguments from first two
                              -- inputs -- since fcdArgs will likely be
                              -- variables, they are not included in this call.
    , fcdFile        :: Doc   -- ^ for debugging
  }

makeGenerator
  :: Grammar
  -> (db -> MorlocMonad Code)
  -> db
  -> MorlocMonad Script
makeGenerator g gen
  = \ep ->
          Script
      <$> pure "pool" -- TODO remove hard-coded name
      <*> pure (gLang g)
      <*> gen ep

defaultCodeGenerator
  :: (SparqlDatabaseLike db)
  => Grammar
  -> (MT.Text -> MorlocMonad Doc) -- source name parser
  -> db
  -> MorlocMonad Code
defaultCodeGenerator g f ep = do
  manifolds <- Manifold.fromSparqlDb ep
  packMap <- Serializer.fromSparqlDb (gLang g) ep
  paksrcs <- mapM f (serialSources packMap)
  mansrcs <- Man.getManSrcs (gLang g) f manifolds
  lib <- MM.asks MC.configLibrary
  let srcs = map ((gImport g) (text' lib)) (paksrcs ++ mansrcs)
  srcManifolds <- makeSourceManifolds g packMap manifolds 
  cisManifolds <- makeCisManifolds g packMap (getMorlocFun2sourceFun manifolds) manifolds
  usedManifolds <- Man.getUsedManifolds (gLang g) manifolds
  let dispatchSerializerDict = (gHash g) (text' . MT.show' . mid) (Man.getPacker packMap) manifolds
  let dispatchFunDict = (gHash g) (text' . MT.show' . mid) (callIdToName g) usedManifolds
  doc <- (gMain g)
    srcs                   -- [Doc]
    srcManifolds           -- Doc
    cisManifolds           -- Doc
    dispatchFunDict        -- Doc
    dispatchSerializerDict -- Doc
  return $ render doc

-- | When a morloc function is declared, for example @foo x = sqrt(pow x 2)@,
-- then calling @foo@ from the nexus should call the source function @sqrt@ not
-- the morloc function @foo@ (since the morloc function does not really exist).
-- The @getMorlocFun2sourceFun@ function maps from morloc names (@foo@) to the
-- top-level source name (@sqrt@).
getMorlocFun2sourceFun :: [Manifold] -> Map.Map Name Manifold
getMorlocFun2sourceFun ms = Map.fromList
  [(n,m) | (Just n, m) <- (map (\m -> (mComposition m, m)) ms)]

callIdToName :: Grammar -> Manifold -> Doc
callIdToName g m = text' . MT.show' $ (gId2Function g) (mid m)

-- | inifinite list of named variables
iArgs :: Doc -> [Doc]
iArgs prefix = zipWith (<>) (repeat prefix) (map int [0..])

makeSourceManifolds :: Grammar -> SerialMap -> [Manifold] -> MorlocMonad Doc
makeSourceManifolds g h ms
  =   Man.filterByManifoldClass (gLang g) Source ms
  >>= mapM (makeSourceManifold g h)
  |>> vsep

makeCisManifolds
  :: Grammar
  -> SerialMap
  -> Map.Map MT.Text Manifold
  -> [Manifold]
  -> MorlocMonad Doc
makeCisManifolds g h cs ms
  =   Man.filterByManifoldClass (gLang g) Cis ms
  >>= mapM (makeCisManifold g h cs)
  |>> vsep

makeCisManifold
  :: Grammar
  -> SerialMap
  -> Map.Map MT.Text Manifold
  -> Manifold
  -> MorlocMonad Doc
makeCisManifold g h cs m = do
  argTypes <- zip <$> (Man.getUnpackers h m) <*> pure (mArgs m) -- [(Doc, Argument)]
  args <- sequence $ zipWith makeArg (iArgs "a") argTypes
  let name = callIdToName g m
  return
    $  ((gComment g) "cis manifold") <> line
    <> ((gComment g) (fname m <> " :: " <> maybe "undefined" mshow (mAbstractType m))) <> line
    <> ((gFunction g)
         name
         (map text' (mBoundVars m))
         (    vsep args <> line
           <> (gReturn g) ((gCall g) calledFunction (take n (iArgs "a")))
         ))
  where
    n = length (mArgs m)

    -- Get either the callName of the current manifold or, if the current
    -- manifold is a morloc function, get the name of the top-level manifold in
    -- the composition. 
    calledFunction :: Doc
    calledFunction = maybe (text' (mCallName m))
                           (callIdToName g)
                           (Map.lookup (mCallName m) cs)

    makeArg :: Doc -> (Doc, Argument) -> MorlocMonad Doc
    makeArg lhs b = (gAssign g) <$> pure lhs <*> (makeArg' b)

    makeArg' :: (Doc, Argument) -> MorlocMonad Doc
    makeArg' (u, arg) =
      if
        useUnpacker g arg m
      then
        (writeArgument g (mBoundVars m) arg) >>= unpack' u 
      else
        (writeArgument g (mBoundVars m) arg)

    useUnpacker :: Grammar -> Argument -> Manifold -> Bool
    useUnpacker _  (ArgName n') m' = elem n' (mBoundVars m')
    useUnpacker g' (ArgCall m') _  = (mLang m') /= (Just (gLang g'))
    useUnpacker _  (ArgData _)  _  = False
    useUnpacker _  (ArgPosi _)  _  = True

    unpack' :: Doc -> Doc -> MorlocMonad Doc
    unpack' p x = do
      let name = callIdToName g m 
      return ((gUnpacker g) (UnpackerDoc {
            udValue = x
          , udUnpacker = p
          , udMid = name
          -- TODO: remove hard-coded name
          , udFile = text' (ML.makeSourceName (gLang g) "pool")
        }))

makeSourceManifold :: Grammar -> SerialMap -> Manifold -> MorlocMonad Doc
makeSourceManifold g h m = do
  argTypes <- zip <$> (Man.getUnpackers h m) <*> pure (mArgs m)
  let name = callIdToName g m
  return
    $  ((gComment g) "source manifold") <> line
    <> ((gComment g) (fname m <> " :: " <> maybe "undefined" mshow (mAbstractType m))) <> line
    <> ((gFunction g)
         name
         (take n (iArgs "x"))
         (
              vsep (zipWith3 (unpack' name) (iArgs "a") argTypes (iArgs "x")) <> line
           <> (gReturn g) ((gCall g) (fname m) (take n (iArgs "a")))
         ))
  where
    n = length (mArgs m)

    unpack' :: Doc -> Doc -> (Doc, Argument) -> Doc -> Doc
    unpack' name lhs (u, _) x
      = (gAssign g) lhs ((gUnpacker g) (UnpackerDoc {
              udValue = x   
            , udUnpacker = u
            , udMid = name
            -- TODO: remove hard-coded name
            , udFile = text' (ML.makeSourceName (gLang g) "pool")
          }))

-- | writes an argument sans serialization 
writeArgument :: Grammar -> [MT.Text] -> Argument -> MorlocMonad Doc
writeArgument _ _  (ArgName n) = return $ text' n
writeArgument g _  (ArgData d) = return $ writeData g d
writeArgument _ _  (ArgPosi i) = return $ "x" <> int i
writeArgument g xs (ArgCall m) = do
  c <- MM.ask
  let name = callIdToName g m
  case mLang m of
    (Just l) ->
      if
        l == gLang g
      then
        return $ (gCall g) name (map text' xs)
      else
        case (
            MC.getPoolCallBuilder c l (gQuote g)
          , text' (ML.makeSourceName l "pool")
          , map text' xs
        ) of
          (Just poolBuilder, pool, args) -> return $
            (gForeignCall g) (ForeignCallDoc {
                fcdForeignPool = pool
              , fcdMid = name
              , fcdArgs = args
              , fcdCall = poolBuilder pool (integer $ mid m)
              , fcdFile = text' (ML.makeSourceName (gLang g) "pool")
            })
          (Nothing, _, _) -> MM.throwError . GeneratorError $
            "No command could be found to run language " <> (ML.showLangName l)
    Nothing -> MM.throwError . GeneratorError $
      "No language set on: " <> MT.show' m

writeData :: Grammar -> MData -> Doc
writeData _ (Num' x)     = text' x
writeData g (Str' x)     = (gQuote g) (text' x)
writeData g (Log' True)  = gTrue g
writeData g (Log' False) = gFalse g
writeData g (Lst' xs)    = (gList g) (map (writeData g) xs)
writeData g (Tup' xs)    = (gTuple g) (map (writeData g) xs)
writeData g (Rec' xs)    = (gRecord g) (map (\(k, v) -> (text' k, writeData g v)) xs)

fname :: Manifold -> Doc
fname m = text' (mCallName m)
