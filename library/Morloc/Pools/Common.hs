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
  , makeCisManifolds
  , makeSourceManifolds
  , getUsedManifolds
) where

import qualified Morloc.Config as MC
import Morloc.Types
import Morloc.Data.Doc hiding ((<$>))
import qualified Morloc.Error as ME
import qualified Morloc.Component.Serializer as Serializer
import qualified Morloc.Component.Manifold as Manifold
import qualified Morloc.System as MS
import qualified Morloc.Monad as MM
import qualified Morloc.Data.Text as MT
import qualified Data.Map.Strict as Map

data Grammar = Grammar {
      gLang     :: MT.Text
    , gAssign   :: Doc -> Doc -> Doc
    , gCall     :: Doc -> [Doc] -> Doc
    , gFunction :: Doc -> [Doc] -> Doc -> Doc
    , gComment  :: Doc -> Doc
    , gReturn   :: Doc -> Doc
    , gQuote    :: Doc -> Doc
    , gImport   :: Doc -> Doc -> Doc
    , gList     :: [Doc] -> Doc
    , gTuple    :: [Doc] -> Doc
    , gRecord   :: [(Doc,Doc)] -> Doc
    , gTrue     :: Doc
    , gFalse    :: Doc
    , gIndent   :: Doc -> Doc
    , gUnpacker :: UnpackerDoc -> Doc
    , gTry      :: TryDoc -> Doc
    , gForeignCall :: ForeignCallDoc -> Doc
  }

data ManifoldClass
  = Cis -- ^ Wrapper around a Morloc composition
  | Trans
  | Source -- ^ Wrapper around a source function
  | Uncalled -- ^ Does not need to be built in current language
  deriving(Show, Ord, Eq)

data TryDoc = TryDoc {
      tryCmd :: Doc
    , tryRet :: Doc
    , tryArgs :: [Doc]
    , tryMid :: Doc
    , tryFile :: Doc
  }

data UnpackerDoc = UnpackerDoc {
      udValue :: Doc
    , udUnpacker :: Doc
    , udMid :: Doc
    , udFile :: Doc
  }

data ForeignCallDoc = ForeignCallDoc {
      fcdForeignProg :: Doc
    , fcdForeignPool :: Doc
    , fcdMid :: Doc
    , fcdArgs :: [Doc]
    , fcdFile :: Doc
  }

makeGenerator
  :: Grammar
  -> (db -> MorlocMonad Code)
  -> db
  -> MorlocMonad Script
makeGenerator g gen
  = \ep ->
          Script
      <$> pure "pool"
      <*> pure (MT.unpack (gLang g))
      <*> gen ep

defaultCodeGenerator
  :: (SparqlDatabaseLike db)
  => Grammar
  -> (MT.Text -> MorlocMonad Doc) -- source name parser
  -> ([Doc] -> [Manifold] -> SerialMap -> MorlocMonad Doc) -- main
  -> db
  -> MorlocMonad Code
defaultCodeGenerator g f main ep = do
  manifolds <- Manifold.fromSparqlDb ep
  packMap <- Serializer.fromSparqlDb (gLang g) ep
  srcs <- mapM f (serialSources packMap)
  doc <- main srcs manifolds packMap
  return $ render doc

makeSourceManifolds :: Grammar -> SerialMap -> [Manifold] -> Doc
makeSourceManifolds g h ms
  = vsep . concat $ map decide' ms
  where
    decide' m = case determineManifoldClass g m of
      Source -> return $ makeSourceManifold g h m
      _ -> []

-- | inifinite list of named variables
iArgs :: Doc -> [Doc]
iArgs prefix = zipWith (<>) (repeat prefix) (map int [0..])

determineManifoldClass :: Grammar -> Manifold -> ManifoldClass
determineManifoldClass g m
  | mLang m == Just (gLang g)
      && not (mCalled m)
      && mSourced m
      && mExported m = Source
  | not (mCalled m) && mExported m = Uncalled
  | mLang m == Just (gLang g) = Cis 
  | (mLang m /= Just (gLang g)) && mCalled m = Trans
  | otherwise = error "Unexpected manifold class"

makeCisManifolds :: MC.Config -> Grammar -> SerialMap -> [Manifold] -> Doc
makeCisManifolds c g h ms = vsep . concat $ map decide' ms
  where
    decide' m = case determineManifoldClass g m of
      Cis -> return $ makeCisManifold c g h m
      _ -> []

makeSourceManifold :: Grammar -> SerialMap -> Manifold -> Doc
makeSourceManifold g h m
  =  ((gComment g) "source manifold") <> line
  <> ((gComment g) (fname m <> " :: " <> maybe "undefined" mshow (mAbstractType m))) <> line
  <> ((gFunction g)
       (callIdToName m)
       (take n (iArgs "x"))
       (
            vsep (zipWith3 unpack' (iArgs "a") argTypes (iArgs "x")) <> line
         <> (gReturn g) ((gCall g) (fname m) (take n (iArgs "a")))
       ))
  where
    n = length (mArgs m)

    argTypes :: [(Doc, Argument)] -- unpacker and argument
    argTypes = zip (getUnpackers h m) (mArgs m)

    unpack' :: Doc -> (Doc, Argument) -> Doc -> Doc
    unpack' lhs (u, _) x
      = (gAssign g) lhs ((gUnpacker g) (UnpackerDoc {
              udValue = x   
            , udUnpacker = u
            , udMid = callIdToName m
            , udFile = text' (MS.makePoolName (gLang g))
          }))

makeCisManifold :: MC.Config -> Grammar -> SerialMap -> Manifold -> Doc
makeCisManifold c g h m
  =  ((gComment g) "cis manifold") <> line
  <> ((gComment g) (fname m <> " :: " <> maybe "undefined" mshow (mAbstractType m))) <> line
  <> ((gFunction g)
       (callIdToName m)
       (map text' (mBoundVars m))
       (
            vsep (zipWith makeArg (iArgs "a") argTypes) <> line
         <> (gReturn g) ((gCall g) (fname m) (take n (iArgs "a")))
       ))
  where
    n = length (mArgs m)

    argTypes :: [(Doc, Argument)] -- unpacker and argument
    argTypes = zip (getUnpackers h m) (mArgs m)

    makeArg :: Doc -> (Doc, Argument) -> Doc
    makeArg lhs b = (gAssign g) lhs (makeArg' b)

    makeArg' :: (Doc, Argument) -> Doc
    makeArg' (u, arg) =
      if
        useUnpacker g arg m
      then
        unpack' u (writeArgument c g (mBoundVars m) arg)
      else
        (writeArgument c g (mBoundVars m) arg)

    useUnpacker :: Grammar -> Argument -> Manifold -> Bool
    useUnpacker _  (ArgName n') m' = elem n' (mBoundVars m')
    useUnpacker g' (ArgCall m') _  = (mLang m') /= (Just (gLang g'))
    useUnpacker _  (ArgData _)  _  = False
    useUnpacker _  (ArgPosi _)  _  = True

    unpack' :: Doc -> Doc -> Doc
    unpack' p x
      = ((gUnpacker g) (UnpackerDoc {
              udValue = x
            , udUnpacker = p
            , udMid = callIdToName m
            , udFile = text' (MS.makePoolName (gLang g))
          }))


-- find a packer for each argument
getUnpackers :: SerialMap -> Manifold -> [Doc]
getUnpackers h m = case mConcreteType m of
  (Just (MFuncType _ ts _)) -> map (unpackerName h . return) ts 
  (Just _) -> ME.error' ("Expected a function type for:" <> MT.pretty m)
  Nothing -> take (length (mArgs m)) (repeat (unpackerName h Nothing))
  where
    unpackerName :: SerialMap -> Maybe MType -> Doc 
    unpackerName h' n' = case (n' >>= (flip Map.lookup) (serialUnpacker h')) of 
      (Just f) -> text' f
      Nothing  -> text' (serialGenericUnpacker h')

callIdToName :: Manifold -> Doc
callIdToName m = text' $ MS.makeManifoldName (mCallId m)

-- | writes an argument sans serialization 
writeArgument :: MC.Config -> Grammar -> [MT.Text] -> Argument -> Doc
writeArgument _ _ _  (ArgName n) = text' n
writeArgument _ g _  (ArgData d) = writeData g d
writeArgument _ _ _  (ArgPosi i) = "x" <> int i
writeArgument c g xs (ArgCall m) = case mLang m of
  (Just l) ->
    if
      l == gLang g
    then
      (gCall g) (callIdToName m) (map text' xs)
    else
      case (MC.getExecutor c l) of
        (Just exe) -> (gForeignCall g) (ForeignCallDoc {
              fcdForeignProg = text' exe
            , fcdForeignPool = text' (MS.makePoolName l)
            , fcdMid = text' $ MS.makeManifoldName (mCallId m)
            , fcdArgs = map text' xs
            , fcdFile = text' (MS.makePoolName (gLang g))
          })
        Nothing -> error ("No command could be found to run language " ++ MT.unpack l)
  Nothing -> error ("No language set on: " ++ show m)

writeData :: Grammar -> MData -> Doc
writeData _ (Num' x)     = text' x
writeData g (Str' x)     = (gQuote g) (text' x)
writeData g (Log' True)  = gTrue g
writeData g (Log' False) = gFalse g
writeData g (Lst' xs)    = (gList g) (map (writeData g) xs)
writeData g (Tup' xs)    = (gTuple g) (map (writeData g) xs)
writeData g (Rec' xs)    = (gRecord g) (map (\(k, v) -> (text' k, writeData g v)) xs)

getUsedManifolds :: Grammar -> [Manifold] -> [Doc]
getUsedManifolds g ms = map callIdToName (filter isBuilt ms)
  where
    isBuilt :: Manifold -> Bool
    isBuilt m = case determineManifoldClass g m of
      Cis -> True
      Source -> True
      _ -> False

fname :: Manifold -> Doc
fname m = text' (mCallName m)
