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

import Morloc.Types
import Morloc.Quasi
import qualified Morloc.Component.Serializer as Serializer
import qualified Morloc.Component.Manifold as Manifold
import Morloc.Builder hiding ((<$>))
import qualified Morloc.System as MS
import qualified Morloc.Text as MT
import qualified Data.Map.Strict as Map

data Grammar = Grammar {
      gLang     :: MT.Text
    , gAssign   :: Doc -> Doc -> Doc
    , gCall     :: Doc -> [Doc] -> Doc
    , gFunction :: Doc -> [Doc] -> Doc -> Doc
    , gComment  :: Doc -> Doc
    , gReturn   :: Doc -> Doc
    , gQuote    :: Doc -> Doc
    , gImport   :: Doc -> Doc
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
  -> CodeGenerator
  -> ScriptGenerator
makeGenerator g gen
  = \ep ->
          Script
      <$> pure "pool"
      <*> pure (MT.unpack (gLang g))
      <*> gen ep

defaultCodeGenerator
  :: Grammar
  -> (MT.Text -> Doc) -- source name parser
  -> ([Doc] -> [Manifold] -> SerialMap -> Doc) -- main
  -> CodeGenerator 
defaultCodeGenerator g f main ep = do
  manifolds <- Manifold.fromSparqlDb ep
  packMap <- Serializer.fromSparqlDb (gLang g) ep
  let srcs = map f (serialSources packMap)
  (return . render) $ main srcs manifolds packMap

makeSourceManifolds :: Grammar -> SerialMap -> [Manifold] -> Doc
makeSourceManifolds g h ms
  = vsep . concat $ map (decide' g h) ms
  where
    decide' g h m = case determineManifoldClass g m of
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

makeCisManifolds :: Grammar -> SerialMap -> [Manifold] -> Doc
makeCisManifolds g h ms
  = vsep . concat $ map (decide' g h) ms
  where
    decide' g h m = case determineManifoldClass g m of
      Cis -> return $ makeCisManifold g h m
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
    argTypes = zip (getUnpackers g h m) (mArgs m)

    unpack' :: Doc -> (Doc, Argument) -> Doc -> Doc
    unpack' lhs (u, _) x
      = (gAssign g) lhs ((gUnpacker g) (UnpackerDoc {
              udValue = x   
            , udUnpacker = u
            , udMid = callIdToName m
            , udFile = text' (MS.makePoolName (gLang g))
          }))

makeCisManifold :: Grammar -> SerialMap -> Manifold -> Doc
makeCisManifold g h m
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
    argTypes = zip (getUnpackers g h m) (mArgs m)

    makeArg :: Doc -> (Doc, Argument) -> Doc
    makeArg lhs b = (gAssign g) lhs (makeArg' b)

    makeArg' :: (Doc, Argument) -> Doc
    makeArg' (u, arg) =
      if
        useUnpacker g arg m
      then
        unpack' u (writeArgument g (mBoundVars m) arg)
      else
        (writeArgument g (mBoundVars m) arg)

    unpack' :: Doc -> Doc -> Doc
    unpack' p x
      = ((gUnpacker g) (UnpackerDoc {
              udValue = x
            , udUnpacker = p
            , udMid = callIdToName m
            , udFile = text' (MS.makePoolName (gLang g))
          }))


-- find a packer for each argument
getUnpackers :: Grammar -> SerialMap -> Manifold -> [Doc]
getUnpackers g h m = case mConcreteType m of
  (Just (MFuncType _ ts _)) -> map (unpackerName g h . return) ts 
  (Just _) -> error "Expected a function type"
  Nothing -> take (length (mArgs m)) (repeat (unpackerName g h Nothing))

useUnpacker :: Grammar -> Argument -> Manifold -> Bool
useUnpacker g (ArgName n) m = elem n (mBoundVars m)
useUnpacker g (ArgCall m) _ = (mLang m) /= (Just (gLang g))
useUnpacker _ (ArgData _) _ = False
useUnpacker _ (ArgPosi _) _ = True

commaSep :: [Doc] -> Doc
commaSep = hcat . punctuate ", "

unpackerName :: Grammar -> SerialMap -> Maybe MType -> Doc 
unpackerName g h n = case (n >>= (flip Map.lookup) (serialUnpacker h)) of 
  (Just f) -> text' f
  Nothing  -> text' (serialGenericUnpacker h)

unpack :: Grammar -> SerialMap -> Maybe MType -> Doc -> Doc
unpack g h n d = (gCall g) (unpackerName g h n) [d]

callIdToName :: Manifold -> Doc
callIdToName m = text' $ MS.makeManifoldName (mCallId m)

-- | writes an argument sans serialization 
writeArgument :: Grammar -> [MT.Text] -> Argument -> Doc
writeArgument g _  (ArgName n) = text' n
writeArgument g _  (ArgData d) = writeData g d
writeArgument _ _  (ArgPosi i) = "x" <> int i
writeArgument g xs (ArgCall m) = case mLang m of
  (Just l) ->
    if
      l == gLang g
    then
      (gCall g) (callIdToName m) (map text' xs)
    else
      (gForeignCall g) (ForeignCallDoc {
            fcdForeignProg = text' (MS.findExecutor l)
          , fcdForeignPool = text' (MS.makePoolName l)
          , fcdMid = text' $ MS.makeManifoldName (mCallId m)
          , fcdArgs = map text' xs
          , fcdFile = text' (MS.makePoolName (gLang g))
        })
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
