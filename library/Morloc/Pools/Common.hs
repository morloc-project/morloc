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

import qualified Morloc.Component.Serializer as Serializer
import qualified Morloc.Component.Manifold as Manifold

import Morloc.Types
import Morloc.Quasi
import qualified Morloc.System as MS
import qualified Data.Text as DT
import Text.PrettyPrint.Leijen.Text hiding ((<$>))
import qualified Data.Map.Strict as Map

data Grammar = Grammar {
      gLang     :: DT.Text
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
      <*> pure (DT.unpack (gLang g))
      <*> gen ep

defaultCodeGenerator
  :: Grammar
  -> (DT.Text -> Doc) -- source name parser
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
  <> ((gComment g) (fname m <> " :: " <> maybe "undefined" mshow (mType m))) <> line
  <> ((gFunction g)
       (callIdToName m)
       (take n (iArgs "x"))
       (
            vsep (zipWith3 unpack' (iArgs "a") (mArgs m) (iArgs "x")) <> line
         <> (gReturn g) ((gCall g) (fname m) (take n (iArgs "a")))
       ))
  where
    n = length (mArgs m)

    unpack' :: Doc -> Argument -> Doc -> Doc
    unpack' lhs (ArgPosi _ t) x
      = (gComment g) (maybe "undefined type" mshow t) <> line <>
        (gAssign g) lhs ((gUnpacker g) (UnpackerDoc {
              udValue = x   
            , udUnpacker = unpackerName g h t
            , udMid = callIdToName m
            , udFile = text' (MS.makePoolName (gLang g))
          }))

makeCisManifold :: Grammar -> SerialMap -> Manifold -> Doc
makeCisManifold g h m
  =  ((gComment g) "cis manifold") <> line
  <> ((gComment g) (fname m <> " :: " <> maybe "undefined" mshow (mType m))) <> line
  <> ((gFunction g)
       (callIdToName m)
       (map text' (mBoundVars m))
       (
            vsep (zipWith3 makeArg (iArgs "a") (mArgs m) (iArgs "x")) <> line
         <> (gReturn g) ((gCall g) (fname m) (take n (iArgs "a")))
       ))
  where
    n = length (mArgs m)

    makeArg :: Doc -> Argument -> Doc -> Doc
    makeArg lhs a x = (gComment g) (maybe "undefined type" mshow (argType a)) <> line <>
                      makeArg' lhs a x

    makeArg' :: Doc -> Argument -> Doc -> Doc
    makeArg' lhs arg x = case ( chooseUnpacker g h arg m
                              , writeArgument g (mBoundVars m) arg ) of
      (Just p, x) -> (gAssign g) lhs (unpack' p x)
      (Nothing, x) -> (gAssign g) lhs x

    unpack' :: Doc -> Doc -> Doc
    unpack' p x
      = ((gUnpacker g) (UnpackerDoc {
              udValue = x   
            , udUnpacker = p
            , udMid = callIdToName m
            , udFile = text' (MS.makePoolName (gLang g))
          }))

chooseUnpacker :: Grammar -> SerialMap -> Argument -> Manifold -> Maybe Doc 
chooseUnpacker g h (ArgName n t) m =
  if
    elem n (mBoundVars m)
  then
    Just (unpackerName g h t)
  else
    Nothing -- currently manifold parameters are always JSON data from the user
chooseUnpacker g h (ArgCall _ t (Just l)) m =
  -- output is coming from another manifold inside this language
  if
    l == (gLang g)
  then
    Nothing 
  else
    Just (unpackerName g h t)
chooseUnpacker g _ (ArgData d _) _ = Nothing
chooseUnpacker g h (ArgPosi _ t) _ = Just (unpackerName g h t)

argType :: Argument -> Maybe MType
argType (ArgName _ t)   = t
argType (ArgCall _ t _) = t
argType (ArgData _ t)   = t
argType (ArgPosi _ t)   = t

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

srcLangDoc :: Manifold -> Doc
srcLangDoc m = case mLang m of
  Just l -> text' l
  Nothing -> text' "Undefined"

-- | writes an argument sans serialization 
writeArgument :: Grammar -> [DT.Text] -> Argument -> Doc
writeArgument g _ (ArgName n _  )  = text' n
writeArgument g _ (ArgData d _  )  = writeData g d
writeArgument _ _ (ArgPosi i _  )  = "x" <> int i
writeArgument g xs (ArgCall n t (Just l))
  | l == (gLang g) = (gCall g) (text' $ MS.makeManifoldName n) (map text' xs)
  | otherwise = (gForeignCall g) (ForeignCallDoc {
        fcdForeignProg = text' (MS.findExecutor l)
      , fcdForeignPool = text' (MS.makePoolName l)
      , fcdMid = text' $ MS.makeManifoldName n
      , fcdArgs = map text' xs
      , fcdFile = text' (MS.makePoolName (gLang g))
    })

writeData :: Grammar -> MData -> Doc
writeData _ (Num' x)     = text' x
writeData g (Str' x)     = (gQuote g) (text' x)
writeData g (Log' True)  = gTrue g
writeData g (Log' False) = gFalse g
writeData g (Lst' xs)    = (gList g) (map (writeData g) xs)
writeData g (Tup' xs)    = (gTuple g) (map (writeData g) xs)
writeData g (Rec' xs)    = (gRecord g) (map (\(k, v) -> (text' k, writeData g v)) xs)

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

getUsedManifolds :: Grammar -> [Manifold] -> [Doc]
getUsedManifolds g ms = map callIdToName (filter isBuilt ms)
  where
    isBuilt :: Manifold -> Bool
    isBuilt m = case determineManifoldClass g m of
      Cis -> True
      Source -> True
      _ -> False

-- nameArgs :: [a] -> [Doc]
-- nameArgs xs = map ((<>) "x") (map int [0 .. (length xs - 1)])

fname :: Manifold -> Doc
fname m = text' (mCallName m)
