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
  = (gFunction g)
      (callIdToName m)
      (take n (iArgs "x"))
      (
           vsep (zipWith3 unpack' (iArgs "a") (mArgs m) (iArgs "x")) <> line
        <> (gCall g) (fname m) (take n (iArgs "a"))
      )
  where
    n = length (mArgs m)

    unpack' :: Doc -> Argument -> Doc -> Doc
    unpack' lhs (ArgPosi _ t) x
      = (gAssign g) lhs ((gUnpacker g) (UnpackerDoc {
              udValue = x   
            , udUnpacker = unpackerName g h t
            , udMid = callIdToName m
            , udFile = text' (MS.makePoolName (gLang g))
          }))


makeCisManifold :: Grammar -> SerialMap -> Manifold -> Doc
makeCisManifold g s m
  = (gFunction g)
      (callIdToName m)
      (map text' (mBoundVars m))
      ((gCall g) (fname m) (map makeArg (mArgs m)))
  where
    makeArg :: Argument -> Doc
    makeArg a = writeArgument g (mBoundVars m) a

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
writeArgument _ _ (ArgName n _  )  = text' n
writeArgument g xs (ArgCall n t (Just l))
  = (gCall g) (text' $ MS.makeManifoldName n) (map text' xs)
writeArgument g _ (ArgData d _  )  = writeData g d
writeArgument _ _ (ArgPosi i _  )  = "x" <> int i

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
--
-- iArgs :: Int -> [Doc]
-- iArgs i = map ((<>) "x") (map int [1 .. i])

fname :: Manifold -> Doc
fname m = text' (mCallName m)

-- castArgsPosi :: Grammar -> SerialMap -> Manifold -> [Doc]
-- castArgsPosi g h m = map cast (mArgs m)
--   where
--     cast (ArgPosi i t) = unpack g h t ("x" <> int i)
--     cast _ = error "Expected only user arguments"
--
-- castArgsName :: Grammar -> SerialMap -> Manifold -> [Doc]
-- castArgsName g h m = map (\a -> (pack a) (cast a)) (mArgs m)
--   where
--     cast arg = writeArgument g (mBoundVars m) arg
--     pack arg d = case arg of
--       (ArgName _ t) -> if (mCalled m) && not (mSourced m)
--                        then d
--                        else unpack g h t d
--       _ -> d

-- makeCisManifolds :: Grammar -> SerialMap -> [Manifold] -> Doc
-- makeCisManifolds g h ms
--   = vsep . concat $ map (makeCisManifold g h) ms
--
-- makeCisManifold :: Grammar -> SerialMap -> Manifold -> [Doc]
-- makeCisManifold g h m = case determineManifoldClass g m of
--   Cis -> return $ (gCis g) (CisManifoldDoc {
--         cisCallId = callIdToName m
--       , cisName = text' (mCallName m)
--       , cisBndArgs = map text' (mBoundVars m)
--       , cisFunArgs = castArgsName g h m
--       , cisPool = text' (MS.makePoolName (gLang g))
--     })
--   _ -> []

-- makeSourceManifolds :: Grammar -> SerialMap -> [Manifold] -> Doc
-- makeSourceManifolds g h ms
--   = vsep . concat $ map (makeSourceManifold g h) ms
--
-- makeSourceManifold :: Grammar -> SerialMap -> Manifold -> [Doc]
-- makeSourceManifold g h m = case determineManifoldClass g m of
--   Source -> return $ (gSource g) (SourceManifoldDoc {
--         srcCallId = callIdToName m
--       , srcName = text' (mCallName m)
--       , srcBndArgs = iArgs (length (mArgs m))
--       , srcFunArgs = castArgsPosi g h m
--       , srcPool = text' (MS.makePoolName (gLang g))
--     })
--   _ -> []

-- makeTransManifolds :: Grammar -> SerialMap -> [Manifold] -> Doc
-- makeTransManifolds g h ms
--   = vsep . concat . concat
--   $ map (\m -> map (makeTransManifold g h m) (mArgs m)) ms
--
-- makeTransManifold :: Grammar -> SerialMap -> Manifold -> Argument -> [Doc]
-- makeTransManifold g h m (ArgCall k t (Just lang))
--   | (lang /= (gLang g)) && (mLang m == Just (gLang g))
--     = return . gTrans g $ TransManifoldDoc {
--         transCallId = text' (MS.makeManifoldName k)
--       , transCaller = text' (MS.findExecutor lang)
--       , transPool = text' (MS.makePoolName lang)
--       , transCallingPool = maybe "" text' (fmap MS.makePoolName (mLang m))
--       , transArgs = map text' (mBoundVars m)
--       , transUnpacker = unpackerName g h t
--     }
--   | otherwise = []
-- makeTransManifold _ _ _ _ = []
