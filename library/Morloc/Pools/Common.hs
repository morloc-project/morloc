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
  , makeGenerator
  , defaultCodeGenerator
  , defaultManifold
) where

import Morloc.Vortex
import Morloc.Types
import Morloc.Quasi
import qualified Morloc.System as MS
import qualified Data.Text as DT
import Text.PrettyPrint.Leijen.Text hiding ((<$>))
import qualified Data.HashMap.Strict as Map

data Grammar = Grammar {
      gLang     :: DT.Text
    , gCall     :: Doc -> [Doc] -> Doc
    , gFunction :: Doc -> [Doc] -> Doc -> Doc
    , gComment  :: Doc -> Doc
    , gReturn   :: Doc -> Doc
    , gQuote    :: Doc -> Doc
    , gSource   :: Doc -> Doc
    , gList     :: [Doc] -> Doc
    , gTuple    :: [Doc] -> Doc
    , gRecord   :: [(Doc,Doc)] -> Doc
    , gTrue     :: Doc
    , gFalse    :: Doc
    , gSysCall  :: [Doc] -> Doc
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
  -> ([Doc] -> [Manifold] -> PackHash -> Doc) -- main
  -> CodeGenerator 
defaultCodeGenerator g f main ep = do
  manifolds <- buildManifolds ep
  packHash <- buildPackHash (gLang g) ep
  let srcs = map f (sources packHash)
  (return . render) $ main srcs manifolds packHash

commaSep :: [Doc] -> Doc
commaSep = hcat . punctuate ", "

nameArgs :: [a] -> [Doc]
nameArgs xs = map ((<>) "x") (map int [0 .. (length xs - 1)])

iArgs :: Int -> [Doc]
iArgs i = map ((<>) "x") (map int [1 .. i])

unpack :: Grammar -> PackHash -> Maybe Name -> Doc -> Doc
unpack g h n d = case (n >>= (flip Map.lookup) (unpacker h)) of 
  (Just f) -> (gCall g) (text' f) [d]
  Nothing  -> (gCall g) (text' (genericUnpacker h)) [d]

callIdToName :: Manifold -> Doc
callIdToName m = text' $ MS.makeManifoldName (mCallId m)

srcLangDoc :: Manifold -> Doc
srcLangDoc m = case mLang m of
  Just l -> text' l
  Nothing -> text' "Undefined"

fname :: Manifold -> Doc
fname m = text' (mCallName m)

castArgsPosi :: Grammar -> PackHash -> Manifold -> [Doc]
castArgsPosi g h m = map cast (mArgs m)
  where
    cast (ArgPosi i t) = unpack g h t ("x" <> int i)
    cast _ = error "Expected only user arguments"

castArgsName :: Grammar -> PackHash -> Manifold -> [Doc]
castArgsName g h m = map (\a -> (pack a) (cast a)) (mArgs m)
  where
    cast arg = writeArgument g (mBoundVars m) arg
    pack arg d = case arg of
      (ArgName _ t) -> if (mCalled m) && not (mSourced m)
                       then d
                       else unpack g h t d
      (ArgCall n t (Just l)) -> if (Just l) == mLang m
                                then d
                                else unpack g h t d
      _ -> d

-- | writes an argument sans serialization 
writeArgument :: Grammar -> [DT.Text] -> Argument -> Doc
writeArgument _ _ (ArgName n _  )  = text' n
writeArgument g xs (ArgCall n t (Just l))
  | l == gLang g = (gCall g) (text' $ MS.makeManifoldName n) (map text' xs)
  | otherwise = (gSysCall g) ([
        (gQuote g) $ text' (MS.findExecutor l)
      , (gQuote g) $ text' (MS.makePoolName l)
      , (gQuote g) $ text' (MS.makeManifoldName n)
      ] ++ (map text' xs))
  -- | ArgCall Key (Maybe ReturnType) (Maybe Lang)
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

defaultManifold :: Grammar -> PackHash -> Manifold -> Doc
defaultManifold g h m
  | mLang m == Just (gLang g)
      && not (mCalled m)
      && mSourced m
      && mExported m = defaultSourceWrapperManifold g h m
  | not (mCalled m) && mExported m = "" -- this is not a thing
  | mLang m == Just (gLang g) = defaultCisManifold g h m
  | otherwise = ""

defaultSourceWrapperManifold :: Grammar -> PackHash -> Manifold -> Doc
defaultSourceWrapperManifold g h m
  =  (gComment g) (srcLangDoc m <> ": " <> text' (mMorlocName m) <> " source wrapper") <> line
  <> (gFunction g)
        (callIdToName m)
        (nameArgs (mArgs m))
        ((gReturn g) ((gCall g) (fname m) (castArgsPosi g h m)))

defaultCisManifold :: Grammar -> PackHash -> Manifold -> Doc
defaultCisManifold g h m
  =  (gComment g) (srcLangDoc m <> ": " <> text' (mMorlocName m) <> " standard manifold") <> line
  <> (gFunction g)
        (callIdToName m)
        (map text' (mBoundVars m))
        ((gReturn g) ((gCall g) (fname m) (castArgsName g h m)))
