{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.Pools.Template.Python3
Description : R language generation
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Pools.Template.Python3 (generate) where

import Morloc.Quasi
import Morloc.Types
import Morloc.Vortex
import qualified Morloc.System as MS
import qualified Morloc.Util as MU
import qualified Morloc.Query as Q

import qualified Data.Text as DT 
import qualified Data.List as DL
import qualified Data.HashMap.Strict as Map
import qualified System.FilePath as SF
import qualified Data.Maybe as DM
import qualified Data.Char as DC
import Text.PrettyPrint.Leijen.Text hiding ((<$>))

generate :: SparqlEndPoint -> IO Script
generate e
  =   Script
  <$> pure "pool"
  <*> pure "py3"
  <*> generateCode e

generateCode :: SparqlEndPoint -> IO DT.Text
generateCode e = do
  manifolds <- buildManifolds e
  packHash <- buildPackHash "py" e
  srcsRaw <- findSources e
  let srcs = [(text' . asImport) s | (s, "py") <- srcsRaw]
  (return . render) $ main srcs manifolds packHash

asImport :: DT.Text -> DT.Text
asImport s = case DT.uncons s of
  (Just (x, xs)) -> DT.cons
    (DC.toLower x)
    -- FIXME: generalize this to work with any path separator
    ((DT.replace "/" ".") ((DT.pack . SF.dropExtensions . DT.unpack) xs)) 
  _ -> error "Expected import to have at least length 1"

commaSep :: [Doc] -> Doc
commaSep = hcat . punctuate ", "

nameArgs :: [a] -> [Doc]
nameArgs xs = map ((<>) "x") (map int [0 .. (length xs - 1)])

iArgs :: Int -> [Doc]
iArgs i = map ((<>) "x") (map int [1 .. i])

writeCall' :: DT.Text -> [DT.Text] -> Doc
writeCall' x xs = text' x <> tupled (map text' xs)

unpack :: PackHash -> Maybe Name -> Doc -> Doc
unpack h n d = case (n >>= (flip Map.lookup) (unpacker h)) of 
  (Just f) -> text' f <> parens d
  Nothing  -> text' (genericUnpacker h) <> parens d

-- | writes an argument sans serialization 
writeArgument :: [DT.Text] -> Argument -> Doc
writeArgument _ (ArgName n _  ) = text' n
writeArgument xs (ArgCall n _ _) = writeCall' (MS.makeManifoldName n) xs
writeArgument _ (ArgData d _  ) = writeData d
writeArgument _ (ArgPosi i _  ) = "x" <> int i

writeData :: MData -> Doc
writeData (Num' x) = text' x
writeData (Str' x) = dquotes (text' x) -- FIXME: need to escape
writeData (Log' True) = "True"
writeData (Log' False) = "False"
writeData (Lst' xs) = (brackets . commaSep . map writeData) xs
writeData (Tup' xs) = (parens . commaSep . map writeData) xs
writeData (Rec' xs) = "dict" <> (parens . commaSep . map writeEntry) xs
  where
    writeEntry (key, val) = text' key <> "=" <> writeData val 

main
  :: [Doc] -> [Manifold] -> PackHash -> Doc
main srcs manifolds hash = [idoc|#!/usr/bin/env python

import sys

${vsep (map sourceT srcs)}

${vsep (map (manifoldT hash) manifolds)}

if __name__ == '__main__':
  f = eval(sys.argv[0])
  print(f[sys.argv[1:]])
|]

sourceT s = [idoc|import "${s}")|]

manifoldT h m
  | not (mCalled m) && mSourced m && mExported m = sourceWrapperManifold h m
  | not (mCalled m) && mExported m = compositionWrapperManifold h m
  | otherwise = standardManifoldT h m

sourceWrapperManifold h m = [idoc|
# ${text' $ mMorlocName m}
def ${text' $ MS.makeManifoldName (mCallId m)}(${commaSep $ nameArgs (mArgs m)}):
  return(${fname}(${commaSep (map castArg (mArgs m))}))
|]
  where
    castArg :: Argument -> Doc
    castArg arg = case arg of
      (ArgPosi i t) -> unpack h t ("x" <> int i)
      _ -> error "Expected only user arguments"

    fname = text' $ maybe (mMorlocName m) id (mSourceName m)

compositionWrapperManifold h m = [idoc| |]

standardManifoldT h m = [idoc|
# ${text' $ mMorlocName m}
def ${text' $ MS.makeManifoldName (mCallId m)}(${commaSep (map text' (mBoundVars m))}):
  return(${fname}(${commaSep (map castArg (mArgs m))}))
|]
  where
    castArg :: Argument -> Doc
    castArg arg = case arg of
      (ArgName _ t) -> unpack h t (writeArgument (mBoundVars m) arg)
      _ -> writeArgument (mBoundVars m) arg

    fname = text' $ maybe (mMorlocName m) id (mSourceName m)
