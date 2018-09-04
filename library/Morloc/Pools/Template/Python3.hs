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
  packHash <- buildPackHash e
  let srcs = map text' (sources packHash)
  (return . render) $ main srcs manifolds packHash

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
main srcs manifolds hash = [idoc|# Python pool

import sys

${vsep (map sourceT srcs)}

${vsep (map (manifoldT hash) manifolds)}

if __name__ == '__main__':
  args

if(len(sys.argv) == 0){
  sys.exit("Expected 1 or more arguments")
} else if(exists(args[[1]])){
  x <- get(args[[1]])
  result <- if(class(x) == "function"){
    do.call(get(args[[1]]), as.list(args[-1, drop=FALSE]))
  } else {
    x
  }
  cat(packGeneric(result), "\n")
} else {
  stop("Could not find function '", args[[1]], "'")
}
|]

sourceT s = [idoc|source("${s}")|]

manifoldT h m
  | not (mCalled m) && mSourced m && mExported m = sourceWrapperManifold h m
  | not (mCalled m) && mExported m = compositionWrapperManifold h m
  | otherwise = standardManifoldT h m

sourceWrapperManifold h m = [idoc|
# ${text' $ mMorlocName m}

${text' $ MS.makeManifoldName (mCallId m)} <- function(${commaSep $ nameArgs (mArgs m)}){
  ${fname}(${commaSep (map castArg (mArgs m))})
}
|]
  where
    castArg :: Argument -> Doc
    castArg arg = case arg of
      (ArgPosi i t) -> unpack h t ("x" <> int i)
      _ -> error "Expected only user arguments"

    fname = text' $ maybe (mMorlocName m) id (mSourceName m)

compositionWrapperManifold h m = [idoc|# FLUFF|]

standardManifoldT h m = [idoc|
# ${text' $ mMorlocName m}

${text' $ MS.makeManifoldName (mCallId m)} <- function(${commaSep (map text' (mBoundVars m))}){
  ${fname}(${commaSep (map castArg (mArgs m))})
}
|]
  where
    castArg :: Argument -> Doc
    castArg arg = case arg of
      (ArgName _ t) -> unpack h t (writeArgument (mBoundVars m) arg)
      _ -> writeArgument (mBoundVars m) arg

    fname = text' $ maybe (mMorlocName m) id (mSourceName m)
