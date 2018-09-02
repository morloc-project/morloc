{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.R
Description : R language generation
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Language.R (generate) where

import Morloc.Quasi
import Morloc.Types
import Morloc.Vortex
import qualified Morloc.System as MS
import qualified Morloc.Util as MU
import qualified Morloc.Query as Q

import qualified Data.Text as DT 
import qualified Data.List as DL
import Text.PrettyPrint.Leijen.Text hiding ((<$>))

generate :: SparqlEndPoint -> IO Script
generate e
  =   Script
  <$> pure "pool"
  <*> pure "R"
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
nameArgs xs = map ((<>) "x") (map int [1 .. length xs])

iArgs :: Int -> [Doc]
iArgs i = map ((<>) "x") (map int [1 .. i])

-- | writes an argument sans serialization 
writeArgument :: Argument -> Doc
writeArgument (ArgName n _  ) = text' n
writeArgument (ArgCall n _ _) = text' n
writeArgument (ArgData d _  ) = writeData d
writeArgument (ArgPosi i _  ) = "x" <> int i

writeData :: MData -> Doc
writeData (Num' x) = text' x
writeData (Str' x) = dquotes (text' x) -- FIXME: need to escape
writeData (Log' True) = "TRUE"
writeData (Log' False) = "FALSE"
writeData (Lst' xs) = "c" <> (parens . commaSep . map writeData) xs
writeData (Tup' xs) = "list" <> (parens . commaSep . map writeData) xs
writeData (Rec' xs) = "list" <> (parens . commaSep . map writeEntry) xs
  where
    writeEntry (key, val) = text' key <> "=" <> writeData val 

main
  :: [Doc] -> [Manifold] -> PackHash -> Doc
main srcs manifolds hash = [idoc|#!/usr/bin/env Rscript

${vsep (map sourceT srcs)}

${vsep (map (manifoldT hash) manifolds)}

args <- commandArgs(trailingOnly=TRUE)
if(length(args) == 0){
  stop("Expected 1 or more arguments")
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
  | mExported m && (not (mCalled m)) = exportedT m h
  | otherwise = "XXX"

exportedT m h = [idoc|
# ${text' $ mMorlocName m}

${text' $ MS.makeManifoldName (mCallId m)} <- function(${commaSep (map text' (mBoundVars m))}){
  ${fname}(${commaSep (map castArg (mArgs m))})
}
|]
  where
    castArg :: Argument -> Doc
    castArg arg = case arg of
      (ArgName n (Just t)) -> "unpacker" <> parens (writeArgument arg)
      (ArgName n Nothing)  -> "genericUnpacker" <> parens (writeArgument arg)
      _ -> writeArgument arg

    fname = text' $ maybe (mMorlocName m) id (mSourceName m)

    generic = genericUnpacker h
