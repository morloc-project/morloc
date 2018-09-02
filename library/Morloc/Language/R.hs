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
  (return . render) $ main srcs manifolds

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
  :: [Doc] -> [Manifold] -> Doc
main srcs manifolds = [idoc|#!/usr/bin/env Rscript

${vsep (map sourceT srcs)}

${vsep (map manifoldT manifolds)}

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

manifoldT m = [idoc|
XXX
|]

-- exportedT ((alias, fname, uid, generic), args) = [idoc|
-- # ${alias}
--
-- ${uid} <- function(${commaSep (nameArgs args)}){
--   ${fname}(${commaSep (castArgs (generic, args))})
-- }
-- |]
--   where
--     castArgs :: (Doc, [Maybe (Doc, Doc)]) -> [Doc]
--     castArgs (generic, xss) = zipWith
--       (\w i -> [idoc|${w}(x${i})|])
--       (map (wrapper generic) xss)
--       (map int [1..])
--
--     wrapper :: Doc -> (Maybe (Doc, Doc)) -> Doc
--     wrapper _ (Just (specific, _)) = specific
--     wrapper generic _ = generic
--
--
-- cisT (callid, fid, nargs, Just (falias, fname)) = [idoc|
-- # ${falias}
--
-- ${callid} <- function(${commaSep (iArgs nargs)}){
--   ${fname}(${commaSep (iArgs nargs)})
-- }
-- |]
--
-- cisT (callid, fid, nargs, Nothing) = [idoc|
-- # ${callid}
--
-- ${callid} <- function(${commaSep (iArgs nargs)}){
--   ${fid}(${commaSep (iArgs nargs)})
-- }
-- |]
