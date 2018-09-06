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

import Morloc.Types
import Morloc.Quasi
import Morloc.Pools.Common

import qualified Data.Text as DT 
import qualified System.FilePath as SF
import qualified Data.Char as DC
import Text.PrettyPrint.Leijen.Text hiding ((<$>))

generate = makeGenerator g (defaultCodeGenerator g asImport main)

asImport :: DT.Text -> Doc
asImport s = text' $ case DT.uncons s of
  (Just (x, xs)) -> DT.cons
    (DC.toLower x)
    -- FIXME: generalize this to work with any path separator
    ((DT.replace "/" ".") ((DT.pack . SF.dropExtensions . DT.unpack) xs)) 
  _ -> error "Expected import to have at least length 1"

g = Grammar {
      gLang     = "py"
    , gCall     = call'
    , gFunction = function'
    , gComment  = comment'
    , gReturn   = return'
    , gQuote    = dquotes
    , gSource   = gSource'
    , gTrue     = "True"
    , gFalse    = "False"
    , gList     = gList'
    , gTuple    = gTuple'
    , gRecord   = gRecord'
    , gSysCall  = gSysCall'
  } where
    call' :: Doc -> [Doc] -> Doc
    call' n args = n <> tupled args

    function' :: Doc -> [Doc] -> Doc -> Doc
    function' name args body
      = "def " <> name <> tupled args <> ":" <> line <> indent 2 body <> line

    comment' :: Doc -> Doc
    comment' d = "# " <> d

    return' :: Doc -> Doc
    return' x = call' "return" [x]

    gList' :: [Doc] -> Doc
    gList' = list

    gTuple' :: [Doc] -> Doc
    gTuple' = tupled

    gRecord' :: [(Doc,Doc)] -> Doc
    gRecord' xs = encloseSep "{" "}" ", " (map (\(k,v) -> k <> "=" <> v) xs)

    -- FIXME: qualify the calls (I don't have handling for this yet ...)
    gSource' :: Doc -> Doc
    gSource' s = [idoc|from ${s} import *|]

    gSysCall' :: [Doc] -> Doc
    gSysCall' xs = [idoc|subprocess.run(${args}, stdout=subprocess.PIPE).stdout|] where
      args = gList' xs
    
main
  :: [Doc] -> [Manifold] -> SerialMap -> Doc
main srcs manifolds hash = [idoc|#!/usr/bin/env python

import sys
import subprocess


${vsep (map (gSource g) srcs) <> line}

${vsep (map (defaultManifold g hash) manifolds)}

if __name__ == '__main__':
  f = eval(sys.argv[1])
  print(f(*sys.argv[2:]))
|]
