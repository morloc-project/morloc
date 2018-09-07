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
    , gImport   = gImport'
    , gTrue     = "True"
    , gFalse    = "False"
    , gList     = gList'
    , gTuple    = gTuple'
    , gRecord   = gRecord'
    , gTrans    = transManifoldT
    , gCis      = cisManifoldT
    , gSource   = sourceManifoldT
  } where
    call' :: Doc -> [Doc] -> Doc
    call' n args = n <> tupled args

    function' :: Doc -> [Doc] -> Doc -> Doc
    function' name args body
      = "def " <> name <> tupled args <> ":" <> line <> indent 4 body <> line

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
    gImport' :: Doc -> Doc
    gImport' s = [idoc|from ${s} import *|]

sourceManifoldT :: SourceManifoldDoc -> Doc
sourceManifoldT s = [idoc|
# src manifold
def ${srcCallId s}(${commaSep (srcBndArgs s)}):
  return(${srcName s}(${commaSep (srcFunArgs s)}))
|]

transManifoldT :: TransManifoldDoc -> Doc
transManifoldT t = [idoc| 
# trans manifold 
def ${transCallId t}(${commaSep (transArgs t)}):
    try:
        sysObj = subprocess.run(
            ${args},
            capture_output=True,
            check=True
        )
    except subprocess.CalledProcessError as e:
        sys.exit(str(e))

    jsonString = sysObj.stdout
    jsonLog = sysObj.stderr

    if(len(jsonLog) > 0):
      print(jsonLog, file=sys.stderr)

    try:
        pyObj = ${transUnpacker t}(jsonString)
    except json.decoder.JSONDecodeError as e:
        msg = "Error in ${transCallingPool t}::${transCallId t} - JSON parse failure"
        if(len(e.doc) == 0):
            msg += ": empty document"
        else:
            msg += ", bad document:\n%s" % str(e.doc)
        sys.exit(msg)

    return(pyObj)
|]
  where
    args = list ([
          dquotes (transCaller t)
        , dquotes (transPool t)
        , dquotes (transCallId t)
      ] ++ transArgs t)

cisManifoldT :: CisManifoldDoc -> Doc
cisManifoldT c = [idoc|
# cis manifold
def ${cisCallId c}(${commaSep (cisBndArgs c)}):
  return(${cisName c}(${commaSep (cisFunArgs c)}))
|]
    
main
  :: [Doc] -> [Manifold] -> SerialMap -> Doc
main srcs manifolds hash = [idoc|#!/usr/bin/env python

import sys
import subprocess
import json

${vsep (map (gImport g) srcs) <> line}

${makeSourceManifolds g hash manifolds}

${makeTransManifolds g hash manifolds}

${makeCisManifolds g hash manifolds}

dispatch = dict${tupled (map (\x -> x <> "=" <> x) (getUsedManifolds g manifolds))}

if __name__ == '__main__':
    script_name = sys.argv[0] 

    try:
        cmd = sys.argv[1]
    except IndexError:
        sys.exit("Internal error in {}: no manifold id found".format(script))

    try:
        function = dispatch[cmd]
    except KeyError:
        sys.exit("Internal error in {}: expected manifold id (e.g. m34), got {}".format(script, cmd))

    args = sys.argv[2:]

    print(function(*args))

|]
