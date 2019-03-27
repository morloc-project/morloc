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
import Morloc.Data.Doc hiding ((<$>))
import Morloc.Config (Config)
import qualified Morloc.Data.Text as MT

import qualified System.FilePath as SF
import qualified Data.Char as DC

generate :: SparqlDatabaseLike db => Config -> db -> IO Script
generate config = makeGenerator config g (defaultCodeGenerator config g asImport main)

asImport :: MT.Text -> Doc
asImport s = text' $ case MT.uncons s of
  (Just (x, xs)) -> MT.cons
    (DC.toLower x)
    -- FIXME: generalize this to work with any path separator
    ((MT.replace "/" ".") ((MT.pack . SF.dropExtensions . MT.unpack) xs)) 
  _ -> error "Expected import to have at least length 1"

g = Grammar {
      gLang        = "py"
    , gAssign      = assign' 
    , gCall        = call'
    , gFunction    = function'
    , gComment     = comment'
    , gReturn      = return'
    , gQuote       = dquotes
    , gImport      = import'
    , gTrue        = "True"
    , gFalse       = "False"
    , gList        = list'
    , gTuple       = tuple'
    , gRecord      = record'
    , gIndent      = indent'
    , gTry         = pytry
    , gUnpacker    = unpacker'
    , gForeignCall = foreignCall'

  } where

    indent' = indent 4

    assign' l r = l <> " = " <> r 

    call' :: Doc -> [Doc] -> Doc
    call' n args = n <> tupled args

    function' :: Doc -> [Doc] -> Doc -> Doc
    function' name args body
      = "def " <> name <> tupled args <> ":" <> line <> indent 4 body <> line

    comment' :: Doc -> Doc
    comment' d = "# " <> d

    return' :: Doc -> Doc
    return' x = call' "return" [x]

    list' :: [Doc] -> Doc
    list' = list

    tuple' :: [Doc] -> Doc
    tuple' = tupled

    record' :: [(Doc,Doc)] -> Doc
    record' xs = encloseSep "{" "}" ", " (map (\(k,v) -> k <> "=" <> v) xs)

    -- 1st argument (home directory) is ignored (it is added to path in main)
    import' :: Doc -> Doc -> Doc
    import' _ s = [idoc|from #{s} import *|]

    unpacker' :: UnpackerDoc -> Doc
    unpacker' u = call' "_morloc_unpack"
      [ udUnpacker u
      , udValue u
      , "mid=" <> dquotes (udMid u)
      , "filename=" <> dquotes (udFile u)
      ]

    foreignCall' :: ForeignCallDoc -> Doc
    foreignCall' f = call' "_morloc_foreign_call"
      [ dquotes (fcdForeignProg f)
      , dquotes (fcdForeignPool f)
      , dquotes (fcdMid f)
      , list' (fcdArgs f)
      ]


pytry :: TryDoc -> Doc
pytry t = [idoc|
try:
    #{tryRet t} = #{tryCmd t}#{tupled (tryArgs t)}
except Exception as e:
    sys.exit("Error in %s:%s\n%s" % (__FILE__, __name__, str(e)))
|]

main
  :: Doc -> [Doc] -> [Manifold] -> SerialMap -> Doc
main lib srcs manifolds hash = [idoc|#!/usr/bin/env python

import sys
import subprocess
import json

sys.path.append('#{lib}')
#{vsep (map ((gImport g) lib) srcs)}


def _morloc_unpack(unpacker, jsonString, mid, filename):
    try:
        pyObj = unpacker(jsonString)
    except Exception:
        msg = "Error in %s::%s - JSON parse failure" % (filename, mid)
        if(len(jsonString) == 0):
            msg += ": empty document"
        else:
            msg += ", bad document:\n%s" % str(jsonString)
        sys.exit(msg)
    return(pyObj)

def _morloc_foreign_call(interpreter, pool, mid, args):
    try:
        sysObj = subprocess.run(
            [interpreter, pool, mid, *args],
            capture_output=True,
            check=True
        )
    except subprocess.CalledProcessError as e:
        sys.exit(str(e))

    jsonString = sysObj.stdout
    jsonLog = sysObj.stderr

    if(len(jsonLog) > 0):
      print(jsonLog, file=sys.stderr)

    return(jsonString)


#{makeSourceManifolds g hash manifolds}

#{makeCisManifolds g hash manifolds}

dispatch = dict#{tupled (map (\x -> x <> "=" <> x) (getUsedManifolds g manifolds))}

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

    print(packGeneric(function(*args)))

|]
