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
import qualified Morloc.Config as MC
import qualified Morloc.Monad as MM
import qualified Morloc.Data.Text as MT

import qualified System.FilePath as SF
import qualified Data.Char as DC

generate :: SparqlDatabaseLike db => db -> MorlocMonad Script
generate db = makeGenerator g (defaultCodeGenerator g asImport main) db

asImport :: MT.Text -> MorlocMonad Doc
asImport s = do
  lib <- MM.asks configLibrary
  return . text'
         . MT.liftToText (map DC.toLower)
         . MT.replace "/" "."
         . MT.stripPrefixIfPresent "/" -- strip the leading slash (if present)
         . MT.stripPrefixIfPresent lib  -- make the path relative to the library
         . MT.liftToText SF.dropExtensions
         $ s

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
    import' _ s = [idoc|from lib.#{s} import *|]
    -- TODO: FIX THE 'lib.' HACK - the hack avoids namespace collisions (e.g.
    -- of Morloc 'bio' against Biopython 'bio'). But it also hard-codes the
    -- 'lib' foler name. 

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

toDict :: (a -> Doc) -> (a -> Doc) -> [a] -> Doc
toDict l r xs = "dict" <> tupled (map (\x -> l x <> "=" <> r x) xs)

main
  :: [Doc] -> [Manifold] -> SerialMap -> MorlocMonad Doc
main srcs manifolds hash = do
  home <- fmap text' $ MM.asks MC.configHome
  usedManifolds <- getUsedManifolds g manifolds
  let dispatchFunDict = toDict id id usedManifolds
  mids <- MM.mapM callIdToName manifolds
  let dispatchSerializerDict = toDict fst (getUnpacker hash . snd) (zip mids manifolds)
  let sources = vsep (map ((gImport g) "lib") srcs)
  sourceManifolds <- makeSourceManifolds g hash manifolds
  cisManifolds <- makeCisManifolds g hash manifolds
  return $ [idoc|#!/usr/bin/env python

import sys
import subprocess
import json

sys.path = ["#{home}"] + sys.path

#{sources}

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


#{sourceManifolds}

#{cisManifolds}

dispatchFun = #{dispatchFunDict}
dispatchSerializer = #{dispatchSerializerDict}

if __name__ == '__main__':
    script_name = sys.argv[0] 

    try:
        cmd = sys.argv[1]
    except IndexError:
        sys.exit("Internal error in {}: no manifold id found".format(script))

    try:
        function = dispatchFun[cmd]
    except KeyError:
        sys.exit("Internal error in {}: expected manifold id (e.g. m34), got {}".format(script, cmd))

    args = sys.argv[2:]
    serialize = dispatchSerializer[cmd]

    print(serialize(function(*args)))

|]
