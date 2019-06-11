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

import Morloc.Global
import Morloc.Quasi
import Morloc.Pools.Common
import Morloc.Data.Doc hiding ((<$>))
import qualified Morloc.Config as MC
import qualified Morloc.Monad as MM
import qualified Morloc.Data.Text as MT

import qualified System.FilePath as SF
import qualified Data.Char as DC

generate :: SparqlDatabaseLike db => db -> MorlocMonad Script
generate db = makeGenerator g (defaultCodeGenerator g asImport) db

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

pytry :: TryDoc -> Doc
pytry t = [idoc|
try:
    #{tryRet t} = #{tryCmd t}#{tupled (tryArgs t)}
except Exception as e:
    sys.exit("Error in %s:%s\n%s" % (__FILE__, __name__, str(e)))
|]

g = Grammar {
      gLang        = Python3Lang
    , gAssign      = assign' 
    , gCall        = call'
    , gFunction    = function'
    , gId2Function = id2function'
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
    , gSignature   = signature'
    , gHash        = hash'
    , gMain        = main'
  } where

    indent' = indent 4

    hash' :: (a -> Doc) -> (a -> Doc) -> [a] -> Doc
    hash' l r xs = encloseSep "{" "}" "," (map (\x -> l x <> ":" <> r x) xs)

    assign' :: GeneralAssignment -> Doc
    assign' g = case gaType g of
      (Just t) -> gaName g <> " = " <> gaValue g <+> comment' ("::" <+> t) 
      Nothing  -> gaName g <> " = " <> gaValue g 

    call' :: Doc -> [Doc] -> Doc
    call' n args = n <> tupled args

    signature' :: GeneralFunction -> Doc
    signature' gf
      =   gfReturnType gf
      <+> gfName gf
      <>  tupledNoFold (map (\(t,x) -> t <+> x) (gfArgs gf))

    function' :: GeneralFunction -> Doc
    function' gf = comment' (signature' gf) <> line
      <> "def "
      <> gfName gf <> tupled (map snd (gfArgs gf)) <> ":"
      <> line <> indent' (gfBody gf) <> line

    id2function' :: Integer -> Doc
    id2function' i = "m" <> (text' (MT.show' i))

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
    foreignCall' f
      = call' "_morloc_foreign_call"
      $ fcdCall f ++ [list (fcdArgs f)]

    main' :: [Doc] -> Doc -> Doc -> Doc -> Doc -> MorlocMonad Doc
    main' sources sourceManifolds cisManifolds dispatchFunDict dispatchSerializerDict = do
      lib <- fmap text' $ MM.asks MC.configLibrary
      return $ [idoc|#!/usr/bin/env python

import sys
import subprocess
import json

sys.path = ["#{lib}"] + sys.path

#{vsep sources}

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
        cmdID = int(sys.argv[1])
    except IndexError:
        sys.exit("Internal error in {}: no manifold id found".format(script))
    except ValueError:
        sys.exit("Internal error in {}: expected integer manifold id".format(script))

    try:
        function = dispatchFun[cmdID]
    except IndexError:
        sys.exit("Internal error in {}: expected manifold id, got {}".format(script, cmdID))

    args = sys.argv[2:]
    serialize = dispatchSerializer[cmdID]

    print(serialize(function(*args)))

|]
