{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.CodeGenerator.Grammars.Template.Python3
Description : R language generation
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.CodeGenerator.Grammars.Template.Python3
(
  grammar
) where

import Morloc.Data.Doc
import Morloc.Namespace
import Morloc.CodeGenerator.Grammars.Common
import Morloc.Quasi
import Morloc.Pretty (prettyType)
import qualified Data.Char as DC
import qualified Morloc.Config as MC
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified System.FilePath as SF
import qualified Morloc.TypeChecker.Macro as MTM

asImport :: MT.Text -> MorlocMonad MDoc
asImport s = do
  lib <- MM.asks configLibrary
  return . pretty
         . MT.liftToText (map DC.toLower)
         . MT.replace "/" "."
         . MT.stripPrefixIfPresent "/" -- strip the leading slash (if present)
         . MT.stripPrefixIfPresent lib  -- make the path relative to the library
         . MT.liftToText SF.dropExtensions
         $ s

pyIfElse :: [(MDoc, MDoc)] -> Maybe MDoc -> MDoc
pyIfElse [] _ = ""
pyIfElse [(cond,block)] Nothing = nest 4 ("if" <+> cond <> ":" <> line <> block)
pyIfElse (r:rs) els
  =  pyIfElse [r] Nothing
  <> line <> vsep (map pyElif rs) <> pyElse els
  where
    pyElif (cond,block) = nest 4 ("elif" <+> cond <> ":" <> line <> block)

    pyElse Nothing = ""
    pyElse (Just elseblock) = nest 4 ("else:" <> line <> elseblock)

grammar = Grammar {
      gLang        = gLang'
    , gSerialType  = gSerialType'
    , gAssign      = gAssign'
    , gCall        = gCall'
    , gFunction    = gFunction'
    , gId2Function = gId2Function'
    , gCurry       = gCurry'
    , gComment     = gComment'
    , gReturn      = gReturn'
    , gQuote       = gQuote'
    , gImport      = gImport'
    , gTrue        = gTrue'
    , gFalse       = gFalse'
    , gList        = gList'
    , gTuple       = gTuple'
    , gRecord      = gRecord'
    , gIndent      = gIndent'
    , gTry         = gTry'
    , gUnpacker    = gUnpacker'
    , gForeignCall = gForeignCall'
    , gSignature   = gSignature'
    -- , gSwitch      = gSwitch'
    , gCmdArgs     = gCmdArgs'
    , gShowType    = gShowType'
    , gMain        = gMain'
  }

gLang' :: Lang
gLang' = Python3Lang

gSerialType' :: Type
gSerialType' = VarT (TV (Just Python3Lang) "str")

gAssign' :: GeneralAssignment -> MDoc
gAssign' ga = case gaType ga of
  (Just t) -> gaName ga <> " = " <> gaValue ga <+> gComment' ("::" <+> t)
  Nothing  -> gaName ga <> " = " <> gaValue ga

gCall' :: MDoc -> [MDoc] -> MDoc
gCall' n args = n <> tupled args

gFunction' :: GeneralFunction -> MDoc
gFunction' gf
  =  gComment' (gfComments gf)
  <> "def "
  <> gfName gf <> tupled (map snd (gfArgs gf)) <> ":"
  <> line <> gIndent' (gfBody gf) <> line

gId2Function' :: Integer -> MDoc
gId2Function' i = "m" <> (pretty (MT.show' i))

gCurry' :: MDoc -> [MDoc] -> Int -> MDoc
gCurry' f args i
  = "lambda" <+> hsep (punctuate comma rems) <> ":" <+> gCall' f (args ++ rems) where
    rems = if i == 1
      then ["x"]
      else map (\i' -> "x" <> pretty i') (take i ([1..] :: [Int]))

gComment' :: MDoc -> MDoc
gComment' d = "# " <> d

gReturn' :: MDoc -> MDoc
gReturn' x = gCall' "return" [x]

gQuote' :: MDoc -> MDoc
gQuote' = dquotes

-- 1st argument (home directory) is ignored (it is added to path in main)
gImport' :: MDoc -> MDoc -> MDoc
gImport' _ s = [idoc|from #{s} import *|]

gTrue' :: MDoc
gTrue' = "True"

gFalse' :: MDoc
gFalse' = "False"

gList' :: [MDoc] -> MDoc
gList' = list

gTuple' :: [MDoc] -> MDoc
gTuple' = tupled

gRecord' :: [(MDoc,MDoc)] -> MDoc
gRecord' xs = encloseSep "{" "}" ", " (map (\(k,v) -> k <> "=" <> v) xs)

gIndent' :: MDoc -> MDoc
gIndent' = indent 4

gTry' :: TryDoc -> MDoc
gTry' t = [idoc|
try:
    #{tryRet t} = #{tryCmd t}#{tupled (tryArgs t)}
except Exception as e:
    sys.exit("Error in %s:%s\n%s" % (__FILE__, __name__, str(e)))
|]

gUnpacker' :: UnpackerDoc -> MDoc
gUnpacker' u = gCall' "_morloc_unpack"
  [ udUnpacker u
  , udValue u
  , "mid=" <> dquotes (udMid u)
  , "filename=" <> dquotes (udFile u)
  ]

gForeignCall' :: ForeignCallDoc -> MDoc
gForeignCall' f = gCall' "_morloc_foreign_call" (fcdCall f ++ [list (fcdArgs f)])

gSignature' :: GeneralFunction -> MDoc
gSignature' gf
  =   maybe "?" id (gfReturnType gf)
  <+> gfName gf
  <>  tupledNoFold (map (\(t,x) -> maybe "?" id t <+> x) (gfArgs gf))

-- gSwitch' :: (a -> MDoc) -> (a -> MDoc) -> [a] -> MDoc -> MDoc -> MDoc
-- gSwitch' l r ms x var = pyIfElse cases Nothing
--   where
--     cases = map (\m -> (x <+> "==" <+> l m, var <+> "=" <+> r m)) ms
-- -- -- This is a cleaner approach, but python is too eager, leading every
-- -- -- case to be evaluated, thus this does not behave like a switch statement.
-- -- -- Doing all if statements (as above) does work, but is slow.
-- -- gSwitch' :: (a -> MDoc) -> (a -> MDoc) -> [a] -> MDoc -> MDoc -> MDoc
-- -- gSwitch' l r xs x var
-- --   =   var <+> "=" <+> "if"
-- --   <+> encloseSep "{" "}" "," (map (\x -> l x <> ":" <> r x) xs) <> brackets x

gCmdArgs' :: [MDoc]
gCmdArgs' = map (\i -> "sys.argv[" <> int i <> "]") [2..]

gShowType' :: Type -> MDoc
gShowType' = prettyType

gMain' :: PoolMain -> MorlocMonad MDoc
gMain' pm = do
  lib <- fmap pretty $ MM.asks MC.configLibrary
  return $ [idoc|#!/usr/bin/env python

import sys
import subprocess
import json

sys.path = ["#{lib}"] + sys.path

#{vsep (pmSources pm)}

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
            check=True,
            encoding="ascii"
        )
    except subprocess.CalledProcessError as e:
        sys.exit(str(e))

    jsonString = sysObj.stdout
    jsonLog = sysObj.stderr

    if(len(jsonLog) > 0):
      print(jsonLog, file=sys.stderr)

    return(jsonString)


#{vsep (pmPoolManifolds pm)}

if __name__ == '__main__':
    try:
        cmdID = int(sys.argv[1])
    except IndexError:
        sys.exit("Internal error in {}: no manifold id found".format(sys.argv[0]))
    except ValueError:
        sys.exit("Internal error in {}: expected integer manifold id".format(sys.argv[0]))

    try:

#{indent 8 ((pmDispatchManifold pm) "cmdID" "result")}
    except KeyError:
        sys.exit("Internal error in {}: no manifold found with id={}".format(sys.argv[0], cmdID))

    print(result)
|]
