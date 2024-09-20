{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.CodeGenerator.SystemConfig
Description : Configure the system as needed for the given composition
Copyright   : (c) Zebulun Arendsee, 2016-2024
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.CodeGenerator.SystemConfig
(
  configure
) where

import Morloc.CodeGenerator.Namespace
import qualified Morloc.Monad as MM
import Control.Monad (when)
import Morloc.DataFiles (rSocketLib)

import qualified Morloc.Data.Text as MT
import qualified Morloc.Data.Doc as MD

import System.Process (callCommand)
import System.Directory (doesFileExist, removeFile, renameFile)
import System.FilePath (takeBaseName, replaceExtension)
import System.IO (hClose, withFile, IOMode(WriteMode))


configure :: [AnnoS (Indexed Type) One (Indexed Lang)] -> MorlocMonad ()
configure rASTs = do 
  let langs = unique $ concatMap findAllLangsSAnno rASTs
  when (RLang `elem` langs) (makeRSocketLib rSocketLib)
  return ()

makeRSocketLib :: MDoc -> MorlocMonad ()
makeRSocketLib socketLib = do
  config <- MM.ask
  let srcpath = configHome config <> "/lib/socketr.c"
      objpath = configHome config <> "/lib/socketr.o"
      libpath = configHome config <> "/lib/socketr.so"
  liftIO (compileCCodeIfNeeded (MD.render socketLib) srcpath libpath objpath)
  
compileCCodeIfNeeded :: MT.Text -> Path -> Path -> Path -> IO ()
compileCCodeIfNeeded codeText srcPath libPath objPath = do
    alreadyExists <- doesFileExist libPath
    if alreadyExists
        then error (show libPath) -- return ()
        else do
            -- Write the code to the temporary file
            withFile srcPath WriteMode $ \tempHandle -> do
                MT.hPutStr tempHandle codeText

           -- Compile the C code, will generate a .so file with same path and
           -- basename as the source .c file
            let compileCommand = "R CMD SHLIB " ++ srcPath
            callCommand compileCommand

            -- Delete the source .c file
            removeFile srcPath
            removeFile objPath


findAllLangsSAnno :: AnnoS e One (Indexed Lang) -> [Lang]
findAllLangsSAnno (AnnoS _ (Idx _ lang) e) = lang : findAllLangsExpr e where
  findAllLangsExpr (VarS _ (One x)) = findAllLangsSAnno x
  findAllLangsExpr (AccS _ x) = findAllLangsSAnno x
  findAllLangsExpr (AppS x xs) = concatMap findAllLangsSAnno (x:xs)
  findAllLangsExpr (LamS _ x) = findAllLangsSAnno x
  findAllLangsExpr (LstS xs) = concatMap findAllLangsSAnno xs
  findAllLangsExpr (TupS xs) = concatMap findAllLangsSAnno xs
  findAllLangsExpr (NamS rs) = concatMap (findAllLangsSAnno . snd) rs
  findAllLangsExpr _ = []
