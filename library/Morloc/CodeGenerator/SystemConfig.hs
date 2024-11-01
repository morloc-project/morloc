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
import Morloc.DataFiles (rSocketLib, pympack, msgpackSource)

import qualified Morloc.Data.Text as MT
import qualified Morloc.Data.Doc as MD
import qualified Data.Text.IO as TIO

import System.Process (callCommand, callProcess)
import System.Directory (doesFileExist, removeFile, createDirectoryIfMissing)
import System.IO (withFile, IOMode(WriteMode))


configure :: [AnnoS (Indexed Type) One (Indexed Lang)] -> MorlocMonad ()
configure rASTs = do 
  let langs = unique $ concatMap findAllLangsSAnno rASTs
  when (RLang `elem` langs) (makeRSocketLib (MD.pretty . snd $ rSocketLib))
  setupMsgPackHandling langs
  return ()

makeRSocketLib :: MDoc -> MorlocMonad ()
makeRSocketLib socketLib = do
  config <- MM.ask
  let srcpath = configHome config </> "lib" </> "socketr.c"
      objpath = configHome config </> "lib" </> "socketr.o"
      libpath = configHome config </> "lib" </> "libsocketr.so"
  liftIO (compileCCodeIfNeeded (MD.render socketLib) srcpath libpath objpath)


setupMsgPackHandling :: [Lang] -> MorlocMonad ()
setupMsgPackHandling langs = do 
  config <- MM.ask

  -- setup python module required for msgpack
  let (pympackFilename, pympackCode) = pympack
  liftIO $ createDirectoryIfMissing True $ configHome config </> "opt"
  let pymackPath = configHome config </> "opt" </> pympackFilename
  liftIO $ TIO.writeFile pymackPath pympackCode

  let includeDir = configHome config </> "include"
  let libDir = configHome config </> "lib"

  liftIO $ createDirectoryIfMissing True includeDir
  liftIO $ createDirectoryIfMissing True libDir

  let mlcmsgpackHeader = includeDir </> fst msgpackSource

  -- write mlcmpack header
  liftIO $ TIO.writeFile mlcmsgpackHeader (snd msgpackSource)

  -- Check if mlcmpack.so exists
  let soPath = libDir </> "libmlcmpack.so"
  soExists <- liftIO $ doesFileExist soPath

  -- if the library doesn't exist, make it
  unless soExists $ do
    -- this is a stupid hack to make gcc compile a header to a shared object
    liftIO $ TIO.writeFile "x.c" ("#include \"" <> MT.pack mlcmsgpackHeader <> "\"")
    let gccCmd = [ "-shared", "-o", soPath, "-fPIC", "x.c" ]
    liftIO $ callProcess "gcc" gccCmd
    liftIO $ removeFile "x.c"

  
compileCCodeIfNeeded :: MT.Text -> Path -> Path -> Path -> IO ()
compileCCodeIfNeeded codeText sourcePath libPath objPath = do
    alreadyExists <- doesFileExist libPath
    if alreadyExists
        then return ()
        else do
            -- Write the code to the temporary file
            withFile sourcePath WriteMode $ \tempHandle -> do
                MT.hPutStr tempHandle codeText

           -- Compile the C code, will generate a .so file with same path and
           -- basename as the source .c file
            let compileCommand = "R CMD SHLIB " ++ sourcePath ++ " -o " ++ libPath 

            callCommand compileCommand

            -- Delete the source .c file
            sourcePathExists <- doesFileExist sourcePath
            when (sourcePathExists) (removeFile sourcePath)

            -- Delete the source .o file
            objPathExists <- doesFileExist objPath
            when (objPathExists) (removeFile objPath)


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
