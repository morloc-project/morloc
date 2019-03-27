{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Module
Description : Morloc module imports and paths 
Copyright   : (c) Zebulun Arendsee, 2019
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Module
( 
  findModule
) where

import qualified Morloc.Data.Text as MT
import qualified Morloc.Config as Config
import qualified Morloc.Types as Types
import qualified System.Directory as SD
import qualified Control.Monad as CM
import qualified Data.Maybe as DM
import System.FilePath ((</>))
import Morloc.Operators
import qualified Data.List as DL

findModule :: MT.Text -> IO (Types.ThrowsError MT.Text)
findModule x = do
  lib <- Config.getDefaultMorlocLibrary
  let allPaths = getModulePaths lib x
  existingPaths <- fmap DM.catMaybes $ CM.mapM getFile allPaths
  case existingPaths of
    (x:_) -> return $ Right x
    [] -> return $ Left (Types.CannotLoadModule (
          "module not found among the paths: ["
          <> (MT.concat $ DL.intersperse ", " allPaths)
          <> "]"
      ))

findModule x
  =   Config.getDefaultMorlocLibrary -- IO Text
  >>= (\lib -> CM.mapM getFile (getModulePaths lib x)) -- IO [Maybe Text]
  |>> DM.catMaybes -- IO [Text]
  >>= (\xs ->
        case xs of 
          -- report failure if no module is found
          []    -> return $ Left (Types.CannotLoadModule $ "module '" <> x <> "' not found") 
          -- otherwise, return the first path found
          (p:_) -> return $ Right p
      )

-- | Find an ordered list of possible locations to search for a module
getModulePaths :: MT.Text -> MT.Text -> [MT.Text]
getModulePaths lib base = [
    base <> ".loc"                         -- "./${base}.loc"
  , base <> "/main.loc"                    -- "${base}/main.loc"
  , lib <> "/" <> base <> ".loc"      -- "${LIB}/${base}.loc"
  , lib <> "/" <> base <> "/main.loc" -- "${LIB}/${base}/main.loc"
  ]

getFile :: MT.Text -> IO (Maybe MT.Text)
getFile x = do
  fileExists <- SD.doesFileExist (MT.unpack x) 
  return $ if fileExists
           then Just x
           else Nothing
