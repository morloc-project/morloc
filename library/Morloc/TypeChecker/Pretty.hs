{-|
Module      : Morloc.TypeChecker.Pretty
Description : Pretty is as pretty does
Copyright   : (c) Zebulun Arendsee, 2020
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.TypeChecker.Pretty (cute, ugly) where

import Morloc.Namespace
import qualified Data.Map as Map

cute :: DAG MVar (Map.Map EVar EVar) TypedNode -> IO ()
cute = undefined

ugly :: DAG MVar (Map.Map EVar EVar) TypedNode -> IO ()
ugly = undefined
