{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Morloc.CodeGenerator.Docstrings
Description : Generate the final docstring records
Copyright   : (c) Zebulun Arendsee, 2016-2025
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.CodeGenerator.Docstrings (processDocstrings) where

import Morloc.Namespace
import qualified Morloc.Monad as MM
import qualified Morloc.Data.GMap as GMap

processDocstrings :: AnnoS (Indexed Type) One a -> MorlocMonad (AnnoS (Indexed Type) One a, CmdDocSet)
processDocstrings e@(AnnoS (Idx i _) _ _) = do
    sgmap <- MM.gets stateSignatures
    doc <- case GMap.lookup i sgmap of
      (GMapJust (Monomorphic (TermTypes (Just et) _ _))) -> return $ edocs et
      (GMapJust (Polymorphic _ _ et _)) -> return $ edocs et
      _ -> return $ defaultValue
    return (e, doc)

    -- handle resolved generics - the types may be associated with docstrings
    -- that can be looked up and inserted

    -- handle records, they were stored by name (e.g., literal "Person") in the
    -- parser, but now the full descriptions of the records should be available
    -- in scope
