{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Nexus.Nexus
Description : Code generators for the user interface
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Nexus.Nexus (generate) where

import Morloc.Types
import Morloc.Operators
import Morloc.Config (Config)
import qualified Morloc.Monad as MM
import qualified Morloc.Nexus.Template.Perl as Perl

-- | Generate the nexus, which is a program that coordinates the execution of
-- the language-specific function pools.
generate :: SparqlDatabaseLike db => Lang -> db -> MorlocMonad Script
generate "perl" d = Perl.generate d
generate l _ = MM.throwError (GeneratorError $ "Cannot generate nexus in language: " <> l)
