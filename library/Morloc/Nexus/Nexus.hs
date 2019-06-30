{-|
Module      : Morloc.Nexus.Nexus
Description : Code generators for the user interface
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Nexus.Nexus (generate) where

import Morloc.Global
import Morloc.Operators
import qualified Morloc.Language as ML
import qualified Morloc.Monad as MM
import qualified Morloc.Nexus.Template.Perl as Perl

-- | Generate the nexus, which is a program that coordinates the execution of
-- the language-specific function pools.
generate :: Lang -> [Manifold] -> MorlocMonad Script
generate PerlLang manifolds = Perl.generate manifolds
generate l _ = MM.throwError . GeneratorError $
  ("Cannot generate nexus in language: " <> ML.showLangName l)
