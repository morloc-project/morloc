{-|
Module      : Morloc.Nexus.Nexus
Description : Code generators for the user interface
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.Nexus.Nexus
  ( generate
  ) where

import Morloc.Namespace
import qualified Morloc.Nexus.Template.Perl as Perl

-- | For now there is just one nexus. Eventually I will add many more, such as
-- a minimal CLI nexus for running a single command; local GUI nexa, apps, etc.
-- Or just different flavors of CLI nexa. Once there are multiple nexa, then I
-- will gather common functionallity into this module and dispatch to the
-- templates. But for now, this module is just a light wrapper around the
-- terminal template.
generate :: [Manifold] -> MorlocMonad Script
generate manifolds = Perl.generate manifolds
