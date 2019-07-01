{-|
Module      : Morloc.Pools.Pools
Description : Generate language-specific code
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Pools.Pools
(
    generate
) where

import Morloc.Global
import Morloc.Operators
import qualified Morloc.Language as ML
import qualified Morloc.Monad as MM
import qualified Morloc.Pools.Template.R as RLang
import qualified Morloc.Pools.Template.Python3 as Py3
import qualified Morloc.Pools.Template.C as C
import qualified Morloc.Pools.Template.Cpp as Cpp
import qualified Morloc.Component.Serializer as Serializer
import qualified Morloc.Component.MType as MCM 

import qualified Control.Monad as CM
import qualified Data.List as DL
import qualified Data.Map.Strict as Map

generate :: SparqlDatabaseLike db => db -> [Manifold] -> MorlocMonad [Script]
generate db manifolds = do
  let langs = DL.nub . map mLang $ manifolds
  typemap <- MCM.fromSparqlDb db
  CM.mapM (generateLang db manifolds typemap) langs 

-- | If you want to add a new language, this is the function you currently need
-- to modify. Add a case for the new language name, and then the function that
-- will generate the code for a script in that language.
generateLang
  :: SparqlDatabaseLike db
  => db
  -> [Manifold]
  -> (Map.Map Key MType) -- type map
  -> Lang
  -> MorlocMonad Script
generateLang db manifolds typemap lang = do
  packMap <- Serializer.fromSparqlDb lang typemap db
  case lang of
    RLang       -> RLang.generate manifolds packMap
    Python3Lang -> Py3.generate manifolds packMap
    CLang       -> C.generate manifolds packMap
    CppLang     -> Cpp.generate manifolds packMap
    MorlocLang  -> MM.throwError . GeneratorError $ "Too much meta, don't generate morloc code"
    x           -> MM.throwError . GeneratorError $ ML.showLangName x <> " is not yet supported"
