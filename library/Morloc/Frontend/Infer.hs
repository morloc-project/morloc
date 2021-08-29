{-|
Module      : Morloc.Frontend.Infer
Description : Core inference module
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.Frontend.Infer (typecheck) where

import Morloc.Frontend.Namespace
import Morloc.Frontend.Internal
import qualified Morloc.Frontend.PartialOrder as P
import qualified Morloc.Frontend.Lang.DefaultTypes as MLD
import qualified Morloc.Data.DAG as MDD
import qualified Morloc.Data.GMap as GMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Morloc.Data.Text as MT
import qualified Control.Monad.Reader as R

import Morloc.Data.Doc hiding (putDoc)
import Morloc.Frontend.Pretty
import Data.Text.Prettyprint.Doc.Render.Terminal (putDoc, AnsiStyle)
import qualified Control.Monad.State as CMS

-- true things to remember:
--   * indexing is a good idea, I need it at very least to link source code lines to
--     error messages

-- | Each SAnno object in the input list represents one exported function.
-- Modules, scopes, imports and and everything else are abstracted away,
-- wrapped into GMeta or stored in the Stack state.
typecheck
  :: [SAnno Int Many Int]
  -> MorlocMonad [SAnno (Indexed Type) Many Int]
typecheck es = mapM typecheckGeneral es

typecheckGeneral
  :: SAnno Int Many Int
  -> MorlocMonad (SAnno (Indexed Type) Many Int)
typecheckGeneral x = do
  s <- CMS.gets stateSignatures
  case typecheckGeneralPure (lookupType s) x of
    (Left err) -> undefined
    (Right x') -> return x'
  where
    lookupType :: GMap Int Int TermTypes -> Int -> Maybe UnresolvedType
    lookupType m i = case GMap.lookup i m of
      GMapNoFst -> Nothing
      GMapNoSnd -> Nothing
      GMapJust (TermTypes t _ _) -> fmap etype t

-- | Check the general types, do nothing to the concrete types which may only be
-- solved after segregation. Later the concrete types will need to be checked
-- for type consistency, correctness of packers, inferences of packers (both
-- for serialization and for casting).
typecheckGeneralPure
  :: (Int -> Maybe UnresolvedType)
  -> SAnno Int Many Int
  -> Either (Indexed TypeError) (SAnno (Indexed Type) Many Int)
typecheckGeneralPure = undefined

infer
  :: Gamma
  -> SAnno Int Many Int
  -> Either
       TypeError
       ( Gamma
       , [UnresolvedType]
       , SAnno (Indexed Type) Many Int
       )
infer = undefined

check
  :: Gamma
  -> SAnno Int Many Int
  -> UnresolvedType
  -> Either
       TypeError
       ( Gamma
       , UnresolvedType
       , SAnno (Indexed Type) Many Int
       )
check = undefined

derive ::
     Gamma
  -> SAnno Int Many Int
  -> UnresolvedType
  -> Either
       TypeError
       ( Gamma
       , UnresolvedType
       , SAnno (Indexed Type) Many Int
       )
derive = undefined
