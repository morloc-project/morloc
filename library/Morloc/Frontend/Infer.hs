{-|
Module      : Morloc.Frontend.Infer
Description : Core inference module
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.Frontend.Infer
  (
  -- * The main type checker
    typecheck
  -- * Internal functions used in testing
  , subtype
  ) where

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

data TypeError
  = SubtypeError UnresolvedType UnresolvedType
  | TypeMismatch UnresolvedType UnresolvedType
  | OtherTypeError MT.Text

-- | Each SAnno object in the input list represents one exported function.
-- Modules, scopes, imports and and everything else are abstracted away,
-- wrapped into GMeta or stored in the Stack state.
typecheck
  :: [SAnno Int Many Int]
  -> MorlocMonad [SAnno (Indexed Type) Many Int]
typecheck es = mapM checkSources es >> mapM typecheckGeneral es

-- I don't need explicit convert functions, necessarily. The pack functions can
-- be used to convert between values that are in the same language. Because
-- they hae the same general types and the general types define the packed
-- form. Minimizing convert steps would certainly be a valuable optimization,
-- but I can leave that for later.

-- | Ensure that all concrete source signatures match general types
checkSources :: SAnno Int Many a -> MorlocMonad () 
checkSources (SAnno (Many xs) i) = do
  mayts <- lookupSig i   
  case mayts |>> toTypePairs >>= mapM (uncurry checkConcrete) of 
    (Just ((e1, e2):_)) -> undefined -- create error message for mismatched general/concrete types
    _ -> return ()
  where
  toTypePairs :: TermTypes -> [(EType, EType)]
  toTypePairs (TermTypes Nothing _ _) = []
  toTypePairs (TermTypes (Just gt) cts _) = [(gt, ct) | ct <- concat [ts | (_, _, ts, _) <- cts]]

  -- return Nothing if the types are the same, otherwise return the types
  checkConcrete :: EType -> EType -> Maybe (EType, EType)
  checkConcrete e1 e2
    | checkConcreteType e1 e2 = Nothing
    | otherwise = Just (e1, e2)

-- | This is a key function that is exported primarily so it can be tested.
checkConcreteType :: EType -> EType -> Bool
checkConcreteType = undefined

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

subtype
  :: UnresolvedType
  -> UnresolvedType
  -> Gamma
  -> Either TypeError Gamma
subtype = undefined

instantiate
  :: UnresolvedType
  -> UnresolvedType
  -> Gamma
  -> Either TypeError Gamma
instantiate = undefined

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
