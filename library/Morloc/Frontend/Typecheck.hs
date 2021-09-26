{-|
Module      : Morloc.Frontend.Typecheck
Description : Core inference module
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.Frontend.Typecheck (typecheck) where

import Morloc.Frontend.Namespace
import Morloc.Frontend.Internal
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

-- true facts to keep in mind:
--   * indexing is a good idea, I need it at very least to link source code lines to
--     error messages

-- | Each SAnno object in the input list represents one exported function.
-- Modules, scopes, imports and and everything else are abstracted away,
-- wrapped into GMeta or stored in the Stack state.
typecheck
  :: [SAnno Int Many Int]
  -> MorlocMonad [SAnno (Indexed Type) Many Int]
typecheck es = mapM typecheckGeneral es |>> map resolveTypes

resolveTypes :: SAnno (Indexed TypeU) Many Int -> SAnno (Indexed Type) Many Int
resolveTypes (SAnno (Many es) (Idx i t)) = SAnno (Many (map (\(e, i) -> (f e, i)) es)) (Idx i (typeOf t)) where
  f :: SExpr (Indexed TypeU) Many Int -> SExpr (Indexed Type) Many Int
  f (AccS x k) = AccS (resolveTypes x) k
  f (AppS x xs) = AppS (resolveTypes x) (map resolveTypes xs) 
  f (LamS vs x) = LamS vs (resolveTypes x)
  f (LstS xs) = LstS (map resolveTypes xs)
  f (TupS xs) = TupS (map resolveTypes xs)
  f (NamS rs) = NamS (zip (map fst rs) (map (resolveTypes . snd) rs))
  f (NumS x) = NumS x
  f (LogS x) = LogS x
  f (StrS x) = StrS x
  f (CallS x) = CallS x
  f UniS = UniS


typecheckGeneral
  :: SAnno Int Many Int
  -> MorlocMonad (SAnno (Indexed TypeU) Many Int)
typecheckGeneral x = do
  s <- CMS.gets stateSignatures
  case typecheckGeneralPure (lookupType s) initialContext x of
    (Left err) -> undefined
    (Right x') -> return x'
  where
    initialContext = Gamma
      { gammaCounter = 0
      , gammaContext = []
      }
    
    lookupType :: GMap Int Int TermTypes -> Int -> Maybe TypeU
    lookupType m i = case GMap.lookup i m of
      GMapNoFst -> Nothing
      GMapNoSnd -> Nothing
      GMapJust (TermTypes t _ _) -> fmap etype t

-- | Check the general types, do nothing to the concrete types which may only be
-- solved after segregation. Later the concrete types will need to be checked
-- for type consistency, correctness of packers, inferences of packers (both
-- for serialization and for casting).
typecheckGeneralPure
  :: (Int -> Maybe TypeU)
  -> Gamma
  -> SAnno Int Many Int
  -> Either (Indexed TypeError) (SAnno (Indexed TypeU) Many Int)
typecheckGeneralPure f g e = fmap (\(_,_,e) -> e) (synthG f g e)

synthG
  :: (Int -> Maybe TypeU)
  -> Gamma
  -> SAnno Int Many Int
  -> Either
       (Indexed TypeError)
       ( Gamma
       , TypeU
       , SAnno (Indexed TypeU) Many Int
       )
synthG _ _ (SAnno (Many []) _) = impossible
synthG l g0 (SAnno (Many ((e, j):es)) i) = do
  (g1, t1, e') <- synthE l i g0 e
  (g2, t2, SAnno (Many es') _) <- checkG l g1 (SAnno (Many es) i) t1
  return (g2, t2, SAnno (Many ((e', j):es')) (Idx i t2))

checkG
  :: (Int -> Maybe TypeU)
  -> Gamma
  -> SAnno Int Many Int
  -> TypeU
  -> Either
       (Indexed TypeError)
       ( Gamma
       , TypeU
       , SAnno (Indexed TypeU) Many Int
       )
checkG l g (SAnno (Many []) i) t = return (g, t, SAnno (Many []) (Idx i t)) 
checkG l g0 (SAnno (Many ((e, j):es)) i) t0 = do 
  (g1, t1, e') <- checkE l g0 e t0
  (g2, t2, SAnno (Many es') idType) <- checkG l g1 (SAnno (Many es) i) t1
  return (g2, t2, SAnno (Many ((e', j):es')) idType)


synthE
  :: (Int -> Maybe TypeU)
  -> Int
  -> Gamma
  -> SExpr Int Many Int
  -> Either
       (Indexed TypeError)
       ( Gamma
       , TypeU
       , SExpr (Indexed TypeU) Many Int
       )
synthE l i g UniS = undefined
synthE l i g (VarS v) = undefined
synthE l i g (AccS e k) = undefined
synthE l i g (AppS e es) = undefined
synthE l i g (LamS vs e) = undefined
synthE l i g (LstS es) = undefined
synthE l i g (TupS es) = undefined
synthE l i g (NamS rs) = undefined
synthE l i g (NumS x) = undefined
synthE l i g (LogS x) = undefined
synthE l i g (StrS x) = undefined
synthE l i g (CallS src) = undefined


checkE
  :: (Int -> Maybe TypeU)
  -> Gamma
  -> SExpr Int Many Int
  -> TypeU
  -> Either
       (Indexed TypeError)
       ( Gamma
       , TypeU
       , SExpr (Indexed TypeU) Many Int
       )
checkE = undefined

application
  :: (Int -> Maybe TypeU)
  -> Gamma
  -> SAnno Int Many Int
  -> TypeU
  -> Either
       (Indexed TypeError)
       ( Gamma
       , TypeU
       , SAnno (Indexed TypeU) Many Int
       )
application = undefined
