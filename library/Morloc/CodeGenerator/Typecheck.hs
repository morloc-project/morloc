{-|
Module      : Morloc.CodeGenerator.Typecheck
Description : Check the concrete type of an unambiguous AST (SAnno One)
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.CodeGenerator.Typecheck
(
  typecheck
) where

import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.Internal
import Morloc.CodeGenerator.Grammars.Common
import Morloc.Typecheck.Internal
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Data.Map as Map
import qualified Data.Set as Set

typecheck
  :: SAnno (Indexed Type) One (Indexed Lang)
  -> MorlocMonad (SAnno Int One (Indexed TypeP))
typecheck e = do
  packers <- MM.gets statePackers
  e' <- retrieveTypes e
  let g0 = Gamma {gammaCounter = 0, gammaContext = []}
  case synth g0 e' of
    (Left err) -> MM.throwError . ConcreteTypeError $ err
    (Right (_, _, e'')) -> weaveAndResolve e''

retrieveTypes
  :: SAnno (Indexed Type) One (Indexed Lang)
  -> MorlocMonad (SAnno (Indexed Type) One (Indexed [EType]))
retrieveTypes = undefined

weaveAndResolve
  :: SAnno (Indexed Type) One (Indexed UnresolvedType)
  -> MorlocMonad (SAnno Int One (Indexed TypeP))
weaveAndResolve = undefined


synth
  :: Gamma
  -> SAnno (Indexed Type) One (Indexed [EType])
  -> Either
       TypeError
       ( Gamma
       , UnresolvedType
       , SAnno (Indexed Type) One (Indexed UnresolvedType)
       )
synth = undefined


check
  :: Gamma
  -> SAnno (Indexed Type) One (Indexed [EType])
  -> UnresolvedType
  -> Either
        TypeError
        ( Gamma
        , UnresolvedType
        , SAnno (Indexed Type) One (Indexed UnresolvedType)
        )
check = undefined


synthApply
  :: Gamma
  -> SAnno (Indexed Type) One [EType]
  -> UnresolvedType
  -> Either
       TypeError
       ( Gamma
       , UnresolvedType
       , SAnno (Indexed Type) One (Indexed UnresolvedType)
       )
synthApply = undefined


lookupSourceTypes :: Int -> Source -> MorlocMonad [EType]
lookupSourceTypes i src = do
  mayts <- lookupSig i
  case mayts of
    Nothing -> MM.throwError . CallTheMonkeys $ "Missing TermTypes for source"
    (Just ts) -> case [ es | (_, src', es, _) <- termConcrete ts, src' == src] of
      [es'] -> return es'
      _ -> MM.throwError . CallTheMonkeys $ "Expected exactly one list of types for a source"


-- I don't need explicit convert functions, necessarily. The pack functions can
-- be used to convert between values that are in the same language. Because
-- they hae the same general types and the general types define the packed
-- form. Minimizing convert steps would certainly be a valuable optimization,
-- but I can leave that for later.

-- -- | Ensure that all concrete source signatures match general types
-- checkSources :: SAnno (Indexed Type) One (Indexed Lang) -> MorlocMonad ()
-- checkSources (SAnno (Many xs) i) = do
--   mayts <- lookupSig i
--   case mayts |>> toTypePairs >>= mapM (uncurry checkConcrete) of
--     (Just ((e1, e2):_)) -> undefined -- create error message for mismatched general/concrete types
--     _ -> return ()
--   where
--   toTypePairs :: TermTypes -> [(EType, EType)]
--   toTypePairs (TermTypes Nothing _ _) = []
--   toTypePairs (TermTypes (Just gt) cts _) = [(gt, ct) | ct <- concat [ts | (_, _, ts, _) <- cts]]
--
--   -- return Nothing if the types are the same, otherwise return the types
--   checkConcrete :: EType -> EType -> Maybe (EType, EType)
--   checkConcrete e1 e2
--     | checkConcreteType e1 e2 = Nothing
--     | otherwise = Just (e1, e2)
--
-- -- | This is a key function that is exported primarily so it can be tested.
-- checkConcreteType :: EType -> EType -> Bool
-- checkConcreteType = undefined
