{-|
Module      : TypeHandler
Description : Functions for handling general Morloc type operations
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.TypeHandler
(
    childOf 
  , standardizeGenerics
  , extractGenerics
  , findMostSpecificType
  , findMostGeneralType
  , chooseRealization
  , chooseAbstraction
  , chooseDeclaration
) where

import Morloc.Global
import Morloc.Operators
import qualified Morloc.Monad as M
import qualified Morloc.Data.Text as MT
import qualified Data.List as DL

-- | Determine if the first MType object is equal to, or a specialization of,
-- the second MType object.
childOf
  :: MType -- ^ Instance type (e.g. Matrix Num 5 6)
  -> MType -- ^ Parent type (e.g. Matrix a m n)
  -> Bool  -- ^ True if arg #1 is equal to or an instance of arg #2
childOf (MConcType _ n1 xs1) (MConcType _ n2 xs2)
  -- I currently don't check out the properties, should I?
  = n1 == n2 -- type names must be the same 
  && all id (zipWith childOf xs1 xs2) -- child types must be the same

childOf (MConcType _ _ xs1) (MAbstType _ _ xs2)
  =  length xs2 == 0   -- TODO: check for required properties
  || all id (zipWith childOf xs1 xs2)
childOf (MAbstType _ _ xs1) (MAbstType _ _ xs2)
  = all id (zipWith childOf xs1 xs2)
childOf (MFuncType _ is1 o1) (MFuncType _ is2 o2)
  = all id (zipWith childOf is1 is2) && childOf o1 o2
childOf _ _ = False

-- | Replace generic variables with canonically numbered ids. For example, the
-- signature, "A x a -> B Int y a", would become "A x0 x1 -> B Int x2 x1".
standardizeGenerics :: MType -> MorlocMonad MType
standardizeGenerics t = f (extractGenerics t) t where
  f :: [Name] -> MType -> MorlocMonad MType
  f ts (MConcType d n xs) = MConcType <$> pure d <*> pure n <*> (mapM (f ts) xs)
  f ts (MAbstType d n xs) = MAbstType <$> pure d <*> nameGeneric ts n <*> (mapM (f ts) xs)
  f ts (MFuncType d inputs out) = MFuncType <$> pure d <*> mapM (f ts) inputs <*> f ts out

  nameGeneric :: [Name] -> Name -> MorlocMonad Name
  nameGeneric ts n = case (DL.elemIndex n ts) of
    (Just i) -> return $ "x" <> MT.show' i
    Nothing  ->  M.throwError $ CallTheMonkeys "TypeHandler::standardizeGenerics"

-- | Extract the names of all generic variables (in order)
extractGenerics :: MType -> [Name]
extractGenerics = DL.nub . extractGenerics' where
  extractGenerics' :: MType -> [Name]
  extractGenerics' (MConcType _ _ xs) = concat . map extractGenerics $ xs
  extractGenerics' (MAbstType _ n xs) = n:(concat . map extractGenerics $ xs)
  extractGenerics' (MFuncType _ xs out)
    = (concat . map extractGenerics $ xs) ++ extractGenerics out

-- | Get the most specifc type from a list of compatible types. By compatible I
-- mean types where either (childOf a b) or (childOf b a) is true. That is, one
-- of them must generalize the other.
findMostSpecificType :: [MType] -> Maybe MType
findMostSpecificType [] = Nothing
findMostSpecificType xs = Just (maximum xs)

-- | Find the most general type. Also see @findMostSpecificType@.
findMostGeneralType :: [MType] -> Maybe MType
findMostGeneralType [] = Nothing
findMostGeneralType xs = Just (minimum xs)

-- | Chooses which manifold instance to use when there are multiple
-- alternatives. It should choose the instance that that is supported on the
-- current system and that maximizes performance (e.g., by avoiding foreign
-- calls). Currently, however, it just chooses whichever realization is first
-- in the list.
chooseRealization :: [Manifold] -> Manifold -> Manifold 
chooseRealization _ m = case mRealizations m of
  -- -------------^ ignored, since I am not using context yet
  [] -> m
  [_] -> m
  (x:_) -> m { mRealizations = [x] }

-- | What should we do when there are multiple definitions of a given morloc
-- function name? We could raise an error. We could implement overloading of
-- some sort. We use the latest definition. For now I will use the most general
-- type, and look into alternatives later.
chooseAbstraction :: [MType] -> Maybe MType
chooseAbstraction = findMostGeneralType

-- | What should we do when there are multiple definitions of a morloc
-- function? For now, I'll just choose the last one defined.
chooseDeclaration :: [FunctionDeclaration] -> Maybe FunctionDeclaration
chooseDeclaration [] = Nothing
chooseDeclaration [f] = Just f
chooseDeclaration fs = Just (head . reverse $ fs) -- choose the latest and greatest?
