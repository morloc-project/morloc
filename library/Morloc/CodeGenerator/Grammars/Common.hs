{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Morloc.CodeGenerator.Grammars.Common
Description : A common set of utility functions for language templates
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.CodeGenerator.Grammars.Common
  ( argType
  , packArgument
  , unpackArgument
  , replaceArgumentType
  , typeOfExprM
  , gmetaOf
  , typeOfTypeM
  , invertExprM
  , packTypeM
  , packExprM
  , unpackExprMByType
  , unpackExprM
  , unpackTypeM
  , nargsTypeM
  , argsOf
  , arg2typeM
  , PoolDocs(..)
  , mergePoolDocs
  ) where

import Morloc.Data.Doc
import Morloc.CodeGenerator.Namespace
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.CodeGenerator.Serial as MCS

import qualified Data.Set as Set

-- Stores pieces of code made while building a pool
data PoolDocs = PoolDocs
  { poolCompleteManifolds :: [MDoc]
    -- ^ completely generated manifolds
  , poolExpr :: MDoc
    -- ^ the inplace expression
  , poolPriorLines :: [MDoc]
    -- ^ lines to precede the returned expression
  , poolPriorExprs :: [MDoc]
    -- ^ expressions that should precede this manifold, may include helper
    -- functions or imports
  }

-- | Merge a series of pools, keeping prior lines, expression and manifolds, but
-- merging bodies with a function. For example, merge all elements in a list and
-- process the poolExpr variales into list syntax in the given language.
mergePoolDocs :: ([MDoc] -> MDoc) -> [PoolDocs] -> PoolDocs
mergePoolDocs f ms = PoolDocs
    { poolCompleteManifolds = concatMap poolCompleteManifolds ms
    , poolExpr = f (map poolExpr ms)
    , poolPriorLines = concatMap poolPriorLines ms
    , poolPriorExprs = concatMap poolPriorExprs ms
    }

argType :: Argument -> Maybe TypeP
argType (SerialArgument _ t) = Just t
argType (NativeArgument _ t) = Just t
argType (PassThroughArgument _) = Nothing

unpackArgument :: Argument -> Argument
unpackArgument (SerialArgument i t) = NativeArgument i t
unpackArgument x = x

packArgument :: Argument -> Argument
packArgument (NativeArgument i t) = SerialArgument i t
packArgument x = x

nargsTypeM :: TypeM -> Int
nargsTypeM (Function ts _) = length ts
nargsTypeM _ = 0

argsOf :: ExprM f -> Set.Set Argument
argsOf (ManifoldM _ form x) = Set.fromList (manifoldArgs form) `Set.union` argsOf x
argsOf (ForeignInterfaceM _ _ x) = argsOf x
argsOf (PoolCallM _ _ _ args) = Set.fromList args
argsOf (LetM _ _ x) = argsOf x
argsOf (AppM x xs) = Set.unions (map argsOf (x:xs))
argsOf (LamM contextArgs boundArgs x) = (argsOf x `Set.union` Set.fromList contextArgs) `Set.difference` Set.fromList boundArgs
argsOf (AccM x _) = argsOf x
argsOf (ListM _ xs) = Set.unions (map argsOf xs)
argsOf (TupleM _ xs) = Set.unions (map argsOf xs)
argsOf (RecordM _ ks) = Set.unions (map (argsOf . snd) ks)
argsOf (SerializeM _ x) = argsOf x
argsOf (DeserializeM _ x) = argsOf x
argsOf (ReturnM x) = argsOf x
argsOf _ = Set.empty

-- recordPVar :: TypeP -> MDoc
-- recordPVar (VarP (PV _ _ v)) = pretty v
-- recordPVar (UnkP (PV _ _ v)) = pretty v
-- recordPVar (FunP _ _) = "<FunP>" -- illegal value
-- recordPVar (AppP _ _) = "<AppP>" -- illegal value
-- recordPVar (NamP _ (PV _ _ v) _ _) = pretty v

-- see page 112 of my super-secret notes ...
-- example:
-- > f [g x, 42] (h 1 [1,2])
-- converts to:
-- > let a0 = g x
-- > in let a1 = [a0, 42]
-- >    in let a2 = [1,2]
-- >       in let a3 = h 1 a2
-- >          in f a1 a3
-- expression inversion will not alter expression type
invertExprM :: ExprM f -> MorlocMonad (ExprM f)
invertExprM (ManifoldM m form e) = do
  MM.startCounter
  e' <- invertExprM e
  return $ ManifoldM m form e'
invertExprM (LetM v e1 e2) = do
  e2' <- invertExprM e2
  return $ LetM v e1 e2'
invertExprM (AppM c@(PoolCallM _ _ _ _) es) = do
  es' <- mapM invertExprM es
  return $ foldl dependsOn (AppM c (map terminalOf es')) es'
invertExprM (PoolCallM t i cmds args) = do
  v <- MM.getCounter
  return $ LetM v (PoolCallM t i cmds args) (LetVarM t v)
invertExprM e@(AppM f es) = do
  f' <- invertExprM f
  es' <- mapM invertExprM es
  v <- MM.getCounter
  let t = typeOfExprM e
      appM' = LetM v (AppM (terminalOf f') (map terminalOf es')) (LetVarM t v)
  return $ foldl dependsOn appM' (f':es') 
-- A LamM will generate a new function declaration in the output code. This
-- function will be like a manifold, but lighter, since at the moment the only
-- thing it is used for is wrapping higher order functions that call external manifolds.
invertExprM (LamM contextArgs boundArgs body) = do
  -- restart the counter, this is NOT a lambda expression so variables are NOT
  -- in the parent scope, the body will be in a fresh function declaration and
  -- this function will be called with
  -- arguments `vs`
  MM.startCounter
  LamM contextArgs boundArgs <$> invertExprM body
invertExprM (AccM e k) = do
  e' <- invertExprM e
  return $ dependsOn (AccM (terminalOf e') k) e'
invertExprM (ListM c es) = do
  es' <- mapM invertExprM es
  v <- MM.getCounter
  let e = LetM v (ListM c (map terminalOf es')) (LetVarM c v)
      e' = foldl dependsOn e es'
  return e'
invertExprM (TupleM c es) = do
  es' <- mapM invertExprM es
  v <- MM.getCounter
  let e = LetM v (TupleM c (map terminalOf es')) (LetVarM c v)
      e' = foldl dependsOn e es'
  return e'
invertExprM (RecordM c entries) = do
  es' <- mapM (invertExprM . snd) entries
  v <- MM.getCounter
  let entries' = zip (map fst entries) (map terminalOf es')
      e = LetM v (RecordM c entries') (LetVarM c v)
      e' = foldl dependsOn e es'
  return e'
invertExprM (SerializeM p e) = do
  e' <- invertExprM e
  v <- MM.getCounter
  let t' = packTypeM $ typeOfExprM e
  return $ dependsOn (LetM v (SerializeM p (terminalOf e')) (LetVarM t' v)) e'
invertExprM (DeserializeM p e) = do
  e' <- invertExprM e
  v <- MM.getCounter
  let t' = unpackTypeM $ typeOfExprM e
  return $ dependsOn (LetM v (DeserializeM p (terminalOf e')) (LetVarM t' v)) e'
invertExprM (ReturnM e) = do
  e' <- invertExprM e
  return $ dependsOn (ReturnM (terminalOf e')) e'
invertExprM e = return e

-- transfer all let-dependencies from y to x
--
-- Technically, I should check for variable reuse in the let-chain and
-- resolve conflicts be substituting in fresh variable names. However, for
-- now, I will trust that my name generator created names that are unique
-- within the manifold.
dependsOn :: ExprM f -> ExprM f -> ExprM f
dependsOn x (LetM v e y) = LetM v e (dependsOn x y)
dependsOn x _ = x

-- get the rightmost expression in a let-tree
terminalOf :: ExprM f -> ExprM f
terminalOf (LetM _ _ e) = terminalOf e
terminalOf e = e

typeOfTypeM :: TypeM -> Maybe TypeP 
typeOfTypeM Passthrough = Nothing
typeOfTypeM (Serial c) = Just c
typeOfTypeM (Native c) = Just c
typeOfTypeM (Function ins out) = FunP <$> mapM typeOfTypeM ins <*> typeOfTypeM out

arg2typeM :: Argument -> TypeM
arg2typeM (SerialArgument _ c) = Serial c
arg2typeM (NativeArgument _ c) = Native c
arg2typeM (PassThroughArgument _) = Passthrough

replaceArgumentType :: TypeP -> Argument -> Argument
replaceArgumentType t (SerialArgument i _) = SerialArgument i t
replaceArgumentType t (NativeArgument i _) = NativeArgument i t
replaceArgumentType _ (PassThroughArgument i) = PassThroughArgument i

-- | Get the manifold type of an expression
--
-- The ExprM must have exactly enough type information to infer the type of any
-- element without reference to the element's parent.
typeOfExprM :: ExprM f -> TypeM
typeOfExprM (ManifoldM _ form e) = Function (map arg2typeM (manifoldArgs form)) (typeOfExprM e)
typeOfExprM (PoolCallM t _ _ _) = t
typeOfExprM (LetM _ _ e2) = typeOfExprM e2

-- ------------------------------------------
-- FIXME: I clearly need to store more type info foreign calls
typeOfExprM (ForeignInterfaceM t _ _) = t          -- FIXME, this is just the return type, should be a function
typeOfExprM (AppM (PoolCallM t _ _ _) _) = t     -- FIXME, only correct when fully applied
typeOfExprM (AppM (ForeignInterfaceM t _ _) _) = t -- FIXME, only correct when fully applied
-- ------------------------------------------

typeOfExprM (AppM f xs) = case typeOfExprM f of
  (Function inputs output) -> case drop (length xs) inputs of
    [] -> output
    inputs' -> Function inputs' output
  _ -> error . MT.unpack . render $ "COMPILER BUG: application of non-function" <+> parens (pretty $ typeOfExprM f)
typeOfExprM (SrcM t _) = t
-- The lambda function will pass scoped arguments to its children, but only the
-- bound arguments are part of the type signature
typeOfExprM (LamM _ boundArgs x) = Function (map arg2typeM boundArgs) (typeOfExprM x)
typeOfExprM (BndVarM t _) = t
typeOfExprM (LetVarM t _) = t
typeOfExprM (AccM e key) = case typeOfExprM e of
  Passthrough -> error "Passthrough type cannot be accessed by key"
  (Function _ _) -> error "Function type cannot be accessed by key"
  (Serial (NamP _ _ _ rs)) -> Serial (lookupKey rs)
  (Native (NamP _ _ _ rs)) -> Native (lookupKey rs)
  _ -> error "Bad access, expected a named type"
  where
    lookupKey :: [(PVar, TypeP)] -> TypeP
    lookupKey [] = error "Bad access, key not found"
    lookupKey ((PV _ _ key', t) : rs')
        | key == key' = t
        | otherwise = lookupKey rs'
typeOfExprM (ListM t _) = t
typeOfExprM (TupleM t _) = t
typeOfExprM (RecordM t _) = t
typeOfExprM (LogM t _) = t
typeOfExprM (RealM t _) = t
typeOfExprM (IntM t _) = t
typeOfExprM (StrM t _) = t
typeOfExprM (NullM t) = t
typeOfExprM (SerializeM _ e) = packTypeM (typeOfExprM e)
typeOfExprM (DeserializeM _ e) = unpackTypeM (typeOfExprM e)
typeOfExprM (ReturnM e) = typeOfExprM e

-- | Serialize a type if possible, otherwise return the original value
packTypeM :: TypeM -> TypeM
packTypeM (Native t) = Serial t
packTypeM t@(Function _ _) = t -- functions cannot be serialized
packTypeM t = t

unpackTypeM :: TypeM -> TypeM
unpackTypeM (Serial t) = Native t
unpackTypeM Passthrough = error "BUG: Cannot unpack a passthrough type"
unpackTypeM t = t 

-- Deserializes anything that is not already Native
-- FIXME needing both of these functions is fucked up
unpackExprM :: GIndex -> TypeP -> ExprM Many -> MorlocMonad (ExprM Many) 
unpackExprM m p e = do
  packers <- MM.metaPackMap m
  case typeOfExprM e of
    (Serial t)  -> DeserializeM <$> MCS.makeSerialAST packers t <*> pure e
    Passthrough -> DeserializeM <$> MCS.makeSerialAST packers p <*> pure e
    _ -> return e

-- Deserializes anything that is not already Native
unpackExprMByType :: GIndex -> TypeP -> ExprM Many -> MorlocMonad (ExprM Many) 
unpackExprMByType m p e = do
  packers <- MM.metaPackMap m
  case typeOfExprM e of
    (Serial _)  -> DeserializeM <$> MCS.makeSerialAST packers p <*> pure e
    Passthrough -> DeserializeM <$> MCS.makeSerialAST packers p <*> pure e
    _ -> return e

packExprM :: GIndex -> ExprM Many -> MorlocMonad (ExprM Many)
packExprM m e = do
  packers <- MM.metaPackMap m
  case typeOfExprM e of
    (Native t) -> SerializeM <$> MCS.makeSerialAST packers t <*> pure e
    -- (Function _ _) -> error "Cannot pack a function"
    _ -> return e

gmetaOf :: ExprM f -> GIndex
gmetaOf (ManifoldM m _ _) = m
gmetaOf (LamM _ _ e) = gmetaOf e
gmetaOf _ = error "Malformed top-expression"
