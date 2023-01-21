{-# LANGUAGE OverloadedStrings #-}

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
  , unpackArgument
  , typeOfExprM
  , gmetaOf
  , argsOf
  , typeOfTypeM
  , invertExprM
  , packTypeM
  , packExprM
  , unpackTypeM
  , nargsTypeM
  , arg2typeM
  , type2jsontype
  , jsontype2json
  , splitArgs
  , PoolDocs(..)
  , mergePoolDocs
  ) where

import Morloc.Data.Doc
import Morloc.CodeGenerator.Namespace
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.CodeGenerator.Serial as MCS


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

nargsTypeM :: TypeM -> Int
nargsTypeM (Function ts _) = length ts
nargsTypeM _ = 0

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
invertExprM (ManifoldM m args e) = do
  MM.startCounter
  e' <- invertExprM e
  return $ ManifoldM m args e'
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
invertExprM (LamM vs body) = do
  -- restart the counter, this is NOT a lambda expression so variables are NOT
  -- in the parent scope, the body will be in a fresh function declaration and
  -- this function will be called with
  -- arguments `vs`
  MM.startCounter
  LamM vs <$> invertExprM body
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

-- | Get the manifold type of an expression
--
-- The ExprM must have exactly enough type information to infer the type of any
-- element without reference to the element's parent.
typeOfExprM :: ExprM f -> TypeM
typeOfExprM (ManifoldM _ args e) = Function (map arg2typeM args) (typeOfExprM e)
typeOfExprM (PoolCallM t _ _ _) = t
typeOfExprM (LetM _ _ e2) = typeOfExprM e2

-- ------------------------------------------
-- FIXME: I clearly need to store more type info foreign calls
typeOfExprM (ForeignInterfaceM t _) = t          -- FIXME, this is just the return type, should be a function
typeOfExprM (AppM (PoolCallM t _ _ _) _) = t     -- FIXME, only correct when fully applied
typeOfExprM (AppM (ForeignInterfaceM t _) _) = t -- FIXME, only correct when fully applied
-- ------------------------------------------

typeOfExprM (AppM f xs) = case typeOfExprM f of
  (Function inputs output) -> case drop (length xs) inputs of
    [] -> output
    inputs' -> Function inputs' output
  _ -> error . MT.unpack . render $ "COMPILER BUG: application of non-function" <+> parens (pretty $ typeOfExprM f)
typeOfExprM (SrcM t _) = t
typeOfExprM (LamM args x) = Function (map arg2typeM args) (typeOfExprM x)
typeOfExprM (BndVarM t _) = t
typeOfExprM (LetVarM t _) = t
typeOfExprM (AccM e _) = typeOfExprM e
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

packTypeM :: TypeM -> TypeM
packTypeM (Native t) = Serial t
packTypeM (Function _ _) = error "BUG: Cannot pack a function"
packTypeM t = t

unpackTypeM :: TypeM -> TypeM
unpackTypeM (Serial t) = Native t
unpackTypeM Passthrough = error "BUG: Cannot unpack a passthrough type"
unpackTypeM t = t 

packExprM :: GIndex -> ExprM Many -> MorlocMonad (ExprM Many)
packExprM m e = do
  packers <- MM.metaPackMap m
  case typeOfExprM e of
    (Native t) -> SerializeM <$> MCS.makeSerialAST packers t <*> pure e
    -- (Function _ _) -> error "Cannot pack a function"
    _ -> return e

type2jsontype :: TypeP -> MorlocMonad JsonType
type2jsontype (VarP (PV _ _ v)) = return $ VarJ v
type2jsontype (AppP (VarP (PV _ _ v)) ts) = ArrJ v <$> mapM type2jsontype ts
type2jsontype (AppP _ _) = MM.throwError . SerializationError $ "Invalid JSON type: complex AppP"
type2jsontype (NamP _ (PV _ _ v) _ rs) = do
  ts <- mapM (type2jsontype . snd) rs
  return $ NamJ v (zip [k | (PV _ _ k, _) <- rs] ts) 
type2jsontype (UnkP _) = MM.throwError . SerializationError $ "Invalid JSON type: UnkT"
type2jsontype (FunP _ _) = MM.throwError . SerializationError $ "Invalid JSON type: cannot serialize function"

jsontype2json :: JsonType -> MDoc
jsontype2json (VarJ v) = dquotes (pretty v)
jsontype2json (ArrJ v ts) = "{" <> key <> ":" <> val <> "}" where
  key = dquotes (pretty v)
  val = encloseSep "[" "]" "," (map jsontype2json ts)
jsontype2json (NamJ v rs) = "{" <> dquotes (pretty v) <> ":" <> encloseSep "{" "}" "," rs' <> "}" where
  keys = map (dquotes . pretty) (map fst rs) 
  vals = map jsontype2json (map snd rs)
  rs' = zipWith (\key val -> key <> ":" <> val) keys vals

argsOf :: ExprM f -> [Argument]
argsOf (LamM args _) = args
argsOf (ManifoldM _ args _) = args
argsOf _ = []

gmetaOf :: ExprM f -> GIndex
gmetaOf (ManifoldM m _ _) = m
gmetaOf (LamM _ e) = gmetaOf e
gmetaOf _ = error "Malformed top-expression"

-- divide a list of arguments based on wheither they are in a second list
splitArgs :: [Argument] -> [Argument] -> ([Argument], [Argument])
splitArgs args1 args2 = partitionEithers $ map splitOne args1 where
  splitOne :: Argument -> Either Argument Argument
  splitOne r = if elem r args2
               then Left r
               else Right r
