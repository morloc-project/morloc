{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Frontend.Rename
Description : Give every local binder a unique name
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Every name bound locally -- a lambda parameter, a @let@ binder, a @where@
binding and its signature -- is renamed to @name`k@, unique across the whole
program, and every reference that resolves to it lexically is rewritten to
match. Top-level terms and instance methods keep their names: those are
resolved across modules by Link.

Treeify expands a term's body at each place the term is named, so after
renaming a free name in that body can only mean the binder it meant where it
was written; it cannot be captured by a same-named binder at the use site, a
@where@ binding cannot be mistaken for the outer term it shadows, and two
unrelated local helpers never share a name.

A backtick occurs in neither a source identifier nor an operator.
'displayName' removes the suffix for anything shown to a user.
-}
module Morloc.Frontend.Rename
  ( renameLocals
  , displayName
  , fresh
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Morloc.Frontend.Namespace
import qualified Morloc.Monad as MM

type Env = Map.Map EVar EVar

-- | Rename the local binders of one module.
renameLocals :: ExprI -> MorlocMonad ExprI
renameLocals (ExprI i (ModE m es)) = ExprI i . ModE m <$> mapM topDecl es
renameLocals e = expr Map.empty e

-- | The name as the user wrote it, without the suffix any rename or generated
-- name appends (everything from the first backtick).
displayName :: EVar -> EVar
displayName (EV v) = EV (T.takeWhile (/= '`') v)

-- | A fresh local name for @v@.
fresh :: EVar -> MorlocMonad EVar
fresh v = do
  k <- MM.getCounter
  return (EV (unEVar (displayName v) <> "`" <> T.pack (show k)))

-- | A top-level declaration: its own name is global, its body and where-block
-- are local scopes.
topDecl :: ExprI -> MorlocMonad ExprI
topDecl (ExprI i (AssE v e es)) = do
  (e', es') <- definition Map.empty e es
  return (ExprI i (AssE v e' es'))
topDecl (ExprI i (IstE cls ctx ts es)) = ExprI i . IstE cls ctx ts <$> mapM topDecl es
topDecl e = expr Map.empty e

-- | A definition's body and where-block, which see the parameters and every
-- where-binding (the block is recursive). No where-binding shares a
-- parameter's name ('Desugar.checkWhereScope').
definition :: Env -> ExprI -> [ExprI] -> MorlocMonad (ExprI, [ExprI])
definition env e es = do
  (_, env1) <- bindAll env (uniq (concatMap declName es))
  (params, env2) <- case e of
    ExprI _ (LamE vs _) -> bindAll env1 vs
    _ -> return ([], env1)
  e' <- case e of
    ExprI i (LamE _ body) -> ExprI i . LamE params <$> expr env2 body
    _ -> expr env2 e
  es' <- mapM (localDecl env1 env2) es
  return (e', es')
  where
    declName (ExprI _ (AssE v _ _)) = [v]
    declName (ExprI _ (SigE (Signature v _ _))) = [v]
    declName _ = []
    uniq = Set.toList . Set.fromList

-- | A where-block entry. 'names' renames the entry's own name; 'env' is the
-- scope its body is resolved in.
localDecl :: Env -> Env -> ExprI -> MorlocMonad ExprI
localDecl names env (ExprI i (AssE v e es)) = do
  (e', es') <- definition env e es
  return (ExprI i (AssE (lookupName names v) e' es'))
localDecl names _ (ExprI i (SigE (Signature v l t))) =
  return (ExprI i (SigE (Signature (lookupName names v) l t)))
localDecl _ env e = expr env e

bindAll :: Env -> [EVar] -> MorlocMonad ([EVar], Env)
bindAll env vs = do
  vs' <- mapM fresh vs
  return (vs', foldr (uncurry Map.insert) env (zip vs vs'))

lookupName :: Env -> EVar -> EVar
lookupName env v = Map.findWithDefault v v env

expr :: Env -> ExprI -> MorlocMonad ExprI
expr env (ExprI i e0) = ExprI i <$> go e0
  where
    rec = expr env
    go (VarE cfg v) = return (VarE cfg (lookupName env v))
    go (BopE l p op r) = BopE <$> rec l <*> pure p <*> pure (lookupName env op) <*> rec r
    go (LamE vs body) = do
      (vs', env') <- bindAll env vs
      LamE vs' <$> expr env' body
    -- sequential: each right-hand side sees only the binders before it
    go (LetE binds body) = do
      (binds', env') <- letBinds env binds
      LetE binds' <$> expr env' body
    go (AssE v e es) = do
      (e', es') <- definition env e es
      return (AssE (lookupName env v) e' es')
    go (ModE m es) = ModE m <$> mapM topDecl es
    go (IstE cls ctx ts es) = IstE cls ctx ts <$> mapM topDecl es
    go (LstE es) = LstE <$> mapM rec es
    go (TupE es) = TupE <$> mapM rec es
    go (NamE kes) = NamE <$> mapM (\(k, x) -> (,) k <$> rec x) kes
    go (AppE f xs) = AppE <$> rec f <*> mapM rec xs
    go (AnnE x t) = AnnE <$> rec x <*> pure t
    go (ConE tv n k xs) = ConE tv n k <$> mapM rec xs
    go (IfE c t f) = IfE <$> rec c <*> rec t <*> rec f
    go (DoBlockE x) = DoBlockE <$> rec x
    go (EvalE x) = EvalE <$> rec x
    go (IntrinsicE intr xs) = IntrinsicE intr <$> mapM rec xs
    go (ParenE x) = ParenE <$> rec x
    go e = return e

letBinds :: Env -> [(EVar, ExprI)] -> MorlocMonad ([(EVar, ExprI)], Env)
letBinds env [] = return ([], env)
letBinds env ((v, rhs) : rest) = do
  rhs' <- expr env rhs
  v' <- fresh v
  (rest', env') <- letBinds (Map.insert v v' env) rest
  return ((v', rhs') : rest', env')
