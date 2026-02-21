{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.CodeGenerator.Parameterize
Description : Propagate function arguments down through the AnnoS tree
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Threads the top-level function parameters through the expression tree
so that each manifold node knows which arguments it needs. This is
necessary because the tree may contain multiple alternative implementations
that share the same parameter interface.
-}
module Morloc.CodeGenerator.Parameterize
  ( parameterize
  ) where

import Data.Text (Text)
import Morloc.CodeGenerator.Namespace
import Morloc.Data.Doc
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM

{- | Add arguments that are required for each term. Unneeded arguments are
removed at each step.
-}
parameterize ::
  AnnoS (Indexed Type) One (Indexed Lang) ->
  MorlocMonad (AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]))
parameterize (AnnoS m@(Idx _ (FunT inputs _)) c (LamS vs x)) = do
  MM.sayVVV "Entering parameterize LamS"
  ids <- MM.takeFromCounter (length inputs)
  let args0 = fromJust $ safeZipWith Arg ids vs
  x' <- parameterize' args0 x
  return $ AnnoS m (c, args0) (LamS vs x')
parameterize (AnnoS m@(Idx _ (FunT inputs _)) c@(Idx _ lang) (BndS v)) = do
  MM.sayVVV $ "Entering parameterize VarS function - " <> pretty v <> "@" <> pretty lang
  ids <- MM.takeFromCounter (length inputs)
  let vs = map EV (freshVarsAZ [])
      args0 = fromJust $ safeZipWith Arg ids vs
  return $ AnnoS m (c, args0) (BndS v)
parameterize x = do
  MM.sayVVV "Entering parameterize Other"
  parameterize' [] x

parameterize' ::
  [Arg EVar] -> -- arguments in parental scope (child needn't retain them)
  AnnoS (Indexed Type) One (Indexed Lang) ->
  MorlocMonad (AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]))
-- primitives, no arguments are required for a primitive, so empty lists
parameterize' _ (AnnoS g c UniS) = return $ AnnoS g (c, []) UniS
parameterize' _ (AnnoS g c (RealS x)) = return (AnnoS g (c, []) (RealS x))
parameterize' _ (AnnoS g c (IntS x)) = return (AnnoS g (c, []) (IntS x))
parameterize' _ (AnnoS g c (LogS x)) = return (AnnoS g (c, []) (LogS x))
parameterize' _ (AnnoS g c (StrS x)) = return (AnnoS g (c, []) (StrS x))
parameterize' args (AnnoS g c (BndS v)) = do
  let args' = [r | r@(Arg _ v') <- args, v' == v]
  return $ AnnoS g (c, args') (BndS v)
parameterize' _ (AnnoS m c (ExeS (SrcCall src))) =
  return $ AnnoS m (c, []) (ExeS (SrcCall src))
parameterize' _ (AnnoS g c (ExeS (PatCall x))) =
  return (AnnoS g (c, []) (ExeS (PatCall x)))
parameterize' args (AnnoS g c (LstS xs)) = do
  xs' <- mapM (parameterize' args) xs
  let args' = pruneArgs args xs'
  return $ AnnoS g (c, args') (LstS xs')
parameterize' args (AnnoS g c (TupS xs)) = do
  xs' <- mapM (parameterize' args) xs
  let args' = pruneArgs args xs'
  return $ AnnoS g (c, args') (TupS xs')
parameterize' args (AnnoS g c (NamS entries)) = do
  xs' <- mapM (parameterize' args . snd) entries
  let args' = pruneArgs args xs'
  return $ AnnoS g (c, args') (NamS (zip (map fst entries) xs'))
parameterize' args (AnnoS g@(Idx _ (FunT inputs _)) c (LamS vs x)) = do
  ids <- MM.takeFromCounter (length inputs)
  let contextArgs = [r | r@(Arg _ v) <- args, v `notElem` vs] -- remove shadowed arguments
      boundArgs = fromJust $ safeZipWith Arg ids vs
  x' <- parameterize' (contextArgs <> boundArgs) x
  let contextArgs' = pruneArgs contextArgs [x']
  return $ AnnoS g (c, contextArgs' <> boundArgs) (LamS vs x')
-- LamS MUST have a functional type, deviations would have been caught by the typechecker
parameterize' _ (AnnoS _ _ (LamS _ _)) = error "impossible"
parameterize' args (AnnoS g c (AppS x xs)) = do
  x' <- parameterize' args x
  xs' <- mapM (parameterize' args) xs
  let args' = pruneArgs args (x' : xs')
  return $ AnnoS g (c, args') (AppS x' xs')
parameterize' args (AnnoS g c (LetBndS v)) = do
  let args' = [r | r@(Arg _ v') <- args, v' == v]
  return $ AnnoS g (c, args') (LetBndS v)
parameterize' args (AnnoS g c (LetS v e1 e2)) = do
  e1' <- parameterize' args e1
  idx <- MM.getCounter
  let letArg = Arg idx v
      bodyArgs = letArg : [r | r@(Arg _ v') <- args, v' /= v]
  e2' <- parameterize' bodyArgs e2
  let args' = pruneArgs args [e1', e2']
  return $ AnnoS g (c, args') (LetS v e1' e2')
parameterize' args (AnnoS g c (SuspendS e)) = do
  e' <- parameterize' args e
  let args' = pruneArgs args [e']
  return $ AnnoS g (c, args') (SuspendS e')
parameterize' args (AnnoS g c (ForceS e)) = do
  e' <- parameterize' args e
  let args' = pruneArgs args [e']
  return $ AnnoS g (c, args') (ForceS e')
parameterize' _ (AnnoS _ _ (VarS _ _)) = undefined

pruneArgs :: [Arg a] -> [AnnoS c One (g, [Arg a])] -> [Arg a]
pruneArgs args xs =
  let usedArgs = unique $ concatMap (map ann . sannoSnd) xs
   in [r | r@(Arg i _) <- args, i `elem` usedArgs]

sannoSnd :: AnnoS g One (a, b) -> b
sannoSnd (AnnoS _ (_, x) _) = x

-- generate infinite list of fresh variables of form
-- ['a','b',...,'z','aa','ab',...,'zz',...]
freshVarsAZ ::
  [Text] -> -- variables to exclude
  [Text]
freshVarsAZ exclude =
  filter
    (`notElem` exclude)
    ([1 ..] >>= flip replicateM ['a' .. 'z'] |>> MT.pack)
