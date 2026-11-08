{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Frontend.Generalize
Description : Give unsigned top-level definitions their most general types
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

POLY-8: an unsigned top-level definition is inferred once and given the
most general type its body has, which then stands as its signature: it is
checked against it, and each use instantiates it.
-}
module Morloc.Frontend.Generalize (generalize) where

import Control.Monad.Except (catchError, throwError)
import qualified Data.Set as Set
import Morloc.Data.Doc
import qualified Morloc.Data.Map as Map
import Morloc.Frontend.Namespace
import Morloc.Frontend.Treeify (Validation (..), definitionTree, firstPerTerm, unsignedDefs)
import Morloc.Frontend.Typecheck (checkSigned, inferScheme)
import qualified Morloc.Monad as MM

-- | Give each unsigned top-level definition its type.
-- A definition is inferred after the unsigned definitions it refers to, so
-- that it refers to them by their types; definitions that refer to each
-- other are expanded into one another and inferred together. A definition
-- whose type no signature could state keeps no signature, and is expanded
-- into each of its uses.
generalize :: Validation -> MorlocMonad Validation
generalize checks = do
  sigs <- MM.gets stateSignatures
  let byTerm =
        Map.fromList
          [ (k, d)
          | d@(k, _, _) <- firstPerTerm (concatMap (unsignedDefs sigs) (validationModules checks))
          ]
  (_, typed) <- foldM (visit byTerm) (Set.empty, Map.empty) (Map.keys byTerm)
  return
    checks
      { validationDefs = Map.union (validationDefs checks) typed
      , validationInferred = Set.union (validationInferred checks) (Map.keysSet typed)
      }

visit ::
  Map.Map Int (Int, EVar, Int) ->
  (Set.Set Int, Map.Map Int (Int, EVar)) ->
  Int ->
  MorlocMonad (Set.Set Int, Map.Map Int (Int, EVar))
visit byTerm (seen, typed) k
  | Set.member k seen = return (seen, typed)
  | otherwise = case Map.lookup k byTerm of
      Nothing -> return (seen, typed)
      Just d@(_, v, i) -> do
        let seen1 = Set.insert k seen
            attempt tree
              | valueCycle tree = return (Left "it is part of a cycle of values")
              | otherwise = inferScheme tree
        firstPass <- scratch $ do
          tree <- definitionTree d
          deps <- referenced byTerm tree
          case filter (`Set.notMember` seen1) deps of
            [] -> Right <$> attempt tree
            pending -> return (Left pending)
        (seen', typed', inferred) <- case firstPass of
          Right (Left pending) -> do
            (s', t') <- foldM (visit byTerm) (seen1, typed) pending
            r <- scratch (definitionTree d >>= attempt)
            return (s', t', r)
          Right (Right r) -> return (seen1, typed, Right r)
          Left e -> return (seen1, typed, Left e)
        case inferred of
          Right (Right et) -> do
            install k (Just et)
            ok <- scratch (definitionTree d >>= checkSigned)
            case ok of
              Right _ -> do
                MM.sayV $ "generalized" <+> pretty v <+> "::" <+> tupled (map pretty (Set.toList (econs et))) <+> "=>" <+> pretty (etype et)
                return (seen', Map.insert k (i, v) typed')
              Left _ -> do
                install k Nothing
                MM.sayV $ "not generalized" <+> pretty v <> ": its inferred type" <+> pretty (etype et) <+> "does not check"
                return (seen', typed')
          Right (Left reason) -> do
            MM.sayV $ "not generalized" <+> pretty v <> ":" <+> reason
            return (seen', typed')
          Left _ -> do
            MM.sayV $ "not generalized" <+> pretty v <> ": inference failed"
            return (seen', typed')

-- | A back-edge reached outside every lambda: a value that needs itself,
-- which the recursion check reports where the definition is expanded.
valueCycle :: AnnoS Int ManyPoly Int -> Bool
valueCycle (AnnoS _ _ e) = case e of
  CallS _ -> True
  LamS _ _ -> False
  _ -> or (foldExprS (\x -> [valueCycle x]) e)

-- | The unsigned top-level terms a definition's tree expands.
referenced :: Map.Map Int (Int, EVar, Int) -> AnnoS Int ManyPoly Int -> MorlocMonad [Int]
referenced byTerm tree = do
  GMap idmap _ <- MM.gets stateSignatures
  return
    [ j
    | AnnoS gi _ (VarS _ (MonomorphicExpr Nothing (_ : _))) <- annoNodes tree
    , Just j <- [Map.lookup gi idmap]
    , Map.member j byTerm
    ]

install :: Int -> Maybe EType -> MorlocMonad ()
install k et = MM.modify' $ \s -> case stateSignatures s of
  GMap idmap sigmap -> case Map.lookup k sigmap of
    Just (Monomorphic t) -> s {stateSignatures = GMap idmap (Map.insert k (Monomorphic t {termGeneral = et}) sigmap)}
    _ -> s

-- | Run an action on a copy of the state, which is then restored: collecting
-- and checking copy index-keyed state. A program's error is returned; a
-- compiler fault is raised.
scratch :: MorlocMonad a -> MorlocMonad (Either MorlocError a)
scratch act = do
  before <- MM.get
  r <- (Right <$> act) `catchError` (return . Left)
  MM.put before
  case r of
    Left e@(Fault _ _) -> throwError e
    _ -> return r
