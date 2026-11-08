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
import Morloc.Frontend.Rename (displayName)
import Morloc.Frontend.Typecheck (checkSigned, inferScheme, typecheckRoot)
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
  root <- MM.gets stateModuleName
  MM.modify' $ \st ->
    st
      { stateRootUnsigned =
          Set.fromList
            [ v
            | m@(ExprI _ (ModE mv _)) <- validationModules checks
            , Just mv == root
            , (_, v, _) <- unsignedDefs sigs m
            ]
      }
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
            attempt tree = case valueCycle [] tree of
              Just ring -> Refused <$> cycleError i ring
              Nothing -> do
                before <- MM.get
                r <- (Right <$> inferScheme tree) `catchError` (return . Left)
                case r of
                  Right x -> return (Done x)
                  Left e@(Fault _ _) -> throwError e
                  Left e -> do
                    -- SIG-10: a definition inference refuses is an error
                    -- unless it checks as an export would.
                    MM.put before
                    root <- (Right <$> typecheckRoot tree) `catchError` (return . Left)
                    case root of
                      Left e'@(Fault _ _) -> throwError e'
                      Left e' -> return (Refused e')
                      Right _ -> do
                        st <- MM.get
                        return (Done (Left ("inference failed:" <+> message st e)))
        firstPass <- scratchBy (either (const Nothing) refusedAttempt) $ do
          tree <- definitionTree d
          deps <- referenced byTerm tree
          case filter (`Set.notMember` seen1) deps of
            [] -> Right <$> attempt tree
            pending -> return (Left pending)
        (seen', typed', inferred) <- case firstPass of
          Right (Left pending) -> do
            (s', t') <- foldM (visit byTerm) (seen1, typed) pending
            r <- scratchBy refusedAttempt (definitionTree d >>= attempt)
            return (s', t', r)
          Right (Right r) -> return (seen1, typed, Right r)
          Left e -> return (seen1, typed, Left e)
        case inferred of
          Right (Refused e) -> throwError e
          Right (Done (Right et)) -> do
            install k (Just et)
            ok <- scratch (definitionTree d >>= checkSigned)
            case ok of
              Right _ -> do
                MM.sayV $ "generalized" <+> pretty v <+> "::" <+> tupled (map pretty (Set.toList (econs et))) <+> "=>" <+> pretty (etype et)
                return (seen', Map.insert k (i, v) typed')
              Left e -> do
                st <- MM.get
                throwError . Fault (Just (AtNode i)) $
                  "The type inferred for" <+> squotes (pretty (displayName v)) <> "," <+> pretty (etype et)
                    <> ", does not check against its definition; a signature avoids this:" <+> message st e
          Right (Done (Left why)) -> do
            MM.sayV $ "not generalized" <+> pretty v <> ":" <+> why
            return (seen', typed')
          Left e -> do
            st <- MM.get
            MM.sayV $ "not generalized" <+> pretty v <> ": its tree could not be collected:" <+> MM.makeMorlocError st e
            return (seen', typed')

-- | The outcome of inferring a definition: its scheme, or the reason it has
-- none; or the error that refuses it.
data Attempt = Done (Either MDoc EType) | Refused MorlocError

-- | A back-edge reached outside every lambda: a value that needs itself
-- (BIND-7), with the definitions around it from the one it names, each
-- with its term index.
valueCycle :: [(EVar, Int)] -> AnnoS Int ManyPoly Int -> Maybe [(EVar, Int)]
valueCycle path (AnnoS _ c e) = case e of
  CallS w -> Just (dropWhile ((/= w) . fst) (reverse path))
  LamS _ _ -> Nothing
  VarS w _ -> found ((w, c) : path)
  _ -> found path
  where
    found p = case [r | Just r <- foldExprS (\x -> [valueCycle p x]) e] of
      (r : _) -> Just r
      [] -> Nothing

-- | The BIND-7 error for a ring of bindings, at the definition of its first.
cycleError :: Int -> [(EVar, Int)] -> MorlocMonad MorlocError
cycleError i ring = case ring of
  ((w, c) : _) -> do
    GMap idmap _ <- MM.gets stateSignatures
    sites <- MM.gets stateDefinitionSite
    let at = fromMaybe i (Map.lookup c idmap >>= (`Map.lookup` sites))
        names = map (name . fst) ring
    return . Rejected (AtNode at) $
      "The bindings" <+> hsep (punctuate " ->" (names <> take 1 names))
        <+> "form a cycle, and computing" <+> name w <+> "needs the value of" <+> name w <+> "itself"
  [] -> return (Fault (Just (AtNode i)) "a value cycle with no bindings")
  where
    name x = squotes (pretty (displayName x))

-- | The unsigned top-level terms a definition's tree expands.

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

-- | An error's text, without the location a message it is quoted in has.
message :: MorlocState -> MorlocError -> MDoc
message st e = case e of
  Rejected _ m -> m
  Fault _ m -> m
  _ -> MM.makeMorlocError st e

install :: Int -> Maybe EType -> MorlocMonad ()
install k et = MM.modify' $ \s -> case stateSignatures s of
  GMap idmap sigmap -> case Map.lookup k sigmap of
    Just (Monomorphic t) -> s {stateSignatures = GMap idmap (Map.insert k (Monomorphic t {termGeneral = et}) sigmap)}
    _ -> s

-- | Run an action on a copy of the state, which is then restored: collecting
-- and checking copy index-keyed state. A program's error is returned; a
-- compiler fault is raised.
scratch :: MorlocMonad a -> MorlocMonad (Either MorlocError a)
scratch = scratchBy (const Nothing)

-- | 'scratch' that raises the refusal a result carries, in the state the
-- action left, whose indices its location names.
scratchBy :: (a -> Maybe MorlocError) -> MorlocMonad a -> MorlocMonad (Either MorlocError a)
scratchBy refusal act = do
  before <- MM.get
  r <- (Right <$> act) `catchError` (return . Left)
  case r of
    Left e@(Fault _ _) -> throwError e
    Right x | Just e <- refusal x -> throwError e
    _ -> MM.put before >> return r

refusedAttempt :: Attempt -> Maybe MorlocError
refusedAttempt (Refused e) = Just e
refusedAttempt _ = Nothing
