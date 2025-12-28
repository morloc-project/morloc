{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Morloc.CodeGenerator.Docstrings
Description : Generate the final docstring records
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}

module Morloc.CodeGenerator.Docstrings (processDocstrings) where

import Morloc.Namespace
import Morloc.CodeGenerator.Namespace
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Morloc.Monad as MM
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.BaseTypes as MBT

-- Most of the transmogrification of docstrings occurs in the parser, but there
-- are some limitations there since the types are not yet known. If a type in a
-- function signature is labeled as `unrolled: true`, then the standard
-- positional argument should be replaced with a group of arguments for every
-- field in the record. But the type signature for the function will have only
-- the name of the type, not the details.
--
-- In addition to unrolling arguments, docstring information can be inherited
-- even for non-record types. The main use case for this is defining a type that
-- has a description and short/long option terms that are reused across function
-- signatures.
--
-- This top-level function collects docstring info and passes it to `processArgDoc`
processDocstrings :: AnnoS (Indexed Type) One a -> MorlocMonad (AnnoS (Indexed Type) One a, CmdDocSet)
processDocstrings e@(AnnoS (Idx i t) _ _) = do
    sgmap <- MM.gets stateSignatures
    argdoc <- case GMap.lookup i sgmap of
      (GMapJust (Monomorphic (TermTypes (Just et) _ _))) -> return $ edocs et
      (GMapJust (Polymorphic _ _ et _)) -> return $ edocs et
      _ -> case t of
        (FunT ts _) -> return $ ArgDocSig defaultValue (take (length ts) (repeat defaultValue)) defaultValue
        _ -> return $ ArgDocAlias defaultValue
    doc <- processArgDoc i t argdoc
    return (e, doc)

-- dispatch docstring info for each argument to `processArgDoc`
processArgDoc :: Int -> Type -> ArgDoc -> MorlocMonad CmdDocSet
processArgDoc i (FunT ts t) (ArgDocSig cmddoc argdocs retdoc) = do
  (ts', argdocs') <- zipWithM (reduceArgDoc i) ts (map ArgDocAlias argdocs) |>> unzip
  cmdargs <- zipWithM makeCmdArg ts' argdocs'
  (t', retdoc') <- reduceArgDoc i t (ArgDocAlias retdoc)
  return $ CmdDocSet
    { cmdDocDesc = docLines cmddoc
    , cmdDocName = docName cmddoc
    , cmdDocArgs = cmdargs
    , cmdDocRet = (t', getReturnDesc retdoc' (docReturn cmddoc))
    }
processArgDoc i t (ArgDocSig cmddoc [] retdoc) = do
  (t', retdoc') <- reduceArgDoc i t (ArgDocAlias retdoc)
  return $ CmdDocSet
    { cmdDocDesc = docLines cmddoc
    , cmdDocName = docName cmddoc
    , cmdDocArgs = []
    , cmdDocRet = (t', getReturnDesc retdoc' (docReturn cmddoc))
    }
processArgDoc _ t (ArgDocAlias r) =
  return $ CmdDocSet
    { cmdDocDesc = docLines r
    , cmdDocName = docName r
    , cmdDocArgs = []
    , cmdDocRet = (t, [])
    }
processArgDoc i t r = do
  (t', r') <- reduceArgDoc i t r
  case (t', r') of
    (NamT _ _ _ ts, ArgDocRec args entries) -> do
      cmdargs <- zipWithM makeCmdArg (map snd ts) (map (ArgDocAlias . snd) entries)
      return $ CmdDocSet
        { cmdDocDesc = docLines args
        , cmdDocName = docName args
        , cmdDocArgs = cmdargs
        , cmdDocRet = (t, [])
        }
    _ -> error "Impossible - record type and argument expected"


getReturnDesc :: ArgDoc -> Maybe Text -> [Text]
getReturnDesc _ (Just ret) = [ret]
getReturnDesc (ArgDocRec r _) _ = docLines r
getReturnDesc (ArgDocSig r _ _) _ = docLines r
getReturnDesc (ArgDocAlias r) _ = docLines r

reduceArgDoc :: Int -> Type -> ArgDoc -> MorlocMonad (Type, ArgDoc)
reduceArgDoc i t@(VarT v) arg = do
  scope <- MM.getGeneralScope i
  case Map.lookup v scope of
    (Just [(_, typeOf -> parentType, parentArg, _)]) ->
      inheritArgDoc arg parentArg >>= reduceArgDoc i parentType
    (Just _) -> error "Impossible?"
    Nothing -> return (t, arg)
  where

  inheritArgDoc :: ArgDoc -> ArgDoc -> MorlocMonad ArgDoc
  inheritArgDoc (ArgDocAlias r1) (ArgDocAlias r2) = return $ ArgDocAlias (inheritArgDocVars r1 r2)
  inheritArgDoc (ArgDocAlias r1) (ArgDocRec r2 rs) = return $ ArgDocRec (inheritArgDocVars r1 r2) rs
  inheritArgDoc _ _ = error "Impossible?"

  inheritArgDocVars :: ArgDocVars -> ArgDocVars -> ArgDocVars
  inheritArgDocVars r1 r2 = ArgDocVars
    { docLines = if (length (docLines r1) > 0) then docLines r1 else docLines r2
    , docName = docName r1 <|> docName r2
    , docLiteral = docLiteral r1 <|> docLiteral r2
    , docUnroll = docUnroll r1 <|> docUnroll r2
    , docDefault = docDefault r1 <|> docDefault r2
    , docMetavar = docMetavar r1 <|> docMetavar r2
    , docArg = docArg r1 <|> docArg r2
    , docTrue = docTrue r1 <|> docTrue r2
    , docFalse = docFalse r1 <|> docFalse r2
    , docReturn = docReturn r1 <|> docReturn r2
    }
reduceArgDoc _ t r = return (t, r)

makeCmdArg :: Type -> ArgDoc -> MorlocMonad CmdArg
makeCmdArg recType@(NamT _ _ _ rs) (ArgDocRec arg entries) = do
  -- Set the metavar default for groups to the record type name
  let typedEntries = zipWith (\(k,t) (_,r) -> (k, (t,r))) rs entries
  resolveArgDocVars typedEntries recType arg
makeCmdArg t (ArgDocRec r _) = resolveArgDocVars [] t r
makeCmdArg t (ArgDocAlias r) = resolveArgDocVars [] t r
makeCmdArg _ (ArgDocSig _ _ _) = MM.throwError . DocStrError $ "Illegal functional CLI parameter"

resolveArgDocVars :: [(Key, (Type, ArgDocVars))] -> Type -> ArgDocVars -> MorlocMonad CmdArg
resolveArgDocVars rs t r
  | docUnroll r == Just False = resolvePos t r |>> CmdArgPos
  | length rs > 0 && docUnroll r == Just True = resolveGrp t r rs
  | t == VarT MBT.bool = resolveFlagCmdArg r
  | isJust (docArg r) = resolveOpt t r |>> CmdArgOpt
  | otherwise = resolvePos t r |>> CmdArgPos

resolveGrp :: Type -> ArgDocVars -> [(Key, (Type, ArgDocVars))] -> MorlocMonad CmdArg
resolveGrp recType@(NamT _ v _ _) arg argEntries = do
  entries <- mapM resolveRecDocVars argEntries
  return . CmdArgGrp $ RecDocSet
       { recDocType = recType
       , recDocDesc = docLines arg
       , recDocMetavar = fromMaybe (unTVar v) (docMetavar arg)
       , recDocOpt = docArg arg
       , recDocEntries = entries
       }
  where
    resolveRecDocVars :: (Key, (Type, ArgDocVars)) -> MorlocMonad (Key, Either ArgFlagDocSet ArgOptDocSet)
    resolveRecDocVars (k, (t, r))
      | t == VarT MBT.bool = do
          eitherFlag <- resolveFlag r
          case eitherFlag of
            (Right flag) -> return $ (k, Left flag)
            (Left _) -> MM.throwError . DocStrError $ "Non-optional field found in unrolled record"
      | otherwise = do
          opt <- resolveOpt t r
          return (k, Right opt)
resolveGrp _ _ _ = error "Impossible, groups must be records"

-- resolve a boolean into either a flag option or a positional
resolveFlag :: ArgDocVars -> MorlocMonad (Either ArgPosDocSet ArgFlagDocSet)
resolveFlag r =
  case (docTrue r, docFalse r, (==) "true" <$> docDefault r) of
      -- if no default value is given, make default based on given args
      -- e.g., true: -v/--verbose
      (Just rt, Nothing, Nothing    ) -> flag rt  Nothing   False
      -- e.g., false: -q/--quit
      (Nothing, Just rf, Nothing    ) -> flag rf  Nothing   True
      -- e.g., true: -v/--verbose
      --       false: -q/--quit
      (Just rt, Just rf, Nothing    ) -> flag rt  (Just rf) False
      -- set default to TRUE
      (Nothing, Just rf, Just True  ) -> flag rf  Nothing   True
      (Just rt, Just rf, Just True  ) -> flag rf  (Just rt) True
      -- set default to FALSE
      (Just rt, Nothing, Just False ) -> flag rt  Nothing   False
      (Just rt, Just rf, Just False ) -> flag rt  (Just rf) False
      -- handle noop cases
      (Just _, Nothing, Just True  ) -> MM.throwError . DocStrError $ "Noop flag"
      (Nothing, Just _, Just False ) -> MM.throwError . DocStrError $ "Noop flag"
      -- handle positional with a given default
      (Nothing, Nothing, Just _    ) -> MM.throwError . DocStrError $ "Positional argument with default"
      -- handle positional
      (Nothing, Nothing, Nothing    ) -> return . Left $ ArgPosDocSet
        { argPosDocType = VarT MBT.bool
        , argPosDocDesc = docLines r
        , argPosDocMetavar = docMetavar r <|> Just "BOOL"
        , argPosDocLiteral = docLiteral r
        }
  where
  flag :: CliOpt -> Maybe CliOpt -> Bool -> MorlocMonad (Either ArgPosDocSet ArgFlagDocSet)
  flag opt rev def = return . Right $ ArgFlagDocSet
    { argFlagDocDesc = docLines r
    , argFlagDocOpt = opt
    , argFlagDocOptRev = rev
    , argFlagDocDefault = if def then "true" else "false"
    }

resolveFlagCmdArg :: ArgDocVars -> MorlocMonad CmdArg
resolveFlagCmdArg r = do
  eitherFlag <- resolveFlag r
  case eitherFlag of
    (Right flag) -> return . CmdArgFlag $ flag
    (Left pos) -> return . CmdArgPos $ pos

resolveOpt :: Type -> ArgDocVars -> MorlocMonad ArgOptDocSet
resolveOpt t r = case (docArg r, docDefault r) of
  (Nothing, _) -> MM.throwError . DocStrError $ "Optional argument missing tags"
  (_, Nothing) -> MM.throwError . DocStrError $ "Optional arguments must have default values"
  (Just opt, Just def) -> return $ ArgOptDocSet
    { argOptDocType = t
    , argOptDocDesc = docLines r
    , argOptDocMetavar = fromMaybe (makeOptMeta t) (docMetavar r)
    , argOptDocLiteral = docLiteral r
    , argOptDocArg = opt
    , argOptDocDefault = def
    }

makeOptMeta :: Type -> Text
makeOptMeta (UnkT v) = unTVar v
makeOptMeta (VarT v) = unTVar v
makeOptMeta (FunT _ _) = "FUN" -- illegal, but who's watching?
makeOptMeta (AppT (VarT v) _) = unTVar v
makeOptMeta (AppT _ _) = "VAL" -- weird stuff, choose your own metadata
makeOptMeta (NamT _ v _ _) = unTVar v

resolvePos :: Type -> ArgDocVars -> MorlocMonad ArgPosDocSet
resolvePos t r = do
  return $ ArgPosDocSet
    { argPosDocType = t
    , argPosDocDesc = docLines r
    , argPosDocMetavar = docMetavar r
    , argPosDocLiteral = docLiteral r
    }
