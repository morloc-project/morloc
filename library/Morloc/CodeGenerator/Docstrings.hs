{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.CodeGenerator.Docstrings
Description : Generate CLI help text and argument documentation for exported functions
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Processes docstring annotations from type signatures into the final
'MDoc' records used by the nexus for @--help@ output, including argument
names, default values, metavars, and CLI option flags.
-}
module Morloc.CodeGenerator.Docstrings
  ( processDocstrings
  , argLocPrefix
  -- * CLI shape validation
  , ArgShape (..)
  , StrShape (..)
  , ListPipeline (..)
  , LineFileMode (..)
  , BytesVariant (..)
  , InlineBytesVariant (..)
  , validateArgShape
  , validateCmdArg
  -- * Schema-text predicates (used by emit helpers in Nexus.hs)
  , peelHint
  , peelRecDecl
  , isStrWireSchema
  , isOptStrWireSchema
  , isStrOrOptStrWireSchema
  , isArrayWireSchema
  , listElementSchema
  , isListOfStrWireSchema
  , isTabularRowWireSchema
  , isFixedWidthScalar
  , SchemaCat (..)
  , classifySchema
  ) where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Morloc.BaseTypes as MBT
import Morloc.CodeGenerator.Namespace
import Morloc.Data.Doc
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM

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
processDocstrings ::
  AnnoS (Indexed Type) One a -> MorlocMonad (AnnoS (Indexed Type) One a, CmdDocSet)
processDocstrings e@(AnnoS (Idx i t) _ _) = do
  sgmap <- MM.gets stateSignatures
  argdoc <- case GMap.lookup i sgmap of
    (GMapJust (Monomorphic (TermTypes (Just et) _ _))) -> return $ edocs et
    (GMapJust (Polymorphic _ _ et _)) -> return $ edocs et
    _ -> case t of
      (FunT ts _) -> return $ ArgDocSig defaultValue (take (length ts) (repeat defaultValue)) defaultValue
      _ -> return $ ArgDocAlias defaultValue
  -- Declaration-level docstrings take precedence over signature docstrings
  -- for the command-level description.
  declDocs <- lookupDeclDocs i
  let argdoc' = case declDocs of
        [] -> argdoc
        ls -> overrideCmdDocLines ls argdoc
  doc <- processArgDoc i t argdoc'
  return (e, doc)

-- | Look up the declaration-level docstring for the term at a given index.
-- Returns an empty list if no declaration docstring exists.
lookupDeclDocs :: Int -> MorlocMonad [Text]
lookupDeclDocs i = do
  nameMap <- MM.gets stateName
  case Map.lookup i nameMap of
    Nothing -> return []
    Just name -> do
      termDocs <- MM.gets stateTermDocs
      return $ Map.findWithDefault [] name termDocs

-- | Override the command-level docLines while preserving all other docstring
-- fields (argument docs, return docs, metavars, etc.).
overrideCmdDocLines :: [Text] -> ArgDoc -> ArgDoc
overrideCmdDocLines ls (ArgDocSig cmd args ret) =
  ArgDocSig (cmd { docLines = ls }) args ret
overrideCmdDocLines ls (ArgDocRec vars fields) =
  ArgDocRec (vars { docLines = ls }) fields
overrideCmdDocLines ls (ArgDocAlias vars) =
  ArgDocAlias (vars { docLines = ls })

-- | A "In <module>:<function>, " prefix used to locate a faulty argument
-- in diagnostics. Falls back gracefully when the module or term name is
-- unavailable at this stage.
argLocPrefix :: Int -> MorlocMonad MDoc
argLocPrefix i = do
  mmod <- MM.gets stateModuleName
  mfun <- Map.lookup i <$> MM.gets stateName
  return $ case (mmod, mfun) of
    (Just m, Just f) -> "In " <> pretty m <> ":" <> pretty f <> ", "
    (Just m, Nothing) -> "In module " <> pretty m <> ", "
    (Nothing, Just f) -> "In " <> pretty f <> ", "
    (Nothing, Nothing) -> ""

-- dispatch docstring info for each argument to `processArgDoc`
processArgDoc :: Int -> Type -> ArgDoc -> MorlocMonad CmdDocSet
processArgDoc i (FunT ts t) (ArgDocSig cmddoc argdocs retdoc) = do
  (ts', argdocs') <- zipWithM (reduceArgDoc i) ts (map ArgDocAlias argdocs) |>> unzip
  loc <- argLocPrefix i
  validateCommandLevelDirectives loc cmddoc
  cmdargs <-
    sequence
      [ makeCmdArg (loc <> "argument #" <> pretty n) t' a'
      | (n, t', a') <- zip3 [(1 :: Int) ..] ts' argdocs'
      ]
  validateManyOrdering loc cmdargs
  validateFlagRevCollisions loc cmdargs
  (t', retdoc') <- reduceArgDoc i t (ArgDocAlias retdoc)
  return $
    CmdDocSet
      { cmdDocDesc = docLines cmddoc
      , cmdDocName = docName cmddoc
      , cmdDocArgs = cmdargs
      , cmdDocRet = (t', getReturnDesc retdoc' (docReturn cmddoc))
      }
processArgDoc i t (ArgDocSig cmddoc [] retdoc) = do
  loc <- argLocPrefix i
  validateCommandLevelDirectives loc cmddoc
  (t', retdoc') <- reduceArgDoc i t (ArgDocAlias retdoc)
  return $
    CmdDocSet
      { cmdDocDesc = docLines cmddoc
      , cmdDocName = docName cmddoc
      , cmdDocArgs = []
      , cmdDocRet = (t', getReturnDesc retdoc' (docReturn cmddoc))
      }
processArgDoc i t (ArgDocAlias r) = do
  loc <- argLocPrefix i
  validateCommandLevelDirectives loc r
  return $
    CmdDocSet
      { cmdDocDesc = docLines r
      , cmdDocName = docName r
      , cmdDocArgs = []
      , cmdDocRet = (t, [])
      }
processArgDoc i t r = do
  (t', r') <- reduceArgDoc i t r
  case (t', r') of
    (NamT _ _ _ ts, ArgDocRec args entries) -> do
      loc <- argLocPrefix i
      validateCommandLevelDirectives loc args
      cmdargs <-
        sequence
          [ makeCmdArg (loc <> "field " <> pretty k) ty (ArgDocAlias dv)
          | ((k, ty), (_, dv)) <- zip ts entries
          ]
      return $
        CmdDocSet
          { cmdDocDesc = docLines args
          , cmdDocName = docName args
          , cmdDocArgs = cmdargs
          , cmdDocRet = (t, [])
          }
    _ -> MM.throwSystemError "Expected a record type with docstrings but found a non-record type"

-- | Reject argument-only directives that appear at the command /
-- function level (i.e. on the docstring lines above `name ::`
-- instead of inside the signature next to a specific type). Without
-- this check the directive is silently dropped: `literal: true`
-- above a function only affects the command's metadata record,
-- never any argument, so the user's intent never reaches the
-- manifest.
validateCommandLevelDirectives :: MDoc -> ArgDocVars -> MorlocMonad ()
validateCommandLevelDirectives loc r =
  mapM_ check
    [ ("literal",     isJust (docLiteral r))
    , ("many",        isJust (docMany r))
    , ("unroll",      isJust (docUnroll r))
    , ("default",     isJust (docDefault r))
    , ("metavar",     isJust (docMetavar r))
    , ("arg",         isJust (docArg r))
    , ("true",        isJust (docTrue r))
    , ("false",       isJust (docFalse r))
    , ("source",      isJust (docSource r))
    , ("form",        isJust (docForm r))
    , ("check",       not (null (docChecks r)))
    , ("list.source", isJust (docListSource r))
    , ("list.form",   isJust (docListForm r))
    , ("list.check",  not (null (docListChecks r)))
    ]
  where
    check :: (Text, Bool) -> MorlocMonad ()
    check (name, True) = MM.throwSystemError $
      loc <> "docstring directive '" <> pretty name <> "' cannot be used"
        <> " to describe a function; it applies to a specific argument and"
        <> " must appear inside the signature."
    check (_, False) = return ()

getReturnDesc :: ArgDoc -> Maybe Text -> [Text]
getReturnDesc _ (Just ret) = [ret]
getReturnDesc (ArgDocRec r _) _ = docLines r
getReturnDesc (ArgDocSig r _ _) _ = docLines r
getReturnDesc (ArgDocAlias r) _ = docLines r

reduceArgDoc :: Int -> Type -> ArgDoc -> MorlocMonad (Type, ArgDoc)
reduceArgDoc i t@(VarT v) arg = do
  scope <- MM.getGeneralScope i
  case Map.lookup v scope of
    -- Record-newtype: declared via @record Foo where { ... }@. The body
    -- is a 'NamU' carrying the record-level and field-level
    -- docstrings; non-record newtypes are opaque (their docstring is
    -- their own). Walk into the NamU so the CLI flag derivation sees
    -- the per-field @arg:@/@metavar:@/@default:@ directives.
    (Just [(_, typeOf -> parentType@(NamT _ _ _ _), parentArg, _, TypedefNewtype)]) ->
      inheritArgDoc arg parentArg >>= reduceArgDoc i parentType
    -- Newtype and primitive boundaries otherwise stop inheritance:
    -- their docstring is their own, the wire-parent's (if any) is not
    -- consulted.
    (Just [(_, _, _, _, TypedefNewtype)]) -> return (t, arg)
    (Just [(_, _, _, _, TypedefPrimitive)]) -> return (t, arg)
    (Just [(_, typeOf -> parentType, parentArg, _, TypedefAlias)]) ->
      inheritArgDoc arg parentArg >>= reduceArgDoc i parentType
    (Just _) -> MM.throwSystemError $ "Multiple definitions for type alias '" <> pretty (unTVar v) <> "'"
    Nothing -> return (t, arg)
  where
    inheritArgDoc :: ArgDoc -> ArgDoc -> MorlocMonad ArgDoc
    inheritArgDoc (ArgDocAlias r1) (ArgDocAlias r2) = return $ ArgDocAlias (inheritArgDocVars r1 r2)
    inheritArgDoc (ArgDocAlias r1) (ArgDocRec r2 rs) = return $ ArgDocRec (inheritArgDocVars r1 r2) rs
    inheritArgDoc _ _ = MM.throwSystemError $ "Cannot inherit docstrings for type alias '" <> pretty (unTVar v) <> "'"

    inheritArgDocVars :: ArgDocVars -> ArgDocVars -> ArgDocVars
    inheritArgDocVars r1 r2 =
      ArgDocVars
        { docLines = if (length (docLines r1) > 0) then docLines r1 else docLines r2
        , docName = docName r1 <|> docName r2
        , docLiteral = docLiteral r1 <|> docLiteral r2
        , docMany = docMany r1 <|> docMany r2
        , docUnroll = docUnroll r1 <|> docUnroll r2
        , docDefault = docDefault r1 <|> docDefault r2
        , docMetavar = docMetavar r1 <|> docMetavar r2
        , docArg = docArg r1 <|> docArg r2
        , docTrue = docTrue r1 <|> docTrue r2
        , docFalse = docFalse r1 <|> docFalse r2
        , docReturn = docReturn r1 <|> docReturn r2
        , docSource = docSource r1 <|> docSource r2
        , docForm = docForm r1 <|> docForm r2
        , docChecks = if null (docChecks r1) then docChecks r2 else docChecks r1
        , docListSource = docListSource r1 <|> docListSource r2
        , docListForm = docListForm r1 <|> docListForm r2
        , docListChecks = if null (docListChecks r1) then docListChecks r2 else docListChecks r1
        }
reduceArgDoc i (NamT o v ps (map snd -> ts)) (ArgDocRec arg rs) = do
  let args = map (ArgDocAlias . snd) rs
      keys = map fst rs
  entries <- zipWithM (reduceArgDoc i) ts args
  let args' = [r | (ArgDocAlias r) <- map snd entries]
  return (NamT o v ps (zip keys (map fst entries)), ArgDocRec arg (zip keys args'))
reduceArgDoc _ t r = return (t, r)

makeCmdArg :: MDoc -> Type -> ArgDoc -> MorlocMonad CmdArg
makeCmdArg loc recType@(NamT _ _ _ rs) (ArgDocRec arg entries) = do
  -- Set the metavar default for groups to the record type name
  let typedEntries = zipWith (\(k, t) (_, r) -> (k, (t, r))) rs entries
  resolveArgDocVars loc typedEntries recType arg
makeCmdArg loc t (ArgDocRec r _) = resolveArgDocVars loc [] t r
makeCmdArg loc t (ArgDocAlias r) = resolveArgDocVars loc [] t r
makeCmdArg _ _ (ArgDocSig _ _ _) = MM.throwSystemError "Illegal functional CLI parameter"

resolveArgDocVars :: MDoc -> [(Key, (Type, ArgDocVars))] -> Type -> ArgDocVars -> MorlocMonad CmdArg
resolveArgDocVars loc rs t r
  -- `default:` only makes sense on a flag (true:/false:) or an
  -- option (arg:). On a bare positional it is rejected. An
  -- unrolled record group is exempt: each field carries its own
  -- directives.
  | isJust (docDefault r)
      && isNothing (docArg r)
      && isNothing (docTrue r)
      && isNothing (docFalse r)
      && not (not (null rs) && docUnroll r == Just True) =
      MM.throwSystemError $
        loc
          <> " is given a default value, but positional arguments are"
          <> " required and cannot take defaults. Either remove the"
          <> " 'default:' line, or make the field optional by adding an"
          <> " 'arg:' docstring entry (e.g. \"arg: -f/--flag\")."
  -- list-of-elements (many) and per-field unroll describe
  -- incompatible CLI shapes for the same slot.
  | docMany r == Just True && docUnroll r == Just True =
      MM.throwSystemError $
        loc <> " cannot combine `many: true` with `unroll: true`."
  | docUnroll r == Just False = resolvePos t r |>> CmdArgPos
  | length rs > 0 && docUnroll r == Just True = resolveGrp loc t r rs
  | t == VarT MBT.bool = resolveFlagCmdArg loc r
  -- Any non-Bool typed argument with `arg:` becomes an option;
  -- `resolveOpt` errors if no `default:` accompanies it.
  | isJust (docArg r) = resolveOpt loc t r |>> CmdArgOpt
  | otherwise = resolvePos t r |>> CmdArgPos

resolveGrp :: MDoc -> Type -> ArgDocVars -> [(Key, (Type, ArgDocVars))] -> MorlocMonad CmdArg
resolveGrp loc recType@(NamT _ v _ _) arg argEntries = do
  entries <- mapM resolveRecDocVars argEntries
  return . CmdArgGrp $
    RecDocSet
      { recDocType = recType
      , recDocDesc = docLines arg
      , recDocMetavar = fromMaybe (unTVar v) (docMetavar arg)
      , recDocOpt = docArg arg
      , recDocEntries = entries
      }
  where
    resolveRecDocVars ::
      (Key, (Type, ArgDocVars)) -> MorlocMonad (Key, Either ArgFlagDocSet ArgOptDocSet)
    resolveRecDocVars (k, (t, r))
      | t == VarT MBT.bool = do
          eitherFlag <- resolveFlag (loc <> ", field " <> pretty (unKey k)) r
          case eitherFlag of
            (Right flag) -> return $ (k, Left flag)
            (Left _) -> MM.throwSystemError $
              loc <> ", field " <> pretty (unKey k) <> ": non-optional field found in unrolled record"
      | otherwise = do
          opt <- resolveOpt (loc <> ", field " <> pretty (unKey k)) t r
          return (k, Right opt)
resolveGrp loc _ _ _ = MM.throwSystemError $
  loc <> ": cannot unroll a non-record type into CLI argument groups"

-- resolve a boolean into either a flag option or a positional
resolveFlag :: MDoc -> ArgDocVars -> MorlocMonad (Either ArgPosDocSet ArgFlagDocSet)
resolveFlag loc r = do
  -- `metavar:` only makes sense on something with a value. A Bool
  -- positional carries an implicit BOOL metavar; an actual flag
  -- carries none. Reject the latter before constructing the flag.
  when (isJust (docMetavar r) && hasAnyFlagDirective) $
    MM.throwSystemError $
      loc <> ": `metavar:` is not allowed on a Bool flag because flags have no value."
        <> " Remove the `metavar:` directive."
  -- A Bool flag's CLI shape is encoded by `true:` / `false:`, not
  -- `arg:`. Reject before the (Nothing, Nothing, Nothing) case
  -- below silently produces a Bool positional and drops the
  -- directive.
  when (isJust (docArg r) && isNothing (docTrue r) && isNothing (docFalse r)) $
    MM.throwSystemError $
      loc <> ": a Bool argument cannot use `arg:`. Use `true: <opt>`"
        <> " (default false, the flag turns it on) or `false: <opt>`"
        <> " (default true, the flag turns it off) instead."
  case (docTrue r, docFalse r, (==) "true" <$> docDefault r) of
    -- if no default value is given, make default based on given args
    -- e.g., true: -v/--verbose
    (Just rt, Nothing, Nothing) -> flag rt Nothing False
    -- e.g., false: -q/--quit
    (Nothing, Just rf, Nothing) -> flag rf Nothing True
    -- e.g., true: -v/--verbose
    --       false: -q/--quit
    (Just rt, Just rf, Nothing) -> flag rt (Just rf) False
    -- set default to TRUE
    (Nothing, Just rf, Just True) -> flag rf Nothing True
    (Just rt, Just rf, Just True) -> flag rf (Just rt) True
    -- set default to FALSE
    (Just rt, Nothing, Just False) -> flag rt Nothing False
    (Just rt, Just rf, Just False) -> flag rt (Just rf) False
    -- Noop: the only declared direction agrees with the default,
    -- so flipping the flag would never change the value.
    (Just rt, Nothing, Just True) -> MM.throwSystemError $
      loc <> ": `true: " <> pretty (makeArg rt) <> "` combined with `default: true`"
        <> " is a no-op -- supplying the flag never changes the value."
        <> " Remove `default: true` (let the flag turn true on from a false default),"
        <> " or replace `true:` with `false:` (the flag will turn the true default off)."
    (Nothing, Just rf, Just False) -> MM.throwSystemError $
      loc <> ": `false: " <> pretty (makeArg rf) <> "` combined with `default: false`"
        <> " is a no-op -- supplying the flag never changes the value."
        <> " Remove `default: false` (let the flag turn false on from a true default),"
        <> " or replace `false:` with `true:` (the flag will turn the false default off)."
    -- handle positional with a given default
    (Nothing, Nothing, Just _) -> MM.throwSystemError "Positional argument with default"
    -- handle positional
    (Nothing, Nothing, Nothing) ->
      return . Left $
        ArgPosDocSet
          { argPosDocType = VarT MBT.bool
          , argPosDocDesc = docLines r
          , argPosDocMetavar = docMetavar r <|> Just "BOOL"
          , argPosDocLiteral = docLiteral r
          , argPosDocMany = False
          , argPosDocSource = docSource r
          , argPosDocForm = docForm r
          , argPosDocChecks = docChecks r
          , argPosDocListSource = docListSource r
          , argPosDocListForm = docListForm r
          , argPosDocListChecks = docListChecks r
          }
  where
    -- "any flag-like directive" -- true/false/default. If none is set
    -- and only metavar: is given, we'd fall through to the Bool
    -- positional branch, where the BOOL metavar IS meaningful. So the
    -- error only fires when the arg actually wants to be a flag.
    hasAnyFlagDirective =
      isJust (docTrue r) || isJust (docFalse r) || isJust (docDefault r) || isJust (docArg r)
    flag :: CliOpt -> Maybe CliOpt -> Bool -> MorlocMonad (Either ArgPosDocSet ArgFlagDocSet)
    flag opt rev def =
      return . Right $
        ArgFlagDocSet
          { argFlagDocDesc = docLines r
          , argFlagDocOpt = opt
          , argFlagDocOptRev = rev
          , argFlagDocDefault = if def then "true" else "false"
          }

resolveFlagCmdArg :: MDoc -> ArgDocVars -> MorlocMonad CmdArg
resolveFlagCmdArg loc r = do
  eitherFlag <- resolveFlag loc r
  case eitherFlag of
    (Right flag) -> return . CmdArgFlag $ flag
    (Left pos) -> return . CmdArgPos $ pos

resolveOpt :: MDoc -> Type -> ArgDocVars -> MorlocMonad ArgOptDocSet
resolveOpt loc t r = do
  let many = docMany r == Just True
  case (docArg r, docDefault r) of
    (Nothing, _) -> MM.throwSystemError $ loc <> ": optional argument missing tags"
    (Just opt, Nothing)
      -- `many` options without an explicit default fall back to "[]".
      | many -> makeOpt many opt "[]"
      -- literal ?Str: auto-default to null (the only way to get null is to omit the flag)
      | isLiteralOptStr -> makeOpt many opt "null"
      | otherwise ->
          MM.throwSystemError $
            loc <> ": optional argument " <> pretty (makeArg opt)
              <> " must be given a default value"
    (Just opt, Just def)
      -- literal ?Str with non-null default is an error
      | isLiteralOptStr && def /= "null" ->
          MM.throwSystemError $
            loc <> ": optional argument " <> pretty (makeArg opt)
            <> " has type ?Str with literal: true, so default must be null (got \""
            <> pretty def <> "\")"
      | otherwise -> makeOpt many opt def
  where
    isLiteralOptStr = docLiteral r == Just True && isOptionalStrType t

    isOptionalStrType (OptionalT (VarT v)) = v == MBT.str
    isOptionalStrType _ = False

    makeOpt many opt def = return $
      ArgOptDocSet
        { argOptDocType = t
        , argOptDocDesc = docLines r
        , argOptDocMetavar = fromMaybe (makeOptMeta t) (docMetavar r)
        , argOptDocLiteral = docLiteral r
        , argOptDocMany = many
        , argOptDocArg = opt
        , argOptDocDefault = def
        , argOptDocSource = docSource r
        , argOptDocForm = docForm r
        , argOptDocChecks = docChecks r
        , argOptDocListSource = docListSource r
        , argOptDocListForm = docListForm r
        , argOptDocListChecks = docListChecks r
        }

makeArg ::
  CliOpt ->
  Text -- argument string, such as "-h/--help"
makeArg (CliOptShort s) = "-" <> MT.singleton s
makeArg (CliOptLong l) = "--" <> l
makeArg (CliOptBoth s l) = "-" <> MT.singleton s <> "/--" <> l

makeOptMeta :: Type -> Text
makeOptMeta (UnkT v) = unTVar v
makeOptMeta (VarT v) = unTVar v
makeOptMeta (FunT _ _) = "FUN" -- illegal, but who's watching?
makeOptMeta (AppT (VarT v) _) = unTVar v
makeOptMeta (AppT _ _) = "VAL" -- weird stuff, choose your own metadata
makeOptMeta (NamT _ v _ _) = unTVar v
makeOptMeta (EffectT _ t) = "<E>" <> makeOptMeta t
makeOptMeta (OptionalT t) = "?" <> makeOptMeta t
makeOptMeta (NatLitT n) = MT.show' n
makeOptMeta (NatAddT _ _) = "NAT"
makeOptMeta (NatMulT _ _) = "NAT"
makeOptMeta (NatSubT _ _) = "NAT"
makeOptMeta (NatDivT _ _) = "NAT"
makeOptMeta NatVoidT = "NAT"
makeOptMeta (StrLitT _) = "STR"
makeOptMeta (StrConcatT _ _) = "STR"
makeOptMeta StrVoidT = "STR"

resolvePos :: Type -> ArgDocVars -> MorlocMonad ArgPosDocSet
resolvePos t r = do
  let many = docMany r == Just True
  return $
    ArgPosDocSet
      { argPosDocType = t
      , argPosDocDesc = docLines r
      , argPosDocMetavar = docMetavar r
      , argPosDocLiteral = docLiteral r
      , argPosDocMany = many
      , argPosDocSource = docSource r
      , argPosDocForm = docForm r
      , argPosDocChecks = docChecks r
      , argPosDocListSource = docListSource r
      , argPosDocListForm = docListForm r
      , argPosDocListChecks = docListChecks r
      }

-- Validate that within a function's argument list, at most one
-- positional carries `many: true`, and it is the LAST positional
-- (options and flags may follow without issue, but no second
-- positional may appear after it -- clap requires variadic
-- positionals to occupy the final positional slot).
validateManyOrdering :: MDoc -> [CmdArg] -> MorlocMonad ()
validateManyOrdering loc = go False
  where
    go :: Bool -> [CmdArg] -> MorlocMonad ()
    go _ [] = return ()
    go seenMany (CmdArgPos r : rest)
      | seenMany && argPosDocMany r =
          MM.throwSystemError $
            loc <> " has more than one positional with `many: true`; at most one is allowed."
      | seenMany =
          MM.throwSystemError $
            loc <> " has a positional with `many: true` that is not the last positional;"
              <> " variadic positionals must occupy the final positional slot."
      | otherwise = go (argPosDocMany r) rest
    go seenMany (_ : rest) = go seenMany rest

-- Reject collisions between option / flag long-form names within a
-- single subcommand. Without this check, two flags that happen to
-- share a long name (e.g. one's `false: --no-foo` matches another's
-- `arg: --no-foo`) fail downstream in clap with an opaque
-- duplicate-argument message instead of pointing at the source.
validateFlagRevCollisions :: MDoc -> [CmdArg] -> MorlocMonad ()
validateFlagRevCollisions loc cmdargs = do
  let longs = concatMap collect cmdargs
      grouped = Map.fromListWith (++) [(n, [src]) | (n, src) <- longs]
      dups = [(n, srcs) | (n, srcs) <- Map.toList grouped, length srcs > 1]
  case dups of
    [] -> return ()
    ((name, srcs) : _) ->
      MM.throwSystemError $
        loc <> ": option long name '--" <> pretty name
          <> "' is declared by " <> hsep (punctuate "," (map pretty srcs))
          <> ". Long-form names must be unique within a subcommand."
  where
    collect :: CmdArg -> [(Text, Text)]
    collect (CmdArgOpt r)  = [(n, "arg:") | n <- longOf (argOptDocArg r)]
    collect (CmdArgFlag r) =
         [(n, mainSrc r) | n <- longOf (argFlagDocOpt r)]
      ++ [(n, revSrc r)  | rev <- maybe [] (:[]) (argFlagDocOptRev r), n <- longOf rev]
    collect (CmdArgGrp r) =
         concatMap (collect . CmdArgOpt)  [opt | (_, Right opt) <- recDocEntries r]
      ++ concatMap (collect . CmdArgFlag) [fl  | (_, Left  fl)  <- recDocEntries r]
    collect (CmdArgPos _)  = []

    -- The main and reverse halves of a flag come from `true:` or
    -- `false:` depending on which side of the default the user
    -- declared first. Naming the directive in the error keeps
    -- diagnostics actionable.
    mainSrc r = if argFlagDocDefault r == "true" then "false:" else "true:"
    revSrc  r = if argFlagDocDefault r == "true" then "true:"  else "false:"

    longOf :: CliOpt -> [Text]
    longOf (CliOptShort _)   = []
    longOf (CliOptLong l)    = [l]
    longOf (CliOptBoth _ l)  = [l]

-- ======================================================================
-- CLI shape validation
-- ======================================================================
--
-- After type checking, every CLI argument has a wire schema (e.g.
-- `s`, `aj`, `t2js`) and a bag of docstring fields (literal/source/
-- form/checks/list.*). 'validateArgShape' folds those into an
-- 'ArgShape' value: every constructor encodes exactly one legal
-- configuration, so downstream emit code does not need to re-check
-- coherence.

-- | Wire-schema category. The hint and recursive-decl prefixes are
-- stripped before classification; the outer `?` of an optional is
-- also peeled so optional types inherit their underlying category.
data SchemaCat
  = CatScalarPrim
  | CatStr
  | CatList Text  -- ^ element schema text
  | CatOtherCompound
  deriving (Eq, Show)

-- | Bytes-family form atoms (`form: bytes`, `form: bytes-only`,
-- `form: packet`) on a fixed-width-scalar array.
data BytesVariant = Bytes | BytesOnly | Packet
  deriving (Eq, Show)

-- | The two variants legal for the `source: inline + form:` ASCII
-- shortcut on `[UInt8]`. `Packet` is excluded by construction.
data InlineBytesVariant = IbBytes | IbBytesOnly
  deriving (Eq, Show)

-- | Per-line dispatch under `form: list + list.source: file`.
data LineFileMode
  = LfmAuto                       -- ^ list.form: auto (default)
  | LfmPacket                     -- ^ list.form: packet (strict)
  | LfmBytes InlineBytesVariant   -- ^ list.form: bytes / bytes-only
  deriving (Eq, Show)

-- | Per-line interpretation under `form: list`.
data ListPipeline
  = LpInlineBare                  -- ^ each line is the bare element value
  | LpInlineCheck PathPerm        -- ^ each line is a Str path; perms checked
  | LpFile LineFileMode           -- ^ each line is a path; opened per mode
  deriving (Eq, Show)

-- | The Str-specific shape variants.
data StrShape
  = StrDefault                    -- ^ Str default: argv is the literal value
  | StrFile                       -- ^ source: file: file contents become the Str
  | StrCheckPath PathPerm         -- ^ check.path on inline argv
  deriving (Eq, Show)

-- | Validated CLI shape. Every constructor encodes one legal
-- configuration; invalid combinations are unrepresentable.
data ArgShape
  = AsScalarPrim
      -- ^ Non-Str scalar primitive. argv is parsed bare; no modifiers.
  | AsStr StrShape
      -- ^ Str with its three variants.
  | AsOtherCompound
      -- ^ Tuple, record, map: no modifiers; default classifier handles
      --   JSON inline or file path.
  | AsListDefault
      -- ^ List type, no modifiers: JSON inline OR auto-classified
      --   file path.
  | AsListMany
      -- ^ `many: true` on a list. Multiple argv tokens, each parsed
      --   as the element type. Mutex with every shape modifier.
  | AsListBytes BytesVariant
      -- ^ Fixed-width-scalar array with `form: bytes`/`bytes-only`/
      --   `packet`. Always file-borne.
  | AsListInlineBytes InlineBytesVariant
      -- ^ `[UInt8]` with `source: inline + form: bytes`/`bytes-only`.
      --   The ASCII byte-sequence shortcut.
  | AsListLines ListPipeline
      -- ^ `form: list` with per-line dispatch.
  deriving (Eq, Show)

-- ── Schema-text utilities ─────────────────────────────────────────

-- | Strip a leading `<...>` hint decoration. Hints carry the
-- concrete-type tag for newtypes / aliases over a built-in primitive.
peelHint :: Text -> Text
peelHint t = case MT.uncons t of
  Just ('<', tt) -> case MT.span (/= '>') tt of
    (_, post) -> case MT.uncons post of
      Just ('>', after) -> peelHint after
      _ -> t
  _ -> t

-- | Strip a leading `&<klen><name>` rec-decl prefix.
peelRecDecl :: Text -> Text
peelRecDecl t = case MT.uncons t of
  Just ('&', tt) -> case MT.uncons tt of
    Just (c, rest)
      | c >= 'a' && c <= 'p' ->
          let klen = fromEnum c - fromEnum 'a'
              (_name, post) = MT.splitAt klen rest
          in post
    _ -> t
  _ -> t

isStrWireSchema :: Text -> Bool
isStrWireSchema s = peelHint (peelRecDecl s) == "s"

isOptStrWireSchema :: Text -> Bool
isOptStrWireSchema s = case MT.uncons (peelHint (peelRecDecl s)) of
  Just ('?', rest) -> peelHint rest == "s"
  _ -> False

isStrOrOptStrWireSchema :: Text -> Bool
isStrOrOptStrWireSchema s = isStrWireSchema s || isOptStrWireSchema s

isArrayWireSchema :: Text -> Bool
isArrayWireSchema s = case MT.uncons (peelHint (peelRecDecl s)) of
  Just ('a', _) -> True
  _ -> False

listElementSchema :: Text -> Maybe Text
listElementSchema s = case MT.uncons (peelHint (peelRecDecl s)) of
  Just ('a', rest) -> Just rest
  _ -> Nothing

isListOfStrWireSchema :: Text -> Bool
isListOfStrWireSchema s = maybe False isStrWireSchema (listElementSchema s)

isTabularRowWireSchema :: Text -> Bool
isTabularRowWireSchema s = case MT.uncons (peelHint (peelRecDecl s)) of
  Just ('t', _) -> True
  Just ('m', _) -> True
  _ -> False

isScalarPrimCore :: Text -> Bool
isScalarPrimCore c = c `elem`
  [ "b", "j", "z"
  , "i1", "i2", "i4", "i8"
  , "u1", "u2", "u4", "u8"
  , "f4", "f8"
  ]

-- | True iff the schema is a fixed-width numeric scalar eligible for
-- `form: bytes` / `form: bytes-only` / `form: packet` element types.
-- `j` (`Int`, arbitrary-precision) and `z` (`Null`) are excluded.
isFixedWidthScalar :: Text -> Bool
isFixedWidthScalar s =
  peelHint s `elem`
    [ "b"
    , "i1", "i2", "i4", "i8"
    , "u1", "u2", "u4", "u8"
    , "f4", "f8"
    ]

classifySchema :: Text -> SchemaCat
classifySchema s0 =
  let peeled = peelHint (peelRecDecl s0)
      core = case MT.uncons peeled of
        Just ('?', rest) -> peelHint rest
        _ -> peeled
  in if core == "s"
       then CatStr
       else if isScalarPrimCore core
              then CatScalarPrim
              else case MT.uncons core of
                Just ('a', rest) -> CatList rest
                _                -> CatOtherCompound

-- ── Validation ────────────────────────────────────────────────────

-- | Validate the docstring fields on a positional or optional arg
-- against its wire-schema text. Returns the validated 'ArgShape' or
-- a one-sentence error describing the rejected configuration.
--
-- `literal: true` is silently treated as the implicit default
-- (source: inline on Str, ignored elsewhere); the field is preserved
-- in 'ArgDocVars' only for backward-compatible parsing.
validateArgShape :: Text -> Bool -> ArgDocVars -> Either Text ArgShape
validateArgShape schema many docs
  | many = validateMany schema docs
  | otherwise = case classifySchema schema of
      CatScalarPrim    -> validateScalarPrim schema docs
      CatStr           -> validateStr docs
      CatList elemS    -> validateList schema elemS docs
      CatOtherCompound -> validateOtherCompound schema docs

validateMany :: Text -> ArgDocVars -> Either Text ArgShape
validateMany schema docs = do
  if not (isArrayWireSchema schema)
    then Left $ "`many: true` requires a list-typed argument (wire schema"
              <> " starting with `a`); got `" <> schema <> "`."
    else do
      let banIf b name = if b then Left (name <> " is not allowed with `many: true`.") else Right ()
      banIf (docSource docs /= Nothing)      "`source:`"
      banIf (docForm docs /= Nothing)        "`form:`"
      banIf (not (null (docChecks docs)))    "`check.*`"
      banIf (docListSource docs /= Nothing)  "`list.source:`"
      banIf (docListForm docs /= Nothing)    "`list.form:`"
      banIf (not (null (docListChecks docs))) "`list.check.*`"
      Right AsListMany

-- | Non-Str scalars accept no shape modifiers.
validateScalarPrim :: Text -> ArgDocVars -> Either Text ArgShape
validateScalarPrim schema docs = do
  let banIf b name = if b then Left
        ("`" <> name <> "` is not allowed on a non-`Str` scalar primitive"
         <> " (wire schema `" <> schema <> "`).") else Right ()
  banIf (docSource docs /= Nothing)       "source:"
  banIf (docForm docs /= Nothing)         "form:"
  banIf (not (null (docChecks docs)))     "check.*"
  banIf (docListSource docs /= Nothing)   "list.source:"
  banIf (docListForm docs /= Nothing)     "list.form:"
  banIf (not (null (docListChecks docs))) "list.check.*"
  Right AsScalarPrim

-- | Str: default inline, or `source: file`, or `check.path: <perm>`.
-- The three are mutually exclusive.
validateStr :: ArgDocVars -> Either Text ArgShape
validateStr docs = do
  when' (docForm docs /= Nothing) $
    "`form:` is not allowed on `Str`. With `source: file` the runtime"
    <> " sniffs packet magic and falls back to raw UTF-8 text; UTF-8's"
    <> " structural constraints make the sniff collision-free."
  when' (docListSource docs /= Nothing) $
    "`list.source:` is not allowed on a `Str` argument."
  when' (docListForm docs /= Nothing) $
    "`list.form:` is not allowed on a `Str` argument."
  when' (not (null (docListChecks docs))) $
    "`list.check.*` is not allowed on a `Str` argument."
  case (docSource docs, docChecks docs) of
    (Nothing,           [])             -> Right (AsStr StrDefault)
    (Just SourceInline, [])             -> Right (AsStr StrDefault)
    (Just SourceFile,   [])             -> Right (AsStr StrFile)
    (Nothing,           [CheckPath p])  -> Right (AsStr (StrCheckPath p))
    (Just SourceInline, [CheckPath p])  -> Right (AsStr (StrCheckPath p))
    (Just SourceFile,   [CheckPath _])  -> Left
      "`check.path:` requires the argv to be the literal path; it cannot be combined with `source: file`."
    (_, (_:_:_))                        -> Left
      "multiple `check.*` fields on one argument are not supported in v1."

-- | List arguments: a JSON / file default, a bytes-family form, the
-- ASCII inline shortcut on `[UInt8]`, or `form: list`.
validateList :: Text -> Text -> ArgDocVars -> Either Text ArgShape
validateList schema elemS docs = do
  when' (not (null (docChecks docs))) $
    "`check.path:` requires a `Str` argument; got wire schema `" <> schema <> "`."
  case (docSource docs, docForm docs) of
    (Nothing, Nothing) ->
      if hasAnyListField docs
        then Left "`list.*` fields are only meaningful when `form: list` is set."
        else Right AsListDefault
    (Just SourceInline, Nothing) ->
      if hasAnyListField docs
        then Left "`list.*` fields are only meaningful when `form: list` is set."
        else Right AsListDefault
    (Just SourceInline, Just FormList) -> Left
      "`source: inline` is contradictory with `form: list`; splitting a single argv string on newlines is not meaningful. Use `source: file` (or omit `source:`)."
    (_, Just FormList) ->
      AsListLines <$> validateLinesPipeline elemS docs
    (mSrc, Just FormBytes)     -> bytesArm schema elemS mSrc Bytes docs
    (mSrc, Just FormBytesOnly) -> bytesArm schema elemS mSrc BytesOnly docs
    (mSrc, Just FormPacket)    -> bytesArm schema elemS mSrc Packet docs
    (Just SourceFile, Nothing) -> Left $
      "`source: file` on a list argument requires an explicit `form:`"
      <> " (`form: list`, `form: bytes`, `form: bytes-only`, or `form: packet`)."

bytesArm
  :: Text -> Text -> Maybe SourceAtom -> BytesVariant -> ArgDocVars
  -> Either Text ArgShape
bytesArm schema elemS mSrc variant docs = do
  when' (hasAnyListField docs) $
    "`list.*` fields are only meaningful when `form: list` is set."
  case (mSrc, variant) of
    (Just SourceInline, Bytes)     ->
      requireUInt8 schema elemS *> Right (AsListInlineBytes IbBytes)
    (Just SourceInline, BytesOnly) ->
      requireUInt8 schema elemS *> Right (AsListInlineBytes IbBytesOnly)
    (Just SourceInline, Packet)    -> Left
      "`source: inline` is not compatible with `form: packet`; packets are always file-borne."
    (_, _) ->
      if isFixedWidthScalar elemS
        then Right (AsListBytes variant)
        else Left $
          "`form: " <> showVariant variant <> "` requires every list element"
          <> " to be a fixed-width scalar (Bool, sized Int/UInt, sized Float)."
          <> " Element wire schema `" <> elemS <> "` is not fixed-width."
  where
    requireUInt8 _ es =
      if es == "u1"
        then Right ()
        else Left $
          "`source: inline + form: " <> showVariant variant <> "` is only"
          <> " allowed on `[UInt8]`; got element wire schema `" <> es <> "`."

showVariant :: BytesVariant -> Text
showVariant Bytes      = "bytes"
showVariant BytesOnly  = "bytes-only"
showVariant Packet     = "packet"

-- | Validate the per-line pipeline under `form: list`. Returns the
-- 'ListPipeline' or a one-sentence error.
validateLinesPipeline :: Text -> ArgDocVars -> Either Text ListPipeline
validateLinesPipeline elemS docs =
  case (docListSource docs, docListForm docs, docListChecks docs) of
    (Nothing,           Nothing,            [])             -> defaultPipeline elemS docs
    (Just SourceInline, Nothing,            [])             -> defaultPipeline elemS docs
    (Nothing,           Just FormList,      _)              -> Left
      "`list.form: list` is not allowed -- nested list files are forbidden."
    (Just SourceInline, Just FormList,      _)              -> Left
      "`list.form: list` is not allowed -- nested list files are forbidden."
    (Just SourceFile,   Just FormList,      _)              -> Left
      "`list.form: list` is not allowed -- nested list files are forbidden."
    (Just SourceFile,   Nothing,            [])             -> Right (LpFile LfmAuto)
    (Just SourceFile,   Just FormPacket,    [])             -> Right (LpFile LfmPacket)
    (Just SourceFile,   Just FormBytes,     [])             -> bytesLineFile elemS IbBytes
    (Just SourceFile,   Just FormBytesOnly, [])             -> bytesLineFile elemS IbBytesOnly
    (Nothing,           Nothing,            [CheckPath p])  -> inlineCheck elemS p
    (Just SourceInline, Nothing,            [CheckPath p])  -> inlineCheck elemS p
    (_,                 Just FormPacket,    _)              -> Left
      "`list.form: packet` requires `list.source: file`."
    (_,                 Just FormBytes,     _)              -> Left
      "`list.form: bytes` requires `list.source: file` (or `list.source: inline` on inner `[UInt8]`)."
    (_,                 Just FormBytesOnly, _)              -> Left
      "`list.form: bytes-only` requires `list.source: file` (or `list.source: inline` on inner `[UInt8]`)."
    _ -> Left "unsupported `list.*` combination."
  where
    defaultPipeline _ _ = Right LpInlineBare
    inlineCheck es p
      | es == "s"  = Right (LpInlineCheck p)
      | otherwise  = Left $
          "`list.check.path:` requires a list-of-`Str` element type; got element wire schema `"
          <> es <> "`."
    bytesLineFile es variant
      | maybe (isFixedWidthScalar es) isFixedWidthScalar (listElementSchema es) = Right (LpFile (LfmBytes variant))
      | isFixedWidthScalar es = Right (LpFile (LfmBytes variant))
      | otherwise = Left $
          "`list.form: bytes`/`bytes-only` requires every leaf to be a fixed-width scalar; got element wire schema `"
          <> es <> "`."

-- | Tuples, records, maps: no shape modifiers.
validateOtherCompound :: Text -> ArgDocVars -> Either Text ArgShape
validateOtherCompound schema docs = do
  let banIf b name = if b then Left
        ("`" <> name <> "` is not allowed on this compound type (wire schema `"
         <> schema <> "`).") else Right ()
  banIf (docSource docs /= Nothing)       "source:"
  banIf (docForm docs /= Nothing)         "form:"
  banIf (not (null (docChecks docs)))     "check.*"
  banIf (docListSource docs /= Nothing)   "list.source:"
  banIf (docListForm docs /= Nothing)     "list.form:"
  banIf (not (null (docListChecks docs))) "list.check.*"
  Right AsOtherCompound

-- ── Internal helpers ──────────────────────────────────────────────

when' :: Bool -> Text -> Either Text ()
when' True  msg = Left msg
when' False _   = Right ()

hasAnyListField :: ArgDocVars -> Bool
hasAnyListField docs =
  docListSource docs /= Nothing
  || docListForm docs /= Nothing
  || not (null (docListChecks docs))

-- | Validate a single 'CmdArg' against its rendered wire-schema text.
-- Flag arguments have no shape (they are pure booleans); group
-- arguments delegate to their entries' shapes at emit time.
validateCmdArg :: Text -> CmdArg -> Either Text (Maybe ArgShape)
validateCmdArg schema (CmdArgPos r) =
  Just <$> validateArgShape schema (argPosDocMany r) (argDocVarsFromPos r)
validateCmdArg schema (CmdArgOpt r) =
  Just <$> validateArgShape schema (argOptDocMany r) (argDocVarsFromOpt r)
validateCmdArg _ (CmdArgFlag _) = Right Nothing
validateCmdArg _ (CmdArgGrp _)  = Right Nothing

-- | Project the shape-relevant docstring fields out of an
-- 'ArgPosDocSet'. The other fields (metavar, description, etc.) are
-- consulted at JSON-emit time but are irrelevant to shape validation.
argDocVarsFromPos :: ArgPosDocSet -> ArgDocVars
argDocVarsFromPos r = defaultValue
  { docLiteral      = argPosDocLiteral r
  , docSource       = argPosDocSource r
  , docForm         = argPosDocForm r
  , docChecks       = argPosDocChecks r
  , docListSource   = argPosDocListSource r
  , docListForm     = argPosDocListForm r
  , docListChecks   = argPosDocListChecks r
  }

argDocVarsFromOpt :: ArgOptDocSet -> ArgDocVars
argDocVarsFromOpt r = defaultValue
  { docLiteral      = argOptDocLiteral r
  , docSource       = argOptDocSource r
  , docForm         = argOptDocForm r
  , docChecks       = argOptDocChecks r
  , docListSource   = argOptDocListSource r
  , docListForm     = argOptDocListForm r
  , docListChecks   = argOptDocListChecks r
  }
