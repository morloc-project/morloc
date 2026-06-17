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
module Morloc.CodeGenerator.Docstrings (processDocstrings, argLocPrefix) where

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
    [ ("literal", isJust (docLiteral r))
    , ("many",    isJust (docMany r))
    , ("unroll",  isJust (docUnroll r))
    , ("default", isJust (docDefault r))
    , ("metavar", isJust (docMetavar r))
    , ("arg",     isJust (docArg r))
    , ("true",    isJust (docTrue r))
    , ("false",   isJust (docFalse r))
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
