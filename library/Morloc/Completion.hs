{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Completion
Description : Generate shell completion scripts for morloc and installed programs
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc.Completion
  ( regenerateCompletions
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson ((.:), (.:?), (.!=))
import qualified Data.Aeson as JSON
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum)
import Data.List (isSuffixOf, intercalate, nub)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath ((</>), dropExtension, takeFileName)
import System.IO (hPutStrLn, stderr)

-- Lightweight manifest types for completion generation

data ManifestInfo = ManifestInfo
  { miName     :: Text
  , miCommands :: [CmdInfo]
  , miGroups   :: [GroupInfo]
  }

data CmdInfo = CmdInfo
  { ciName  :: Text
  , ciGroup :: Maybe Text
  , ciArgs  :: [ArgInfo]
  }

data ArgInfo
  = PosArg
  | OptArg { oaShort :: Maybe Char, oaLong :: Maybe Text }
  | FlagArg { faShort :: Maybe Char, faLong :: Maybe Text, faLongRev :: Maybe Text }
  | GrpArg { gaGroupOpt :: Maybe (Maybe Char, Maybe Text), gaEntries :: [ArgInfo] }

data GroupInfo = GroupInfo
  { giName :: Text
  }

-- JSON parsing instances

instance JSON.FromJSON ManifestInfo where
  parseJSON = JSON.withObject "ManifestInfo" $ \o ->
    ManifestInfo
      <$> (o .:? "name" .!= "")
      <*> (o .:? "commands" .!= [])
      <*> (o .:? "groups" .!= [])

instance JSON.FromJSON CmdInfo where
  parseJSON = JSON.withObject "CmdInfo" $ \o ->
    CmdInfo
      <$> o .: "name"
      <*> o .:? "group"
      <*> (o .:? "args" .!= [])

instance JSON.FromJSON ArgInfo where
  parseJSON = JSON.withObject "ArgInfo" $ \o -> do
    kind <- o .: "kind" :: Parser Text
    case kind of
      "pos" -> return PosArg
      "opt" -> do
        s <- o .:? "short"
        l <- o .:? "long"
        return $ OptArg (fmap charFromText s) l
      "flag" -> do
        s <- o .:? "short"
        l <- o .:? "long"
        lr <- o .:? "long_rev"
        return $ FlagArg (fmap charFromText s) l lr
      "grp" -> do
        gopt <- o .:? "group_opt"
        entries <- o .:? "entries" .!= []
        parsedOpt <- traverse parseGroupOpt gopt
        parsedEntries <- mapM parseEntry entries
        return $ GrpArg parsedOpt parsedEntries
      _ -> return PosArg
    where
      charFromText :: Text -> Char
      charFromText t = case T.unpack t of
        [c] -> c
        _ -> '?'

      parseGroupOpt :: JSON.Value -> Parser (Maybe Char, Maybe Text)
      parseGroupOpt = JSON.withObject "GroupOpt" $ \o' -> do
        s <- o' .:? "short"
        l <- o' .:? "long"
        return (fmap charFromText s, l)

      parseEntry :: JSON.Value -> Parser ArgInfo
      parseEntry = JSON.withObject "Entry" $ \o -> do
        arg <- o .: "arg"
        JSON.parseJSON arg

instance JSON.FromJSON GroupInfo where
  parseJSON = JSON.withObject "GroupInfo" $ \o ->
    GroupInfo <$> o .: "name"

-- | Read all manifests and regenerate completion files.
-- The Bool parameter controls whether to print activation instructions.
regenerateCompletions :: Bool -> String -> IO ()
regenerateCompletions printInstructions configHome = do
  let fdbDir = configHome </> "fdb"
      compDir = configHome </> "completions"
  createDirectoryIfMissing True compDir

  manifests <- loadManifests fdbDir
  let bashScript = generateBash manifests
      zshScript = generateZsh manifests
      bashPath = compDir </> "morloc-completions.bash"
      zshPath = compDir </> "_morloc_completions"
  writeFile bashPath bashScript
  writeFile zshPath zshScript

  if printInstructions
    then do
      putStrLn $ "Shell completions written to " ++ compDir ++ "/"
      putStrLn $ "  Bash: add to ~/.bashrc:  source " ++ bashPath
      putStrLn $ "  Zsh:  add to ~/.zshrc:   source " ++ zshPath
    else return ()

-- | Load all .manifest files from the fdb directory
loadManifests :: FilePath -> IO [ManifestInfo]
loadManifests fdbDir = do
  result <- try (listDirectory fdbDir) :: IO (Either SomeException [FilePath])
  case result of
    Left _ -> return []
    Right entries -> do
      let manifestFiles = filter (".manifest" `isSuffixOf`) entries
      catMaybes <$> mapM loadOne manifestFiles
  where
    -- Derive program name from filename (e.g. "pricer.manifest" -> "pricer")
    nameFromFile f = T.pack (dropExtension (takeFileName f))

    loadOne f = do
      r <- try (BL.readFile (fdbDir </> f)) :: IO (Either SomeException BL.ByteString)
      case r of
        Left _ -> return Nothing
        Right bs -> case JSON.eitherDecode bs of
          Right m ->
            let m' = if T.null (miName m) then m { miName = nameFromFile f } else m
            in return (Just m')
          Left err -> do
            hPutStrLn stderr $ "Warning: failed to parse " ++ f ++ ": " ++ err
            return Nothing

-- | Collect all flag/opt strings for a command
argCompletionWords :: [ArgInfo] -> [String]
argCompletionWords = concatMap argWords
  where
    argWords PosArg = []
    argWords (OptArg s l) = shortWord s ++ longWord l
    argWords (FlagArg s l lr) = shortWord s ++ longWord l ++ longWord lr
    argWords (GrpArg gopt entries) =
      maybe [] (\(s, l) -> shortWord s ++ longWord l) gopt
      ++ concatMap argWords entries

    shortWord (Just c) = ['-':[c]]
    shortWord Nothing = []
    longWord (Just t) = ["--" ++ T.unpack t]
    longWord Nothing = []

-- | Get flags/opts that take a value (opts and grp opts, but not flags)
valueArgs :: [ArgInfo] -> [String]
valueArgs = concatMap go
  where
    go PosArg = []
    go (OptArg s l) = shortWord s ++ longWord l
    go FlagArg{} = []
    go (GrpArg gopt entries) =
      maybe [] (\(gs, gl) -> shortWord gs ++ longWord gl) gopt
      ++ concatMap go entries

    shortWord (Just c) = ['-':[c]]
    shortWord Nothing = []
    longWord (Just t) = ["--" ++ T.unpack t]
    longWord Nothing = []

-- | Sanitize a name for use as a bash function name
sanitizeName :: Text -> String
sanitizeName = map (\c -> if isAlphaNum c then c else '_') . T.unpack

-- Bash generation

generateBash :: [ManifestInfo] -> String
generateBash manifests = unlines $
  [ "# Auto-generated by morloc -- do not edit manually"
  , ""
  ] ++ morlocBashCompletion
    ++ concatMap programBashCompletion manifests

morlocBashCompletion :: [String]
morlocBashCompletion =
  [ "_morloc() {"
  , "  local cur prev"
  , "  COMPREPLY=()"
  , "  cur=\"${COMP_WORDS[COMP_CWORD]}\""
  , "  prev=\"${COMP_WORDS[COMP_CWORD-1]}\""
  , ""
  , "  if [[ $COMP_CWORD -eq 1 ]]; then"
  , "    COMPREPLY=($(compgen -W \"make install typecheck dump init\" -- \"$cur\"))"
  , "    return"
  , "  fi"
  , ""
  , "  case \"${COMP_WORDS[1]}\" in"
  , "    make)"
  , "      case \"$prev\" in"
  , "        -o|--outfile|--config|--include) return ;;"
  , "      esac"
  , "      COMPREPLY=($(compgen -W \"-e --expression -o --outfile --install -f --force --include --config -v --vanilla\" -- \"$cur\"))"
  , "      ;;"
  , "    install)"
  , "      case \"$prev\" in"
  , "        --config) return ;;"
  , "      esac"
  , "      COMPREPLY=($(compgen -W \"--config --vanilla -v -f --force --ssh\" -- \"$cur\"))"
  , "      ;;"
  , "    typecheck)"
  , "      case \"$prev\" in"
  , "        --config) return ;;"
  , "      esac"
  , "      COMPREPLY=($(compgen -W \"--config --vanilla -t --type --raw -e --expression -v -r --realize\" -- \"$cur\"))"
  , "      ;;"
  , "    dump)"
  , "      case \"$prev\" in"
  , "        --config) return ;;"
  , "      esac"
  , "      COMPREPLY=($(compgen -W \"--config --vanilla -v -e --expression\" -- \"$cur\"))"
  , "      ;;"
  , "    init)"
  , "      case \"$prev\" in"
  , "        --config) return ;;"
  , "      esac"
  , "      COMPREPLY=($(compgen -W \"--config -q --quiet --vanilla -f --force --slurm\" -- \"$cur\"))"
  , "      ;;"
  , "  esac"
  , "}"
  , "complete -F _morloc morloc"
  , ""
  ]

programBashCompletion :: ManifestInfo -> [String]
programBashCompletion mi =
  let name = miName mi
      safeName = sanitizeName name
      funcName = "_morloc_prog_" ++ safeName
      groups = miGroups mi
      groupNames = map (T.unpack . giName) groups
      cmds = miCommands mi
      ungroupedCmds = [c | c <- cmds, ciGroup c == Nothing]
      ungroupedNames = map (T.unpack . ciName) ungroupedCmds
      firstLevelWords = nub (ungroupedNames ++ groupNames)
      groupedCmds grp = [c | c <- cmds, ciGroup c == Just grp]
  in if T.null name then [] else
  [ "# --- Installed program: " ++ T.unpack name ++ " ---"
  , funcName ++ "() {"
  , "  local cur prev"
  , "  COMPREPLY=()"
  , "  cur=\"${COMP_WORDS[COMP_CWORD]}\""
  , "  prev=\"${COMP_WORDS[COMP_CWORD-1]}\""
  , ""
  , "  if [[ $COMP_CWORD -eq 1 ]]; then"
  , "    COMPREPLY=($(compgen -W \"" ++ unwords firstLevelWords ++ "\" -- \"$cur\"))"
  , "    return"
  , "  fi"
  , ""
  , "  case \"${COMP_WORDS[1]}\" in"
  ]
  -- group cases
  ++ concatMap (\grp ->
      let grpCmds = groupedCmds (giName grp)
          grpCmdNames = map (T.unpack . ciName) grpCmds
      in [ "    " ++ T.unpack (giName grp) ++ ")"
         , "      if [[ $COMP_CWORD -eq 2 ]]; then"
         , "        COMPREPLY=($(compgen -W \"" ++ unwords grpCmdNames ++ "\" -- \"$cur\"))"
         , "        return"
         , "      fi"
         ]
         ++ bashCommandFlagCases 3 grpCmds
         ++ [ "      ;;" ]
    ) groups
  -- ungrouped command cases
  ++ concatMap (bashSingleCommandCase 1) ungroupedCmds
  ++
  [ "  esac"
  , "}"
  , "complete -F " ++ funcName ++ " " ++ T.unpack name
  , ""
  ]

-- | Generate flag completion for a direct command at the given COMP_CWORD depth
bashSingleCommandCase :: Int -> CmdInfo -> [String]
bashSingleCommandCase _depth cmd =
  let words' = argCompletionWords (ciArgs cmd)
      valArgs = valueArgs (ciArgs cmd)
  in if null words' then
    [ "    " ++ T.unpack (ciName cmd) ++ ")"
    , "      ;;"
    ]
  else
    [ "    " ++ T.unpack (ciName cmd) ++ ")"
    ]
    ++ (if null valArgs then [] else
      [ "      case \"$prev\" in"
      , "        " ++ intercalate "|" valArgs ++ ") return ;;"
      , "      esac"
      ])
    ++
    [ "      COMPREPLY=($(compgen -W \"" ++ unwords words' ++ "\" -- \"$cur\"))"
    , "      ;;"
    ]

-- | Generate command dispatch within a group (at COMP_CWORD == depth for command name)
bashCommandFlagCases :: Int -> [CmdInfo] -> [String]
bashCommandFlagCases depth cmds =
  if null cmds then [] else
  [ "      case \"${COMP_WORDS[" ++ show (depth - 1) ++ "]}\" in"
  ]
  ++ concatMap (\cmd ->
      let words' = argCompletionWords (ciArgs cmd)
          valArgs = valueArgs (ciArgs cmd)
      in if null words' then [] else
        [ "        " ++ T.unpack (ciName cmd) ++ ")"
        ]
        ++ (if null valArgs then [] else
          [ "          case \"$prev\" in"
          , "            " ++ intercalate "|" valArgs ++ ") return ;;"
          , "          esac"
          ])
        ++
        [ "          COMPREPLY=($(compgen -W \"" ++ unwords words' ++ "\" -- \"$cur\"))"
        , "          ;;"
        ]
    ) cmds
  ++
  [ "      esac"
  ]

-- Zsh generation

generateZsh :: [ManifestInfo] -> String
generateZsh manifests = unlines $
  [ "#compdef morloc"
  , "# Auto-generated by morloc -- do not edit manually"
  , ""
  ] ++ morlocZshCompletion
    ++ concatMap programZshCompletion manifests

morlocZshCompletion :: [String]
morlocZshCompletion =
  [ "_morloc() {"
  , "  local -a subcmds"
  , "  subcmds=("
  , "    'make:Build a morloc script'"
  , "    'install:Install a morloc module'"
  , "    'typecheck:Typecheck a morloc program'"
  , "    'dump:Dump parsed code'"
  , "    'init:Initialize morloc environment'"
  , "  )"
  , ""
  , "  if (( CURRENT == 2 )); then"
  , "    _describe 'subcommand' subcmds"
  , "    return"
  , "  fi"
  , ""
  , "  case \"$words[2]\" in"
  , "    make)"
  , "      _arguments \\"
  , "        '-e[Read as expression]' \\"
  , "        '--expression[Read as expression]' \\"
  , "        '-o[Output file]:outfile:_files' \\"
  , "        '--outfile[Output file]:outfile:_files' \\"
  , "        '--install[Install to PATH]' \\"
  , "        '-f[Force overwrite]' \\"
  , "        '--force[Force overwrite]' \\"
  , "        '--include[Include pattern]:pattern:' \\"
  , "        '--config[Config file]:config:_files' \\"
  , "        '-v[Verbose]' \\"
  , "        '--vanilla[Ignore local config]' \\"
  , "        '*:script:_files -g \"*.loc\"'"
  , "      ;;"
  , "    install)"
  , "      _arguments \\"
  , "        '--config[Config file]:config:_files' \\"
  , "        '--vanilla[Ignore local config]' \\"
  , "        '-v[Verbose]' \\"
  , "        '-f[Force overwrite]' \\"
  , "        '--force[Force overwrite]' \\"
  , "        '--ssh[Use SSH protocol]'"
  , "      ;;"
  , "    typecheck)"
  , "      _arguments \\"
  , "        '--config[Config file]:config:_files' \\"
  , "        '--vanilla[Ignore local config]' \\"
  , "        '-t[Parse type string]' \\"
  , "        '--type[Parse type string]' \\"
  , "        '--raw[Print raw objects]' \\"
  , "        '-e[Read as expression]' \\"
  , "        '--expression[Read as expression]' \\"
  , "        '-v[Verbose]' \\"
  , "        '-r[Typecheck realizations]' \\"
  , "        '--realize[Typecheck realizations]' \\"
  , "        '*:script:_files -g \"*.loc\"'"
  , "      ;;"
  , "    dump)"
  , "      _arguments \\"
  , "        '--config[Config file]:config:_files' \\"
  , "        '--vanilla[Ignore local config]' \\"
  , "        '-v[Verbose]' \\"
  , "        '-e[Read as expression]' \\"
  , "        '--expression[Read as expression]' \\"
  , "        '*:script:_files -g \"*.loc\"'"
  , "      ;;"
  , "    init)"
  , "      _arguments \\"
  , "        '--config[Config file]:config:_files' \\"
  , "        '-q[Minimal output]' \\"
  , "        '--quiet[Minimal output]' \\"
  , "        '--vanilla[Ignore local config]' \\"
  , "        '-f[Force overwrite]' \\"
  , "        '--force[Force overwrite]' \\"
  , "        '--slurm[Enable SLURM support]'"
  , "      ;;"
  , "  esac"
  , "}"
  , "compdef _morloc morloc"
  , ""
  ]

programZshCompletion :: ManifestInfo -> [String]
programZshCompletion mi =
  let name = miName mi
      safeName = sanitizeName name
      funcName = "_morloc_prog_" ++ safeName
      groups = miGroups mi
      cmds = miCommands mi
      ungroupedCmds = [c | c <- cmds, ciGroup c == Nothing]
      groupedCmds grp = [c | c <- cmds, ciGroup c == Just grp]
      -- Build first-level descriptions
      ungroupedDescs = map (\c -> "    '" ++ T.unpack (ciName c) ++ ":" ++ T.unpack (ciName c) ++ "'") ungroupedCmds
      groupDescs = map (\g -> "    '" ++ T.unpack (giName g) ++ ":" ++ T.unpack (giName g) ++ "'") groups
      allDescs = ungroupedDescs ++ groupDescs
  in if T.null name then [] else
  [ "# --- Installed program: " ++ T.unpack name ++ " ---"
  , funcName ++ "() {"
  , "  local -a cmds"
  , "  cmds=("
  ]
  ++ allDescs ++
  [ "  )"
  , ""
  , "  if (( CURRENT == 2 )); then"
  , "    _describe 'command' cmds"
  , "    return"
  , "  fi"
  , ""
  , "  case \"$words[2]\" in"
  ]
  -- group cases
  ++ concatMap (\grp ->
      let grpCmds = groupedCmds (giName grp)
          grpDescs = map (\c -> "        '" ++ T.unpack (ciName c) ++ ":" ++ T.unpack (ciName c) ++ "'") grpCmds
      in [ "    " ++ T.unpack (giName grp) ++ ")"
         , "      if (( CURRENT == 3 )); then"
         , "        local -a grpcmds=("
         ]
         ++ grpDescs ++
         [ "        )"
         , "        _describe 'command' grpcmds"
         , "        return"
         , "      fi"
         ]
         ++ zshCommandFlagCases 3 grpCmds ++
         [ "      ;;"
         ]
    ) groups
  -- ungrouped command cases
  ++ concatMap zshSingleCommandCase ungroupedCmds
  ++
  [ "  esac"
  , "}"
  , "compdef " ++ funcName ++ " " ++ T.unpack name
  , ""
  ]

-- | Generate zsh _arguments for a single command
zshSingleCommandCase :: CmdInfo -> [String]
zshSingleCommandCase cmd =
  let argSpecs = zshArgSpecs (ciArgs cmd)
  in if null argSpecs then
    [ "    " ++ T.unpack (ciName cmd) ++ ")"
    , "      ;;"
    ]
  else
    [ "    " ++ T.unpack (ciName cmd) ++ ")"
    , "      _arguments \\"
    ]
    ++ formatZshArgs argSpecs ++
    [ "      ;;"
    ]

-- | Generate dispatch within a group for zsh
zshCommandFlagCases :: Int -> [CmdInfo] -> [String]
zshCommandFlagCases depth cmds =
  if null cmds then [] else
  [ "      case \"$words[" ++ show depth ++ "]\" in"
  ]
  ++ concatMap (\cmd ->
      let argSpecs = zshArgSpecs (ciArgs cmd)
      in if null argSpecs then [] else
        [ "        " ++ T.unpack (ciName cmd) ++ ")"
        , "          _arguments \\"
        ]
        ++ map ("    " ++) (formatZshArgs argSpecs) ++
        [ "          ;;"
        ]
    ) cmds
  ++
  [ "      esac"
  ]

-- | Convert args to zsh _arguments spec strings
zshArgSpecs :: [ArgInfo] -> [String]
zshArgSpecs = concatMap go
  where
    go PosArg = []
    go (OptArg s l) =
      maybe [] (\c -> ["'-" ++ [c] ++ "[Option]:value:'"]) s
      ++ maybe [] (\t -> ["'--" ++ T.unpack t ++ "[Option]:value:'"]) l
    go (FlagArg s l lr) =
      maybe [] (\c -> ["'-" ++ [c] ++ "[Flag]'"]) s
      ++ maybe [] (\t -> ["'--" ++ T.unpack t ++ "[Flag]'"]) l
      ++ maybe [] (\t -> ["'--" ++ T.unpack t ++ "[Flag]'"]) lr
    go (GrpArg gopt entries) =
      maybe [] (\(s, l) ->
        maybe [] (\c -> ["'-" ++ [c] ++ "[Group option]:value:'"]) s
        ++ maybe [] (\t -> ["'--" ++ T.unpack t ++ "[Group option]:value:'"]) l
      ) gopt
      ++ concatMap go entries

-- | Format zsh args with proper line continuation
formatZshArgs :: [String] -> [String]
formatZshArgs [] = []
formatZshArgs [x] = ["        " ++ x]
formatZshArgs (x:xs) = ("        " ++ x ++ " \\") : formatZshArgs xs
