{- |
Module      : UI
Description : CLI argument parsing with optparse-applicative
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Defines the command-line interface for the @morloc@ executable using
optparse-applicative: subcommands (make, typecheck, install, init, dump,
completion), their options, and help text.
-}
module UI
  ( opts
  , CliCommand (..)
  , MakeCommand (..)
  , InitCommand (..)
  , InstallCommand (..)
  , TypecheckCommand (..)
  , DumpCommand (..)
  , ListCommand (..)
  , ListKind (..)
  , UninstallCommand (..)
  , NewCommand (..)
  , EvalCommand (..)
  , ConfigCommand (..)
  , ConfigAction (..)
  ) where

import Data.Int (Int64)
import Morloc.Data.Size (parseHumanSize)
import Morloc.Module (GitProtocol (..), OverwriteProtocol (..))
import Morloc.Version (versionStr)
import Options.Applicative
import qualified Options.Applicative.Extra as OAE
import Options.Applicative.Help.Pretty (Doc, pretty, vcat)

opts :: ParserInfo CliCommand
opts =
  info
    (cliParser <**> helper <**> OAE.simpleVersioner versionStr)
    ( fullDesc
        <> progDesc "Call 'morloc make -h', 'morloc install -h', etc for details"
        <> header ("morloc v" <> versionStr)
    )

data CliCommand
  = CmdMake MakeCommand
  | CmdInstall InstallCommand
  | CmdUninstall UninstallCommand
  | CmdList ListCommand
  | CmdTypecheck TypecheckCommand
  | CmdDump DumpCommand
  | CmdInit InitCommand
  | CmdNew NewCommand
  | CmdEval EvalCommand
  | CmdConfig ConfigCommand

cliParser :: Parser CliCommand
cliParser =
  hsubparser
    ( makeSubcommand
        <> installSubcommand
        <> uninstallSubcommand
        <> listSubcommand
        <> typecheckSubcommand
        <> dumpSubcommand
        <> initSubcommand
        <> newSubcommand
        <> evalSubcommand
        <> configSubcommand
    )

data MakeCommand = MakeCommand
  { makeExpression :: Bool
  , makeConfig :: String
  , makeVerbose :: Int
  , makeVanilla :: Bool
  , makeCliOut :: String
  , makeMcpOut :: String
  , makeDaemonOut :: String
  , makeNoCli :: Bool
  , makeBuildDir :: Maybe String
  , makeName :: Maybe String
  , makeInstall :: Bool
  , makeForce :: Bool
  , makeInclude :: [String]
  , makeUnsafeSkipNullCheck :: Bool
  , makeInlineSize :: Maybe Int64
  , makeNoShm :: Bool
  , makeTmpdir :: Maybe String
  , makeDebugTrace :: Bool
  , makeLangParams :: [String]
  , makeScript :: String
  }

makeCommandParser :: Parser MakeCommand
makeCommandParser =
  MakeCommand
    <$> optExpression
    <*> optConfig
    <*> optVerbose
    <*> optVanilla
    <*> optCliOut
    <*> optMcpOut
    <*> optDaemonOut
    <*> optNoCli
    <*> optBuildDir
    <*> optName
    <*> optMakeInstall
    <*> optMakeForce
    <*> optMakeInclude
    <*> optUnsafeSkipNullCheck
    <*> optInlineSize
    <*> optNoShm
    <*> optTmpdir
    <*> optDebugTrace
    <*> optLangParams
    <*> optScript

makeSubcommand :: Mod CommandFields CliCommand
makeSubcommand = command "make" (info (CmdMake <$> makeCommandParser) (progDesc "Build a morloc script"))

data InitCommand = InitCommand
  { initConfig :: String
  , initQuiet :: Bool
  , initVanilla :: Bool
  , initForce :: OverwriteProtocol
  , initSlurmSupport :: Bool
  , initSanitize :: Bool
  }

initCommandParser :: Parser InitCommand
initCommandParser =
  InitCommand
    <$> optConfig
    <*> optQuiet
    <*> optVanilla
    <*> optForce
    <*> optSlurmSupport
    <*> optSanitize

initSubcommand :: Mod CommandFields CliCommand
initSubcommand = command "init" (info (CmdInit <$> initCommandParser) (progDesc "Initialize morloc environment"))

data ConfigCommand = ConfigCommand
  { configCmdConfig :: String
  , configCmdVanilla :: Bool
  , configCmdAction :: ConfigAction
  }

-- | An action over the per-machine build config's @lang-params@ table.
data ConfigAction
  = ConfigSet [String] -- ^ LANG:KEY=VALUE entries to set
  | ConfigUnset [String] -- ^ LANG:KEY entries to remove
  | ConfigList -- ^ print the current lang-params

configCommandParser :: Parser ConfigCommand
configCommandParser =
  ConfigCommand
    <$> optConfig
    <*> optVanilla
    <*> configActionParser

configActionParser :: Parser ConfigAction
configActionParser =
  hsubparser
    ( command
        "set"
        ( info
            (ConfigSet <$> some (strArgument (metavar "LANG:KEY=VALUE")))
            (progDesc "Set per-machine build parameter defaults")
        )
        <> command
          "unset"
          ( info
              (ConfigUnset <$> some (strArgument (metavar "LANG:KEY")))
              (progDesc "Remove per-machine build parameter defaults")
          )
        <> command
          "list"
          ( info
              (pure ConfigList)
              (progDesc "List per-machine build parameter defaults")
          )
    )

configSubcommand :: Mod CommandFields CliCommand
configSubcommand = command "config" (info (CmdConfig <$> configCommandParser) (progDesc "Manage the per-machine build configuration"))

data NewCommand = NewCommand
  { newName :: String
  }

newCommandParser :: Parser NewCommand
newCommandParser =
  NewCommand
    <$> strArgument
      ( metavar "NAME"
          <> value ""
          <> help "Package name (defaults to current directory name)"
      )

newSubcommand :: Mod CommandFields CliCommand
newSubcommand = command "new" (info (CmdNew <$> newCommandParser) (progDesc "Create a new morloc package"))

data InstallCommand = InstallCommand
  { installForce :: OverwriteProtocol
  , installBuild :: Bool
  , installNoTypecheck :: Bool
  , installUseSSH :: GitProtocol
  , installConfig :: String
  , installVanilla :: Bool
  , installVerbose :: Int
  , installModuleStrings :: [String]
  }

makeInstallParser :: Parser InstallCommand
makeInstallParser =
  InstallCommand
    <$> optForce
    <*> optInstallBuild
    <*> optNoTypecheck
    <*> optUseSSH
    <*> optConfig
    <*> optVanilla
    <*> optVerbose
    <*> optModuleStrings

installSubcommand :: Mod CommandFields CliCommand
installSubcommand =
  command "install" $
    info
      (CmdInstall <$> makeInstallParser)
      (progDescDoc (Just installDescDoc))

installDescDoc :: Doc
installDescDoc = vcat $ map pretty
  [ "Install one or more morloc modules"
  , ""
  , "Examples:"
  , "  # Remote install"
  , "  morloc install root root-py math   # one or more stdlib modules"
  , "  morloc install weena/calendar      # owner/repo on github"
  , "  morloc install codeberg:weena/foo  # specify the remote"
  , "  morloc install weena/calendar@1.0  # specific release tag"
  , "  morloc install weena/calendar@dev  # specific branch"
  , "  morloc install gitlab:weena/foo@version:1.0"
  , "  morloc install github:weena/foo@hash:0123abc"
  , ""
  , "  # Local install"
  , "  morloc install .                   # install working dir"
  , "  morloc install ./mymod ~/code/foo  # install by path"
  , "  morloc install ./mymod@branch:dev  # specific local branch"
  , "  morloc install ./mymod@v0.2.0      # specific local tag"
  ]

data TypecheckCommand = TypecheckCommand
  { typecheckConfig :: String
  , typecheckVanilla :: Bool
  , typecheckType :: Bool
  , typecheckRaw :: Bool
  , typecheckExpression :: Bool
  , typecheckVerbose :: Int
  , typecheckRealize :: Bool
  , typecheckScript :: String
  }

makeTypecheckParser :: Parser TypecheckCommand
makeTypecheckParser =
  TypecheckCommand
    <$> optConfig
    <*> optVanilla
    <*> optType
    <*> optRaw
    <*> optExpression
    <*> optVerbose
    <*> optRealize
    <*> optScript

typecheckSubcommand :: Mod CommandFields CliCommand
typecheckSubcommand =
  command
    "typecheck"
    (info (CmdTypecheck <$> makeTypecheckParser) (progDesc "Typecheck a morloc program"))

dumpSubcommand :: Mod CommandFields CliCommand
dumpSubcommand =
  command "dump" (info (CmdDump <$> makeDumpParser) (progDesc "Dump parsed code"))

data DumpCommand = DumpCommand
  { dumpConfig :: String
  , dumpVanilla :: Bool
  , dumpVerbose :: Int
  , dumpExpression :: Bool
  , dumpScript :: String
  }

makeDumpParser :: Parser DumpCommand
makeDumpParser =
  DumpCommand
    <$> optConfig
    <*> optVanilla
    <*> optVerbose
    <*> optExpression
    <*> optScript

data ListKind = ListModules | ListPrograms
  deriving (Show, Eq)

data ListCommand = ListCommand
  { listPattern :: Maybe String
  , listConfig :: String
  , listVanilla :: Bool
  , listVerbose :: Int
  , listKind :: Maybe ListKind
  }

makeListParser :: Parser ListCommand
makeListParser =
  ListCommand
    <$> optListPattern
    <*> optConfig
    <*> optVanilla
    <*> optVerbose
    <*> optListKind

listSubcommand :: Mod CommandFields CliCommand
listSubcommand =
  command "list" (info (CmdList <$> makeListParser) (progDesc "List installed modules and programs"))

data UninstallCommand = UninstallCommand
  { uninstallNames :: [String]
  , uninstallConfig :: String
  , uninstallVanilla :: Bool
  , uninstallKind :: Maybe ListKind
  , uninstallDryRun :: Bool
  , uninstallAll :: Bool
  }

makeUninstallParser :: Parser UninstallCommand
makeUninstallParser =
  UninstallCommand
    <$> optUninstallNamesOrNone
    <*> optConfig
    <*> optVanilla
    <*> optUninstallKind
    <*> optDryRun
    <*> optUninstallAll

uninstallSubcommand :: Mod CommandFields CliCommand
uninstallSubcommand =
  command
    "uninstall"
    (info (CmdUninstall <$> makeUninstallParser) (progDesc "Uninstall a module or program"))

optExpression :: Parser Bool
optExpression =
  switch
    ( long "expression"
        <> short 'e'
        <> help "Read script as string rather than file"
    )

optAllowLocalModules :: Parser Bool
optAllowLocalModules =
  switch
    ( long "allow-local-modules"
        <> help "Permit eval to import local-filesystem modules (development only; insecure for server use -- prefer 'morloc make')"
    )

optVanilla :: Parser Bool
optVanilla =
  switch
    ( long "vanilla"
        <> help "Ignore local configuration files"
    )

optForce :: Parser OverwriteProtocol
optForce =
  flag
    DoNotOverwrite
    ForceOverwrite
    ( long "force"
        <> short 'f'
        <> help "Overwrite files if they already exist"
    )

optUseSSH :: Parser GitProtocol
optUseSSH =
  flag
    HttpsProtocol
    SshProtocol
    ( long "ssh"
        <> help "Use SSH protocol for remote git access"
    )

optNoTypecheck :: Parser Bool
optNoTypecheck =
  switch
    ( long "no-typecheck"
        <> help "Skip typechecking during install"
    )

optInstallBuild :: Parser Bool
optInstallBuild =
  switch
    ( long "build"
        <> short 'b'
        <> help "Build and install executable after module install"
    )

optModuleStrings :: Parser [String]
optModuleStrings =
  some -- one or more
    . strArgument
    $ ( metavar "INSTALL"
          <> help "One or more morloc module install strings"
      )

optRaw :: Parser Bool
optRaw =
  switch
    ( long "raw"
        <> help "Print raw objects"
    )

optSlurmSupport :: Parser Bool
optSlurmSupport =
  switch
    ( long "slurm"
        <> help "Allow use of SLURM for remote jobs"
    )

optSanitize :: Parser Bool
optSanitize =
  switch
    ( long "sanitize"
        <> help "Enable alignment sanitizer for debugging memory layout issues"
    )

optVerbose :: Parser Int
optVerbose = length <$> many (flag' () (short 'v'))

optQuiet :: Parser Bool
optQuiet =
  switch
    ( long "quiet"
        <> short 'q'
        <> help "Print minimal output to STDERR"
    )

optRealize :: Parser Bool
optRealize =
  switch
    ( long "realize"
        <> short 'r'
        <> help "Typecheck the composition realizations"
    )

optConfig :: Parser String
optConfig =
  strOption
    ( long "config"
        <> metavar "CONFIG"
        <> value ""
        <> help "Use this config rather than the one in morloc home"
    )

optCliOut :: Parser String
optCliOut =
  strOption
    ( long "cli-out"
        <> short 'o'
        <> metavar "OUT"
        <> value ""
        <> showDefault
        <> help "The name of the generated CLI executable (default: program name)"
    )

optMcpOut :: Parser String
optMcpOut =
  strOption
    ( long "mcp-out"
        <> metavar "OUT"
        <> value ""
        <> help "Also emit a wrapper that serves the program as an MCP server"
    )

optDaemonOut :: Parser String
optDaemonOut =
  strOption
    ( long "daemon-out"
        <> metavar "OUT"
        <> value ""
        <> help "Also emit a wrapper that runs the program as a daemon"
    )

optNoCli :: Parser Bool
optNoCli =
  switch
    ( long "no-cli"
        <> help "Suppress the default CLI executable"
    )

optBuildDir :: Parser (Maybe String)
optBuildDir =
  optional
    ( strOption
        ( long "build-dir"
            <> metavar "DIR"
            <> help "Parent directory in which the <name>-build/ folder is created (default: CWD)"
        )
    )

optName :: Parser (Maybe String)
optName =
  optional
    ( strOption
        ( long "name"
            <> metavar "NAME"
            <> help "Program identity for the build directory (required for -e expression builds)"
        )
    )

optMakeInstall :: Parser Bool
optMakeInstall =
  switch
    ( long "install"
        <> help "Install module to PATH"
    )

optMakeForce :: Parser Bool
optMakeForce =
  switch
    ( long "force"
        <> short 'f'
        <> help "Overwrite existing install"
    )

optMakeInclude :: Parser [String]
optMakeInclude =
  many
    ( strOption
        ( long "include"
            <> metavar "PATTERN"
            <> help "File pattern to include in install"
        )
    )

optLangParams :: Parser [String]
optLangParams =
  many
    ( strOption
        ( short 'X'
            <> long "lang-param"
            <> metavar "LANG:KEY=VALUE"
            <> help
                ( "Pass a build parameter to a language's builder, e.g. "
                    ++ "-X futhark:backend=cuda or -X cpp:flags=-march=native. "
                    ++ "Repeatable. The value is taken verbatim; morloc does not "
                    ++ "interpret it -- the language's builder does. The reserved "
                    ++ "key 'flags' accumulates in order as raw compiler flags."
                )
        )
    )

optDebugTrace :: Parser Bool
optDebugTrace =
  switch
    ( long "debug"
        <> help
            ( "Wrap every foreign-call manifold with an exception-path "
                ++ "trace recorder. On any thrown exception, each active "
                ++ "frame's args are content-addressed and dumped to "
                ++ "$MORLOC_DEBUG_DIR (or $MORLOC_RUN_DIR/debug if --log-dir "
                ++ "is set). Zero cost on the happy path; opt-in only for "
                ++ "diagnostic builds."
            )
    )

optUnsafeSkipNullCheck :: Parser Bool
optUnsafeSkipNullCheck =
  switch
    ( long "unsafe-skip-null-check"
        <> help
            ( "Disable the runtime scan that rejects Str values with "
                ++ "embedded NUL bytes at boundaries into languages that "
                ++ "cannot represent them (e.g. R). Unsafe: a NUL passing "
                ++ "into R may crash inside user code instead of being "
                ++ "diagnosed cleanly at the FFI boundary."
            )
    )

optInlineSize :: Parser (Maybe Int64)
optInlineSize =
  optional $ option (eitherReader parseHumanSize)
    ( long "inline-size"
        <> metavar "BYTES"
        <> help
            ( "Threshold (in bytes) under which data is inlined into "
                ++ "packets instead of routed through shared memory or "
                ++ "files. Accepts k/m/g suffixes (binary, 1024-based). "
                ++ "0 = never inline. Default: 64k."
            )
    )

optNoShm :: Parser Bool
optNoShm =
  switch
    ( long "no-shm"
        <> help
            ( "Disable shared memory. Data above the inline threshold "
                ++ "is written to a temp file and passed by path."
            )
    )

optTmpdir :: Parser (Maybe String)
optTmpdir =
  optional $ strOption
    ( long "tmpdir"
        <> metavar "PATH"
        <> help
            ( "Directory for data files emitted by --no-shm "
                ++ "(format `morloc-pkt-<pid>-<seq>.mpk`). When this "
                ++ "flag is set, files are NOT auto-deleted at end of "
                ++ "eval (useful for testing/inspection). Default "
                ++ "(unset): files go to $TMPDIR or /tmp and are "
                ++ "unlinked when the eval that produced them ends."
            )
    )

optScript :: Parser String
optScript = argument str (metavar "<script>" <> value "main.loc")

optType :: Parser Bool
optType =
  switch
    ( long "type"
        <> short 't'
        <> help "Parse a typestring instread of an expression"
    )

optListKind :: Parser (Maybe ListKind)
optListKind =
  flag' (Just ListModules) (long "modules" <> help "List only modules")
    <|> flag' (Just ListPrograms) (long "programs" <> help "List only programs")
    <|> pure Nothing

optListPattern :: Parser (Maybe String)
optListPattern =
  optional . strArgument $
    ( metavar "PATTERN"
        <> help "Filter by subsequence match on name"
    )

optUninstallNamesOrNone :: Parser [String]
optUninstallNamesOrNone =
  many
    . strArgument
    $ ( metavar "NAME"
          <> help "Names of modules or programs to uninstall"
      )

optUninstallAll :: Parser Bool
optUninstallAll =
  switch
    ( long "all"
        <> help "Uninstall all installed modules"
    )

optUninstallKind :: Parser (Maybe ListKind)
optUninstallKind =
  flag' (Just ListModules) (long "module" <> help "Uninstall only the module")
    <|> flag' (Just ListPrograms) (long "program" <> help "Uninstall only the program")
    <|> pure Nothing

optDryRun :: Parser Bool
optDryRun =
  switch
    ( long "dry-run"
        <> help "Show what would be removed without removing"
    )

data EvalCommand = EvalCommand
  { evalConfig :: String
  , evalVanilla :: Bool
  , evalVerbose :: Int
  , evalSave :: String
  , evalExpression :: Bool
  , evalAllowLocalModules :: Bool
  , evalScript :: String
  , evalArgs :: [String]
  }

evalCommandParser :: Parser EvalCommand
evalCommandParser =
  EvalCommand
    <$> optConfig
    <*> optVanilla
    <*> optVerbose
    <*> optSave
    <*> optExpression
    <*> optAllowLocalModules
    <*> optScript
    <*> many (strArgument (metavar "ARGS..." <> help "Extra arguments passed to the compiled program"))

evalSubcommand :: Mod CommandFields CliCommand
evalSubcommand =
  command
    "eval"
    (info (CmdEval <$> evalCommandParser) (progDesc "Evaluate a morloc expression"))

optSave :: Parser String
optSave =
  strOption
    ( long "save"
        <> metavar "NAME"
        <> value ""
        <> help "Save as a named command instead of running"
    )
