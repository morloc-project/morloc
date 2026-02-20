{- |
Module      : UI
Description : Define the Morloc CLI
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
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
  ) where

import Morloc.Module (GitProtocol (..), OverwriteProtocol (..))
import Morloc.Version (versionStr)
import Options.Applicative
import qualified Options.Applicative.Extra as OAE

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
    )

data MakeCommand = MakeCommand
  { makeExpression :: Bool
  , makeConfig :: String
  , makeVerbose :: Int
  , makeVanilla :: Bool
  , makeOutfile :: String
  , makeInstall :: Bool
  , makeForce :: Bool
  , makeInclude :: [String]
  , makeScript :: String
  }

makeCommandParser :: Parser MakeCommand
makeCommandParser =
  MakeCommand
    <$> optExpression
    <*> optConfig
    <*> optVerbose
    <*> optVanilla
    <*> optOutfile
    <*> optMakeInstall
    <*> optMakeForce
    <*> optMakeInclude
    <*> optScript

makeSubcommand :: Mod CommandFields CliCommand
makeSubcommand = command "make" (info (CmdMake <$> makeCommandParser) (progDesc "Build a morloc script"))

data InitCommand = InitCommand
  { initConfig :: String
  , initQuiet :: Bool
  , initVanilla :: Bool
  , initForce :: OverwriteProtocol
  , initSlurmSupport :: Bool
  }

initCommandParser :: Parser InitCommand
initCommandParser =
  InitCommand
    <$> optConfig
    <*> optQuiet
    <*> optVanilla
    <*> optForce
    <*> optSlurmSupport

initSubcommand :: Mod CommandFields CliCommand
initSubcommand = command "init" (info (CmdInit <$> initCommandParser) (progDesc "Initialize morloc environment"))

data InstallCommand = InstallCommand
  { installConfig :: String
  , installVanilla :: Bool
  , installVerbose :: Int
  , installForce :: OverwriteProtocol
  , installUseSSH :: GitProtocol
  , installNoTypecheck :: Bool
  , installModuleStrings :: [String]
  }

makeInstallParser :: Parser InstallCommand
makeInstallParser =
  InstallCommand
    <$> optConfig
    <*> optVanilla
    <*> optVerbose
    <*> optForce
    <*> optUseSSH
    <*> optNoTypecheck
    <*> optModuleStrings

installSubcommand :: Mod CommandFields CliCommand
installSubcommand = command "install" (info (CmdInstall <$> makeInstallParser) (progDesc "Install a morloc module"))

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
  , listConfig  :: String
  , listVanilla :: Bool
  , listVerbose :: Int
  , listKind    :: Maybe ListKind
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
  { uninstallNames   :: [String]
  , uninstallConfig  :: String
  , uninstallVanilla :: Bool
  , uninstallKind    :: Maybe ListKind
  , uninstallDryRun  :: Bool
  }

makeUninstallParser :: Parser UninstallCommand
makeUninstallParser =
  UninstallCommand
    <$> optUninstallNames
    <*> optConfig
    <*> optVanilla
    <*> optUninstallKind
    <*> optDryRun

uninstallSubcommand :: Mod CommandFields CliCommand
uninstallSubcommand =
  command "uninstall" (info (CmdUninstall <$> makeUninstallParser) (progDesc "Uninstall a module or program"))

optExpression :: Parser Bool
optExpression =
  switch
    ( long "expression"
        <> short 'e'
        <> help "Read script as string rather than file"
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

optModuleStrings :: Parser [String]
optModuleStrings =
  some -- one or more
    . strArgument
    $ ( metavar "INSTALL"
          <> help "Module install strings"
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

optOutfile :: Parser String
optOutfile =
  strOption
    ( long "outfile"
        <> short 'o'
        <> metavar "OUT"
        <> value ""
        <> showDefault
        <> help "The name of the generated executable"
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

optScript :: Parser String
optScript = argument str (metavar "<script>")

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
  optional . strArgument
    $ ( metavar "PATTERN"
          <> help "Filter by subsequence match on name"
      )

optUninstallNames :: Parser [String]
optUninstallNames =
  some
    . strArgument
    $ ( metavar "NAME"
          <> help "Names of modules or programs to uninstall"
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
