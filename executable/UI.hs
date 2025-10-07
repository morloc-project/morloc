module UI (
    opts
  , CliCommand(..)
  , MakeCommand(..)
  , InitCommand(..)
  , InstallCommand(..)
  , TypecheckCommand(..)
  , DumpCommand(..)
) where

import Options.Applicative
import Morloc.Namespace
import Morloc.Module (GitProtocol(..), OverwriteProtocol(..))
import qualified Options.Applicative.Extra as OAE
import Morloc.Version (versionStr)

opts :: ParserInfo CliCommand
opts = info (cliParser <**> helper <**> OAE.simpleVersioner versionStr)
  (    fullDesc
    <> progDesc "call 'morloc make -h', 'morloc install -h', etc for details"
    <> header ("morloc v" <> versionStr)
  )


data CliCommand
  = CmdMake MakeCommand
  | CmdInit InitCommand
  | CmdInstall InstallCommand
  | CmdTypecheck TypecheckCommand
  | CmdDump DumpCommand

cliParser :: Parser CliCommand
cliParser = hsubparser
  ( makeSubcommand
  <> installSubcommand
  <> typecheckSubcommand
  <> initSubcommand
  <> dumpSubcommand
  )


data MakeCommand = MakeCommand
  { makeExpression :: Bool
  , makeConfig :: String
  , makeVerbose :: Int
  , makeVanilla :: Bool
  , makeOutfile :: String
  , makeScript :: String
  }

makeCommandParser :: Parser MakeCommand
makeCommandParser = MakeCommand
  <$> optExpression
  <*> optConfig
  <*> optVerbose
  <*> optVanilla
  <*> optOutfile
  <*> optScript

makeSubcommand :: Mod CommandFields CliCommand
makeSubcommand = command "make" (info (CmdMake <$> makeCommandParser) (progDesc "build a morloc script"))


data InitCommand = InitCommand
  { initConfig :: String
  , initQuiet :: Bool
  , initVanilla :: Bool
  , initForce :: OverwriteProtocol
  , initSlurmSupport :: Bool
  }

initCommandParser :: Parser InitCommand
initCommandParser = InitCommand
  <$> optConfig
  <*> optQuiet
  <*> optVanilla
  <*> optForce
  <*> optSlurmSupport

initSubcommand :: Mod CommandFields CliCommand
initSubcommand = command "init" (info (CmdInit <$> initCommandParser) (progDesc "initialize morloc environment"))

data InstallCommand = InstallCommand
  { installConfig :: String
  , installVanilla :: Bool
  , installVerbose :: Int
  , installForce :: OverwriteProtocol
  , installUseSSH :: GitProtocol
  , installModuleStrings :: [String]
  }

makeInstallParser :: Parser InstallCommand
makeInstallParser = InstallCommand
  <$> optConfig
  <*> optVanilla
  <*> optVerbose
  <*> optForce
  <*> optUseSSH
  <*> optModuleStrings

installSubcommand :: Mod CommandFields CliCommand
installSubcommand = command "install" (info (CmdInstall <$> makeInstallParser) (progDesc "install a morloc module"))


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
makeTypecheckParser = TypecheckCommand
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
  command "typecheck" (info (CmdTypecheck <$> makeTypecheckParser) (progDesc "typecheck a morloc program"))


dumpSubcommand :: Mod CommandFields CliCommand
dumpSubcommand =
  command "dump" (info (CmdDump <$> makeDumpParser) (progDesc "dump parsed code"))

data DumpCommand = DumpCommand
  { dumpConfig :: String
  , dumpVanilla :: Bool
  , dumpVerbose :: Int
  , dumpExpression :: Bool
  , dumpScript :: String
  }

makeDumpParser :: Parser DumpCommand
makeDumpParser = DumpCommand
  <$> optConfig
  <*> optVanilla
  <*> optVerbose
  <*> optExpression
  <*> optScript



optExpression :: Parser Bool
optExpression = switch
  ( long "expression"
  <> short 'e'
  <> help "read script as string rather than file"
  )

optVanilla :: Parser Bool
optVanilla = switch
  ( long "vanilla"
  <> help "ignore local configuration files"
  )

optForce :: Parser OverwriteProtocol
optForce = flag DoNotOverwrite ForceOverwrite
  ( long "force"
  <> short 'f'
  <> help "Overwrite files if they already exist"
  )

optUseSSH :: Parser GitProtocol
optUseSSH = flag HttpsProtocol SshProtocol
  ( long "--ssh"
  <> help "Use the SSH protocol for remote git access (default: https)"
  )

optModuleStrings :: Parser [String]
optModuleStrings
  = some -- one or more
  . strArgument
  $ ( metavar "INSTALL"
    <> help "Module install strings"
    )

optRaw :: Parser Bool
optRaw = switch
  ( long "raw"
  <> help "Print raw objects"
  )

optSlurmSupport :: Parser Bool
optSlurmSupport = switch
  ( long "slurm"
  <> help "Allow use of SLURM for remote jobs"
  )

optVerbose :: Parser Int
optVerbose = length <$> many (flag' () (short 'v'))

optQuiet :: Parser Bool
optQuiet = switch
  ( long "quiet"
  <> short 'q'
  <> help "print minimal output to STDERR"
  )

optRealize :: Parser Bool
optRealize = switch
  ( long "realize"
  <> short 'r'
  <> help "typecheck the composition realizations"
  )

optConfig :: Parser String
optConfig = strOption
  ( long "config"
  <> metavar "CONFIG"
  <> value ""
  <> help "use this config file rather than the one in morloc home"
  )

optOutfile :: Parser String
optOutfile = strOption
  ( long "outfile"
  <> short 'o'
  <> metavar "OUT"
  <> value ""
  <> showDefault
  <> help "the name of the generated executable"
  )

optScript :: Parser String
optScript = argument str (metavar "<script>")

optType :: Parser Bool
optType = switch
  ( long "type"
  <> short 't'
  <> help "parse a typestring instread of an expression"
  )
