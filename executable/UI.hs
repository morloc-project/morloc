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
import qualified Options.Applicative.Extra as OAE

version :: String
version = "v0.49.0"   -- FIXME: HARDCODED VERSION NUMBER!!!

opts :: ParserInfo CliCommand
opts = info (cliParser <**> helper <**> OAE.simpleVersioner version)
  (    fullDesc
    <> progDesc "call 'morloc make -h', 'morloc install -h', etc for details"
    <> header ("morloc " <> version)
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
  , initForce :: Bool
  }

initCommandParser :: Parser InitCommand
initCommandParser = InitCommand
  <$> optConfig
  <*> optQuiet
  <*> optVanilla
  <*> optForce

initSubcommand :: Mod CommandFields CliCommand
initSubcommand = command "init" (info (CmdInit <$> initCommandParser) (progDesc "initialize morloc environment"))

data InstallCommand = InstallCommand
  { installConfig :: String
  , installVerbose :: Int
  , installGithub :: Bool
  , installVanilla :: Bool
  , installModuleName :: String
  }

makeInstallParser :: Parser InstallCommand
makeInstallParser = InstallCommand
  <$> optConfig
  <*> optVerbose
  <*> optGithub
  <*> optVanilla
  <*> optModuleName

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

optRaw :: Parser Bool
optRaw = switch
  ( long "raw"
  <> help "print raw objects"
  )

optForce :: Parser Bool
optForce = switch
  ( long "force"
  <> short 'f'
  <> help "Force action overwriting existing files"
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

optGithub :: Parser Bool
optGithub = switch
  ( long "github"
  <> help "install module from github"
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

optModuleName :: Parser String
optModuleName = argument str (metavar "<module_name>")
