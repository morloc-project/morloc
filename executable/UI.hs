module UI (
    opts
  , CliCommand(..)
  , MakeCommand(..)
  , InstallCommand(..)
  , TypecheckCommand(..)
  , DumpCommand(..)
) where

import Options.Applicative

opts :: ParserInfo CliCommand
opts = info (cliParser <**> helper)
  (    fullDesc
    <> progDesc "call 'morloc make -h', 'morloc install -h', etc for details"
    <> header "morloc v0.46.0"  -- FIXME: HARDCODED VERSION NUMBER!!!
  )


data CliCommand
  = CmdMake MakeCommand
  | CmdInstall InstallCommand
  | CmdTypecheck TypecheckCommand
  | CmdDump DumpCommand

cliParser :: Parser CliCommand
cliParser = hsubparser
  ( makeSubcommand
  <> installSubcommand
  <> typecheckSubcommand
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

optVerbose :: Parser Int
optVerbose = length <$> many (flag' () (short 'v'))

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
