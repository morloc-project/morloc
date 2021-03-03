{-# LANGUAGE QuasiQuotes #-}

module Main where

import Subcommands
import Data
import Options.Applicative
import Data.Semigroup ((<>))

sample :: Parser CliCommand
sample = CliCommand
      <$> strOption
          ( long "hello"
         <> metavar "TARGET"
         <> help "Target for the greeting" )
      <*> switch
          ( long "quiet"
         <> short 'q'
         <> help "Whether to be quiet" )
      <*> option auto
          ( long "enthusiasm"
         <> help "How enthusiastically to greet"
         <> showDefault
         <> value 1
         <> metavar "INT" )

greet :: CliCommand -> IO ()
greet (CliCommand h False n) = putStrLn $ "Hello, " ++ h ++ replicate n '!'
greet _ = return ()

main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )


-- patterns :: Docopt
-- patterns = [docoptFile|USAGE|]
--
-- getArgOrExit :: Arguments -> Option -> IO String
-- getArgOrExit = getArgOrExitWith patterns
--
-- main :: IO ()
-- main = do
--   args <- parseArgsOrExit patterns =<< SE.getArgs
--   config <- getConfig args
--   when (isPresent args (command "install")) (cmdInstall args config)
--   -- do the following if we are processing Morloc code
--   when (isPresent args (argument "script")) $ do
--     when (isPresent args (command "make")) (cmdMake args config)
--   when (isPresent args (command "typecheck")) $ cmdTypecheck args config
