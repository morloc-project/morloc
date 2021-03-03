module Data where

data CliCommand = CliCommand
  { hello      :: String
  , quiet      :: Bool
  , enthusiasm :: Int }
