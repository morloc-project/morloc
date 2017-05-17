module MorlocExecutable.Repl (repl) where

import System.Console.Repline
import Control.Monad.State.Strict
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)

import qualified Morloc.Type as Type

type Repl a = HaskelineT IO a

-- main command, interpret Morloc
cmd :: (String -> String) -> String -> Repl()
cmd process line = liftIO $ putStr $ process line

opts :: [(String, [String] -> Repl ())]
opts = [
    ( "validate"     , wrapValidateE )
  , ( "output-gen"   , wrapGenerateO )
  , ( "input-gen"    , wrapGenerateI )
  , ( "convert-json" , wrapConvertE  )
  , ( "cat"          , catFiles      )
  ]

say :: MonadIO m => String -> m ()
say = liftIO . putStrLn

says :: (MonadIO m, Show a) => a -> m ()
says = liftIO . print

-- validateE :: Type -> EdgeSpec -> Common -> Bool 
wrapValidateE :: [String] -> Repl ()
wrapValidateE [s1,s2,s3] = says . Type.validateE typ spec $ input where
  typ   = read     s1
  spec  = read     s2
  input = Type.Raw s3
wrapValidateE _ = say "ERROR: could not parse command"

-- generateO :: Lang -> Type -> TypeSpec -> Maybe Code
wrapGenerateO :: [String] -> Repl ()
wrapGenerateO [s1,s2,s3] = say . fromMaybe msg . Type.generateO lang typ $ spec where
  msg  = "Failed to generate code"
  lang = read s1
  typ  = read s2
  spec = read s3
wrapGenerateO _ = say "ERROR: could not parse command"

-- generateI :: Lang -> Type -> TypeSpec -> Maybe Code
wrapGenerateI :: [String] -> Repl ()
wrapGenerateI [s1,s2,s3] = say . fromMaybe msg . Type.generateI lang typ $ spec where
  msg  = "Failed to generate code"
  lang = read s1
  typ  = read s2
  spec = read s3
wrapGenerateI _ = say "ERROR: could not parse command"

-- convertE :: Type -> Type -> EdgeSpec -> Common -> Maybe Common
wrapConvertE :: [String] -> Repl ()
wrapConvertE [s1,s2,s3,s4] = says . fromMaybe msg . Type.convertE typ1 typ2 spec $ common where
  msg    = Type.Raw "Failed to convert code"
  typ1   = read s1
  typ2   = read s2
  spec   = read s3
  common = Type.Raw s4
wrapConvertE _ = say "ERROR: could not parse command"

catFiles :: [String] -> Repl ()
catFiles args = liftIO $ do
  contents <- readFile (unwords args)
  putStrLn contents

repl f = evalRepl prompt eval opts autocomplete start where

  prompt = "morloc> "

  eval = cmd f

  matcher :: MonadIO m => [(String, CompletionFunc m)]
  matcher = [
      (":validate"     , fileCompleter)
    , (":output-gen"   , fileCompleter)
    , (":input-gen"    , fileCompleter)
    , (":convert-json" , fileCompleter)
    , (":cat"          , fileCompleter)
    ]

  byWord :: Monad m => WordCompleter m
  byWord n = do
    let names = [":validate", ":output-gen", ":input-gen", ":convert-json", ":cat"]
    return $ filter (isPrefixOf n) names

  autocomplete = Prefix (wordCompleter byWord) matcher

  start :: Repl ()
  start = return ()
