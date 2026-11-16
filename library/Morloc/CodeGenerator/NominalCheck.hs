{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.CodeGenerator.NominalCheck
Description : Test-mode comparison of identity by key with identity by shape

Where code generation decides whether two occurrences are one type, it
also asks their keys (NOM-1). When @MORLOC_NOMINAL_LOG@ names a file, each
disagreement is appended to it as one line of tab-separated fields: site,
the shape's answer, the keys' answer, the type. The decision is always the
shape's. With the variable unset nothing is computed or written.
-}
module Morloc.CodeGenerator.NominalCheck
  ( shadow
  ) where

import Control.Exception (IOException, try)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

nominalLog :: Maybe FilePath
nominalLog = unsafePerformIO (lookupEnv "MORLOC_NOMINAL_LOG")
{-# NOINLINE nominalLog #-}

-- | The shape's answer, after recording where the keys' answer differs.
shadow :: T.Text -> T.Text -> Bool -> Bool -> Bool
shadow site what old new = case nominalLog of
  Just path
    | old /= new ->
        unsafePerformIO $ do
          let line = T.intercalate "\t" [site, T.pack (show old), T.pack (show new), T.unwords (T.words what)] <> "\n"
          r <- try (BS.appendFile path (TE.encodeUtf8 line)) :: IO (Either IOException ())
          either (\e -> hPutStrLn stderr ("MORLOC_NOMINAL_LOG: cannot write " <> path <> ": " <> show e)) return r
          return old
  _ -> old
{-# NOINLINE shadow #-}
