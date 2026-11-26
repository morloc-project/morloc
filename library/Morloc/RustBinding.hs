{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Morloc.RustBinding
Description : The Rust language binding (rustmorloc) the compiler installs
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

The rustmorloc crate, embedded file by file. Generated Rust code calls
@::rustmorloc::*@, so the crate belongs to the compiler that generates the
calls: @morloc init@ installs this compiler's copy (SHIM-6), and Rust pools
depend on it by path. Kept in a module of its own so that an edit to the
crate recompiles only what installs or names it.
-}
module Morloc.RustBinding
  ( rustBindingFiles
  , rustBindingHash
  , rustBindingDir
  ) where

import qualified Data.ByteString as BS
import Data.FileEmbed (embedFileRelative)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Morloc.Data.PoolHash (hashText)
import System.FilePath ((</>))

-- | The crate's files, relative to its directory, as bytes. A unit test
-- checks that this list is the crate's directory listing.
rustBindingFiles :: [(FilePath, BS.ByteString)]
rustBindingFiles =
  [ ("Cargo.toml", $(embedFileRelative "data/lang/rust/rustmorloc/Cargo.toml"))
  , ("Cargo.lock", $(embedFileRelative "data/lang/rust/rustmorloc/Cargo.lock"))
  , ("build.rs", $(embedFileRelative "data/lang/rust/rustmorloc/build.rs"))
  , ("src/lib.rs", $(embedFileRelative "data/lang/rust/rustmorloc/src/lib.rs"))
  , ("src/fv.rs", $(embedFileRelative "data/lang/rust/rustmorloc/src/fv.rs"))
  , ("src/panic_probe_pool.rs", $(embedFileRelative "data/lang/rust/rustmorloc/src/panic_probe_pool.rs"))
  , ("src/panic_probe_user.rs", $(embedFileRelative "data/lang/rust/rustmorloc/src/panic_probe_user.rs"))
  ]

-- | A hash of the crate's names and contents, which names its installed
-- directory.
rustBindingHash :: T.Text
rustBindingHash =
  hashText . T.concat $
    concat [[T.pack name, "\0", TE.decodeUtf8Lenient bytes, "\0"] | (name, bytes) <- rustBindingFiles]

-- | Where a home holds this compiler's copy of the crate. Its relative path
-- dependency on the runtime types (@../../../rust/morloc-runtime-types@)
-- reaches the home's persisted runtime source from here.
rustBindingDir :: FilePath -> FilePath
rustBindingDir homeDir = homeDir </> "lang" </> "rust" </> ("rustmorloc-" <> T.unpack rustBindingHash)
