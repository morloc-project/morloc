{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Data.PoolHash
Description : Deterministic 64-bit pool fingerprint for cache discrimination
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Per-pool source fingerprint plumbing for the runtime cache. The hash
covers the pool's emitted source text and the contents of any files
listed under the program YAML's @hash-include@ field. The result is
stored as a 16-character lowercase hex string in the manifest's
@pool_hash@ slot and then exported to the running pool via
@MORLOC_POOL_HASH@ so it can be mixed into every cache key.

XXH64 is the hash used both here and in the runtime; any correct
implementation produces identical output for identical input. This
module uses @xxhash-ffi@; the runtime uses @twox-hash@.
-}
module Morloc.Data.PoolHash
  ( poolHash
  , poolHashHex
  , computePoolHashes
  , patchManifestPoolHashes
  ) where

import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)
import System.Directory.Tree (AnchoredDirTree ((:/)), DirTree (Dir, File))
import Text.Printf (printf)

import qualified Data.Digest.XXHash.FFI as XXH

import qualified Morloc.Language as ML
import Morloc.Namespace.Prim (Code (..), Lang)
import Morloc.Namespace.State (Script (..))

-- | Hash one pool's emitted source, then chain in the contents of every
-- declared @hash-include@ file (in the order given -- callers are
-- expected to have pre-sorted the list for reproducibility). The seed
-- chain (output of one hash becomes the seed of the next) makes the
-- result order-sensitive and avoids materializing every file in memory
-- at once.
poolHash :: T.Text -> [FilePath] -> IO Word64
poolHash poolSrc includes = do
  let h0 = XXH.xxh64 (TE.encodeUtf8 poolSrc) 0
  foldFiles h0 includes
  where
    foldFiles h [] = return h
    foldFiles h (fp : rest) = do
      -- Strict read so xxh64 (which takes a strict ByteString) can
      -- consume the file in one shot. Hash-include files are
      -- user-curated and typically modest; if very large files become
      -- common, swap to a streaming approach.
      contents <- BS.readFile fp
      let h' = XXH.xxh64 contents h
      foldFiles h' rest

-- | Hex-encode a 64-bit pool hash as a 16-character lowercase string.
-- This is the form embedded into each pool's manifest entry and later
-- exported via @MORLOC_POOL_HASH@.
poolHashHex :: Word64 -> T.Text
poolHashHex = T.pack . printf "%016x"

-- | Hash every script in @pools@, returning a per-language map. The
-- supplied @hash-include@ paths are folded into every pool's hash
-- (a single, program-wide list shared across pools).
computePoolHashes :: [FilePath] -> [Script] -> IO (Map Lang Word64)
computePoolHashes includes pools =
  Map.fromList <$> mapM go pools
  where
    go p = do
      h <- poolHash (extractSource p) includes
      return (scriptLang p, h)

    -- A 'Script' may emit a directory tree with files at any depth.
    -- Concatenating every 'Code' text in traversal order makes the
    -- fingerprint stable as long as the file contents are unchanged.
    extractSource :: Script -> T.Text
    extractSource Script {scriptCode = _ :/ tree} = T.concat (walk tree)
      where
        walk (File _ (Code t)) = [t]
        walk (Dir _ children) = concatMap walk children
        walk _ = []

-- | Replace the @<MORLOC_POOL_HASH:lang>@ placeholders that
-- 'Morloc.CodeGenerator.Nexus.buildManifest' wrote into the manifest's
-- per-pool @pool_hash@ slots with the real hex hashes. One substring
-- pass per language; placeholders are distinct so the order of
-- replacements doesn't matter.
patchManifestPoolHashes :: Map Lang Word64 -> Script -> Script
patchManifestPoolHashes hashes s = s {scriptCode = patchTree (scriptCode s)}
  where
    patchTree (anchor :/ tree) = anchor :/ patchNode tree
    patchNode (File n (Code t)) = File n (Code (Map.foldlWithKey applyOne t hashes))
    patchNode (Dir n xs) = Dir n (map patchNode xs)
    patchNode other = other

    applyOne :: T.Text -> Lang -> Word64 -> T.Text
    applyOne t lang h =
      let key = "<MORLOC_POOL_HASH:" <> ML.showLangName lang <> ">"
       in T.replace key (poolHashHex h) t
