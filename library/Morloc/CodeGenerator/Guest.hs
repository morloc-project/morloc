{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.CodeGenerator.Guest
Description : Interface between a host pool generator and a guest language
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

A guest is a foreign language that has no pool of its own. Its sourced functions
are compiled to a linkable library and called from generated glue in a host
language's pool. This module defines the interface a host pool generator uses to
drive a guest: build the guest source, check the morloc signatures against what
was built, and generate the host glue.

The interface is a record of functions parameterized by the guest's parsed-entry
type @e@. Build and check run in 'MorlocMonad'. Glue generation is pure: the
driver renders the argument and return types and passes them in through
'GlueEntry', so glue codegen never touches the translator monad.
-}
module Morloc.CodeGenerator.Guest
  ( Guest (..)
  , GuestSource (..)
  , BuildOpts (..)
  , BuildProducts (..)
  , SourcedSig (..)
  , GlueEntry (..)
  , HostGlue (..)
  ) where

import Data.Text (Text)
import Morloc.CodeGenerator.Namespace

-- | A guest source file to build. Deduplicated from the 'Source' declarations
-- that reference it (many entries may share one file); the output stem is
-- @takeBaseName gsPath@.
data GuestSource = GuestSource
  { gsPath :: Path
  , gsLang :: Lang
  }

-- | Build options resolved from the build parameters and the build environment.
data BuildOpts = BuildOpts
  { boBackend :: Maybe Text -- ^ backend selector; Nothing = the guest default
  , boDevice :: Maybe Text -- ^ optional device selector; guest-specific meaning
  , boOutDir :: Path -- ^ where artifacts are written (pools/<module>/)
  }

-- | The linkable output of a guest build plus the interface descriptor(s) for
-- 'guestCheck'. Holds only paths and flags, no parsed entries.
data BuildProducts = BuildProducts
  { bpObjects :: [Path] -- ^ objects (or sources) to add to the pool build
  , bpHeaders :: [Path] -- ^ headers the glue includes
  , bpIncludeDirs :: [Path]
  , bpLinkFlags :: [Text]
  , bpManifests :: [Path] -- ^ interface descriptors; empty if the guest has none
  , bpDevice :: Maybe Text -- ^ optional device selector to configure at runtime
  }

-- | A sourced guest function: its morloc-side declaration paired with its
-- post-typecheck signature. 'ssType' feeds 'guestCheck'.
data SourcedSig = SourcedSig
  { ssSource :: Source
  , ssType :: TypeU
  }

-- | One entry ready for glue generation: its declaration, the guest's parsed
-- entry, and the argument and return types rendered by the driver (passed in so
-- glue generation stays pure).
data GlueEntry e = GlueEntry
  { gleSig :: SourcedSig
  , gleEntry :: e
  , gleArgTypes :: [MDoc] -- ^ host-rendered argument types, in order
  , gleRetType :: MDoc -- ^ host-rendered return type
  }

-- | The rendered glue for one guest module. The host writes 'hgCode' to
-- 'hgHeader', then rewrites each 'Source' to call the glue function named in
-- 'hgBindings'.
data HostGlue = HostGlue
  { hgCode :: MDoc
  , hgHeader :: Path
  , hgBindings :: [(EVar, SrcName)] -- ^ srcAlias -> generated glue function name
  }

-- | The host-to-guest interface, parameterized by the guest's parsed-entry type
-- @e@ ('guestCheck' produces it, 'guestGlue' consumes it).
data Guest e = Guest
  { guestLang :: Lang
  , guestBuild :: [GuestSource] -> BuildOpts -> MorlocMonad BuildProducts
  , guestCheck :: [SourcedSig] -> BuildProducts -> MorlocMonad [(SourcedSig, e)]
  , guestGlue :: BuildProducts -> [GlueEntry e] -> HostGlue
  }
