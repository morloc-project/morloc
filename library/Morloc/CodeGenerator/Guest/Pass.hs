{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.CodeGenerator.Guest.Pass
Description : Lower guest-language sources into host-language glue
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

A pre-realization pass that lowers sourced guest functions into the host pool.
For each guest source it builds the guest library, validates the morloc
signatures against it, generates host glue, and rewrites the 'Source' so that
downstream code sees an ordinary host source. Build products (objects, link
flags) are injected into the package metadata so the host pool links them.

Currently the only guest is Futhark and the only host is C++. Artifacts are
written under @pools/<module>/<host>-guests/<guest>/@ (alongside the host pool),
one directory per guest language, with absolute paths.
-}
module Morloc.CodeGenerator.Guest.Pass
  ( lowerGuests
  ) where

import Control.Monad.Writer (execWriter, tell)
import Data.Functor.Identity (runIdentity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (canonicalizePath, createDirectoryIfMissing, getCurrentDirectory)

import Morloc.CodeGenerator.Guest
import Morloc.CodeGenerator.Guest.Futhark (FutharkEntry, futharkGuest, hostSigTypes, resolveFutharkBuild)
import Morloc.CodeGenerator.Namespace
import Morloc.Data.Doc (pretty, render)
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.Monad as MM

-- | Lower every guest source in the typed AST to host glue. A no-op when there
-- are no guest sources.
lowerGuests :: [AnnoS (Indexed TypeU) Many Int] -> MorlocMonad [AnnoS (Indexed TypeU) Many Int]
lowerGuests asts = do
  let futSrcs = concatMap (collectLang futharkLangName) asts
  if null futSrcs
    then return asts
    else lowerFuthark asts futSrcs

-- the guest we currently lower; its Lang drives source collection and rewrite
futharkLangName :: Text
futharkLangName = langName (guestLang futharkGuest)

-- ---------------------------------------------------------------------------
-- Futhark lowering
-- ---------------------------------------------------------------------------

lowerFuthark ::
  [AnnoS (Indexed TypeU) Many Int] ->
  [(Source, TypeU)] ->
  MorlocMonad [AnnoS (Indexed TypeU) Many Int]
lowerFuthark asts futSrcs = do
  outDir <- setupBuildDir (langName (guestLang futharkGuest))
  guestSources <- dedupSources futSrcs
  langParams <- MM.gets stateLangParams
  (backend, device) <- case resolveFutharkBuild langParams of
    Left msg -> MM.throwSystemError (pretty msg)
    Right bd -> return bd
  products <- guestBuild futharkGuest guestSources (BuildOpts backend device outDir)
  let sigs = [SourcedSig src t | (src, t) <- futSrcs]
  checked <- guestCheck futharkGuest sigs products
  let glueEntries = map toGlueEntry checked
      hg = guestGlue futharkGuest products glueEntries
  MM.liftIO $ writeFile (hgHeader hg) (T.unpack (render (hgCode hg)))
  injectBuild products
  let bindings = Map.fromList [(alias, (hgHeader hg, fn)) | (alias, fn) <- hgBindings hg]
  -- Rewrite the source table too, so downstream include-collection
  -- (metaSources / lookupConstructors) sees the C++ glue header rather than
  -- the raw .fut (which is compiled separately to kernels.o, not #included).
  MM.modify $ \s -> s {stateSources = GMap.mapVals (map (rewriteFutSource bindings)) (stateSources s)}
  return (map (rewriteSrc bindings) asts)

-- Point a futhark Source at its generated glue (header path + function name),
-- keeping srcLang = futhark. Since the lang is not flipped to cpp, the Source
-- is internally inconsistent (lang says futhark, path points at a C++ header),
-- so every store that holds futhark Sources must apply this rewrite or it will
-- try to #include the raw .fut. Currently applied to both the call-site ASTs
-- and stateSources.
rewriteFutSource :: Map EVar (Path, SrcName) -> Source -> Source
rewriteFutSource bindings src
  | langName (srcLang src) == futharkLangName =
      case Map.lookup (srcAlias src) bindings of
        Just (gluePath, glueFn) -> src {srcPath = Just gluePath, srcName = glueFn}
        Nothing -> src
  | otherwise = src

-- build a GlueEntry by rendering the host (C++) arg/return types from the sig
toGlueEntry :: (SourcedSig, FutharkEntry) -> GlueEntry FutharkEntry
toGlueEntry (sig, entry) =
  let (argTs, retT) = hostSigTypes (ssType sig)
   in GlueEntry sig entry argTs retT

-- ---------------------------------------------------------------------------
-- AST helpers
-- ---------------------------------------------------------------------------

-- collect (source, node type) for every SrcCall of the given language
collectLang :: Text -> AnnoS (Indexed TypeU) Many Int -> [(Source, TypeU)]
collectLang lang = go
  where
    go :: AnnoS (Indexed TypeU) Many Int -> [(Source, TypeU)]
    go (AnnoS (Idx _ t) _ e) = case e of
      ExeS (SrcCall src)
        | langName (srcLang src) == lang -> [(src, t)]
      _ -> execWriter (mapExprSM (\c -> tell (go c) >> pure c) e)

-- Point each futhark SrcCall at the generated C++ glue function, but KEEP
-- srcLang = futhark. The call then realizes as its own (futhark) language and
-- co-locates into the host (cpp) pool via 'poolOf' (see Emit.pool /
-- decideRemoteness), rather than being rewritten to a cpp source. The glue is
-- C++, so the cpp translator emits an ordinary in-process call to it.
rewriteSrc ::
  Map EVar (Path, SrcName) ->
  AnnoS (Indexed TypeU) Many Int ->
  AnnoS (Indexed TypeU) Many Int
rewriteSrc bindings = runIdentity . go
  where
    go (AnnoS g c e) = do
      e' <- case e of
        ExeS (SrcCall src)
          | langName (srcLang src) == futharkLangName ->
              pure $ ExeS (SrcCall (rewriteFutSource bindings src))
        _ -> mapExprSM go e
      pure (AnnoS g c e')

-- ---------------------------------------------------------------------------
-- build environment + product injection
-- ---------------------------------------------------------------------------

-- Guest artifacts live alongside the host pool, under
-- pools/<module>/<host>-guests/<guest>/ -- one directory per guest language so
-- multiple guests never collide. `MM.getModuleName` is the host pool's subdir.
-- Absolute paths are used downstream so the source-rewrite and link resolve
-- regardless of build cwd.
setupBuildDir :: Text -> MorlocMonad Path
setupBuildDir guestName = do
  cwd <- MM.liftIO getCurrentDirectory
  poolSubdir <- MM.getModuleName
  let dir = cwd </> "pools" </> poolSubdir </> "cpp-guests" </> T.unpack guestName
  MM.liftIO (createDirectoryIfMissing True dir)
  return dir

dedupSources :: [(Source, TypeU)] -> MorlocMonad [GuestSource]
dedupSources futSrcs =
  mapM toGuestSource (nub [p | (src, _) <- futSrcs, Just p <- [srcPath src]])
  where
    toGuestSource p = do
      absP <- MM.liftIO (canonicalizePath p)
      return (GuestSource absP (guestLang futharkGuest))

-- append the guest objects + link flags to the package metadata so the host
-- pool build links them
injectBuild :: BuildProducts -> MorlocMonad ()
injectBuild products = do
  let flags = map T.pack (bpObjects products) ++ bpLinkFlags products
  MM.modify $ \s ->
    let pms' = case statePackageMeta s of
          [] -> [defaultValue {packageCxxFlags = flags}]
          (p : ps) -> p {packageCxxFlags = packageCxxFlags p ++ flags} : ps
     in s {statePackageMeta = pms'}
