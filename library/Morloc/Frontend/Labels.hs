{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Frontend.Labels
Description : Check what a label may be written on
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

A label wraps each call of a function or each run of a suspension, so it may
only be written on a name of one of those types (ANN-8, ANN-10). A cache or
a remote job needs to know what it runs, so those directives are refused on
a value bound by a lambda or a let (ANN-11).
-}
module Morloc.Frontend.Labels
  ( checkLabels
  ) where

import qualified Data.Map as Map
import Morloc.Data.Doc
import Morloc.Frontend.Namespace
import Morloc.Frontend.Rename (displayName)
import qualified Morloc.Monad as MM

checkLabels :: AnnoS (Indexed Type) Many Int -> MorlocMonad (AnnoS (Indexed Type) Many Int)
checkLabels tree = do
  mapM_ check [(i, t, k, inner) | AnnoS (Idx i t) _ (LabelS k inner) <- annoNodes tree]
  return tree
  where
    check :: (Int, Type, Int, AnnoS (Indexed Type) Many Int) -> MorlocMonad ()
    check (i, t, k, AnnoS _ _ inner) = do
      cfg <- MM.gets (Map.lookup k . stateManifoldConfig)
      c <- maybe (MM.throwCompilerBug ("label" <+> pretty k <+> "has no configuration")) return cfg
      let loc = fromMaybe i (manifoldConfigLabelIdx c)
          (name, local) = case inner of
            VarS v _ -> (pretty (displayName v), False)
            BndS v -> (pretty (displayName v), True)
            LetBndS v -> (pretty (displayName v), True)
            _ -> ("this term", False)
          onName = "The label" <+> squotes (pretty (fromMaybe "" (manifoldConfigLabel c))) <+> "on" <+> squotes name
      unless (isLabelable t) $
        MM.throwSourcedError loc $
          onName <+> "is on a value of type" <+> squotes (pretty t) <> ", not a function or a suspension."
            <+> "A label wraps each call of a function; give the computation a parameter and label that function."
      when (local && (manifoldConfigCache c == Just True || isJust (manifoldConfigRemote c))) $
        MM.throwSourcedError loc $
          onName <+> "asks for a cache or a remote job on a value bound inside this definition;"
            <+> "both need a function defined at the top level or sourced, so label that function where it is called."
    isLabelable (FunT _ _) = True
    isLabelable (EffectT _ _) = True
    isLabelable _ = False
