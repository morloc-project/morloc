{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Frontend.TerminalActions
Description : Synthesize terminal-action commands and expand @collect
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

A command's terminal actions (@--' with:@, @\@with@, @\@render@) become
synthesized commands, and every @\@collect@ is expanded into its streaming
form. Both read the types of the command, its producer and its handler, which
may be declared in other modules and spelled through aliases, so this runs
after imports are resolved and types are collected, on signatures whose
aliases are expanded in the scope of the module that declared them.
-}
module Morloc.Frontend.TerminalActions
  ( synthesizeTerminalActions
  ) where

import qualified Control.Monad.State.Strict as State
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Morloc.Data.DAG as DAG
import Morloc.Data.Doc
import qualified Morloc.Data.Map as Map
import qualified Morloc.Frontend.AST as AST
import qualified Morloc.Frontend.Desugar as Desugar
import Morloc.Frontend.Token (Pos (..))
import Morloc.Frontend.Namespace
import qualified Morloc.Monad as MM
import Morloc.Typecheck.Internal (expandTransparentAliases)

-- | Synthesize every module's terminal-action commands and expand its
-- @collect nodes. A single 'Desugar.DState' is threaded across modules so
-- synthesized nodes get globally unique indices, seeded from and returned to
-- the compiler's counter.
synthesizeTerminalActions ::
  DAG MVar [AliasedSymbol] ExprI -> MorlocMonad (DAG MVar [AliasedSymbol] ExprI)
synthesizeTerminalActions dag = do
  visible <- visibleSigs dag
  srcMap0 <- MM.gets stateSourceMap
  idx0 <- MM.gets stateCounter
  let ds0 = mkDState idx0 srcMap0
      -- What a module synthesizes is kept under its own definitions'
      -- names, which other modules may reuse.
      finalizeModule (m, (node, edges)) = do
        State.modify (\st -> st
          { Desugar.dsStreamElems = Map.empty
          , Desugar.dsReplayPlans = Map.empty
          , Desugar.dsCompanions = Map.empty
          , Desugar.dsParseSlots = Map.empty
          })
        let sigs = Map.findWithDefault Map.empty m visible
        node' <- Desugar.injectTerminalActionsWithSigs sigs node
                   >>= Desugar.expandCollectE
        made <- State.gets (\st -> ModuleCommands
          { mcCompanions = Desugar.dsCompanions st
          , mcReplayPlans = Desugar.dsReplayPlans st
          , mcStreamElems = Desugar.dsStreamElems st
          , mcParseSlots = Desugar.dsParseSlots st
          })
        return ((m, (node', edges)), (m, made))
  case State.runStateT (mapM finalizeModule (Map.toList dag)) ds0 of
    Left err -> do
      let file = posFile (Desugar.pePos err)
      srcText <- MM.gets stateSourceText
      let err' = case Map.lookup file srcText of
            Just txt | null (Desugar.peSourceLines err) -> err {Desugar.peSourceLines = T.lines txt}
            _ -> err
          label = if null file then "<terminal-action synthesis>" else file
      MM.throwSystemError . pretty $ Desugar.showParseError label err'
    Right (results, dsFinal) -> do
      MM.setCounter (Desugar.dsExpIndex dsFinal)
      MM.modify (\st -> st
        { stateSourceMap = Desugar.dsSourceMap dsFinal
        , stateModuleCommands = Map.fromList (map snd results)
        , stateErrorNotes = Map.map pretty (Desugar.dsErrorNotes dsFinal) <> stateErrorNotes st
        })
      case Desugar.dsWarnings dsFinal of
        [] -> return ()
        ws -> MM.tell ws
      return (Map.fromList (map fst results))

-- | For each module, the term signatures visible to it: its own, shadowing
-- those its imports export, under the names it imports them by, with each
-- type's transparent aliases expanded.
visibleSigs :: DAG MVar [AliasedSymbol] ExprI -> MorlocMonad (Map.Map MVar (Map.Map EVar TypeU))
visibleSigs dag = do
  scope <- MM.getGeneralScope
  let resolve _ node children =
        let local = Map.fromList
              [ (v, expandTransparentAliases scope (etype et))
              | (v, _, et) <- AST.findSignatures node ]
            imported = Map.fromList
              [ (alias, t)
              | (_, syms, (_, childExported)) <- children
              , AliasedTerm orig alias <- syms
              , Just t <- [Map.lookup orig childExported]
              ]
            visible = Map.union local imported
            exports = AST.findExportSet node
            exported = Map.filterWithKey (\k _ -> Set.member (TermSymbol k) exports) visible
         in return (visible, exported)
  result <- DAG.synthesizeNodes resolve dag
  case result of
    Just resolved -> return (Map.map (fst . fst) resolved)
    Nothing -> MM.throwSystemError "Found cyclic module dependency"

-- | A 'Desugar.DState' for this pass. Only the index counter, source map,
-- warnings and stream element types carry live data.
mkDState :: Int -> Map.Map Int SrcLoc -> Desugar.DState
mkDState idx srcMap = Desugar.DState
  { Desugar.dsExpIndex = idx
  , Desugar.dsSourceMap = srcMap
  , Desugar.dsDocMap = Map.empty
  , Desugar.dsModulePath = Nothing
  , Desugar.dsModuleConfig = defaultValue
  , Desugar.dsSourceLines = []
  , Desugar.dsLangMap = Map.empty
  , Desugar.dsProjectRoot = Nothing
  , Desugar.dsTermDocs = Map.empty
  , Desugar.dsWarnings = []
  , Desugar.dsModuleDoc = []
  , Desugar.dsModuleEpilogues = []
  , Desugar.dsNamespaces = Set.empty
  , Desugar.dsDataCtors = Map.empty
  , Desugar.dsStreamElems = Map.empty
  , Desugar.dsReplayPlans = Map.empty
  , Desugar.dsCompanions = Map.empty
  , Desugar.dsParseSlots = Map.empty
  , Desugar.dsErrorNotes = Map.empty
  }
