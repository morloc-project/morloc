{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.Frontend.Treeify
Description : Dissolve the module DAG into per-export 'AnnoS' call trees
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

After linking populates 'MorlocState', this module builds one 'AnnoS' tree
per exported function by inlining declarations, resolving sources, and
renaming lambda-bound variables for uniqueness. The resulting trees are the
input to the typechecker and code generator.
-}
module Morloc.Frontend.Treeify
  ( treeify
  , Validation (..)
  , contractTrees
  , definitionTree
  , unsignedDefs
  , firstPerTerm
  , instanceBodyTree
  , instanceBodySites
  , collectRoot
  , recName
  ) where

import qualified Data.Set as Set
import qualified Morloc.Data.DAG as DAG
import Morloc.Data.Doc
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.Data.Map as Map
import qualified Morloc.Data.Text as MT
import qualified Morloc.Frontend.AST as AST
import qualified Morloc.Frontend.Link as MFL
import Morloc.Frontend.Rename (displayName, renameLocals)
import Morloc.Frontend.Namespace
import qualified Morloc.Monad as MM
import qualified Morloc.Frontend.Instances as FI
import Morloc.Frontend.Instances (MethodTerm (..))
import Morloc.Typecheck.Internal (collectEffLabels, instanceArgHead, patternAccessibleMethod, structuralMethods)
import qualified Morloc.DataFiles as DF
import qualified Morloc.Config as MC
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import qualified Morloc.Language as ML
import Morloc.CodeGenerator.LanguageDescriptor (SourceRules (..), declaredSourceRules, matchNamePattern)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)

-- | Every term must either be sourced or declared.
data TermOrigin = Declared ExprI | Sourced Source
  deriving (Show, Ord, Eq)

data BindKind = LambdaBound | LetBound
  deriving (Show, Eq)

-- Manage unique naming in each tree
data Namer = Namer
  { namerMap :: Map.Map EVar (EVar, BindKind)
  , namerIndex :: Int
  , namerExpanding :: Map.Map Int EVar  -- terms currently being expanded, by term identity, with the name their back-edges use
  , namerRecursive :: Set.Set Int  -- terms reached through a back-edge while being expanded
  , namerShallow :: Int  -- the term id of the definition collected
  , namerShallowOnly :: Maybe (Set.Set Int)
  -- ^ with 'namerShallow': only these terms are referenced by signature;
  -- every other term is expanded
  , namerInline :: Set.Set Int
  -- ^ the class methods expanded in place at their uses
  }
  deriving (Show)

-- | Drop each source the program's own modules shadow (SRC-9): a term the
-- program sources in a language has no imported source in it. Every reader
-- of a term's sources sees the result. The program's own modules are those
-- of the root module's package, or, for a script, the root module and its
-- local submodules.
shadowImportedSources :: MVar -> MorlocMonad ()
shadowImportedSources root = do
  packages <- MM.gets stateModulePackages
  let package m = Map.lookup m packages
      own m = case package root of
        Just p -> package m == Just p
        Nothing -> m == root || MT.isPrefixOf "." (unMVar m)
      unshadowed srcs =
        let ownLangs = Set.fromList [srcLang src | (m, Idx _ src) <- srcs, own m]
         in [x | x@(m, Idx _ src) <- srcs, own m || Set.notMember (srcLang src) ownLangs]
      shadow (Monomorphic t) = Monomorphic t {termConcrete = unshadowed (termConcrete t)}
      shadow x = x
  MM.modify' $ \st -> st {stateSignatures = GMap.mapVals shadow (stateSignatures st)}

-- When I see a term, I need to look it up. To do so, I need to walk up through
-- scope and fine all sources/declarations and type annotations. This involves
-- walking up through where statements (shadowing is possible), up through lambdas
-- (where again shadowing is possible), to the module scope, and to the imported
-- scope (where terms are merged, not shadowed).
--
-- Generate integers for all positions in the tree, use these to map into a table that includes:
--  * manual type annotations or signatures
--  * inferred type annotations
--
-- All expressions are mapped to integer indices linking expressions to their
-- ultimate type annotations. The indices also match terms to their signatures
-- and locations in source code.
-- | What 'Morloc.Frontend.Typecheck.validate' checks, and what
-- specialization references by signature.
data Validation = Validation
  { validationModules :: [ExprI]
  -- ^ every module of the program, the root first ('contractTrees')
  , validationDefs :: Map.Map Int (Int, EVar)
  -- ^ each signed top-level definition of the program by term id: its
  -- declaration index and name
  , validationInferred :: Set.Set Int
  -- ^ the terms among them whose type was inferred, not written (POLY-8)
  }

treeify ::
  DAG MVar [AliasedSymbol] ExprI ->
  MorlocMonad (Validation, [(Int, EVar)])
treeify d
  | Map.size d == 0 = return (Validation [] Map.empty Set.empty, [])
  | otherwise = case DAG.roots d of
      -- if no parentless element exists, then the graph must be empty or cyclic
      [] -> MM.throwCompilerBug "cyclic import dependency in treeify"
      -- else if exactly one module name key (k) is found
      [k] -> do
        case DAG.lookupNode k d of
          -- if the key is not in the DAG, then something is dreadfully wrong codewise
          Nothing -> MM.throwCompilerBug $ "module DAG is missing key" <+> pretty k
          (Just (AST.findExport -> ExportMany symbols groups)) -> do
            -- every sourced symbol must be a name or operator of its language
            -- (lang.yaml patterns, never hardcoded here): the name is emitted
            -- verbatim, so code in this position would be injected into the
            -- pool
            sourceRules <- liftIO (newIORef Map.empty)
            d' <- DAG.mapNodeM (linkAndRemoveAnnotations >=> renameLocals >=> checkSources sourceRules) d

            -- move all to state, after this the DAG will no longer be needed
            _ <- MFL.link d'
            shadowImportedSources k

            -- every effect label used in a `<...>` row must be declared
            -- (the compiler hardcodes none); checked after link populates
            -- the effect registry
            checkDeclaredEffects d'

            -- PatternAccessible dispatches the pattern-accessor syntax
            -- (`.foo`, `.[i]`, `.[s:e:p]`) at codegen; Indexable /
            -- Sliceable / SliceableDim dispatch the same syntax
            -- structurally. Declaring both for the same head is
            -- ambiguous.
            checkPatternAccessibleCoherence

            -- an instance's methods, at its types, are held to the same rule
            checkInstanceEffects

            -- find all term exports (ungrouped + grouped)
            let allSymbols = Set.unions (symbols : [exportGroupMembers g | g <- groups])
                exports = [(i, v) | (i, TermSymbol v) <- Set.toList allSymbols]
                exportedNames = Set.fromList [v | (_, v) <- exports]

            -- Root the companions of every command the root exports. A
            -- companion already among the root's exports (a command the
            -- root defines itself) is rooted there.
            (companions, replayPlans, streamElems, parseSlots) <- rootCompanions d k exports
            let companionExports =
                  [ e | e@(_, m) <- companions, not (Set.member m exportedNames) ]
                exports' = exports ++ companionExports

            -- Build export group info for the state
            let exportGroupInfo =
                  Map.fromList
                    [ ( exportGroupName g
                      , (exportGroupDesc g, [i | (i, TermSymbol _) <- Set.toList (exportGroupMembers g)])
                      )
                    | g <- groups
                    ]

            -- - store all exported indices in state
            -- - Add the export name to state. Failing to do so here, will lose
            --   the name of terms that are exported but not defined, this leads
            --   to cryptic error messages.
            MM.modify'
              ( \s ->
                  s
                    { stateExports = map fst exports'
                    , stateName =
                        Map.union (Map.fromList companionExports)
                          (Map.union (stateName s) (Map.fromList exports))
                    , stateExportGroups = exportGroupInfo
                    , stateReplayPlans = replayPlans
                    , stateParseSlots = parseSlots
                    , stateStreamElems = Map.union streamElems (stateStreamElems s)
                    }
              )

            let modules = [n | (k', (n, _)) <- Map.toList d', k' == k] <> [n | (k', (n, _)) <- Map.toList d', k' /= k]
            mapM_ checkBareClassConstraints modules
            sigs <- MM.gets stateSignatures
            -- every module's signed definitions, each referenced by its
            -- signature and elaborated once per type it is used at
            let defs = Map.fromList [(termId, (i, v)) | (termId, v, i) <- firstPerTerm (concatMap (signedDefs sigs) modules)]
            return (Validation modules defs Set.empty, exports')
          (Just _) ->
            error "This should not be possible, all ExportAll cases should have been removed in Restructure.hs"

      -- There is no currently supported use case that exposes multiple roots in
      -- one compilation process. The compiler executable takes a single morloc
      -- file as input, therefore this MUST be the root. In the future compiling
      -- multiple projects in parallel with potentially shared information and
      -- constraints could be valuable.
      roots ->
        MM.throwCompilerBug $
          "unsupported multi-rooted module DAG:"
            <+> tupled (map pretty roots)

-- | The companions (terminal-action, replay and `@parse` entries) of every
-- command the root module exports, each with the name the program knows it
-- by, and the facts synthesis recorded about them under those names.
--
-- Program roots come only from the root module's export list, but a
-- companion is exported by the module that defines its command (see
-- 'Desugar.addToExport'). Each root export is followed through the imports
-- to its definition; the companions synthesis recorded there
-- ('stateModuleCommands') are found in that module's own exports and
-- renamed after the root's export name. Root export names are unique, so a
-- command imported under an alias, or two same-named commands from
-- different modules, keep distinct companions.
rootCompanions ::
  DAG MVar [AliasedSymbol] ExprI ->
  MVar ->
  [(Int, EVar)] ->
  MorlocMonad
    ( [(Int, EVar)]
    , Map.Map EVar ReplayPlan
    , Map.Map EVar TypeU
    , Map.Map EVar [Int]
    )
rootCompanions d root exports = do
  made <- MM.gets stateModuleCommands
  found <- mapM (forExport made) exports
  return
    ( concat [cs | (cs, _, _, _) <- found]
    , Map.unions [ps | (_, ps, _, _) <- found]
    , Map.unions [ss | (_, _, ss, _) <- found]
    , Map.unions [ws | (_, _, _, ws) <- found]
    )
  where
    forExport made (_, v) = case definitionOf root v of
      Nothing -> return ([], Map.empty, Map.empty, Map.empty)
      Just (m, orig) -> do
        let mc = Map.lookup m made
            roles = maybe [] (Map.findWithDefault [] orig . mcCompanions) mc
            plans = maybe Map.empty mcReplayPlans mc
            streams = maybe Map.empty mcStreamElems mc
            exported = Map.fromList [(n, i) | (i, n) <- moduleExportTerms m]
        cs <- mapM (rootOne m orig v exported) roles
        let renamed = (orig, v) : [(companionName orig r, companionName v r) | r <- roles]
        return
          ( cs
          , Map.fromList
              [ (mangleReplayName v long, p)
              | ((parent, long), p) <- Map.toList plans
              , parent == orig
              ]
          , Map.fromList
              [ (new, t) | (old, new) <- renamed, Just t <- [Map.lookup old streams] ]
          , maybe Map.empty (Map.singleton v) (Map.lookup orig . mcParseSlots =<< mc)
          )

    rootOne :: MVar -> EVar -> EVar -> Map.Map EVar Int -> CompanionRole -> MorlocMonad (Int, EVar)
    rootOne m orig v exported role =
      let n = companionName orig role
       in case Map.lookup n exported of
            Just i -> return (i, companionName v role)
            Nothing ->
              MM.throwCompilerBug $
                "the companion" <+> pretty n <+> "of" <+> pretty orig
                  <+> "is not exported by module" <+> pretty m

    -- the module that defines what module m exports as v, and its name there
    definitionOf m v = case Map.lookup m d of
      Nothing -> Nothing
      Just (node, edges)
        | v `elem` [n | (n, _, _) <- AST.findSignatures node] -> Just (m, v)
        | otherwise ->
            listToMaybe
              [ def
              | (child, syms) <- edges
              , AliasedTerm orig alias <- syms
              , alias == v
              , Just def <- [definitionOf child orig]
              ]

    moduleExportTerms m = case AST.findExport <$> DAG.lookupNode m d of
      Just (ExportMany symbols groups) ->
        let allSs = Set.toList symbols ++ concatMap (Set.toList . exportGroupMembers) groups
         in [(i, v) | (i, TermSymbol v) <- allSs]
      _ -> []

-- | Check every sourced symbol against its language's rules (lang.yaml,
-- read once per language through the shared cache) and mark whether it is
-- an operator (called infix): backtick-quoted, or a symbol its language's
-- name pattern refuses and its operator pattern accepts. A symbol neither
-- accepts is refused, since it is emitted verbatim; so is one with a
-- segment the pool reserves or the language keeps as a keyword.
checkSources :: IORef (Map.Map MT.Text (Either String SourceRules)) -> ExprI -> MorlocMonad ExprI
checkSources cache = go
  where
    go (ExprI i e) = descend e >>= fmap (ExprI i) . node i
    descend (ModE v es) = ModE v <$> mapM go es
    descend (AssE v x es) = AssE v <$> go x <*> mapM go es
    descend (IstE a b c d es) = IstE a b c d <$> mapM go es
    descend x = AST.mapExprIChildrenM go x
    node i (SrcE src) = do
      let lang = ML.langName (srcLang src)
      known <- liftIO (readIORef cache)
      rules <- case Map.lookup lang known of
        Just r -> return r
        Nothing -> do
          r <- (>>= declaredSourceRules) <$> languageDescriptorText lang
          liftIO (modifyIORef' cache (Map.insert lang r))
          return r
      SrcE <$> checkSource i rules src
    node _ e = return e

checkSource :: Int -> Either String SourceRules -> Source -> MorlocMonad Source
checkSource i rules src = do
  let lang = ML.langName (srcLang src)
      nm = unSrcName (srcName src)
      cannot :: String -> MorlocMonad a
      cannot why =
        MM.throwSourcedError i $
          "Cannot check" <+> pretty lang <+> "source" <+> squotes (pretty nm) <> ":" <+> pretty why
      refuse :: MDoc -> MorlocMonad ()
      refuse why = MM.throwSourcedError i $ "The" <+> pretty lang <+> "source" <+> squotes (pretty nm) <+> why
  sr <- either cannot return rules
  let segments = nm : concatMap (MT.splitOn ".") (MT.splitOn "::" nm)
      reserved s = s `elem` srReservedNames sr || any (`MT.isPrefixOf` s) (srReservedPrefixes sr)
  case filter reserved (nm : MT.splitOn "::" nm) of
    (s : _) -> refuse $ "uses" <+> squotes (pretty s) <> ", a name reserved for what the" <+> pretty lang <+> "pool defines."
    [] -> return ()
  -- Backtick-quoted names bypass the patterns and keywords: they are the
  -- user's explicit "emit this verbatim as an infix operator" for
  -- keyword-shaped operators (Python `and`, R `%in%`).
  if srcBacktick src
    then return src {srcOperator = True}
    else do
      case filter (`elem` srKeywords sr) segments of
        (k : _) -> refuse $ "uses" <+> squotes (pretty k) <> ", a" <+> pretty lang <+> "keyword, not a name."
        [] -> return ()
      isName <- either cannot return (matchNamePattern (srNamePattern sr) nm)
      isOp <- if isName then return False else either cannot return (matchNamePattern (srOperatorPattern sr) nm)
      unless (isName || isOp) $
        refuse $
          "is not a valid" <+> pretty lang <+> "name or operator. A sourced symbol is emitted"
            <+> "verbatim, so it must not be code. Expected pattern"
            <+> squotes (pretty (srNamePattern sr)) <+> "or"
            <+> squotes (pretty (srOperatorPattern sr)) <> "."
      return src {srcOperator = isOp}

-- | A language's lang.yaml: the embedded one, else a plugin's under the
-- morloc home, as the code generator finds it.
languageDescriptorText :: MT.Text -> MorlocMonad (Either String MT.Text)
languageDescriptorText lang =
  case lookup (MT.unpack lang) [(n, DF.embededFileText ef) | (n, ef) <- DF.langRegistryFiles] of
    Just t -> return (Right t)
    Nothing -> do
      home <- MM.asks MC.configHome
      let path = home </> "lang" </> MT.unpack lang </> "lang.yaml"
      found <- liftIO (doesFileExist path)
      if found
        then Right . TE.decodeUtf8 <$> liftIO (BS.readFile path)
        else return (Left ("no language descriptor at " <> path))

-- | Two signature-level effect checks, run after 'MFL.link' has
-- populated 'stateEffects':
--
--  1. Every concrete effect label in a `<...>` row of a signature,
--     annotation, typedef body, or class-method type must be declared
--     with `effect` / `escapable effect`. The compiler hardcodes no
--     effect names, so an undeclared label is always a user error.
--
--  2. Inescapable-propagation: every *inescapable* concrete label that
--     appears in a parameter's effect row must also appear in the
--     result row. This holds for all functions, sourced or defined --
--     a sourced handler may discharge an *escapable* effect, but no
--     function (not even a sourced one) may silently consume an
--     inescapable one. Effect variables always propagate (they appear
--     in the result), so they satisfy the rule automatically.
checkDeclaredEffects :: DAG MVar [AliasedSymbol] ExprI -> MorlocMonad ()
checkDeclaredEffects d = do
  declared <- MM.gets stateEffects
  mapM_ (AST.checkExprI (checkNode declared)) (DAG.nodes d)
  where
    checkNode :: Map.Map EffectLabel Bool -> ExprI -> MorlocMonad ()
    checkNode declared (ExprI i e) = do
      let tys = case e of
            SigE (Signature _ _ et) -> [etype et]
            AnnE _ t -> [t]
            TypE (ExprTypeE _ _ _ t _ _) -> [t]
            ClsE (Typeclass _ _ _ sigs) -> [etype et | Signature _ _ et <- sigs]
            _ -> []
          -- an instance's types name effects too, but are not functions
          -- whose arguments could leak one
          heads = case e of
            IstE _ _ _ ts _ -> ts
            _ -> []
          used = Set.unions (map collectEffLabels (tys <> heads))
          undeclared = Set.filter (\l -> not (Map.member l declared)) used
      case Set.toList undeclared of
        (lbl0 : rest) ->
          let isPlural = not (null rest)
           in MM.throwSourcedError i $
                "Undeclared effect"
                  <> (if isPlural then "s" else "")
                  <+> hcat
                    (punctuate ", " (map (squotes . pretty) (lbl0 : rest)))
                  <> "."
                  <+> (if isPlural then "Declare each" else "Declare it")
                  <+> "with `effect"
                  <+> pretty lbl0 <> "`"
                  <+> "(or `escapable effect"
                  <+> pretty lbl0 <> "`) and import it."
        [] -> do
          let leaked = Set.unions (map (inescapableLeak declared) tys)
          if Set.null leaked
            then return ()
            else MM.throwSourcedError i (leakMessage leaked)

-- | The inescapable effect labels in a function type's parameters that its
-- result does not carry.
inescapableLeak :: Map.Map EffectLabel Bool -> TypeU -> Set.Set EffectLabel
inescapableLeak declared ty =
  let (params, ret) = uncurryFun ty
      paramLabels = Set.unions (map collectEffLabels params)
      inescapable = Set.filter (\l -> Map.lookup l declared == Just False) paramLabels
   in Set.difference inescapable (collectEffLabels ret)
  where
    -- Peel quantifiers and uncurry to (all parameters, final result).
    uncurryFun (ForallU _ t) = uncurryFun t
    uncurryFun (FunU ps r) =
      let (ps', r') = uncurryFun r in (ps ++ ps', r')
    uncurryFun t = ([], t)

-- | The diagnostic for inescapable effects an argument performs that the
-- result does not carry (EFF-13).
leakMessage :: Set.Set EffectLabel -> MDoc
leakMessage leaked =
  "Inescapable effect"
    <> (if Set.size leaked > 1 then "s" else "")
    <+> hcat (punctuate ", " (map (squotes . pretty) (Set.toList leaked)))
    <+> "appear(s) in an argument but not in the result row."
    <+> "An inescapable effect performed via an argument must"
    <+> "propagate to the result (only a sourced handler may"
    <+> "discharge an escapable effect)."

-- | An instance's methods at the instance's types follow the rule
-- 'checkDeclaredEffects' holds signatures to: a class method whose argument
-- is a class variable takes, at an instance with an effect there, an
-- argument that performs the effect, which its result must then carry.
checkInstanceEffects :: MorlocMonad ()
checkInstanceEffects = do
  declared <- MM.gets stateEffects
  env <- MM.gets stateInstances
  sequence_
    [ throwAt (FI.methodTermSite t) $
        "At its instance of" <+> pretty (ikClass (mtKey t))
          <+> squotes (pretty (mtMethod t))
          <+> "has the type" <+> pretty (etype (mtType t)) <> "." <+> leakMessage leaked
    | t <- FI.allMethodTerms env
    , let leaked = inescapableLeak declared (etype (mtType t))
    , not (Set.null leaked)
    ]
  where
    -- an instance with no implementation has no index of its own
    throwAt :: Maybe Int -> MDoc -> MorlocMonad ()
    throwAt (Just i) msg = MM.throwSourcedError i msg
    throwAt Nothing msg = MM.throwRejected msg

-- | Reject any receiver head that has both a @PatternAccessible@
-- instance and a structural (@Indexable@ \/ @Sliceable@ \/
-- @SliceableDim@) instance. Both class families dispatch the same
-- pattern-accessor syntax (@.foo@, @.[i]@, @.[s:e:p]@); silently
-- preferring one at codegen would be a footgun. Force an explicit
-- choice at declaration time.
checkPatternAccessibleCoherence :: MorlocMonad ()
checkPatternAccessibleCoherence = do
  env <- MM.gets stateInstances
  let paHeads = collectReceiverHeads env patternAccessibleMethod
      structural = Map.unions (map (collectReceiverHeads env) structuralMethods)
      conflicts = Map.toList (Map.filterWithKey (\h _ -> Map.member h structural) paHeads)
  case conflicts of
    [] -> return ()
    ((h, paIdx) : _) ->
      MM.throwSourcedError paIdx $
        "Type" <+> squotes (pretty h)
          <+> "has a 'PatternAccessible' instance and also has an"
          <+> "'Indexable', 'Sliceable', or 'SliceableDim' instance."
          <+> "Both class families dispatch the pattern-accessor"
          <+> "syntax (.foo, .[i], .[s:e:p]) on the same receiver,"
          <+> "so silently preferring one would be ambiguous."
          <+> "Choose one: keep 'PatternAccessible" <+> pretty h <> "'"
          <+> "for a custom walker, or drop it and let"
          <+> "'Indexable' / 'Sliceable' provide structural dispatch."

-- | The receiver head of every instance implementing a method, with one
-- source index of the instance, for the caret of an error message.
collectReceiverHeads :: InstanceEnv -> ArgMethod -> Map.Map TVar Int
collectReceiverHeads env (ArgMethod cls method pos) =
  Map.fromList
    [ (h, i)
    | t <- FI.methodTerms env cls method
    , Just h <- [instanceArgHead pos (mtType t)]
    , Just i <- [FI.methodTermSite t]
    ]

linkAndRemoveAnnotations :: ExprI -> MorlocMonad ExprI
linkAndRemoveAnnotations = f
  where
    f :: ExprI -> MorlocMonad ExprI
    f (ExprI _ (AnnE e@(ExprI i _) t)) = do
      --     ^                ^-- this one is connected to the given types
      --     '-- this index disappears with the lost annotation node
      s <- MM.get
      MM.put $! s {stateAnnotations = Map.insert i t (stateAnnotations s)}
      f e -- notice the topology change
      -- everything below is boilerplate (this is why I need recursion schemes)
    f (ExprI i (ModE v es)) = ExprI i <$> (ModE v <$> mapM f es)
    f (ExprI i (IstE o c ctx ts es)) = ExprI i <$> (IstE o c ctx ts <$> mapM f es)
    f (ExprI i (AssE v e es)) = ExprI i <$> (AssE v <$> f e <*> mapM f es)
    f (ExprI i (LstE es)) = ExprI i <$> (LstE <$> mapM f es)
    f (ExprI i (TupE es)) = ExprI i <$> (TupE <$> mapM f es)
    f (ExprI i (NamE rs)) = ExprI i . NamE <$> mapM (traverse f) rs
    f (ExprI i (AppE e es)) = ExprI i <$> (AppE <$> f e <*> mapM f es)
    f (ExprI i (LamE vs e)) = ExprI i <$> (LamE vs <$> f e)
    f (ExprI i (LetE bindings body)) = ExprI i <$> (LetE <$> mapM (\(v, e) -> (,) v <$> f e) bindings <*> f body)
    f (ExprI i (IfE c t e)) = ExprI i <$> (IfE <$> f c <*> f t <*> f e)
    f (ExprI i (DoBlockE e)) = ExprI i <$> (DoBlockE <$> f e)
    f (ExprI i (EvalE e)) = ExprI i <$> (EvalE <$> f e)
    f (ExprI i (IntrinsicE intr es)) = ExprI i <$> (IntrinsicE intr <$> mapM f es)
    f e@(ExprI _ _) = return e

-- | Collect the tree of the term at index @gi@ named @v@. A reference to a
-- term in @only@ other than the root is left as its signature alone, a
-- @VarS v (MonomorphicExpr (Just sig) [])@, and a class method is left as its
-- instances' types unless it is in @inline@.
collectRoot :: Set.Set Int -> Set.Set Int -> (Int, EVar) -> MorlocMonad (AnnoS Int ManyPoly Int)
collectRoot only inline (gi, v) = do
  GMap idmap _ <- MM.gets stateSignatures
  let shallow = fromMaybe (-1) (Map.lookup gi idmap)
  snd <$> collect (Namer Map.empty 0 Map.empty Set.empty shallow (Just only) inline) (gi, v)

-- | The signed top-level definitions of a module: term id, name and
-- declaration index.
signedDefs :: GMap Int Int SignatureSet -> ExprI -> [(Int, EVar, Int)]
signedDefs (GMap idmap sigmap) (ExprI _ (ModE _ es)) =
  [ (termId, v, i)
  | ExprI i (AssE v _ _) <- es
  , Just termId <- [Map.lookup i idmap]
  , Just (Monomorphic t) <- [Map.lookup termId sigmap]
  , Just _ <- [termGeneral t]
  ]
signedDefs _ _ = []

-- | The top-level definitions of a module whose term has no signature and
-- no source: term id, name and declaration index.
unsignedDefs :: GMap Int Int SignatureSet -> ExprI -> [(Int, EVar, Int)]
unsignedDefs (GMap idmap sigmap) (ExprI _ (ModE _ es)) =
  [ (termId, v, i)
  | ExprI i (AssE v _ _) <- es
  , Just termId <- [Map.lookup i idmap]
  , Just (Monomorphic t) <- [Map.lookup termId sigmap]
  , Nothing <- [termGeneral t]
  , null (termConcrete t)
  ]
unsignedDefs _ _ = []

-- | The tree of a top-level term's definitions, collected in shallow mode
-- (see 'namerShallow'): every other signed term is referenced by its
-- signature.
definitionTree :: (Int, EVar, Int) -> MorlocMonad (AnnoS Int ManyPoly Int)
definitionTree (termId, v, i) = snd <$> collect (Namer Map.empty 0 Map.empty Set.empty termId Nothing Set.empty) (i, v)

-- | One entry per term, the first; a term's tree holds every definition of it.
firstPerTerm :: [(Int, EVar, Int)] -> [(Int, EVar, Int)]
firstPerTerm defs = Map.elems (Map.fromListWith (\_ a -> a) [(tid, x) | x@(tid, _, _) <- defs])

-- | The trees 'Morloc.Frontend.Typecheck.validate' checks: one per signed
-- term defined in any of the modules, but those in @inferred@, whose types
-- were checked where they were inferred, collected in shallow mode (see
-- 'namerShallow'), and one per method body of each instance the modules
-- declare, checked against the method's type at the instance's types with
-- the instance's context assumed.
contractTrees :: Set.Set Int -> [ExprI] -> MorlocMonad [AnnoS Int ManyPoly Int]
contractTrees inferred modules = do
  sigs@(GMap _ sigmap) <- MM.gets stateSignatures
  trees <- mapM definitionTree [d | d@(k, _, _) <- firstPerTerm (concatMap (signedDefs sigs) modules), Set.notMember k inferred]
  env <- MM.gets stateInstances
  let bodyType = Map.fromList
        [ (bodyIdx, (k, et))
        | (k, Polymorphic cls v _) <- Map.toList sigmap
        , t <- FI.methodTerms env cls v
        , (et, ExprI bodyIdx _) <- mtBodies t
        ]
  instanceTrees <- sequence
    [ instanceBodyTree Nothing k i v et ctx body
    | ExprI _ (ModE _ es) <- modules
    , ExprI _ (IstE _ _ ctx _ ies) <- es
    , ExprI i (AssE v body@(ExprI bi _) _) <- ies
    , Just (k, et) <- [Map.lookup bi bodyType]
    ]
  return (trees <> instanceTrees)

-- | An instance's body of a method as a definition checked against the
-- method's type at the instance's types, its body assuming the instance's
-- context: @k@ is the method's term, @i@ the binding's index. With @only@,
-- only those terms are referenced by their signatures and every other is
-- expanded, as 'collectRoot' does.
instanceBodyTree :: Maybe (Set.Set Int) -> Int -> Int -> EVar -> EType -> [Constraint] -> ExprI -> MorlocMonad (AnnoS Int ManyPoly Int)
instanceBodyTree only k i v et ctx body = do
  (_, body') <- collectAnnoS (Namer Map.empty 0 Map.empty Set.empty k only Set.empty) body
  let et' = et {econs = Set.union (econs et) (Set.fromList ctx)}
  return (AnnoS i i (VarS v (MonomorphicExpr (Just et') [body'])))

-- | Each instance method body of the modules, by its expression's index:
-- the binding's index and the context of its instance declaration.
instanceBodySites :: [ExprI] -> Map.Map Int (Int, [Constraint])
instanceBodySites modules =
  Map.fromList
    [ (bi, (i, ctx))
    | ExprI _ (ModE _ es) <- modules
    , ExprI _ (IstE _ _ ctx _ ies) <- es
    , ExprI i (AssE _ (ExprI bi _) _) <- ies
    ]

-- | A class constraint names the type it constrains: @Ord a =>@, never a
-- bare @Ord =>@.
checkBareClassConstraints :: ExprI -> MorlocMonad ()
checkBareClassConstraints = AST.checkExprI check
  where
    check :: ExprI -> MorlocMonad ()
    check (ExprI i (SigE (Signature v _ et))) =
      case [cls | Constraint cls [] <- Set.toList (econs et)] of
        [] -> return ()
        (cls : _) ->
          MM.throwSourcedError i $
            "The constraint" <+> squotes (pretty cls) <+> "in the signature of"
              <+> squotes (pretty v)
              <+> "names a class but not the type it constrains; write, for example,"
              <+> squotes (pretty cls <+> "a =>") <> "."
    check _ = return ()

{- | Build the call tree for a single nexus command. The result is ambiguous,
with 1 or more possible tree topologies, each with one or more possible
implementations for each function.

Rewrite all lambda-bound variables to the unique names
"<name>@<index>". Where "<name>" is the original name and "<index>" is an
auto-incrementing integer. This solves naming conflicts while avoiding
excessive traversal of the tree.

Recursion is handled via namerExpanding: when a function is being expanded,
recursive references to it emit CallS back-edge nodes instead of re-expanding.
-}
collect ::
  Namer ->
  ( Int -- the general index for the term
  , EVar -- name of root expression
  ) ->
  MorlocMonad (Namer, AnnoS Int ManyPoly Int)
collect namer0 (gi, v) = do
  MM.sayVVV $
    "collect"
      <> "\n  gi:" <+> pretty gi
      <> "\n  v:" <+> pretty v
  (namer, e) <- collectExprS namer0 (ExprI gi (VarE defaultValue v))
  return (namer, AnnoS gi gi e)

collectAnnoS :: Namer -> ExprI -> MorlocMonad (Namer, AnnoS Int ManyPoly Int)
collectAnnoS namer e@(ExprI gi _) = collectExprS namer e |>> second (AnnoS gi gi)

-- | The shape a pattern skeleton written by Desugar stands for.
skeletonShape :: ExprI -> Maybe PatShape
skeletonShape (ExprI i e) = case e of
  UniE -> Just ShapeAny
  TupE xs -> Shape i . FormTuple <$> mapM skeletonShape xs
  NamE rs -> Shape i . FormRecord <$> mapM (\(k, x) -> (,) k <$> skeletonShape x) rs
  _ -> Nothing

-- | Translate ExprI to ExprS tree
collectExprS :: Namer -> ExprI -> MorlocMonad (Namer, ExprS Int ManyPoly Int)
collectExprS namer0 (ExprI gi0 e0) = f namer0 e0
  where
    -- A labeled reference is the reference wrapped in a label whose
    -- configuration moves to a key no node carries.
    f namer (VarE cfg v)
      | Just _ <- manifoldConfigLabel cfg = do
          (namer', e) <- f namer (VarE defaultValue v)
          inner <- newIndex gi0
          k <- MM.getCounter
          MM.modify' $ \st ->
            st { stateManifoldConfig = Map.insert k cfg {manifoldConfigLabelIdx = Just gi0} (stateManifoldConfig st)
               , stateName = Map.insertWith (\_ old -> old) gi0 (displayName v) (stateName st)
               }
          return (namer', LabelS k (AnnoS inner inner e))
    f namer (VarE _ v) = do
      MM.sayVVV $
        "collectExprS VarE"
          <> "\n  gi:" <+> pretty gi0
          <> "\n  v:" <+> pretty v
      sigs@(GMap idmap _) <- MM.gets stateSignatures
      let termId = Map.lookup gi0 idmap
      case (termId, GMap.lookup gi0 sigs) of
        -- Shallow collection: a class method is left as its instances'
        -- types. For specialization it is elaborated at the use's type; a
        -- use of a method inside its own expansion is not a back-edge, since
        -- it may resolve to another instance. Validation checks a method use as an
        -- obligation and never reads its instances' bodies (they are
        -- checked where the use is elaborated), so it never expands one.
        (Just k, GMapJust (Polymorphic cls clsName t))
          | Nothing <- Map.lookup v (namerMap namer)
          , isNothing (namerShallowOnly namer) || k /= namerShallow namer || Map.member k (namerExpanding namer)
          , Set.notMember k (namerInline namer) || Map.member k (namerExpanding namer) -> do
              env <- MM.gets stateInstances
              return (namer, VarS v (PolymorphicExpr cls clsName t [(mtKey x, mtType x, []) | x <- FI.methodTerms env cls clsName]))
        -- A reference to a term that is being expanded is a recursive
        -- back-edge. It is recognized by the term's identity, not its name:
        -- two unrelated terms may share a name.
        (Just k, _)
          | Just token <- Map.lookup k (namerExpanding namer)
          , Nothing <- Map.lookup v (namerMap namer) -> do
              MM.sayVVV $ "collectExprS: recursive call to" <+> pretty v
              return (namer {namerRecursive = Set.insert k (namerRecursive namer)}, CallS token)
        -- Checking a definition against its signature: another signed
        -- top-level term is referenced by its signature, not expanded (it is
        -- checked against its signature on its own).
        (Just k, GMapJust (Monomorphic t))
          | namerShallow namer /= k
          , maybe True (Set.member k) (namerShallowOnly namer)
          , Just _ <- termGeneral t
          , not (MT.isInfixOf "`" (unEVar v)) ->
              return (namer, VarS v (MonomorphicExpr (termGeneral t) []))
        -- A monomorphic term will have a type if it is linked to any source
        -- since sources require signatures. But if it associated only with a
        -- declaration, then it will have no type.
        (Just k, GMapJust (Monomorphic t)) -> do
          MM.sayVVV $ "  monomorphic term" <+> pretty v <> ":" <+> maybe "?" pretty (termGeneral t)
          (namer', es) <- expanding k namer (\n -> implsToAnnoS gi0 n (map snd (termConcrete t)) (termDecl t))
          v' <- nameIfRecursive namer' k
          return (namer', VarS v' (MonomorphicExpr (termGeneral t) es))
        -- A polymorphic term should always have a type.
        (Just k, GMapJust (Polymorphic cls clsName t)) -> do
          env <- MM.gets stateInstances
          let ts = FI.methodTerms env cls clsName
              typed n' x = second ((,,) (mtKey x) (mtType x)) <$> implsToAnnoS gi0 n' (map snd (mtSources x)) (map snd (mtBodies x))
          MM.sayVVV $ "  polymorphic term" <+> pretty v <> ":" <+> list (map (pretty . mtType) ts)
          (namer', ess) <- expanding k namer (\n -> statefulMapM typed n ts)
          v' <- nameIfRecursive namer' k
          return (namer', VarS v' (PolymorphicExpr cls clsName t ess))
        -- Terms not associated with TermTypes objects must be lambda-bound or let-bound
        -- These terms will be renamed for uniqueness
        _ -> do
          MM.sayVVV $ "bound term" <+> pretty v
          case Map.lookup v (namerMap namer) of
            (Just (v', LambdaBound)) -> return (namer, BndS v')
            (Just (v', LetBound)) -> return (namer, LetBndS v')
            Nothing -> MM.throwSourcedError gi0 $ "Undefined term in namer map:" <+> pretty v
      where
        -- Expand term k's alternatives with k marked as in progress, so a
        -- reference back to it becomes a 'CallS'. On return, k's own mark
        -- in 'namerRecursive' is kept only long enough to name its 'VarS'.
        -- The back-edges' name is fixed here, from this reference, so it
        -- matches the 'VarS' however the term is named at the back-edge (an
        -- import alias, a namespace).
        expanding k n0 act = do
          let n1 = n0 { namerExpanding = Map.insert k (recName v k) (namerExpanding n0)
                      , namerRecursive = Set.delete k (namerRecursive n0) }
          (n2, x) <- act n1
          let n3 = n2 { namerExpanding = namerExpanding n0 }
          return (n3, x)
        -- A recursive term and its back-edges share one name that is unique to
        -- the term, so Typecheck and Realize can match them by name.
        nameIfRecursive :: Namer -> Int -> MorlocMonad EVar
        nameIfRecursive n k
          | Set.member k (namerRecursive n) = do
              let token = recName v k
              -- Only an export keeps its own name in 'stateName' (it is the
              -- command name), so only an export's back-edges are resolved
              -- through this map; every other recursive term is hoisted and
              -- renamed by Realize.
              exports <- MM.gets stateExports
              when (gi0 `elem` exports) $
                MM.modify' (\st -> st {stateRecursionTargets = Map.insert token gi0 (stateRecursionTargets st)})
              return token
          | otherwise = return v

        implsToAnnoS :: Int -> Namer -> [Indexed Source] -> [ExprI] -> MorlocMonad (Namer, [AnnoS Int ManyPoly Int])
        implsToAnnoS gi n srcs bodies = do
          let calls = [AnnoS gi ci (ExeS (SrcCall isrc)) | isrc@(Idx ci _) <- srcs]

          (n', declarations) <- statefulMapM termExprToAnnoS n bodies
          return (n', (calls <> declarations))

        termExprToAnnoS :: Namer -> ExprI -> MorlocMonad (Namer, AnnoS Int ManyPoly Int)
        termExprToAnnoS n e = do
          -- Capture ci AFTER reindexExprI: each inlining of a top-level term
          -- needs its own concrete index so downstream passes (e.g. Express's
          -- LetS handler that falls back to cidx as a let-binding id when the
          -- bound variable is unused) don't collide across inlinings.
          e2@(ExprI ci _) <- reindexExprI e
          (n', e') <- collectExprS n e2
          return (n', AnnoS gi0 ci e')
    f namer (LstE es) = statefulMapM collectAnnoS namer es |>> second LstS
    f namer (TupE es) = statefulMapM collectAnnoS namer es |>> second TupS
    f namer (NamE rs) = do
      (namer', rs') <- statefulMapM (\n (key, x) -> second ((,) key) <$> collectAnnoS n x) namer rs
      return (namer', NamS rs')
    f namer (LamE vs e) = do
      let namer' = foldr (updateRenamer LambdaBound) namer vs
          vs' = map (fst . fromJust . (flip Map.lookup) (namerMap namer')) vs
      (inner, e') <- collectAnnoS namer' e
      -- return the original namer, the lambda bound terms are defined only
      -- below; only the back-edges found inside and the renaming counter
      -- escape the scope
      return (namer {namerRecursive = namerRecursive inner, namerIndex = namerIndex inner}, LamS vs' e')
    f namer (LetE ((v, e1) : rest) body) = do
      (namer1, e1') <- collectAnnoS namer e1
      let namer2 = updateRenamer LetBound v namer1
          v' = fst $ fromJust $ Map.lookup v (namerMap namer2)
      -- Each remaining binding is a let node of its own, so it takes an
      -- index of its own, placed at that binding: nodes sharing an index
      -- share whatever is keyed by it, such as the name of the manifold a
      -- node is emitted as.
      innerBody <- case rest of
        [] -> return body
        ((_, ExprI k _) : _) -> do
          j <- MM.getCounterWithPos k
          return (ExprI j (LetE rest body))
      (inner, body') <- collectAnnoS namer2 innerBody
      return (namer {namerRecursive = namerRecursive inner, namerIndex = namerIndex inner}, LetS v' e1' body')
    f _ (LetE [] _) = MM.throwCompilerBugAt gi0 "a let with no bindings reached Treeify"
    f namer (AppE (ExprI hi (PatE p)) es)
      | null es && patternArity p > 0 = etaPattern namer hi p
      | otherwise = do
          (namer', es') <- statefulMapM collectAnnoS namer es
          case applyPattern p es' of
            Just app -> return (namer', PatS hi hi noPatEvidence app)
            Nothing ->
              MM.throwSourcedError gi0 $
                "This pattern takes" <+> pretty (patternArity p) <+> "arguments, got" <+> pretty (length es)
    f namer (AppE e es) = do
      (namer', e') <- collectAnnoS namer e
      (namer'', es') <- statefulMapM collectAnnoS namer' es
      return (namer'', AppS e' es')
    f namer UniE = return (namer, UniS)
    f namer NullE = return (namer, NullS)
    -- gi0 is the literal's own ExprI index (its source-map entry was
    -- populated by Desugar.freshIdSpan and is preserved across reindex
    -- via copyState). The wrapping AnnoS uses a different index when
    -- this literal is inlined as a top-level term body (Treeify.hs:248
    -- termExprToAnnoS), so carry the literal's own index explicitly.
    f namer (RealE x) = return (namer, RealS gi0 x)
    f namer (IntE x) = return (namer, IntS gi0 x)
    f namer (LogE x) = return (namer, LogS x)
    f namer (StrE x) = return (namer, StrS x)
    f namer (ConE tv n i xs) =
      statefulMapM collectAnnoS namer xs |>> second (ConS tv n i)
    f namer (PatE p) = do
      hi <- MM.getCounterWithPos gi0
      etaPattern namer hi p
    f namer (DoBlockE e) = do
      (namer', e') <- collectAnnoS namer e
      return (namer', DoBlockS e')
    f namer (EvalE e) = do
      (namer', e') <- collectAnnoS namer e
      return (namer', EvalS e')
    f namer (IntrinsicE IntrShape es) = case es of
      [sk, r] | Just sh <- skeletonShape sk -> do
        (namer1, r') <- collectAnnoS namer r
        return (namer1, ShapeS PreTyped sh r')
      _ -> MM.throwCompilerBugAt gi0 "malformed shape intrinsic"
    f namer (IntrinsicE IntrMap es@[_, _]) = do
      (namer1, es') <- statefulMapM collectAnnoS namer es
      GMap idmap sigmap <- MM.gets stateSignatures
      case [k | (k, Polymorphic cls v _) <- Map.toList sigmap, cls == ClassName "Functor", v == EV "map"] of
        [] -> return (namer1, IntrinsicS IntrMap es')
        (k : _) -> do
          i <- MM.getCounterWithPos gi0
          MM.modify' (\st -> st {stateSignatures = GMap (Map.insert i k idmap) sigmap})
          (namer2, use) <- collectExprS namer1 {namerMap = Map.delete (EV "map") (namerMap namer1)} (ExprI i (VarE defaultValue (EV "map")))
          return (namer2 {namerMap = namerMap namer1}, IntrinsicS IntrMap (es' <> [AnnoS i i use]))
    f namer (IntrinsicE intr es) = do
      (namer', es') <- go namer [] es
      return (namer', IntrinsicS intr es')
      where
        go n acc [] = return (n, reverse acc)
        go n acc (x:xs) = do
          (n', x') <- collectAnnoS n x
          go n' (x':acc) xs
    f namer (IfE c t e) = do
      (namer1, c') <- collectAnnoS namer c
      (namer2, t') <- collectAnnoS namer1 t
      (namer3, e') <- collectAnnoS namer2 e
      return (namer3, IfS c' t' e')
    -- all other expressions are strictly illegal here and represent compiler bugs
    f _ e = error $ "Bug in collectExprS: " <> show (render (pretty e))

    -- A pattern used as a function: a lambda over the arguments it takes.
    etaPattern namer hi p = do
      vs <- replicateM (patternArity p) (MM.getCounterWithPos gi0)
      let name i = EV (".pat_" <> MT.show' i)
          vars = [ExprI i (VarE defaultValue (name i)) | i <- vs]
      appI <- MM.getCounterWithPos gi0
      f namer (LamE (map name vs) (ExprI appI (AppE (ExprI hi (PatE p)) vars)))

-- | The name shared by a recursive term's 'VarS' and its 'CallS' back-edges.
-- The @`t@ segment is removed by 'Morloc.Frontend.Rename.displayName'.
recName :: EVar -> Int -> EVar
recName v k = EV (unEVar v <> "`t" <> MT.show' k)

updateRenamer :: BindKind -> EVar -> Namer -> Namer
updateRenamer kind v namer =
  let v' = EV (unEVar v <> "@" <> MT.show' (namerIndex namer))
   in namer
        { namerMap = Map.insert v (v', kind) (namerMap namer)
        , namerIndex = namerIndex namer + 1
        }

exprIIdx :: ExprI -> Int
exprIIdx (ExprI i _) = i

reindexExprI :: ExprI -> MorlocMonad ExprI
reindexExprI (ExprI i e) = ExprI <$> newIndex i <*> reindexExpr e

reindexExpr :: Expr -> MorlocMonad Expr
reindexExpr (ModE m es) = ModE m <$> mapM reindexExprI es
reindexExpr (AnnE e ts) = AnnE <$> reindexExprI e <*> pure ts
reindexExpr (AppE e es) = AppE <$> reindexExprI e <*> mapM reindexExprI es
reindexExpr (AssE v e es) = AssE v <$> reindexExprI e <*> mapM reindexExprI es
reindexExpr (LamE vs e) = LamE vs <$> reindexExprI e
reindexExpr (LstE es) = LstE <$> mapM reindexExprI es
reindexExpr (NamE rs) = NamE <$> mapM (\(k, e) -> (,) k <$> reindexExprI e) rs
reindexExpr (TupE es) = TupE <$> mapM reindexExprI es
reindexExpr (LetE bindings body) = LetE <$> mapM (\(v, e) -> (,) v <$> reindexExprI e) bindings <*> reindexExprI body
reindexExpr (IfE c t e) = IfE <$> reindexExprI c <*> reindexExprI t <*> reindexExprI e
reindexExpr (DoBlockE e) = DoBlockE <$> reindexExprI e
reindexExpr (EvalE e) = EvalE <$> reindexExprI e
reindexExpr (IntrinsicE intr es) = IntrinsicE intr <$> mapM reindexExprI es
reindexExpr e = return e
