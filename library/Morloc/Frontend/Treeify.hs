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
  , collectRoot
  , recName
  ) where

import qualified Data.Set as Set
import qualified Morloc.BaseTypes as BT
import qualified Morloc.Data.DAG as DAG
import Morloc.Data.Doc
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.Data.Map as Map
import qualified Morloc.Data.Text as MT
import qualified Morloc.Frontend.AST as AST
import qualified Morloc.Frontend.Link as MFL
import Morloc.Frontend.Rename (renameLocals)
import Morloc.Frontend.Namespace
import qualified Morloc.Monad as MM
import Morloc.Typecheck.Internal (collectEffLabels, unqualify)
import qualified Morloc.DataFiles as DF
import qualified Morloc.Language as ML
import Morloc.CodeGenerator.LanguageDescriptor
  (loadLangDescriptorFromText, ldNamePattern, ldOperatorPattern, matchNamePattern)

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
  , namerShallow :: Maybe Int  -- when collecting a definition to check against its signature: its term id
  , namerShallowOnly :: Maybe (Set.Set Int)
  -- ^ with 'namerShallow': only these terms are referenced by signature;
  -- every other term is expanded
  }
  deriving (Show)

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
-- | What 'Morloc.Frontend.Typecheck.validate' checks: each signed top-level
-- definition of the root module as a tree of its own body, and the root
-- module's instance declarations.
data Validation = Validation
  { validationTrees :: [AnnoS Int ManyPoly Int]
  , validationInstances :: [(Int, ClassName, [TypeU])]
  , validationDefs :: Map.Map Int (Int, EVar)
  -- ^ each checked definition by term id: its declaration index and name
  }

treeify ::
  DAG MVar [AliasedSymbol] ExprI ->
  MorlocMonad (Validation, [(Int, EVar)])
treeify d
  | Map.size d == 0 = return (Validation [] [] Map.empty, [])
  | otherwise = case DAG.roots d of
      -- if no parentless element exists, then the graph must be empty or cyclic
      [] -> MM.throwSystemError "cyclic import dependency in treeify"
      -- else if exactly one module name key (k) is found
      [k] -> do
        case DAG.lookupNode k d of
          -- if the key is not in the DAG, then something is dreadfully wrong codewise
          Nothing -> MM.throwCompilerBug $ "module DAG is missing key" <+> pretty k
          (Just (AST.findExport -> ExportMany symbols groups)) -> do
            d' <- DAG.mapNodeM (linkAndRemoveAnnotations >=> renameLocals) d

            -- move all to state, after this the DAG will no longer be needed
            _ <- MFL.link d'

            -- every effect label used in a `<...>` row must be declared
            -- (the compiler hardcodes none); checked after link populates
            -- the effect registry
            checkDeclaredEffects d'

            -- every sourced symbol name must match the foreign language's
            -- name pattern (declared in lang.yaml, never hardcoded here);
            -- the name is emitted verbatim, so code in this position would
            -- be injected into the pool
            checkSourceNames d'

            -- PatternAccessible dispatches the pattern-accessor syntax
            -- (`.foo`, `.[i]`, `.[s:e:p]`) at codegen; Indexable /
            -- Sliceable / SliceableDim dispatch the same syntax
            -- structurally. Declaring both for the same head is
            -- ambiguous.
            checkPatternAccessibleCoherence

            -- find all term exports (ungrouped + grouped)
            let allSymbols = Set.unions (symbols : [exportGroupMembers g | g <- groups])
                exports = [(i, v) | (i, TermSymbol v) <- Set.toList allSymbols]
                -- Root the terminal-action handlers of every command the root
                -- exports. When a formatter command is defined in a submodule
                -- and imported by name, its handlers are exported by that
                -- submodule, not the root, so they must be rooted explicitly or
                -- codegen prunes them (see the collect-cross-module golden). A
                -- name already in `exports` (the self-contained case, where
                -- `addToExport` put the handler in the root's own exports) is
                -- skipped to avoid a duplicate root.
                exportedNames = Set.fromList [v | (_, v) <- exports]
                terminalExports =
                  [ e
                  | e@(_, m) <- terminalActionRoots d exportedNames
                  , not (Set.member m exportedNames)
                  ]
                exports' = exports ++ terminalExports

            -- Build export group info for the state
            let exportGroupInfo =
                  Map.fromList
                    [ ( exportGroupName g
                      , (exportGroupDesc g, [i | (i, TermSymbol _) <- Set.toList (exportGroupMembers g)])
                      )
                    | g <- groups
                    ]

            -- Validate command groups
            let ungroupedNames = Set.fromList [v | (_, TermSymbol v) <- Set.toList symbols]
                groupNames = Set.fromList [exportGroupName g | g <- groups]
                collisions = Set.intersection (Set.map unEVar ungroupedNames) groupNames
            -- group names must not collide with ungrouped command names
            if not (Set.null collisions)
              then
                MM.throwSystemError $
                  "Command group names collide with ungrouped command names:"
                    <+> list (map pretty (Set.toList collisions))
              else return ()

            -- - store all exported indices in state
            -- - Add the export name to state. Failing to do so here, will lose
            --   the name of terms that are exported but not defined, this leads
            --   to cryptic error messages.
            MM.modify
              ( \s ->
                  s
                    { stateExports = map fst exports'
                    , stateName = Map.union (stateName s) (Map.fromList exports')
                    , stateExportGroups = exportGroupInfo
                    }
              )

            (checks, instances, defs) <- case DAG.lookupNode k d' of
              Just node -> do
                checkBareClassConstraints node
                (cs, ds) <- contractRoots node
                return (cs, rootInstances node, ds)
              Nothing -> return ([], [], Map.empty)
            return (Validation checks instances defs, exports')
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

-- | The terminal-action handler bindings (`--' with:`/`render:` synthesis, e.g.
-- @mlcp_cut_fasta@) for every command the root module exports.
--
-- Program roots come only from the root module's export list, but a
-- terminal-action binding is exported by the module that DEFINES its parent
-- command (via 'Desugar.addToExport'), not by the root. So when a formatter
-- command is imported by name from a submodule, its handlers are not among the
-- root's exports and codegen prunes them -- the flag still appears in the nexus
-- (its terminal record rides the command's docstring across the import) but
-- dispatches to a manifold that was never lowered. Rooting these entries here
-- makes the imported case behave exactly like a self-contained root module.
--
-- The mangled names are re-derived from each root command's @with@/@render@
-- docstrings (the same source 'Desugar.collectWithSpecs' and the nexus use) and
-- resolved against every module's export symbols to recover the linked index
-- ('MFL.link' links all modules' exports, so a submodule's terminal-action
-- export index is a valid root). A command re-exported under an alias is not
-- handled (its submodule signature name would not match the root export name);
-- that matches the nexus, which also keys the terminal entry off the command's
-- own name.
terminalActionRoots :: DAG MVar e ExprI -> Set.Set EVar -> [(Int, EVar)]
terminalActionRoots d rootCmds =
  [ (i, m)
  | m <- Set.toList mangledNames
  , Just i <- [Map.lookup m exportIndex]
  ]
  where
    ns = DAG.nodes d
    mangledNames =
      Set.fromList $ concat
        [ terminals ++ parseEntries
        | node <- ns
        , (name, _, et) <- AST.findSignatures node
        , Set.member name rootCmds
        , ArgDocSig cmdDoc argDocs _ <- [edocs et]
        , let terminals = [ mangleTerminalName name (wsLong w) | w <- docWith cmdDoc ]
              parseEntries = map parseEntryName (parseEntryTargets name cmdDoc argDocs)
        ]
    exportIndex =
      Map.fromList
        [ (v, i)
        | node <- ns
        , (i, v) <- moduleExportTerms node
        ]
    moduleExportTerms node = case AST.findExport node of
      ExportMany symbols groups ->
        let allSs = Set.toList symbols ++ concatMap (Set.toList . exportGroupMembers) groups
         in [(i, v) | (i, TermSymbol v) <- allSs]
      _ -> []

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
-- | Reject a sourced symbol whose name does not match its language's
-- identifier or operator pattern. Both patterns live only in lang.yaml
-- (ldNamePattern / ldOperatorPattern); the compiler hardcodes no
-- foreign-language naming rule. Operator vs identifier is decided by the
-- symbol itself (srcOperator), so "foo" and "+" are valid but "foo+"
-- matches neither. The name is emitted verbatim into the pool, so a
-- non-name here is a code-injection vector. An empty pattern, or no
-- embedded descriptor, is not checked (fail open).
checkSourceNames :: DAG MVar [AliasedSymbol] ExprI -> MorlocMonad ()
checkSourceNames d = mapM_ (AST.checkExprI checkNode) (DAG.nodes d)
  where
    checkNode :: ExprI -> MorlocMonad ()
    -- Backtick-quoted names bypass the language-descriptor patterns
    -- entirely: they are the user's explicit "trust me, emit this string
    -- verbatim as an infix operator" opt-out for keyword-shaped foreign
    -- operators (Python `and`, R `%in%`, etc.).
    checkNode (ExprI _ (SrcE src)) | srcBacktick src = return ()
    checkNode (ExprI i (SrcE src)) =
      let lang = ML.langName (srcLang src)
          nm = unSrcName (srcName src)
       in case lookup
            (MT.unpack lang)
            [(n, DF.embededFileText ef) | (n, ef) <- DF.langRegistryFiles] of
            Just yamlText ->
              case loadLangDescriptorFromText yamlText of
                Right desc ->
                  let isOp = srcOperator src
                      pat = if isOp then ldOperatorPattern desc else ldNamePattern desc
                      kind = if isOp then "operator" else "name"
                   in if MT.null pat || matchNamePattern pat nm
                        then return ()
                        else
                          MM.throwSourcedError i $
                            "Invalid"
                              <+> pretty lang
                              <+> "source"
                              <+> kind
                              <+> squotes (pretty nm) <> "."
                              <+> "A sourced symbol is emitted verbatim, so it"
                              <+> "must be a valid"
                              <+> pretty lang
                              <+> kind <> ", not code."
                              <+> "Expected pattern"
                              <+> squotes (pretty pat) <> "."
                Left _ -> return ()
            Nothing -> return ()
    checkNode _ = return ()

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
          used = Set.unions (map collectEffLabels tys)
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
          let leaked = Set.unions (map inescapableLeak tys)
          if Set.null leaked
            then return ()
            else
              MM.throwSourcedError i $
                "Inescapable effect"
                  <> (if Set.size leaked > 1 then "s" else "")
                  <+> hcat
                    (punctuate ", " (map (squotes . pretty) (Set.toList leaked)))
                  <+> "appear(s) in an argument but not in the result row."
                  <+> "An inescapable effect performed via an argument must"
                  <+> "propagate to the result (only a sourced handler may"
                  <+> "discharge an escapable effect)."
      where
        -- Inescapable concrete labels in any parameter row that are
        -- absent from the result row.
        inescapableLeak ty =
          let (params, ret) = uncurryFun ty
              paramLabels = Set.unions (map collectEffLabels params)
              inescapable =
                Set.filter (\l -> Map.lookup l declared == Just False) paramLabels
           in Set.difference inescapable (collectEffLabels ret)

        -- Peel quantifiers and uncurry to (all parameters, final result).
        uncurryFun (ForallU _ t) = uncurryFun t
        uncurryFun (FunU ps r) =
          let (ps', r') = uncurryFun r in (ps ++ ps', r')
        uncurryFun t = ([], t)

-- | Reject any receiver head that has both a @PatternAccessible@
-- instance and a structural (@Indexable@ \/ @Sliceable@ \/
-- @SliceableDim@) instance. Both class families dispatch the same
-- pattern-accessor syntax (@.foo@, @.[i]@, @.[s:e:p]@); silently
-- preferring one at codegen would be a footgun. Force an explicit
-- choice at declaration time.
checkPatternAccessibleCoherence :: MorlocMonad ()
checkPatternAccessibleCoherence = do
  paHeads <- collectReceiverHeads (EV BT.extractPatternMethod) 2
  -- Fast path: most compiles declare no 'PatternAccessible' instance,
  -- so skip the structural-class scan entirely.
  if Map.null paHeads
    then return ()
    else do
      ixHeads <- collectReceiverHeads (EV "__access_index__") 1
      slHeads <- collectReceiverHeads (EV "__get_slice__") 3
      sdHeads <- collectReceiverHeads (EV "__get_slice_dim__") 3
      let structural = Map.unions [ixHeads, slHeads, sdHeads]
          conflicts  = [ (h, paIdx)
                       | (h, paIdx) <- Map.toList paHeads
                       , Map.member h structural
                       ]
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

-- | Collect the receiver's outermost 'TVar' from every 'TermTypes'
-- entry of a class method's instance list, keyed on that TVar. The
-- 'pos' parameter is the argument index the receiver occupies in the
-- method's function type. The 'Int' value is one representative
-- source index for the instance (used as the caret in error
-- messages). Sourced instances (@source Py from ...@) carry the
-- index in 'termConcrete'; body-declared instances (@__method__ =
-- someExpr@) carry it in 'termDecl'.
collectReceiverHeads :: EVar -> Int -> MorlocMonad (Map.Map TVar Int)
collectReceiverHeads method pos = do
  sigmap <- MM.gets stateTypeclasses
  case Map.lookup method sigmap of
    Nothing -> return Map.empty
    Just inst -> return $ Map.fromList
      [ (h, i)
      | tt@(TermTypes (Just et) _ _) <- instanceTerms inst
      , (h : _) <- [receiverHeads et]
      , Just i <- [instanceIndex tt]
      ]
  where
    receiverHeads :: EType -> [TVar]
    receiverHeads et = case snd (unqualify (etype et)) of
      FunU args _ | length args > pos -> [extractKey (args !! pos)]
      _                               -> []

    instanceIndex :: TermTypes -> Maybe Int
    instanceIndex (TermTypes _ ((_, Idx ix _) : _) _) = Just ix
    instanceIndex (TermTypes _ _ (ExprI ix _ : _))    = Just ix
    instanceIndex _                                    = Nothing

linkAndRemoveAnnotations :: ExprI -> MorlocMonad ExprI
linkAndRemoveAnnotations = f
  where
    f :: ExprI -> MorlocMonad ExprI
    f (ExprI _ (AnnE e@(ExprI i _) t)) = do
      --     ^                ^-- this one is connected to the given types
      --     '-- this index disappears with the lost annotation node
      s <- MM.get
      MM.put $ s {stateAnnotations = Map.insert i t (stateAnnotations s)}
      f e -- notice the topology change
      -- everything below is boilerplate (this is why I need recursion schemes)
    f (ExprI i (ModE v es)) = ExprI i <$> (ModE v <$> mapM f es)
    f (ExprI i (AssE v e es)) = ExprI i <$> (AssE v <$> f e <*> mapM f es)
    f (ExprI i (LstE es)) = ExprI i <$> (LstE <$> mapM f es)
    f (ExprI i (TupE es)) = ExprI i <$> (TupE <$> mapM f es)
    f (ExprI i (NamE rs)) = do
      es' <- mapM (f . snd) rs
      return . ExprI i $ NamE (zip (map fst rs) es')
    f (ExprI i (AppE e es)) = ExprI i <$> (AppE <$> f e <*> mapM f es)
    f (ExprI i (LamE vs e)) = ExprI i <$> (LamE vs <$> f e)
    f (ExprI i (LetE bindings body)) = ExprI i <$> (LetE <$> mapM (\(v, e) -> (,) v <$> f e) bindings <*> f body)
    f (ExprI i (IfE c t e)) = ExprI i <$> (IfE <$> f c <*> f t <*> f e)
    f (ExprI i (DoBlockE e)) = ExprI i <$> (DoBlockE <$> f e)
    f (ExprI i (EvalE e)) = ExprI i <$> (EvalE <$> f e)
    f (ExprI i (IntrinsicE intr es)) = ExprI i <$> (IntrinsicE intr <$> mapM f es)
    f e@(ExprI _ _) = return e

-- | Collect the tree of the term at index @gi@ named @v@. With @Nothing@
-- every term it reaches is expanded. With @Just ts@ a reference to a term in
-- @ts@ other than the root is left as its signature alone, a
-- @VarS v (MonomorphicExpr (Just sig) [])@.
collectRoot :: Maybe (Set.Set Int) -> (Int, EVar) -> MorlocMonad (AnnoS Int ManyPoly Int)
collectRoot only (gi, v) = do
  GMap idmap _ <- MM.gets stateSignatures
  let shallow = case only of
        Nothing -> Nothing
        Just _ -> Just (fromMaybe (-1) (Map.lookup gi idmap))
  snd <$> collect (Namer Map.empty 0 Map.empty Set.empty shallow only) (gi, v)

-- | One tree per signed top-level definition of the root module, collected
-- in shallow mode (see 'namerShallow'), and each such definition's
-- declaration index and name by term id.
contractRoots :: ExprI -> MorlocMonad ([AnnoS Int ManyPoly Int], Map.Map Int (Int, EVar))
contractRoots (ExprI _ (ModE _ es)) = do
  GMap idmap sigmap <- MM.gets stateSignatures
  let defs =
          [ (termId, v, i)
          | ExprI i (AssE v _ _) <- es
          , Just termId <- [Map.lookup i idmap]
          , Just (Monomorphic t) <- [Map.lookup termId sigmap]
          , Just _ <- [termGeneral t]
          ]
      firstPerTerm = Map.elems (Map.fromListWith (\_ a -> a) [(tid, x) | x@(tid, _, _) <- defs])
  trees <- mapM
    (\(termId, v, i) -> snd <$> collect (Namer Map.empty 0 Map.empty Set.empty (Just termId) Nothing) (i, v))
    firstPerTerm
  -- each method an instance of the module defines, checked against the
  -- method's type at the instance's types, its body assuming the instance's
  -- context
  let methodTypes =
        [ (bodyIdx, (k, et))
        | (k, Polymorphic _ _ _ tts) <- Map.toList sigmap
        , tt <- tts
        , Just et <- [termGeneral tt]
        , ExprI bodyIdx _ <- termDecl tt
        ]
      bodyType = Map.fromList methodTypes
  instanceTrees <- sequence
    [ do
        (_, body') <- collectAnnoS (Namer Map.empty 0 Map.empty Set.empty (Just k) Nothing) body
        let et' = et {econs = Set.union (econs et) (Set.fromList ctx)}
        return (AnnoS i i (VarS v (MonomorphicExpr (Just et') [body'])))
    | ExprI _ (IstE _ ctx _ ies) <- es
    , ExprI i (AssE v body@(ExprI bi _) _) <- ies
    , Just (k, et) <- [Map.lookup bi bodyType]
    ]
  return (trees <> instanceTrees, Map.fromList [(termId, (i, v)) | (termId, v, i) <- firstPerTerm])
contractRoots _ = return ([], Map.empty)

-- | The root module's instance declarations: index, class, instance types.
rootInstances :: ExprI -> [(Int, ClassName, [TypeU])]
rootInstances (ExprI _ (ModE _ es)) = [(i, cls, ts) | ExprI i (IstE cls _ ts _) <- es]
rootInstances _ = []

-- | A class constraint names the type it constrains: @Ord a =>@, never a
-- bare @Ord =>@. Checked on the root module's signatures.
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

-- | Translate ExprI to ExprS tree
collectExprS :: Namer -> ExprI -> MorlocMonad (Namer, ExprS Int ManyPoly Int)
collectExprS namer0 (ExprI gi0 e0) = f namer0 e0
  where
    f namer (VarE _ v) = do
      MM.sayVVV $
        "collectExprS VarE"
          <> "\n  gi:" <+> pretty gi0
          <> "\n  v:" <+> pretty v
      sigs@(GMap idmap _) <- MM.gets stateSignatures
      let termId = Map.lookup gi0 idmap
      case (termId, GMap.lookup gi0 sigs) of
        -- Collecting for specialization: a class method is used by its
        -- instances' types alone, and the instance a use resolves to is
        -- elaborated at the use's type. Within a method's own expansion a
        -- use of it is not a back-edge: it may resolve to another instance.
        (Just k, GMapJust (Polymorphic cls clsName t ts))
          | Just _ <- namerShallowOnly namer
          , Just r <- namerShallow namer
          , Nothing <- Map.lookup v (namerMap namer)
          , k /= r || Map.member k (namerExpanding namer) ->
              return (namer, VarS v (PolymorphicExpr cls clsName t [(et, []) | Just et <- map termGeneral ts]))
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
          | Just r <- namerShallow namer
          , r /= k
          , maybe True (Set.member k) (namerShallowOnly namer)
          , Just _ <- termGeneral t
          , not (MT.isInfixOf "`" (unEVar v)) ->
              return (namer, VarS v (MonomorphicExpr (termGeneral t) []))
        -- A monomorphic term will have a type if it is linked to any source
        -- since sources require signatures. But if it associated only with a
        -- declaration, then it will have no type.
        (Just k, GMapJust (Monomorphic t)) -> do
          MM.sayVVV $ "  monomorphic term" <+> pretty v <> ":" <+> maybe "?" pretty (termGeneral t)
          (namer', es) <- expanding k namer (\n -> termtypesToAnnoS gi0 n t)
          v' <- nameIfRecursive namer' k
          return (namer', VarS v' (MonomorphicExpr (termGeneral t) es))
        -- A polymorphic term should always have a type.
        (Just k, GMapJust (Polymorphic cls clsName t ts)) -> do
          MM.sayVVV $
            "  polymorphic term" <+> pretty v <> ":" <+> list (map (maybe "?" pretty . termGeneral) ts)
          (namer', ess) <- expanding k namer (\n -> statefulMapM (termtypesToAnnoS gi0) n ts)
          let etypes = map (fromJust . termGeneral) ts
          v' <- nameIfRecursive namer' k
          return (namer', VarS v' (PolymorphicExpr cls clsName t (zip etypes ess)))
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
                MM.modify (\st -> st {stateRecursionTargets = Map.insert token gi0 (stateRecursionTargets st)})
              return token
          | otherwise = return v

        termtypesToAnnoS :: Int -> Namer -> TermTypes -> MorlocMonad (Namer, [AnnoS Int ManyPoly Int])
        termtypesToAnnoS gi n t = do
          let calls = [AnnoS gi ci (ExeS (SrcCall src)) | (_, Idx ci src) <- termConcrete t]

          (n', declarations) <- statefulMapM termExprToAnnoS n (termDecl t)
          return (n', (calls <> declarations))

        termExprToAnnoS :: Namer -> ExprI -> MorlocMonad (Namer, AnnoS Int ManyPoly Int)
        termExprToAnnoS n e = do
          -- Capture ci AFTER reindexExprI: each inlining of a top-level term
          -- needs its own concrete index so downstream passes (e.g. Express's
          -- LetS handler that falls back to cidx as a let-binding id when the
          -- bound variable is unused) don't collide across inlinings.
          e2@(ExprI ci _) <- reindexExprI e
          (n', e') <- collectExprS n e2
          -- The labeled call config 'Restructure.collectTags' parked at
          -- the source VarE's idx gets propagated by reindexExprI's
          -- copyState to @ci@, but the wrapping AnnoS here uses @gi0@
          -- as its outer idx -- so without an extra copy from @ci@ to
          -- @gi0@ the label config sits at an idx that no AnnoS in
          -- the rAST references, leaving downstream codegen unable to
          -- detect the labeled call (this manifests as @cache: true@
          -- or @log: true@ silently failing on point-free bindings).
          st <- MM.get
          case Map.lookup ci (stateManifoldConfig st) of
            Just cfg | hasLabel cfg ->
              MM.modify (\s -> s {stateManifoldConfig = Map.insert gi0 cfg (stateManifoldConfig s)})
            _ -> return ()
          return (n', AnnoS gi0 ci e')
          where
            hasLabel cfg = case manifoldConfigLabel cfg of
              Just _ -> True
              Nothing -> False
    f namer (LstE es) = statefulMapM collectAnnoS namer es |>> second LstS
    f namer (TupE es) = statefulMapM collectAnnoS namer es |>> second TupS
    f namer (NamE rs) = do
      (namer', vals) <- statefulMapM collectAnnoS namer (map snd rs)
      let keys = map fst rs
      return (namer', NamS (zip keys vals))
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
          innerBody = case rest of
            [] -> body
            _ -> ExprI (exprIIdx body) (LetE rest body)
      (inner, body') <- collectAnnoS namer2 innerBody
      return (namer {namerRecursive = namerRecursive inner, namerIndex = namerIndex inner}, LetS v' e1' body')
    f _ (LetE [] _) = error "Bug in collectExprS: empty let bindings"
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
    f namer (PatE p) = return (namer, ExeS (PatCall p))
    f namer (DoBlockE e) = do
      (namer', e') <- collectAnnoS namer e
      return (namer', DoBlockS e')
    f namer (EvalE e) = do
      (namer', e') <- collectAnnoS namer e
      return (namer', EvalS e')
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
