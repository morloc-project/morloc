{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Frontend.TypeNames
Description : Resolve every written type name to the declaration it denotes
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

A type name is resolved where it is written: in the module's own
declarations and the type names it imports. Each declaration gets one
program-wide name, its bare name unless another module declares a type of
the same name, in which case each such declaration is qualified by its
module (@lib.P@). After this pass every type name in the program denotes
exactly one declaration, wherever the type is later used.

Names the compiler itself defines ('reservedTypeNames') are global and need
no declaration.
-}
module Morloc.Frontend.TypeNames
  ( resolveTypeNames
  , reservedTypeNames
  , isReservedTypeName
  ) where

import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Morloc.Data.DAG as DAG
import Morloc.Data.Doc
import Morloc.Data.Map (Map)
import qualified Morloc.Data.Map as Map
import Morloc.Frontend.Namespace
import qualified Morloc.Monad as MM
import Morloc.Typecheck.Internal (traverseTypeUChildren)

-- | Type names the compiler defines and refers to by name.
reservedTypeNames :: Set.Set Text
reservedTypeNames =
  Set.fromList
    [ "Unit", "Real", "F32", "F64", "Int", "I8", "I16", "I32", "I64"
    , "U8", "UInt", "U16", "U32", "U64", "Bool", "Str", "List", "Optional"
    , "Vector", "Matrix", "Table", "ArrowTable", "Record", "Rec"
    , "IFile", "IStream", "OStream", "Cell", "Try", "Closure"
    , "PatternChain", "PatternAccessible"
    , "Keys", "ListToSet", "Size", "ProjectField", "Singleton", "Restrict"
    ]

isReservedTypeName :: Text -> Bool
isReservedTypeName t =
  Set.member t reservedTypeNames || numbered "Tuple" || numbered "Tensor"
  where
    numbered p = case T.stripPrefix p t of
      Just n -> not (T.null n) && T.all (`elem` ['0' .. '9']) n
      Nothing -> False

-- | What a written name resolves to in one module.
data Binding
  = Bound TVar
  | Ambiguous [TVar]

-- | Rewrite every written type name to the name of the declaration it
-- denotes, and the type edges of the module graph with it.
resolveTypeNames ::
  DAG MVar [AliasedSymbol] ExprI ->
  MorlocMonad (DAG MVar [AliasedSymbol] ExprI)
resolveTypeNames d = do
  let declaring = Map.fromListWith Set.union
        [ (n, Set.singleton m) | (m, (e, _)) <- Map.toList d, TV n <- localTypes e ]
      canonical m (TV n)
        | isReservedTypeName n = TV n
        | maybe 0 Set.size (Map.lookup n declaring) <= 1 = TV n
        | otherwise = TV (qualifier m <> "." <> n)
      -- A local module's key carries the leading dot of its import; it is
      -- dropped unless another module's key differs from it only by that.
      bare m = T.dropWhile (== '.') (unMVar m)
      qualifier m
        | length [k | k <- Map.keys d, bare k == bare m] > 1 = unMVar m
        | otherwise = bare m
  result <- DAG.synthesize (resolveModule canonical) rewriteEdge d
  case result of
    Nothing -> MM.throwSystemError "Cyclic module dependency in type name resolution"
    Just d' -> return (DAG.mapNode fst d')
  where
    rewriteEdge ::
      [AliasedSymbol] ->
      (ExprI, Map TVar Binding) ->
      (ExprI, Map TVar Binding) ->
      MorlocMonad [AliasedSymbol]
    rewriteEdge syms _ (_, childNames) = return (map f syms)
      where
        f (AliasedType src _) = case Map.lookup src childNames of
          Just (Bound c) -> AliasedType c c
          _ -> AliasedType src src
        f s = s

resolveModule ::
  (MVar -> TVar -> TVar) ->
  MVar ->
  ExprI ->
  [(MVar, [AliasedSymbol], (ExprI, Map TVar Binding))] ->
  MorlocMonad (ExprI, Map TVar Binding)
resolveModule canonical m e children = do
  let locals = localTypes e
      localMap = Map.fromList [(v, canonical m v) | v <- locals]
      imported =
        Map.fromListWith (<>)
          [ (alias, [c])
          | (_, syms, (_, childNames)) <- children
          , AliasedType src alias <- syms
          , c <- case Map.lookup src childNames of
              Just (Bound c) -> [c]
              Just (Ambiguous cs) -> cs
              Nothing -> [src | isReservedTypeName (unTVar src)]
          ]
  mapM_ (checkExplicitImport localMap) (explicitTypeImports e)
  let importedBindings = Map.map (binding . Set.toList . Set.fromList) imported
      names = Map.union (Map.map Bound localMap) importedBindings
  e' <- rewriteExpr m names e
  return (e', names)
  where
    binding [c] = Bound c
    binding cs = Ambiguous cs

    checkExplicitImport :: Map TVar TVar -> (Int, TVar, MVar) -> MorlocMonad ()
    checkExplicitImport localMap (i, alias, from)
      | Just c <- Map.lookup alias localMap
      , not (isReservedTypeName (unTVar c)) =
          MM.throwSourcedError i $
            "Module" <+> squotes (pretty m) <+> "imports the type"
              <+> squotes (pretty alias) <+> "from" <+> squotes (pretty from)
              <+> "and also declares a type of that name; remove the import or rename one of them"
      | otherwise = return ()

-- | The general types a module declares.
localTypes :: ExprI -> [TVar]
localTypes (ExprI _ (ModE _ es)) = concatMap localTypes es
localTypes (ExprI _ (TypE (ExprTypeE Nothing v _ _ _ _))) = [v]
localTypes _ = []

-- | Type names a module imports through an include list.
explicitTypeImports :: ExprI -> [(Int, TVar, MVar)]
explicitTypeImports (ExprI _ (ModE _ es)) = concatMap explicitTypeImports es
explicitTypeImports (ExprI i (ImpE (Import from (Just items) _ _))) =
  [(i, alias, from) | AliasedType _ alias <- items]
explicitTypeImports _ = []

isTypeName :: TVar -> Bool
isTypeName (TV t) = maybe False (isUpper . fst) (T.uncons t)

rewriteExpr :: MVar -> Map TVar Binding -> ExprI -> MorlocMonad ExprI
rewriteExpr m names = go
  where
    resolveName :: Int -> TVar -> MorlocMonad TVar
    resolveName i v = case Map.lookup v names of
      Just (Bound c) -> return c
      Just (Ambiguous cs) ->
        MM.throwSourcedError i $
          "The type name" <+> squotes (pretty v) <+> "is ambiguous in module"
            <+> squotes (pretty m) <> "; it is imported as" <+> hsep (punctuate "," (map (squotes . pretty) cs))
      Nothing
        | isReservedTypeName (unTVar v) -> return v
        | otherwise ->
            MM.throwSourcedError i $
              "The type" <+> squotes (pretty v) <+> "is not declared or imported in module"
                <+> squotes (pretty m)

    -- a general type: every capitalized name is a type name
    typ :: Int -> TypeU -> MorlocMonad TypeU
    typ i t = case t of
      VarU v | isTypeName v -> VarU <$> resolveName i v
      AppU h ts -> AppU <$> typ i h <*> mapM (typ i) ts
      _ -> traverseTypeUChildren (typ i) t

    -- A record's body carries the record's name, or, in the constructor
    -- form (@record Foo = Bar {...}@), the constructor's.
    renameOwn :: TVar -> TVar -> TypeU -> TypeU
    renameOwn v v' (NamU o n ps rs) | n == v = NamU o v' ps rs
    renameOwn _ _ t = t

    -- the body of a language form whose head is the language's own name
    terminal :: Int -> TVar -> TVar -> TypeU -> MorlocMonad TypeU
    terminal i v v' t = case t of
      VarU _ -> return t
      AppU h ts -> AppU h <$> mapM (typ i) ts
      NamU o n ps rs -> do
        ps' <- mapM (typ i) ps
        rs' <- mapM (\(k, x) -> (,) k <$> typ i x) rs
        return (renameOwn v v' (NamU o n ps' rs'))
      _ -> typ i t

    param :: Int -> Either (TVar, Kind) TypeU -> MorlocMonad (Either (TVar, Kind) TypeU)
    param _ p@(Left _) = return p
    param i (Right t) = Right <$> typ i t

    constraint :: Int -> Constraint -> MorlocMonad Constraint
    constraint i (Constraint cls ts) = Constraint cls <$> mapM (typ i) ts
    constraint i (CMember a s) = CMember <$> typ i a <*> typ i s
    constraint i (CSubset a b) = CSubset <$> typ i a <*> typ i b
    constraint i (CDisjoint a b) = CDisjoint <$> typ i a <*> typ i b

    resolveEType :: Int -> EType -> MorlocMonad EType
    resolveEType i et = do
      t' <- typ i (etype et)
      cs' <- mapM (constraint i) (Set.toList (econs et))
      return et {etype = t', econs = Set.fromList cs'}

    go :: ExprI -> MorlocMonad ExprI
    go (ExprI i x) = ExprI i <$> case x of
      ModE mv es -> ModE mv <$> mapM go es
      SigE (Signature v l et) -> SigE . Signature v l <$> resolveEType i et
      TypE (ExprTypeE Nothing v ps t doc k) -> do
        v' <- resolveName i v
        ps' <- mapM (param i) ps
        t' <- typ i t
        return (TypE (ExprTypeE Nothing v' ps' (renameOwn v v' t') doc k))
      TypE (ExprTypeE form@(Just (_, isTerminal)) v ps t doc k) -> do
        v' <- resolveName i v
        ps' <- mapM (param i) ps
        t' <- if isTerminal then terminal i v v' t else typ i t
        return (TypE (ExprTypeE form v' ps' t' doc k))
      AnnE e t -> AnnE <$> go e <*> typ i t
      IstE cls ctx ts es ->
        IstE cls <$> mapM (constraint i) ctx <*> mapM (typ i) ts <*> mapM go es
      ClsE (Typeclass cs cls vs sigs) ->
        ClsE <$> (Typeclass <$> mapM (constraint i) cs <*> pure cls <*> pure vs
                   <*> mapM (\(Signature v l et) -> Signature v l <$> resolveEType i et) sigs)
      ConE t n k es -> ConE <$> resolveName i t <*> pure n <*> pure k <*> mapM go es
      ExpE ex -> return (ExpE ex)
      AssE v e es -> AssE v <$> go e <*> mapM go es
      LamE vs e -> LamE vs <$> go e
      AppE e es -> AppE <$> go e <*> mapM go es
      LstE es -> LstE <$> mapM go es
      TupE es -> TupE <$> mapM go es
      NamE rs -> NamE <$> mapM (\(k, e) -> (,) k <$> go e) rs
      LetE bs e -> LetE <$> mapM (\(v, b) -> (,) v <$> go b) bs <*> go e
      IfE c t f -> IfE <$> go c <*> go t <*> go f
      DoBlockE e -> DoBlockE <$> go e
      EvalE e -> EvalE <$> go e
      IntrinsicE intr es -> IntrinsicE intr <$> mapM go es
      BopE a j op b -> BopE <$> go a <*> pure j <*> pure op <*> go b
      ParenE e -> ParenE <$> go e
      _ -> return x
