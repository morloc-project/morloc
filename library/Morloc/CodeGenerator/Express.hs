{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.CodeGenerator.Express
Description : Resolve type aliases and infer concrete types in manifold trees
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Transforms the parameterized 'AnnoS' trees into a form where every
node carries both its general type and inferred concrete type. This is
the step where language-specific type aliases are resolved and type
parameters are fully instantiated.
-}
module Morloc.CodeGenerator.Express
  ( express
  ) where

import qualified Data.Set as Set
import qualified Morloc.BaseTypes as BT
import Morloc.CodeGenerator.Infer
import Morloc.CodeGenerator.Namespace
import Morloc.Data.Doc
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.Data.Map as Map
import qualified Morloc.Monad as MM
import qualified Morloc.TypeEval as TE

mkIdx :: AnnoS g One (Indexed c, d) -> Type -> Indexed Type
mkIdx (AnnoS _ (Idx i _, _) _) = Idx i

-- C3 invariant: the record literal's key order must match the declared
-- schema's key order. The bidirectional checkE rule in
-- Frontend/Typecheck.hs reorders NamS against the declared NamU; this
-- assertion catches any future regression of that pass and turns a
-- silent wire-format corruption into a sourced compiler-internal error.
assertRecordKeyOrder ::
  Int -> [(Key, a)] -> [(Key, b)] -> MorlocMonad ()
assertRecordKeyOrder midx entries rs
  | map fst entries == map fst rs = return ()
  | otherwise = MM.throwSourcedError midx $
      "Compiler-internal error: record literal key order does not match"
      <+> "declared schema order (C3 invariant violated)."
      <+> "Literal keys:" <+> hsep (punctuate "," (map (pretty . fst) entries))
      <> "; declared keys:" <+> hsep (punctuate "," (map (pretty . fst) rs))

setManifoldConfig ::
  Int ->
  AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) ->
  MorlocMonad ()
setManifoldConfig midx (AnnoS _ _ (AppS (AnnoS (Idx fidx _) _ (VarS _ _)) _)) = linkConfigIndex midx fidx
setManifoldConfig midx (AnnoS _ _ (AppS (AnnoS (Idx fidx _) _ (ExeS _)) _)) = linkConfigIndex midx fidx
setManifoldConfig midx (AnnoS _ _ (AppS e _)) = setManifoldConfig midx e
setManifoldConfig midx (AnnoS _ _ (LamS _ e)) = setManifoldConfig midx e
setManifoldConfig midx (AnnoS _ _ (DoBlockS e)) = setManifoldConfig midx e
setManifoldConfig midx (AnnoS _ _ (EvalS e)) = setManifoldConfig midx e
setManifoldConfig midx (AnnoS _ _ (CoerceS _ e)) = setManifoldConfig midx e
setManifoldConfig _ (AnnoS _ _ (IntrinsicS _ _)) = return ()
setManifoldConfig midx (AnnoS _ _ (IfS _ t _)) = setManifoldConfig midx t
setManifoldConfig _ (AnnoS _ _ (CallS _)) = return ()
setManifoldConfig _ _ = return ()

linkConfigIndex :: Int -> Int -> MorlocMonad ()
linkConfigIndex midx fidx = do
  s <- MM.get
  case Map.lookup fidx (stateManifoldConfig s) of
    Nothing -> return ()
    (Just mconfig) -> do
      MM.put (s {stateManifoldConfig = Map.insert midx mconfig (stateManifoldConfig s)})

propagateScope :: Int -> Int -> MorlocMonad ()
propagateScope calleeIdx appIdx = do
  s <- MM.get
  case GMap.yIsX calleeIdx appIdx (stateConcreteTypedefs s) of
    (Just gmap') -> MM.put $ s {stateConcreteTypedefs = gmap'}
    Nothing -> return ()

express :: AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) -> MorlocMonad PolyHead
express e@(AnnoS (Idx midx t) (Idx cidx _, _) _) = do
  -- Store the return effect labels before forceExportThunks strips them
  let retEffects = extractReturnEffects t
  MM.modify (\s -> s { stateManifoldEffects = Map.insert midx retEffects (stateManifoldEffects s) })
  forceExportThunks cidx t <$> expressCore e
  where
    extractReturnEffects (FunT _ (EffectT effs _)) = effs
    extractReturnEffects (EffectT effs _) = effs
    extractReturnEffects _ = Set.empty

-- At the export boundary, thunks cannot be serialized. This function:
--   1. Wraps thunk-typed args in PolyDoBlock so they are received as plain
--      values from the CLI and suspended inside the pool.
--   2. Wraps thunk return types in PolyEval so they are evaluated before
--      serialization back to the user.
forceExportThunks :: Int -> Type -> PolyHead -> PolyHead
forceExportThunks cidx t (PolyHead lang midx args body) =
  let inputTs = case t of FunT inputs _ -> inputs; _ -> []
      thunkArgIds = [ann a | (a, EffectT _ _) <- zip args inputTs]
      retT = case t of FunT _ ret -> ret; t' -> t'
      body' = suspendThunkArgs thunkArgIds body
      body'' = forceAtReturn cidx retT body'
   in PolyHead lang midx args body''
  where
    -- Wrap BndVar references to thunk-typed args in PolyDoBlock.
    -- The arg is deserialized as the inner type; the suspend creates the thunk.
    suspendThunkArgs [] e = e
    suspendThunkArgs ids e = goExpr ids e

    goExpr ids (PolyBndVar (C (Idx ci (EffectT effs inner))) i)
      | i `elem` ids = wrapSuspends ci (EffectT effs inner) i
    goExpr ids (PolyManifold l m f e) = PolyManifold l m f (goExpr ids e)
    goExpr ids (PolyLet i e1 e2) = PolyLet i (goExpr ids e1) (goExpr ids e2)
    goExpr ids (PolyReturn e) = PolyReturn (goExpr ids e)
    goExpr ids (PolyApp e es) = PolyApp (goExpr ids e) (map (goExpr ids) es)
    goExpr ids (PolyEval ti e) = PolyEval ti (goExpr ids e)
    goExpr ids (PolyDoBlock ti e) = PolyDoBlock ti (goExpr ids e)
    goExpr ids (PolyCoerce c ti e) = PolyCoerce c ti (goExpr ids e)
    goExpr ids (PolyIntrinsic ti intr es) = PolyIntrinsic ti intr (map (goExpr ids) es)
    goExpr ids (PolyList v ti es) = PolyList v ti (map (goExpr ids) es)
    goExpr ids (PolyTuple v es) = PolyTuple v (map (fmap (goExpr ids)) es)
    goExpr ids (PolyRecord o v ps rs) = PolyRecord o v ps (map (fmap (fmap (goExpr ids))) rs)
    goExpr ids (PolyIf c t' e) = PolyIf (goExpr ids c) (goExpr ids t') (goExpr ids e)
    goExpr ids (PolyRemoteInterface l ti is rf e) = PolyRemoteInterface l ti is rf (goExpr ids e)
    goExpr _ e = e

    -- Peel EffectT layers, wrapping each in PolyDoBlock, with the innermost
    -- BndVar carrying the fully-unwrapped type.
    wrapSuspends ci (EffectT effs inner) i =
      PolyDoBlock (Idx ci (EffectT effs inner)) (wrapSuspends ci inner i)
    wrapSuspends ci inner i = PolyBndVar (C (Idx ci inner)) i

    forceAtReturn c rt (PolyReturn e) = PolyReturn (wrapForces c rt e)
    forceAtReturn c rt (PolyManifold l m f e) = PolyManifold l m f (forceAtReturn c rt e)
    forceAtReturn c rt (PolyLet i e1 e2) = PolyLet i e1 (forceAtReturn c rt e2)
    forceAtReturn c rt e = wrapForces c rt e

    wrapForces c (EffectT _ inner) e = wrapForces c inner (PolyEval (Idx c inner) e)
    wrapForces _ _ e = e

expressCore :: AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) -> MorlocMonad PolyHead
expressCore (AnnoS (Idx midx c@(FunT inputs _)) (Idx cidx lang, _) (ExeS exe)) = do
  ids <- MM.takeFromCounter (length inputs)
  exe' <- case exe of
    (SrcCall src) -> return $ SrcCallP src
    (PatCall pat) -> return $ PatCallP pat
  let lambdaVals = fromJust $ safeZipWith PolyBndVar (map (C . Idx cidx) inputs) ids
  return
    . PolyHead lang midx [Arg i None | i <- ids]
    . PolyReturn
    $ PolyApp (PolyExe (Idx midx c) exe') lambdaVals
-- Point-free export aliasing a recursive function. extractRecursiveHelpers
-- has pulled the helper into its own manifold and left a bare CallS back-edge
-- at the export root. Eta-expand here so the PolyHead carries the function's
-- arity; otherwise the export manifold would be emitted with zero args while
-- the nexus manifest correctly records the type-driven arity.
expressCore (AnnoS (Idx midx c@(FunT inputs out)) (Idx cidx lang, _) (CallS v)) = do
  (mid, crossLang) <- lookupRecursiveTarget lang v
  ids <- MM.takeFromCounter (length inputs)
  let lambdaVals = fromJust $ safeZipWith PolyBndVar (map (C . Idx cidx) inputs) ids
      headArgs = [Arg i None | i <- ids]
  case out of
    EffectT effs innerOut ->
      return
        . PolyHead lang midx headArgs
        . PolyReturn
        . PolyDoBlock (Idx cidx (EffectT effs innerOut))
        $ PolyApp (PolyExe (Idx midx (FunT inputs innerOut)) (RecCallP mid crossLang)) lambdaVals
    _ ->
      return
        . PolyHead lang midx headArgs
        . PolyReturn
        $ PolyApp (PolyExe (Idx midx c) (RecCallP mid crossLang)) lambdaVals
expressCore (AnnoS (Idx midx _) (_, lambdaArgs) (LamS _ e@(AnnoS (Idx _ applicationType) (c, _) x))) = do
  setManifoldConfig midx e
  expressCore (AnnoS (Idx midx applicationType) (c, lambdaArgs) x)
expressCore (AnnoS (Idx midx (AppT (VarT v) ts)) (Idx cidx lang, args) (LstS xs))
  | [t] <- filter (not . isNatTypeT) ts = do
      xs' <- mapM (\x -> expressPolyExprWrap lang (mkIdx x t) x) xs
      -- Preserve the FULL applied type args (Nat positions included) so
      -- downstream language code generators can see phantom dims.
      let x = PolyList (Idx cidx v) (map (Idx cidx) ts) xs'
      return $ PolyHead lang midx [Arg i None | Arg i _ <- args] (PolyReturn x)
-- LstS at a type whose head is a type-alias (e.g. @type Pat = [Pat]@
-- used as a manifold return type). Reduce one alias step and retry.
-- Same shape as the NamS reduce-and-retry below.
expressCore (AnnoS (Idx midx t) (Idx cidx lang, args) (LstS xs)) = do
  mayT <- evalGeneralStep midx (type2typeu t)
  case mayT of
    (Just t') -> expressCore (AnnoS (Idx midx (typeOf t')) (Idx cidx lang, args) (LstS xs))
    Nothing -> MM.throwSourcedError midx $ "Invalid list form: " <> pretty t
-- TupS literal where the head IS the TupleN constructor: the type args
-- are the slot types, so zip directly. For any other @AppT (VarT v) ts@
-- (user alias whose args don't correspond to slot types -- e.g. the
-- @NatLit n@ in a phantom-Nat tuple alias like
-- @type FixedPair (n :: Nat) a = (a, a)@), fall through to the
-- reduce-and-retry below so the alias body's slot types drive
-- element-wise expression. See the parallel guard in Typecheck.hs C2.
expressCore (AnnoS (Idx midx (AppT (VarT v) ts)) (Idx cidx lang, args) (TupS xs))
  | v == BT.tuple (length xs)
  , length ts == length xs = do
      let idxTs = zipWith mkIdx xs ts
      xs' <- fromJust <$> safeZipWithM (expressPolyExprWrap lang) idxTs xs
      let x = PolyTuple (Idx cidx v) (fromJust $ safeZip idxTs xs')
      return $ PolyHead lang midx [Arg i None | Arg i _ <- args] (PolyReturn x)
-- TupS at a non-tuple head (user alias, including phantom-Nat aliases,
-- and bare @VarT WrappedLL@ for @type WrappedLL = (Int, LL)@). Reduce
-- one alias step and retry; same shape as the NamS reduce-and-retry.
expressCore (AnnoS (Idx midx t) (Idx cidx lang, args) (TupS xs)) = do
  mayT <- evalGeneralStep midx (type2typeu t)
  case mayT of
    (Just t') -> expressCore (AnnoS (Idx midx (typeOf t')) (Idx cidx lang, args) (TupS xs))
    Nothing -> MM.throwSourcedError midx $ "Invalid tuple form: " <> pretty t
expressCore (AnnoS (Idx midx (NamT o v ps rs)) (Idx cidx lang, args) (NamS entries)) = do
  -- C3 invariant: the literal's key order must equal the declared
  -- schema's key order. Typecheck.hs reorders NamS against the
  -- declared NamU; if that ever regresses, this assertion catches the
  -- mis-zip at the point where it would corrupt the wire format.
  assertRecordKeyOrder midx entries rs
  let idxTypes = zipWith mkIdx (map snd entries) (map snd rs)
  xs' <- fromJust <$> safeZipWithM (expressPolyExprWrap lang) idxTypes (map snd entries)
  let x = PolyRecord o (Idx cidx v) (map (Idx cidx) ps) (zip (map fst rs) (zip idxTypes xs'))
  return $ PolyHead lang midx [Arg i None | Arg i _ <- args] (PolyReturn x)
expressCore (AnnoS (Idx midx t) (Idx cidx lang, args) (NamS entries)) = do
  mayT <- evalGeneralStep midx (type2typeu t)
  case mayT of
    (Just t') -> expressCore (AnnoS (Idx midx (typeOf t')) (Idx cidx lang, args) (NamS entries))
    Nothing -> MM.throwSourcedError midx $ "Missing concrete:" <+> "t=" <> pretty t
expressCore e = expressDefault e

reduceType :: Scope -> Type -> Maybe Type
reduceType scope t0 =
  let tu0 = type2typeu t0
   in case TE.evaluateStep scope tu0 of
        (Just tu1) -> if tu0 == tu1 then Nothing else Just (typeOf tu1)
        Nothing -> Nothing

expressDefault :: AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) -> MorlocMonad PolyHead
expressDefault e0@(AnnoS (Idx midx t) (Idx cidx lang, args) _) =
  PolyHead lang midx [Arg i None | Arg i _ <- args] . ensurePolyReturn
    <$> expressPolyExprWrap lang (Idx cidx t) e0
  where
    -- ensure the manifold body has PolyReturn at the return position
    ensurePolyReturn (PolyReturn x) = PolyReturn x
    ensurePolyReturn (PolyLet i e1 e2) = PolyLet i e1 (ensurePolyReturn e2)
    ensurePolyReturn (PolyManifold l m f e) = PolyManifold l m f (ensurePolyReturn e)
    ensurePolyReturn x = PolyReturn x

expressPolyExprWrap ::
  Lang ->
  Indexed Type ->
  AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) ->
  MorlocMonad PolyExpr
expressPolyExprWrap l t e@(AnnoS (Idx midx _) _ (LamS _ lamExpr)) = do
  setManifoldConfig midx lamExpr
  expressPolyExprWrapCommon l t e
expressPolyExprWrap l t e = expressPolyExprWrapCommon l t e

expressPolyExprWrapCommon ::
  Lang -> Indexed Type -> AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) -> MorlocMonad PolyExpr
expressPolyExprWrapCommon l t e@(AnnoS _ _ (AppS (AnnoS (Idx gidxCall _) _ _) _)) = do
  bconf <- MM.gets stateBuildConfig
  mconMap <- MM.gets stateManifoldConfig
  expressPolyExpr (decideRemoteness bconf (Map.lookup gidxCall mconMap)) l t e
expressPolyExprWrapCommon l t e@(AnnoS (Idx midx _) _ _) = do
  bconf <- MM.gets stateBuildConfig
  mconMap <- MM.gets stateManifoldConfig
  expressPolyExpr (decideRemoteness bconf (Map.lookup midx mconMap)) l t e

decideRemoteness :: BuildConfig -> Maybe ManifoldConfig -> Lang -> Lang -> Maybe RemoteForm
decideRemoteness _ Nothing l1 l2
  | l1 == l2 = Nothing
  | otherwise = Just ForeignCall
decideRemoteness _ (Just (ManifoldConfig _ _ Nothing)) l1 l2
  | l1 == l2 = Nothing
  | otherwise = Just ForeignCall
decideRemoteness bconf (Just (ManifoldConfig _ _ (Just res))) l1 l2 =
  case (buildConfigSlurmSupport bconf, l1 /= l2) of
    (Just True, _) -> Just $ RemoteCall res
    (_, True) -> Just $ ForeignCall
    _ -> Nothing

-- Express a function argument. A do-block passed where an effect-typed
-- (thunk) parameter is expected must be suspended whole -- identically to
-- a bare effectful application argument -- so the handler receives an
-- unevaluated thunk. Keep the EffectT on the inner expression so its
-- source call is auto-suspended (Grammars.Common). The shared DoBlockS
-- clause still strips EffectT for a return/export-position do-block, which
-- forceExportThunks / pushForceIntoRemote discharge at the boundary.
expressPolyArg ::
  Lang ->
  Indexed Type ->
  AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) ->
  MorlocMonad PolyExpr
expressPolyArg parentLang pc@(Idx _ (EffectT _ _)) (AnnoS (Idx midx t@(EffectT _ _)) (Idx cidx lang, args) (DoBlockS x)) = do
  x' <- expressPolyExprWrap lang (mkIdx x t) x
  let e = PolyDoBlock (Idx cidx t) x'
  return $ expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e
expressPolyArg l pc e = expressPolyExprWrap l pc e

expressPolyExpr ::
  (Lang -> Lang -> Maybe RemoteForm) ->
  Lang ->
  Indexed Type ->
  AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) ->
  MorlocMonad PolyExpr
expressPolyExpr
  findRemote
  parentLang
  _
  ( AnnoS
      (Idx midx (FunT lamInputTypes lamOutType))
      (Idx cidxLam _, lamArgs)
      ( LamS
          vs
          ( AnnoS
              _
              (Idx _ appLang, appArgs)
              ( AppS
                  funExpr@(AnnoS (Idx gidxCall (FunT callInputTypes _)) (Idx _ callLang, _) _)
                  xs
                )
            )
        )
    )
    | isLocal = do
        propagateScope gidxCall midx
        let nContextArgs = length appArgs - length vs
            contextArgs = map unvalue (take nContextArgs appArgs)

            typedLambdaArgs =
              fromJust $
                safeZipWith
                  (\(Arg i _) t -> Arg i (Just t))
                  (drop nContextArgs lamArgs)
                  lamInputTypes

        xs' <- fromJust <$> safeZipWithM (expressPolyExprWrap appLang) (zipWith mkIdx xs callInputTypes) xs

        call <- expressPolyApp parentLang funExpr xs'

        return
          . PolyManifold parentLang midx (ManifoldPart contextArgs typedLambdaArgs)
          $ call
    | not isLocal = do
        propagateScope gidxCall midx

        xsInfo <- mapM partialExpress xs

        let xs' = map (\(_, _, e) -> e) xsInfo
            callArgs = unique (concatMap (\(rs, _, _) -> rs) xsInfo)
            args = [i | Arg i _ <- appArgs]
            allParentArgs = args <> [i | (_, Just (i, _), _) <- xsInfo]
            lets = [PolyLet i e | (_, Just (i, e), _) <- xsInfo]
            passedParentArgs = concat [[r | r <- allParentArgs, r == i] | i <- callArgs]
            nContextArgs = length appArgs - length vs

            lambdaTypeMap = zip vs (map (Idx cidxLam) lamInputTypes)
            boundVars =
              [ PolyBndVar (maybe (A parentLang) C (lookup v lambdaTypeMap)) i
              | Arg i v <- appArgs
              ]
            untypedContextArgs = map unvalue $ take nContextArgs appArgs
            typedPassedArgs = fromJust $ safeZipWith (\(Arg i _) t -> Arg i (Just t)) (drop nContextArgs lamArgs) lamInputTypes

            localForm = ManifoldPart untypedContextArgs typedPassedArgs

            foreignForm = ManifoldFull [Arg i None | i <- passedParentArgs]

        call <- expressPolyApp parentLang funExpr xs'

        return
          . PolyManifold parentLang midx localForm
          . chain lets
          . PolyReturn
          . PolyApp
            ( PolyRemoteInterface callLang (Idx cidxLam lamOutType) passedParentArgs (fromJust remote)
                . PolyManifold callLang midx foreignForm
                $ call
            )
          $ boundVars
    where
      remote = findRemote parentLang callLang
      isLocal = isNothing remote

      chain :: [a -> a] -> a -> a
      chain [] x = x
      chain (f : fs) x = chain fs (f x)

      partialExpress ::
        AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) ->
        MorlocMonad
          ( [Int]
          , Maybe (Int, PolyExpr)
          , PolyExpr
          )
      partialExpress (AnnoS (Idx _ t) (Idx cidx _argLang, [Arg idx _]) (BndS _)) = do
        let x' = PolyBndVar (C (Idx cidx t)) idx
        return ([idx], Nothing, x')
      partialExpress x@(AnnoS (Idx _ t) (Idx cidx argLang, args) _)
        | argLang == callLang = do
            let argParentType = Idx cidx t
            x' <- expressPolyExprWrap argLang argParentType x
            return ([i | Arg i _ <- args], Nothing, x')
        | otherwise = do
            let argparentType = Idx cidx t
            letVal <- expressPolyExprWrap argLang argparentType x
            idx <- MM.getCounter

            let x' = PolyLetVar (Idx cidx t) idx
            return ([idx], Just (idx, letVal), x')
expressPolyExpr _ _ _ (AnnoS lambdaType@(Idx midx _) (Idx _ lang, manifoldArguments) (LamS vs body)) = do
  body' <- expressPolyExprWrap lang lambdaType body

  inputTypes <- case val lambdaType of
    (FunT ts _) -> return ts
    _ -> return []

  let contextArguments = map unvalue $ take (length manifoldArguments - length vs) manifoldArguments
      boundArguments = map unvalue $ drop (length contextArguments) manifoldArguments
      typeBoundArguments = fromJust $ safeZipWith (\t (Arg i _) -> Arg i (Just t)) inputTypes boundArguments

  return
    . PolyManifold lang midx (ManifoldPart contextArguments typeBoundArguments)
    . PolyReturn
    $ body'
-- Inline source call: skip PolyManifold, emit as direct subexpression
expressPolyExpr
  findRemote
  parentLang
  _
  ( AnnoS
      (Idx midx _)
      _
      (AppS f@(AnnoS (Idx gidxCall (FunT inputs _)) (Idx cidxCall callLang, _) (ExeS (SrcCall src))) xs)
    )
    | srcInline src && isLocal = do
        propagateScope gidxCall midx
        xsExpr <- zipWithM (expressPolyArg callLang) (map (Idx cidxCall) inputs) xs
        expressPolyApp parentLang f xsExpr >>= stripPolyReturn
    where
      remote = findRemote parentLang callLang
      isLocal = isNothing remote
      stripPolyReturn (PolyReturn e) = return e
      stripPolyReturn e = return e
-- Inline pattern call: skip PolyManifold. PatCallP (record/struct
-- accessors, format patterns) renders as a direct expression in the
-- target language (e.g. `x.field` for .isFile). Wrapping in
-- PolyManifold creates a nested manifold whose body's `return` leaks
-- into the parent's scope when the manifold gets inlined by codegen.
-- Treating PatCallP like srcInline matches its semantics - it's
-- always a leaf expression, never a real function call.
expressPolyExpr
  findRemote
  parentLang
  _
  ( AnnoS
      (Idx midx _)
      _
      (AppS f@(AnnoS (Idx gidxCall (FunT inputs _)) (Idx cidxCall callLang, _) (ExeS (PatCall _))) xs)
    )
    | isLocal = do
        propagateScope gidxCall midx
        xsExpr <- zipWithM (expressPolyArg callLang) (map (Idx cidxCall) inputs) xs
        expressPolyApp parentLang f xsExpr >>= stripPolyReturn
    where
      remote = findRemote parentLang callLang
      isLocal = isNothing remote
      stripPolyReturn (PolyReturn e) = return e
      stripPolyReturn e = return e
expressPolyExpr
  findRemote
  parentLang
  pc
  ( AnnoS
      (Idx midx _)
      (_, args)
      (AppS f@(AnnoS (Idx gidxCall (FunT inputs _)) (Idx cidxCall callLang, _) _) xs)
    )
    | isLocal = do
        propagateScope gidxCall midx
        xsExpr <- zipWithM (expressPolyArg callLang) (map (Idx cidxCall) inputs) xs

        func <- expressPolyApp parentLang f xsExpr
        return
          . PolyManifold callLang midx (ManifoldFull (map unvalue args))
          $ func
    | not isLocal = do
        propagateScope gidxCall midx
        let idxInputTypes = zipWith mkIdx xs inputs
        mayXs <- safeZipWithM (expressPolyArg callLang) idxInputTypes xs
        func <- expressPolyApp parentLang f (fromJust mayXs)
        return
          . PolyManifold parentLang midx (ManifoldFull (map unvalue args))
          . PolyReturn
          . PolyApp
            ( PolyRemoteInterface callLang pc [] (fromJust remote)
                . PolyManifold callLang midx (ManifoldFull (map unvalue args))
                $ func
            )
          $ [PolyBndVar (A parentLang) i | Arg i _ <- args]
    where
      remote = findRemote parentLang callLang
      isLocal = isNothing remote
expressPolyExpr
  findRemote
  parentLang
  (val -> FunT pinputs poutput)
  e@(AnnoS (Idx midx (FunT callInputs _)) (Idx cidx callLang, _) _)
    | isLocal = do
        ids <- MM.takeFromCounter (length callInputs)
        let lambdaVals = bindVarIds ids (map (C . Idx cidx) callInputs)
            lambdaTypedArgs = fromJust $ safeZipWith annotate ids (map Just callInputs)
        retapp <- expressPolyApp parentLang e lambdaVals
        return
          . PolyManifold callLang midx (ManifoldPass lambdaTypedArgs)
          $ retapp
    | otherwise = do
        ids <- MM.takeFromCounter (length callInputs)
        let lambdaArgs = [Arg i None | i <- ids]
            lambdaTypedArgs = map (`Arg` Nothing) ids
            callVals = bindVarIds ids (map (C . Idx cidx) callInputs)
        retapp <- expressPolyApp callLang e callVals
        return
          . PolyManifold parentLang midx (ManifoldPass lambdaTypedArgs)
          . PolyReturn
          . PolyApp
            ( PolyRemoteInterface callLang (Idx cidx poutput) (map ann lambdaArgs) (fromJust remote)
                . PolyManifold callLang midx (ManifoldFull lambdaArgs)
                $ retapp
            )
          $ fromJust
          $ safeZipWith (PolyBndVar . C) (map (Idx cidx) pinputs) (map ann lambdaArgs)
    where
      remote = findRemote parentLang callLang
      isLocal = isNothing remote
expressPolyExpr _ _ _ (AnnoS (Idx i c) (Idx cidx _, rs) (BndS v)) = do
  case [j | (Arg j v') <- rs, v == v'] of
    [r] -> return $ PolyBndVar (C (Idx cidx c)) r
    rs' ->
      MM.throwSourcedError i $
        "Expected VarS"
          <+> dquotes (pretty v)
          <+> "of type"
          <+> parens (pretty c)
          <+> "to match exactly one argument, found:"
          <+> list (map pretty rs')
          <> "\n  v:" <+> pretty v
          <> "\n  cidx:" <+> pretty cidx
          <> "\n  gidx:" <+> pretty cidx
          <> "\n  rs:" <+> list (map pretty rs)
expressPolyExpr _ _ _ (AnnoS (Idx i c) (Idx cidx _, rs) (LetBndS v)) = do
  case [j | (Arg j v') <- rs, v == v'] of
    [r] -> return $ PolyLetVar (Idx cidx c) r
    _ -> MM.throwSourcedError i $ "Undefined let-bound variable:" <+> pretty v
expressPolyExpr
  _
  parentLang
  pc
  (AnnoS (Idx midx _) (Idx cidx lang, args) (LetS v e1 e2)) = do
    let bodyArgs = case e2 of AnnoS _ (_, ba) _ -> ba
        letId = case [j | Arg j v' <- bodyArgs, v' == v] of
          [j] -> j
          _ -> cidx
    let e1Type = case e1 of AnnoS (Idx _ t) _ _ -> mkIdx e1 t
    -- Express children under the LetS's OWN language (from Realize), not the
    -- caller's. expressContainer wraps in a cross-language manifold when the
    -- chain's lang differs from parentLang. This fuses sequential
    -- same-language calls (especially do-blocks) into one manifold.
    e1' <- expressPolyExprWrap lang e1Type e1
    e2' <- expressPolyExprWrap lang pc e2
    let e = PolyLet letId e1' e2'
    return $ expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e
expressPolyExpr _ _ _ (AnnoS (Idx _ (VarT v)) (Idx cidx _, _) (RealS _ x)) = return $ PolyReal (Idx cidx v) x
expressPolyExpr _ _ _ (AnnoS (Idx _ (VarT v)) (Idx cidx _, _) (IntS _ x)) = return $ PolyInt (Idx cidx v) x
expressPolyExpr _ _ _ (AnnoS (Idx _ (VarT v)) (Idx cidx _, _) (LogS x)) = return $ PolyLog (Idx cidx v) x
expressPolyExpr _ _ _ (AnnoS (Idx _ (VarT v)) (Idx cidx _, _) (StrS x)) = return $ PolyStr (Idx cidx v) x
expressPolyExpr _ _ _ (AnnoS (Idx _ t@(VarT _)) (Idx cidx _, _) UniS) = return $ PolyNull (Idx cidx t)
-- Null carries the full type it stands in for. Earlier this was just
-- a @TVar@ extracted via @innerNullVar@, but a bare constructor name
-- loses the arg list of parameterised aliases. For
-- @Null :: ?(BTree Int)@, peeling Optional and discarding the @[Int]@
-- yielded just @BTree@, whose downstream @inferConcreteVar@ chain
-- resolved to an @FV BTree "tuple"@ FVar that the typeFof of the
-- enclosing tuple/record/if surfaced as a bare-args VarF -- which
-- @makeSerialAST@'s @dispatchVarF@ then could not interpret.
--
-- Now @innerNullType@ returns the full inner @Type@ (still peeling
-- Optional wrappers for nested @?(?T)@ idempotence). Downstream
-- consumers run @inferConcreteType@ on the stored type and surface
-- the proper @TypeF@ at typeFof time.
expressPolyExpr _ _ _ (AnnoS (Idx midx t) (Idx cidx _, _) NullS) =
  case canonNullType t of
    Just t' -> return $ PolyNull (Idx cidx t')
    Nothing -> MM.throwSourcedError midx $
      "Cannot infer type for Null literal of type"
      <+> dquotes (pretty t) <> "."
      <+> "This usually means an unsolved generic term escaped typechecking."
  where
    -- Require the outermost to be @OptionalT@ (which the typecheck
    -- always supplies for @NullS@ via @synthE NullS@'s
    -- @OptionalU v@). Then collapse nested Optionals (idempotence:
    -- ?(?T) == ?T) and require a concrete head underneath. The stored
    -- type is the canonical @?T@ where @T@ is @VarT@ or
    -- @AppT (VarT _) _@, so downstream typeFof presents a single
    -- Optional layer matching the slot's expected shape.
    canonNullType (OptionalT inner) = OptionalT <$> peelOpt inner
    canonNullType _                 = Nothing
    peelOpt (OptionalT inner)       = peelOpt inner
    peelOpt t'@(VarT _)             = Just t'
    peelOpt t'@(AppT (VarT _) _)    = Just t'
    peelOpt _                       = Nothing
-- A list literal at type `Foo a1 ... an` has exactly one element-type arg
-- (the rest, if any, are Nat-kinded phantom dims, e.g. for Vector n a).
-- Extract the single non-Nat arg for typing children, but PRESERVE the
-- full args list in PolyList so phantom dims survive into language code
-- generators (Nat positions render as mempty in macro expansion).
expressPolyExpr _ parentLang pc (AnnoS (Idx midx (AppT (VarT v) ts)) (Idx cidx lang, args) (LstS xs))
  | [t] <- filter (not . isNatTypeT) ts = do
      xs' <- mapM (\x -> expressPolyExprWrap lang (mkIdx x t) x) xs
      let e = PolyList (Idx cidx v) (map (Idx cidx) ts) xs'
      return $ expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e
-- A list literal whose recorded type head is a type-alias (e.g.
-- @type Pat = [Pat]@ used at the bare-name position, recorded as
-- @VarT "Pat"@ rather than @AppT (VarT "List") [...]@). The pattern
-- above matches only when the head is already the list constructor;
-- when the head is an alias, reduce one step and retry. Mirrors the
-- @TupS@ and @NamS@ reduce-and-retry fallbacks elsewhere in this
-- function.
expressPolyExpr _ pl pc (AnnoS (Idx midx t) c e@(LstS _)) = do
  scope <- MM.getGeneralScope midx
  case reduceType scope t of
    Just t' -> expressPolyExprWrap pl pc (AnnoS (Idx midx t') c e)
    Nothing -> MM.throwSourcedError midx "Expected a list type"
-- Same guard as expressCore: only treat the head as a tuple when it IS the
-- TupleN constructor. A bare @length ts == length xs@ match would catch
-- user aliases like @FixedPair (n :: Nat) a@ whose type args (NatLit n,
-- a) are NOT the slot types; the cross-language wrap inside
-- expressContainer would then use a Nat literal as the foreign call's
-- parent type and surface downstream as @makeSerialAST' error on type:
-- NatLitF n@. Falling through to the reduce-and-retry below expands the
-- alias to its body's tuple constructor.
expressPolyExpr _ parentLang pc (AnnoS (Idx midx (AppT (VarT v) ts)) (Idx cidx lang, args) (TupS xs))
  | v == BT.tuple (length xs)
  , length ts == length xs = do
      let idxTs = zipWith mkIdx xs ts
      xs' <- fromJust <$> safeZipWithM (expressPolyExprWrap lang) idxTs xs
      let e = PolyTuple (Idx cidx v) (fromJust $ safeZip idxTs xs')
      return $ expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e
-- A tuple literal whose recorded type head is a type-alias (e.g.
-- @type LL a = (a, ?(LL a))@). The alias's argument list does NOT zip
-- 1:1 with the literal's entries -- the alias body does. Reduce one
-- step (LL Int -> (Int, ?(LL Int))) and retry; the reduced head is
-- the underlying tuple constructor whose arity matches the literal.
-- Mirrors the same reduce-and-retry the NamS branch performs for
-- record-shaped aliases below.
expressPolyExpr _ pl pc (AnnoS (Idx midx t) c e@(TupS _)) = do
  scope <- MM.getGeneralScope midx
  case reduceType scope t of
    Just t' -> expressPolyExprWrap pl pc (AnnoS (Idx midx t') c e)
    Nothing -> MM.throwSourcedError midx "Expected a tuple type"
expressPolyExpr _ parentLang pc (AnnoS (Idx midx (NamT o v ps rs)) (Idx cidx lang, args) (NamS entries)) = do
  -- C3 invariant: see comment at expressCore NamT above.
  assertRecordKeyOrder midx entries rs
  let tsIdx = zipWith mkIdx (map snd entries) (map snd rs)
  xs' <- fromJust <$> safeZipWithM (expressPolyExprWrap lang) tsIdx (map snd entries)
  let e = PolyRecord o (Idx cidx v) (map (Idx cidx) ps) (zip (map fst rs) (zip tsIdx xs'))
  return $ expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e
expressPolyExpr _ pl pc (AnnoS (Idx i t) c e@(NamS _)) = do
  scope <- MM.getGeneralScope i
  case reduceType scope t of
    (Just t') -> expressPolyExprWrap pl pc (AnnoS (Idx i t') c e)
    Nothing -> error "Expected a record type"
-- Recursive call used as a value (not applied via AppS)
expressPolyExpr _ parentLang _ (AnnoS (Idx i c) (Idx _cidx _, _) (CallS v)) = do
  (mid, crossLang) <- lookupRecursiveTarget parentLang v
  -- Strip EffectT from return type (serial manifolds force thunks)
  case c of
    FunT inputs (EffectT effs out) ->
      return . PolyDoBlock (Idx i (EffectT effs out))
        $ PolyExe (Idx i (FunT inputs out)) (RecCallP mid crossLang)
    _ ->
      return $ PolyExe (Idx i c) (RecCallP mid crossLang)
expressPolyExpr _ _ _ (AnnoS (Idx i _) _ (AppS (AnnoS _ _ (BndS v)) _)) =
  MM.throwSourcedError i $
    "Undefined function" <+> dquotes (pretty v) <> ", did you forget an import?"
expressPolyExpr _ _ _ (AnnoS _ _ (AppS (AnnoS _ _ (LamS vs _)) _)) =
  error $ "All applications of lambdas should have been eliminated of length " <> show (length vs)
expressPolyExpr _ parentLang pc (AnnoS (Idx midx t) (Idx cidx lang, args) (IfS cond thenE elseE)) = do
  let boolType = VarT (TV "Bool")
  cond' <- expressPolyExprWrap lang (mkIdx cond boolType) cond
  thenE' <- expressPolyExprWrap lang (mkIdx thenE t) thenE
  elseE' <- expressPolyExprWrap lang (mkIdx elseE t) elseE
  let e = PolyIf cond' thenE' elseE'
  return $ expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e
expressPolyExpr _ parentLang pc (AnnoS (Idx midx t) (Idx cidx lang, args) (DoBlockS x)) = do
  -- The inner expression has the unwrapped type (without EffectT).
  -- Passing EffectT through would cause cross-language calls to generate
  -- effect-wrapped return types for pure functions.
  let innerT = case t of EffectT _ inner -> inner; _ -> t
  x' <- expressPolyExprWrap lang (mkIdx x innerT) x
  let e = PolyDoBlock (Idx cidx t) x'
  return $ expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e
expressPolyExpr _ parentLang _ (AnnoS (Idx _ t) (Idx cidx _, _) (CoerceS coercion x)) = do
  let innerType = unapplyCoercion coercion t
  x' <- expressPolyExprWrap parentLang (Idx cidx innerType) x
  return $ PolyCoerce coercion (Idx cidx t) x'
expressPolyExpr _ parentLang _ (AnnoS (Idx _ t) (Idx cidx _lang, _) (EvalS x)) = do
  -- Always use pushForceIntoRemote: if the inner expression contains a
  -- PolyRemoteInterface (cross-language call), it strips EffectT Set.empty so the remote
  -- pool forces the thunk and serializes the concrete result. If no
  -- PolyRemoteInterface is found (same-language), it falls back to PolyEval.
  -- We cannot rely on parentLang /= lang because Realize.hs assigns both to
  -- the same language when the EvalS node lives in a same-language context,
  -- even if the inner expression calls into a foreign language.
  x' <- expressPolyExprWrap parentLang (Idx cidx t) x
  return $ pushForceIntoRemote (Idx cidx t) x'
expressPolyExpr _ parentLang pc (AnnoS (Idx midx t) (Idx cidx lang, args) (IntrinsicS intr xs)) = do
  xs' <- mapM (\x@(AnnoS (Idx xi xt) _ _) -> expressPolyExprWrap lang (Idx xi xt) x) xs
  let e = PolyIntrinsic (Idx cidx t) intr xs'
  return $ expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e

-- Nullary source/pattern call (e.g., clockResNs :: {Int})
expressPolyExpr
  findRemote
  parentLang
  pc
  f@(AnnoS (Idx midx _) (Idx _ callLang, args) (ExeS _))
    | isLocal = do
        call <- expressPolyApp parentLang f []
        return
          . PolyManifold callLang midx (ManifoldFull (map unvalue args))
          $ call
    | otherwise = do
        call <- expressPolyApp callLang f []
        return
          . PolyManifold parentLang midx (ManifoldFull (map unvalue args))
          . PolyReturn
          . PolyApp
            ( PolyRemoteInterface callLang pc [] (fromJust remote)
                . PolyManifold callLang midx (ManifoldFull (map unvalue args))
                $ call
            )
          $ [PolyBndVar (A parentLang) i | Arg i _ <- args]
    where
      remote = findRemote parentLang callLang
      isLocal = isNothing remote
expressPolyExpr _ _ parentType x@(AnnoS (Idx m t) _ _) = do
  MM.sayVVV "Bad case"
  MM.sayVVV $ "  t :: " <> pretty t
  name' <- MM.metaName m
  case name' of
    (Just v) ->
      MM.throwSourcedError m $
        "Missing concrete:"
          <> "\n  t:" <+> viaShow t
          <> "\n  v:" <+> pretty v
          <> "\n parentType:" <+> pretty parentType
          <> "\n x:" <+> pretty x
    Nothing ->
      MM.throwSourcedError m $
        "Missing concrete in unnamed function:"
          <> "\n  t:" <+> pretty t
          <> "\n parentType:" <+> pretty parentType
          <> "\n x:" <+> pretty x

expressPolyApp ::
  Lang ->
  AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) ->
  [PolyExpr] ->
  MorlocMonad PolyExpr
expressPolyApp _ (AnnoS g _ (ExeS (SrcCall src))) xs =
  return . PolyReturn $ PolyApp (PolyExe g (SrcCallP src)) xs
expressPolyApp _ (AnnoS g _ (ExeS (PatCall pat))) xs =
  return . PolyReturn $ PolyApp (PolyExe g (PatCallP pat)) xs
expressPolyApp lang f@(AnnoS g@(Idx i _) _ (AppS _ _)) es = do
  fe <- expressPolyExprWrap lang g f
  return
    . PolyLet i fe
    . PolyReturn
    $ PolyApp (PolyLetVar g i) es
expressPolyApp _ (AnnoS g (_, args) (BndS v)) xs = do
  case [j | (Arg j u) <- args, u == v] of
    [j] -> return . PolyReturn $ PolyApp (PolyExe g (LocalCallP j)) xs
    _ -> error "Unreachable? BndS value should have been wired uniquely to args previously"
expressPolyApp parentLang (AnnoS (Idx i t) _ (CallS v)) xs = do
  (mid, crossLang) <- lookupRecursiveTarget parentLang v
  -- Serial manifolds force thunks before serializing, so strip EffectT from the
  -- return type and wrap in PolyDoBlock to reconstruct the thunk after deserializing.
  case t of
    FunT inputs (EffectT effs out) ->
      return . PolyReturn
        . PolyDoBlock (Idx i (EffectT effs out))
        $ PolyApp (PolyExe (Idx i (FunT inputs out)) (RecCallP mid crossLang)) xs
    _ ->
      return . PolyReturn $ PolyApp (PolyExe (Idx i t) (RecCallP mid crossLang)) xs
expressPolyApp _ (AnnoS _ _ (LamS _ _)) _ = error "unexpected LamS - should have been handled"
expressPolyApp _ (AnnoS _ _ (VarS _ _)) _ = error "unexpected VarS - should have been substituted"
expressPolyApp _ (AnnoS (Idx i t) _ e) _ =
  MM.throwSourcedError i $
    "expressPolyApp: function position has unexpected expression form:"
      <+> tagExpr e <+> "of type" <+> pretty t
  where
    tagExpr :: ExprS (Indexed Type) One (Indexed Lang, [Arg EVar]) -> MDoc
    tagExpr (UniS) = "UniS"
    tagExpr (NullS) = "NullS"
    tagExpr (BndS v) = "BndS" <+> pretty v
    tagExpr (LetBndS v) = "LetBndS" <+> pretty v
    tagExpr (CallS v) = "CallS" <+> pretty v
    tagExpr (ExeS _) = "ExeS"
    tagExpr (LamS _ _) = "LamS"
    tagExpr (AppS _ _) = "AppS"
    tagExpr (LstS _) = "LstS"
    tagExpr (TupS _) = "TupS"
    tagExpr (NamS _) = "NamS"
    tagExpr (RealS _ _) = "RealS"
    tagExpr (IntS _ _) = "IntS"
    tagExpr (LogS _) = "LogS"
    tagExpr (StrS _) = "StrS"
    tagExpr (DoBlockS _) = "DoBlockS"
    tagExpr (EvalS _) = "EvalS"
    tagExpr (CoerceS _ _) = "CoerceS"
    tagExpr (IfS _ _ _) = "IfS"
    tagExpr (LetS v _ _) = "LetS" <+> pretty v
    tagExpr (VarS v _) = "VarS" <+> pretty v
    tagExpr (IntrinsicS _ _) = "IntrinsicS"

expressContainer ::
  Indexed Type -> Indexed Lang -> Indexed Lang -> [Arg EVar] -> PolyExpr -> PolyExpr
expressContainer pc (Idx midx parentLang) (Idx _ lang) args e
  | parentLang /= lang =
      PolyApp
        ( PolyRemoteInterface lang pc [i | Arg i _ <- args] ForeignCall
            . PolyManifold lang midx (ManifoldFull (map unvalue args))
            $ wrapReturn e
        )
        $ [PolyBndVar (A parentLang) i | Arg i _ <- args]
  | otherwise = e
  where
    -- Push PolyReturn through PolyLet to the tail. Skip wrap if the tail
    -- is already a PolyReturn or a PolyManifold (whose own body produces a
    -- return when MonoManifold is unwrapped during serialize). Avoids
    -- `return(return(...))` in generated pools.
    wrapReturn (PolyLet i e1 e2) = PolyLet i e1 (wrapReturn e2)
    wrapReturn r@(PolyReturn _) = r
    wrapReturn r@(PolyManifold _ _ _ _) = r
    wrapReturn x = PolyReturn x

unvalue :: Arg a -> Arg None
unvalue (Arg i _) = Arg i None

{- | Handle cross-language force by stripping EffectT from the callee's function
return type. The source function actually returns the unwrapped type; the
EffectT wrapper is a type-system abstraction. By removing it, Common.hs won't
auto-wrap in DoBlockN, so the raw value is serialized directly.
If no PolyRemoteInterface is found, falls back to wrapping in PolyEval.
-}
pushForceIntoRemote :: Indexed Type -> PolyExpr -> PolyExpr
pushForceIntoRemote t = go
  where
    go (PolyManifold l m f e) = PolyManifold l m f (go e)
    go (PolyReturn e) = PolyReturn (go e)
    go (PolyLet i e1 e2) = PolyLet i e1 (go e2)
    go (PolyApp (PolyRemoteInterface lang _ args remote callee) xs) =
      PolyApp (PolyRemoteInterface lang t args remote (stripThunkReturn callee)) xs
    go e = PolyEval t e -- fallback for local expressions

    -- Strip EffectT from the function's return type inside the callee manifold
    stripThunkReturn (PolyManifold l m f body) = PolyManifold l m f (stripInBody body)
    stripThunkReturn e = stripInBody e

    stripInBody (PolyReturn e) = PolyReturn (stripInExe e)
    stripInBody (PolyLet i e1 e2) = PolyLet i e1 (stripInBody e2)
    stripInBody e = stripInExe e

    stripInExe (PolyApp (PolyExe (Idx gidx (FunT inputs (EffectT _ out))) exe) xs) =
      PolyApp (PolyExe (Idx gidx (FunT inputs out)) exe) xs
    stripInExe (PolyApp (PolyExe (Idx gidx (EffectT _ out)) exe) xs) =
      PolyApp (PolyExe (Idx gidx out) exe) xs
    stripInExe e = e

-- | Resolve a function name to its manifold ID and determine if the call is cross-language.
-- Returns (manifold ID, Nothing) for same-pool calls, (manifold ID, Just targetLang) for foreign calls.
-- Searches all manifolds in stateName, not just exports, to support non-exported recursive helpers.
lookupRecursiveTarget :: Lang -> EVar -> MorlocMonad (Int, Maybe Lang)
lookupRecursiveTarget parentLang v = do
  nameMap <- MM.gets stateName
  langMap <- MM.gets stateManifoldLang
  -- Filter to concrete manifolds only (those in langMap) to avoid picking up
  -- general/polymorphic indices that don't have serial manifold definitions
  let reverseMap = Map.fromList [(name, idx) | (idx, name) <- Map.toList nameMap, Map.member idx langMap]
  case Map.lookup v reverseMap of
    (Just mid) -> do
      let crossLang = case Map.lookup mid langMap of
            Just tl | tl /= parentLang -> Just tl
            _ -> Nothing
      return (mid, crossLang)
    Nothing -> MM.throwSystemError $ "Cannot resolve recursive call to" <+> pretty v

bindVarIds :: [Int] -> [Three Lang Type (Indexed Type)] -> [PolyExpr]
bindVarIds [] [] = []
bindVarIds (i : args) (t : types) = PolyBndVar t i : bindVarIds args types
bindVarIds [] ts = error $ "bindVarIds: too few arguments: " <> show ts
bindVarIds _ [] = error "bindVarIds: too few types"
