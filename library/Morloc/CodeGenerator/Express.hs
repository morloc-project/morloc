{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.CodeGenerator.Express
Description : Convert parameterized AST to polymorphic manifold trees
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc.CodeGenerator.Express
  ( express
  ) where

import Morloc.CodeGenerator.Infer
import Morloc.CodeGenerator.Namespace
import Morloc.Data.Doc
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.Data.Map as Map
import qualified Morloc.Monad as MM
import qualified Morloc.TypeEval as TE

mkIdx :: AnnoS g One (Indexed c, d) -> Type -> Indexed Type
mkIdx (AnnoS _ (Idx i _, _) _) = Idx i

setManifoldConfig ::
  Int ->
  AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) ->
  MorlocMonad ()
setManifoldConfig midx (AnnoS _ _ (AppS (AnnoS (Idx fidx _) _ (VarS _ _)) _)) = linkConfigIndex midx fidx
setManifoldConfig midx (AnnoS _ _ (AppS (AnnoS (Idx fidx _) _ (ExeS _)) _)) = linkConfigIndex midx fidx
setManifoldConfig midx (AnnoS _ _ (AppS e _)) = setManifoldConfig midx e
setManifoldConfig midx (AnnoS _ _ (LamS _ e)) = setManifoldConfig midx e
setManifoldConfig _ _ = return ()

linkConfigIndex :: Int -> Int -> MorlocMonad ()
linkConfigIndex midx fidx = do
  s <- MM.get
  case Map.lookup fidx (stateManifoldConfig s) of
    Nothing -> return ()
    (Just mconfig) -> do
      MM.sayVVV $ "Copy manifold config from" <+> pretty fidx <+> "to" <+> pretty midx
      MM.put (s {stateManifoldConfig = Map.insert midx mconfig (stateManifoldConfig s)})

propagateScope :: Int -> Int -> MorlocMonad ()
propagateScope calleeIdx appIdx = do
  s <- MM.get
  case GMap.yIsX calleeIdx appIdx (stateConcreteTypedefs s) of
    (Just gmap') -> MM.put $ s { stateConcreteTypedefs = gmap' }
    Nothing -> return ()

express :: AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) -> MorlocMonad PolyHead
express (AnnoS (Idx midx c@(FunT inputs _)) (Idx cidx lang, _) (ExeS exe)) = do
  MM.sayVVV $ "express CallS (midx=" <> pretty midx <> "," <+> "cidx=" <> pretty cidx <> "):"
  ids <- MM.takeFromCounter (length inputs)
  exe' <- case exe of
    (SrcCall src) -> return $ SrcCallP src
    (PatCall pat) -> return $ PatCallP pat
  let lambdaVals = fromJust $ safeZipWith PolyBndVar (map (C . Idx cidx) inputs) ids
  return
    . PolyHead lang midx [Arg i None | i <- ids]
    . PolyReturn
    $ PolyApp (PolyExe (Idx midx c) exe') lambdaVals

express (AnnoS (Idx midx _) (_, lambdaArgs) (LamS _ e@(AnnoS (Idx _ applicationType) (c, _) x))) = do
  MM.sayVVV $ "express LamS (midx=" <> pretty midx <> "):"
  setManifoldConfig midx e
  express (AnnoS (Idx midx applicationType) (c, lambdaArgs) x)
express (AnnoS (Idx midx (AppT (VarT v) [t])) (Idx cidx lang, args) (LstS xs)) = do
  MM.sayVVV $ "express LstS"
  xs' <- mapM (\x -> expressPolyExprWrap lang (mkIdx x t) x) xs
  let x = PolyList (Idx cidx v) (Idx cidx t) xs'
  return $ PolyHead lang midx [Arg i None | Arg i _ <- args] (PolyReturn x)
express (AnnoS (Idx _ t) _ (LstS _)) = error $ "Invalid list form: " <> show t
express (AnnoS t@(Idx midx (AppT (VarT v) ts)) (Idx cidx lang, args) (TupS xs)) = do
  MM.sayVVV $ "express TupS:" <+> pretty t
  let idxTs = zipWith mkIdx xs ts
  xs' <- fromJust <$> safeZipWithM (expressPolyExprWrap lang) idxTs xs
  let x = PolyTuple (Idx cidx v) (fromJust $ safeZip idxTs xs')
  return $ PolyHead lang midx [Arg i None | Arg i _ <- args] (PolyReturn x)
express (AnnoS g _ (TupS _)) = error $ "Invalid tuple form: " <> show g
express (AnnoS (Idx midx t@(NamT o v ps rs)) (Idx cidx lang, args) (NamS entries)) = do
  MM.sayVVV $ "express NamT:" <+> pretty t
  let idxTypes = zipWith mkIdx (map snd entries) (map snd rs)
  xs' <- fromJust <$> safeZipWithM (expressPolyExprWrap lang) idxTypes (map snd entries)
  let x = PolyRecord o (Idx cidx v) (map (Idx cidx) ps) (zip (map fst rs) (zip idxTypes xs'))
  return $ PolyHead lang midx [Arg i None | Arg i _ <- args] (PolyReturn x)

express (AnnoS (Idx midx t) (Idx cidx lang, args) (NamS entries)) = do
  MM.sayVVV $ "express NamT expand:" <+> pretty t
  mayT <- evalGeneralStep midx (type2typeu t)
  case mayT of
    (Just t') -> express (AnnoS (Idx midx (typeOf t')) (Idx cidx lang, args) (NamS entries))
    Nothing -> MM.throwSourcedError midx $ "Missing concrete:" <+> "t=" <> pretty t

express e = do
  MM.sayVVV "express default"
  expressDefault e

reduceType :: Scope -> Type -> Maybe Type
reduceType scope t0 =
  let tu0 = type2typeu t0
   in case TE.evaluateStep scope tu0 of
        (Just tu1) -> if tu0 == tu1 then Nothing else Just (typeOf tu1)
        Nothing -> Nothing

expressDefault :: AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) -> MorlocMonad PolyHead
expressDefault e@(AnnoS (Idx midx t) (Idx cidx lang, args) _) =
  PolyHead lang midx [Arg i None | Arg i _ <- args] <$> expressPolyExprWrap lang (Idx cidx t) e

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
        MM.sayVVV "case #4"
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

        MM.sayVVV $ "  xsInfo:" <+> pretty xsInfo

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
      partialExpress (AnnoS (Idx _ t) (Idx cidx argLang, args@[Arg idx _]) (BndS v)) = do
        MM.sayVVV $
          "partialExpress case #0:" <+> "x="
            <> pretty v <+> "cidx="
            <> pretty cidx <+> "t =" <+> pretty t
            <> "\n  parentLang:"
            <> pretty parentLang
            <> "\n  callLang:"
            <> pretty callLang
            <> "\n  argLang:"
            <> pretty argLang
            <> "\n  args:"
            <> pretty args
        let x' = PolyBndVar (C (Idx cidx t)) idx
        return ([idx], Nothing, x')
      partialExpress x@(AnnoS (Idx _ t) (Idx cidx argLang, args) _)
        | argLang == callLang = do
            MM.sayVVV $
              "partialExpress case #2:" <+> "cidx="
                <> pretty cidx <+> "t =" <+> pretty t
                <> "\n  parentLang:"
                <> pretty parentLang
                <> "\n  callLang:"
                <> pretty callLang
                <> "\n  argLang:"
                <> pretty argLang
                <> "\n  args:"
                <> pretty args
            let argParentType = Idx cidx t
            x' <- expressPolyExprWrap argLang argParentType x
            return ([i | Arg i _ <- args], Nothing, x')
        | otherwise = do
            MM.sayVVV $
              "partialExpress case #1:" <+> "cidx="
                <> pretty cidx <+> "t =" <+> pretty t
                <> "\n  parentLang:"
                <> pretty parentLang
                <> "\n  callLang:"
                <> pretty callLang
                <> "\n  argLang:"
                <> pretty argLang
                <> "\n  args:"
                <> pretty args
            let argparentType = Idx cidx t
            letVal <- expressPolyExprWrap argLang argparentType x
            idx <- MM.getCounter
            MM.sayVVV $ "making index in partialExpress #1:" <+> pretty idx

            let x' = PolyLetVar (Idx cidx t) idx
            return ([idx], Just (idx, letVal), x')
expressPolyExpr _ _ _ (AnnoS lambdaType@(Idx midx _) (Idx _ lang, manifoldArguments) (LamS vs body)) = do
  MM.sayVVV $ "expressPolyExpr LamS:" <+> pretty lambdaType

  body' <- expressPolyExprWrap lang lambdaType body

  inputTypes <- case val lambdaType of
    (FunT ts _) -> return ts
    _ -> return []

  let contextArguments = map unvalue $ take (length manifoldArguments - length vs) manifoldArguments
      boundArguments = map unvalue $ drop (length contextArguments) manifoldArguments
      typeBoundArguments = fromJust $ safeZipWith (\t (Arg i _) -> Arg i (Just t)) inputTypes boundArguments

  MM.sayVVV $
    "Express lambda:"
      <> "\n  vs:" <+> pretty vs
      <> "\n  lambdaType:" <+> pretty lambdaType
      <> "\n  manifoldArguments:" <+> list (map pretty manifoldArguments)
      <> "\n  contextArguments:" <+> list (map pretty contextArguments)
      <> "\n  boundArguments" <+> list (map pretty typeBoundArguments)

  return
    . PolyManifold lang midx (ManifoldPart contextArguments typeBoundArguments)
    . PolyReturn
    $ body'

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
        xsExpr <- zipWithM (expressPolyExprWrap callLang) (map (Idx cidxCall) inputs) xs

        func <- expressPolyApp parentLang f xsExpr
        return
          . PolyManifold callLang midx (ManifoldFull (map unvalue args))
          $ func

    | not isLocal = do
        propagateScope gidxCall midx
        let idxInputTypes = zipWith mkIdx xs inputs
        mayXs <- safeZipWithM (expressPolyExprWrap callLang) idxInputTypes xs
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
  MM.sayVVV $ "express' VarS" <+> parens (pretty v) <+> "::" <+> pretty c
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

expressPolyExpr findRemote parentLang parentType
    (AnnoS _ (Idx cidx lang, _) (LetS v e1 e2)) = do
  let bodyArgs = case e2 of AnnoS _ (_, args) _ -> args
      letId = case [j | Arg j v' <- bodyArgs, v' == v] of
                [j] -> j
                _   -> error "Bug: let-bound variable not found in body annotation"
  e1' <- expressPolyExprWrap lang parentType e1
  e2' <- expressPolyExprWrap lang parentType e2
  return $ PolyLet letId e1' e2'

expressPolyExpr _ _ _ (AnnoS (Idx _ (VarT v)) (Idx cidx _, _) (RealS x)) = return $ PolyReal (Idx cidx v) x
expressPolyExpr _ _ _ (AnnoS (Idx _ (VarT v)) (Idx cidx _, _) (IntS x)) = return $ PolyInt (Idx cidx v) x
expressPolyExpr _ _ _ (AnnoS (Idx _ (VarT v)) (Idx cidx _, _) (LogS x)) = return $ PolyLog (Idx cidx v) x
expressPolyExpr _ _ _ (AnnoS (Idx _ (VarT v)) (Idx cidx _, _) (StrS x)) = return $ PolyStr (Idx cidx v) x
expressPolyExpr _ _ _ (AnnoS (Idx _ (VarT v)) (Idx cidx _, _) UniS) = return $ PolyNull (Idx cidx v)
expressPolyExpr _ parentLang pc (AnnoS (Idx midx (AppT (VarT v) [t])) (Idx cidx lang, args) (LstS xs)) = do
  xs' <- mapM (\x -> expressPolyExprWrap lang (mkIdx x t) x) xs
  let e = PolyList (Idx cidx v) (Idx cidx t) xs'
  return $ expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e
expressPolyExpr _ _ _ (AnnoS _ _ (LstS _)) = error "LstS can only be (AppP (VarP _) [_]) type"
expressPolyExpr _ parentLang pc (AnnoS (Idx midx (AppT (VarT v) ts)) (Idx cidx lang, args) (TupS xs)) = do
  let idxTs = zipWith mkIdx xs ts
  xs' <- fromJust <$> safeZipWithM (expressPolyExprWrap lang) idxTs xs
  let e = PolyTuple (Idx cidx v) (fromJust $ safeZip idxTs xs')
  return $ expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e

expressPolyExpr _ parentLang pc (AnnoS (Idx midx (NamT o v ps rs)) (Idx cidx lang, args) (NamS entries)) = do
  let tsIdx = zipWith mkIdx (map snd entries) (map snd rs)
  xs' <- fromJust <$> safeZipWithM (expressPolyExprWrap lang) tsIdx (map snd entries)
  let e = PolyRecord o (Idx cidx v) (map (Idx cidx) ps) (zip (map fst rs) (zip tsIdx xs'))
  return $ expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e

expressPolyExpr _ pl pc (AnnoS (Idx i t) c e@(NamS _)) = do
  scope <- MM.getGeneralScope i
  case reduceType scope t of
    (Just t') -> expressPolyExprWrap pl pc (AnnoS (Idx i t') c e)
    Nothing -> error "Expected a record type"
expressPolyExpr _ _ _ (AnnoS (Idx i _) _ (AppS (AnnoS _ _ (BndS v)) _)) =
  MM.throwSourcedError i $ "Undefined function" <+> dquotes (pretty v) <> ", did you forget an import?"

expressPolyExpr _ _ _ (AnnoS _ _ (AppS (AnnoS _ _ (LamS vs _)) _)) =
  error $ "All applications of lambdas should have been eliminated of length " <> show (length vs)
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

expressPolyApp _ (AnnoS _ _ (LamS _ _)) _ = error "unexpected LamS - should have been handled"
expressPolyApp _ (AnnoS _ _ (VarS _ _)) _ = error "unexpected VarS - should have been substituted"
expressPolyApp _ _ _ = error "Unreachable? This does not seem to be applicable"

expressContainer ::
  Indexed Type -> Indexed Lang -> Indexed Lang -> [Arg EVar] -> PolyExpr -> PolyExpr
expressContainer pc (Idx midx parentLang) (Idx _ lang) args e
  | parentLang /= lang =
      PolyApp
        ( PolyRemoteInterface lang pc [i | Arg i _ <- args] ForeignCall
            . PolyManifold lang midx (ManifoldFull (map unvalue args))
            . PolyReturn
            $ e
        )
        $ [PolyBndVar (A parentLang) i | Arg i _ <- args]
  | otherwise = e

unvalue :: Arg a -> Arg None
unvalue (Arg i _) = Arg i None

bindVarIds :: [Int] -> [Three Lang Type (Indexed Type)] -> [PolyExpr]
bindVarIds [] [] = []
bindVarIds (i : args) (t : types) = PolyBndVar t i : bindVarIds args types
bindVarIds [] ts = error $ "bindVarIds: too few arguments: " <> show ts
bindVarIds _ [] = error "bindVarIds: too few types"
