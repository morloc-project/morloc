{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Morloc.CodeGenerator.Generate
Description : Translate AST forests into target language source code
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}

module Morloc.CodeGenerator.Generate
(
    generate
  , generatePools
) where

import Morloc.CodeGenerator.Namespace
import Morloc.Data.Doc
import qualified Morloc.Config as MC
import qualified Morloc.Data.Text as MT
import Data.Text (Text)
import qualified Morloc.Data.Map as Map
import qualified Morloc.Monad as MM
import qualified Morloc.CodeGenerator.Nexus as Nexus
import qualified Morloc.TypeEval as TE
import Morloc.CodeGenerator.Infer

import qualified Morloc.CodeGenerator.Grammars.Translator.Cpp as Cpp
import qualified Morloc.CodeGenerator.Grammars.Translator.R as R
import qualified Morloc.CodeGenerator.Grammars.Translator.Python3 as Python3
import qualified Morloc.CodeGenerator.Serial as Serial


-- | Translate typed, abstract syntax forests into compilable code
generate
  :: [(AnnoS (Indexed Type) One (), CmdDocSet)]
  -> [(AnnoS (Indexed Type) One (Indexed Lang), CmdDocSet)]
  -> MorlocMonad (Script, [Script])
  -- ^ the nexus code and the source code for each language pool
generate gASTs rASTs = do
  -- build nexus
  -- -----------
  -- Each nexus subcommand calls one function from one one pool.
  -- The call passes the pool an index for the function (manifold) that will be called.

  nexus <- mapM makeFData rASTs >>= Nexus.generate gASTs

  -- initialize counter for use in express
  MM.startCounter

  -- for each language, collect all functions into one "pool"
  pools <- generatePools (map fst rASTs) >>= mapM (uncurry encode)

  return (nexus, pools)

-- Prep the data needed for each subcommand in the nexus
makeFData :: (AnnoS (Indexed Type) One (Indexed Lang), CmdDocSet) -> MorlocMonad (Type, Int, Lang, CmdDocSet, [Socket])
makeFData (e@(AnnoS (Idx i t) (Idx _ lang) _), d) = do
  sockets <- findSockets e
  return (t, i, lang, d, sockets)

findSockets :: AnnoS e One (Indexed Lang) -> MorlocMonad [Socket]
findSockets rAST = do
  config <- MM.ask
  return . map (MC.setupServerAndSocket config) . unique $ findAllLangsSAnno rAST


findAllLangsSAnno :: AnnoS e One (Indexed Lang) -> [Lang]
findAllLangsSAnno (AnnoS _ (Idx _ lang) e) = lang : findAllLangsExpr e where
  findAllLangsExpr (VarS _ (One x)) = findAllLangsSAnno x
  findAllLangsExpr (AppS x xs) = concatMap findAllLangsSAnno (x:xs)
  findAllLangsExpr (LamS _ x) = findAllLangsSAnno x
  findAllLangsExpr (LstS xs) = concatMap findAllLangsSAnno xs
  findAllLangsExpr (TupS xs) = concatMap findAllLangsSAnno xs
  findAllLangsExpr (NamS rs) = concatMap (findAllLangsSAnno . snd) rs
  findAllLangsExpr _ = []

-- | Do everything except language specific code generation.
generatePools :: [AnnoS (Indexed Type) One (Indexed Lang)] -> MorlocMonad [(Lang, [SerialManifold])]
generatePools rASTs = do
  -- for each language, collect all functions into one "pool"

    -- thread arguments across the tree
    mapM parameterize rASTs

    -- convert from AST to manifold tree
    >>= mapM express

    -- Separate the call trees into mono-lingual segments terminated in
    -- primitives or foreign calls.
    >>= mapM segment |>> concat

    >>= mapM serialize

    -- Gather segments into pools, currently this entails gathering all
    -- segments from a given language into one pool. Later it may be more
    -- nuanced.
    |>> pool

-- | Add arguments that are required for each term. Unneeded arguments are
-- removed at each step.
parameterize
  :: AnnoS (Indexed Type) One (Indexed Lang)
  -> MorlocMonad (AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]))
parameterize (AnnoS m@(Idx _ (FunT inputs _)) c (LamS vs x)) = do
  MM.sayVVV "Entering parameterize LamS"
  ids <- MM.takeFromCounter (length inputs)
  let args0 = fromJust $ safeZipWith Arg ids vs
  x' <- parameterize' args0 x
  return $ AnnoS m (c, args0) (LamS vs x')
parameterize (AnnoS m@(Idx _ (FunT inputs _)) c@(Idx _ lang) (BndS v)) = do
  MM.sayVVV $ "Entering parameterize VarS function - " <> pretty v <> "@" <> pretty lang
  ids <- MM.takeFromCounter (length inputs)
  let vs = map EV (freshVarsAZ [])
      args0 = fromJust $ safeZipWith Arg ids vs
  return $ AnnoS m (c, args0) (BndS v)
parameterize x = do
  MM.sayVVV "Entering parameterize Other"
  parameterize' [] x

parameterize'
  :: [Arg EVar] -- arguments in parental scope (child needn't retain them)
  -> AnnoS (Indexed Type) One (Indexed Lang)
  -> MorlocMonad (AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]))
-- primitives, no arguments are required for a primitive, so empty lists
parameterize' _ (AnnoS g c UniS) = return $ AnnoS g (c, []) UniS
parameterize' _ (AnnoS g c (RealS x)) = return (AnnoS g (c, []) (RealS x))
parameterize' _ (AnnoS g c (IntS x))  = return (AnnoS g (c, []) (IntS x))
parameterize' _ (AnnoS g c (LogS x))  = return (AnnoS g (c, []) (LogS x))
parameterize' _ (AnnoS g c (StrS x))  = return (AnnoS g (c, []) (StrS x))
parameterize' args (AnnoS g c (BndS v)) = do
  let args' = [r | r@(Arg _ v') <- args, v' == v]
  return $ AnnoS g (c, args') (BndS v)
parameterize' _ (AnnoS m c (ExeS (SrcCall src)))
  = return $ AnnoS m (c, []) (ExeS (SrcCall src))
parameterize' _ (AnnoS g c (ExeS (PatCall x)))
  = return (AnnoS g (c, []) (ExeS (PatCall x)))
parameterize' args (AnnoS g c (LstS xs)) = do
  xs' <- mapM (parameterize' args) xs
  let args' = pruneArgs args xs'
  return $ AnnoS g (c, args') (LstS xs')
parameterize' args (AnnoS g c (TupS xs)) = do
  xs' <- mapM (parameterize' args) xs
  let args' = pruneArgs args xs'
  return $ AnnoS g (c, args') (TupS xs')
parameterize' args (AnnoS g c (NamS entries)) = do
  xs' <- mapM (parameterize' args . snd) entries
  let args' = pruneArgs args xs'
  return $ AnnoS g (c, args') (NamS (zip (map fst entries) xs'))
parameterize' args (AnnoS g@(Idx _ (FunT inputs _)) c (LamS vs x)) = do
  ids <- MM.takeFromCounter (length inputs)
  let contextArgs = [r | r@(Arg _ v) <- args, v `notElem` vs] -- remove shadowed arguments
      boundArgs = fromJust $ safeZipWith Arg ids vs
  x' <- parameterize' (contextArgs <> boundArgs) x
  let contextArgs' = pruneArgs contextArgs [x']
  return $ AnnoS g (c, contextArgs' <> boundArgs) (LamS vs x')
-- LamS MUST have a functional type, deviations would have been caught by the typechecker
parameterize' _ (AnnoS _ _ (LamS _ _)) = error "impossible"
parameterize' args (AnnoS g c (AppS x xs)) = do
  x' <- parameterize' args x
  xs' <- mapM (parameterize' args) xs
  let args' = pruneArgs args (x':xs')
  return $ AnnoS g (c, args') (AppS x' xs')
parameterize' _ (AnnoS _ _ (VarS _ _)) = undefined

pruneArgs :: [Arg a] -> [AnnoS c One (g, [Arg a])] -> [Arg a]
pruneArgs args xs =
  let usedArgs = unique $ concatMap (map ann . sannoSnd) xs
  in [r | r@(Arg i _) <- args, i `elem` usedArgs]

mkIdx :: AnnoS g One (Indexed c, d) -> Type -> Indexed Type
mkIdx (AnnoS _ (Idx i _, _) _) = Idx i


-- Walks down through an expression until it finds the top-level function
-- term. These terms are the ones that may be associated with tags that contain
-- runtime info. This info is then linked to the given index (presumably a
-- manifold index) that wraps the main function (but that may have automatically
-- generated earlier).
setManifoldConfig
  :: Int
  -> AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar])
  -> MorlocMonad ()
-- The positive case of an application to a term. The VarS here directly maps to
-- a term in the users input code, e.g., `myTag:foo`.
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
        MM.put(s {stateManifoldConfig = Map.insert midx mconfig (stateManifoldConfig s)})

-- express e@(AnnoS fi@(Idx midx (FunT inputs _)) (Idx cidx lang, _) exe) = do
--   MM.sayVVV $ "express ExeS (midx=" <> pretty midx <> "," <+> "cidx=" <> pretty cidx <> "):"
--   ids <- MM.takeFromCounter (length inputs)
--   let lambdaVals = fromJust $ safeZipWith PolyBndVar (map (C . Idx cidx) inputs) ids
--       headExpr = PolyHead lang midx [Arg i None | i <- ids]
--   exeExpr <- case exe of
--     (ExeS (SrcCall src)) -> return . PolyReturn $
--         PolyApp (PolyExe fi (SrcCallP src)) lambdaVals
--     (ExeS (PatCall pat)) -> return . PolyReturn $
--         PolyApp (PolyExe fi (PatCallP pat)) lambdaVals
--     _ -> do
--       fe <- expressPolyExprWrap lang fi e
--       return
--         . PolyLet midx fe
--         . PolyReturn
--         $ PolyApp (PolyLetVar fi midx) lambdaVals
--
--   return (headExpr exeExpr)


-- Conventions:
--   midx: The general index, used for identifying manifolds since this index is
--         common across languages.
--   cidx: The concrete index, will be associated with types that in the given
--         language and is required later for evaluating them into concrete
--         types. The language of the type cidx is coupled to in `express` must
--         be the same as the language cidx is coupled to in the SAnno
--         expression.
express :: AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) -> MorlocMonad PolyHead
-- CallS - direct export of a sourced function, e.g.:
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

-- *****************  EVIL INDEX REWRITE HACK WARNING ************************
-- Move the index from the lambda to the application.
-- Changing indices is a BAD idea, it breaks the link to the source code
-- I do it here so that the nexus indices and pool indices match, but there
-- should be a more elegant solution.
-- ***************************************************************************
-- We pass the index and the arguments from the top-level lambda expression
-- to the application. The arguments for the lambda will include the specific
-- arguments the type signature and declaration specify, the arguments that
-- the user expects to enter. However, the application will prune any
-- arguments that are not used. So here we want the lambda, not the
-- application.
-- ----
-- lambda
express (AnnoS (Idx midx _) (_, lambdaArgs) (LamS _ e@(AnnoS (Idx _ applicationType) (c, _) x))) = do
  MM.sayVVV $ "express LamS (midx=" <> pretty midx <> "):"
  -- More evil, copy the manifold config from the top application to this lambda
  setManifoldConfig midx e
  -- attach the lambda index to the application ManifoldConfig from state
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

-- records
express (AnnoS (Idx midx t@(NamT o v ps rs)) (Idx cidx lang, args) (NamS entries)) = do
  MM.sayVVV $ "express NamT:" <+> pretty t
  let idxTypes = zipWith mkIdx (map snd entries) (map snd rs)
  xs' <- fromJust <$> safeZipWithM (expressPolyExprWrap lang) idxTypes (map snd entries)
  let x = PolyRecord o (Idx cidx v) (map (Idx cidx) ps) (zip (map fst rs) (zip idxTypes xs'))
  return $ PolyHead lang midx [Arg i None | Arg i _ <- args] (PolyReturn x)

-- expand the record type if possible, otherwise die
express (AnnoS (Idx midx t) (Idx cidx lang, args) (NamS entries)) = do
  MM.sayVVV $ "express NamT expand:" <+> pretty t
  mayT <- evalGeneralStep midx (type2typeu t)
  case mayT of
    (Just t') -> express (AnnoS (Idx midx (typeOf t')) (Idx cidx lang, args) (NamS entries))
    Nothing -> MM.throwError . OtherError . render $ "Missing concrete:" <+> "t=" <> pretty t

-- In other cases, it doesn't matter whether we are at the top of the call
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
expressDefault e@(AnnoS (Idx midx t) (Idx cidx lang, args) _)
  = PolyHead lang midx [Arg i None | Arg i _ <- args] <$> expressPolyExprWrap lang (Idx cidx t) e


expressPolyExprWrap
    :: Lang
    -> Indexed Type -- indexed with the concrete type
    -> AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar])
    -> MorlocMonad PolyExpr
expressPolyExprWrap l t e@(AnnoS (Idx midx _) _ (LamS _ lamExpr)) = do
  setManifoldConfig midx lamExpr
  expressPolyExprWrapCommon l t e
expressPolyExprWrap l t e = expressPolyExprWrapCommon l t e

-- | Determine the "remoteness" of an expression
expressPolyExprWrapCommon :: Lang -> Indexed Type -> AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) -> MorlocMonad PolyExpr
-- If the expression is an application, the check if the function index (not the application index)
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


expressPolyExpr
    :: (Lang -> Lang -> Maybe RemoteForm)
    -> Lang
    -> Indexed Type
    -> AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar])
    -> MorlocMonad PolyExpr
-- these cases will include partially applied functions and explicit lambdas
-- the former is transformed into the latter in the frontend typechecker
expressPolyExpr findRemote parentLang _
  (AnnoS (Idx midx (FunT lamInputTypes lamOutType)) (Idx cidxLam _, lamArgs)
    (LamS vs
      (AnnoS _ (Idx _ appLang, appArgs)
        (AppS
          funExpr@(AnnoS (Idx _ (FunT callInputTypes _)) (Idx _ callLang, _) _) xs))))

  ----------------------------------------------------------------------------------------
  -- #4 cis partial lambda                                     | contextArgs | boundArgs |
  ----------------------------------------------------------------------------------------
  --      g_L (\z x -> f_L x y z) xs                           | [y]         | [x,z]     |
  --      --------------------------                           |             |           |
  --      def m1(xs, y):                                       |             |           |
  --          return g(lambda x, z: m2(z, y, x), xs)           |             |           |
  --                                                           |             |           |
  ----------------------------------------------------------------------------------------
  | isLocal = do
      MM.sayVVV "case #4"
      let nContextArgs = length appArgs - length vs
          contextArgs = map unvalue (take nContextArgs appArgs)

          typedLambdaArgs = fromJust $ safeZipWith (\(Arg i _) t -> Arg i (Just t))
            (drop nContextArgs lamArgs)
            lamInputTypes

      xs' <- fromJust <$> safeZipWithM (expressPolyExprWrap appLang) (zipWith mkIdx xs callInputTypes) xs

      call <- expressPolyApp parentLang funExpr xs'

      return
        . PolyManifold parentLang midx (ManifoldPart contextArgs typedLambdaArgs)
        $ call


  ----------------------------------------------------------------------------------------
  -- #8 trans partial lambda                                   | contextArgs | boundArgs |
  ----------------------------------------------------------------------------------------
  --      g_L (\z x -> f_L x y z)                              | [y]         | [z,x]     |
  --      -----------------------                              |             |           |
  --      def m2(x, y', z):                                    |             |           |
  --          x' = SERIALIZE(x)                                |             |           |
  --          z' = SERIALIZE(z)                                |             |           |
  --          r' = CALL(2, x', y', z')                         |             |           |
  --          return DESERIALIZE(r')                           |             |           |
  --                                                           |             |           |
  --      def m1(y):                                           |             |           |
  --          y' = SERIALIZE(y)                                |             |           |
  --          return g(lambda z, x: m2(x, y, z))               |             |           |
  ----------------------------------------------------------------------------------------
  | not isLocal = do
      -- 1. express all xs under the foreign type if they are in the foreign language or
      --    the parent type (pc) if they are in the parent language
      --
      -- 2. let extract each expression that is not in the foreign type,
      --    replace the corresponding xs' value with a BndVar. store these
      --    ids.
      --
      -- 3. Translate the letArgs to the foreign type (based on id and position in
      --    callInputs)
      --
      -- 4. Find foreign arg list: extract args in foreign language from each
      --    xs' element and then append translated let args (step 3)
      --
      -- 5. Using argument ids, map these foreign args to local args in
      --    (appArgs + letArgs)
      --
      -- 6. Fold let statements over local manifold

      -- evaluate arguments and derive any required let bindings
      xsInfo <- mapM partialExpress xs

      MM.sayVVV $ "  xsInfo:" <+> pretty xsInfo

      let xs' = map (\(_, _, e) -> e) xsInfo
          -- rs: the list of arguments (by index) required by a single expression
          --     passed to the foreign function
          -- callArgs: the ordered unique set of the required arguments
          callArgs = unique (concatMap (\(rs, _, _) -> rs) xsInfo)
          args = [i | Arg i _ <- appArgs]
          allParentArgs = args <> [i | (_, Just (i, _), _) <- xsInfo]
          lets = [PolyLet i e | (_, Just (i, e), _) <- xsInfo]
          -- arguments on the calling side
          passedParentArgs = concat [[r | r <- allParentArgs, r == i] | i <- callArgs]
          -- manifold arguments
          nContextArgs = length appArgs - length vs

          -- the types of the terms passed through the lambda
          lambdaTypeMap = zip vs (map (Idx cidxLam) lamInputTypes)
          -- variables passed to the manifold, those bound by the lambda will
          -- have known local types
          boundVars = [ PolyBndVar (maybe (A parentLang) C (lookup v lambdaTypeMap)) i
                      | Arg i v <- appArgs
                      ]
          untypedContextArgs = map unvalue $ take nContextArgs appArgs
          typedPassedArgs = fromJust $ safeZipWith (\(Arg i _) t -> Arg i (Just t)) (drop nContextArgs lamArgs) lamInputTypes

          localForm = ManifoldPart untypedContextArgs typedPassedArgs

          -- contextArgs
          -- boundArgs = map unvalue (drop () appArgs)
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
    chain (f:fs) x = chain fs (f x)

    -- This function is applied to every argument of a foreign call in a lambda (case #8)
    --
    -- Partitions evaluation of expressions applied to a foreign pool between the
    -- local and foreign contexts
    partialExpress
        :: AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) -- expression
        -> MorlocMonad
            ( [Int] -- ordered foreign arguments, should include ids bound by let (next arg)
            , Maybe (Int, PolyExpr) -- parent let statement if not in child language and eval is needed
            , PolyExpr -- final foreign expression
            )
    -- If the argument is a variable, link the argument id to the variable id and
    -- assign it the foreign call type
    partialExpress (AnnoS (Idx _ t) (Idx cidx argLang, args@[Arg idx _]) (BndS v)) = do
      MM.sayVVV $ "partialExpress case #0:" <+> "x=" <> pretty v <+> "cidx=" <> pretty cidx <+> "t =" <+> pretty t
                <> "\n  parentLang:" <> pretty parentLang
                <> "\n  callLang:" <> pretty callLang
                <> "\n  argLang:" <> pretty argLang
                <> "\n  args:" <> pretty args
      let x' = PolyBndVar (C (Idx cidx t)) idx
      return ([idx], Nothing, x')
    -- Otherwise
    partialExpress x@(AnnoS (Idx _ t) (Idx cidx argLang, args) _)
      -- if this expression is implemented on the foreign side,
      --   translate to ExprM and record
      | argLang == callLang = do
          MM.sayVVV $ "partialExpress case #2:" <+> "cidx=" <> pretty cidx <+> "t =" <+> pretty t
                    <> "\n  parentLang:" <> pretty parentLang
                    <> "\n  callLang:" <> pretty callLang
                    <> "\n  argLang:" <> pretty argLang
                    <> "\n  args:" <> pretty args
          let argParentType = Idx cidx t
          x' <- expressPolyExprWrap argLang argParentType x
          return ([i | Arg i _ <- args], Nothing, x')
      -- if this expression is implemented on the calling side,
      --   let-bind the expression on the calling side and use the let-index as an
      --   argument index
      | otherwise = do
          MM.sayVVV $ "partialExpress case #1:" <+> "cidx=" <> pretty cidx <+> "t =" <+> pretty t
                    <> "\n  parentLang:" <> pretty parentLang
                    <> "\n  callLang:" <> pretty callLang
                    <> "\n  argLang:" <> pretty argLang
                    <> "\n  args:" <> pretty args
          let argparentType = Idx cidx t
          letVal <- expressPolyExprWrap argLang argparentType x
          idx <- MM.getCounter
          MM.sayVVV $ "making index in partialExpress #1:" <+> pretty idx

          -- This let variable is used in the foreign side, so the cidx from the
          -- argument is correct
          let x' = PolyLetVar (Idx cidx t) idx
          -- Only the let-bound argument is used on the foreign side
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

    MM.sayVVV $ "Express lambda:"
              <> "\n  vs:" <+> pretty vs
              <> "\n  lambdaType:" <+> pretty lambdaType
              <> "\n  manifoldArguments:" <+> list (map pretty manifoldArguments)
              <> "\n  contextArguments:" <+> list (map pretty contextArguments)
              <> "\n  boundArguments" <+> list (map pretty typeBoundArguments)

    return
      . PolyManifold lang midx (ManifoldPart contextArguments typeBoundArguments)
      . PolyReturn
      $ body'

-- Apply arguments to a sourced function
-- * The CallS object may be in a foreign language. These inter-language
--   connections will be snapped apart in the segment step.
-- * These applications will be fully applied, the case of partially applied
--   functions will have been handled previously by LamM
expressPolyExpr findRemote parentLang pc (AnnoS (Idx midx _) (_, args)
  (AppS f@(AnnoS (Idx _ (FunT inputs _)) (Idx cidxCall callLang, _) _) xs))

  ----------------------------------------------------------------------------------------
  -- #1 cis applied                                            | contextArgs | boundArgs |
  ----------------------------------------------------------------------------------------
  --      f_L :: A -> B -> C -> D                              | [x,y,z]     | []        |
  --      g_L (f_L x y z)                                      |             |           |
  --      -----------------------                              |             |           |
  --      def m1(x,y,z):                                       |             |           |
  --          g m2(x, y, z)                                    |             |           |
  ----------------------------------------------------------------------------------------
  | isLocal = do
      MM.sayVVV $ "case #1"
      -- There should be an equal number of input types and input arguments
      -- That is, the function should be fully applied. If it were partially
      -- applied, the lambda case would have been entered previously instead.
      xsExpr <- zipWithM (expressPolyExprWrap callLang) (map (Idx cidxCall) inputs) xs

      func <- expressPolyApp parentLang f xsExpr
      return
          . PolyManifold callLang midx (ManifoldFull (map unvalue args))
          $ func

  ----------------------------------------------------------------------------------------
  -- #5 trans applied                                          | contextArgs | boundArgs |
  ----------------------------------------------------------------------------------------
  --                                                           | [x,y,z]     | []        |
  --      f_M :: A -> B -> C -> D                              |             |           |
  --      g_L (f_M x y z)                                      |             |           |
  --      -----------------------                              |             |           |
  --      def m1(x,y,z):                                       |             |           |
  --          x' = SERIALIZE(x)   -- maybe, depending on       |             |           |
  --          y' = SERIALIZE(y)   -- context                   |             |           |
  --          z' = SERIALIZE(z)                                |             |           |
  --          r' = CALL(2, x', y', z')   -- where 2 is         |             |           |
  --          r = DESERIALIZE(r')        -- the foreign        |             |           |
  --          return g(r)                -- manifold number    |             |           |
  --                                                           |             |           |
  ----------------------------------------------------------------------------------------
  | not isLocal = do
        MM.sayVVV $ "case #5"
        let idxInputTypes = zipWith mkIdx xs inputs
        mayXs <- safeZipWithM (expressPolyExprWrap callLang) idxInputTypes xs
        func <- expressPolyApp parentLang f (fromJust mayXs)
        return
          . PolyManifold parentLang midx (ManifoldFull (map unvalue args))
          . PolyReturn
          . PolyApp
              ( PolyRemoteInterface callLang pc [] (fromJust remote) -- no args are passed, so empty
              . PolyManifold callLang midx (ManifoldFull (map unvalue args))
              $ func
              )
          -- non-native use, leave as passthrough
          -- the contextual language, though, is the same as the parent
          $ [PolyBndVar (A parentLang) i | Arg i _ <- args]

  where
    remote = findRemote parentLang callLang
    isLocal = isNothing remote

-- An un-applied source call
expressPolyExpr findRemote parentLang
  (val -> FunT pinputs poutput)
  e@(AnnoS (Idx midx (FunT callInputs _)) (Idx cidx callLang, _) _)

  ----------------------------------------------------------------------------------------
  -- #2 cis passed                                             | contextArgs | boundArgs |
  ----------------------------------------------------------------------------------------
  --      f_L :: A -> B                                        | []          | []        | -- FIXME
  --      g_L f_L xs                                           |             |           |
  --      -------------                                        |             |           |
  --      def m1(xs):                                          |             |           |
  --          g(m2, xs)                                        |             |           |
  ----------------------------------------------------------------------------------------
  | isLocal = do
      MM.sayVVV $ "case #2"
      ids <- MM.takeFromCounter (length callInputs)
      let lambdaVals = bindVarIds ids (map (C . Idx cidx) callInputs)
          lambdaTypedArgs = fromJust $ safeZipWith annotate ids (map Just callInputs)
      retapp <- expressPolyApp parentLang e lambdaVals
      return
        . PolyManifold callLang midx (ManifoldPass lambdaTypedArgs)
        $ retapp

  ----------------------------------------------------------------------------------------
  -- #6 trans passed                                           | contextArgs | boundArgs |
  ----------------------------------------------------------------------------------------
  --      f_L :: A -> B                                        | []          | []        | -- FIXME
  --      g_L f_L xs                                           |             |           |
  --      -------------                                        |             |           |
  --      def m2(x):                                           |             |           |
  --          x' = SERIALiZE(x)                                |             |           |
  --          r' = CALL(2, x')                                 |             |           |
  --          return DESERIALIZE(r')                           |             |           |
  --                                                           |             |           |
  --      def m1(xs):                                          |             |           |
  --          return g(m2, xs)                                 |             |           |
  ----------------------------------------------------------------------------------------
  | otherwise = do
      MM.sayVVV $ "case #6"
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
       $ fromJust $ safeZipWith (PolyBndVar . C) (map (Idx cidx) pinputs) (map ann lambdaArgs)
    where
    remote = findRemote parentLang callLang
    isLocal = isNothing remote

-- bound variables
expressPolyExpr _ _ _ (AnnoS (Idx _ c) (Idx cidx _, rs) (BndS v)) = do
  MM.sayVVV $ "express' VarS" <+> parens (pretty v) <+> "::" <+> pretty c
  case [i | (Arg i v') <- rs, v == v'] of
    [r] -> return $ PolyBndVar (C (Idx cidx c)) r
    rs' -> MM.throwError . OtherError . render
        $ "Expected VarS" <+> dquotes (pretty v) <+>
          "of type" <+> parens (pretty c) <+> "to match exactly one argument, found:" <+> list (map pretty rs')
        <> "\n  v:" <+> pretty v
        <> "\n  cidx:" <+> pretty cidx
        <> "\n  gidx:" <+> pretty cidx
        <> "\n  rs:" <+> list (map pretty rs)

-- primitives
expressPolyExpr _ _ _ (AnnoS (Idx _ (VarT v)) (Idx cidx _, _) (RealS x )) = return $ PolyReal (Idx cidx v) x
expressPolyExpr _ _ _ (AnnoS (Idx _ (VarT v)) (Idx cidx _, _) (IntS x  )) = return $ PolyInt  (Idx cidx v) x
expressPolyExpr _ _ _ (AnnoS (Idx _ (VarT v)) (Idx cidx _, _) (LogS x  )) = return $ PolyLog  (Idx cidx v) x
expressPolyExpr _ _ _ (AnnoS (Idx _ (VarT v)) (Idx cidx _, _) (StrS x  )) = return $ PolyStr  (Idx cidx v) x
expressPolyExpr _ _ _ (AnnoS (Idx _ (VarT v)) (Idx cidx _, _)  UniS     ) = return $ PolyNull (Idx cidx v)

-- lists
expressPolyExpr _ parentLang pc (AnnoS (Idx midx (AppT (VarT v) [t])) (Idx cidx lang, args) (LstS xs)) = do
  xs' <- mapM (\x -> expressPolyExprWrap lang (mkIdx x t) x) xs
  let e = PolyList (Idx cidx v) (Idx cidx t) xs'
  return $ expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e
expressPolyExpr _ _ _ (AnnoS _ _ (LstS _)) = error "LstS can only be (AppP (VarP _) [_]) type"

-- tuples
expressPolyExpr _ parentLang pc (AnnoS (Idx midx (AppT (VarT v) ts)) (Idx cidx lang, args) (TupS xs)) = do
  let idxTs = zipWith mkIdx xs ts
  xs' <- fromJust <$> safeZipWithM (expressPolyExprWrap lang) idxTs xs
  let e = PolyTuple (Idx cidx v) (fromJust $ safeZip idxTs xs')
  return $ expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e

-- records
expressPolyExpr _ parentLang pc (AnnoS (Idx midx (NamT o v ps rs)) (Idx cidx lang, args) (NamS entries)) = do
  MM.sayVVV $ "expressPolyExpr records"
  let tsIdx = zipWith mkIdx (map snd entries) (map snd rs)
  xs' <- fromJust <$> safeZipWithM (expressPolyExprWrap lang) tsIdx (map snd entries)
  let e = PolyRecord o (Idx cidx v) (map (Idx cidx) ps) (zip (map fst rs) (zip tsIdx xs'))
  return $ expressContainer pc (Idx midx parentLang) (Idx cidx lang) args e


-- if the type is not a record, evaluate one step and try again
expressPolyExpr _ pl pc (AnnoS (Idx i t) c e@(NamS _)) = do
    scope <- MM.getGeneralScope i
    case reduceType scope t of
        (Just t') -> expressPolyExprWrap pl pc (AnnoS (Idx i t') c e)
        Nothing -> error "Expected a record type"


expressPolyExpr _ _ _ (AnnoS _ _ (AppS (AnnoS _ _ (BndS v)) _))
  = MM.throwError . ConcreteTypeError $ FunctionSerialization v

-- catch all exception case - not very classy
expressPolyExpr _ _ _ (AnnoS _ _ (AppS (AnnoS _ _ (LamS vs _)) _))
  = error $ "All applications of lambdas should have been eliminated of length " <> show (length vs)

expressPolyExpr _ _ parentType x@(AnnoS (Idx m t) _ _) = do
  MM.sayVVV "Bad case"
  MM.sayVVV $ "  t :: " <> pretty t
  name' <- MM.metaName m
  case name' of
      (Just v) -> MM.throwError . OtherError . render
               $ "Missing concrete:"
               <> "\n  t:" <+> viaShow t
               <> "\n  v:" <+> pretty v
               <> "\n parentType:" <+> pretty parentType
               <> "\n x:" <+> pretty x
      Nothing ->  MM.throwError . OtherError . render
               $ "Missing concrete in unnamed function:"
               <> "\n  t:" <+> pretty t
               <> "\n parentType:" <+> pretty parentType
               <> "\n x:" <+> pretty x


-- evaluate an expression that has a functional type and is applied
expressPolyApp
  :: Lang
  -> AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar])
  -> [PolyExpr]
  -> MorlocMonad PolyExpr

-- handle directly sourced functions
expressPolyApp _ (AnnoS g _ (ExeS (SrcCall src))) xs = do
  MM.sayVVV $ "expressPolyApp src g:" <+> pretty g
  return . PolyReturn $ PolyApp (PolyExe g (SrcCallP src)) xs

-- handle functions extracted from data by patterns
expressPolyApp _ (AnnoS g _ (ExeS (PatCall pat))) xs
  = return . PolyReturn $ PolyApp (PolyExe g (PatCallP pat)) xs

-- handle functions returned from source functions
expressPolyApp lang f@(AnnoS g@(Idx i _) _ (AppS _ _)) es = do
  fe <- expressPolyExprWrap lang g f
  return
    . PolyLet i fe
    . PolyReturn
    $ PolyApp (PolyLetVar g i) es

-- handle functions passed as lambda-bound variables
expressPolyApp _ (AnnoS g (_, args) (BndS v)) xs = do
  case [j | (Arg j u) <- args, u == v] of
    [j] -> return . PolyReturn $ PolyApp (PolyExe g (LocalCallP j)) xs
    _ -> error "Unreachable? BndS value should have been wired uniquely to args previously"

-- all other function sources are invalid
expressPolyApp _ (AnnoS _ _ (LamS _ _)) _ = error "unexpected LamS - should have been handled"
expressPolyApp _ (AnnoS _ _ (VarS _ _)) _ = error "unexpected VarS - should have been substituted"
expressPolyApp _ _ _ = error "Unreachable? This does not seem to be applicable"


expressContainer :: Indexed Type -> Indexed Lang -> Indexed Lang -> [Arg EVar] -> PolyExpr -> PolyExpr
expressContainer pc (Idx midx parentLang) (Idx _ lang) args e
  | parentLang /= lang =
      PolyApp
        ( PolyRemoteInterface lang pc [i | Arg i _ <- args] ForeignCall
        . PolyManifold lang midx (ManifoldFull (map unvalue args))
        . PolyReturn
        $ e
        )
      -- pass required args from the parent language
      $ [PolyBndVar (A parentLang) i | Arg i _ <- args]
  | otherwise = e

unvalue :: Arg a -> Arg None
unvalue (Arg i _) = Arg i None

bindVarIds :: [Int] -> [Three Lang Type (Indexed Type)] -> [PolyExpr]
bindVarIds [] [] = []
bindVarIds (i : args) (t   : types) = PolyBndVar t i : bindVarIds args types
-- These error states indicate a bug in the compiler, not the user code, so no mercy
bindVarIds [] ts = error $ "bindVarIds: too few arguments: " <> show ts
bindVarIds _ [] = error "bindVarIds: too few types"



segment :: PolyHead -> MorlocMonad [MonoHead]
segment (PolyHead lang m0 args0 e0) = do
  (heads, (_, topExpr)) <- segmentExpr m0 (map ann args0) e0

  MM.sayVVV $ "segmentation complete"
            <> "\n  topExpr language:" <+> pretty lang
            <> "\n  topExpr: " <+> pretty topExpr
            <> "\n  heads:" <+> list (map pretty heads)

  return (MonoHead lang m0 args0 HeadManifoldFormLocalRoot topExpr : heads)

segmentExpr
  :: Int -- manifold index
  -> [Int] -- argument indices
  -> PolyExpr
  -> MorlocMonad ([MonoHead], (Maybe Lang, MonoExpr))
-- This is where segmentation happens, every other match is just traversal
segmentExpr _ args (PolyRemoteInterface lang callingType cargs remoteCall e@(PolyManifold _ m (ManifoldFull foreignArgs) _)) = do
  MM.sayVVV $ "segmentExpr PolyRemoteInterface PolyManifold m" <> pretty m
            <> "\n  forced ManifoldFull" <+> pretty foreignArgs
            <> "\n  lang" <+> pretty lang
            <> "\n  args" <+> pretty args
            <> "\n  cargs" <+> pretty cargs
            <> "\n  foreignArgs" <+> pretty (map ann foreignArgs)
  (ms, (_, e')) <- segmentExpr m (map ann foreignArgs) e
  headForm <- case remoteCall of
    ForeignCall -> return HeadManifoldFormLocalForeign
    (RemoteCall _) -> return HeadManifoldFormRemoteWorker
  let foreignHead = MonoHead lang m foreignArgs headForm e'
  config <- MM.ask
  let socket = MC.setupServerAndSocket config lang
  return (foreignHead:ms, (Nothing, MonoPoolCall callingType m socket remoteCall foreignArgs))

segmentExpr m _ (PolyRemoteInterface lang callingType args remoteCall e) = do
  MM.sayVVV $ "segmentExpr PolyRemoteInterface m" <> pretty m
            <> "\n  args" <+> pretty args
            <> "\n  lang" <+> pretty lang
  (ms, (_, e')) <- segmentExpr m args e
  headForm <- case remoteCall of
    ForeignCall -> return HeadManifoldFormLocalForeign
    (RemoteCall _) -> return HeadManifoldFormRemoteWorker
  -- create the foreign manifold, make sure all arugments are packed
  let foreignHead = MonoHead lang m [Arg i None | i <- args] headForm (MonoReturn e')
      -- pack the arguments that will be passed to the foreign manifold
      es' = map (MonoBndVar (A None)) args

  config <- MM.ask
  let socket = MC.setupServerAndSocket config lang
      localFun = MonoApp (MonoPoolCall callingType m socket remoteCall [Arg i None | i <- args]) es'

  return (foreignHead:ms, (Nothing, localFun))

segmentExpr _ _ (PolyManifold lang m form e) = do
  (ms, (_, e')) <- segmentExpr m (abilist const const form) e
  return (ms, (Just lang, MonoManifold m form e'))

segmentExpr m args (PolyApp e es) = do
  (ms, (lang, e')) <- segmentExpr m args e
  (mss, es') <- mapM (segmentExpr m args) es |>> unzip
  return (ms ++ concat mss, (lang, MonoApp e' (map snd es')))

segmentExpr m args (PolyLet i e1 e2) = do
  MM.sayVVV "segmentExpr PolyLet"
  (ms1, (_, e1')) <- segmentExpr m args e1
  (ms2, (lang2, e2')) <- segmentExpr m args e2
  return (ms1 ++ ms2, (lang2, MonoLet i e1' e2'))

segmentExpr m args (PolyList v t es) = do
  (mss, es') <- mapM (segmentExpr m args) es |>> unzip
  return (concat mss, (Nothing, MonoList v t (map snd es')))

segmentExpr m args (PolyTuple v es) = do
  (mss, es') <- mapM (segmentExpr m args . snd) es |>> unzip
  return (concat mss, (Nothing, MonoTuple v (zip (map fst es) (map snd es'))))

segmentExpr m args (PolyRecord o v ps entries) = do
  let entryTypes = map (fst . snd) entries
  (mss, es') <- mapM (segmentExpr m args . snd . snd) entries |>> unzip
  let keys = map fst entries
  return (concat mss, (Nothing, MonoRecord o v ps (zip keys (zip entryTypes (map snd es')))))

segmentExpr m args (PolyReturn e) = do
  (ms, (lang, e')) <- segmentExpr m args e
  return (ms, (lang, MonoReturn e'))

segmentExpr _ _ (PolyLetVar t x) = return ([], (Nothing, MonoLetVar t x))
segmentExpr _ _ (PolyBndVar (A lang) i) = return ([], (Just lang, MonoBndVar (A None) i))
segmentExpr _ _ (PolyBndVar (B t) i) = return ([], (Nothing, MonoBndVar (B t) i))
segmentExpr _ _ (PolyBndVar (C t) i) = return ([], (Nothing, MonoBndVar (C t) i))
segmentExpr _ _ (PolyExe t exe) = return ([], (Nothing, MonoExe t exe))
segmentExpr _ _ (PolyLog v x)   = return ([], (Nothing, MonoLog v x))
segmentExpr _ _ (PolyReal v x)  = return ([], (Nothing, MonoReal v x))
segmentExpr _ _ (PolyInt v x)   = return ([], (Nothing, MonoInt v x))
segmentExpr _ _ (PolyStr v x)   = return ([], (Nothing, MonoStr v x))
segmentExpr _ _ (PolyNull v)    = return ([], (Nothing, MonoNull v))


-- | This step is performed after segmentation, so all terms are in the same
-- language. Here we need to determine where inputs are (de)serialized and the
-- serialization states of arguments and variables.
serialize :: MonoHead -> MorlocMonad SerialManifold
serialize (MonoHead lang m0 args0 headForm0 e0) = do

  form0 <- ManifoldFull <$> mapM prepareArg args0

  MM.sayVVV $ "In serialize for" <+> "m" <> pretty m0 <+> pretty lang <+> "segment"
            <>  "\n  form0:" <+> pretty form0
            <>  "\n  typemap:" <+> viaShow typemap
            <>  "\n  This map we made from the expression:\n  " <> pretty e0

  se1 <- serialExpr m0 e0
  let sm = SerialManifold m0 lang form0 headForm0 se1
  wireSerial lang sm
  where

  inferType = inferConcreteType lang
  inferTypeUniversal = inferConcreteTypeUniversal lang
  inferVar = inferConcreteVar lang

  -- map of argument indices to native types
  typemap = makeTypemap m0 e0

  prepareArg
    :: Arg None
    -> MorlocMonad (Arg (Or TypeS TypeF))
  prepareArg (Arg i _) = case Map.lookup i typemap of
    Nothing -> return $ Arg i (L PassthroughS)
    (Just (Right t)) -> do
      t' <- inferType t
      return $ Arg i (L (typeSof t'))
    (Just (Left t)) -> do
      MM.sayVVV "Warning: using universal inference at prepareArg"
      t' <- inferTypeUniversal t
      return $ Arg i (L (typeSof t'))

  contextArg
    :: Int
    -> MorlocMonad (Or TypeS TypeF)
  contextArg i = case Map.lookup i typemap of
    (Just (Right t)) -> do
      t' <- inferType t
      return $ LR (typeSof t') t'
    Nothing -> return $ L PassthroughS
    (Just (Left t)) -> do
      MM.sayVVV "Warning: using universal inference at contextArg"
      t' <- inferTypeUniversal t
      return $ LR (typeSof t') t'

  boundArg :: Int -> MorlocMonad TypeF
  boundArg i = case Map.lookup i typemap of
    (Just (Right t)) -> inferType t
    Nothing -> error "Untyped native arg"
    (Just (Left t)) -> do
      MM.sayVVV "Warning: using universal inference at boundArg"
      inferTypeUniversal t

  serialExpr
    :: Int
    -> MonoExpr
    -> MorlocMonad SerialExpr
  serialExpr _ (MonoManifold m form e) = do
    MM.sayVVV $ "serialExpr MonoManifold m" <> pretty m <> parens (pretty form)
    serialExpr m e
  serialExpr m (MonoLet i e1 e2) = case inferState e1 of
    Serialized -> SerialLetS i <$> serialExpr m e1 <*> serialExpr m e2
    Unserialized -> do
      ne1 <- nativeExpr m e1
      NativeLetS i ne1 <$> serialExpr m e2
  serialExpr _ (MonoLetVar t i) = do
    t' <- inferType t
    return $ LetVarS (Just t') i
  serialExpr m (MonoReturn e) = ReturnS <$> serialExpr m e
  serialExpr _ (MonoApp (MonoPoolCall t m docs remoteCall contextArgs) es) = do
    contextArgs' <- mapM (typeArg Serialized . ann) contextArgs
    let poolCall' = PoolCall m docs remoteCall contextArgs'
    es' <- mapM (serialArg m) es
    t' <- inferType t
    return $ AppPoolS t' poolCall' es'
  serialExpr _ (MonoBndVar (A _) i) = return $ BndVarS Nothing i
  serialExpr _ (MonoBndVar (B _) i) =
    case Map.lookup i typemap of
      (Just (Right t)) -> BndVarS <$> fmap Just (inferType t) <*> pure i
      _ -> return $ BndVarS Nothing i
  serialExpr _ (MonoBndVar (C t) i) = BndVarS <$> fmap Just (inferType t) <*> pure i
  -- failing cases that should be unreachable
  serialExpr _ (MonoExe _ _) = error "Can represent MonoSrc as SerialExpr"
  serialExpr _ MonoPoolCall{} = error "MonoPoolCall does not map to a SerialExpr"
  serialExpr _ (MonoApp MonoManifold{} _) = error "Illegal?"
  -- the following are all native types that need to be directly serialized
  serialExpr m e = nativeExpr m e >>= serializeS "serialE e" m

  serialArg
    :: Int
    -> MonoExpr
    -> MorlocMonad SerialArg
  serialArg _ e@(MonoManifold m form _) = do
    MM.sayVVV $ "serialArg MonoManifold m" <> pretty m <> parens (pretty form)
    se <- serialExpr m e
    case se of
      (ManS sm) -> return $ SerialArgManifold sm
      _ -> error "Unreachable?"
  -- Pool and source calls should have previously been wrapped in manifolds
  serialArg _ MonoPoolCall{} = error "This step should be unreachable"
  serialArg _ (MonoExe    _ _) = error "This step should be unreachable"
  serialArg _ (MonoReturn _) = error "Return should not happen hear (really I should remove this term completely)"
  serialArg m e = SerialArgExpr <$> serialExpr m e

  nativeArg
    :: Int
    -> MonoExpr
    -> MorlocMonad NativeArg
  -- This case may occur, for example, with `(add 1.0 2.0)`. Here `add` has two
  -- native arguments, but the manifold that wraps it will have no
  -- arguments. This is because `1.0` and `2.0` are primitive and will be
  -- generated in place rather than passed as arguments.
  nativeArg _ e@(MonoManifold m form _) = do
    MM.sayVVV $ "nativeArg MonoManifold m" <> pretty m <> parens (pretty form)
    ne <- nativeExpr m e
    case ne of
      (ManN nm) -> return $ NativeArgManifold nm
      _ -> error "Unreachable?"
  -- Pool and source calls should have previously been wrapped in manifolds
  nativeArg _ MonoPoolCall{} = error "This step should be unreachable"
  nativeArg _ (MonoExe    _ _) = error "This step should be unreachable"
  nativeArg _ (MonoReturn _) = error "Return should not happen here (really I should remove this term completely)"
  nativeArg m e = NativeArgExpr <$> nativeExpr m e

  nativeExpr
    :: Int
    -> MonoExpr
    -> MorlocMonad NativeExpr
  nativeExpr _ (MonoManifold m form e) = do
    MM.sayVVV $ "nativeExpr MonoManifold m" <> pretty m <> parens (pretty form)
    ne <- nativeExpr m e
    form' <- abimapM (\i _ -> contextArg i) (\i _ -> boundArg i) form
    return . ManN $ NativeManifold m lang form' ne

  nativeExpr _ MonoPoolCall{} = error "MonoPoolCall does not map to NativeExpr"
  nativeExpr m (MonoLet i e1 e2) = case inferState e1 of
    Serialized -> do
      ne2 <- nativeExpr m e2
      SerialLetN i <$> serialExpr m e1 <*> pure ne2
    Unserialized -> do
      ne1 <- nativeExpr m e1
      ne2 <- nativeExpr m e2
      return $ NativeLetN i ne1 ne2
  nativeExpr _ (MonoLetVar t i) = LetVarN <$> inferType t <*> pure i
  nativeExpr m (MonoReturn e) = ReturnN <$> nativeExpr m e
  nativeExpr m (MonoApp (MonoExe (Idx idx (FunT inputTypes outputType)) exe) es) = do
    args <- mapM (nativeArg m) es
    appType <- case drop (length es) inputTypes of
        [] -> inferType (Idx idx outputType)
        remaining -> inferType $ Idx idx (FunT remaining outputType)

    -- Pull the parameter types that were solved in the frontend typechecker
    qualifiers <- MM.gets stateTypeQualifier

    -- WARNING: removing all higher-kinded qualifier types
    -- This will bite me in the future. Future me has been warned.
    -- Currently, these qualifiers are used only in C++ code for explicit template resolution.
    let qsAll = maybe [] id (Map.lookup idx qualifiers)
        qs = [(v, t) | (v, t, 1) <- qsAll]

    -- check if any qualifiers where skipped
    case [v | (v, _, i) <- qsAll, i > 1] of
      [] -> return ()
      vs -> MM.sayVVV $ "Warning: skipping higher-kinded qualifiers:" <+> list (map pretty vs)

    -- Infer concrete types for all
    ftypes <- mapM (\t -> do
      inferType (Idx idx (typeOf t))) (map snd qs)
    -- Clean up and zip together
    let vs = map (unTVar . fst) qs
        qs' = zip vs ftypes

    return $ AppExeN appType exe qs' args
  nativeExpr m e@(MonoApp (MonoPoolCall t _ _ _ _) _) = do
    e' <- serialExpr m e
    t' <- inferType t
    MM.sayVVV $ "nativeExpr MonoApp:" <+> pretty t'
    naturalizeN "nativeE MonoApp" m lang t' e'

  -- covers functions generated at run-time from source functions or data
  nativeExpr m (MonoApp (MonoLetVar (Idx idx (FunT inputTypes outputType)) i) es) = do
    MM.sayVVV $ "MonoLetVar case"
    args <- mapM (nativeArg m) es
    appType <- case drop (length es) inputTypes of
        [] -> inferType (Idx idx outputType)
        remaining -> inferType $ Idx idx (FunT remaining outputType)
    return $ AppExeN appType (LocalCallP i) [] args

  nativeExpr _ (MonoApp e es) = do
    MM.sayVVV "nativeExprr MonoApp"
    MM.sayVVV $ "e:" <+> pretty e
    MM.sayVVV $ "es:" <+> list (map pretty es)
    error "Illegal application"

  nativeExpr _ (MonoExe t exe) = ExeN <$> inferType t <*> pure exe
  nativeExpr _ (MonoBndVar (A _) _) = error "MonoBndVar must have a type if used in native context"
  nativeExpr _ (MonoBndVar (B _) i) =
    case Map.lookup i typemap of
      (Just (Right t)) -> BndVarN <$> inferType t <*> pure i
      _ -> error "No type found"
  nativeExpr _ (MonoBndVar (C t) i) = BndVarN <$> inferType t <*> pure i
  -- simple native types
  nativeExpr m (MonoList v t es) =
    ListN
      <$> inferVar v
      <*> inferType t
      <*> mapM (nativeExpr m) es
  nativeExpr m (MonoTuple v rs) =
    TupleN
      <$> inferVar v
      <*> mapM (nativeExpr m . snd) rs
  nativeExpr m (MonoRecord o v ps rs) =
    RecordN o
      <$> inferVar v
      <*> mapM inferType ps
      <*> mapM (secondM (nativeExpr m . snd)) rs
  -- primitives
  nativeExpr _ (MonoLog    v x) = LogN  <$> inferVar v <*> pure x
  nativeExpr _ (MonoReal   v x) = RealN <$> inferVar v <*> pure x
  nativeExpr _ (MonoInt    v x) = IntN  <$> inferVar v <*> pure x
  nativeExpr _ (MonoStr    v x) = StrN  <$> inferVar v <*> pure x
  nativeExpr _ (MonoNull   v  ) = NullN <$> inferVar v

  typeArg
    :: SerializationState
    -> Int
    -> MorlocMonad (Arg TypeM)
  typeArg s i = case (s, Map.lookup i typemap) of
    (Serialized, Just (Right t)) -> do
      t' <- inferType t
      return $ Arg i (Serial t')
    (Serialized, Nothing) -> return $ Arg i Passthrough
    (Serialized, Just (Left t)) -> do
      MM.sayVVV $ "typeArg universal inference of unindexed type " <> pretty t
      t' <- inferTypeUniversal t
      return $ Arg i (Serial t')
    (Unserialized, Just (Right t)) -> do
      t' <- inferType t
      return $ Arg i (Native t')
    (Unserialized, Nothing) -> error "Bug: untyped non-passthrough value"
    (Unserialized, Just (Left t)) -> do
      MM.sayVVV $ "typeArg universal inference of unindexed type " <> pretty t
      t' <- inferTypeUniversal t
      return $ Arg i (Native t')

  makeTypemap :: Int -> MonoExpr -> Map.Map Int (Either Type (Indexed Type))
  -- map variables used in this segment to their types
  makeTypemap _ (MonoLetVar t i) = Map.singleton i (Right t)
  makeTypemap parentIndex (MonoBndVar (B t) i) = Map.singleton i (Right (Idx parentIndex t))
  makeTypemap _ (MonoBndVar (C t) i) = Map.singleton i (Right t)
  -- recursive calls
  makeTypemap _ (MonoManifold midx (manifoldBound -> ys) e) =
    -- bound arguments may not be used, but they where passed in from the
    -- source function, so they cannot be removed.
    Map.union (Map.fromList [(i, Left t) | (Arg i (Just t)) <- ys]) (makeTypemap midx e)
  makeTypemap parentIdx (MonoLet _ e1 e2) = Map.union (makeTypemap parentIdx e1) (makeTypemap parentIdx e2)
  makeTypemap parentIdx (MonoReturn e) = makeTypemap parentIdx e
  makeTypemap _ (MonoApp (MonoExe (ann -> idx) _) es) = Map.unionsWith mergeTypes (map (makeTypemap idx) es)
  makeTypemap parentIdx (MonoApp e es) = Map.unionsWith mergeTypes (map (makeTypemap parentIdx) (e:es))
  makeTypemap _ (MonoList (ann -> idx) _ es) = Map.unionsWith mergeTypes (map (makeTypemap idx) es)
  makeTypemap _ (MonoTuple (ann -> idx) (map snd -> es)) = Map.unionsWith mergeTypes (map (makeTypemap idx) es)
  makeTypemap _ (MonoRecord _ (ann -> idx) _ (map (snd . snd) -> es)) = Map.unionsWith mergeTypes (map (makeTypemap idx) es)
  makeTypemap _ _ = Map.empty

  mergeTypes :: Either Type (Indexed Type) -> Either Type (Indexed Type) -> Either Type (Indexed Type)
  mergeTypes (Right t) _ = Right t
  mergeTypes _ (Right t) = Right t
  mergeTypes x _ = x

  -- make (SerialAST One) then convert to serializeMany
  serializeS :: MDoc -> Int -> NativeExpr -> MorlocMonad SerialExpr
  serializeS msg m se = do
    MM.sayVVV $ "serializeS" <+> pretty m <> ":" <+> msg
    SerializeS <$> Serial.makeSerialAST m lang (typeFof se) <*> pure se

  -- infer the preferred serialization state for an expression.
  inferState :: MonoExpr -> SerializationState
  inferState (MonoApp MonoPoolCall{} _) = Serialized
  inferState (MonoApp MonoExe{} _) = Unserialized
  inferState (MonoApp (MonoManifold _ _ e) _) = inferState e
  inferState (MonoLet _ _ e) = inferState e
  inferState (MonoReturn e) = inferState e
  inferState (MonoManifold _ _ e) = inferState e
  inferState MonoPoolCall{} = Unserialized
  inferState MonoBndVar{} = error "Ambiguous bound term"
  inferState _ = Unserialized

class IsSerializable a where
  serialLet :: Int -> SerialExpr -> a -> a
  nativeLet :: Int -> NativeExpr -> a -> a

instance IsSerializable SerialExpr where
  serialLet = SerialLetS
  nativeLet = NativeLetS

instance IsSerializable NativeExpr where
  serialLet = SerialLetN
  nativeLet = NativeLetN

type (D a) = (Map.Map Int Request, a)

-- (de)serialize arguments as needed
-- determines whether arguments are passed as serialized, native, or both
wireSerial :: Lang -> SerialManifold -> MorlocMonad SerialManifold
wireSerial lang sm0@(SerialManifold m0 _ _ _ _) = foldSerialManifoldM fm sm0 |>> snd
  where
  defs = makeMonoidFoldDefault Map.empty (Map.unionWith (<>))

  fm = FoldManifoldM
    { opSerialManifoldM = wireSerialManifold
    , opNativeManifoldM = wireNativeManifold
    , opSerialExprM = wireSerialExpr
    , opNativeExprM = wireNativeExpr
    , opSerialArgM = monoidSerialArg defs
    , opNativeArgM = monoidNativeArg defs
    }

  wireSerialManifold :: SerialManifold_ (D SerialExpr) -> MorlocMonad (D SerialManifold)
  wireSerialManifold (SerialManifold_ m _ form headForm (req, e)) = do
    let form' = afirst (specialize req) form
        req' = Map.map fst (manifoldToMap form')
    e' <- letWrap m form' req e
    return (req', SerialManifold m lang form' headForm e')

  wireNativeManifold :: NativeManifold_ (D NativeExpr) -> MorlocMonad (D NativeManifold)
  wireNativeManifold (NativeManifold_ m _ form (req, e)) = do
    let form' = afirst (specialize req) form
        req' = Map.map fst (manifoldToMap form')
    e' <- letWrap m form' req e
    return (req', NativeManifold m lang form' e')

  wireSerialExpr (LetVarS_ t i) = return (Map.singleton i SerialContent, LetVarS t i)
  wireSerialExpr (BndVarS_ t i) = return (Map.singleton i SerialContent, BndVarS t i)
  wireSerialExpr (AppPoolS_ t p@(PoolCall _ _ _ pargs) args) = do
    let req1 = Map.unionsWith (<>) (map fst args)
        req2 = Map.fromList [(i, requestOf tm) | Arg i tm <- pargs]
        req3 = Map.unionWith (<>) req1 req2
    return (req3, AppPoolS t p (map snd args))

  -- for let expressions, I need to determine how the bound expression is used
  -- downstream and then (un)serialize accordingly
  wireSerialExpr (SerialLetS_ i (req1, se1) (req2, se2)) = do
    let req' = Map.unionWith (<>) req1 req2
    e' <- case Map.lookup i req2 of
      (Just NativeContent) -> case typeSof se1 of
        (SerialS tf) -> NativeLetS i <$> naturalizeN "a" m0 lang tf se1 <*> pure se2
        _ -> error "Unuseable let definition"
      (Just NativeAndSerialContent) -> error "Unsupported dual use let definition"
      _ -> return $ SerialLetS i se1 se2
    return (req', e')
  wireSerialExpr (NativeLetS_ i (req1, ne1) (req2, se2)) = do
    let req' = Map.unionWith (<>) req1 req2
    e' <- case Map.lookup i req2 of
      (Just SerialContent) -> SerialLetS i <$> serializeS "b" m0 (typeFof ne1) ne1 <*> pure se2
      (Just NativeAndSerialContent) -> error "Unsupported dual use let definition"
      _ -> return $ NativeLetS i ne1 se2
    return (req', e')

  wireSerialExpr e = monoidSerialExpr defs e

  wireNativeExpr :: NativeExpr_ (D NativeManifold) (D SerialExpr) (D NativeExpr) (D SerialArg) (D NativeArg) -> MorlocMonad (D NativeExpr)
  wireNativeExpr (LetVarN_ t i) = return (Map.singleton i NativeContent, LetVarN t i)
  wireNativeExpr (BndVarN_ t i) = return (Map.singleton i NativeContent, BndVarN t i)

  wireNativeExpr (SerialLetN_ i (req1, se1) (req2, ne2)) = do
    let req' = Map.unionWith (<>) req1 req2
    e' <- case Map.lookup i req2 of
      (Just NativeContent) -> case typeSof se1 of
        (SerialS tf) -> NativeLetN i <$> naturalizeN "a" m0 lang tf se1 <*> pure ne2
        _ -> error "Unuseable let definition"
      (Just NativeAndSerialContent) -> error "Unsupported dual use let definition"
      _ -> return $ SerialLetN i se1 ne2
    return (req', e')
  wireNativeExpr (NativeLetN_ i (req1, ne1) (req2, ne2)) = do
    let req' = Map.unionWith (<>) req1 req2
    e' <- case Map.lookup i req2 of
      (Just SerialContent) -> SerialLetN i <$> serializeS "b" m0 (typeFof ne1) ne1 <*> pure ne2
      (Just NativeAndSerialContent) -> error "Unsupported dual use let definition"
      _ -> return $ NativeLetN i ne1 ne2
    return (req', e')

  wireNativeExpr e = monoidNativeExpr defs e

  specialize :: Map.Map Int Request -> Int -> Or TypeS TypeF -> Or TypeS TypeF
  specialize req i r = case (Map.lookup i req, r) of
    (Nothing, _) -> L PassthroughS
    (Just SerialContent, LR t _) -> L t
    (Just NativeContent, LR _ t) -> R t
    _ -> r

  letWrap :: (IsSerializable e, HasRequest t, MayHaveTypeF t)
          => Int -> ManifoldForm (Or TypeS TypeF) t -> Map.Map Int Request -> e -> MorlocMonad e
  letWrap m form0 req0 e0 = foldlM wrapAsNeeded e0 (Map.toList req0) where

    formMap = manifoldToMap form0

    wrapAsNeeded :: IsSerializable e => e -> (Int, Request) -> MorlocMonad e
    wrapAsNeeded e (i, req) = case (req, Map.lookup i formMap) of
      (SerialContent,          Just (NativeContent, Just t)) -> serialLet i <$> serializeS "wan 1" m t (BndVarN t i) <*> pure e
      (NativeAndSerialContent, Just (NativeContent, Just t)) -> serialLet i <$> serializeS "wan 2" m t (BndVarN t i) <*> pure e
      (NativeContent,          Just (SerialContent, Just t)) -> nativeLet i <$> naturalizeN "wan 3" m lang t (BndVarS (Just t) i) <*> pure e
      (NativeAndSerialContent, Just (SerialContent, Just t)) -> nativeLet i <$> naturalizeN "wan 4" m lang t (BndVarS (Just t) i) <*> pure e
      _ -> return e

  manifoldToMap :: (HasRequest t, MayHaveTypeF t) => ManifoldForm (Or TypeS TypeF) t -> Map.Map Int (Request, Maybe TypeF)
  manifoldToMap form = f form where
    mapRequestFromXs xs = Map.fromList [(i, (requestOf t, mayHaveTypeF t)) | (Arg i t) <- typeMofRs xs]
    mapRequestFromYs ys = Map.fromList [(i, (requestOf t, mayHaveTypeF t)) | (Arg i t) <- ys]

    f (ManifoldFull xs) = mapRequestFromXs xs
    f (ManifoldPass ys) = mapRequestFromYs ys
    f (ManifoldPart xs ys) = Map.union (mapRequestFromXs xs) (mapRequestFromYs ys)

  serializeS :: MDoc -> Int -> TypeF -> NativeExpr -> MorlocMonad SerialExpr
  serializeS msg m t se = do
    MM.sayVVV $ "serializeS" <+> pretty m <> ":" <+> msg
    SerializeS <$> Serial.makeSerialAST m lang t <*> pure se


naturalizeN :: MDoc -> Int -> Lang -> TypeF -> SerialExpr -> MorlocMonad NativeExpr
naturalizeN msg m lang t se = do
  MM.sayVVV $ "naturalizeN at" <+> msg
  DeserializeN t <$> Serial.makeSerialAST m lang t <*> pure se


data Request = SerialContent | NativeContent | NativeAndSerialContent
  deriving(Ord, Eq, Show)

class HasRequest a where
  requestOf :: a -> Request

instance HasRequest TypeM where
  requestOf Passthrough = SerialContent
  requestOf (Serial _) = SerialContent
  requestOf (Native _) = NativeContent
  requestOf (Function _ _) = NativeContent

instance HasRequest SerialExpr where
  requestOf _ = SerialContent

instance HasRequest NativeExpr where
  requestOf _ = NativeContent

instance HasRequest SerialArg where
  requestOf _ = SerialContent

instance HasRequest NativeArg where
  requestOf _ = NativeContent

instance HasRequest TypeS where
  requestOf _ = SerialContent

instance HasRequest TypeF where
  requestOf _ = NativeContent


instance Semigroup Request where
 SerialContent <> SerialContent = SerialContent
 NativeContent <> NativeContent = NativeContent
 _ <> _ = NativeAndSerialContent

data SerializationState = Serialized | Unserialized
  deriving(Show, Eq, Ord)


-- Sort manifolds into pools. Within pools, group manifolds into call sets.
pool :: [SerialManifold] -> [(Lang, [SerialManifold])]
pool es =
    -- [SerialManifold] --> [(Lang, [(Int, SerialManifold)])]
    let (langs, indexedSegments) = unzip . groupSort . map (\x@(SerialManifold i lang _ _ _) -> (lang, (i, x))) $ es
        {-
        Each of the `SerialManifold` values is represents a single subtree of the
        program and may thus contain many nested manifolds. Each is thus the root
        of a tree. If two of these trees share a same root and language, then they
        should contain the children. So here we prune the duplicate trees.
        -}
        uniqueSegments = map (Map.elems . Map.fromList) indexedSegments
    in zip langs uniqueSegments

encode
  :: Lang
  -> [SerialManifold]
  -- ^ The input preserves the connection between the AST and the specific
  -- sources it uses, currently this information is not used. However, in the
  -- future it may sometimes be necessary to split the functions in one
  -- language into multiple pools (e.g., to resolve version conflicts).
  -> MorlocMonad Script
encode lang xs = do
  srcs' <- findSources xs
  xs' <- mapM (preprocess lang) xs
  -- translate each node in the AST to code
  translate lang srcs' xs'

findSources :: [SerialManifold] -> MorlocMonad [Source]
findSources ms = unique <$> concatMapM (foldSerialManifoldM fm) ms
  where
  fm = defaultValue
    { opSerialExprM = serialExprSrcs
    , opNativeExprM = nativeExprSrcs
    , opNativeManifoldM = nativeManifoldSrcs
    , opSerialManifoldM = nativeSerialSrcs
    }

  nativeExprSrcs (AppExeN_ _ (SrcCallP src) _ xss) = return (src : concat xss)
  nativeExprSrcs (ExeN_ _ (SrcCallP src)) = return [src]
  nativeExprSrcs (DeserializeN_ _ s xs) = return $ serialASTsources s <> xs
  nativeExprSrcs e = return $ foldlNE (<>) [] e

  serialExprSrcs (SerializeS_ s xs) = return $ serialASTsources s <> xs
  serialExprSrcs e = return $ foldlSE (<>) [] e

  -- Collect sources for all type (un)packers that are used in serialization
  serialASTsources :: SerialAST -> [Source]
  serialASTsources (SerialPack _ (p, s)) = [ typePackerForward p, typePackerReverse p ] <> serialASTsources s
  serialASTsources (SerialList _ s) = serialASTsources s
  serialASTsources (SerialTuple _ ss) = concatMap serialASTsources ss
  serialASTsources (SerialObject _ _ _ (map snd -> ss)) = concatMap serialASTsources ss
  serialASTsources _ = []

  nativeManifoldSrcs (NativeManifold_ m lang _ e) = (<>) e <$> lookupConstructors lang m
  nativeSerialSrcs (SerialManifold_ m lang _ _ e) = (<>) e <$> lookupConstructors lang m

  -- Find object constructors that are NOT defined (un)pack functions
  -- These are object constructors imported from the concrete sources that are
  -- used as concrete types. For example:
  --   source py from "person.py" ("PersonObj")
  --   table (Person a) = Person {name :: Str, info :: a}
  --   table py (Person a) = "PersonObj" {name :: "str", info :: a}
  lookupConstructors :: Lang -> Int -> MorlocMonad [Source]
  lookupConstructors lang i = MM.metaSources i |>> filter ((==) lang . srcLang)


translate :: Lang -> [Source] -> [SerialManifold] -> MorlocMonad Script
translate lang srcs es = do
  case lang of
    CppLang -> Cpp.translate srcs es
    RLang -> R.translate srcs es
    Python3Lang -> Python3.translate srcs es
    x -> MM.throwError . PoolBuildError . render
      $ "Language '" <> viaShow x <> "' has no translator"

preprocess :: Lang -> SerialManifold -> MorlocMonad SerialManifold
preprocess CppLang es = Cpp.preprocess es
preprocess RLang es = R.preprocess es
preprocess Python3Lang es = Python3.preprocess es
preprocess l _ = MM.throwError . PoolBuildError . render
               $ "Language '" <> viaShow l <> "' has no translator"


sannoSnd :: AnnoS g One (a, b) -> b
sannoSnd (AnnoS _ (_, x) _) = x

-- generate infinite list of fresh variables of form
-- ['a','b',...,'z','aa','ab',...,'zz',...]
freshVarsAZ
  :: [Text] -- variables to exclude
  -> [Text]
freshVarsAZ exclude =
  filter
    (`notElem` exclude)
    ([1 ..] >>= flip replicateM ['a' .. 'z'] |>> MT.pack)
