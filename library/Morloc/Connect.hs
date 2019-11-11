{-|
Module      : Morloc.Connect
Description : Convert list of modules into manifold graph
Copyright   : (c) Zebulun Arendsee, 2019
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.Connect
  ( connect
  ) where

import Control.Monad.State (get, gets, modify, put)
import Morloc.Namespace
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.System as MS
import qualified Morloc.TypeChecker.Infer as MTI

import Morloc.Data.Doc hiding (putDoc)
import Morloc.Pretty
import Data.Text.Prettyprint.Doc.Render.Terminal (putDoc, AnsiStyle)

connect :: [Module] -> MorlocMonad [Manifold]
connect mods
  -- root :: Module -- fail if there is not a single "root" module, e.g., main
 = do
  root <- rootModule mods
  -- ms :: [manifold]
  -- build all manifold paths starting from exported root declarations the
  -- state monad handles scope and module attributes as well as assignment of
  -- unique integer IDs to all manifolds.
  modelState <- initProgramState mods
  (ms, _) <-
    liftIO . (flip MM.runStateT) modelState $
    fmap concat (mapM (module2manifolds root) (moduleBody root))
  -- extract and store the serialization functions
  hss <- makeSerialMaps mods
  modify (\s -> s {stateSerialMaps = hss})
  -- return [Manifold]
  return ms

-- | local
data Source' =
  Source'
    { sourcePath :: Maybe Path
    , sourceLang :: Lang
    , sourceName :: Name
    , sourceAlias :: Name
    }
  deriving (Show, Ord, Eq)

-- | local
type Program a = MM.StateT ProgramState IO a

-- | local
data ProgramState =
  ProgramState
    { stateCount :: Integer
    , stateModuleSources :: Map.Map MVar (Map.Map EVar Source')
    , stateModuleExports :: Map.Map MVar (Set.Set EVar)
    , stateModuleImports :: Map.Map MVar (Map.Map EVar (MVar, EVar))
    , stateModuleDeclarations :: Map.Map MVar (Map.Map EVar Expr)
    , stateModuleTypeSetMap :: Map.Map MVar (Map.Map EVar TypeSet)
    , stateModuleMap :: Map.Map MVar Module
    }
  deriving (Show, Ord, Eq)

say :: Doc AnsiStyle -> Program ()
say d = liftIO . putDoc $ " : " <> d <> "\n"

initProgramState :: [Module] -> MorlocMonad ProgramState
initProgramState mods
  -- typesetmap :: Map.Map MVar (Map.Map EVar TypeSet)
  -- all type information within each module (no inheritance) is collected into
  -- one TypeSet object (TypeSet (Maybe EType) [EType]), where the [EType] list
  -- stores all realizations and the (Maybe EType) is the general type. This
  -- information may be stored across multiple signatures and declarations.
 = do
  let typmap = makeTypeSetMap mods
  return $
    ProgramState
      { stateCount = 0 -- counter used to create manifold IDs
      , stateModuleSources = lmap mksrcmap mods
      , stateModuleExports = lmap mkexpmap mods
      , stateModuleImports = lmap mkimpmap mods
      , stateModuleDeclarations = lmap mkdecmap mods
      , stateModuleTypeSetMap = typmap
      , stateModuleMap = modmap
      }
  where
    lmap f ms = Map.fromList $ map (\m -> (moduleName m, f m)) ms

    modmap = Map.fromList [(moduleName m, m) | m <- mods]

    mksrcmap :: Module -> Map.Map EVar Source'
    mksrcmap m =
      Map.unions
        [ makeSource (modulePath m) x y z
        | SrcE x y z <- moduleBody m
        ]

    mkexpmap :: Module -> Set.Set EVar
    mkexpmap m = Set.fromList (moduleExports m)

    mkimpmap :: Module -> Map.Map EVar (MVar, EVar)
    mkimpmap m =
      Map.fromList
        [ (alias, (mv, ev))
        | (mv, ev, alias) <- concat [getImports modmap i | i <- moduleImports m]
        ]

    mkdecmap :: Module -> Map.Map EVar Expr
    mkdecmap m = Map.fromList [(v, e) | (Declaration v e) <- moduleBody m]

getId :: Program Integer
getId = do
  s <- get
  let i = stateCount s
  put (s {stateCount = i + 1})
  return i

makeTypeSetMap :: [Module] -> Map.Map MVar (Map.Map EVar TypeSet)
makeTypeSetMap mods =
  Map.fromList [(moduleName m, moduleTypeMap m) | m <- mods]

modExports :: MVar -> Program (Set.Set EVar)
modExports v = fmap (Map.findWithDefault Set.empty v) (gets stateModuleExports)

modDeclarations :: MVar -> Program (Map.Map EVar Expr)
modDeclarations v =
  fmap (Map.findWithDefault Map.empty v) (gets stateModuleDeclarations)

lookupImport :: MVar -> EVar -> Program (Maybe (MVar, EVar))
lookupImport m e = do
  impmap <- gets stateModuleImports
  return $ Map.lookup m impmap >>= Map.lookup e

lookupSources :: MVar -> EVar -> Program (Maybe Source')
lookupSources m e = do
  srcmap <- gets stateModuleSources
  return $ Map.lookup m srcmap >>= Map.lookup e

lookupTypeSet :: MVar -> EVar -> Program (Maybe TypeSet)
lookupTypeSet mv ev = do
  ts <- gets stateModuleTypeSetMap
  return $ Map.lookup mv ts >>= Map.lookup ev

-- searches the import tree for existence of a declaration
isDeclared :: MVar -> EVar -> Program Bool
isDeclared m e = do
  modmap <- gets stateModuleMap
  decmap <- gets stateModuleDeclarations
  imp <- lookupImport m e
  case (fmap (Map.member e) (Map.lookup m decmap), imp) of
    -- if e is declared
    (Just True, _) -> return True
    -- if e is imported, search the imported module
    (Nothing, Just (m', e')) -> isDeclared m' e'
    -- otherwise it is not declared (i.e., it must be sourced somewhere)
    _ -> return False

isExported :: MVar -> EVar -> Program Bool
isExported mv ev = do
  exportMap <- modExports mv
  return $ Set.member ev exportMap

-- | Type annotations store type infor as `[Type]`, where `Nothing` indicates
-- the general type. There should be exactly one general type for every
-- expression in a program. This could change in the future if we decide to add
-- overloading.
getGeneralType :: [Type] -> Program Type
getGeneralType xs = case [t | t <- xs, langOf t == Nothing] of
  [] -> error "MissingGeneralType"
  [x] -> return x
  _ -> error "AmbiguousGeneralType"

-- | compile a declaration, for example:
--   foo x y = bar x (baz 42 y)
-- "foo" is the name of a lambda, not an application. "foo" can be exported
-- from the nexus as a subcommand the user can call with arguments x and y.
-- "bar" is the function that is called in the application "bar x (baz 42 y)".
module2manifolds :: Module -> Expr -> Program [Manifold]
module2manifolds m (Declaration (EV lambdaName) e0@(AnnE lambda@(LamE _ _) _)) = do
  let ts = functionTypes e0
  gentype <- getGeneralType ts
  i <- getId
  exported <- isExported (moduleName m) (EV lambdaName)
  case (uncurryExpr lambda) of
    (_, _, []) -> do
      say $ prettyScream ("======= " <> lambdaName <> " ======")
      say $ prettyExpr lambda
      error $ "nexus can only accept applications in declarations"
    -- FIXME: this only allows named function, not expressions like:
    --   foo x y = (bar x) 42 y  -- where bar returns a function
    --   foo x y = (x,y)         -- where there is no function on the right
    (vars, (exprAsFunction -> EV functionName), es) -> do
      typeset <- lookupVar (EV functionName) m
      case typeset of
        (TypeSet Nothing _) -> error $
          "No general type for " ++ MT.unpack functionName
        (TypeSet (Just e) rs) -> do
          args <- zipWithM (exprAsArgument vars m) [0 ..] es
          declared <- isDeclared (moduleName m) (EV functionName)
          
          -- say $ prettyScream "module2manifolds Declaration"

          realizations <- toRealizations functionName m (length args) rs ts
          return . (flip (:)) (concat . map snd $ args) $
            Manifold
              { mid = i
              , mCallId = makeURI (moduleName m) i
              , mAbstractType =
                  Just (etype2mtype (Just lambdaName) (e {etype = gentype}))
              , mRealizations = realizations
              , mMorlocName = functionName
              , mExported = exported
              , mCalled = True
              , mDefined = declared
              , mPassed = False
              , mComposition = Just lambdaName
              , mBoundVars = [v' | (EV v') <- vars]
              , mArgs = map fst args
              }
module2manifolds m (Signature ev@(EV v) t) = do
  exported <- isExported (moduleName m) ev
  declared <- isDeclared (moduleName m) ev
  -- If v is exported but not declared, then it must refer directly to a source
  -- function (not a morloc lambda). Also, skip this signature if it is a
  -- realization signature.

  -- * do not generate if not exported, because the function does not need to
  --   exist independently, but will later be instantiated as needed when
  --   called in compositions
  -- * do not generated declared modules since they will be generated when
  --   Declaration is parsed
  -- * do not generate for concrete languages then all languages are merged
  --   together within this one manifold
  if exported && not declared && (langOf . etype) t == Nothing
    then do
      i <- getId
      (TypeSet _ rs) <- lookupVar ev m
      let args = map ArgPosi (take (nargs (etype t)) [0 ..])
          
      -- say $ prettyScream "module2manifolds Signature"

      realizations <- toRealizations v m (length args) rs []
      return
        [ Manifold
            { mid = i
            , mCallId = makeURI (moduleName m) i
            , mAbstractType = Just (etype2mtype (Just v) t)
            , mRealizations = realizations
            , mMorlocName = v
            , mExported = exported
            , mCalled = False
            , mDefined = declared
            , mPassed = False
            , mComposition = Nothing
            , mBoundVars = []
            , mArgs = args
            }
        ]
    else do
      return []
module2manifolds _ _ = return []

nargs :: Type -> Int
nargs (FunT _ t2) = 1 + nargs t2
nargs _ = 0

lookupExpr :: Module -> EVar -> Maybe Expr
lookupExpr m v = case [e | e@(Declaration v' _) <- moduleBody m, v' == v] of
  [e] -> Just e
  [ ] -> Nothing

exprAsArgument ::
     [EVar]
  -> Module
  -> Int -- ^ 0-indexed position of the argument
  -> Expr
  -> Program (Argument, [Manifold])
exprAsArgument bnd m i (AnnE (VarE v@(EV v')) argtypes)
  | elem v bnd = return (ArgName v', [])
  | otherwise =
      case lookupExpr m v of
        (Just (Declaration _ e@(AnnE (ListE  _) _))) -> exprAsArgument bnd m i e
        (Just (Declaration _ e@(AnnE (TupleE _) _))) -> exprAsArgument bnd m i e
        (Just (Declaration _ e@(AnnE (AppE _ _) _))) -> exprAsArgument bnd m i e
        (Just (Declaration _ e@(AnnE (NumE   _) _))) -> exprAsArgument bnd m i e
        (Just (Declaration _ e@(AnnE (LogE   _) _))) -> exprAsArgument bnd m i e
        (Just (Declaration _ e@(AnnE (StrE   _) _))) -> exprAsArgument bnd m i e
        (Just (Declaration _ e@(AnnE (RecE   _) _))) -> exprAsArgument bnd m i e
        (Just e) -> do
          ms' <- module2manifolds m e
          let m' = (head . reverse $ ms') { mPassed = True, mExported = False }
          return (ArgNest ("m" <> (MT.show' . mid) m'), init ms' ++ [m'])
        Nothing -> do
          i <- getId
          Just (TypeSet gentype ts) <- lookupTypeSet (moduleName m) v
          realizations <- toRealizations v' m (length argtypes) ts argtypes
          let uri = makeURI (moduleName m) i
          let man = Manifold {
              mid = i
            , mCallId = uri
            , mAbstractType = fmap (etype2mtype (Just v')) gentype
            , mRealizations = realizations
            , mMorlocName = v'
            , mExported = False
            , mCalled = False
            , mDefined = False
            , mPassed = True
            , mComposition = Nothing
            , mBoundVars = ["x" <> MT.show' i' | i' <- take (nargs (argtypes !! 0)) [0..]]
            , mArgs = [ArgPosi i' | i' <- take (nargs (argtypes !! 0)) [0..]]
            }
          return (ArgNest ("m" <> MT.show' i), [man])

exprAsArgument bnd m _ (AnnE e0@(AppE (AnnE e1 _) e2) _) =
  case uncurryApplication e1 e2 of
    (f, es) -> do
      let ts = functionTypes e0
      gentype <- getGeneralType ts
      let v@(EV mname) = exprAsFunction f
      defined <- isDeclared (moduleName m) v
      i <- getId
      t <- lookupVar v m
      case t of
        (TypeSet Nothing _) -> error "ah fuck shit"
        (TypeSet (Just etyp) rs) -> do
          args <- zipWithM (exprAsArgument bnd m) [0 ..] es
          
          -- say $ prettyScream "exprAsArgument"

          realizations <- toRealizations mname m (length args) rs ts
          let newManifold =
                Manifold
                  { mid = i
                  , mCallId = makeURI (moduleName m) i
                  , mAbstractType =
                      Just (etype2mtype (Just mname) (etyp {etype = gentype}))
                  , mRealizations = realizations
                  , mMorlocName = mname -- TODO: really?
                  , mExported = elem v (moduleExports m)
                  , mCalled = True
                  , mDefined = defined
                  , mPassed = False
                  , mComposition = Nothing
                  , mBoundVars = [b | (EV b) <- bnd]
                  , mArgs = map fst args
                  }
          return
            ( ArgCall (makeURI (moduleName m) i)
            , newManifold : (concat . map snd $ args))
-- ArgData primitives
exprAsArgument _ _ _ (AnnE x@(NumE _) _) =
  return (ArgData $ primitive2mdata x, [])
exprAsArgument _ _ _ (AnnE x@(LogE _) _) =
  return (ArgData $ primitive2mdata x, [])
exprAsArgument _ _ _ (AnnE x@(StrE _) _) =
  return (ArgData $ primitive2mdata x, [])
exprAsArgument _ _ _ (AnnE x@(ListE _) _) =
  return (ArgData $ primitive2mdata x, [])
exprAsArgument _ _ _ (AnnE x@(TupleE _) _) =
  return (ArgData $ primitive2mdata x, [])
exprAsArgument _ _ _ (AnnE x@(RecE _) _) =
  return (ArgData $ primitive2mdata x, [])
-- errors       
exprAsArgument _ _ _ (AnnE (LamE _ _) ts) = do
  gentype <- getGeneralType ts
  case gentype of
    (FunT _ _) -> error "lambdas not yet supported"
exprAsArgument _ _ _ _ = error "expected annotated expression"

functionTypes :: Expr -> [Type]
functionTypes (AnnE (VarE _) ts) = ts
functionTypes (AnnE (LamE _ e) _) = functionTypes e
functionTypes (AnnE e _) = functionTypes e
functionTypes (AppE e _) = functionTypes e
functionTypes e = error $ "Expected a function, found: " <> show e

makeURI :: MVar -> Integer -> URI
makeURI (MV v) i = URI $ v <> "_" <> MT.show' i

exprAsFunction :: Expr -> EVar
exprAsFunction (VarE v) = v
exprAsFunction (AnnE (VarE v) _) = v
exprAsFunction e =
  error $ "I'm sorry man, I can only handle VarE, not this: " <> show e

-- TODO: allow anything inside a container
primitive2mdata :: Expr -> MData
primitive2mdata (AnnE t _) = primitive2mdata t
primitive2mdata (NumE x) = Num' (MT.show' x)
primitive2mdata (LogE x) = Log' x
primitive2mdata (StrE x) = Str' x
primitive2mdata (ListE es) = Lst' $ map primitive2mdata es
primitive2mdata (TupleE es) = Lst' $ map primitive2mdata es
primitive2mdata (RecE xs) = Rec' $ map entry xs
  where
    entry :: (EVar, Expr) -> (Name, MData)
    entry (EV v, e) = (v, primitive2mdata e)
primitive2mdata _ =
  error "complex stuff is not yet allowed in MData (coming soon)"

toRealizations :: Name -> Module -> Int -> [EType] -> [Type] -> Program [Realization]
toRealizations n m _ [] [] = do
  src <- lookupSources (moduleName m) (EV n)
  case src of
    Nothing -> error $ "Bad realization: could not find '" <> MT.unpack n <> "'"
    (Just s) ->
      return
        [ Realization
            { rLang = sourceLang s
            , rName = sourceName s
            , rConcreteType = Nothing
            , rModulePath = modulePath m
            , rSourcePath = sourcePath s
            , rSourced = True
            }
        ]
toRealizations n m _ es ts = do
  let linked = linkTypes es ts

  -- say $ prettyScream "-------------------------------"
  -- say $ "n: " <> pretty n
  -- say $ "linked: " <> (align . vsep . map (\(e,t)->(prettyGreenType . etype) e <> " | " <> prettyGreenType t)) linked

  return . map toRealization $ linked 
  where
    linkTypes :: [EType] -> [Type] -> [(EType, Type)]
    linkTypes es' [] = [(e, etype e) | e <- es', (isJust . langOf) e]
    linkTypes es' ts' = map (linkType [(langOf e, e) | e <- es'])
                            (filter (isJust . langOf) ts')

    linkType :: [(Maybe Lang, EType)] -> Type -> (EType, Type)
    linkType es' t = case lookup (langOf t) es' of
      (Just e) -> (e, t)
      Nothing -> (MTI.fromType (langOf t) t, t)

    toRealization :: (EType, Type) -> Realization
    toRealization (e@(EType _ _ _ (Just (f, EV srcname))), t) =
      Realization
        { rLang = langOf' t
        , rName = srcname
        , rConcreteType = Just $ etype2mtype Nothing (e {etype = t})
        , rModulePath = modulePath m
        , rSourcePath = f -- if Nothing, then $srcname is a builtin of $lang
        , rSourced = True
        }
    toRealization (e@(EType _ _ _ Nothing), t) =
      Realization
        { rLang = langOf' t
        , rName = n
        , rConcreteType = Just $ etype2mtype Nothing (e {etype = t})
        , rModulePath = modulePath m
        , rSourcePath = Nothing
        , rSourced = False
        }

-- | uncurry one level of an expression, pulling out a tuple with
-- (<lambda-args>, <application-base>, <application-args>)
-- Examples:
-- @5@              ==> ([], 5, [])
-- @f a b@          ==> ([], f, [a,b])
-- @f a b = g 4 a b ==> ([a,b], g, [4,a,b])
-- @f x = g (h x) 5 ==> ([x], g, [(h x), 5]  -- no recursion into expressions
uncurryExpr :: Expr -> ([EVar], Expr, [Expr])
uncurryExpr (AnnE e _) = uncurryExpr e
uncurryExpr (LamE v e) = case uncurryExpr e of
  (vs, e', es) -> (v:vs, e', es)
uncurryExpr (AppE e1 e2) = case uncurryApplication e1 e2 of
  (e, es) -> ([], e, es)
uncurryExpr e = ([], e, [])

-- | uncurry an application
-- Examples:
-- @f x@  ==> (f, [x])
-- @f x (g y)@  ==> (f, [x, (g y)])  -- no recursion
uncurryApplication ::
     Expr -- "base" of an application (e.g., @f@ in @f x@)
  -> Expr -- argument in an application (e.g., @x@ in @f x@)
  -> (Expr, [Expr])
uncurryApplication (AppE e1 e2) e0 =
  onSnd ((flip (++)) [e0]) (uncurryApplication e1 e2)
uncurryApplication (AnnE (AppE e1 e2) _) e0 =
  onSnd ((flip (++)) [e0]) (uncurryApplication e1 e2)
uncurryApplication f en = (f, [en])

onSnd :: (b -> c) -> (a, b) -> (a, c)
onSnd f (x, y) = (x, f y)

makeSource ::
     Maybe Path
  -> Lang
  -> Maybe Path
  -> [(EVar, EVar)]
  -> Map.Map EVar Source'
makeSource mpath lang f xs =
  Map.fromList $ map (\(EV n, EV a) -> (EV a, Source' path lang n a)) xs
  where
    path = MS.combine <$> fmap MS.takeDirectory mpath <*> f

rootModule :: [Module] -> MorlocMonad Module
rootModule ms =
  case roots of
    [root] -> return root
    [] -> MM.throwError . GeneratorError $ "cyclic dependency"
    _ -> MM.throwError . GeneratorError $ "expected a unique root"
  where
    mset =
      Set.fromList . concat $
      [[importModuleName x | x <- moduleImports m] | m <- ms]
    roots = filter (\m -> not $ Set.member (moduleName m) mset) ms

makeSerialMaps :: [Module] -> MorlocMonad (Map.Map Lang SerialMap)
makeSerialMaps (concat . map moduleBody -> es) =
  fmap
    (Map.mapWithKey toSerialMap -- Map Lang SerialMap
      .
     Map.fromList -- Map Lang [Expr]
      .
     groupSort -- [(Lang, [Expr])]
      .
     catMaybes) $
  mapM f es -- MorloMonad [Maybe (Lang, Expr)]
    -- collect all sources and signatures paired with language
    -- needed for @toSerialMap@
    -- convert textual language names to Lang's
  where
    f :: Expr -> MorlocMonad (Maybe (Lang, Expr))
    f e@(SrcE lang _ _) = return $ Just (lang, e)
    f e@(Signature _ t) = return $ Just ((langOf' . etype) t, e)
    f _ = return $ Nothing

    toSerialMap :: Lang -> [Expr] -> SerialMap
    toSerialMap lang es' =
      SerialMap
        { serialLang = lang
        , serialPacker = getSerial Pack es'
        , serialUnpacker = getSerial Unpack es'
        , serialSources = nub [p | (SrcE _ (Just p) _) <- es']
        }

getSerial :: Property -> [Expr] -> Map.Map MType Name
getSerial p es =
  Map.fromList
    [ (f (etype2mtype Nothing e), n)
    | (Signature (EV n) e) <- es
    , Set.member p (eprop e)
    ]
  where
    f (MFuncType _ inputs output) =
      case (p, inputs) of
        (Pack, [x]) -> x
        (Unpack, [_]) -> output
        _ -> error "too many args in serial function"
    f _ = error "wrong type for serial function"

etype2mtype :: Maybe Name -> EType -> MType
etype2mtype n e = type2mtype Set.empty (etype e)
  where
    meta =
      MTypeMeta
        { metaName = n
        , metaProp = map prop2text (Set.toList (eprop e))
        , metaLang = (langOf . etype) e
        }
    metaEmpty =
      MTypeMeta
        { metaName = Nothing
        , metaProp = []
        , metaLang = (langOf . etype) e
        }
    prop2text Pack = ["packs"]
    prop2text Unpack = ["unpacks"]
    prop2text Cast = ["casts"]
    prop2text (GeneralProperty ts) = ts

    type2mtype :: Set.Set Name -> Type -> MType
    type2mtype bnds (VarT (TV _ v))
      | Set.member v bnds = MAbstType meta v []
      | otherwise = MConcType meta v []
    type2mtype _ (ExistT _ []) = error "found existential type"
    type2mtype bnds (Forall (TV _ v) t) = (type2mtype (Set.insert v bnds) t)
    type2mtype bnds (FunT t1 t2) =
      let ts = type2mtype bnds t1 : functionTypes' bnds t2
       in MFuncType meta (init ts) (last ts)
    type2mtype bnds (ArrT (TV _ v) ts)
      | Set.member v bnds = MAbstType meta v (map (type2mtype bnds) ts)
      | otherwise = MConcType meta v (map (type2mtype bnds) ts)
    type2mtype bnds (RecT fs) =
      MConcType meta "Record" (map (recordEntry bnds) fs)
    type2mtype _ t = error $ "cannot cast type: " <> show t
    recordEntry :: Set.Set Name -> (TVar, Type) -> MType
    recordEntry bnds (TV _ _, t) =
      MConcType
        metaEmpty
        "RecordEntry"
        [MConcType metaEmpty "Str" [], type2mtype bnds t]
    functionTypes' :: Set.Set Name -> Type -> [MType]
    functionTypes' bnds (FunT t1 t2) = type2mtype bnds t1 : functionTypes' bnds t2
    functionTypes' bnds t = [type2mtype bnds t]

-- | Lookup a variable in a given module. If collect any type information
-- (including realizations). Whether the variable is found or not, recurse into
-- all imported modules searching through exported variables for additional
-- signatures describing the variable. Collect all information in the returned
-- TypeSet object. Die if there is disagreement about the basic general type.
lookupVar :: EVar -> Module -> Program TypeSet
lookupVar v0 m0 = do
  mm <- gets stateModuleMap
  tm <- gets stateModuleTypeSetMap
  return $ lookupVar' mm tm v0 m0
  where
    lookupVar' mm tm v m =
      case Map.lookup (moduleName m) tm >>= Map.lookup v of
        Nothing -> foldr (joinTypeSet const) (TypeSet Nothing []) (r mm tm v m)
        (Just t) -> foldr (joinTypeSet const) t (r mm tm v m)
    r mm tm v m =
      [ maybe
        (TypeSet Nothing [])
        id
        (fmap (lookupVar' mm tm v') $ Map.lookup mv' mm)
      | (mv', v', alias') <- concat [getImports mm i | i <- (moduleImports m)]
      , v == alias'
      ]

getImports :: Map.Map MVar Module -> Import -> [(MVar, EVar, EVar)]
getImports _ (Import mv (Just xs) _ _) = [(mv, v, a) | (v, a) <- xs]
getImports mm (Import mv Nothing _ _) = do
  case Map.lookup mv mm of
    Nothing -> error "Cannot find module"
    (Just m') -> [(mv, v, v) | v <- moduleExports m']

type2etype :: Type -> EType
type2etype t =
  EType
    { etype = t
    , eprop = Set.empty
    , econs = Set.empty
    , esource = Nothing
    }

insertAppendEtype ::
     (EVar, EType) -> Map.Map EVar TypeSet -> Map.Map EVar TypeSet
insertAppendEtype (v, t) =
  Map.insertWith (joinTypeSet f) v (TypeSet (Just t) [])
  where
    f edec eold = eold {etype = etype edec}

appendTypeSet :: EType -> TypeSet -> TypeSet
appendTypeSet e@(langOf . etype -> Nothing) (TypeSet _ es) = TypeSet (Just e) es
appendTypeSet e (TypeSet x es) = TypeSet x (e : es)

joinTypeSet :: (EType -> EType -> EType) -> TypeSet -> TypeSet -> TypeSet
joinTypeSet f (TypeSet g1 es1) (TypeSet g2 es2) =
  foldr appendTypeSet (TypeSet (xor f g1 g2) es1) es2

xor :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
xor _ (Just x) Nothing = Just x
xor _ Nothing (Just x) = Just x
xor f (Just x) (Just y) = Just (f x y)
xor _ _ _ = Nothing
