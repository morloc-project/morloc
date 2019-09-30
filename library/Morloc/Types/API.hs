{-|
Module      : Morloc.Types.API
Description : The primary API for the morloc type system
Copyright   : (c) Zebulun Arendsee, 2019
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Types.API (
    parse
  , typecheck
  , cute
  , ugly
  , ignoreSource
  , localModules
  , prettyExpr
  , prettyType
  , prettyModule
  , rootModule
  , module2manifolds
  , initProgramState
  , makeSerialMaps
  , XP.readType
  , Module(..)
  , MVar(..)
  , EVar(..)
  , TVar(..)
  , Expr(..)
  , EType(..)
  , Type(..)
  , Property(..)
  , Constraint(..)
  , TypeError(..)
  , TypeSet(..)
  , Language
) where

import Morloc.Global
import Morloc.Operators
import Morloc.Types.Namespace
import qualified Morloc.Data.Doc as MD
import qualified Morloc.Data.Text as MT
import qualified Morloc.Language as ML
import qualified Morloc.Monad as MM
import qualified Morloc.Types.Infer as XI 
import qualified Morloc.Types.Parser as XP

import Control.Monad.State (State, evalState, gets, get, put)
import Data.Text.Prettyprint.Doc.Render.Terminal (putDoc)
import qualified Control.Monad as CM
import qualified Data.List as DL
import qualified Data.List.Extra as DLE
import qualified Data.Map as Map
import qualified Data.Maybe as DM
import qualified Data.Set as Set
import qualified Data.Text.IO as DIO
import qualified System.FilePath.Posix as SFP

parse
  :: (Path -> IO ()) -- ^ check existence of a source (file, URL, or whatever)
  -> (MVar -> IO (Maybe Path, MT.Text)) -- ^ open a module (file, URL, or whatever)
  -> Maybe Path
  -> MT.Text -- ^ code of the current module
  -> IO [Module]
parse checkSource loadModule f code = fmap Map.elems $ parse' Map.empty (f, code) where
  parse' :: (Map.Map MVar Module) -> (Maybe Path, MT.Text) -> IO (Map.Map MVar Module)
  parse' visited (f', code') = CM.foldM parse'' visited (XP.readProgram f' code')

  parse'' :: (Map.Map MVar Module) -> Module -> IO (Map.Map MVar Module)
  parse'' visited m
    | Map.member (moduleName m) visited = return visited
    | otherwise = do
        checkSources checkSource m
        imports <- mapM (loadModule . importModuleName) (moduleImports m)
        CM.foldM parse' (Map.insert (moduleName m) m visited) imports

-- assert that all sourced resources exist
checkSources :: (Path -> IO ()) -> Module -> IO ()
checkSources f m = chk' (moduleBody m) where
  chk' ((SrcE _ (Just filename) _):es) = f filename >> chk' es
  chk' (_:es) = chk' es
  chk' [] = return ()

typecheck
  :: [Module]
  -> Either TypeError [Module]
typecheck ms =
  case runStack (XI.typecheck ms) of
    (Right result, _) -> Right result
    (Left err, _) -> Left err

cute :: Either TypeError [Module] -> IO ()
cute (Right es) = mapM_ (\e -> putDoc (prettyModule e) >> putStrLn "") es
cute (Left err) = print err

ugly :: Either TypeError [Module] -> IO ()
ugly (Right es) = print es
ugly (Left err) = print err

-- do not check existence of source files
ignoreSource :: MT.Text -> IO () 
ignoreSource _ = return ()

localModules :: Maybe String -> MVar -> IO (Maybe Path, MT.Text)
localModules (Just filename) (MV f) = do
  code <- DIO.readFile . SFP.replaceFileName filename $ (MT.unpack f <> ".loc")
  return (Just (MT.pack filename), code)
localModules Nothing (MV f) = do
  let filename = MT.unpack f <> ".loc"
  code <- DIO.readFile filename
  return (Just . MT.pack $ filename, code)



-- | local
data Source' = Source' {
    sourcePath :: Maybe Path
  , sourceLang :: Lang
  , sourceName :: Name
  , sourceAlias :: Name
} deriving(Show, Ord, Eq)

-- | local
type Program a = MM.StateT ProgramState IO a

-- | local
data ProgramState = ProgramState {
    stateCount :: Integer
  , stateModuleSources :: Map.Map MVar (Map.Map EVar Source')
  , stateModuleExports :: Map.Map MVar (Set.Set EVar)
  , stateModuleImports :: Map.Map MVar (Map.Map EVar (MVar, EVar))
  , stateModuleDeclarations :: Map.Map MVar (Map.Map EVar Expr)
  , stateModuleTypeSetMap :: Map.Map MVar (Map.Map EVar TypeSet)
  , stateModuleMap :: Map.Map MVar Module
} deriving(Show, Ord, Eq)

initProgramState :: [Module] -> MorlocMonad ProgramState
initProgramState mods = do
  -- typesetmap :: Map.Map MVar (Map.Map EVar TypeSet)
  -- all type information within each module (no inheritance) is collected into
  -- one TypeSet object (TypeSet (Maybe EType) [EType]), where the [EType] list
  -- stores all realizations and the (Maybe EType) is the general type. This
  -- information may be stored across multiple signatures and declarations.
  let typmap = makeTypeSetMap mods

  return $ ProgramState {
        stateCount = 0 -- counter used to create manifold IDs
      , stateModuleSources      = lmap mksrcmap mods -- \
      , stateModuleExports      = lmap mkexpmap mods -- |  These are all global
      , stateModuleImports      = lmap mkimpmap mods --  \ constructs that can
      , stateModuleDeclarations = lmap mkdecmap mods --  / be  used anywhere
      , stateModuleTypeSetMap   = typmap             -- |
      , stateModuleMap          = modmap             -- /
    }
  where
    lmap f ms = Map.fromList $ map (\m -> (moduleName m, f m)) ms

    modmap = Map.fromList [(moduleName m, m) | m <- mods]

    mksrcmap :: Module -> Map.Map EVar Source'
    mksrcmap m = Map.unions [makeSource (ML.readLangName x) y z | SrcE x y z <- moduleBody m]

    mkexpmap :: Module -> Set.Set EVar
    mkexpmap m = Set.fromList (moduleExports m)

    mkimpmap :: Module -> Map.Map EVar (MVar, EVar)
    mkimpmap m = Map.fromList
      [(alias, (mv, ev))
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
makeTypeSetMap mods = Map.fromList [(moduleName m, findSignatures m) | m <- mods]

modExports :: MVar -> Program (Set.Set EVar)
modExports v = fmap (Map.findWithDefault Set.empty v) (gets stateModuleExports)

modDeclarations :: MVar -> Program (Map.Map EVar Expr)
modDeclarations v = fmap (Map.findWithDefault Map.empty v) (gets stateModuleDeclarations)

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

-- compile a declaration, for example:
--   foo x y = bar x (baz 42 y)
-- "foo" is the name of a lambda, not an application. "foo" can be exported
-- from the nexus as a subcommand the user can call with arguments x and y.
-- "bar" is the function that is called in the application "bar x (baz 42 y)".
module2manifolds
  :: Module
  -> Expr
  -> Program [Manifold]
module2manifolds m (Declaration ev@(EV v) (AnnE lambda@(LamE _ _) gentype)) = do
  i <- getId
  exported <- isExported (moduleName m) ev
  case (exported, uncurryExpr lambda) of
    (False, _) -> return []
    (_, (_, _, [])) -> error $ "nexus can only accept applications in declarations: "
                             <> show v <> " :: " <> show lambda
    -- FIXME: this only allows named function, not expressions like:
    --   foo x y = (bar x) 42 y  -- where bar returns a function
    --   foo x y = (x,y)         -- where there is no function on the right
    (_, (vars, (exprAsFunction -> fv@(EV mname)), es)) -> do
      typeset <- lookupVar fv m
      case typeset of
        (TypeSet Nothing _) -> error "no general type"
        (TypeSet (Just e) rs) -> do
          args <- CM.zipWithM (exprAsArgument vars m) [0..] es
          realizations <- toRealizations mname m (length args) rs
          return . (flip (:)) (concat . map snd $ args) $ Manifold
            { mid = i
            , mCallId = makeURI (moduleName m) i
            , mAbstractType = Just (etype2mtype (Just mname) (e {etype = gentype}))
            , mRealizations = realizations
            , mMorlocName = mname
            , mExported = exported
            , mCalled = False
            , mDefined = True
            , mComposition = Just v
            , mBoundVars = [v' | (EV v') <- vars]
            , mArgs = map fst args
            }
module2manifolds m (Signature ev@(EV v) t) = do
  i <- getId
  exported <- isExported (moduleName m) ev
  declared <- isDeclared (moduleName m) ev
  -- If v is exported but not declared, then it must refer directly to a source
  -- function (not a morloc lambda). Also, skip this signature if it is a
  -- realization signature.
  if exported && not declared && elang t == Nothing
  then do
    (TypeSet _ rs) <- lookupVar ev m
    let args = map ArgPosi (take (nargs (etype t)) [0..])
    realizations <- toRealizations v m (length args) rs
    return [Manifold
      { mid = i
      , mCallId = makeURI (moduleName m) i
      , mAbstractType = Just (etype2mtype (Just v) t)
      , mRealizations = realizations
      , mMorlocName = v
      , mExported = exported
      , mCalled = False
      , mDefined = True
      , mComposition = Nothing
      , mBoundVars = []
      , mArgs = args
      }]
  else return []
module2manifolds _ _ = return []

nargs :: Type -> Int
nargs (FunT t1 t2) = 1 + nargs t2
nargs _ = 0

exprAsArgument
  :: [EVar]
  -> Module
  -> Int -- ^ 0-indexed position of the argument
  -> Expr
  -> Program (Argument, [Manifold])
exprAsArgument bnd _ p (AnnE (VarE v@(EV v')) gentype)
  | elem v bnd = return (ArgName v', [])
  | otherwise = return (ArgNest v', [])
exprAsArgument bnd m _ (AnnE (AppE e1 e2) gentype) = case uncurryApplication e1 e2 of
  (f, es) -> do
    let v@(EV mname) = exprAsFunction f
    defined <- isDeclared (moduleName m) v
    i <- getId
    t <- lookupVar v m
    case t of
      (TypeSet Nothing _) -> error "ah fuck shit"
      (TypeSet (Just etyp) rs) -> do
        args <- CM.zipWithM (exprAsArgument bnd m) [0..] es
        realizations <- toRealizations mname m (length args) rs
        let newManifold = Manifold {
              mid = i
            , mCallId = makeURI (moduleName m) i
            , mAbstractType = Just (etype2mtype (Just mname) (etyp {etype = gentype}))
            , mRealizations = realizations
            , mMorlocName = mname -- TODO: really?
            , mExported = elem v (moduleExports m)
            , mCalled = True
            , mDefined = defined
            , mComposition = Nothing -- TODO: are you sure?
            , mBoundVars = [b | (EV b) <- bnd]
            , mArgs = map fst args
          }
        return (  ArgCall (makeURI (moduleName m) i)
                , newManifold : (concat . map snd $ args))
-- ArgData primitives
exprAsArgument _ _ _ (AnnE x@(NumE   _) _) = return (ArgData $ primitive2mdata x, [])
exprAsArgument _ _ _ (AnnE x@(LogE   _) _) = return (ArgData $ primitive2mdata x, [])
exprAsArgument _ _ _ (AnnE x@(StrE   _) _) = return (ArgData $ primitive2mdata x, [])
exprAsArgument _ _ _ (AnnE x@(ListE  _) _) = return (ArgData $ primitive2mdata x, [])
exprAsArgument _ _ _ (AnnE x@(TupleE _) _) = return (ArgData $ primitive2mdata x, [])
exprAsArgument _ _ _ (AnnE x@(RecE   _) _) = return (ArgData $ primitive2mdata x, [])
-- errors
exprAsArgument _ _ _ (AnnE (LamE _ _) (FunT _ _)) = error "lambdas not yet supported"
exprAsArgument _ _ _ _ = error "expected annotated expression"

makeURI :: MVar -> Integer -> URI
makeURI (MV v) i = URI $ v <> "_" <> MT.show' i

exprAsFunction :: Expr -> EVar
exprAsFunction (VarE v) = v
exprAsFunction (AnnE (VarE v) _) = v
exprAsFunction e = error $ "I'm sorry man, I can only handle VarE, not this: " <> show e 

-- TODO: allow anything inside a container
primitive2mdata :: Expr -> MData
primitive2mdata (AnnE t _) = primitive2mdata t
primitive2mdata (NumE x) = Num' (MT.show' x)
primitive2mdata (LogE x) = Log' x
primitive2mdata (StrE x) = Str' x
primitive2mdata (ListE es) = Lst' $ map primitive2mdata es
primitive2mdata (TupleE es) = Lst' $ map primitive2mdata es
primitive2mdata (RecE xs) = Rec' $ map entry xs where
  entry :: (EVar, Expr) -> (Name, MData)
  entry (EV v, e) = (v, primitive2mdata e)
primitive2mdata _ = error "complex stuff is not yet allowed in MData (coming soon)"

toRealizations :: Name -> Module -> Int -> [EType] -> Program [Realization]
toRealizations n m l [] = do
  src <- lookupSources (moduleName m) (EV n) 
  case src of
    Nothing -> error "Bad realization"
    (Just s) -> return [Realization
      { rLang = sourceLang s
      , rName = sourceName s
      , rConcreteType = Nothing
      , rModulePath = modulePath m
      , rSourcePath = sourcePath s
      , rSourced = True
      }]
toRealizations n m _ es = return $ map toRealization es where
  toRealization :: EType -> Realization
  toRealization e@(EType t (Just langText) _ _ (Just (f, EV srcname))) =
    case ML.readLangName langText of 
      (Just lang) -> Realization
        { rLang = lang
        , rName = srcname
        , rConcreteType = Just $ etype2mtype Nothing e
        , rModulePath = modulePath m
        , rSourcePath = f -- if Nothing, then $srcname is a builtin of $lang
        , rSourced = True
        }
      Nothing -> error "unrecognized language"
  toRealization _ = error "This is not a realization"

-- | uncurry one level of an expression, pulling out a tuple with
-- (<lambda-args>, <application-base>, <application-args>)
-- Examples:
-- @5@              ==> ([], 5, [])
-- @f a b@          ==> ([], f, [a,b])
-- @f a b = g 4 a b ==> ([a,b], g, [4,a,b])
-- @f x = g (h x) 5 ==> ([x], g, [(h x), 5]  -- no recursion into expressions
uncurryExpr :: Expr -> ([EVar], Expr, [Expr])
uncurryExpr (LamE v e) = (\(vs, e', es) -> (v:vs, e', es)) (uncurryExpr e)
uncurryExpr (AnnE (AppE e1 e2) _) = (\(e, es) -> ([], e, es)) (uncurryApplication e1 e2)
uncurryExpr (AppE e1 e2) = (\(e, es) -> ([], e, es)) (uncurryApplication e1 e2)
uncurryExpr e = ([], e, [])

-- | uncurry an application
-- Examples:
-- @f x@  ==> (f, [x])
-- @f x (g y)@  ==> (f, [x, (g y)])  -- no recursion
uncurryApplication
  :: Expr -- "base" of an application (e.g., @f@ in @f x@)
  -> Expr -- argument in an application (e.g., @x@ in @f x@)
  -> (Expr, [Expr])
uncurryApplication (AppE e1 e2) e0 = onSnd ((flip (++)) [e0]) (uncurryApplication e1 e2)
uncurryApplication (AnnE (AppE e1 e2) _) e0 = onSnd ((flip (++)) [e0]) (uncurryApplication e1 e2)
uncurryApplication f en = (f, [en])

onSnd :: (b -> c) -> (a, b) -> (a, c)
onSnd f (x, y) = (x, f y)

onFst :: (a -> c) -> (a, b) -> (c, b)
onFst f (x, y) = (f x, y)

makeSource :: Maybe Lang -> Maybe Path -> [(EVar, EVar)] -> Map.Map EVar Source'
makeSource (Just l) f xs = Map.fromList $ map (\(EV n, EV a) -> (EV a, Source' f l n a)) xs
makeSource Nothing _ _ = error "unsupported language"

rootModule :: [Module] -> MorlocMonad Module
rootModule ms = case roots of
  [root] -> return root
  [] -> MM.throwError . GeneratorError $ "cyclic dependency"
  _ -> MM.throwError . GeneratorError $ "expected a unique root"
  where
    mset = Set.fromList . concat $ [[importModuleName x | x <- moduleImports m] | m <- ms]
    roots = filter (\m -> not $ Set.member (moduleName m) mset) ms

makeSerialMaps :: [Module] -> MorlocMonad (Map.Map Lang SerialMap)
makeSerialMaps (concat . map moduleBody -> es)
  = fmap (
      Map.mapWithKey toSerialMap -- Map Lang SerialMap
    . Map.fromList               -- Map Lang [Expr]
    . DLE.groupSort              -- [(Lang, [Expr])]
    . DM.catMaybes
  ) $ mapM f es  -- MorloMonad [Maybe (Lang, Expr)]
  where
    -- collect all sources and signatures paired with language
    -- needed for @toSerialMap@
    -- convert textual language names to Lang's
    f :: Expr -> MorlocMonad (Maybe (Lang, Expr))
    f e@(SrcE l _ _) = case ML.readLangName l of
      (Just lang) -> return $ Just (lang, e)
      _ -> MM.throwError . GeneratorError $ "unrecognized language: " <> l
    f e@(Signature _ (elang -> Just l)) = case ML.readLangName l of
      (Just lang) -> return $ Just (lang, e)
      _ -> MM.throwError . GeneratorError $ "unrecognized language: " <> l
    f _ = return $ Nothing

    toSerialMap :: Lang -> [Expr] -> SerialMap
    toSerialMap lang es = SerialMap {
        serialLang = lang
      , serialPacker = getSerial Pack es
      , serialUnpacker = getSerial Unpack es
      , serialSources = DL.nub [p | (SrcE _ (Just p) _) <- es]
      }

getSerial :: Property -> [Expr] -> Map.Map MType Name
getSerial p es = Map.fromList [(f (etype2mtype Nothing e), n)
                              | (Signature (EV n) e) <- es
                              , Set.member p (eprop e)]
  where
    f (MFuncType _ inputs output) = case (p, inputs) of 
      (Pack, [x]) -> x
      (Unpack, [_]) -> output
      _ -> error "too many args in serial function"
    f _ = error "wrong type for serial function"

etype2mtype :: Maybe Name -> EType -> MType
etype2mtype n e = type2mtype Set.empty (etype e) where

  meta = MTypeMeta {
      metaName = n
    , metaProp = map prop2text (Set.toList (eprop e))
    , metaLang = elang e >>= ML.readLangName
  }

  metaEmpty = MTypeMeta {
      metaName = Nothing
    , metaProp = []
    , metaLang = elang e >>= ML.readLangName
  }

  prop2text Pack = ["pack"]
  prop2text Unpack = ["unpack"]
  prop2text Cast = ["cast"]
  prop2text (GeneralProperty ts) = ts

  type2mtype :: Set.Set Name -> Type -> MType
  type2mtype bnds (VarT (TV v))
    | Set.member v bnds = MAbstType meta v []
    | otherwise = MConcType meta v []
  type2mtype _ (ExistT _) = error "found existential type"
  type2mtype bnds (Forall (TV v) t) = (type2mtype (Set.insert v bnds) t)
  type2mtype bnds (FunT t1 t2) =
    let ts = type2mtype bnds t1 : functionTypes bnds t2
    in MFuncType meta (init ts) (last ts)
  type2mtype bnds (ArrT (TV v) ts)
    | Set.member v bnds = error $ "currently I can't use bound variables in ArrT"
    | otherwise = MAbstType meta v (map (type2mtype bnds) ts)
  type2mtype bnds (RecT fs) = MConcType meta "Record" (map (recordEntry bnds) fs)
  type2mtype _ t = error $ "cannot cast type: " <> show t

  recordEntry :: Set.Set Name -> (TVar, Type) -> MType
  recordEntry bnds (TV v, t)
    = MConcType metaEmpty "RecordEntry" [MConcType metaEmpty "Str" [], type2mtype bnds t]

  functionTypes :: Set.Set Name -> Type -> [MType]
  functionTypes bnds (FunT t1 t2) = type2mtype bnds t1 : functionTypes bnds t2
  functionTypes bnds t = [type2mtype bnds t]

-- | Lookup a variable in a given module. If collect any type information
-- (including realizations). Whether the variable is found or not, recurse into
-- all imported modules searching through exported variables for additional
-- signatures describing the variable. Collect all information in the returned
-- TypeSet object. Die if there is disagreement about the basic general type.
lookupVar
  :: EVar
  -> Module
  -> Program TypeSet
lookupVar v0 m0 = do
  mm <- gets stateModuleMap
  tm <- gets stateModuleTypeSetMap
  return $ lookupVar' mm tm v0 m0
  where
    lookupVar' mm tm v m =
      case Map.lookup (moduleName m) tm >>= Map.lookup v of
        Nothing -> foldr (joinTypeSet const) (TypeSet Nothing []) (r mm tm v m)
        (Just t) -> foldr (joinTypeSet const) t (r mm tm v m)

    r mm tm v m
      = [maybe (TypeSet Nothing []) id (fmap (lookupVar' mm tm v') $ Map.lookup mv' mm)
        | (mv', v', alias') <- concat [getImports mm i | i <- (moduleImports m)]
        , v == alias'
        ]

getImports :: Map.Map MVar Module -> Import -> [(MVar, EVar, EVar)]
getImports _ (Import mv (Just xs) _ _) = [(mv, v, a) | (v,a) <- xs]
getImports mm (Import mv Nothing _ _) = do
  case Map.lookup mv mm of
    Nothing -> error "Cannot find module"
    (Just m') -> [(mv, v, v) | v <- moduleExports m']

-- | collect all type information within a module
-- first all signatures are collected, storing each realization
-- then the
findSignatures :: Module -> Map.Map EVar TypeSet
findSignatures (moduleBody -> es)
  = foldr insertAppendEtype (
        Map.map (foldr appendTypeSet (TypeSet Nothing []))
      . Map.fromList
      . DLE.groupSort
      $ [(v, t) | (Signature v t) <- es]
    ) [(v, type2etype t) | (AnnE (Declaration v _) t) <- es]

type2etype :: Type -> EType
type2etype t = EType
  { etype = t
  , elang = Nothing
  , eprop = Set.empty
  , econs = Set.empty
  , esource = Nothing
  }

insertAppendEtype :: (EVar, EType) -> Map.Map EVar TypeSet -> Map.Map EVar TypeSet
insertAppendEtype (v, t) = Map.insertWith (joinTypeSet f) v (TypeSet (Just t) []) where
  f edec eold = eold { etype = etype edec }

appendTypeSet :: EType -> TypeSet -> TypeSet
appendTypeSet e@(elang -> Nothing) (TypeSet _ es) = TypeSet (Just e) es
appendTypeSet e (TypeSet x es) = TypeSet x (e:es)

joinTypeSet :: (EType -> EType -> EType) -> TypeSet -> TypeSet -> TypeSet
joinTypeSet f (TypeSet g1 es1) (TypeSet g2 es2)
  = foldr appendTypeSet (TypeSet (xor f g1 g2) es1) es2

xor :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
xor _ (Just x) Nothing = Just x
xor _ Nothing (Just x) = Just x
xor f (Just x) (Just y) = Just (f x y)
xor _ _ _ = Nothing
