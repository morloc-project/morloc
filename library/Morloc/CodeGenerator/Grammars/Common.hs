{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.CodeGenerator.Grammars.Common
Description : Shared codegen utilities: manifold inversion, naming, pool doc merging
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Provides 'invertSerialManifold' (the preprocessing step that all translators
run), 'PoolDocs' (the accumulator for the lowering fold), naming convention
helpers, and the fold framework ('FoldRules', 'foldWithSerialManifoldM').
-}
module Morloc.CodeGenerator.Grammars.Common
  ( invertSerialManifold
  , PoolDocs (..)
  , mergePoolDocs

    -- * Naming conventions
  , svarNamer
  , nvarNamer
  , helperNamer
  , argNamer
  , manNamer
  , patternSetter

    -- * Record collection/unification
  , RecEntry (..)
  , RecMap
  , collectRecords
  , unifyRecords
  , structName

    -- * Dispatch extraction
  , DispatchEntry (..)
  , extractLocalDispatch
  , extractRemoteDispatch

    -- * Utilities
  , provideClosure
  , makeManifoldIndexer
  , renderPoolDocs
  ) where

import qualified Control.Monad.State as CMS
import Data.Binary (Binary)
import GHC.Generics (Generic)
import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.Serial (serialAstToType)
import Morloc.Data.Doc
import Morloc.Data.Text (Text)
import Morloc.Monad (Identity, Index, newIndex, runIdentity, runIndex)

-- Stores pieces of code made while building a pool
data PoolDocs = PoolDocs
  { poolCompleteManifolds :: [MDoc]
  -- ^ completely generated manifolds
  , poolExpr :: MDoc
  -- ^ the inplace expression
  , poolPriorLines :: [MDoc]
  -- ^ lines to precede the returned expression
  , poolPriorExprs :: [MDoc]
  -- ^ expressions that should precede this manifold, may include helper
  -- functions or imports
  }

instance Defaultable PoolDocs where
  defaultValue =
    PoolDocs
      { poolCompleteManifolds = []
      , poolExpr = ""
      , poolPriorLines = []
      , poolPriorExprs = []
      }

{- | Merge a series of pools, keeping prior lines, expression and manifolds, but
merging bodies with a function. For example, merge all elements in a list and
process the poolExpr variables into list syntax in the given language.
-}
mergePoolDocs :: ([MDoc] -> MDoc) -> [PoolDocs] -> PoolDocs
mergePoolDocs f ms =
  PoolDocs
    { poolCompleteManifolds = concatMap poolCompleteManifolds ms
    , poolExpr = f (map poolExpr ms)
    , poolPriorLines = concatMap poolPriorLines ms
    , poolPriorExprs = concatMap poolPriorExprs ms
    }

provideClosure :: Source -> [MDoc] -> [[MDoc]]
provideClosure src args0 = f (srcRsize src) args0
  where
    f [] args = [args]
    f (n : ns) args
      | n < length args = take n args : f ns (drop n args)
      | otherwise =
          error $
            "Invalid rsize value for imported "
              <> show (srcLang src)
              <> " function "
              <> show (unEVar (srcAlias src))

svarNamer :: Int -> MDoc
svarNamer i = "s" <> viaShow i

nvarNamer :: Int -> MDoc
nvarNamer i = "n" <> viaShow i

helperNamer :: Int -> MDoc
helperNamer i = "helper" <> viaShow i

argNamer :: (HasTypeM t) => Arg t -> MDoc
argNamer (Arg i (typeMof -> Native _)) = nvarNamer i
argNamer (Arg i (typeMof -> Function _ _)) = nvarNamer i
argNamer (Arg i _) = svarNamer i

-- create a name for a manifold based on a unique id
manNamer :: Int -> MDoc
manNamer i = "m" <> viaShow i

renderPoolDocs :: PoolDocs -> MDoc
renderPoolDocs e = vsep . punctuate line $ poolPriorExprs e <> poolCompleteManifolds e

-- The surround rules control the setting of manifold ids across the recursion
makeManifoldIndexer :: (Monad m) => m Int -> (Int -> m ()) -> SurroundManifoldM m sm nm se ne sr nr
makeManifoldIndexer getId putId =
  defaultValue
    { surroundSerialManifoldM = surroundSM
    , surroundNativeManifoldM = surroundNM
    }
  where
    -- \| Run a computation in a child manifold, manage manifold indices
    descend childManifoldIndex x f = do
      originalManifoldIndex <- getId
      putId childManifoldIndex
      x' <- f x
      putId originalManifoldIndex
      return x'

    surroundSM f sm@(SerialManifold i _ _ _ _) = descend i sm f

    surroundNM f nm@(NativeManifold i _ _ _) = descend i nm f

patternSetter ::
  (TypeF -> [MDoc] -> MDoc) -> -- make a tuple from a type and list of elements
  (TypeF -> [MDoc] -> MDoc) -> -- make a record from a type and list of elements
  (TypeF -> MDoc -> Int -> MDoc) -> -- access an element in a tuple
  (TypeF -> MDoc -> Text -> MDoc) -> -- access an element in a record
  MDoc -> -- initial data variable name
  TypeF -> -- data type
  Selector -> -- selection pattern
  [MDoc] -> -- ordered arguments substituted at set sites
  MDoc -- the returned data structure with a new spine that reuses unchanged fields
patternSetter makeTuple makeRecord accessTuple accessRecord dat0 t0 s0 args0 =
  snd (setter dat0 t0 s0 args0)
  where
    setter :: MDoc -> TypeF -> Selector -> [MDoc] -> ([MDoc], MDoc)

    -- tuple setters
    setter dat1 tupleType@(AppF _ ts1) (SelectorIdx s1 ss1) args1 =
      second (makeTuple tupleType) $ statefulMap (chooseField dat1 (s1 : ss1)) args1 (zip [0 ..] ts1)
      where
        chooseField :: MDoc -> [(Int, Selector)] -> [MDoc] -> (Int, TypeF) -> ([MDoc], MDoc)
        chooseField dat ss args (i, t) =
          let dat' = accessTuple tupleType dat i
           in case (lookup i ss) of
                (Just s) -> setter dat' t s args
                Nothing -> (args, dat')

    -- record setters
    setter dat1 recType@(NamF _ _ _ rs1) (SelectorKey s1 ss1) args1 =
      second (makeRecord recType) $ statefulMap (chooseField dat1 (s1 : ss1)) args1 rs1
      where
        chooseField :: MDoc -> [(Text, Selector)] -> [MDoc] -> (Key, TypeF) -> ([MDoc], MDoc)
        chooseField dat ss args (Key k, t) =
          let dat' = accessRecord recType dat k
           in case (lookup k ss) of
                (Just s) -> setter dat' t s args
                Nothing -> (args, dat')
    setter _ _ _ (arg : args2) = (args2, arg)
    setter _ _ _ [] = error "Illegal setter"

-- Represents the dependency of a on previously bound expressions
data D a = D a [(Int, Either SerialExpr NativeExpr)]

unD :: D a -> a
unD (D a _) = a

getDeps :: D a -> [(Int, Either SerialExpr NativeExpr)]
getDeps (D _ d) = d

class Dependable a where
  weave :: D a -> a
  atomize :: a -> [(Int, Either SerialExpr NativeExpr)] -> Index (D a)
  isAtomic :: a -> Bool

instance Dependable NativeExpr where
  weave (D x ((i, Left se) : deps)) = weave $ D (SerialLetN i se x) deps
  weave (D x ((i, Right ne) : deps)) = weave $ D (NativeLetN i ne x) deps
  weave (D x []) = x

  atomize e deps
    | isAtomic e = return $ D e deps
    | otherwise = do
        i <- newIndex
        return $ D (LetVarN (typeFof e) i) ((i, Right e) : deps)

  isAtomic (AppExeN _ _ _ _) = False
  isAtomic ManN {} = False
  isAtomic SerialLetN {} = False
  isAtomic NativeLetN {} = False
  isAtomic ListN {} = False
  isAtomic TupleN {} = False
  isAtomic RecordN {} = False
  isAtomic _ = True

instance Dependable SerialExpr where
  weave (D x ((i, Left se) : deps)) = weave $ D (SerialLetS i se x) deps
  weave (D x ((i, Right ne) : deps)) = weave $ D (NativeLetS i ne x) deps
  weave (D x []) = x

  atomize e deps
    | isAtomic e = return $ D e deps
    | otherwise = do
        i <- newIndex
        t <- case typeMof e of
          Passthrough -> return Nothing
          (Serial ft) -> return $ Just ft
          _ -> return Nothing
        -- _ -> error "This type must be serialized"
        return $ D (LetVarS t i) ((i, Left e) : deps)

  isAtomic (LetVarS _ _) = True
  isAtomic (BndVarS _ _) = True
  isAtomic (ReturnS _) = True
  isAtomic (SerializeS _ _) = True
  isAtomic _ = False

invertSerialManifold :: SerialManifold -> SerialManifold
invertSerialManifold sm0 =
  runIndex (maxIndex sm0) (unD <$> foldSerialManifoldM fm sm0)
  where
    fm =
      FoldManifoldM
        { opSerialManifoldM = invertSerialManifoldM
        , opNativeManifoldM = invertNativeManifoldM
        , opSerialExprM = invertSerialExprM
        , opNativeExprM = invertNativeExprM
        , opSerialArgM = invertSerialArgM
        , opNativeArgM = invertNativeArgM
        }

    invertSerialManifoldM :: SerialManifold_ (D SerialExpr) -> Index (D SerialManifold)
    invertSerialManifoldM (SerialManifold_ m lang form headForm se) = do
      return (D (SerialManifold m lang form headForm (weave se)) [])

    invertNativeManifoldM :: NativeManifold_ (D NativeExpr) -> Index (D NativeManifold)
    invertNativeManifoldM (NativeManifold_ m lang form (weave -> ne)) = do
      return (D (NativeManifold m lang form ne) [])

    invertSerialExprM ::
      SerialExpr_ (D SerialManifold) (D SerialExpr) (D NativeExpr) (D SerialArg) (D NativeArg) ->
      Index (D SerialExpr)
    invertSerialExprM (ManS_ (D sm lets)) = return $ D (ManS sm) lets
    invertSerialExprM (AppPoolS_ t pool serialArgs) = do
      let serialArgs' = map unD serialArgs
          deps = concatMap getDeps serialArgs
      atomize (AppPoolS t pool serialArgs') deps
    invertSerialExprM (ReturnS_ (D se lets)) = return $ D (ReturnS se) lets
    invertSerialExprM (SerialLetS_ i (D se1 lets1) (D se2 lets2)) =
      return $ D se2 (lets2 <> ((i, Left se1) : lets1))
    invertSerialExprM (NativeLetS_ i (D ne1 lets1) (D se2 lets2)) =
      return $ D se2 (lets2 <> ((i, Right ne1) : lets1))
    invertSerialExprM (LetVarS_ t i) = atomize (LetVarS t i) []
    invertSerialExprM (BndVarS_ t i) = atomize (BndVarS t i) []
    invertSerialExprM (SerializeS_ s (D ne lets)) = atomize (SerializeS s ne) lets

    invertNativeExprM ::
      NativeExpr_ (D NativeManifold) (D SerialExpr) (D NativeExpr) (D SerialArg) (D NativeArg) ->
      Index (D NativeExpr)
    invertNativeExprM (AppExeN_ t exe qs nativeArgs) = do
      let nativeArgs' = map unD nativeArgs
          deps = concatMap getDeps nativeArgs
      case (t, exe) of
        -- Source functions return the unwrapped type; the compiler wraps in suspend
        (ThunkF innerT, SrcCallP _) ->
          return $ D (SuspendN t (weave (D (AppExeN innerT exe qs nativeArgs') deps))) []
        _ -> atomize (AppExeN t exe qs nativeArgs') deps
    invertNativeExprM (ManN_ (D nm lets)) = atomize (ManN nm) lets
    invertNativeExprM (ReturnN_ (D ne lets)) = atomize (ReturnN ne) lets
    invertNativeExprM (SerialLetN_ i (D se1 lets1) (D ne2 lets2)) =
      return $ D ne2 (lets2 <> ((i, Left se1) : lets1))
    invertNativeExprM (NativeLetN_ i (D ne1 lets1) (D ne2 lets2)) =
      return $ D ne2 (lets2 <> ((i, Right ne1) : lets1))
    invertNativeExprM (LetVarN_ t i) = atomize (LetVarN t i) []
    invertNativeExprM (BndVarN_ t i) = atomize (BndVarN t i) []
    invertNativeExprM (DeserializeN_ t s (D se lets)) = atomize (DeserializeN t s se) lets
    invertNativeExprM (ExeN_ t x) = atomize (ExeN t x) []
    invertNativeExprM (ListN_ v t nes) = atomize (ListN v t (map unD nes)) (concatMap getDeps nes)
    invertNativeExprM (TupleN_ v xs) = atomize (TupleN v (map unD xs)) (concatMap getDeps xs)
    invertNativeExprM (RecordN_ o v ps rs) = atomize (RecordN o v ps (map (second unD) rs)) (concatMap (getDeps . snd) rs)
    invertNativeExprM (LogN_ v x) = atomize (LogN v x) []
    invertNativeExprM (RealN_ v x) = atomize (RealN v x) []
    invertNativeExprM (IntN_ v x) = atomize (IntN v x) []
    invertNativeExprM (StrN_ v x) = atomize (StrN v x) []
    invertNativeExprM (NullN_ v) = atomize (NullN v) []
    -- keep dependencies inside suspend so thunk body stays lazy
    invertNativeExprM (SuspendN_ t (D ne lets)) = return $ D (SuspendN t (weave (D ne lets))) []
    invertNativeExprM (ForceN_ t (D ne lets)) = atomize (ForceN t ne) lets

    invertSerialArgM :: SerialArg_ (D SerialManifold) (D SerialExpr) -> Index (D SerialArg)
    invertSerialArgM (SerialArgManifold_ (D sm deps)) = return $ D (SerialArgManifold sm) deps
    invertSerialArgM (SerialArgExpr_ (D se deps)) = return $ D (SerialArgExpr se) deps

    invertNativeArgM :: NativeArg_ (D NativeManifold) (D NativeExpr) -> Index (D NativeArg)
    invertNativeArgM (NativeArgManifold_ (D nm deps)) = return $ D (NativeArgManifold nm) deps
    invertNativeArgM (NativeArgExpr_ (D ne deps)) = return $ D (NativeArgExpr ne) deps

maxIndex :: SerialManifold -> Int
maxIndex = (+ 1) . runIdentity . foldSerialManifoldM fm
  where
    fm =
      FoldManifoldM
        { opSerialManifoldM = findSerialManifoldIndices
        , opNativeManifoldM = findNativeManifoldIndices
        , opSerialExprM = findSerialIndices
        , opNativeExprM = findNativeIndices
        , opSerialArgM = return . foldlSA max 0
        , opNativeArgM = return . foldlNA max 0
        }

    findSerialManifoldIndices :: (Monad m) => SerialManifold_ Int -> m Int
    findSerialManifoldIndices (SerialManifold_ _ _ form _ bodyMax) = do
      let formIndices = abilist const const form
      return $ foldl max bodyMax formIndices

    findNativeManifoldIndices :: (Monad m) => NativeManifold_ Int -> m Int
    findNativeManifoldIndices (NativeManifold_ _ _ form bodyMax) = do
      let formIndices = abilist const const form
      return $ foldl max bodyMax formIndices

    findSerialIndices :: (Monad m) => SerialExpr_ Int Int Int Int Int -> m Int
    findSerialIndices (LetVarS_ _ i) = return i
    findSerialIndices (BndVarS_ _ i) = return i
    findSerialIndices e = return $ foldlSE max 0 e

    findNativeIndices :: (Monad m) => NativeExpr_ Int Int Int Int Int -> m Int
    findNativeIndices (LetVarN_ _ i) = return i
    findNativeIndices (BndVarN_ _ i) = return i
    findNativeIndices e = return $ foldlNE max 0 e

-- | A record entry stores the common name, keys, and types of records that are
-- not imported from source. These records are generated as structs (or
-- equivalent) in the pool. 'unifyRecords' takes all such records and "unifies"
-- ones with the same name and keys. The unified records may have different
-- types, but they will all be instances of the same generic struct. Fields that
-- differ between instances are made generic.
data RecEntry = RecEntry
  { recName :: MDoc
  , recFields ::
      [ ( Key
        , Maybe TypeF
        )
      ]
  }
  deriving (Show)

-- | Lookup table mapping (FVar, keys) to their unified RecEntry.
type RecMap = [((FVar, [Key]), RecEntry)]

collectRecords :: SerialManifold -> [(FVar, Int, [(Key, TypeF)])]
collectRecords e0@(SerialManifold i0 _ _ _ _) =
  unique $ CMS.evalState (surroundFoldSerialManifoldM manifoldIndexer fm e0) i0
  where
    fm = defaultValue {opFoldWithNativeExprM = nativeExpr, opFoldWithSerialExprM = serialExpr}

    manifoldIndexer = makeManifoldIndexer CMS.get CMS.put

    nativeExpr _ (DeserializeN_ t s xs) = do
      manifoldIndex <- CMS.get
      let tRecs = seekRecs manifoldIndex t
          sRecs = seekRecs manifoldIndex (serialAstToType s)
      return $ xs <> tRecs <> sRecs
    nativeExpr efull e = do
      manifoldIndex <- CMS.get
      let newRecs = seekRecs manifoldIndex (typeFof efull)
      return $ foldlNE (<>) newRecs e

    serialExpr _ (SerializeS_ s xs) = do
      manifoldIndex <- CMS.get
      return $ seekRecs manifoldIndex (serialAstToType s) <> xs
    serialExpr _ e = return $ foldlSE (<>) [] e

    seekRecs :: Int -> TypeF -> [(FVar, Int, [(Key, TypeF)])]
    seekRecs m (NamF _ v@(FV _ (CV "struct")) _ rs) = [(v, m, rs)] <> concatMap (seekRecs m . snd) rs
    seekRecs m (NamF _ _ _ rs) = concatMap (seekRecs m . snd) rs
    seekRecs m (FunF ts t) = concatMap (seekRecs m) (t : ts)
    seekRecs m (AppF t ts) = concatMap (seekRecs m) (t : ts)
    seekRecs _ (UnkF _) = []
    seekRecs _ (VarF _) = []
    seekRecs m (ThunkF t) = seekRecs m t

unifyRecords ::
  [ ( FVar
    , Int
    , [(Key, TypeF)]
    )
  ] ->
  RecMap
unifyRecords xs =
  zipWith (\i ((v, ks), es) -> ((v, ks), RecEntry (structName i v) es)) [1 ..]
    . map (\((v, ks), rss) -> ((v, ks), map unifyField (transpose (map snd rss))))
    . groupSort
    . unique
    $ [((v, map fst es), (m, es)) | (v, m, es) <- xs]

structName :: Int -> FVar -> MDoc
structName i (FV v (CV "struct")) = "mlc_" <> pretty v <> "_" <> pretty i
structName _ (FV _ v) = pretty v

unifyField :: [(Key, TypeF)] -> (Key, Maybe TypeF)
unifyField [] = error "Empty field"
unifyField rs@((v, _) : _)
  | not (all ((== v) . fst) rs) =
      error $ "Bad record - unequal fields: " <> show (unique rs)
  | otherwise = case unique (map snd rs) of
      [t] -> (v, Just t)
      _ -> (v, Nothing)

-- | A dispatch table entry: manifold ID and argument count.
data DispatchEntry = DispatchEntry
  { dispatchId :: Int
  , dispatchArgCount :: Int
  }
  deriving (Show, Eq, Ord, Generic)

instance Binary DispatchEntry

-- | Extract local dispatch entries from serial manifolds.
-- Skips manifolds marked as remote workers.
extractLocalDispatch :: [SerialManifold] -> [DispatchEntry]
extractLocalDispatch = catMaybes . map localEntry
  where
    localEntry (SerialManifold _ _ _ HeadManifoldFormRemoteWorker _) = Nothing
    localEntry (SerialManifold i _ form _ _) = Just $ DispatchEntry i (getSize form)

    getSize :: ManifoldForm (Or TypeS TypeF) TypeS -> Int
    getSize = sum . abilist (\_ _ -> 1) (\_ _ -> 1)

-- | Extract remote dispatch entries by walking the AST.
extractRemoteDispatch :: [SerialManifold] -> [DispatchEntry]
extractRemoteDispatch = map (uncurry DispatchEntry) . unique . concatMap getRemotes
  where
    getRemotes :: SerialManifold -> [(Int, Int)]
    getRemotes = runIdentity . foldSerialManifoldM (defaultValue {opSerialExprM = getRemoteSE})

    getRemoteSE ::
      SerialExpr_ [(Int, Int)] [(Int, Int)] [(Int, Int)] [(Int, Int)] [(Int, Int)] ->
      Identity [(Int, Int)]
    getRemoteSE (AppPoolS_ _ (PoolCall i _ (RemoteCall _) _) xss) = return $ (i, length xss) : concat xss
    getRemoteSE x = return $ foldlSE mappend mempty x

