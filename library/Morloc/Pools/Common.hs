{-|
Module      : Morloc.Pools.Common
Description : A common set of utility functions for language templates
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.Pools.Common
  ( Grammar(..)
  , TryDoc(..)
  , GeneralFunction(..)
  , GeneralAssignment(..)
  , UnpackerDoc(..)
  , ForeignCallDoc(..)
  , PoolMain(..)
  , defaultCodeGenerator
  ) where

import qualified Data.List as DL
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as DM
import qualified Morloc.Config as MC
import Morloc.Data.Doc
import qualified Morloc.Data.Text as MT
import qualified Morloc.Language as ML
import qualified Morloc.Manifold as Man
import qualified Morloc.Monad as MM
import Morloc.Namespace
import Morloc.Pretty () -- just for mshow instances
import qualified Morloc.System as MS

data Grammar =
  Grammar
    { gLang :: Lang
    , gSerialType :: MType
    , gAssign :: GeneralAssignment -> MDoc
    , gCall :: MDoc -> [MDoc] -> MDoc
    , gFunction :: GeneralFunction -> MDoc
    , gSignature :: GeneralFunction -> MDoc
    , gId2Function :: Integer -> MDoc
    , gComment :: MDoc -> MDoc
    , gReturn :: MDoc -> MDoc
    , gQuote :: MDoc -> MDoc
    , gImport :: MDoc -> MDoc -> MDoc
    ---------------------------------------------
    , gList :: [MDoc] -> MDoc
    , gTuple :: [MDoc] -> MDoc
    , gRecord :: [(MDoc, MDoc)] -> MDoc
    ---------------------------------------------
    , gTrue :: MDoc
    , gFalse :: MDoc
    , gIndent :: MDoc -> MDoc
    , gUnpacker :: UnpackerDoc -> MDoc
    , gTry :: TryDoc -> MDoc
    , gForeignCall :: ForeignCallDoc -> MDoc
    , gSwitch :: (Manifold -> MDoc) -> (Manifold -> MDoc) -> [Manifold] -> MDoc -> MDoc -> MDoc
    , gCmdArgs :: [MDoc] -- ^ infinite list of main arguments (e.g. "argv[2]")
    , gShowType :: MType -> MDoc
    , gMain :: PoolMain -> MorlocMonad MDoc
    }

data GeneralAssignment =
  GeneralAssignment
    { gaType :: Maybe MDoc
    , gaName :: MDoc
    , gaValue :: MDoc
    , gaArg :: Maybe Argument
    }
  deriving (Show)

data GeneralFunction =
  GeneralFunction
    { gfComments :: MDoc
    , gfReturnType :: Maybe MDoc -- ^ concrete return type
    , gfName :: MDoc -- ^ function name
    , gfArgs :: [(Maybe MDoc, MDoc)] -- ^ (variable concrete type, variable name)
    , gfBody :: MDoc
    }
  deriving (Show)

data TryDoc =
  TryDoc
    { tryCmd :: MDoc -- ^ The function we attempt to run
    , tryRet :: MDoc -- ^ A name for the returned variable?
    , tryArgs :: [MDoc] -- ^ Arguments passed to function
    , tryMid :: MDoc -- ^ The name of the calling manifold (for debugging)
    , tryFile :: MDoc -- ^ The file where the issue occurs (for debugging)
    }
  deriving (Show)

data UnpackerDoc =
  UnpackerDoc
    { udValue :: MDoc -- ^ The expression that will be unpacked
    , udUnpacker :: MDoc -- ^ The function for unpacking the value
    , udMid :: MDoc -- ^ Manifold name for debugging messages
    , udFile :: MDoc -- ^ File name for debugging messages
    }
  deriving (Show)

data ForeignCallDoc =
  ForeignCallDoc
    { fcdForeignPool :: MDoc -- ^ the name of the foreign pool (e.g., "R.pool")
    , fcdForeignExe :: MDoc -- ^ path to the foreign executable
    , fcdMid :: MDoc -- ^ the function integer identifier
    , fcdArgs :: [MDoc] -- ^ CLI arguments passed to foreign function
    , fcdCall :: [MDoc] -- ^ make a list of CLI arguments from first two
                            -- inputs -- since fcdArgs will likely be
                            -- variables, they are not included in this call.
    , fcdFile :: MDoc -- ^ for debugging
    }
  deriving (Show)

data PoolMain =
  PoolMain
    { pmSources :: [MDoc]
    , pmPoolManifolds :: [GeneralFunction]
    , pmDispatchManifold :: MDoc -> MDoc -> MDoc
    }

defaultCodeGenerator ::
     Grammar
  -> (MT.Text -> MorlocMonad MDoc) -- source name parser
  -> [Manifold]
  -> SerialMap
  -> MorlocMonad Script
defaultCodeGenerator g f manifolds packMap = do
  state <- MM.get
  paksrcs <- mapM f (serialSources packMap)
  mansrcs <- Man.getManSrcs (gLang g) f manifolds
  lib <- MM.asks MC.configLibrary
  poolManifolds <-
    makePoolManifolds g packMap (getMorlocFun2sourceFun manifolds) manifolds
  let srcs = map ((gImport g) (pretty lib)) (paksrcs ++ mansrcs)
      dispatchManifold =
        (gSwitch g)
          (pretty . MT.show' . mid)
          (mainCall g packMap)
          (Man.getUsedManifolds (gLang g) manifolds)
  doc <-
    (gMain g)
      (PoolMain
         { pmSources = srcs
         , pmPoolManifolds = poolManifolds
         , pmDispatchManifold = dispatchManifold
         })
  return $
    Script
      { scriptBase = "pool"
      , scriptLang = gLang g
      , scriptCode = render doc
      , scriptCompilerFlags =
          filter (/= "") . map packageGccFlags $ statePackageMeta state
      , scriptInclude = DL.nub . map MS.takeDirectory $ (serialSources packMap)
      }

-- call a manifold and deserialize the return value
mainCall :: Grammar -> SerialMap -> Manifold -> MDoc
mainCall g h m =
  (gCall g) (Man.getPacker h m) $
  [(gCall g) (callIdToName g m) (take n (gCmdArgs g))]
  where
    n =
      if mSourced m && not (mCalled m)
        then length (mArgs m)
        else length (mBoundVars m)

-- | When a morloc function is declared, for example @foo x = sqrt(pow x 2)@,
-- then calling @foo@ from the nexus should call the source function @sqrt@ not
-- the morloc function @foo@ (since the morloc function does not really exist).
-- The @getMorlocFun2sourceFun@ function maps from morloc names (@foo@) to the
-- top-level source name (@sqrt@).
getMorlocFun2sourceFun :: [Manifold] -> Map.Map Name Manifold
getMorlocFun2sourceFun ms =
  Map.fromList [(n, m) | (Just n, m) <- (map (\m -> (mComposition m, m)) ms)]

callIdToName :: Grammar -> Manifold -> MDoc
callIdToName g m = pretty . MT.show' $ (gId2Function g) (mid m)

-- | inifinite list of named variables
iArgs :: MDoc -> [MDoc]
iArgs prefix = zipWith (<>) (repeat prefix) (map pretty ([0 ..] :: [Int]))

makePoolManifolds ::
     Grammar
  -> SerialMap
  -> Map.Map Name Manifold
  -> [Manifold]
  -> MorlocMonad [GeneralFunction]
makePoolManifolds g h cs ms = fmap DM.catMaybes . mapM makePoolManifold $ ms
  where
    makePoolManifold m =
      case Man.determineManifoldClass (gLang g) m of
        Cis -> Just <$> makeCisManifold g h cs ms m
        Source -> Just <$> makeSourceManifold g h m
        _ -> return Nothing

makeCisManifold ::
     Grammar
  -> SerialMap
  -> Map.Map MT.Text Manifold
  -> [Manifold]
  -> Manifold
  -> MorlocMonad GeneralFunction
makeCisManifold g h cs ms m = do
  argTypes <-  zip3
           <$> pure (getConcreteArgTypes g m) -- [Maybe MDoc]
           <*> packersIfNeeded                -- Maybe [MDoc]
           <*> pure (mArgs m)                 -- [Argument]
  args <- sequence $ zipWith makeArg (iArgs "a") argTypes
  let name = callIdToName g m
      comments
        =  (gComment g) "cis manifold" <> line
        <> (gComment g) (maybe (fname m) pretty (mComposition m)) <> line
        <> (gComment g) (fname m <> " :: " <> maybe "undefined" pretty (mAbstractType m))
        <> line
  return $
    GeneralFunction
      { gfComments = comments
      , gfReturnType = getConcreteReturnType g m
      , gfName = name
      , gfArgs =
          zip
            (repeat $ Just . gShowType g $ gSerialType g)
            (map pretty (mBoundVars m))
      , gfBody =
          (vsep args <>
           line <> (gReturn g) ((gCall g) calledFunction (take n (iArgs "a"))))
      }
  where
    n = length (mArgs m)

    calledFunction :: MDoc
    calledFunction
      | Man.isMorlocCall m = (callIdToName g) (findComposition (mMorlocName m))
      | otherwise =
        maybe (pretty (mName m)) (callIdToName g) (Map.lookup (mName m) cs)

    packersIfNeeded :: MorlocMonad ([Maybe MDoc])
    packersIfNeeded
      | Man.isMorlocCall m = return $ repeat Nothing
      | otherwise = Man.getUnpackers h m

    findComposition :: Name -> Manifold
    findComposition n = (filter (\m -> mComposition m == (Just n)) ms) !! 0

    makeArg :: MDoc -> (Maybe MDoc, Maybe MDoc, Argument) -> MorlocMonad MDoc
    makeArg lhs (ctype, unpacker, arg) = do
      argDoc <- makeArg' (unpacker, arg)
      return . gAssign g $
        GeneralAssignment
          {gaType = ctype, gaName = lhs, gaValue = argDoc, gaArg = Just arg}

    makeArg' :: (Maybe MDoc, Argument) -> MorlocMonad MDoc
    makeArg' (Nothing, arg) = writeArgument g ms (mBoundVars m) arg
    makeArg' (Just u, arg) =
      if useUnpacker g arg m
        then (writeArgument g ms (mBoundVars m) arg) >>= unpack' u
        else writeArgument g ms (mBoundVars m) arg

    -- Does this argument need to be deserialized?
    useUnpacker :: Grammar -> Argument -> Manifold -> Bool
    useUnpacker _ (ArgName n') m' = elem n' (mBoundVars m')
    useUnpacker g' (ArgCall k) _ =
      fmap mLang (lookupKey ms k) /= Just (gLang g')
    useUnpacker _ (ArgData _) _ = False
    useUnpacker _ (ArgPosi _) _ = True
    useUnpacker _  (ArgNest _)  _  = False -- will this always be true?

    unpack' :: MDoc -> MDoc -> MorlocMonad MDoc
    unpack' p x = do
      let name = callIdToName g m
      return
        ((gUnpacker g)
           (UnpackerDoc
              { udValue = x
              , udUnpacker = p
              , udMid = name
          -- TODO: remove hard-coded name
              , udFile = pretty (ML.makeSourceName (gLang g) "pool")
              }))

-- O(n) lookup of a manifold given a key (e.g., from Argument).
lookupKey :: [Manifold] -> URI -> Maybe Manifold
lookupKey ms k =
  case filter (\m -> mCallId m == k) ms of
    [m] -> Just m
    _ -> Nothing

makeSourceManifold ::
     Grammar -> SerialMap -> Manifold -> MorlocMonad GeneralFunction
makeSourceManifold g h m = do
  let name = callIdToName g m
      comments =
        (gComment g) "source manifold" <>
        line <>
        (gComment g) (maybe (fname m) pretty (mComposition m)) <>
        line <>
        (gComment g)
          (fname m <> " :: " <> maybe "undefined" pretty (mAbstractType m)) <>
        line
  argTypes <-
    DL.zip5 <$> pure (iArgs "a") <*> pure (getConcreteArgTypes g m) <*>
    (Man.getUnpackers h m) <*>
    pure (mArgs m) <*>
    pure (iArgs "x")
  argAssign <- mapM (unpack' name) argTypes
  return $
    GeneralFunction
      { gfComments = comments
      , gfReturnType = getConcreteReturnType g m
      , gfName = name
      , gfArgs =
          zip (repeat $ Just . gShowType g $ gSerialType g) (take n (iArgs "x"))
      , gfBody =
          (vsep argAssign) <>
          line <> (gReturn g) ((gCall g) (fname m) (take n (iArgs "a")))
      }
  where
    n = length (mArgs m)

    unpack' ::
         MDoc
      -> (MDoc, Maybe MDoc, Maybe MDoc, Argument, MDoc)
      -> MorlocMonad MDoc
    unpack' name (lhs, ctype, (Just u), _, x) =
      return . gAssign g $
      GeneralAssignment
        { gaType = ctype
        , gaName = lhs
        , gaValue =
            ((gUnpacker g)
               (UnpackerDoc
                  { udValue = x
                  , udUnpacker = u
                  , udMid = name
              -- TODO: remove hard-coded name
                  , udFile = pretty (ML.makeSourceName (gLang g) "pool")
                  }))
        , gaArg = Nothing
        }
    unpack' name t@(lhs, ctype, _, a, x) =
      MM.throwError . GeneratorError $
      "No unpacker found for argument: " <> render name

getConcreteArgTypes :: Grammar -> Manifold -> [Maybe MDoc]
getConcreteArgTypes g m
  | Man.isMorlocCall m = repeat Nothing
  | otherwise =
    case (mConcreteType m) of
      (Just (MFuncType _ ts _)) -> map (Just . gShowType g) ts
      _ -> repeat Nothing -- infinite nothing, fine for zipping, but don't map

getConcreteReturnType :: Grammar -> Manifold -> Maybe MDoc
getConcreteReturnType g m =
  case (mConcreteType m, mAbstractType m) of
    (Just (MFuncType _ _ t), _) -> Just . gShowType g $ t
    (_, Just (MFuncType _ _ t)) -> Just . gShowType g $ t
    _ -> Nothing

-- | writes an argument sans serialization 
writeArgument ::
     Grammar -> [Manifold] -> [MT.Text] -> Argument -> MorlocMonad MDoc
writeArgument _ _ _ (ArgName n) = return $ pretty n
writeArgument g _ _ (ArgData d) = return $ writeData g d
writeArgument _ _ _ (ArgPosi i) = return $ "x" <> pretty i
-- TODO: This currently does not handle morloc data definitions, such as:
-- x = [1,2,3]
-- This should compile into a literal, compile-time initilized, value. It should
-- be given a mangled name in the generated code, and made available as needed.
writeArgument g ms xs (ArgNest n) =
  case filter (\m -> mComposition m == Just n) ms of
    (m:_) -> return $ callIdToName g m
    [] ->
      case filter (\m -> mMorlocName m == n && mSourced m) ms of
        (m:_) -> return $ callIdToName g m
        [] ->
          MM.throwError $
          GeneratorError ("Could not find unbound value '" <> n <> "'")
writeArgument g ms xs (ArgCall k) = do
  m <-
    case lookupKey ms k of
      (Just m') -> return m'
      Nothing ->
        MM.throwError $
        CallTheMonkeys
          ("Could not find ArgCall key in manifold list: key=" <> MT.show' k)
  c <- MM.ask
  let name = callIdToName g m
      lang = mLang m
  if lang == gLang g
    then return $ (gCall g) name (map pretty xs)
    else case ( MC.getPoolCallBuilder c lang (gQuote g)
              , pretty (ML.makeSourceName lang "pool")
              , pretty (ML.makeExecutableName lang "pool")
              , map pretty xs) of
           (Just poolBuilder, pool, exe, args) ->
             return $
             (gForeignCall g)
               (ForeignCallDoc
                  { fcdForeignPool = pool
                  , fcdForeignExe = exe
                  , fcdMid = name
                  , fcdArgs = args
                  , fcdCall = poolBuilder exe (pretty $ mid m)
                  , fcdFile = pretty (ML.makeSourceName (gLang g) "pool")
                  })
           (Nothing, _, _, _) ->
             MM.throwError . GeneratorError $
             "No command could be found to run language " <>
             (ML.showLangName lang)

writeData :: Grammar -> MData -> MDoc
writeData _ (Num' x) = pretty x
writeData g (Str' x) = (gQuote g) (pretty x)
writeData g (Log' True) = gTrue g
writeData g (Log' False) = gFalse g
writeData g (Lst' xs) = (gList g) (map (writeData g) xs)
writeData g (Tup' xs) = (gTuple g) (map (writeData g) xs)
writeData g (Rec' xs) =
  (gRecord g) (map (\(k, v) -> (pretty k, writeData g v)) xs)

fname :: Manifold -> MDoc
fname m = pretty (mName m)
