{-|
Module      : Morloc.Pools.Common
Description : A common set of utility functions for language templates
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Pools.Common
(
    Grammar(..)
  , TryDoc(..)
  , GeneralFunction(..)
  , GeneralAssignment(..)
  , UnpackerDoc(..)
  , ForeignCallDoc(..)
  , PoolMain(..)
  , defaultCodeGenerator
) where

import Morloc.Global
import Morloc.Data.Doc hiding ((<$>))
import qualified Morloc.Language as ML
import qualified Morloc.Config as MC
import Morloc.Component.MType () -- just for mshow instances
import qualified Morloc.Monad as MM
import qualified Morloc.Manifold as Man
import qualified Morloc.Data.Text as MT
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as DM

data Grammar = Grammar {
      gLang        :: Lang
    , gSerialType  :: MType
    , gAssign      :: GeneralAssignment -> Doc
    , gCall        :: Doc -> [Doc] -> Doc
    , gFunction    :: GeneralFunction -> Doc
    , gSignature   :: GeneralFunction -> Doc
    , gId2Function :: Integer -> Doc
    , gComment     :: Doc -> Doc
    , gReturn      :: Doc -> Doc
    , gQuote       :: Doc -> Doc
    , gImport      :: Doc -> Doc -> Doc
    , gList        :: [Doc] -> Doc
    , gTuple       :: [Doc] -> Doc
    , gRecord      :: [(Doc,Doc)] -> Doc
    , gTrue        :: Doc
    , gFalse       :: Doc
    , gIndent      :: Doc -> Doc
    , gUnpacker    :: UnpackerDoc -> Doc
    , gTry         :: TryDoc -> Doc
    , gForeignCall :: ForeignCallDoc -> Doc
    , gSwitch      :: (Manifold -> Doc) -> (Manifold -> Doc) -> [Manifold] -> Doc -> Doc -> Doc
    , gCmdArgs     :: [Doc] -- ^ infinite list of main arguments (e.g. "argv[2]")
    , gShowType    :: MType -> Doc
    , gMain        :: PoolMain -> MorlocMonad Doc
  }

data GeneralAssignment = GeneralAssignment {
    gaType :: Maybe Doc
  , gaName :: Doc
  , gaValue :: Doc
} deriving (Show)

data GeneralFunction = GeneralFunction {
    gfComments :: Doc
  , gfReturnType :: Maybe Doc -- ^ concrete return type
  , gfName :: Doc -- ^ function name
  , gfArgs :: [(Maybe Doc, Doc)] -- ^ (variable concrete type, variable name)
  , gfBody :: Doc 
} deriving (Show)

data TryDoc = TryDoc {
    tryCmd :: Doc    -- ^ The function we attempt to run
  , tryRet :: Doc    -- ^ A name for the returned variable?
  , tryArgs :: [Doc] -- ^ Arguments passed to function
  , tryMid :: Doc    -- ^ The name of the calling manifold (for debugging)
  , tryFile :: Doc   -- ^ The file where the issue occurs (for debugging)
} deriving (Show)

data UnpackerDoc = UnpackerDoc {
    udValue :: Doc    -- ^ The expression that will be unpacked
  , udUnpacker :: Doc -- ^ The function for unpacking the value
  , udMid :: Doc      -- ^ Manifold name for debugging messages
  , udFile :: Doc     -- ^ File name for debugging messages
} deriving (Show)

data ForeignCallDoc = ForeignCallDoc {
    fcdForeignPool :: Doc   -- ^ the name of the foreign pool (e.g., "R.pool")
  , fcdForeignExe  :: Doc   -- ^ path to the foreign executable
  , fcdMid         :: Doc   -- ^ the function integer identifier
  , fcdArgs        :: [Doc] -- ^ CLI arguments passed to foreign function
  , fcdCall        :: [Doc] -- ^ make a list of CLI arguments from first two
                            -- inputs -- since fcdArgs will likely be
                            -- variables, they are not included in this call.
  , fcdFile        :: Doc   -- ^ for debugging
} deriving (Show)

data PoolMain = PoolMain {
    pmSources :: [Doc]
  , pmPoolManifolds :: [GeneralFunction]
  , pmDispatchManifold :: Doc -> Doc -> Doc
}

defaultCodeGenerator
  :: Grammar
  -> (MT.Text -> MorlocMonad Doc) -- source name parser
  -> [Manifold]
  -> SerialMap
  -> MorlocMonad Script
defaultCodeGenerator g f manifolds packMap = do
  state <- MM.get 
  paksrcs <- mapM f (serialSources packMap)
  mansrcs <- Man.getManSrcs (gLang g) f manifolds
  lib <- MM.asks MC.configLibrary
  poolManifolds <- makePoolManifolds g packMap (getMorlocFun2sourceFun manifolds) manifolds
  let srcs = map ((gImport g) (text' lib)) (paksrcs ++ mansrcs)
      dispatchManifold =
        (gSwitch g)
           (text' . MT.show' . mid)
           (mainCall g packMap)
           (Man.getUsedManifolds (gLang g) manifolds)
  doc <- (gMain g) (PoolMain
      { pmSources = srcs
      , pmPoolManifolds = poolManifolds
      , pmDispatchManifold = dispatchManifold
      }
    )
  return $ Script {
        scriptBase = "pool"
      , scriptLang = gLang g
      , scriptCode = render doc
      , scriptCompilerFlags = filter (/= "") . map packageGccFlags $ statePackageMeta state
    }

-- call a manifold and deserialize the return value
mainCall :: Grammar -> SerialMap -> Manifold -> Doc
mainCall g h m
  = (gCall g) (Man.getPacker h m)
  $ [(gCall g) (callIdToName g m) (take n (gCmdArgs g))]
  where
    n = if mSourced m && not (mCalled m)
        then length (mArgs m)
        else length (mBoundVars m)
      

-- | When a morloc function is declared, for example @foo x = sqrt(pow x 2)@,
-- then calling @foo@ from the nexus should call the source function @sqrt@ not
-- the morloc function @foo@ (since the morloc function does not really exist).
-- The @getMorlocFun2sourceFun@ function maps from morloc names (@foo@) to the
-- top-level source name (@sqrt@).
getMorlocFun2sourceFun :: [Manifold] -> Map.Map Name Manifold
getMorlocFun2sourceFun ms = Map.fromList
  [(n,m) | (Just n, m) <- (map (\m -> (mComposition m, m)) ms)]

callIdToName :: Grammar -> Manifold -> Doc
callIdToName g m = text' . MT.show' $ (gId2Function g) (mid m)

-- | inifinite list of named variables
iArgs :: Doc -> [Doc]
iArgs prefix = zipWith (<>) (repeat prefix) (map int [0..])

makePoolManifolds
  :: Grammar
  -> SerialMap
  -> Map.Map Key Manifold
  -> [Manifold]
  -> MorlocMonad [GeneralFunction]
makePoolManifolds g h cs ms = fmap DM.catMaybes . mapM makePoolManifold $ ms
  where
    makePoolManifold m = case Man.determineManifoldClass (gLang g) m of 
      Cis -> Just <$> makeCisManifold g h cs ms m
      Source -> Just <$> makeSourceManifold g h m
      _ -> return Nothing

makeCisManifold
  :: Grammar
  -> SerialMap
  -> Map.Map MT.Text Manifold
  -> [Manifold]
  -> Manifold
  -> MorlocMonad GeneralFunction
makeCisManifold g h cs ms m = do
  argTypes <-  zip3
           <$> pure (getConcreteArgTypes g m) -- [Maybe Doc]
           <*> packersIfNeeded                -- Maybe [Doc]
           <*> pure (mArgs m)                 -- [Argument]
  args <- sequence $ zipWith makeArg (iArgs "a") argTypes

  let name = callIdToName g m
      comments
        =  (gComment g) "cis manifold" <> line
        <> (gComment g) (maybe (fname m) text' (mComposition m)) <> line
        <> (gComment g) (fname m <> " :: " <> maybe "undefined" mshow (mAbstractType m))
        <> line
  return $ GeneralFunction {
           gfComments = comments
         , gfReturnType = getConcreteReturnType g m
         , gfName = name
         , gfArgs = zip (repeat $ Just . gShowType g $ gSerialType g)
                        (map text' (mBoundVars m))
         , gfBody = (vsep args <>
                     line <>
                     (gReturn g) ((gCall g) calledFunction (take n (iArgs "a"))))
         }
  where
    n = length (mArgs m)

    calledFunction :: Doc
    calledFunction
      | Man.isMorlocCall m = (callIdToName g) (findComposition (mMorlocName m))
      | otherwise =  maybe (text' (mName m))
                           (callIdToName g)
                           (Map.lookup (mName m) cs)

    packersIfNeeded :: MorlocMonad ([Maybe Doc])
    packersIfNeeded
      | Man.isMorlocCall m = return $ repeat Nothing
      | otherwise = fmap (map Just) $ Man.getUnpackers h m

    findComposition :: Name -> Manifold
    findComposition n = (filter (\m -> mComposition m == (Just n)) ms) !! 0

    makeArg :: Doc -> (Maybe Doc, Maybe Doc, Argument) -> MorlocMonad Doc
    makeArg lhs (ctype, unpacker, arg) = do
      arg <- makeArg' (unpacker, arg)
      return . gAssign g $ GeneralAssignment {
            gaType = ctype
          , gaName = lhs
          , gaValue = arg
        }

    makeArg' :: (Maybe Doc, Argument) -> MorlocMonad Doc
    makeArg' (Nothing, arg) = writeArgument g ms (mBoundVars m) arg
    makeArg' (Just u, arg) =
      if
        useUnpacker g arg m
      then
        (writeArgument g ms (mBoundVars m) arg) >>= unpack' u 
      else                  
        writeArgument g ms (mBoundVars m) arg

    -- Does this argument need to be deserialized?
    useUnpacker :: Grammar -> Argument -> Manifold -> Bool
    useUnpacker _  (ArgName n') m' = elem n' (mBoundVars m')
    useUnpacker g' (ArgCall k)  _  = fmap mLang (lookupKey ms k) /= Just (gLang g')
    useUnpacker _  (ArgData _)  _  = False
    useUnpacker _  (ArgPosi _)  _  = True

    unpack' :: Doc -> Doc -> MorlocMonad Doc
    unpack' p x = do
      let name = callIdToName g m 
      return ((gUnpacker g) (UnpackerDoc {
            udValue = x
          , udUnpacker = p
          , udMid = name
          -- TODO: remove hard-coded name
          , udFile = text' (ML.makeSourceName (gLang g) "pool")
        }))

-- O(n) lookup of a manifold given a key (e.g., from Argument).
lookupKey :: [Manifold] -> Key -> Maybe Manifold
lookupKey ms k = case filter (\m -> mCallId m == k) ms of 
  [m] -> Just m
  _ -> Nothing

makeSourceManifold :: Grammar -> SerialMap -> Manifold -> MorlocMonad GeneralFunction
makeSourceManifold g h m = do
  argTypes <- zip3 <$> pure (getConcreteArgTypes g m) <*> (Man.getUnpackers h m) <*> pure (mArgs m)
  let name = callIdToName g m
      comments
        =  (gComment g) "source manifold" <> line
        <> (gComment g) (maybe (fname m) text' (mComposition m)) <> line
        <> (gComment g) (fname m <> " :: " <> maybe "undefined" mshow (mAbstractType m))
        <> line
  return $ GeneralFunction {
           gfComments = comments
         , gfReturnType = getConcreteReturnType g m
         , gfName = name
         , gfArgs = zip (repeat $ Just . gShowType g $ gSerialType g)
                        (take n (iArgs "x"))
         , gfBody = (vsep $ zipWith3
                              (unpack' name)
                              (iArgs "a")
                              argTypes
                              (iArgs "x"))
                  <> line
                  <> (gReturn g) ((gCall g) (fname m) (take n (iArgs "a")))
         }
  where
    n = length (mArgs m)

    unpack' :: Doc -> Doc -> (Maybe Doc, Doc, Argument) -> Doc -> Doc
    unpack' name lhs (ctype, u, _) x
      = (gAssign g) $ GeneralAssignment {
            gaType = ctype
          , gaName = lhs
          , gaValue = ((gUnpacker g) (UnpackerDoc {
                udValue = x   
              , udUnpacker = u
              , udMid = name
              -- TODO: remove hard-coded name
              , udFile = text' (ML.makeSourceName (gLang g) "pool")
            }))
          }

getConcreteArgTypes :: Grammar -> Manifold -> [Maybe Doc]
getConcreteArgTypes g m
  | Man.isMorlocCall m = repeat Nothing
  | otherwise = case (mConcreteType m) of
      (Just (MFuncType _ ts _)) -> map (Just . gShowType g) ts
      _ -> repeat Nothing -- infinite nothing, fine for zipping, but don't map

getConcreteReturnType :: Grammar -> Manifold -> Maybe Doc
getConcreteReturnType g m = case (mConcreteType m, mAbstractType m) of
  (Just (MFuncType _ _ t), _) -> Just . gShowType g $ t
  (_, Just (MFuncType _ _ t)) -> Just . gShowType g $ t
  _ -> Nothing

-- | writes an argument sans serialization 
writeArgument :: Grammar -> [Manifold] -> [MT.Text] -> Argument -> MorlocMonad Doc
writeArgument _ _  _  (ArgName n) = return $ text' n
writeArgument g _  _  (ArgData d) = return $ writeData g d
writeArgument _ _  _  (ArgPosi i) = return $ "x" <> int i
writeArgument g ms xs (ArgCall k) = do
  m <- case lookupKey ms k of
    (Just m') -> return m'
    Nothing -> MM.throwError $ CallTheMonkeys ("Could not find ArgCall key in manifold list: key=" <> k)
  c <- MM.ask
  let name = callIdToName g m
      lang = mLang m
  if
    lang == gLang g
  then
    return $ (gCall g) name (map text' xs)
  else
    case (
        MC.getPoolCallBuilder c lang (gQuote g)
      , text' (ML.makeSourceName lang "pool")
      , text' (ML.makeExecutableName lang "pool")
      , map text' xs
    ) of
      (Just poolBuilder, pool, exe, args) -> return $
        (gForeignCall g) (ForeignCallDoc {
            fcdForeignPool = pool
          , fcdForeignExe = exe
          , fcdMid = name
          , fcdArgs = args
          , fcdCall = poolBuilder exe (integer $ mid m)
          , fcdFile = text' (ML.makeSourceName (gLang g) "pool")
        })
      (Nothing, _, _, _) -> MM.throwError . GeneratorError $
        "No command could be found to run language " <> (ML.showLangName lang)

writeData :: Grammar -> MData -> Doc
writeData _ (Num' x)     = text' x
writeData g (Str' x)     = (gQuote g) (text' x)
writeData g (Log' True)  = gTrue g
writeData g (Log' False) = gFalse g
writeData g (Lst' xs)    = (gList g) (map (writeData g) xs)
writeData g (Tup' xs)    = (gTuple g) (map (writeData g) xs)
writeData g (Rec' xs)    = (gRecord g) (map (\(k, v) -> (text' k, writeData g v)) xs)

fname :: Manifold -> Doc
fname m = text' (mName m)
