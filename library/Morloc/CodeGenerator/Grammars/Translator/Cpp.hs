{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, ViewPatterns, FlexibleContexts #-}

{-|
Module      : Morloc.CodeGenerator.Grammars.Translator.Cpp
Description : C++ translator
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.CodeGenerator.Grammars.Translator.Cpp
  ( 
    translate
  , preprocess
  ) where

import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.Serial ( isSerializable
                                   , prettySerialOne
                                   , serialAstToType
                                   , shallowType
                                   )
import Morloc.CodeGenerator.Grammars.Common
import qualified Morloc.CodeGenerator.Grammars.Translator.Source.CppInternals as Src
import Morloc.Data.Doc
import Morloc.Quasi
import Morloc.CodeGenerator.Grammars.Macro (expandMacro)
import qualified Morloc.Monad as MM
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Morloc.Data.Text as MT
import qualified Morloc.Module as Mod
import qualified Morloc.Language as ML
import qualified Control.Monad.State as CMS
import Control.Monad.Identity (Identity)

-- | @RecEntry@ stores the common name, keys, and types of records that are not
-- imported from C++ source. These records are generated as structs in the C++
-- pool. @unifyRecords@ takes all such records and "unifies" ones with the same
-- name and keys. The unified records may have different types, but they will
-- all be instances of the same generic struct. That is, any fields that differ
-- between instances will be made generic.
data RecEntry = RecEntry
  { recName :: MDoc -- ^ the automatically generated name for this anonymous type
  , recFields :: [( FVar -- The field key
                  , Maybe TypeF -- The field type if not generic
                  )]
  }
  deriving (Show)

-- | @RecMap@ is used to lookup up the struct name shared by all records that
-- are not imported from C++ source.
type RecMap = [((FVar, [FVar]), RecEntry)]

data CppTranslatorState = CppTranslatorState
  { translatorCounter :: Int
  , translatorRecmap :: RecMap
  , translatorSignatureSet :: Set.Set Int
  , translatorManifoldSet :: Set.Set Int
  , translatorCurrentManifold :: Int
  }

setManifoldIndex :: Int -> CppTranslatorState -> CppTranslatorState
setManifoldIndex i s = s { translatorCurrentManifold = i }


instance Defaultable CppTranslatorState where
  defaultValue = CppTranslatorState
    { translatorCounter = 0
    , translatorRecmap = []
    , translatorSignatureSet = Set.empty
    , translatorManifoldSet = Set.empty
    , translatorCurrentManifold = -1 -- -1 indicates we are not inside a manifold
    }

type CppTranslator a = CMS.StateT CppTranslatorState Identity a

getCounter :: CppTranslator Int
getCounter = do
    s <- CMS.get
    let i = translatorCounter s
    CMS.put $ s {translatorCounter = translatorCounter s + 1}
    return i

resetCounter :: CppTranslator ()
resetCounter = do
    s <- CMS.get
    CMS.put $ s {translatorCounter = 0}

-- tree rewrites
preprocess :: SerialManifold -> MorlocMonad SerialManifold
preprocess = return . invertSerialManifold

translate :: [Source] -> [SerialManifold] -> MorlocMonad Script
translate srcs es = do
  -- -- diagnostics
  -- liftIO . putDoc . vsep $ "-- C++ translation --" : map pretty es

  -- generate code for serialization
  serializationBoilerplate <- generateSourcedSerializers es

  let recmap = unifyRecords . concatMap collectRecords $ es
      translatorState = defaultValue { translatorRecmap = recmap }
      code = CMS.evalState (makeCppCode srcs es serializationBoilerplate) translatorState

  maker <- makeTheMaker srcs

  return $ Script
    { scriptBase = "pool"
    , scriptLang = CppLang
    , scriptCode = "." :/ File "pool.cpp" (Code . render $ code)
    , scriptMake = maker
    }

makeCppCode :: [Source] -> [SerialManifold] -> ([MDoc], [MDoc]) -> CppTranslator MDoc
makeCppCode srcs es (srcDecl, srcSerial) = do

  -- write include statements for sources
  let includeDocs = map translateSource (unique . mapMaybe srcPath $ srcs)

  let dispatch = makeDispatch es

  signatures <- concat <$> mapM makeSignature es

  (autoDecl, autoSerial) <- generateAnonymousStructs
  let serializationCode = autoDecl ++ srcDecl ++ autoSerial ++ srcSerial

  -- translate each manifold tree, rooted on a call from nexus or another pool
  mDocs <- mapM translateSegment es

  -- create and return complete pool script
  return $ makeMain includeDocs signatures serializationCode mDocs dispatch

makeTheMaker :: [Source] -> MorlocMonad [SysCommand]
makeTheMaker srcs = do
  let outfile = pretty $ ML.makeExecutableName CppLang "pool"
  let src = pretty (ML.makeSourceName CppLang "pool")

  -- this function cleans up source names (if needed) and generates compiler flags and paths to search
  (_, flags, includes) <- Mod.handleFlagsAndPaths CppLang srcs

  let incs = [pretty ("-I" <> i) | i <- includes]
  let flags' = map pretty flags

  -- TODO: This is garbage - the C++ version should NOT be specified here
  let cmd = SysRun . Code . render $ [idoc|g++ --std=c++17 -o #{outfile} #{src} #{hsep flags'}|]

  return [cmd]

letNamer :: Int -> MDoc
letNamer i = "a" <> viaShow i

helperNamer :: Int -> MDoc
helperNamer i = "helper" <> viaShow i

manNamer :: Int -> MDoc
manNamer i = "m" <> viaShow i

bndNamer :: Int -> MDoc
bndNamer i = "x" <> viaShow i

serialType :: MDoc
serialType = pretty $ ML.serialType CppLang 

makeSignature :: SerialManifold -> CppTranslator [MDoc]
makeSignature = foldSerialManifoldM fm where
  fm = FoldManifoldM
    { opSerialManifoldM = serialManifold
    , opNativeManifoldM = nativeManifold
    , opSerialExprM = return . foldlSE (<>) []
    , opNativeExprM = return . foldlNE (<>) []
    , opSerialArgM = return . foldlSA (<>) []
    , opNativeArgM = return . foldlNA (<>) []
    }

  serialManifold (SerialManifold_ i _ form _) = manifoldSignature i serialType form

  nativeManifold e@(NativeManifold_ i _ form _) = do
    typestr <- showTypeF (typeFof e)
    manifoldSignature i typestr form

  manifoldSignature :: Int -> MDoc -> ManifoldForm TypeM -> CppTranslator [MDoc]
  manifoldSignature i typestr form = do
    s <- CMS.get
    if Set.member i (translatorSignatureSet s)
      then return []
      else do
        args <- mapM makeArg (manifoldArgs form)
        CMS.put (s {translatorSignatureSet = Set.insert i (translatorSignatureSet s)})
        return [typestr <+> manNamer i <> tupled args <> ";"]


makeArg :: Arg TypeM -> CppTranslator MDoc
makeArg (Arg i (Serial _)) = return $ serialType <+> bndNamer i
makeArg (Arg i (Native c)) = do
    typestr <- showTypeF c
    return $ typestr <+> bndNamer i
makeArg (Arg i Passthrough) = return $ serialType <+> bndNamer i
makeArg (Arg _ (Function _ _)) = error "Function support not implemented?"

argName :: Arg TypeM -> MDoc
argName (argId -> i) = bndNamer i

tupleKey :: Int -> MDoc -> MDoc
tupleKey i v = [idoc|std::get<#{pretty i}>(#{v})|]

recordAccess :: MDoc -> MDoc -> MDoc
recordAccess record field = record <> "." <> field

-- TLDR: Use `#include "foo.h"` rather than `#include <foo.h>`
-- Include statements in C can be either wrapped in angle brackets (e.g.,
-- `<stdio.h>`) or in quotes (e.g., `"myfile.h"`). The difference between these
-- is implementation specific. I currently use the GCC compiler. For quoted
-- strings, it first searches relative to the working directory and then, if
-- nothing is found, searches system files. For angle brackets, it searches
-- only system files: <https://gcc.gnu.org/onlinedocs/cpp/Search-Path.html>. So
-- quoting seems more reasonable, for now. This might change only if I start
-- loading the morloc libraries into the system directories (which might be
-- reasonable), though still, quotes would work.
--
-- UPDATE: The build system will now read the source paths from the Script
-- object and write an `-I${MORLOC_HOME}/lib/${MORLOC_PACKAGE}` argument for
-- g++. This will tell g++ where to look for headers. So now in the generated
-- source code I can just write the basename. This makes the generated code
-- neater (no hard-coded local paths), but now the g++ compiler will search
-- through all the module paths for each file, which introduces the possibility
-- of name conflicts.
--
-- UPDATE: And now those naming conflicts have bitten me. Simply including every
-- module directory is a wretched idea.
translateSource
  :: Path -- ^ Path to a header (e.g., `$MORLOC_HOME/src/foo.h`)
  -> MDoc
translateSource path = "#include" <+> (dquotes . pretty) path

serialize :: MDoc -> SerialAST -> CppTranslator PoolDocs
serialize nativeExpr s0 = do
  (x, before) <- serialize' nativeExpr s0
  typestr <- showTypeF $ serialAstToType s0

  -- TODO: I can remove the requirement for this schema term by adding a type
  -- annotation to the serialization function (I think)
  schemaIndex <- getCounter
  let schemaName = [idoc|#{helperNamer schemaIndex}_schema|]
      schema = [idoc|#{typestr} #{schemaName};|]
      final = [idoc|serialize(#{x}, #{schemaName})|]
  return $ PoolDocs
      { poolCompleteManifolds = []
      , poolExpr = final
      , poolPriorLines = before <> [schema]
      , poolPriorExprs = []
      }

  where
    serialize'
      :: MDoc -- a variable name that stores the data described by the SerialAST object
      -> SerialAST -> CppTranslator (MDoc, [MDoc])
    serialize' v s
      | isSerializable s = return (v, [])
      | otherwise = construct v s

    construct :: MDoc -> SerialAST -> CppTranslator (MDoc, [MDoc])
    construct v (SerialPack _ (p, s)) = do
      let unpacker = pretty . srcName . typePackerReverse $ p
      serialize' [idoc|#{unpacker}(#{v})|] s

    construct v lst@(SerialList _ s) = do
      idx <- fmap pretty getCounter
      typestr <- showTypeF $ serialAstToType lst
      let v' = "s" <> idx
          decl = [idoc|#{typestr} #{v'};|]
      (x, before) <- serialize' [idoc|#{v}[i#{idx}]|] s
      let push = [idoc|#{v'}.push_back(#{x});|]
          loop  = block 4 [idoc|for(size_t i#{idx} = 0; i#{idx} < #{v}.size(); i#{idx}++)|]
                         (vsep (before ++ [push]))
      return (v', [decl, loop])

    construct v tup@(SerialTuple _ ss) = do
      (ss', befores) <- unzip <$> zipWithM (\i s -> serialize' (tupleKey i v) s) [0..] ss
      idx <- fmap pretty getCounter
      typestr <- showTypeF $ serialAstToType tup
      let v' = "s" <> idx
          x = [idoc|#{typestr} #{v'} = std::make_tuple#{tupled ss'};|]
      return (v', concat befores ++ [x]);

    construct v rec@(SerialObject NamRecord _ _ rs) = do
      (ss', befores) <- unzip <$> mapM (\(FV _ k,s) -> serialize' (recordAccess v (pretty k)) s) rs
      idx <- fmap pretty getCounter
      t <- showTypeF (serialAstToType rec)
      let v' = "s" <> idx
          decl = encloseSep "{" "}" "," ss'
          x = [idoc|#{t} #{v'} = #{decl};|]
      return (v', concat befores ++ [x]);

    construct _ (SerialObject NamObject _ _ _) = error "C++ object serialization not yet implemented"
    construct _ (SerialObject NamTable _ _ _) = error "C++ table serialization not yet implemented"

    construct v (SerialReal    _) = return (v, [])
    construct v (SerialInt     _) = return (v, [])
    construct v (SerialBool    _) = return (v, [])
    construct v (SerialString  _) = return (v, [])
    construct v (SerialNull    _) = return (v, [])
    construct v (SerialUnknown _) = return (v, [])

-- reverse of serialize, parameters are the same
deserialize :: MDoc -> MDoc -> SerialAST -> CppTranslator (MDoc, [MDoc])
deserialize varname0 typestr0 s0
  | isSerializable s0 = do
      schemaVar <- helperNamer <$> getCounter
      let schemaName = [idoc|#{schemaVar}_schema|]
          schema = [idoc|#{typestr0} #{schemaName};|]
          term = [idoc|deserialize(#{varname0}, #{schemaName})|]
      return (term, [schema])
  | otherwise = do
      schemaVar <- helperNamer <$> getCounter
      idx <- getCounter
      rawtype <- showTypeF $ serialAstToType s0
      let schemaName = [idoc|#{schemaVar}_schema|]
          rawvar = helperNamer idx
          schema = [idoc|#{rawtype} #{schemaName};|]
          deserializing = [idoc|#{rawtype} #{rawvar} = deserialize(#{varname0}, #{schemaName});|]
      (x, before) <- construct rawvar s0
      let final = [idoc|#{typestr0} #{schemaVar} = #{x};|]
      return (schemaVar , [schema, deserializing] ++ before ++ [final])

  where
    check :: MDoc -> SerialAST -> CppTranslator (MDoc, [MDoc])
    check v s
      | isSerializable s = return (v, [])
      | otherwise = construct v s

    construct :: MDoc -> SerialAST -> CppTranslator (MDoc, [MDoc])
    construct v (SerialPack _ (p, s')) = do
      (x, before) <- check v s'
      let packer = pretty . srcName . typePackerForward $ p
          deserialized = [idoc|#{packer}(#{x})|]
      return (deserialized, before)

    construct v lst@(SerialList _ s) = do
      idx <- fmap pretty getCounter
      t <- showTypeF $ shallowType lst
      let v' = "s" <> idx
          decl = [idoc|#{t} #{v'};|]
      (x, before) <- check [idoc|#{v}[i#{idx}]|] s
      let push = [idoc|#{v'}.push_back(#{x});|]
          loop = block 4 [idoc|for(size_t i#{idx} = 0; i#{idx} < #{v}.size(); i#{idx}++)|]
                         (vsep (before ++ [push]))
      return (v', [decl, loop])

    construct v tup@(SerialTuple _ ss) = do
      idx <- fmap pretty getCounter
      (ss', befores) <- unzip <$> zipWithM (\i s -> check (tupleKey i v) s) [0..] ss
      typestr <- showTypeF $ shallowType tup
      let v' = "s" <> idx
          x = [idoc|#{typestr} #{v'} = std::make_tuple#{tupled ss'};|]
      return (v', concat befores ++ [x]);

    construct v rec@(SerialObject NamRecord _ _ rs) = do
      idx <- fmap pretty getCounter
      (ss', befores) <- mapAndUnzipM (\(FV _ k,s) -> check (recordAccess v (pretty k)) s) rs
      t <- showTypeF (shallowType rec)
      let v' = "s" <> idx
          decl = encloseSep "{" "}" "," ss'
          x = [idoc|#{t} #{v'} = #{decl};|]
      return (v', concat befores ++ [x]);

    construct _ _ = undefined -- TODO add support for deserialization of remaining types (e.g. other records)


-- translateManifold :: RecMap -> Set.Set Int -> SerialManifold -> MorlocMonad (Set.Set Int, MDoc)
translateSegment :: SerialManifold -> CppTranslator MDoc
translateSegment m0 = do
  resetCounter
  e <- surroundFoldSerialManifoldM surroundRules foldRules m0
  return $ vsep . punctuate line $ poolPriorExprs e <> poolCompleteManifolds e
  where

  -- The surround rules control the setting of manifold ids across the recursion
  surroundRules = defaultValue
    { surroundSerialManifoldM = surroundSM
    , surroundNativeManifoldM = surroundNM
    }

  -- | Run a computation in a child manifold, manage manifold indices
  descend :: Int -> a -> (a -> CppTranslator b) -> CppTranslator b
  descend childManifoldIndex x f = do
    originalManifoldIndex <- CMS.gets translatorCurrentManifold
    s <- CMS.get
    CMS.put $ s {translatorCurrentManifold = childManifoldIndex}
    x' <- f x
    CMS.put $ s {translatorCurrentManifold = originalManifoldIndex}
    return x'

  surroundSM :: (SerialManifold -> CppTranslator a) -> SerialManifold -> CppTranslator a
  surroundSM f sm@(SerialManifold i _ _ _) = descend i sm f 

  surroundNM :: (NativeManifold -> CppTranslator a) -> NativeManifold -> CppTranslator a
  surroundNM f nm@(NativeManifold i _ _ _) = descend i nm f

  foldRules = FoldManifoldM
    { opSerialManifoldM = makeSerialManifold
    , opNativeManifoldM = makeNativeManifold
    , opSerialExprM = makeSerialExpr
    , opNativeExprM = makeNativeExpr
    , opSerialArgM = makeSerialArg
    , opNativeArgM = makeNativeArg
    }

  makeSerialManifold :: SerialManifold_ PoolDocs -> CppTranslator PoolDocs
  makeSerialManifold sm@(SerialManifold_ i _ form e) = makeManifold i form (typeFof sm) e

  makeNativeManifold :: NativeManifold_ PoolDocs -> CppTranslator PoolDocs
  makeNativeManifold nm@(NativeManifold_ i _ form (_, e)) = makeManifold i form (typeFof nm) e

  makeSerialArg :: SerialArg_ PoolDocs PoolDocs -> CppTranslator PoolDocs
  makeSerialArg (SerialArgManifold_ x) = return x
  makeSerialArg (SerialArgExpr_ x) = return x

  makeNativeArg :: NativeArg_ PoolDocs PoolDocs -> CppTranslator PoolDocs
  makeNativeArg (NativeArgManifold_ x) = return x
  makeNativeArg (NativeArgExpr_ x) = return x

  makeSerialExpr :: SerialExpr_ PoolDocs PoolDocs PoolDocs PoolDocs PoolDocs -> CppTranslator PoolDocs
  makeSerialExpr (AppManS_ e (map catEither -> es)) =
    return $ mergePoolDocs ((<>) (poolExpr e) . tupled) (e:es)
  makeSerialExpr (AppPoolS_ (PoolCall _ cmds args) es) = do
    let bufDef = "std::ostringstream s;"
        callArgs = map dquotes cmds ++ map argName args
        cmd = "s << " <> cat (punctuate " << \" \" << " (callArgs <> map poolExpr es)) <> ";"
        call = [idoc|foreign_call(s.str())|]
    return $ PoolDocs
      { poolCompleteManifolds = []
      , poolExpr = call
      , poolPriorLines = [bufDef, cmd]
      , poolPriorExprs = []
      }
  makeSerialExpr (ReturnS_ e) = return $ e {poolExpr = "return(" <> poolExpr e <> ");"}
  makeSerialExpr (SerialLetS_ letIndex sa sb) = return $ makeLet letIndex serialType sa sb
  makeSerialExpr (NativeLetS_ letIndex (t, na) sb) = do
    typestr <- showTypeF t
    return $ makeLet letIndex typestr na sb
  makeSerialExpr (LetVarS_ i) = return $ PoolDocs [] (letNamer i) [] []
  makeSerialExpr (BndVarS_ i) = return $ PoolDocs [] (bndNamer i) [] []
  makeSerialExpr (SerializeS_ s e) = do
    se <- serialize (poolExpr e) s 
    return $ mergePoolDocs (\_ -> poolExpr se) [e, se]

  makeNativeExpr :: NativeExpr_ PoolDocs PoolDocs PoolDocs PoolDocs PoolDocs -> CppTranslator PoolDocs
  makeNativeExpr (AppSrcN_      _ src es) =
    return $ mergePoolDocs ((<>) (pretty $ srcName src) . tupled) es
  makeNativeExpr (AppManN_      _ call (map catEither -> xs)) =
    return $ mergePoolDocs ((<>) (poolExpr call) . tupled) (call : xs)
  makeNativeExpr (ReturnN_      _ e) =
    return $ e {poolExpr = "return" <> parens (poolExpr e) <> ";"} 
  makeNativeExpr (SerialLetN_   i sa (_, nb)) = return $ makeLet i serialType sa nb
  makeNativeExpr (NativeLetN_   i (t1, na) (_, nb)) = makeLet i <$> showTypeF t1 <*> pure na <*> pure nb
  makeNativeExpr (LetVarN_      _ i) = return $ PoolDocs [] (letNamer i) [] []
  makeNativeExpr (BndVarN_      _ i) = return $ PoolDocs [] (bndNamer i) [] []
  makeNativeExpr (DeserializeN_ t s e) = do
    typestr <- showTypeF t
    (deserialized, assignments) <- deserialize (poolExpr e) typestr s
    return $ e
      { poolExpr = deserialized
      , poolPriorLines = poolPriorLines e <> assignments
      }

  makeNativeExpr (AccN_ _ _ _ e k) =
    return (e {poolExpr = poolExpr e <> "." <> pretty k})

  makeNativeExpr (SrcN_ _ _) = undefined

  makeNativeExpr (ListN_ _ _ es) =
    return $ mergePoolDocs (encloseSep "{" "}" ",") es

  makeNativeExpr (TupleN_ _ es) =
    return $ mergePoolDocs ((<>) "std::make_tuple" . tupled) (map snd es)

  makeNativeExpr e@(RecordN_ _ _ _ rs) = do
    t <- showTypeF $ typeFof e
    idx <- getCounter
    let v' = "a" <> pretty idx
        decl = t <+> v' <+> encloseSep "{" "}" "," (map (poolExpr . snd . snd) rs) <> ";"
    let p = mergePoolDocs (const v') (map (snd . snd) rs)
    return (p {poolPriorLines = poolPriorLines p <> [decl]})

  makeNativeExpr (LogN_         _ x) = return (PoolDocs [] (if x then "true" else "false") [] [])
  makeNativeExpr (RealN_        _ x) = return (PoolDocs [] (viaShow x) [] [])
  makeNativeExpr (IntN_         _ x) = return (PoolDocs [] (viaShow x) [] [])
  makeNativeExpr (StrN_         _ x) = return (PoolDocs [] (dquotes $ pretty x) [] [])
  makeNativeExpr (NullN_        _  ) = return (PoolDocs [] "null" [] [])


  makeLet :: Int -> MDoc -> PoolDocs -> PoolDocs -> PoolDocs
  makeLet letIndex typestr (PoolDocs ms1 e1 rs1 pes1) (PoolDocs ms2 e2 rs2 pes2) =
    let letAssignment = [idoc|#{typestr} #{letNamer letIndex} = #{e1};|]
        rs = rs1 <> [ letAssignment ] <> rs2 <> [e2]
    in PoolDocs
      { poolCompleteManifolds = ms1 <> ms2
      , poolExpr = vsep rs
      , poolPriorLines = []
      , poolPriorExprs = pes1 <> pes2
      }


makeManifold
  :: Int -- ^ The index of the manifold that is being created
  -> ManifoldForm TypeM
  -> TypeF -- ^ The type of the manifold (usually a function, serialized terms are of general type "Str" and C++ type "std::string"
  -> PoolDocs -- ^ Generated content for the manifold body
  -> CppTranslator PoolDocs
makeManifold callIndex form manifoldType e = do
  completeManifold <- makeManifoldBody (poolExpr e)
  call <- makeManifoldCall form
  return $ e { poolExpr = call
             , poolCompleteManifolds = poolCompleteManifolds e <> maybeToList completeManifold
             , poolPriorLines = poolPriorLines e
             }
  where

  mname = manNamer callIndex

  makeManifoldCall :: ManifoldForm TypeM -> CppTranslator MDoc 
  makeManifoldCall (ManifoldFull rs) = return $ mname <> tupled (map (bndNamer . argId) rs)
  makeManifoldCall (ManifoldPass args) = do
    typestr <- stdFunction (returnType manifoldType) args
    return $ typestr <> parens mname
  makeManifoldCall (ManifoldPart rs vs) = do
    -- Partial application is fucking ugly in C++.
    -- Probably there is a prettier way to do this.
    -- In Haskell:
    --   mul :: Num -> Num -> Num
    --   multiplyByFive = map (mul 5) xs
    --
    -- In C++, (mul 5) becomes:
    --   std::function<double(double)>(std::bind(static_cast<double(*)(double, double)>(&mul), 5, std::placeholders::_1))
    --
    -- TODO: find some magic to make this suck less
    appliedTypeStr <- stdFunction (returnType manifoldType) vs
    castFunction <- staticCast (returnType manifoldType) (rs <> vs) mname
    let vs' = take
              (length vs)
              (map (\j -> "std::placeholders::_" <> viaShow j) ([1..] :: [Int]))
        rs' = map (bndNamer . argId) rs
        bindStr = stdBind $ castFunction : (rs' ++ vs')
    return $ appliedTypeStr <+> parens bindStr


  makeManifoldBody :: MDoc -> CppTranslator (Maybe MDoc)
  makeManifoldBody body = do
    manifoldHasBeenGenerated <- CMS.gets (Set.member callIndex . translatorManifoldSet)
    if manifoldHasBeenGenerated
      then return Nothing
      else do
        typestr <- showTypeF (returnType manifoldType)
        args <- mapM makeArg (manifoldArgs form)
        let decl = typestr <+> mname <> tupled args
        return . Just $ block 4 decl body

  returnType :: TypeF -> TypeF
  returnType (FunF _ t) = t
  returnType t = t


stdFunction :: TypeF -> [Arg TypeM] -> CppTranslator MDoc
stdFunction t args = do
  args' <- mapM argTypeM args
  let argList = cat (punctuate "," args')
  typestr <- showTypeF t
  return [idoc|std::function<#{typestr}(#{argList})>|]

stdBind :: [MDoc] -> MDoc
stdBind xs = [idoc|std::bind(#{args})|] where
  args = cat (punctuate "," xs)

staticCast :: TypeF -> [Arg TypeM] -> MDoc -> CppTranslator MDoc
staticCast t args name' = do
  output <- showTypeF t
  inputs <- mapM argTypeM args
  let argList = cat (punctuate "," inputs)
  return [idoc|static_cast<#{output}(*)(#{argList})>(&#{name'})|]

argTypeM :: Arg TypeM -> CppTranslator MDoc
argTypeM (Arg _ (Serial _)) = return serialType
argTypeM (Arg _ (Native c)) = showTypeF c
argTypeM (Arg _ Passthrough) = return serialType

makeDispatch :: [SerialManifold] -> MDoc
makeDispatch ms = block 4 "switch(std::stoi(argv[1]))" (vsep (map makeCase ms))
  where
    makeCase :: SerialManifold -> MDoc
    makeCase (SerialManifold i _ form _) =
      let args' = take (length (manifoldArgs form)) $ map (\j -> "argv[" <> viaShow j <> "]") ([2..] :: [Int])
      in
        (nest 4 . vsep)
          [ "case" <+> viaShow i <> ":"
          , "__mlc_result__ = " <> manNamer i <> tupled args' <> ";"
          , "break;"
          ]

typeParams :: [(Maybe TypeF, TypeF)] -> CppTranslator MDoc
typeParams ts = do
  ds <- mapM showTypeF [t | (Nothing, t) <- ts] 
  return $
    if null ds
      then ""
      else encloseSep "<" ">" "," ds

showTypeF :: TypeF -> CppTranslator MDoc
showTypeF (UnkF (FV _ x)) = return $ pretty x 
showTypeF (VarF (FV _ x)) = return $ pretty x
showTypeF (FunF ts t) = do
  t' <- showTypeF t
  ts' <- mapM showTypeF ts
  return $ "std::function<" <> t' <> tupled ts' <> ">"
showTypeF (AppF t ts) = do
  t' <- showTypeF t
  ts' <- mapM showTypeF ts
  return . pretty $ expandMacro (render t') (map render ts')
showTypeF (NamF _ (FV gc "struct") _ rs) = do
  recmap <- CMS.gets translatorRecmap
  -- handle autogenerated structs
  case lookup (FV gc "struct", map fst rs) recmap of
    (Just rec) -> do
      params <- typeParams (zip (map snd (recFields rec)) (map snd rs))
      return $ recName rec <> params
    Nothing -> error "Should not happen"
showTypeF (NamF _ (FV _ s) ps _) = do
  ps' <- mapM showTypeF ps
  return $ pretty s <> encloseSep "<" ">" "," ps'


collectRecords :: SerialManifold -> [(FVar, GIndex, [(FVar, TypeF)])]
collectRecords e0@(SerialManifold i0 _ _ _) = CMS.evalState (foldSerialManifoldM fm e0) i0 where
  fm = FoldManifoldM
    { opSerialManifoldM = serialManifold
    , opNativeManifoldM = nativeManifold
    , opSerialExprM = return . foldlSE (<>) []
    , opNativeExprM = nativeExpr
    , opSerialArgM = return . foldlSA (<>) []
    , opNativeArgM = return . foldlNA (<>) []
    }

  serialManifold (SerialManifold_ i _ _ x) = CMS.put i >> return x

  nativeManifold (NativeManifold_ i _ _ (_, x)) = CMS.put i >> return x

  nativeExpr e@(RecordN_ _ v _ rs) = do
    m <- CMS.get
    let entry = (v, m, [(key, valType) | (key, (valType, _)) <- rs])
    return $ entry : foldlNE (<>) [] e
  nativeExpr x = return . foldlNE (<>) [] $ x

-- unify records with the same name/keys
unifyRecords
  :: [(FVar -- The "v" in (NamP _ v@(PV _ _ "struct") _ rs)
     , GIndex
     , [(FVar, TypeF)]) -- key/type terms for this record
     ] -> RecMap
unifyRecords xs
  = zipWith (\i ((v,ks),es) -> ((v,ks), RecEntry (structName i v) es)) [1..]
  . map (\((v,m,ks), rss) -> ((v,ks), [unifyField m fs | fs <- transpose rss]))
  . map (\((v,ks), rss) -> ((v, fst (head rss),ks), map snd rss))
  -- [((record_name, record_keys), [(GIndex, [(key,type)])])]
  -- associate unique pairs of record name and keys with their edge types
  . groupSort
  . unique
  $ [((v, map fst es), (m, es)) | (v, m, es) <- xs]

structName :: Int -> FVar -> MDoc
structName i (FV v "struct") = "mlc_" <> pretty v <> "_" <> pretty i
structName _ (FV _ v) = pretty v

unifyField :: GIndex -> [(FVar, TypeF)] -> (FVar, Maybe TypeF)
unifyField _ [] = error "Empty field"
unifyField _ rs@((v,_):_)
  | not (all ((== v) . fst) rs)
      = error $ "Bad record - unequal fields: " <> show (unique rs)
  | otherwise = case unique (map snd rs) of
      [t] -> (v, Just t)
      _ -> (v, Nothing)

generateAnonymousStructs :: CppTranslator ([MDoc],[MDoc])
generateAnonymousStructs = do
  recmap <- CMS.gets translatorRecmap

  xs <- mapM makeSerializers (reverse . map snd $ recmap)

  return (concatMap fst xs, concatMap snd xs)

  where

  makeSerializers :: RecEntry -> CppTranslator ([MDoc],[MDoc])
  makeSerializers rec = do

    let templateTerms = map (("T" <>) . pretty) ([1..] :: [Int])
        rs' = zip templateTerms (recFields rec)

    let params = [t | (t, (_, Nothing)) <- rs']
        rname = recName rec
        rtype = rname <> recordTemplate [v | (v, (_, Nothing)) <- rs']

    let fieldNames = [k | (_, (FV _ k, _)) <- rs'] 

    fieldTypes <- mapM (\(t, v) -> maybeM t showTypeF v) [(t', v') | (t', (_, v')) <- rs']

    let fields = [(pretty k, v) | (k, v) <- zip fieldNames fieldTypes]

    let structDecl = structTypedefTemplate params rname fields
        serialDecl = serialHeaderTemplate params rtype
        deserialDecl = deserialHeaderTemplate params rtype
        serializer = serializerTemplate params rtype fields
        deserializer = deserializerTemplate False params rtype fields
    
    return ([structDecl, serialDecl, deserialDecl], [serializer, deserializer])

  -- monadic form of `maybe` function
  maybeM :: Monad m => a -> (b -> m a) -> Maybe b -> m a
  maybeM _ f (Just x) = f x
  maybeM x _ Nothing = return x


generateSourcedSerializers :: [SerialManifold] -> MorlocMonad ([MDoc],[MDoc])
generateSourcedSerializers es0 = do
  typemap <- Map.unions <$> mapM (foldSerialManifoldM fm) es0
  return $ foldl groupQuad ([],[]) . Map.elems . Map.mapMaybeWithKey makeSerial $ typemap
  where

    fm = FoldManifoldM
      { opSerialManifoldM = \(SerialManifold_ i _ _     e ) -> Map.union <$> MM.metaTypedefs i <*> pure e
      , opNativeManifoldM = \(NativeManifold_ i _ _ (_, e)) -> Map.union <$> MM.metaTypedefs i <*> pure e
      , opSerialExprM = return . foldlSE Map.union Map.empty
      , opNativeExprM = return . foldlNE Map.union Map.empty
      , opSerialArgM = return . foldlSA Map.union Map.empty
      , opNativeArgM = return . foldlNA Map.union Map.empty
      }

    groupQuad :: ([a],[a]) -> (a, a, a, a) -> ([a],[a])
    groupQuad (xs,ys) (x1, y1, x2, y2) = (x1:x2:xs, y1:y2:ys)

    makeSerial :: TVar -> (Type, [TVar]) -> Maybe (MDoc, MDoc, MDoc, MDoc)
    makeSerial _ (NamT _ (TV _ "struct") _ _, _) = Nothing
    makeSerial (TV (Just CppLang) _) (NamT r (TV _ v) _ rs, ps)
      = Just (serialDecl, serializer, deserialDecl, deserializer) where

        templateTerms = ["T" <> pretty p | (TV _ p) <- ps]

        params = map (\p -> "T" <> pretty (unTVar p)) ps
        rtype = pretty v <> recordTemplate templateTerms
        fields = [(pretty k, showDefType ps t) | (k, t) <- rs]

        serialDecl = serialHeaderTemplate params rtype
        deserialDecl = deserialHeaderTemplate params rtype

        serializer = serializerTemplate params rtype fields

        deserializer = deserializerTemplate (r == NamObject) params rtype fields
    makeSerial _ _ = Nothing

    showDefType :: [TVar] -> Type -> MDoc
    showDefType ps (UnkT v@(TV _ s))
      | v `elem` ps = "T" <> pretty s
      | otherwise = pretty s
    showDefType ps (VarT v@(TV _ s))
      | v `elem` ps = "T" <> pretty s
      | otherwise = pretty s
    showDefType _ (FunT _ _) = error "Cannot serialize functions"
    showDefType _ (NamT _ (TV _ _) _ _)
      = undefined -- pretty v <> encloseSep "<" ">" "," (map (showDefType ps) ts)
    showDefType ps (AppT (VarT (TV _ v)) ts) = pretty $ expandMacro v (map (render . showDefType ps) ts)
    showDefType _ (AppT _ _) = error "AppT is only OK with VarT, for now"

makeTemplateHeader :: [MDoc] -> MDoc
makeTemplateHeader [] = ""
makeTemplateHeader ts = "template" <+> encloseSep "<" ">" "," ["class" <+> t | t <- ts]

recordTemplate :: [MDoc] -> MDoc
recordTemplate [] = ""
recordTemplate ts = encloseSep "<" ">" "," ts



-- Example
-- > template <class T>
-- > struct Person
-- > {
-- >     std::vector<std::string> name;
-- >     std::vector<T> info;
-- > };
structTypedefTemplate
  :: [MDoc] -- template parameters (e.g., ["T"])
  -> MDoc -- the name of the structure (e.g., "Person")
  -> [(MDoc, MDoc)] -- key and type for all fields
  -> MDoc -- structure definition
structTypedefTemplate params rname fields = vsep [template, struct] where
  template = makeTemplateHeader params
  struct = block 4 ("struct" <+> rname)
                   (vsep [t <+> k <> ";" | (k,t) <- fields]) <> ";"



-- Example
-- > template <class T>
-- > std::string serialize(person<T> x, person<T> schema);
serialHeaderTemplate :: [MDoc] -> MDoc -> MDoc
serialHeaderTemplate params rtype = vsep [template, prototype]
  where
  template = makeTemplateHeader params
  prototype = [idoc|std::string serialize(#{rtype} x, #{rtype} schema);|]



-- Example:
-- > template <class T>
-- > bool deserialize(const std::string json, size_t &i, person<T> &x);
deserialHeaderTemplate :: [MDoc] -> MDoc -> MDoc
deserialHeaderTemplate params rtype = vsep [template, prototype]
  where
  template = makeTemplateHeader params
  prototype = [idoc|bool deserialize(const std::string json, size_t &i, #{rtype} &x);|]



serializerTemplate
  :: [MDoc] -- template parameters
  -> MDoc -- type of thing being serialized
  -> [(MDoc, MDoc)] -- key and type for all fields
  -> MDoc -- output serializer function
serializerTemplate params rtype fields = [idoc|
#{makeTemplateHeader params}
std::string serialize(#{rtype} x, #{rtype} schema){
    #{schemata}
    std::ostringstream json;
    json << "{" << #{align $ vsep (punctuate " << ',' <<" writers)} << "}";
    return json.str();
}
|] where
  schemata = align $ vsep (map (\(k,t) -> t <+> k <> "_" <> ";") fields)
  writers = map (\(k,_) -> dquotes ("\\\"" <> k <> "\\\"" <> ":")
          <+> "<<" <+> [idoc|serialize(x.#{k}, #{k}_)|] ) fields



deserializerTemplate
  :: Bool -- build object with constructor
  -> [MDoc] -- ^ template parameters
  -> MDoc -- ^ type of thing being deserialized
  -> [(MDoc, MDoc)] -- ^ key and type for all fields
  -> MDoc -- ^ output deserializer function
deserializerTemplate isObj params rtype fields
  = [idoc|
#{makeTemplateHeader params}
bool deserialize(const std::string json, size_t &i, #{rtype} &x){
    #{schemata}
    try {
        whitespace(json, i);
        if(! match(json, "{", i))
            throw 1;
        whitespace(json, i);
        #{fieldParsers}
        if(! match(json, "}", i))
            throw 1;
        whitespace(json, i);
    } catch (int e) {
        return false;
    }
    #{assign}
    return true;
}
|] where
  schemata = align $ vsep (map (\(k,t) -> t <+> k <> "_" <> ";") fields)
  fieldParsers = align $ vsep (punctuate parseComma (map (makeParseField . fst) fields))
  values = [k <> "_" | (k,_) <- fields]
  assign = if isObj
           then [idoc|#{rtype} y#{tupled values}; x = y;|]
           else let obj = encloseSep "{" "}" "," values
                in [idoc|#{rtype} y = #{obj}; x = y;|]

parseComma = [idoc|
if(! match(json, ",", i))
    throw 800;
whitespace(json, i);|]

makeParseField :: MDoc -> MDoc
makeParseField field = [idoc|
if(! match(json, "\"#{field}\"", i))
    throw 1;
whitespace(json, i);
if(! match(json, ":", i))
    throw 1;
whitespace(json, i);
if(! deserialize(json, i, #{field}_))
    throw 1;
whitespace(json, i);|]


makeMain :: [MDoc] -> [MDoc] -> [MDoc] -> [MDoc] -> MDoc -> MDoc
makeMain includes signatures serialization manifolds dispatch = [idoc|#include <string>
#include <iostream>
#include <sstream>
#include <functional>
#include <vector>
#include <string>
#include <algorithm> // for std::transform
using namespace std;

#{Src.foreignCallFunction}

#{vsep includes}

#{Src.serializationHandling}

#{vsep serialization}

#{vsep signatures}

#{vsep manifolds}

int main(int argc, char * argv[])
{
    #{serialType} __mlc_result__;
    #{dispatch}
    if(__mlc_result__ != "null"){
        std::cout << __mlc_result__ << std::endl;
    }
    return 0;
}
|]
