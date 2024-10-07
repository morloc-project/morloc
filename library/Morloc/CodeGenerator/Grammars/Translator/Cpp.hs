{-# LANGUAGE TemplateHaskell
  , QuasiQuotes
  , OverloadedStrings
  , ViewPatterns
  , FlexibleContexts
  , FlexibleInstances
  , UndecidableInstances
#-}

{-|
Module      : Morloc.CodeGenerator.Grammars.Translator.Cpp
Description : C++ translator
Copyright   : (c) Zebulun Arendsee, 2016-2024
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
                                   , serialAstToType
                                   , shallowType
                                   )
import Morloc.CodeGenerator.Grammars.Common
import Morloc.DataFiles as DF
import Morloc.Data.Doc
import Morloc.Quasi
import Morloc.CodeGenerator.Grammars.Macro (expandMacro)
import qualified Morloc.Monad as MM
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Morloc.Module as Mod
import qualified Morloc.Language as ML
import qualified Control.Monad.State as CMS
import qualified Morloc.TypeEval as TE
import qualified Morloc.Data.Text as MT
import Control.Monad.Identity (Identity)

data CallSemantics = Copy | Reference

class HasCppType a where
  cppTypeOf :: a -> CppTranslator MDoc

  cppArgOf :: CallSemantics -> Arg a -> CppTranslator MDoc


setCallSemantics :: CallSemantics -> MDoc -> MDoc
setCallSemantics Copy typestr = typestr
setCallSemantics Reference typestr = "const" <+> typestr <> "&"

instance HasCppType TypeM where
  cppTypeOf (Serial _) = return serialType
  cppTypeOf (Native c) = cppTypeOf c
  cppTypeOf Passthrough = return serialType
  cppTypeOf (Function ts t) = do
    t' <- cppTypeOf t
    ts' <- mapM cppTypeOf ts
    return $ "std::function<" <> t' <> tupled ts' <> ">"

  cppArgOf (setCallSemantics -> setCall) = f where
    f (Arg i t@(Serial _)) = do
      t' <- cppTypeOf t
      return $ setCall t' <+> svarNamer i
    f (Arg i t@(Native _)) = do
      t' <- cppTypeOf t
      return $ setCall t' <+> nvarNamer i
    f (Arg i Passthrough) = return $ setCall serialType <+> svarNamer i
    f (Arg i t@(Function _ _)) = do
      t' <- cppTypeOf t
      return $ t' <+> nvarNamer i

instance HasCppType NativeManifold where
  cppTypeOf = cppTypeOf . typeMof
  cppArgOf s r = cppArgOf s $ fmap typeMof r

instance {-# OVERLAPPABLE #-} HasTypeF e => HasCppType e where
  cppTypeOf = f . typeFof where
    f (UnkF (FV _ x)) = return $ pretty x
    f (VarF (FV _ x)) = return $ pretty x
    f (FunF ts t) = do
      t' <- f t
      ts' <- mapM f ts
      return $ "std::function<" <> t' <> tupled ts' <> ">"
    f (AppF t ts) = do
      t' <- f t
      ts' <- mapM f ts
      return . pretty $ expandMacro (render t') (map render ts')
    f t@(NamF _ (FV gc (CV "struct")) _ rs) = do
      recmap <- CMS.gets translatorRecmap
      -- handle autogenerated structs
      case lookup (FV gc (CV "struct"), map fst rs) recmap of
        (Just rec) -> do
          params <- typeParams (zip (map snd (recFields rec)) (map snd rs))
          return $ recName rec <> params
        Nothing -> error $ "Record missing from recmap: " <> show t <> " from map: " <> show recmap
    f (NamF _ (FV _ s) ps _) = do
      ps' <- mapM f ps
      return $ pretty s <> recordTemplate ps'


  cppArgOf s (Arg i t) = do
    t' <- cppTypeOf (typeFof t)
    return $ setCallSemantics s t' <+> nvarNamer i


-- | @RecEntry@ stores the common name, keys, and types of records that are not
-- imported from C++ source. These records are generated as structs in the C++
-- pool. @unifyRecords@ takes all such records and "unifies" ones with the same
-- name and keys. The unified records may have different types, but they will
-- all be instances of the same generic struct. That is, any fields that differ
-- between instances will be made generic.
data RecEntry = RecEntry
  { recName :: MDoc -- ^ the automatically generated name for this anonymous type
  , recFields :: [( Key -- The field key
                  , Maybe TypeF -- The field type if not generic
                  )]
  }
  deriving (Show)

-- | @RecMap@ is used to lookup up the struct name shared by all records that are not imported from C++ source.
type RecMap = [((FVar, [Key]), RecEntry)]

data CppTranslatorState = CppTranslatorState
  { translatorCounter :: Int
  , translatorRecmap :: RecMap
  , translatorSignatureSet :: Set.Set Int
  , translatorManifoldSet :: Set.Set Int
  , translatorCurrentManifold :: Int
  }

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
  let cmd = SysRun . Code . render $ [idoc|g++ --std=c++17 -o #{outfile} #{src} #{hsep flags'} #{hsep incs}|]

  return [cmd]

serialType :: MDoc
serialType = pretty $ ML.serialType CppLang

makeSignature :: SerialManifold -> CppTranslator [MDoc]
makeSignature = foldWithSerialManifoldM fm where
  fm = defaultValue
    { opFoldWithSerialManifoldM = serialManifold
    , opFoldWithNativeManifoldM = nativeManifold
    }

  serialManifold (SerialManifold m _ form _) _ = manifoldSignature m serialType form

  nativeManifold e@(NativeManifold m _ form _) _ = do
    typestr <- cppTypeOf e
    manifoldSignature m typestr form

  manifoldSignature :: HasTypeM t => Int -> MDoc -> ManifoldForm (Or TypeS TypeF) t -> CppTranslator [MDoc]
  manifoldSignature i typestr form = do
    s <- CMS.get
    if Set.member i (translatorSignatureSet s)
      then return []
      else do
        let formArgs = typeMofForm form
        args <- mapM (cppArgOf Reference) formArgs
        CMS.put (s {translatorSignatureSet = Set.insert i (translatorSignatureSet s)})
        return [typestr <+> manNamer i <> tupled args <> ";"]

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
  typestr <- cppTypeOf $ serialAstToType s0

  -- TODO: I can remove the requirement for this schema term by adding a type
  -- annotation to the serialization function (I think)
  schemaIndex <- getCounter
  let schemaName = [idoc|#{helperNamer schemaIndex}_schema|]
      schema = [idoc|#{typestr} #{schemaName};|]
      final = [idoc|_put_value(serialize(#{x}, #{schemaName}))|]
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
      idx <- getCounter
      typestr <- cppTypeOf $ serialAstToType lst
      let v' = helperNamer idx
          idxStr = pretty idx
          decl = [idoc|#{typestr} #{v'};|]
      (x, before) <- serialize' [idoc|#{v}[i#{idxStr}]|] s
      let push = [idoc|#{v'}.push_back(#{x});|]
          loop  = block 4 [idoc|for(size_t i#{idxStr} = 0; i#{idxStr} < #{v}.size(); i#{idxStr}++)|]
                         (vsep (before ++ [push]))
      return (v', [decl, loop])

    construct v tup@(SerialTuple _ ss) = do
      (ss', befores) <- unzip <$> zipWithM (\i s -> serialize' (tupleKey i v) s) [0..] ss
      idx <- getCounter
      typestr <- cppTypeOf $ serialAstToType tup
      let v' = helperNamer idx
          x = [idoc|#{typestr} #{v'} = std::make_tuple#{tupled ss'};|]
      return (v', concat befores ++ [x]);

    construct v rec@(SerialObject NamRecord _ _ rs) = do
      (ss', befores) <- unzip <$> mapM (\(k, s) -> serialize' (recordAccess v (pretty k)) s) rs
      idx <- getCounter
      t <- cppTypeOf (serialAstToType rec)
      let v' = helperNamer idx
          decl = encloseSep "{" "}" "," ss'
          x = [idoc|#{t} #{v'} = #{decl};|]
      return (v', concat befores ++ [x]);

    construct _ (SerialObject NamObject _ _ _) = error "C++ object serialization not yet implemented"
    construct _ (SerialObject NamTable _ _ _) = error "C++ table serialization not yet implemented"

    construct _ _ = error "Unreachable"

-- reverse of serialize, parameters are the same
deserialize :: MDoc -> MDoc -> SerialAST -> CppTranslator (MDoc, [MDoc])
deserialize varname0 typestr0 s0
  | isSerializable s0 = do
      schemaVar <- helperNamer <$> getCounter
      let schemaName = [idoc|#{schemaVar}_schema|]
          schema = [idoc|#{typestr0} #{schemaName};|]
          term = [idoc|deserialize(_get_value(#{varname0}), #{schemaName})|]
      return (term, [schema])
  | otherwise = do
      schemaVar <- helperNamer <$> getCounter
      rawtype <- cppTypeOf $ serialAstToType s0
      rawvar <- helperNamer <$> getCounter
      let schemaName = [idoc|#{schemaVar}_schema|]
          schema = [idoc|#{rawtype} #{schemaName};|]
          deserializing = [idoc|#{rawtype} #{rawvar} = deserialize(_get_value(#{varname0}), #{schemaName});|]
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
      t <- cppTypeOf $ shallowType lst
      idx <- getCounter
      let v' = helperNamer idx
          idxStr = pretty idx
          decl = [idoc|#{t} #{v'};|]
      (x, before) <- check [idoc|#{v}[i#{idxStr}]|] s
      let push = [idoc|#{v'}.push_back(#{x});|]
          loop = block 4 [idoc|for(size_t i#{idxStr} = 0; i#{idxStr} < #{v}.size(); i#{idxStr}++)|]
                         (vsep (before ++ [push]))
      return (v', [decl, loop])

    construct v tup@(SerialTuple _ ss) = do
      (ss', befores) <- unzip <$> zipWithM (\i s -> check (tupleKey i v) s) [0..] ss
      typestr <- cppTypeOf $ shallowType tup
      v' <- helperNamer <$> getCounter
      let x = [idoc|#{typestr} #{v'} = std::make_tuple#{tupled ss'};|]
      return (v', concat befores ++ [x]);

    construct v rec@(SerialObject NamRecord _ _ rs) = do
      (ss', befores) <- mapAndUnzipM (\(k,s) -> check (recordAccess v (pretty k)) s) rs
      t <- cppTypeOf (shallowType rec)
      v' <- helperNamer <$> getCounter
      let decl = encloseSep "{" "}" "," ss'
          x = [idoc|#{t} #{v'} = #{decl};|]
      return (v', concat befores ++ [x]);

    construct _ _ = undefined -- TODO add support for deserialization of remaining types (e.g. other records)


translateSegment :: SerialManifold -> CppTranslator MDoc
translateSegment m0 = do
  resetCounter
  e <- surroundFoldSerialManifoldM manifoldIndexer foldRules m0
  return $ vsep . punctuate line $ poolPriorExprs e <> poolCompleteManifolds e
  where

  foldRules = FoldWithManifoldM
    { opFoldWithSerialManifoldM = makeSerialManifold
    , opFoldWithNativeManifoldM = makeNativeManifold
    , opFoldWithSerialExprM = makeSerialExpr
    , opFoldWithNativeExprM = makeNativeExpr
    , opFoldWithSerialArgM = makeSerialArg
    , opFoldWithNativeArgM = makeNativeArg
    }


  manifoldIndexer = makeManifoldIndexer (CMS.gets translatorCurrentManifold)
                                        (\i -> CMS.modify (\s -> s { translatorCurrentManifold = i}))

  makeSerialManifold :: SerialManifold -> SerialManifold_ PoolDocs -> CppTranslator PoolDocs
  makeSerialManifold sm (SerialManifold_ i _ form e) = makeManifold i form (typeMof sm) e

  makeNativeManifold :: NativeManifold -> NativeManifold_ PoolDocs -> CppTranslator PoolDocs
  makeNativeManifold nm (NativeManifold_ i _ form e) = makeManifold i form (typeMof nm) e

  makeSerialArg :: SerialArg -> SerialArg_ PoolDocs PoolDocs -> CppTranslator (TypeS, PoolDocs)
  makeSerialArg sr (SerialArgManifold_ x) = return (typeSof sr, x)
  makeSerialArg sr (SerialArgExpr_ x) = return (typeSof sr, x)

  makeNativeArg :: NativeArg -> NativeArg_ PoolDocs PoolDocs -> CppTranslator (TypeM, PoolDocs)
  makeNativeArg nr (NativeArgManifold_ x) = return (typeMof nr, x)
  makeNativeArg nr (NativeArgExpr_ x) = return (typeMof nr, x)

  makeSerialExpr :: SerialExpr -> SerialExpr_ PoolDocs PoolDocs PoolDocs (TypeS, PoolDocs) (TypeM, PoolDocs) -> CppTranslator PoolDocs
  makeSerialExpr _ (ManS_ e) = return e
  makeSerialExpr _ (AppPoolS_ _ (PoolCall mid (Socket _ _ socketFile) args) _) = do
    let bufDef = "std::ostringstream s;"
        argList = encloseSep "{" "}" ", " (map argNamer args)
        argsDef = [idoc|std::vector<std::string> args = #{argList};|]
        call = [idoc|foreign_call("#{socketFile}", "#{pretty mid}", args)|]
    return $ PoolDocs
      { poolCompleteManifolds = []
      , poolExpr = call
      , poolPriorLines = [bufDef, argsDef]
      , poolPriorExprs = []
      }
  makeSerialExpr _ (ReturnS_ e) = return $ e {poolExpr = "return(" <> poolExpr e <> ");"}
  makeSerialExpr _ (SerialLetS_ letIndex sa sb) = return $ makeLet svarNamer letIndex serialType sa sb
  makeSerialExpr (NativeLetS _ (typeFof -> t) _) (NativeLetS_ letIndex na sb) = do
    typestr <- cppTypeOf t
    return $ makeLet nvarNamer letIndex typestr na sb
  makeSerialExpr _ (LetVarS_ _ i) = return $ PoolDocs [] (svarNamer i) [] []
  makeSerialExpr _ (BndVarS_ _ i) = return $ PoolDocs [] (svarNamer i) [] []
  makeSerialExpr _ (SerializeS_ s e) = do
    se <- serialize (poolExpr e) s
    return $ mergePoolDocs (\_ -> poolExpr se) [e, se]
  makeSerialExpr _ _ = error "Unreachable"

  makeNativeExpr :: NativeExpr -> NativeExpr_ PoolDocs PoolDocs PoolDocs (TypeS, PoolDocs) (TypeM, PoolDocs) -> CppTranslator PoolDocs
  makeNativeExpr _ (AppSrcN_ _ src qs (map snd -> es)) = do
    templateStr <- templateArguments qs
    return $ mergePoolDocs ((<>) (pretty (srcName src) <> templateStr) . tupled) es
  makeNativeExpr _ (ManN_ call) = return call
  makeNativeExpr _ (ReturnN_ e) =
    return $ e {poolExpr = "return" <> parens (poolExpr e) <> ";"}
  makeNativeExpr _ (SerialLetN_ i sa nb) = return $ makeLet svarNamer i serialType sa nb
  makeNativeExpr (NativeLetN _ (typeFof -> t1) _) (NativeLetN_ i na nb) = makeLet nvarNamer i <$> cppTypeOf t1 <*> pure na <*> pure nb
  makeNativeExpr _ (LetVarN_ _ i) = return $ PoolDocs [] (nvarNamer i) [] []
  makeNativeExpr _ (BndVarN_ _ i) = return $ PoolDocs [] (nvarNamer i) [] []
  makeNativeExpr _ (DeserializeN_ t s e) = do
    typestr <- cppTypeOf t
    (deserialized, assignments) <- deserialize (poolExpr e) typestr s
    return $ e
      { poolExpr = deserialized
      , poolPriorLines = poolPriorLines e <> assignments
      }

  makeNativeExpr _ (AccN_ _ _ e k) =
    return (e {poolExpr = poolExpr e <> "." <> pretty k})

  makeNativeExpr _ (SrcN_ _ _) = undefined

  makeNativeExpr _ (ListN_ _ _ es) =
    return $ mergePoolDocs (encloseSep "{" "}" ",") es

  makeNativeExpr _ (TupleN_ _ es) =
    return $ mergePoolDocs ((<>) "std::make_tuple" . tupled) es

  makeNativeExpr e (RecordN_ _ _ _ rs) = do
    t <- cppTypeOf e
    idx <- getCounter
    let v' = "a" <> pretty idx
        decl = t <+> v' <+> encloseSep "{" "}" "," (map (poolExpr . snd) rs) <> ";"
    let p = mergePoolDocs (const v') (map snd rs)
    return (p {poolPriorLines = poolPriorLines p <> [decl]})

  makeNativeExpr _ (LogN_         _ x) = return (PoolDocs [] (if x then "true" else "false") [] [])
  makeNativeExpr _ (RealN_        _ x) = return (PoolDocs [] (viaShow x) [] [])
  makeNativeExpr _ (IntN_         _ x) = return (PoolDocs [] (viaShow x) [] [])
  makeNativeExpr _ (StrN_         _ x) = return (PoolDocs [] [idoc|std::string("#{pretty x}")|] [] [])
  makeNativeExpr _ (NullN_        _  ) = return (PoolDocs [] "null" [] [])
  makeNativeExpr _ _ = error "Unreachable"

  templateArguments :: [(MT.Text, TypeF)] -> CppTranslator MDoc
  templateArguments [] = return ""
  templateArguments qs = do
    ts <- mapM (cppTypeOf . snd) qs
    return $ recordTemplate ts


  makeLet :: (Int -> MDoc) -> Int -> MDoc -> PoolDocs -> PoolDocs -> PoolDocs
  makeLet namer letIndex typestr (PoolDocs ms1 e1 rs1 pes1) (PoolDocs ms2 e2 rs2 pes2) =
    let letAssignment = [idoc|#{typestr} #{namer letIndex} = #{e1};|]
        rs = rs1 <> [ letAssignment ] <> rs2 <> [e2]
    in PoolDocs
      { poolCompleteManifolds = ms1 <> ms2
      , poolExpr = vsep rs
      , poolPriorLines = []
      , poolPriorExprs = pes1 <> pes2
      }

makeManifold
  :: (HasTypeM t)
  => Int -- ^ The index of the manifold that is being created
  -> ManifoldForm (Or TypeS TypeF) t
  -> TypeM -- ^ The type of the manifold (usually a function, serialized terms are of general type "Str" and C++ type "std::string"
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

  makeManifoldCall :: ManifoldForm (Or TypeS TypeF) t -> CppTranslator MDoc
  makeManifoldCall (ManifoldFull rs) = do
    let args = map argNamer (typeMofRs rs)
    return $ mname <> tupled args
  makeManifoldCall (ManifoldPass _) = return mname
  makeManifoldCall (ManifoldPart rs vs) = do
    let vs' = take
              (length vs)
              (map (\j -> "std::placeholders::_" <> viaShow j) ([1..] :: [Int]))
        rs' = map argNamer (typeMofRs rs)
        bindStr = stdBind $ mname : (rs' ++ vs')
    return bindStr

  makeManifoldBody :: MDoc -> CppTranslator (Maybe MDoc)
  makeManifoldBody body = do
    state <- CMS.get
    let manifoldHasBeenGenerated = Set.member callIndex (translatorManifoldSet state)
    if manifoldHasBeenGenerated
      then return Nothing
      else do
        CMS.put $ state {translatorManifoldSet = Set.insert callIndex (translatorManifoldSet state)}
        returnTypeStr <- returnType manifoldType
        let argList = typeMofForm form
        args <- mapM (cppArgOf Reference) argList
        let decl = returnTypeStr <+> mname <> tupled args
        let tryBody = block 4 "try" body
            throwStatement = vsep
              [ [idoc|std::string error_message = "Error in m#{pretty callIndex} " + std::string(e.what());|]
              , [idoc|log_message(error_message);|]
              , [idoc|throw std::runtime_error(error_message);|]
              ]
            catchBody = block 4 "catch (const std::exception& e)" throwStatement
            tryCatchBody = tryBody <+> catchBody
        return . Just . block 4 decl . vsep $
          [ {- can add diagnostic statements here -}
            [idoc|log_message("Entering #{pretty callIndex}");|]
          , tryCatchBody
          ]
  returnType :: TypeM -> CppTranslator MDoc
  returnType (Function _ t) = cppTypeOf t
  returnType t = cppTypeOf t

stdBind :: [MDoc] -> MDoc
stdBind xs = [idoc|std::bind(#{args})|] where
  args = cat (punctuate "," xs)

makeDispatch :: [SerialManifold] -> MDoc
makeDispatch ms = block 4 "switch(std::stoi(args[0]))" (vsep (map makeCase ms <> [defaultCase]))
  where
    makeCase :: SerialManifold -> MDoc
    makeCase (SerialManifold i _ form _) =
      -- this made more sense when I was using ArgTypes
      -- it may make sense yet again when I switch to Or
      let size = sum $ abilist (\_ _ -> 1) (\_ _ -> 1) form
          args' = take size $ map (\j -> "args[" <> viaShow j <> "]") ([1..] :: [Int])
      in
        (nest 4 . vsep)
          [ "case" <+> viaShow i <> ":"
          , "result = " <> manNamer i <> tupled args' <> ";"
          , "break;"
          ]

    defaultCase =
        (nest 4 . vsep)
          [ "default:"
          , [idoc|log_message("Manifold id not found");|]
          , "break;"
          ]

typeParams :: [(Maybe TypeF, TypeF)] -> CppTranslator MDoc
typeParams ts = recordTemplate <$> mapM cppTypeOf [t | (Nothing, t) <- ts]

collectRecords :: SerialManifold -> [(FVar, Int, [(Key, TypeF)])]
collectRecords e0@(SerialManifold i0 _ _ _)
  = unique $ CMS.evalState (surroundFoldSerialManifoldM manifoldIndexer fm e0) i0
  where
    fm = defaultValue { opFoldWithNativeExprM = nativeExpr, opFoldWithSerialExprM = serialExpr }

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
    seekRecs m (FunF ts t) = concatMap (seekRecs m) (t:ts)
    seekRecs m (AppF t ts) = concatMap (seekRecs m) (t:ts)
    seekRecs _ (UnkF _) = []
    seekRecs _ (VarF _) = []


-- unify records with the same name/keys
unifyRecords
  :: [(FVar -- The "v" in (NamP _ v@(PV _ _ "struct") _ rs)
     , Int -- general index
     , [(Key, TypeF)]) -- key/type terms for this record
     ] -> RecMap
unifyRecords xs
  = zipWith (\i ((v,ks),es) -> ((v,ks), RecEntry (structName i v) es)) [1..]
  . map (\((v, ks), rss) -> ((v, ks), map unifyField (transpose (map snd rss))))
  -- associate unique pairs of record name and keys with their edge types
  . groupSort
  . unique
  $ [((v, map fst es), (m, es)) | (v, m, es) <- xs]

structName :: Int -> FVar -> MDoc
structName i (FV v (CV "struct")) = "mlc_" <> pretty v <> "_" <> pretty i
structName _ (FV _ v) = pretty v

unifyField :: [(Key, TypeF)] -> (Key, Maybe TypeF)
unifyField [] = error "Empty field"
unifyField rs@((v,_):_)
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

    let fieldNames = [k | (_, (k, _)) <- rs']

    fieldTypes <- mapM (\(t, v) -> maybeM t cppTypeOf v) [(t', v') | (t', (_, v')) <- rs']

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


generateSourcedSerializers
  :: [SerialManifold] -- all segments that can be called in this pool
  -> MorlocMonad ( [MDoc]
                 , [MDoc]
                 )
generateSourcedSerializers es0 = do
  typedef <- Map.unions <$> mapM (foldSerialManifoldM fm) es0

  scopeMap <- MM.gets stateUniversalConcreteTypedefs

  scope <- case Map.lookup CppLang scopeMap of
    (Just scope) -> return scope
    Nothing -> return Map.empty


  MM.sayVVV $ "typedef:" <+> viaShow typedef

  return $ foldl groupQuad ([],[]) . Map.elems . Map.mapMaybeWithKey (makeSerial scope) $ typedef
  where

    fm = defaultValue
      { opSerialManifoldM = \(SerialManifold_ i _ _ e) -> Map.union <$> MM.metaTypedefs i CppLang <*> pure e
      , opNativeManifoldM = \(NativeManifold_ i _ _ e) -> Map.union <$> MM.metaTypedefs i CppLang <*> pure e
      }

    groupQuad :: ([a],[a]) -> (a, a, a, a) -> ([a],[a])
    groupQuad (xs,ys) (x1, y1, x2, y2) = (x1:x2:xs, y1:y2:ys)

    makeSerial :: Scope -> TVar -> ([TVar], TypeU, Bool) -> Maybe (MDoc, MDoc, MDoc, MDoc)
    makeSerial _ _ (_, NamU _ (TV "struct") _ _, _) = Nothing
    makeSerial scope _ (ps, NamU r (TV v) _ rs, _)
      = Just (serialDecl, serializer, deserialDecl, deserializer) where

        templateTerms = ["T" <> pretty p | p <- ps]

        params = map (\p -> "T" <> pretty (unTVar p)) ps
        rtype = pretty v <> recordTemplate templateTerms

        rs' = map (second (evaluateTypeU scope)) rs

        fields = [(pretty k, showDefType ps (typeOf t)) | (k, t) <- rs']

        serialDecl = serialHeaderTemplate params rtype
        deserialDecl = deserialHeaderTemplate params rtype

        serializer = serializerTemplate params rtype fields

        deserializer = deserializerTemplate (r == NamObject) params rtype fields
    makeSerial _ _ _ = Nothing

    evaluateTypeU :: Scope -> TypeU -> TypeU
    evaluateTypeU scope t = case TE.evaluateType scope t of
      (Left e) -> error $ show e
      (Right t') -> t'

    showDefType :: [TVar] -> Type -> MDoc
    showDefType ps (UnkT v)
      | v `elem` ps = "T" <> pretty v
      | otherwise = pretty v
    showDefType ps (VarT v)
      | v `elem` ps = "T" <> pretty v
      | otherwise = pretty v
    showDefType _ (FunT _ _) = error "Cannot serialize functions"
    showDefType _ NamT{}
      = undefined -- pretty v <> encloseSep "<" ">" "," (map (showDefType ps) ts)
    showDefType ps (AppT (VarT (TV v)) ts) = pretty $ expandMacro v (map (render . showDefType ps) ts)
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

parseComma :: Doc ann
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
#{srcPreamble langSrc}

#{vsep includes}

#{srcSerialization langSrc}

#{vsep serialization}

#{srcInterop langSrc}

#{vsep signatures}

#{vsep manifolds}

Message dispatch(const Message& msg){

    Header header = read_header(msg.data);

    int mid = read_int(header.command, 1, 4);

    std::vector<Message> args;

    char* data_ptr = msg.data + 32 + header.offset;

    Header arg_header;

    while(data_ptr - msg.data < msg.length){
        arg_header = read_header(data_ptr);
        Message arg;
        arg.data = data_ptr;
        arg.length = 32 + arg_header.offset + arg_header.length;
        args.push_back(arg);
    }

    #{dispatch}

    std::string errmsg = "Call failed";
    return make_callret(errmsg.c_str(), errmsg.size(), false);
}



#{srcMain langSrc}
|]
  where
    langSrc = DF.languageFiles CppLang
