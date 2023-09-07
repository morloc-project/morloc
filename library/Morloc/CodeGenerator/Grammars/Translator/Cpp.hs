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
preprocess = invertSerialManifold

translate :: [Source] -> [SerialManifold] -> MorlocMonad Script
translate srcs es = do
  -- -- diagnostics
  -- liftIO . putDoc . vsep $ "-- C++ translation --" : map pretty es

  let recmap = unifyRecords . concatMap collectRecords $ es
      translatorState = CppTranslatorState 0 recmap Set.empty Set.empty
      code = CMS.evalState (makeCppCode srcs es) translatorState

  maker <- makeTheMaker srcs

  return $ Script
    { scriptBase = "pool"
    , scriptLang = CppLang
    , scriptCode = "." :/ File "pool.cpp" (Code . render $ code)
    , scriptMake = maker
    }

makeCppCode :: [Source] -> [SerialManifold] -> CppTranslator MDoc
makeCppCode srcs es = do

  -- generate code for serialization
  (srcDecl, srcSerial) <- generateSourcedSerializers es

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
    , opSerialExprM = return . foldl (<>) []
    , opNativeExprM = return . foldl (<>) []
    , opSerialArgM = return . foldl (<>) []
    , opNativeArgM = return . foldl (<>) []
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
  let schemaName = "KILL_ME_schema" -- I can remove the requirement for this schema term by adding a type annotation to the serialization function (I think)
      schema = [idoc|#{typestr} #{schemaName};|]
      final = [idoc|serialize(#{x}, #{schemaName});|]
  return $ PoolDocs
      { poolCompleteManifolds = []
      , poolExpr = final
      , poolPriorLines = before <> [schemaName, schema]
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
deserialize :: MDoc -> SerialAST -> CppTranslator (MDoc, [MDoc])
deserialize e _ = return ("DESERIALIZE" <> parens e, ["ASSHAT1", "ASSHAT2"])
-- deserialize :: Int -> MDoc -> MDoc -> SerialAST One -> CppTranslator [MDoc]
-- deserialize letIndex typestr0 varname0 s0
--   | isSerializable s0 = do
--       let schemaName = [idoc|#{letNamer letIndex}_schema|]
--           schema = [idoc|#{typestr0} #{schemaName};|]
--           deserializing = [idoc|#{typestr0} #{letNamer letIndex} = deserialize(#{varname0}, #{schemaName});|]
--       return [schema, deserializing]
--   | otherwise = do
--       idx <- fmap pretty getCounter
--       rawtype <- showTypeF $ serialAstToType s0
--       let schemaName = [idoc|#{letNamer letIndex}_schema|]
--           rawvar = "s" <> idx
--           schema = [idoc|#{rawtype} #{schemaName};|]
--           deserializing = [idoc|#{rawtype} #{rawvar} = deserialize(#{varname0}, #{schemaName});|]
--       (x, before) <- construct rawvar s0
--       let final = [idoc|#{typestr0} #{letNamer letIndex} = #{x};|]
--       return ([schema, deserializing] ++ before ++ [final])
--
--   where
--     check :: MDoc -> SerialAST One -> CppTranslator (MDoc, [MDoc])
--     check v s
--       | isSerializable s = return (v, [])
--       | otherwise = construct v s
--
--     construct :: MDoc -> SerialAST One -> CppTranslator (MDoc, [MDoc])
--     construct v (SerialPack _ (One (p, s'))) = do
--       (x, before) <- check v s'
--       let packer = pretty . srcName . unOne . typePackerForward $ p
--           deserialized = [idoc|#{packer}(#{x})|]
--       return (deserialized, before)
--
--     construct v lst@(SerialList _ s) = do
--       idx <- fmap pretty getCounter
--       t <- showTypeF $ shallowType lst
--       let v' = "s" <> idx
--           decl = [idoc|#{t} #{v'};|]
--       (x, before) <- check [idoc|#{v}[i#{idx}]|] s
--       let push = [idoc|#{v'}.push_back(#{x});|]
--           loop = block 4 [idoc|for(size_t i#{idx} = 0; i#{idx} < #{v}.size(); i#{idx}++)|]
--                          (vsep (before ++ [push]))
--       return (v', [decl, loop])
--
--     construct v tup@(SerialTuple _ ss) = do
--       idx <- fmap pretty getCounter
--       (ss', befores) <- unzip <$> zipWithM (\i s -> check (tupleKey i v) s) [0..] ss
--       typestr <- showTypeF $ shallowType tup
--       let v' = "s" <> idx
--           x = [idoc|#{typestr} #{v'} = std::make_tuple#{tupled ss'};|]
--       return (v', concat befores ++ [x]);
--
--     construct v rec@(SerialObject NamRecord _ _ rs) = do
--       idx <- fmap pretty getCounter
--       (ss', befores) <- mapAndUnzipM (\(FV _ k,s) -> check (recordAccess v (pretty k)) s) rs
--       t <- showTypeF (shallowType rec)
--       let v' = "s" <> idx
--           decl = encloseSep "{" "}" "," ss'
--           x = [idoc|#{t} #{v'} = #{decl};|]
--       return (v', concat befores ++ [x]);
--
--     construct _ s = MM.throwError . SerializationError . render
--       $ "deserializeDescend: " <> prettySerialOne s


-- translateManifold :: RecMap -> Set.Set Int -> SerialManifold -> MorlocMonad (Set.Set Int, MDoc)
translateSegment :: SerialManifold -> CppTranslator MDoc
translateSegment m0 = do
  resetCounter
  e <- foldSerialManifoldM fm m0
  return $ vsep . punctuate line $ poolPriorExprs e <> poolCompleteManifolds e
  where
  fm = FoldManifoldM
    { opSerialManifoldM = makeSerialManifold
    , opNativeManifoldM = makeNativeManifold
    , opSerialExprM = makeSerialExpr
    , opNativeExprM = makeNativeExpr
    , opSerialArgM = makeSerialArg
    , opNativeArgM = makeNativeArg
    }

  makeSerialManifold :: SerialManifold_ PoolDocs -> CppTranslator PoolDocs
  makeSerialManifold (SerialManifold_ i _ form e) = makeManifold i form e

  makeNativeManifold :: NativeManifold_ PoolDocs -> CppTranslator PoolDocs
  makeNativeManifold (NativeManifold_ i _ form (_, e)) = makeManifold i form e

  makeSerialArg :: SerialArg_ PoolDocs -> CppTranslator PoolDocs
  makeSerialArg (SerialArgManifold_ x) = return x
  makeSerialArg (SerialArgExpr_ x) = return x

  makeNativeArg :: NativeArg_ PoolDocs -> CppTranslator PoolDocs
  makeNativeArg (NativeArgManifold_ x) = return x
  makeNativeArg (NativeArgExpr_ x) = return x

  makeSerialExpr :: SerialExpr_ PoolDocs -> CppTranslator PoolDocs
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
  makeSerialExpr (ReturnS_ e) = return $ e {poolExpr = "return(" <> poolExpr e <> ")"}
  makeSerialExpr (SerialLetS_ letIndex sa sb) = return $ makeLet letIndex serialType sa sb
  makeSerialExpr (NativeLetS_ letIndex (t, na) sb) = do
    typestr <- showTypeF t
    return $ makeLet letIndex typestr na sb
  makeSerialExpr (LetVarS_ i) = return $ PoolDocs [] (letNamer i) [] []
  makeSerialExpr (BndVarS_ i) = return $ PoolDocs [] (bndNamer i) [] []
  makeSerialExpr (SerializeS_ s e) = do
    se <- serialize (poolExpr e) s 
    return $ mergePoolDocs (\_ -> poolExpr se) [e, se]

  makeNativeExpr :: NativeExpr_ PoolDocs -> CppTranslator PoolDocs
  makeNativeExpr (AppSrcN_      _ src es) =
    return $ mergePoolDocs ((<>) (pretty $ srcName src) . tupled) es
  makeNativeExpr (AppManN_      _ call (map catEither -> xs)) =
    return $ mergePoolDocs ((<>) (poolExpr call) . tupled) (call : xs)
  makeNativeExpr (ReturnN_      _ e) =
    return $ e {poolExpr = "return" <> parens (poolExpr e)} 
  makeNativeExpr (SerialLetN_   i sa (_, nb)) = return $ makeLet i serialType sa nb
  makeNativeExpr (NativeLetN_   i (t1, na) (_, nb)) = makeLet i <$> showTypeF t1 <*> pure na <*> pure nb
  makeNativeExpr (LetVarN_      _ i) = return $ PoolDocs [] (letNamer i) [] []
  makeNativeExpr (BndVarN_      _ i) = return $ PoolDocs [] (bndNamer i) [] []
  makeNativeExpr (DeserializeN_ t s e) = do
    (deserialized, assignments) <- deserialize (poolExpr e) s
    return $ e
      { poolExpr = deserialized
      , poolPriorLines = poolPriorLines e <> assignments
      }

  makeNativeExpr (AccN_ _ _ _ e k) =
    return (e {poolExpr = poolExpr e <> "." <> pretty k})

  makeNativeExpr (SrcN_         t src) = undefined

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
    let letAssignment = [idoc|#{typestr} #{letNamer letIndex} = #{e1}|]
        rs = rs1 <> [ letAssignment ] <> rs2 <> [e2]
    in PoolDocs
      { poolCompleteManifolds = ms1 <> ms2
      , poolExpr = vsep rs
      , poolPriorLines = []
      , poolPriorExprs = pes1 <> pes2
      }

  makeManifold :: Int -> ManifoldForm TypeM -> PoolDocs -> CppTranslator PoolDocs
  makeManifold = undefined
    -- -- -- If this manifold has already been evaluated, just generate the call to it
    -- -- | Set.member i done = do
    -- --     let mname = manNamer i
    -- --     call <- case form of
    -- --       (ManifoldFull rs) -> return $ mname <> tupled (map (bndNamer . argId) rs)
    -- --       (ManifoldPass _) -> return mname
    -- --       (ManifoldPart _ _) -> return $ mname <> "_fun"
    -- --     return (done, PoolDocs
    -- --       { poolCompleteManifolds = []
    -- --       , poolExpr = call
    -- --       , poolPriorLines = []
    -- --       , poolPriorExprs = []
    -- --       })
    -- -- -- Otherwise, generate a signature for the manifold and all children
    -- -- | otherwise = do
    -- --     let args = manifoldArgs form
    -- --     (d2, PoolDocs ms' body ps1 pes1) <- f (Set.insert i done) args e
    -- --     let t = typeOfExprM e
    -- --         decl = showTypeM recmap t <+> manNamer i <> tupled (map (makeArg recmap) args)
    -- --         mdoc = block 4 decl body
    -- --         mname = manNamer i
    -- --     (call, ps2) <- case form of
    -- --       (ManifoldFull rs) -> return (mname <> tupled (map (bndNamer . argId) rs), [])
    -- --       (ManifoldPass _) -> return (mname, [])
    -- --       (ManifoldPart rs vs) -> do
    -- --         let v = mname <> "_fun"
    -- --
    -- --         MM.sayVVV $ "ManifoldPart" <+> v <> ":"
    -- --                   <+> "\n  vs = " <> list (map pretty vs)
    -- --                   <+> "\n  rs = " <> list (map pretty rs)
    -- --
    -- --         lhs <- stdFunction recmap t vs |>> (<+> v)
    -- --         castFunction <- staticCast recmap t args mname
    -- --         let vs' = take
    -- --                   (length vs)
    -- --                   (map (\j -> "std::placeholders::_" <> viaShow j) ([1..] :: [Int]))
    -- --             rs' = map (bndNamer . argId) rs
    -- --             rhs = stdBind $ castFunction : (rs' ++ vs')
    -- --             sig = nest 4 (vsep [lhs <+> "=", rhs]) <> ";"
    -- --         return (v, [sig])
    -- --     return (d2, PoolDocs
    -- --       { poolCompleteManifolds = mdoc : ms'
    -- --       , poolExpr = call
    -- --       , poolPriorLines = ps1 ++ ps2
    -- --       , poolPriorExprs = pes1
    -- --       })






-- translateManifold :: RecMap -> Set.Set Int -> SerialManifold -> MorlocMonad (Set.Set Int, MDoc)
-- translateManifold recmap done0 m0@(ManifoldM _ form0 _) = do
--   MM.startCounter
--   (done, e) <- f done0 (manifoldArgs form0) m0
--   m <- return . vsep . punctuate line $ poolPriorExprs e <> poolCompleteManifolds e
--   return (done, m)
--   where
--
--   f :: Set.Set Int
--     -> [Arg TypeM]
--     -> ExprM One
--     -> MorlocMonad (Set.Set Int, PoolDocs)
--
--   f done args (LetM i (SerializeM s e1) e2) = do
--     (d1, PoolDocs ms1 e1' ps1 pes1) <- f done args e1
--     (d2, PoolDocs ms2 e2' ps2 pes2) <- f d1 args e2
--     serialized <- serialize recmap i e1' s
--     return (d2, PoolDocs
--       { poolCompleteManifolds = ms1 <> ms2
--       , poolExpr = vsep $ ps1 <> ps2 <> serialized <> [e2']
--       , poolPriorLines = []
--       , poolPriorExprs = pes1 <> pes2
--       }
--       )
--
--   f done args (LetM i (DeserializeM s e1) e2) = do
--     (d1, PoolDocs ms1 e1' ps1 pes1) <- f done args e1
--     (d2, PoolDocs ms2 e2' ps2 pes2) <- f d1 args e2
--     t <- showNativeTypeM recmap (typeOfExprM e1)
--     deserialized <- deserialize recmap i t e1' s
--     return (d2, PoolDocs
--       { poolCompleteManifolds = ms1 <> ms2
--       , poolExpr = vsep $ ps1 <> ps2 <> deserialized <> [e2']
--       , poolPriorLines = []
--       , poolPriorExprs = pes1 <> pes2
--       })
--
--   f _ _ (SerializeM _ _) = MM.throwError . SerializationError
--     $ "SerializeM should only appear in an assignment"
--
--   f _ _ (DeserializeM _ _) = MM.throwError . SerializationError
--     $ "DeserializeM should only appear in an assignment"
--
--   f done args (LetM i e1 e2) = do
--     (d1, PoolDocs ms1' e1' ps1 pes1) <- f done args e1
--     (d2, PoolDocs ms2' e2' ps2 pes2) <- f d1 args e2
--     let t = showTypeM recmap (typeOfExprM e1)
--         ps = ps1 ++ ps2 ++ [[idoc|#{t} #{letNamer i} = #{e1'};|], e2']
--     return (d2, PoolDocs
--       { poolCompleteManifolds = ms1' <> ms2'
--       , poolExpr = vsep ps
--       , poolPriorLines = []
--       , poolPriorExprs = pes1 <> pes2
--       })
--
--   -- dropping the redefinition
--   f done args (AppM (SrcM (Function inputs _) src) xs) = do
--     (d2, es) <- statefulMapM (`f` args) done xs
--     let name' = pretty $ srcName src
--         ts' = map (showTypeM recmap) inputs
--         m = mergePoolDocs (\xs' -> name' <> tupled (zipWith (\t x -> t <> parens x) ts' xs') ) es
--     return (d2, m {poolPriorLines = poolPriorLines m})
--
--   f done _ (PoolCallM _ _ cmds args) = do
--     let bufDef = "std::ostringstream s;"
--         callArgs = map dquotes cmds ++ map argName args
--         cmd = "s << " <> cat (punctuate " << \" \" << " callArgs) <> ";"
--         call = [idoc|foreign_call(s.str())|]
--     return (done, PoolDocs
--       { poolCompleteManifolds = []
--       , poolExpr = call
--       , poolPriorLines = [bufDef, cmd]
--       , poolPriorExprs = []
--       })
--
--   f done args (AppM (PoolCallM _ _ cmds _) xs) = do
--     (d2, ms) <- statefulMapM (`f` args) done xs
--     let bufDef = "std::ostringstream s;"
--         callArgs = map dquotes cmds <> map poolExpr ms
--         cmd = "s << " <> cat (punctuate " << \" \" << " callArgs) <> ";"
--         call = [idoc|foreign_call(s.str())|]
--         m = mergePoolDocs (const call) ms
--     return (d2, m {poolPriorLines = poolPriorLines m <> [bufDef, cmd]})
--
--   f _ _ (AppM _ _) = error "Can only apply functions"
--
--   f done _ (SrcM _ src) = return (done, PoolDocs [] (pretty $ srcName src) [] [])
--
--   f done _ (ManifoldM i form e)
--     -- If this manifold has already been evaluated, just generate the call to it
--     | Set.member i done = do
--         let mname = manNamer i
--         call <- case form of
--           (ManifoldFull rs) -> return $ mname <> tupled (map (bndNamer . argId) rs)
--           (ManifoldPass _) -> return mname
--           (ManifoldPart _ _) -> return $ mname <> "_fun"
--         return (done, PoolDocs
--           { poolCompleteManifolds = []
--           , poolExpr = call
--           , poolPriorLines = []
--           , poolPriorExprs = []
--           })
--     -- Otherwise, generate a signature for the manifold and all children
--     | otherwise = do
--         let args = manifoldArgs form
--         (d2, PoolDocs ms' body ps1 pes1) <- f (Set.insert i done) args e
--         let t = typeOfExprM e
--             decl = showTypeM recmap t <+> manNamer i <> tupled (map (makeArg recmap) args)
--             mdoc = block 4 decl body
--             mname = manNamer i
--         (call, ps2) <- case form of
--           (ManifoldFull rs) -> return (mname <> tupled (map (bndNamer . argId) rs), [])
--           (ManifoldPass _) -> return (mname, [])
--           (ManifoldPart rs vs) -> do
--             let v = mname <> "_fun"
--
--             MM.sayVVV $ "ManifoldPart" <+> v <> ":"
--                       <+> "\n  vs = " <> list (map pretty vs)
--                       <+> "\n  rs = " <> list (map pretty rs)
--
--             lhs <- stdFunction recmap t vs |>> (<+> v)
--             castFunction <- staticCast recmap t args mname
--             let vs' = take
--                       (length vs)
--                       (map (\j -> "std::placeholders::_" <> viaShow j) ([1..] :: [Int]))
--                 rs' = map (bndNamer . argId) rs
--                 rhs = stdBind $ castFunction : (rs' ++ vs')
--                 sig = nest 4 (vsep [lhs <+> "=", rhs]) <> ";"
--             return (v, [sig])
--         return (d2, PoolDocs
--           { poolCompleteManifolds = mdoc : ms'
--           , poolExpr = call
--           , poolPriorLines = ps1 ++ ps2
--           , poolPriorExprs = pes1
--           })
--
--   f _ _ (ForeignInterfaceM _ _ _) = MM.throwError . CallTheMonkeys $
--     "Foreign interfaces should have been resolved before passed to the translators"
--
--   f _ _ (LamM _ _ _) = undefined
--
--   f done args (AccM e k) = do
--     (d2, p) <- f done args e
--     return (d2, p {poolExpr = poolExpr p <> "." <> pretty k})
--
--   f done args (ListM _ es) = do
--     (done', es') <- statefulMapM (`f` args) done es
--     return (done', mergePoolDocs (encloseSep "{" "}" ",") es')
--
--   f done args (TupleM _ es) = do
--     (done', es') <- statefulMapM (`f` args) done es
--     return (done', mergePoolDocs (\xs -> "std::make_tuple" <> tupled xs) es')
--
--   f done args (RecordM c entries) = do
--     idx <- MM.getCounter
--     let t = showTypeM recmap c
--         v' = "a" <> pretty idx
--     (done', ps) <- statefulMapM (\s a -> f s args . snd $ a) done entries
--     let decl = t <+> v' <+> "=" <+> encloseSep "{" "}" "," (map poolExpr ps) <> ";"
--         p = mergePoolDocs (const v') ps
--     return (done', p {poolPriorLines = poolPriorLines p <> [decl]})
--
--   f done _ (BndVarM _ i) = return (done, PoolDocs [] (bndNamer i) [] [])
--   f done _ (LetVarM _ i) = return (done, PoolDocs [] (letNamer i) [] [])
--   f done _ (LogM _ x) = return (done, PoolDocs [] (if x then "true" else "false") [] [])
--   f done _ (RealM _ x) = return (done, PoolDocs [] (viaShow x) [] [])
--   f done _ (IntM _ x) = return (done, PoolDocs [] (viaShow x) [] [])
--   f done _ (StrM _ x) = return (done, PoolDocs [] (dquotes $ pretty x) [] [])
--   f done _ (NullM _) = return (done, PoolDocs [] "null" [] [])
--
--   f done args (ReturnM e) = do
--     (done', p) <- f done args e
--     return (done', p {poolExpr = "return(" <> poolExpr p <> ");"})
-- translateManifold _ _ _ = error "Every ExprM object must start with a Manifold term"

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

-- showType :: TypeF -> CppTranslator MDoc
-- showType (UnkP _) = return serialType
-- showType (VarP (PV _ _ v)) = return $ pretty v
-- showType (FunP ts t) = do
--     t' <- showType t
--     ts' <- mapM showType ts
--     return $ "std::function" <> "<" <> t' <> tupled ts' <>">"
-- showType (AppP (VarP (PV _ _ v)) ts) = do
--     ts' <- mapM showType ts
--     return . pretty $ expandMacro v (map render ts')
-- showType (AppP _ _) = undefined
-- -- NamRecord struct@CppLang<int@CppLang>
-- -- {
-- --     name :: std::string@CppLang
-- --     info :: int@CppLang
-- -- }
-- showType (NamP _ (PV _ gc "struct") _ rs) = do
--   recmap <- CMS.gets translatorRecmap
--   -- handle autogenerated structs
--   case lookup (FV gc "struct", map fst rs) recmap of
--     (Just rec) -> do
--       params <- typeParams (zip (map snd (recFields rec)) (map snd rs))
--       return $ recName rec <> params
--     Nothing -> error "Should not happen"
-- showType (NamP _ (PV _ _ s) ps _) = do
--   ps' <- mapM showType ps
--   return $ pretty s <> encloseSep "<" ">" "," ps'


collectRecords :: SerialManifold -> [(FVar, GIndex, [(FVar, TypeF)])]
collectRecords e0@(SerialManifold i0 _ _ _) = CMS.evalState (foldSerialManifoldM fm e0) i0 where
  fm = FoldManifoldM
    { opSerialManifoldM = serialManifold
    , opNativeManifoldM = nativeManifold
    , opSerialExprM = return . foldl (<>) []
    , opNativeExprM = nativeExpr
    , opSerialArgM = return . foldl (<>) []
    , opNativeArgM = return . foldl (<>) []
    }

  serialManifold (SerialManifold_ i _ _ x) = CMS.put i >> return x

  nativeManifold (NativeManifold_ i _ _ (_, x)) = CMS.put i >> return x

  nativeExpr e@(RecordN_ _ v _ rs) = do
    m <- CMS.get
    let entry = (v, m, [(key, valType) | (key, (valType, _)) <- rs])
    return $ entry : foldl (<>) [] e
  nativeExpr x = return . foldl (<>) [] $ x

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

  maybeM :: Monad m => a -> (b -> m a) -> Maybe b -> m a
  maybeM _ f (Just x) = f x
  maybeM defaultValue _ Nothing = return defaultValue


generateSourcedSerializers :: [SerialManifold] -> CppTranslator ([MDoc],[MDoc])
generateSourcedSerializers = undefined
-- generateSourcedSerializers es0 = do
--   typemap <- Map.unions <$> mapM collectSerialManifold es0
--   return $ foldl groupQuad ([],[]) . Map.elems . Map.mapMaybeWithKey makeSerial $ typemap
--   where
--     collectNativeManifold (NativeManifold i _ _ _ e) = do
--         t <- MM.metaTypedefs i
--         collectNativeExpr t e
--
--     collectSerialManifold (SerialManifold i _ _   e) =
--         Map.union <$> collectSerialExpr e <*> MM.metaTypedefs i
--
--     collectSerialArg (SerialArgManifold x) = collectSerialManifold x
--     collectSerialArg (SerialArgExpr e) = collectSerialExpr e
--
--     collectNativeArg (NativeArgManifold x) = collectNativeManifold x
--     collectNativeArg (NativeArgExpr e) = collectNativeExpr e
--
--     collectSerialExpr (AppManS e eitherArgs) = do
--         m' <- collectSerialManifold e
--         rs' <- mapM (mapTo collectSerialArg collectNativeArg) eitherArgs
--         return $ Map.unions (m':rs')
--     collectSerialExpr (AppPoolS _ args) = Map.unions <$> mapM collectSerialArg args
--     collectSerialExpr (ReturnS e) = collectSerialExpr e
--     collectSerialExpr (SerialLetS _ sa sb) = Map.union <$> collectSerialExpr sa
--                                                        <*> collectSerialExpr sb
--     collectSerialExpr (NativeLetS _ na sb) = Map.union <$> collectNativeExpr na
--                                                        <*> collectSerialExpr sb
--     collectSerialExpr (LetVarS _) = Map.empty
--     collectSerialExpr (BndVarS _) = Map.empty
--     collectSerialExpr (SerializeS _ e) = collectNativeExpr e
--
--     collectNativeExpr (AppSrcN      _ _ rs) = Map.unions <$> mapM collectNativeArg rs
--     collectNativeExpr (AppManN      _ m args) = do
--         t <- collectNativeManifold m
--         rs <- mapM (mapTo collectSerialArg collectNativeArg) args
--         return $ Map.unions (t:ts)
--     collectNativeExpr (ReturnN      _ e) = collectNativeExpr e
--     collectNativeExpr (SerialLetN   _ _ sa nb) =  Map.union <$> collectSerialExpr sa
--                                                             <*> collectNativeExpr sb
--     collectNativeExpr (NativeLetN   _ _ na nb) =  Map.union <$> collectNativeExpr na
--                                                             <*> collectNativeExpr sb
--     collectNativeExpr (DeserializeN _ _ e) = collectSerialExpr e
--     collectNativeExpr (AccN         _ _ _ e _) = collectNativeExpr e
--     collectNativeExpr (ListN        _ es) = Map.unions <$> mapM collectNativeExpr es
--     collectNativeExpr (TupleN       _ es) = Map.unions <$> mapM collectNativeExpr es
--     collectNativeExpr (RecordN      _ rs) = Map.unions <$> mapM (collectNativeExpr . snd) rs
--     collectNativeExpr _ = Map.empty
--
--
--
--     groupQuad :: ([a],[a]) -> (a, a, a, a) -> ([a],[a])
--     groupQuad (xs,ys) (x1, y1, x2, y2) = (x1:x2:xs, y1:y2:ys)
--
--     makeSerial :: TVar -> (Type, [TVar]) -> Maybe (MDoc, MDoc, MDoc, MDoc)
--     makeSerial = undefined
--     -- makeSerial _ (NamT _ (TV _ "struct") _ _, _) = Nothing
--     -- makeSerial (TV (Just CppLang) _) (NamT r (TV _ v) _ rs, ps)
--     --   = Just (serialDecl, serializer, deserialDecl, deserializer) where
--     --
--     --     templateTerms = ["T" <> pretty p | (TV _ p) <- ps]
--     --
--     --     params = map (\p -> "T" <> pretty (unTVar p)) ps
--     --     rtype = pretty v <> recordTemplate templateTerms
--     --     fields = [(pretty k, showDefType ps t) | (k, t) <- rs]
--     --
--     --     serialDecl = serialHeaderTemplate params rtype
--     --     deserialDecl = deserialHeaderTemplate params rtype
--     --
--     --     serializer = serializerTemplate params rtype fields
--     --
--     --     deserializer = deserializerTemplate (r == NamObject) params rtype fields
--     -- makeSerial _ _ = Nothing
--
--     showDefType :: [TVar] -> Type -> MDoc
--     showDefType ps (UnkT v@(TV _ s))
--       | elem v ps = "T" <> pretty s
--       | otherwise = pretty s
--     showDefType ps (VarT v@(TV _ s))
--       | elem v ps = "T" <> pretty s
--       | otherwise = pretty s
--     showDefType _ (FunT _ _) = error "Cannot serialize functions"
--     showDefType _ (NamT _ (TV _ _) _ _)
--       = undefined -- pretty v <> encloseSep "<" ">" "," (map (showDefType ps) ts)
--     showDefType ps (AppT (VarT (TV _ v)) ts) = pretty $ expandMacro v (map (render . showDefType ps) ts)
--     showDefType _ (AppT _ _) = error "AppT is only OK with VarT, for now"

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
