{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.CodeGenerator.Grammars.Translator.Cpp
Description : C++ translator
Copyright   : (c) Zebulun Arendsee, 2020
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
import Morloc.CodeGenerator.Internal (typeP2typeM)
import Morloc.CodeGenerator.Serial ( isSerializable
                                   , prettySerialOne
                                   , serialAstToType
                                   , serialAstToType'
                                   , shallowType
                                   )

import qualified Morloc.Frontend.Lang.DefaultTypes as Def
import Morloc.CodeGenerator.Grammars.Common
import qualified Morloc.CodeGenerator.Grammars.Translator.Source.CppInternals as Src
import Morloc.Data.Doc
import Morloc.Quasi
import qualified Morloc.System as MS
import Morloc.CodeGenerator.Grammars.Macro (expandMacro)
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM

data RecEntry = RecEntry {
    recAutoName :: MDoc -- ^ the automatically generated name for this anonymous type
  , recFields :: [( PVar -- The field key
                  , Maybe TypeP -- The field type if not generic
                  )]
}

type RecMap = [((PVar, [PVar]), RecEntry)]

-- tree rewrites
preprocess :: ExprM Many -> MorlocMonad (ExprM Many)
preprocess = invertExprM

translate :: [Source] -> [ExprM One] -> MorlocMonad MDoc
translate srcs es = do
  -- translate sources
  includeDocs <- mapM
    translateSource
    (unique . catMaybes . map srcPath $ srcs)

  -- diagnostics
  liftIO . putDoc . vsep $ "-- C++ translation --" : map prettyExprM es

  let recmap = unifyRecords . conmap collectRecords $ es
      (declarations, serializers) = (\xs -> (conmap fst xs, conmap snd xs))
                                  . map (uncurry (makeSerializers recmap))
                                  . reverse $ recmap
      dispatch = makeDispatch es
      signatures = map (makeSignature recmap) es

      serializationCode = declarations ++ serializers

  -- translate each manifold tree, rooted on a call from nexus or another pool
  mDocs <- mapM (translateManifold recmap) es

  -- create and return complete pool script
  return $ makeMain includeDocs signatures serializationCode mDocs dispatch

letNamer :: Int -> MDoc
letNamer i = "a" <> viaShow i

manNamer :: Int -> MDoc
manNamer i = "m" <> viaShow i

bndNamer :: Int -> MDoc
bndNamer i = "x" <> viaShow i

serialType :: MDoc
serialType = "std::string"

defaultRecordTerm :: MT.Text
defaultRecordTerm = head [v | (NamU _ (TV Nothing v) _) <- Def.defaultRecord Nothing []]

makeSignature :: RecMap -> ExprM One -> MDoc
makeSignature recmap e0@(ManifoldM _ _ _) = vsep (f e0) where
  f :: ExprM One -> [MDoc]
  f (ManifoldM (metaId->i) args e) =
    let t = typeOfExprM e
        sig = showTypeM recmap t <+> manNamer i <> tupled (map (makeArg recmap) args) <> ";"
    in sig : f e
  f (LetM _ e1 e2) = f e1 ++ f e2
  f (AppM e es) = f e ++ conmap f es
  f (LamM _ e) = f e
  f (ListM _ es) = conmap f es
  f (TupleM _ es) = conmap f es
  f (RecordM _ entries) = conmap f (map snd entries)
  f (SerializeM _ e) = f e
  f (DeserializeM _ e) = f e
  f (ReturnM e) = f e
  f _ = []

makeArg :: RecMap -> Argument -> MDoc
makeArg recmap (SerialArgument i _) = serialType <+> bndNamer i
makeArg recmap (NativeArgument i c) = showTypeM recmap (Native c) <+> bndNamer i
makeArg recmap (PassThroughArgument i) = serialType <+> bndNamer i

argName :: Argument -> MDoc
argName (SerialArgument i _) = bndNamer i
argName (NativeArgument i _) = bndNamer i
argName (PassThroughArgument i) = bndNamer i

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
translateSource
  :: Path -- ^ Path to a header (e.g., `$MORLOC_HOME/lib/foo.h`)
  -> MorlocMonad MDoc
translateSource path = return $
  "#include" <+> (dquotes . pretty . MS.takeFileName) path


serialize
  :: RecMap
  -> Int -- The let index `i`
  -> MDoc -- The type of e1
  -> MDoc -- A variable name pointing to e1
  -> SerialAST One
  -> MorlocMonad [MDoc]
serialize recmap letIndex typestr0 datavar0 s0 = do
  (x, before) <- serialize' datavar0 s0
  t0 <- (showTypeM recmap . Native) <$> serialAstToType s0
  let schemaName = [idoc|#{letNamer letIndex}_schema|]
      schema = [idoc|#{t0} #{schemaName};|]
      final = [idoc|#{serialType} #{letNamer letIndex} = serialize(#{x}, #{schemaName});|]
  return (before ++ [schema, final])

  where
    serialize'
      :: MDoc -- a variable name that stores the data described by the SerialAST object
      -> SerialAST One -> MorlocMonad (MDoc, [MDoc])
    serialize' v s
      | isSerializable s = return (v, [])
      | otherwise = construct v s

    construct :: MDoc -> SerialAST One -> MorlocMonad (MDoc, [MDoc])
    construct v (SerialPack _ (One (p, s))) = do
      unpacker <- case typePackerReverse p of
        [] -> MM.throwError . SerializationError $ "No unpacker found"
        (src:_) -> return . pretty . srcName $ src
      serialize' [idoc|#{unpacker}(#{v})|] s

    construct v lst@(SerialList s) = do
      idx <- fmap pretty $ MM.getCounter
      t <- serialAstToType lst
      let v' = "s" <> idx 
          decl = [idoc|#{showType recmap t} #{v'};|]
      (x, before) <- serialize' [idoc|#{v}[i#{idx}]|] s
      let push = [idoc|#{v'}.push_back(#{x});|]
          lst  = block 4 [idoc|for(size_t i#{idx} = 0; i#{idx} < #{v}.size(); i#{idx}++)|] 
                         (vsep (before ++ [push]))
      return (v', [decl, lst])

    construct v tup@(SerialTuple ss) = do
      (ss', befores) <- fmap unzip $ zipWithM (\i s -> serialize' (tupleKey i v) s) [0..] ss
      idx <- fmap pretty $ MM.getCounter
      t <- serialAstToType tup
      let v' = "s" <> idx
          x = [idoc|#{showType recmap t} #{v'} = std::make_tuple#{tupled ss'};|]
      return (v', concat befores ++ [x]);

    construct v rec@(SerialObject NamRecord name rs) = do
      (ss', befores) <- fmap unzip $ mapM (\(PV _ _ k,s) -> serialize' (recordAccess v (pretty k)) s) rs
      idx <- fmap pretty $ MM.getCounter
      t <- (showType recmap) <$> serialAstToType rec
      let v' = "s" <> idx
          decl = encloseSep "{" "}" "," ss'
          x = [idoc|#{t} #{v'} = #{decl};|]
      return (v', concat befores ++ [x]);

    construct _ s = MM.throwError . SerializationError . render
      $ "construct: " <> prettySerialOne s

-- reverse of serialize, parameters are the same
deserialize :: RecMap -> Int -> MDoc -> MDoc -> SerialAST One -> MorlocMonad [MDoc]
deserialize recmap letIndex typestr0 varname0 s0
  | isSerializable s0 = do 
      let schemaName = [idoc|#{letNamer letIndex}_schema|]
          schema = [idoc|#{typestr0} #{schemaName};|]
          deserializing = [idoc|#{typestr0} #{letNamer letIndex} = deserialize(#{varname0}, #{schemaName});|]
      return [schema, deserializing]
  | otherwise = do
      idx <- fmap pretty $ MM.getCounter
      t <- serialAstToType s0
      let rawtype = showType recmap $ t
          schemaName = [idoc|#{letNamer letIndex}_schema|]
          rawvar = "s" <> idx
          schema = [idoc|#{rawtype} #{schemaName};|]
          deserializing = [idoc|#{rawtype} #{rawvar} = deserialize(#{varname0}, #{schemaName});|]
      (x, before) <- construct rawvar s0
      let final = [idoc|#{typestr0} #{letNamer letIndex} = #{x};|]
      return ([schema, deserializing] ++ before ++ [final])

  where
    check :: MDoc -> SerialAST One -> MorlocMonad (MDoc, [MDoc])
    check v s
      | isSerializable s = return (v, [])
      | otherwise = construct v s

    construct :: MDoc -> SerialAST One -> MorlocMonad (MDoc, [MDoc])
    construct v (SerialPack _ (One (p, s'))) = do
      packer <- case typePackerForward p of
        [] -> MM.throwError . SerializationError $ "No packer found"
        (x:_) -> return . pretty . srcName $ x
      (x, before) <- check v s'
      let deserialized = [idoc|#{packer}(#{x})|]
      return (deserialized, before)

    construct v lst@(SerialList s) = do
      idx <- fmap pretty $ MM.getCounter
      t <- fmap (showType recmap) $ shallowType lst
      let v' = "s" <> idx 
          decl = [idoc|#{t} #{v'};|]
      (x, before) <- check [idoc|#{v}[i#{idx}]|] s
      let push = [idoc|#{v'}.push_back(#{x});|]
          lst  = block 4 [idoc|for(size_t i#{idx} = 0; i#{idx} < #{v}.size(); i#{idx}++)|] 
                         (vsep (before ++ [push]))
      return (v', [decl, lst])

    construct v tup@(SerialTuple ss) = do
      idx <- fmap pretty $ MM.getCounter
      (ss', befores) <- fmap unzip $ zipWithM (\i s -> check (tupleKey i v) s) [0..] ss
      t <- shallowType tup
      let v' = "s" <> idx
          x = [idoc|#{showType recmap $ t} #{v'} = std::make_tuple#{tupled ss'};|]
      return (v', concat befores ++ [x]);

    construct v rec@(SerialObject NamRecord name rs) = do
      idx <- fmap pretty $ MM.getCounter
      (ss', befores) <- fmap unzip $ mapM (\(PV _ _ k,s) -> check (recordAccess v (pretty k)) s) rs
      t <- fmap (showType recmap) $ shallowType rec
      let v' = "s" <> idx
          decl = encloseSep "{" "}" "," ss'
          x = [idoc|#{t} #{v'} = #{decl};|]
      return (v', concat befores ++ [x]);

    construct _ s = MM.throwError . SerializationError . render
      $ "deserializeDescend: " <> prettySerialOne s

translateManifold :: RecMap -> ExprM One -> MorlocMonad MDoc
translateManifold recmap m@(ManifoldM _ args _) = do
  MM.startCounter
  (vsep . punctuate line . (\(x,_,_)->x)) <$> f args m
  where

  f :: [Argument]
    -> ExprM One
    -> MorlocMonad
       ( [MDoc] -- the collection of final manifolds
       , MDoc -- a call tag for this expression
       , [MDoc] -- a list of statements that should precede this assignment
       )

  f args (LetM i (SerializeM s e1) e2) = do
    (ms1, e1', ps1) <- f args e1
    (ms2, e2', ps2) <- f args e2
    t <- showNativeTypeM recmap (typeOfExprM e1)
    serialized <- serialize recmap i t e1' s
    return (ms1 ++ ms2, vsep $ ps1 ++ ps2 ++ serialized ++ [e2'], [])

  f args (LetM i (DeserializeM s e1) e2) = do
    (ms1, e1', ps1) <- f args e1
    (ms2, e2', ps2) <- f args e2
    t <- showNativeTypeM recmap (typeOfExprM e1)
    deserialized <- deserialize recmap i t e1' s
    return (ms1 ++ ms2, vsep $ ps1 ++ ps2 ++ deserialized ++ [e2'], [])

  f _ (SerializeM _ _) = MM.throwError . SerializationError
    $ "SerializeM should only appear in an assignment"

  f _ (DeserializeM _ _) = MM.throwError . SerializationError
    $ "DeserializeM should only appear in an assignment"

  f args (LetM i e1 e2) = do
    (ms1', e1', ps1) <- (f args) e1
    (ms2', e2', ps2) <- (f args) e2
    let t = showTypeM recmap (typeOfExprM e1)
        ps = ps1 ++ ps2 ++ [[idoc|#{t} #{letNamer i} = #{e1'};|], e2']
    return (ms1' ++ ms2', vsep ps, [])

  f args (AppM (SrcM (Function inputs output) src) xs) = do
    (mss', xs', pss) <- mapM (f args) xs |>> unzip3
    let
        name = pretty $ srcName src
        mangledName = name <> "_fun"
        inputBlock = cat (punctuate "," (map (showTypeM recmap) inputs))
        sig = [idoc|#{showTypeM recmap output}(*#{mangledName})(#{inputBlock}) = &#{name};|]
    return (concat mss', mangledName <> tupled xs', sig : concat pss)
    where
      typeOfExprM' :: ExprM One -> TypeM
      typeOfExprM' m@(ManifoldM _ args' _) = case splitArgs args' args of
        (_, []) -> typeOfExprM m
        (_, ts) -> Function (map arg2typeM ts) (typeOfExprM m)
      typeOfExprM' e = typeOfExprM e

  f args (AppM f xs) = error "Goddamn it! You just had to ask!"

  f args (SrcM t src) = return ([], pretty $ srcName src, [])

  f pargs m@(ManifoldM (metaId->i) args e) = do
    (ms', body, ps1) <- f args e
    let t = typeOfExprM e
        head = showTypeM recmap t <+> manNamer i <> tupled (map (makeArg recmap) args)
        mdoc = block 4 head body
        mname = manNamer i
    (call, ps2) <- case (splitArgs args pargs, nargsTypeM t) of
      ((rs, []), _) -> return (mname <> tupled (map (bndNamer . argId) rs), [])
      (([], vs), _) -> return (mname, [])
      ((rs, vs), _) -> do
        let v = mname <> "_fun"
        lhs <- stdFunction recmap t vs |>> (\x -> x <+> v)
        castFunction <- staticCast recmap t args mname
        let vs' = take
                  (length vs)
                  (map (\i -> "std::placeholders::_" <> viaShow i) [1..])
            rs' = map (bndNamer . argId) rs
            rhs = stdBind $ castFunction : (rs' ++ vs')
            sig = nest 4 (vsep [lhs <+> "=", rhs]) <> ";"
        return (v, [sig])
    return (mdoc : ms', call, ps1 ++ ps2)

  f _ (PoolCallM t _ cmds args) = do
    let bufDef = "std::ostringstream s;"
        callArgs = map dquotes cmds ++ map argName args
        cmd = "s << " <> cat (punctuate " << \" \" << " callArgs) <> ";"
        call = [idoc|foreign_call(s.str())|] 
    return ([], call, [bufDef, cmd])

  f args (ForeignInterfaceM _ _) = MM.throwError . CallTheMonkeys $
    "Foreign interfaces should have been resolved before passed to the translators"

  f args (LamM lambdaArgs e) = undefined

  f args (ListM t es) = do
    (mss', es', pss) <- mapM (f args) es |>> unzip3
    let x' = encloseSep "{" "}" "," es'
    return (concat mss', x', concat pss)

  f args (TupleM _ es) = do
    (mss', es', pss) <- mapM (f args) es |>> unzip3
    return (concat mss', "std::make_tuple" <> tupled es', concat pss)

  f args (RecordM c entries) = do
    (mss', es', pss) <- mapM (f args . snd) entries |>> unzip3
    idx <- fmap pretty $ MM.getCounter
    let t = showTypeM recmap c
        v' = "a" <> idx
        decl = encloseSep "{" "}" "," es'
        x = [idoc|#{t} #{v'} = #{decl};|]
    return (concat mss', v', concat pss ++ [x])

  f _ (BndVarM c i) = return ([], bndNamer i, [])
  f _ (LetVarM c i) = return ([], letNamer i, [])
  f _ (LogM _ x) = return ([], if x then "true" else "false", [])
  f _ (NumM _ x) = return ([], viaShow x, [])
  f _ (StrM _ x) = return ([], dquotes $ pretty x, [])
  f _ (NullM _) = return ([], "null", [])

  f args (ReturnM e) = do
    (ms, e', ps) <- f args e
    return (ms, "return(" <> e' <> ");", ps)


stdFunction :: RecMap -> TypeM -> [Argument] -> MorlocMonad MDoc
stdFunction recmap t args = 
  let argList = cat (punctuate "," (map (argTypeM recmap) args))
  in return [idoc|std::function<#{showTypeM recmap t}(#{argList})>|]

stdBind :: [MDoc] -> MDoc
stdBind xs = [idoc|std::bind(#{args})|] where
  args = cat (punctuate "," xs)

staticCast :: RecMap -> TypeM -> [Argument] -> MDoc -> MorlocMonad MDoc
staticCast recmap t args name = do
  let output = showTypeM recmap t
      inputs = map (argTypeM recmap) args
      argList = cat (punctuate "," inputs)
  return $ [idoc|static_cast<#{output}(*)(#{argList})>(&#{name})|]

argTypeM :: RecMap -> Argument -> MDoc
argTypeM _ (SerialArgument _ _) = serialType
argTypeM recmap (NativeArgument _ c) = showType recmap c
argTypeM _ (PassThroughArgument _) = serialType

-- divide a list of arguments based on wheither they are in a second list
splitArgs :: [Argument] -> [Argument] -> ([Argument], [Argument])
splitArgs args1 args2 = partitionEithers $ map split args1 where
  split :: Argument -> Either Argument Argument
  split r = if elem r args2
            then Left r
            else Right r

makeDispatch :: [ExprM One] -> MDoc
makeDispatch ms = block 4 "switch(cmdID)" (vsep (map makeCase ms))
  where
    makeCase :: ExprM One -> MDoc
    makeCase (ManifoldM (metaId->i) args _) =
      let args' = take (length args) $ map (\i -> "argv[" <> viaShow i <> "]") [2..]
      in
        (nest 4 . vsep)
          [ "case" <+> viaShow i <> ":"
          , "result = " <> manNamer i <> tupled args' <> ";"
          , "break;"
          ]

showType :: RecMap -> TypeP -> MDoc
showType _ (UnkP _) = serialType
showType _ (VarP (PV _ _ v)) = pretty v 
showType recmap t@(FunP _ _) = showTypeM recmap (typeP2typeM t)
showType recmap (ArrP (PV _ _ v) ts) = pretty $ expandMacro v (map (render . showType recmap) ts)
showType recmap (NamP _ v@(PV _ _ "struct") rs) =
  -- handle autogenated structs
  case lookup (v, map fst rs) recmap of
    (Just rec) -> recAutoName rec <> typeParams recmap (zip (map snd (recFields rec)) (map snd rs))
    Nothing -> error "Should not happen"
showType recmap t@(NamP _ v@(PV _ _ s) rs) =
  case lookup (v, map fst rs) recmap of
    (Just rec) -> recAutoName rec <> typeParams recmap (zip (map snd (recFields rec)) (map snd rs))
    Nothing -> error $ "FIXME: need to infer type parameters for named records: " <> show t

typeParams :: RecMap -> [(Maybe TypeP, TypeP)] -> MDoc
typeParams recmap ts
  = case [showTypeM recmap (Native t) | (Nothing, t) <- ts] of
      [] -> ""
      ds -> encloseSep "<" ">" "," ds

showTypeM :: RecMap -> TypeM -> MDoc
showTypeM _ Passthrough = serialType
showTypeM _ (Serial t) = serialType
showTypeM recmap (Native t) = showType recmap t
showTypeM recmap (Function ts t)
  = "std::function<" <> showTypeM recmap t
  <> "(" <> cat (punctuate "," (map (showTypeM recmap) ts)) <> ")>"

-- for use in making schema, where the native type is needed
showNativeTypeM :: RecMap -> TypeM -> MorlocMonad MDoc
showNativeTypeM recmap (Serial t) = return $ showTypeM recmap (Native t)
showNativeTypeM recmap (Native t) = return $ showTypeM recmap (Native t)
showNativeTypeM _ _ = MM.throwError . OtherError $ "Expected a native or serialized type"


collectRecords :: ExprM One -> [(PVar, GMeta, [(PVar, TypeP)])]
collectRecords e0 = f (gmetaOf e0) e0 where
  f _ (ManifoldM m _ e) = f m e
  f m (ForeignInterfaceM t e) = cleanRecord m t ++ f m e
  f m (PoolCallM t _ _ _) = cleanRecord m t
  f m (LetM _ e1 e2) = f m e1 ++ f m e2
  f m (AppM e es) = f m e ++ conmap (f m) es
  f m (LamM _ e) = f m e
  f m (ListM t es) = cleanRecord m t ++ conmap (f m) es
  f m (TupleM t es) = cleanRecord m t ++ conmap (f m) es
  f m (RecordM t rs) = cleanRecord m t ++ conmap (f m . snd) rs
  f m (SerializeM s e)
    = cleanRecord m (Native (serialAstToType' s)) ++ f m e
  f m (DeserializeM s e)
    = cleanRecord m (Serial (serialAstToType' s)) ++ f m e
  f m (ReturnM e) = f m e
  f m (BndVarM t _) = cleanRecord m t
  f m (LetVarM t _) = cleanRecord m t
  f m _ = []

cleanRecord :: GMeta -> TypeM -> [(PVar, GMeta, [(PVar, TypeP)])]
cleanRecord m tm = case typeOfTypeM tm of
  (Just t) -> toRecord t
  Nothing -> []
  where
    toRecord :: TypeP -> [(PVar, GMeta, [(PVar, TypeP)])]
    toRecord (UnkP _) = []
    toRecord (VarP _) = []
    toRecord (FunP t1 t2) = toRecord t1 ++ toRecord t2
    toRecord (ArrP _ ts) = conmap toRecord ts
    toRecord (NamP _ v rs) = (v, m, rs) : conmap toRecord (map snd rs)

unifyRecords :: [(PVar, GMeta, [(PVar, TypeP)])] -> RecMap
unifyRecords rs
  = zipWith (\i ((v,ks),es) -> ((v,ks), RecEntry (structName i v) es)) [1..]
  . map (\((v,m,ks), rss) -> ((v,ks), [unifyField m fs | fs <- transpose rss]))
  . map (\((v,ks), rss@((m,_):_)) -> ((v,m,ks), map snd rss))
  . groupSort
  . unique
  $ [((v, map fst es), (m, es)) | (v, m, es) <- rs]

structName :: Int -> PVar -> MDoc
structName i (PV _ (Just v1) "struct") = "mlc_" <> pretty v1 <> "_" <> pretty i 
structName _ (PV _ _ v) = pretty v

lookupAutoStruct :: PVar -> [(MDoc, (PVar, Maybe TypeP))] -> RecMap -> MDoc
lookupAutoStruct v@(PV _ g _) rs recmap = case lookup (v, [k | (_,(k,_)) <- rs]) recmap of
  (Just (RecEntry v _)) -> v -- an automatically generated type name
  Nothing -> case g of
    (Just g') -> pretty g -- an source type name
    Nothing -> error "All your base are belong to us"

unifyField :: GMeta -> [(PVar, TypeP)] -> (PVar, Maybe TypeP)
unifyField _ [] = error "Empty field"
unifyField _ rs@((v,_):_)
  | not (all ((==) v) (map fst rs))
      = error $ "Bad record - unequal fields: " <> show (unique rs)
  | otherwise = case unique (map snd rs) of
      [t] -> (v, Just t)
      _ -> (v, Nothing)

makeSerializers :: RecMap -> (PVar, [PVar]) -> RecEntry -> ([MDoc],[MDoc])
makeSerializers recmap (t, _) rec
  = ([structDecl, serialDecl, deserialDecl], [serializer, deserializer])
  where
    templateTerms = zipWith (<>) (repeat "T") (map pretty ([1..] :: [Int]))
    rs' = zip templateTerms (recFields rec)

    structDecl = makeStructDecl recmap t rs' <> line
    serialDecl = serialHeader recmap t rs' <> ";"
    deserialDecl = deserialHeader recmap t rs' <> ";" <> line
    serializer = makeSerializer recmap t rs' <> line
    deserializer = makeDeserializer recmap t rs'

makeStructDecl :: RecMap -> PVar -> [(MDoc, (PVar, Maybe TypeP))] -> MDoc
makeStructDecl recmap v rs = vsep [templateLine v rs, struct] where 
  rs' = [(ek, maybe et (showType recmap) ev) | (et, (ek, ev)) <- rs] 
  struct = block 4 ("struct" <+> lookupAutoStruct v rs recmap)
                   (vsep [v' <+> pretty k <> ";" | (PV _ _ k,v') <- rs']) <> ";"

-- Example
-- > template <class T>
-- > std::string serialize(person<T> x, person<T> schema);
serialHeader :: RecMap -> PVar -> [(MDoc, (PVar, Maybe TypeP))] -> MDoc
serialHeader recmap v rs = vsep [templateLine v rs, decl] where
  v' = lookupAutoStruct v rs recmap <> recordTemplate rs
  decl = [idoc|std::string serialize(#{v'} x, #{v'} schema)|]

-- Example:
-- > template <class T>
-- > bool deserialize(const std::string json, size_t &i, person<T> &x);
deserialHeader :: RecMap -> PVar -> [(MDoc, (PVar, Maybe TypeP))] -> MDoc
deserialHeader recmap v rs = vsep [templateLine v rs, decl] where
  v' = lookupAutoStruct v rs recmap <> recordTemplate rs
  decl = [idoc|bool deserialize(const std::string json, size_t &i, #{v'} &x)|]

templateLine :: PVar -> [(MDoc, (PVar, Maybe TypeP))] -> MDoc
templateLine (PV _ _ v) rs = case [v | (v, (_, Nothing)) <- rs] of
  [] -> ""
  ts -> "template" <+> encloseSep "<" ">" "," (map (\t -> "class" <+> t) ts)

makeSerializer :: RecMap -> PVar -> [(MDoc, (PVar, Maybe TypeP))] -> MDoc
makeSerializer recmap v rs = [idoc|
#{template}
std::string serialize(#{rtype} x, #{rtype} schema){
    #{align $ vsep schemata}
    std::ostringstream json;
    json << "{" << #{align $ vsep (punctuate " << ',' <<" fields)} << "}";
    return json.str();
}
|] where
  template = templateLine v rs
  rtype = lookupAutoStruct v rs recmap <> recordTemplate rs
  rs' = [(pretty k, maybe t (showType recmap) v) | (t, (PV _ _ k, v)) <- rs]
  schemata = map (\(k,t) -> t <+> lookupAutoStruct v rs recmap <> "_" <> k <> ";") rs'
  fields = map (\(k,t) -> dquotes ("\\\"" <> k <> "\\\"" <> ":")
         <+> "<<" <+> [idoc|serialize(x.#{k}, #{lookupAutoStruct v rs recmap}_#{k})|] ) rs'

makeDeserializer :: RecMap -> PVar -> [(MDoc, (PVar, Maybe TypeP))] -> MDoc
makeDeserializer recmap v rs = [idoc|
#{template}
bool deserialize(const std::string json, size_t &i, #{rtype} &x){
    #{align $ vsep schemata}
    try {
        whitespace(json, i);
        if(! match(json, "{", i))
            throw 0;
        whitespace(json, i);
        #{align $ vsep (punctuate parseComma fields)}
        if(! match(json, "}", i))
            throw 900;
        whitespace(json, i);
    } catch (int e) {
        return false;
    }
    return true;
}
|] where
  template = templateLine v rs
  rs' = [(pretty k, maybe t (showType recmap) v') | (t, (PV _ _ k, v')) <- rs]
  schemata = map (\(k,t) -> t <+> lookupAutoStruct v rs recmap <> "_" <> k <> ";") rs'
  rtype = lookupAutoStruct v rs recmap <> recordTemplate rs
  fields = zipWith (makeParseField (lookupAutoStruct v rs recmap)) [1,4..] (map fst rs')
  parseComma = [idoc|
if(! match(json, ",", i))
    throw 800;
whitespace(json, i);|]

recordTemplate :: [(MDoc, (PVar, Maybe TypeP))] -> MDoc
recordTemplate rs = case [v | (v, (_, Nothing)) <- rs] of
  [] -> ""
  ts -> encloseSep "<" ">" "," ts

makeParseField :: MDoc -> Int -> MDoc -> MDoc
makeParseField rname i field = [idoc|
if(! match(json, "\"#{field}\"", i))
    throw #{pretty i};
whitespace(json, i);
if(! match(json, ":", i))
    throw #{pretty (i+1)};
whitespace(json, i);
if(! deserialize(json, i, #{rname}_#{field}))
    throw #{pretty (i+2)};
x.#{field} = #{rname}_#{field};
whitespace(json, i);|]



makeMain :: [MDoc] -> [MDoc] -> [MDoc] -> [MDoc] -> MDoc -> MDoc
makeMain includes signatures serialization manifolds dispatch = [idoc|#include <string>
#include <iostream>
#include <sstream>
#include <functional>
#include <vector>
#include <string>
#include <algorithm> // for std::transform

#{Src.foreignCallFunction}

#{Src.serializationHandling}

#{vsep includes}

#{vsep signatures}

#{vsep serialization}

#{vsep manifolds}

int main(int argc, char * argv[])
{
    int cmdID;
    #{serialType} result;
    cmdID = std::stoi(argv[1]);
    #{dispatch}
    std::cout << result << std::endl;
    return 0;
}
|]
