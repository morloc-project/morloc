{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

{-|
Module      : Morloc.CodeGenerator.Grammars.Translator.PseudoCode
Description : Python3 translator
Copyright   : (c) Zebulun Arendsee, 2016-2024
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.CodeGenerator.Grammars.Translator.PseudoCode
  ( pseudocodeNativeManifold
  , pseudocodeSerialManifold
  , pseudocodeSerialArg
  , pseudocodeNativeArg
  , pseudocodeSerialExpr
  , pseudocodeNativeExpr
  ) where

import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.Grammars.Common
import Morloc.Data.Doc
import qualified Control.Monad.Identity as MI

prettyFoldManifold :: (Monad m) => FoldWithManifoldM m PoolDocs PoolDocs PoolDocs PoolDocs PoolDocs PoolDocs
prettyFoldManifold = FoldWithManifoldM
 { opFoldWithSerialManifoldM = makeSerialManifold
 , opFoldWithNativeManifoldM = makeNativeManifold
 , opFoldWithSerialExprM     = makeSerialExpr
 , opFoldWithNativeExprM     = makeNativeExpr
 , opFoldWithSerialArgM      = makeSerialArg
 , opFoldWithNativeArgM      = makeNativeArg
 } where

    makeSerialManifold :: Monad m => SerialManifold -> SerialManifold_ PoolDocs -> m PoolDocs
    makeSerialManifold _ (SerialManifold_ m _ form headForm x) = return
      $ translateManifold (makeFunction "SerialManifold") makeLambda m form (Just headForm) x

    makeNativeManifold :: Monad m => NativeManifold -> NativeManifold_ PoolDocs -> m PoolDocs
    makeNativeManifold _ (NativeManifold_ m _ form x) = return
      $ translateManifold (makeFunction "NativeManifold") makeLambda m form Nothing x

    makeSerialExpr :: Monad m => SerialExpr -> SerialExpr_ PoolDocs PoolDocs PoolDocs PoolDocs PoolDocs -> m PoolDocs
    makeSerialExpr _ (ManS_ m) = return m
    makeSerialExpr _ (AppPoolS_ t (PoolCall mid (Socket _ _ socketFile) remote _) args) = return $ mergePoolDocs makePoolCall args
        where
        makePoolCall xs' = case remote of
            ForeignCall -> parens (pretty t) <+> "__foreign_call__" <> tupled [dquotes socketFile, dquotes (pretty mid), list xs']
            (RemoteCall _) -> "REMOTE_CALL"
    makeSerialExpr _ (ReturnS_ x) = return $ x {poolExpr = "ReturnS(" <> poolExpr x <> ")"}
    makeSerialExpr _ (SerialLetS_ i e1 e2) = return $ makeLet letNamerS "SerialLetS" i e1 e2
    makeSerialExpr _ (NativeLetS_ i e1 e2) = return $ makeLet letNamerN "NativeLetS" i e1 e2
    makeSerialExpr _ (LetVarS_ _ i) = return $ defaultValue { poolExpr = letNamerS i }
    makeSerialExpr _ (BndVarS_ _ i) = return $ defaultValue { poolExpr = bndNamerS i }
    makeSerialExpr _ (SerializeS_ _ e) = return $ e {poolExpr = "SerializeS" <> parens (poolExpr e)}

    makeNativeExpr :: Monad m => NativeExpr -> NativeExpr_ PoolDocs PoolDocs PoolDocs PoolDocs PoolDocs -> m PoolDocs
    makeNativeExpr _ (AppSrcN_ _ (pretty . srcName -> functionName) _ xs) =
      return $ mergePoolDocs ((<>) functionName . tupled) xs
    makeNativeExpr _ (ManN_ call) = return call
    makeNativeExpr _ (ReturnN_ x) =
      return $ x { poolExpr = "ReturnN(" <> poolExpr x <> ")" }
    makeNativeExpr _ (SerialLetN_ i x1 x2) = return $ makeLet letNamerS "SerialLetN" i x1 x2
    makeNativeExpr _ (NativeLetN_ i x1 x2) = return $ makeLet letNamerN "NativeLetN" i x1 x2
    makeNativeExpr _ (LetVarN_ _ i) = return $ defaultValue { poolExpr = letNamerN i }
    makeNativeExpr _ (BndVarN_ _ i) = return $ defaultValue { poolExpr = bndNamerN i }
    makeNativeExpr _ (DeserializeN_ _ _ e) = return $ e {poolExpr = "DeserializeN" <> parens (poolExpr e)}
    makeNativeExpr _ (AccN_ _ _ e k) = return $ e {poolExpr = poolExpr e <> "[" <> dquotes (pretty k) <> "]"}
    makeNativeExpr _ (SrcN_ _ src) = return $ defaultValue { poolExpr = pretty (srcName src) }
    makeNativeExpr _ (ListN_ _ _ xs) = return $ mergePoolDocs list xs
    makeNativeExpr _ (TupleN_ _ xs) = return $ mergePoolDocs tupled xs
    makeNativeExpr _ (RecordN_ _ _ _ rs) =
      return $ mergePoolDocs pyDict (map snd rs) where
        pyDict es' =
          let entries' = zipWith (\k v -> pretty k <> "=" <> v) (map fst rs) es'
          in "OrderedDict" <> tupled entries'
    makeNativeExpr _ (LogN_ _ v) = return $ defaultValue { poolExpr = if v then "True" else "False" }
    makeNativeExpr _ (RealN_ _ v) = return $ defaultValue { poolExpr = viaShow v }
    makeNativeExpr _ (IntN_ _ v) = return $ defaultValue { poolExpr = viaShow v }
    makeNativeExpr _ (StrN_ _ v) = return $ defaultValue { poolExpr = dquotes $ pretty v }
    makeNativeExpr _ (NullN_ _ ) = return $ defaultValue { poolExpr = "None" }

    makeSerialArg :: Monad m => SerialArg -> SerialArg_ PoolDocs PoolDocs -> m PoolDocs
    makeSerialArg _ (SerialArgManifold_ x) = return x
    makeSerialArg _ (SerialArgExpr_ x) = return x

    makeNativeArg :: Monad m => NativeArg -> NativeArg_ PoolDocs PoolDocs -> m PoolDocs
    makeNativeArg _ (NativeArgManifold_ x) = return x
    makeNativeArg _ (NativeArgExpr_ x) = return x

    makeFunction :: MDoc -> MDoc -> [Arg TypeM] -> [MDoc] -> MDoc -> Maybe HeadManifoldForm -> MDoc
    makeFunction manStr mname args priorLines body headForm
      = block 4 def (vsep $ priorLines <> [body])
      where
        makeExt (Just HeadManifoldFormRemoteWorker) = "_remote"
        makeExt _ = ""

        def = manStr <+> mname <> makeExt headForm <> tupled [argName r <+> ":" <+> pretty t | r@(Arg _ t) <- args] <> ":"

    makeLambda :: MDoc -> [MDoc] -> [MDoc] -> MDoc
    makeLambda mname contextArgs boundArgs =
          let functionCall = mname <> tupled (contextArgs <> boundArgs)
          in "lambda" <+> tupled boundArgs <> ":" <+> functionCall

    makeLet :: (Int -> MDoc) -> MDoc -> Int -> PoolDocs -> PoolDocs -> PoolDocs
    makeLet letNamer letStr i (PoolDocs ms1' e1' rs1 pes1) (PoolDocs ms2' e2' rs2 pes2) =
      let rs = rs1 ++ [ letStr <+> letNamer i <+> "=" <+> e1' ] ++ rs2
      in PoolDocs (ms1' <> ms2') e2' rs (pes1 <> pes2)

    letNamerS :: Int -> MDoc
    letNamerS i = "letSvar_" <> pretty i

    letNamerN :: Int -> MDoc
    letNamerN i = "letNvar_" <> pretty i

    bndNamerS :: Int -> MDoc
    bndNamerS i = "bndSvar" <> pretty i

    bndNamerN :: Int -> MDoc
    bndNamerN i = "bndNvar" <> pretty i

    argName :: Arg TypeM -> MDoc
    argName (Arg i (Native _)) = bndNamerN i
    argName (Arg i _) = bndNamerS i


prettyThing :: (p -> MI.Identity PoolDocs) -> p -> Doc ()
prettyThing f a =
  let e = MI.runIdentity $ f a
  in vsep . punctuate line $ poolPriorExprs e <> poolCompleteManifolds e

pseudocodeNativeManifold :: NativeManifold -> MDoc
pseudocodeNativeManifold = prettyThing (foldWithNativeManifoldM prettyFoldManifold)

pseudocodeSerialManifold :: SerialManifold -> MDoc
pseudocodeSerialManifold = prettyThing (foldWithSerialManifoldM prettyFoldManifold)

pseudocodeSerialArg :: SerialArg -> MDoc
pseudocodeSerialArg = prettyThing (foldWithSerialArgM prettyFoldManifold)

pseudocodeNativeArg :: NativeArg -> MDoc
pseudocodeNativeArg = prettyThing (foldWithNativeArgM prettyFoldManifold)

pseudocodeSerialExpr :: SerialExpr -> MDoc
pseudocodeSerialExpr = prettyThing (foldWithSerialExprM prettyFoldManifold)

pseudocodeNativeExpr :: NativeExpr -> MDoc
pseudocodeNativeExpr = prettyThing (foldWithNativeExprM prettyFoldManifold)
