{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

{-|
Module      : Morloc.CodeGenerator.Grammars.Translator.PseudoCode
Description : Python3 translator
Copyright   : (c) Zebulun Arendsee, 2021
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

prettyFoldManifold = FoldManifoldM
 { opSerialManifoldM = makeSerialManifold
 , opNativeManifoldM = makeNativeManifold
 , opSerialExprM     = makeSerialExpr
 , opNativeExprM     = makeNativeExpr
 , opSerialArgM      = makeSerialArg
 , opNativeArgM      = makeNativeArg
 } where

    makeSerialManifold :: Monad m => SerialManifold_ PoolDocs -> m PoolDocs
    makeSerialManifold (SerialManifold_ m _ form x) = translateManifold "SerialManifold" m form x

    makeNativeManifold :: Monad m => NativeManifold_ PoolDocs -> m PoolDocs
    makeNativeManifold (NativeManifold_ m _ form (_, x)) = translateManifold "NativeManifold" m form x

    makeSerialExpr :: Monad m => SerialExpr_ PoolDocs PoolDocs PoolDocs PoolDocs PoolDocs -> m PoolDocs
    makeSerialExpr (AppManS_ f _) = return f
    -- makeSerialExpr (AppManS_ f (map catEither -> rs)) = return $ mergePoolDocs ((<>) (poolExpr f) . tupled) (f : rs)
    makeSerialExpr (AppPoolS_ (PoolCall _ cmds _) args) = return $ mergePoolDocs makePoolCall args
        where
        makePoolCall xs' = "__foreign_call__(" <> list(map dquotes cmds ++ xs') <> ")"
    makeSerialExpr (ReturnS_ x) = return $ x {poolExpr = "ReturnS(" <> poolExpr x <> ")"}
    makeSerialExpr (SerialLetS_ i e1 e2) = return $ makeLet letNamerS "SerialLetS" i e1 e2
    makeSerialExpr (NativeLetS_ i (_, e1) e2) = return $ makeLet letNamerN "NativeLetS" i e1 e2
    makeSerialExpr (LetVarS_ i) = return $ PoolDocs [] (letNamerS i) [] []
    makeSerialExpr (BndVarS_ i) = return $ PoolDocs [] (bndNamerS i) [] []
    makeSerialExpr (SerializeS_ _ e) = return $ e {poolExpr = "SerializeS" <> parens (poolExpr e)}

    makeNativeExpr :: Monad m => NativeExpr_ PoolDocs PoolDocs PoolDocs PoolDocs PoolDocs -> m PoolDocs
    makeNativeExpr (AppSrcN_      _ (pretty . srcName -> functionName) xs) =
        return $ mergePoolDocs ((<>) functionName . tupled) xs
    makeNativeExpr (AppManN_      _ call _) = return call 
    -- makeNativeExpr (AppManN_      _ call (map catEither -> xs)) =
    --     return $ mergePoolDocs ((<>) (poolExpr call) . tupled) (call : xs)
    makeNativeExpr (ReturnN_      _ x) =
        return $ x { poolExpr = "ReturnN(" <> poolExpr x <> ")" }
    makeNativeExpr (SerialLetN_     i x1 (_, x2)) = return $ makeLet letNamerS "SerialLetN" i x1 x2
    makeNativeExpr (NativeLetN_     i (_, x1) (_, x2)) = return $ makeLet letNamerN "NativeLetN" i x1 x2
    makeNativeExpr (LetVarN_      _ i) = return $ PoolDocs [] (letNamerN i) [] []
    makeNativeExpr (BndVarN_      _ i) = return $ PoolDocs [] (bndNamerN i) [] []
    makeNativeExpr (DeserializeN_ _ _ e) = return $ e {poolExpr = "DeserializeN" <> parens (poolExpr e)}
    makeNativeExpr (AccN_         _ _ _ e k) = return $ e {poolExpr = poolExpr e <> "[" <> dquotes (pretty k) <> "]"}
    makeNativeExpr (SrcN_         _ src) = return $ PoolDocs [] (pretty (srcName src)) [] []
    makeNativeExpr (ListN_        _ _ xs) = return $ mergePoolDocs list xs
    makeNativeExpr (TupleN_       _ xs) = return $ mergePoolDocs tupled (map snd xs)
    makeNativeExpr (RecordN_      _ _ _ rs)
        = return $ mergePoolDocs pyDict (map (\(_, (_, x)) -> x) rs)
        where
            pyDict es' =
                let entries' = zipWith (\(FV _ k) v -> pretty k <> "=" <> v) (map fst rs) es'
                in "OrderedDict" <> tupled entries'
    makeNativeExpr (LogN_         _ v) = return $ PoolDocs [] (if v then "True" else "False") [] []
    makeNativeExpr (RealN_        _ v) = return $ PoolDocs [] (viaShow v) [] []
    makeNativeExpr (IntN_         _ v) = return $ PoolDocs [] (viaShow v) [] []
    makeNativeExpr (StrN_         _ v) = return $ PoolDocs [] (dquotes $ pretty v) [] []
    makeNativeExpr (NullN_        _)   = return $ PoolDocs [] "None" [] []

    makeSerialArg :: Monad m => SerialArg_ PoolDocs PoolDocs -> m PoolDocs
    makeSerialArg (SerialArgManifold_ x) = return x
    makeSerialArg (SerialArgExpr_ x) = return x

    makeNativeArg :: Monad m => NativeArg_ PoolDocs PoolDocs -> m PoolDocs
    makeNativeArg (NativeArgManifold_ x) = return x
    makeNativeArg (NativeArgExpr_ x) = return x

    translateManifold :: Monad m => MDoc -> Int -> ManifoldForm TypeM -> PoolDocs -> m PoolDocs
    translateManifold manStr m form (PoolDocs completeManifolds body priorLines priorExprs) = do
      let args = manifoldArgs form
      let mname = manNamer m
          def = manStr <+> mname <> tupled [argName r <+> ":" <+> pretty t | r@(Arg _ t) <- args] <> ":"
          newManifold = block 4 def (vsep $ priorLines <> [body])
          call = case form of
            (ManifoldFull rs) -> mname <> tupled (map argName rs) -- covers #1, #2 and #4
            (ManifoldPass _) -> mname
            (ManifoldPart rs vs) -> makeLambda vs (mname <> tupled (map argName (rs ++ vs))) -- covers #5
      return $ PoolDocs
          { poolCompleteManifolds = newManifold : completeManifolds
          , poolExpr = call
          , poolPriorLines = []
          , poolPriorExprs = priorExprs
          }

    makeLet :: (Int -> MDoc) -> MDoc -> Int -> PoolDocs -> PoolDocs -> PoolDocs
    makeLet letNamer letStr i (PoolDocs ms1' e1' rs1 pes1) (PoolDocs ms2' e2' rs2 pes2) =
      let rs = rs1 ++ [ letStr <+> letNamer i <+> "=" <+> e1' ] ++ rs2
      in PoolDocs (ms1' <> ms2') e2' rs (pes1 <> pes2)

    letNamerS :: Int -> MDoc
    letNamerS i = "letSvar_" <> pretty i

    letNamerN :: Int -> MDoc
    letNamerN i = "letNvar_" <> pretty i

    manNamer :: Int -> MDoc
    manNamer i = "m" <> pretty i

    bndNamerS :: Int -> MDoc
    bndNamerS i = "bndSvar" <> pretty i

    bndNamerN :: Int -> MDoc
    bndNamerN i = "bndNvar" <> pretty i

    argName :: Arg TypeM -> MDoc
    argName (Arg i (Native _)) = bndNamerN i
    argName (Arg i _) = bndNamerS i

    makeLambda :: [Arg TypeM] -> MDoc -> MDoc
    makeLambda args body = "lambda" <+> hsep (punctuate "," (map argName args)) <> ":" <+> body


prettyThing :: (FoldManifoldM MI.Identity PoolDocs PoolDocs PoolDocs PoolDocs PoolDocs PoolDocs -> a -> MI.Identity PoolDocs) -> a -> MDoc
prettyThing f a =
  let e = MI.runIdentity $ f prettyFoldManifold a
  in vsep . punctuate line $ poolPriorExprs e <> poolCompleteManifolds e

pseudocodeNativeManifold :: NativeManifold -> MDoc
pseudocodeNativeManifold = prettyThing foldNativeManifoldM

pseudocodeSerialManifold :: SerialManifold -> MDoc
pseudocodeSerialManifold = prettyThing foldSerialManifoldM

pseudocodeSerialArg :: SerialArg -> MDoc
pseudocodeSerialArg = prettyThing foldSerialArgM

pseudocodeNativeArg :: NativeArg -> MDoc
pseudocodeNativeArg = prettyThing foldNativeArgM

pseudocodeSerialExpr :: SerialExpr -> MDoc
pseudocodeSerialExpr = prettyThing foldSerialExprM

pseudocodeNativeExpr :: NativeExpr -> MDoc
pseudocodeNativeExpr = prettyThing foldNativeExprM
