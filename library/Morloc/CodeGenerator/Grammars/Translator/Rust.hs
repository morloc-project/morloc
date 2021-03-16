{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.CodeGenerator.Grammars.Translator.Rust
Description : Rust translator
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.CodeGenerator.Grammars.Translator.Rust
  ( 
    translate
  , preprocess
  ) where

import Morloc.Namespace
import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.Grammars.Common
import Morloc.Quasi
import Morloc.Data.Doc
import qualified Morloc.CodeGenerator.Serial as Serial
import qualified Morloc.Monad as MM
import qualified Morloc.System as MS

preprocess :: ExprM Many -> MorlocMonad (ExprM Many)
preprocess = invertExprM

translate :: [Source] -> [ExprM One] -> MorlocMonad Script
translate srcs es = do

  deps <- getDependencies srcs

  let imports = map (makeImport . pretty . fst) deps

  cargo <- makeCargo deps

  manifolds <- mapM translateManifold es

  code <- makePool manifolds imports es

  maker <- makeTheMaker srcs 

  return $ Script
    { scriptBase = "pool"
    , scriptLang = RustLang 
    , scriptCode = "." :/ pkg cargo code
    , scriptMake = maker
    }
  where
    pkg cargo code = Dir "pool-rust"
      [ File "Cargo.toml" cargo
      , Dir "src"
        [ File "main.rs" code
        ]
      ]

makeImport :: MDoc -> MDoc
makeImport n = "use" <+> n <> ";"

translateManifold :: ExprM One -> MorlocMonad MDoc
translateManifold m0@(ManifoldM _ args0 _) = do
  MM.startCounter
  (vsep . punctuate line . (\(x,_,_)->x)) <$> f args0 m0
  where

  f :: [Argument]
    -> ExprM One
    -> MorlocMonad
        ( [MDoc] -- completely generated manifolds
        , MDoc   -- a tag for the returned expression
        , [MDoc] -- lines to precede the returned expression
        )
  f pargs m@(ManifoldM (metaId->i) args e) = do
    (ms', e', rs') <- f args e
    let mname = manNamer i
        def = "fn" <+> mname <> tupled (map makeArgument args) <+> "->" <+> showTypeM (typeOfExprM e)
        body = vsep $ rs' ++ [e']
        mdoc = block 4 def body
    call <- return $ case (splitArgs args pargs, nargsTypeM (typeOfExprM m)) of
      ((rs, []), _) -> mname <> tupled (map makeArgument rs) -- covers #1, #2 and #4
      (([], _ ), _) -> mname
      ((rs, vs), _) -> makeLambda vs (mname <> tupled (map makeArgument (rs ++ vs))) -- covers #5
    return (mdoc : ms', call, [])

  f _ (PoolCallM _ _ cmds args) = do
    let call = "foreign_call(" <> list(map dquotes cmds ++ map makeArgument args) <> ")"
    return ([], call, [])

  f _ (ForeignInterfaceM _ _) = MM.throwError . CallTheMonkeys $
    "Foreign interfaces should have been resolved before passed to the translators"

  f args (LetM i (DeserializeM s e1) e2) = do
    (ms1, e1', ps1) <- f args e1
    (ms2, e2', ps2) <- f args e2
    deserialized <- deserialize e1' s
    t <- showTypeP <$> (Serial.serialAstToType s)
    let def = [idoc|let #{letNamer i}: #{t} = #{deserialized};|]
    return (ms1 ++ ms2, vsep $ ps1 ++ [def] ++ ps2 ++ [e2'], [])

  f args (LetM i e1 e2) = do
    (ms1', e1', rs1) <- (f args) e1
    (ms2', e2', rs2) <- (f args) e2
    let rs = rs1 ++ [ "let" <+> letNamer i <+> "=" <+> e1' <> ";" ] ++ rs2
    return (ms1' ++ ms2', e2', rs)

  f args (AppM (SrcM _ src) xs) = do
    (mss', xs', rss') <- mapM (f args) xs |>> unzip3
    return (concat mss', pretty (srcName src) <> tupled xs', concat rss')

  f _ (AppM _ _) = error "Can only apply functions"

  f _ (SrcM _ src) = return ([], pretty (srcName src), [])

  f _ (LamM _ _) = undefined -- FIXME: this is defined in R

  f _ (BndVarM _ i) = return ([], bndNamer i, [])

  f _ (LetVarM _ i) = return ([], letNamer i, [])

  f args (AccM e k) = do
    (ms, e', ps) <- f args e
    return (ms, e' <> "." <> pretty k, ps)

  f args (ListM _ es) = do
    (mss', es', rss') <- mapM (f args) es |>> unzip3
    return (concat mss', list es', concat rss')

  f args (TupleM _ es) = do
    (mss', es', rss') <- mapM (f args) es |>> unzip3
    return (concat mss', tupled es', concat rss')

  f args (RecordM _ entries) = do
    (mss', es', rss') <- mapM (f args . snd) entries |>> unzip3
    let entries' = zipWith (\k v -> pretty k <> "=" <> v) (map fst entries) es'
    return (concat mss', "RECORD" <> tupled entries', concat rss')

  f _ (LogM _ x) = return ([], if x then "true" else "false", [])

  f _ (NumM _ x) = return ([], viaShow x, [])

  f _ (StrM _ x) = return ([], dquotes $ pretty x, [])

  f _ (NullM _) = undefined -- Rust doesn't technically have a NULL

  f args (SerializeM s e) = do
    (ms, e', rs1) <- f args e
    serialized <- serialize e' s
    return (ms, serialized, rs1)

  f args (DeserializeM s e) = do
    (ms, e', rs1) <- f args e
    deserialized <- deserialize e' s
    return (ms, deserialized, rs1)

  f args (ReturnM e) = do
    (ms, e', rs) <- f args e
    return (ms, "return" <+> e' <> ";", rs) -- returned things are wrapped in nothin'
translateManifold _ = error "Every ExprM object must start with a Manifold term"

makeArgument :: Argument -> MDoc
makeArgument (SerialArgument i _) = bndNamer i <> ":" <+> serialType
makeArgument (NativeArgument i t) = bndNamer i <> ":" <+> showTypeP t
makeArgument (PassThroughArgument i) = bndNamer i <> ":" <+> serialType

showTypeP :: TypeP -> MDoc
showTypeP (UnkP (PV _ _ v)) = pretty v
showTypeP (VarP (PV _ _ v)) = pretty v
showTypeP (FunP t1 t2) = [idoc|FUNCTION(#{showTypeP t1} -> (#{showTypeP t2}))|]
showTypeP (ArrP (PV _ _ v) ts) = pretty v <> encloseSep "<" ">" "," (map showTypeP ts)
showTypeP (NamP _ (PV _ _ v) _ _) = pretty v

showTypeM :: TypeM -> MDoc
showTypeM Passthrough = serialType
showTypeM (Serial _) = serialType
showTypeM (Native t) = showTypeP t
showTypeM (Function ts t)
  = "std::function<" <> showTypeM t
  <> "(" <> cat (punctuate "," (map showTypeM ts)) <> ")>"

serialType :: MDoc
serialType = "&str"

makeLambda :: [Argument] -> MDoc -> MDoc
makeLambda _ _ = "LAMBDA"

-- create an internal variable based on a unique id
letNamer :: Int -> MDoc
letNamer i = "a" <> viaShow i

-- create namer for manifold positional arguments
bndNamer :: Int -> MDoc
bndNamer i = "x" <> viaShow i

-- create a name for a manifold based on a unique id
manNamer :: Int -> MDoc
manNamer i = "m" <> viaShow i

serialize :: MDoc -> SerialAST One -> MorlocMonad MDoc
serialize x _ = return $ [idoc|serial::serialize(&#{x})|]

deserialize :: MDoc -> SerialAST One -> MorlocMonad MDoc
deserialize x _ = return $ [idoc|serial::deserialize(#{x}).into()|]

makeCargo :: [(String, Path)] -> MorlocMonad Code 
makeCargo deps = do
  homedir <- liftIO MS.getHomeDirectory
  let guts = MS.combine homedir ".morloc/dev/rustmorlocinternals"
  return . Code . render $ [idoc|[package]
name = "pool-rs"
version = "0.1.0"
authors = ["H. G. Wells"]
edition = "2018"

[dependencies]
rustmorlocinternals = { path = "#{pretty guts}" }
#{vsep (map makeDependency deps)}
|]

-- An example of a dependency entry:
--    rustbase = { path = "/home/username/.morloc/src/rustbase" }
makeDependency :: (String, Path) -> MDoc
makeDependency (pkgName, path) = do
  [idoc|#{pretty pkgName} = { path = "#{pretty path}" }|]

getDependencies :: [Source] -> MorlocMonad [(String, Path)]
getDependencies srcs = do
  paths <- liftIO . mapM MS.canonicalizePath . unique . catMaybes . map srcPath $ srcs
  names <- mapM findPkgName paths
  return $ zip names paths

findPkgName :: Path -> MorlocMonad String
findPkgName path = do
  isDirectory <- liftIO $ MS.doesDirectoryExist path
  -- if is directory and has Cargo.toml file
  -- then assume it is a Rust crate with the package name matching the bottom folder
  
  dir <- case isDirectory of
    True -> liftIO $ MS.readDirectory path
    False -> MM.throwError . PoolBuildError . render $
      "Expected a Rust crate directory, found:" <+> squotes (pretty path)

  pkgName <- case dir of
    (_ :/ Failed _ ioerr) -> MM.throwError . PoolBuildError . render
      $ "While trying to load what I thought as a Rust crate at"
      <+> squotes (pretty path)
      <+> "I encountered the following IO exception:"
      <+> viaShow ioerr
    (_ :/ (File _ _)) -> MM.throwError . PoolBuildError $ "Expected directory"
    (_ :/ Dir modName fs) -> if elem "Cargo.toml" [x | File x _ <- fs]
      then return modName
      else MM.throwError . PoolBuildError . render $
          "Expected" <+> squotes (pretty path) <+> "to be a Rust crate, but found no Cargo.toml file"

  return pkgName


makeTheMaker :: [Source] -> MorlocMonad [SysCommand]
makeTheMaker _ = return
  [ SysRun (Code "cd pool-rust && cargo build && cd ..")
  , SysRun (Code "cp pool-rust/target/debug/pool-rs pool-rust.out")
  ]

makeDispatch :: MDoc -> MDoc -> [ExprM One] -> MDoc
makeDispatch varin varout ms
  = block 4 ("match" <+> varin) (vsep (map makeCase ms ++ [defaultCase]))
  where
    makeCase :: ExprM One -> MDoc
    makeCase (ManifoldM (metaId->i) args _) =
        let args' = take (length args) $ repeat "&args.next().unwrap()"
        in [idoc|#{viaShow i} => #{varout} = #{manNamer i}#{tupled args'},|]
    makeCase _ = error "Every ExprM must start with a manifold object"

    defaultCase = [idoc|_ => panic!("invalid function!")|]

makePool :: [MDoc] -> [MDoc] -> [ExprM One] -> MorlocMonad Code
makePool manifolds imports es = return . Code . render $ [idoc|// Rust template
use rustmorlocinternals::serial;
#{vsep imports}
use std::env;
use std::process::{Command, Stdio};

fn foreign_call(cmd: &str, args: &[&str]) -> String {
    let mut cmd = Command::new(cmd);
    for arg in args {
        cmd.arg(*arg);
    }
    cmd.stdout(Stdio::piped());
    cmd.spawn().unwrap().wait().unwrap();
    String::from_utf8(cmd.output().unwrap().stdout).unwrap()
}

#{vsep manifolds}

fn main() {
    let mut args = env::args();
    args.next();
    let cmd_id: u64 = args.next().unwrap().parse().unwrap();
    let result;
    #{makeDispatch "cmd_id" "result" es}
    println!("{}", result);
}
|] where
