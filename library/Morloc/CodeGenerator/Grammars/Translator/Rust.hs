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

-- -- | Compile a Rust program
-- rustBuild :: Path -> Script -> MorlocMonad ()
-- rustBuild = undefined
-- -- rustBuild exeName s = do
-- --   let poolCargoDir = MS.dropExtensions exeName <> "-mod"
-- --   liftIO $ SD.createDirectoryIfMissing True poolCargoDir

preprocess :: ExprM Many -> MorlocMonad (ExprM Many)
preprocess = invertExprM

translate :: [Source] -> [ExprM One] -> MorlocMonad Script
translate srcs es = do
  code <- makePool es

  maker <- makeTheMaker srcs 

  return $ Script
    { scriptBase = "pool"
    , scriptLang = RustLang 
    , scriptCode = "." :/ File "pool.rs" (Code . render $ code)
    , scriptMake = maker
    }

makeTheMaker :: [Source] -> MorlocMonad [SysCommand]
makeTheMaker = undefined

makePool :: [ExprM One] -> MorlocMonad MDoc
makePool _ = return [idoc|// Rust template
use rustmorlocinternals::serial;
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

pub fn m0(x0: &str) -> String {
    let a0: String = serial::deserialize(x0).into();
    let a1 = morloc_id(&a0);
    let a2 = serial::serialize(&a1);
    a2
}

fn main() {
    let args = env::args();
    args.next();
    let cmd_id: u64 = args.next().unwrap().parse().unwrap();
    let result;
    match cmd_id {
        0 => result = m0(&args.next().unwrap()),
        _ => panic!("invalid function!")
    }
    println!("{}", result);
}
|]


{- Example 2

// Translated from the python file

// py2c x y = add (mul x y) 100
fn m0(x0: &str, x1: &str) -> String {
    let a0: f64 = serial::deserialize(x0).into();
    let a1: f64 = serial::deserialize(x1).into();
    let a2 = morloc_add(a0, a1);
    let a3 = serial::serialize(a2);
    a3
}

// c2py x y = mul (add x y) 100
fn m1(x0: &str, x1: &str) -> String {
    let a0 = foreign_call("./pool-cpp.out", &["13", x0, x1]);
    let a1: f64 = serial::deserialize(x0).into();
    let a2 = morloc_add(a1, 100.0);
    let a3 = serial::serialize(a2);
    a3
}

fn main() {
    let args = env::args();
    args.next();
    let cmd_id: u64 = args.next().unwrap().parse().unwrap();
    let result;
    match cmd_id {
        0 => result = m0(&args.next().unwrap(), &args.next().unwrap()),
        1 => result = m1(&args.next().unwrap(), &args.next().unwrap()),
        _ => panic!("invalid function!")
    }
    println!("{}", result);
}
 - -}
