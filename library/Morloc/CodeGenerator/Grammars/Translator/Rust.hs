{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.CodeGenerator.Grammars.Translator.Rust
Description : Rust translator
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

{- Example 1
use super::serial;
use std::env;

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
 - -}

{- Example 2
use std::env;
use std::process::{Command, Stdio};

use super::serial;

fn foreign_call(cmd: &str, args: &[&str]) -> String {
    let mut cmd = Command::new(cmd);
    for arg in args {
        cmd.arg(*arg);
    }
    cmd.stdout(Stdio::piped());
    cmd.spawn().unwrap().wait().unwrap();
    String::from_utf8(cmd.output().unwrap().stdout).unwrap()
}

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

module Morloc.CodeGenerator.Grammars.Translator.Rust
  ( 
    translate
  , preprocess
  ) where

import Morloc.CodeGenerator.Namespace

preprocess :: ExprM Many -> MorlocMonad (ExprM Many)
preprocess = undefined

translate :: [Source] -> [ExprM One] -> MorlocMonad MDoc
translate = undefined
