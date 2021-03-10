{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.CodeGenerator.Grammars.Translator.Rust
Description : Rust translator
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

{-
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
