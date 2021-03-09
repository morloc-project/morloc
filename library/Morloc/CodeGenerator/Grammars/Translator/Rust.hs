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

import Morloc.CodeGenerator.Namespace

preprocess :: ExprM Many -> MorlocMonad (ExprM Many)
preprocess = undefined

translate :: [Source] -> [ExprM One] -> MorlocMonad MDoc
translate = undefined
