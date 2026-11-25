{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec.Lexical
  ( lexTests
  ) where

import Test.Tasty (TestTree, testGroup)
import Text.RawString.QQ
import Spec.Common

lexTests :: TestTree
lexTests =
  testGroup
    "LEX"
    [ accept
        "spec-lit-5-2"
        [r|
        module main (a, b, c)
        a :: Str
        a = "it\'s \"quoted\""
        b :: Str
        b = '''it\'s'''
        c :: Str
        c = """say \"hi\" and \'bye\'"""
        |]
    , reject
        "spec-lit-5-1"
        [r|
        module main (g)
        g :: Str
        g = "x""
        |]
    , accept
        "spec-lex-4-1"
        [r|
        module main (a, b, c, d, e)
        effect IO
        record R where
          on :: Bool
          n :: Int
        a :: Int -> <IO> R
        a x = do
          {on = True, n = x}
        b :: Int -> <IO> R
        b x = do {on = False, n = x}
        c :: Bool -> <IO> R
        c t = do
          ? t = do
              {on = False, n = 0}
          : do
              {on = True, n = 1}
        d :: Int -> <IO> Int
        d x = do { let y = x; y }
        e :: Int -> <IO> R
        e x = do { let y = x; {on = True, n = y} }
        |]
    , reject
        "spec-lex-4-2"
        [r|
        module main (d)
        effect IO
        d :: Int -> <IO> Int
        d x = do { y = x; y }
        |]
    , accept
        "spec-lex-4-3"
        [r|
        module main (a, b, c, d, e, f, g, h)
        effect IO
        record R where
          on :: Bool
          n :: Int
        source Py from "f.py" ("bump")
        bump :: Int -> <IO> Int
        a :: Int -> (<IO> Int, Int)
        a x = (do bump x, 1)
        b :: Bool -> <IO> Int
        b t = ? t = do bump 0 : do bump 1
        c :: Int -> <IO> Int
        c x = do { k <- bump x; r <- do bump k; r }
        d :: Int -> <IO> Int
        d x = do bump y where y = x
        e :: Int -> (<IO> R, Int)
        e x = (do {on = True, n = x}, 1)
        f :: Int -> <IO> Int
        f y = let a = do bump y in a
        g :: Bool -> Bool -> <IO> Int
        g s t = ? s = do bump 0 ? t = bump 1 : bump 2
        data T = A | B
        h :: T -> <IO> Int
        h | A = do bump 0 | B = bump 1
        |]
    , accept
        "spec-lex-4-4"
        [r|
        module main (a, b)
        effect IO
        record R where
          on :: Bool
          n :: Int
        source Py from "f.py" ("bump")
        bump :: Int -> <IO> Int
        effect Err
        bumpE :: Int -> <IO, Err> Int
        a :: Int -> <IO, Err> Int
        a x = (do bumpE x :: <IO, Err> Int)
        b :: Bool -> Int -> <IO> Int
        b t x = do
          ? t = do bump x
          : bump 0
        |]
    , accept
        "spec-lex-4-6"
        [r|
        effect IO
        source Py from "f.py" ("bump")
        bump :: Int -> <IO> Int
        (do bump 1, 2)
        |]
    , rejectNaming
        "spec-lex-4-5"
        ["unexpected", "further"]
        []
        [r|
        module main (a)
        effect IO
        record R where
          on :: Bool
          n :: Int
        source Py from "f.py" ("bump")
        bump :: Int -> <IO> Int
        a :: Int -> (<IO> Int, Int)
        a x = (do bump x, , 1)
        |]
    , rejectNaming
        "spec-lex-4-7"
        ["reserved"]
        ["further"]
        [r|
        module main (f)
        firstStr :: [Str] -> Str
        --' Count
        --'
        --' @render -r/--raw=firstStr
        f :: Int -> [Str]
        f n = []
        |]
    , rejectNaming
        "spec-lex-4-8"
        ["structt"]
        ["further"]
        [r|
        module main (f)
        record P where
          x :: Int
        record Cpp => P = structt
        f :: Int -> Int
        f x = x
        |]
    , accept
        "spec-src-2-1"
        [r|
        module main (f)
        source Py from "f.py" where
          --' @name plus
          add
        add :: Int -> Int
        f :: Int -> Int
        f x = add x
        |]
    , reject
        "spec-src-2-2"
        [r|
        module main (f)
        source Py from "f.py"
          ( "minus"
          --' @name plus
          , "minus" as add
          )
        add :: Int -> Int
        minus :: Int -> Int
        f :: Int -> Int
        f x = add (minus x)
        |]
    , reject
        "spec-src-2-3"
        [r|
        module main (f)
        source Py from "f.py" where
          --' @name plus
          `and`
        f :: Int -> Int
        f x = x
        |]
    , accept
        "spec-doc-1-2"
        [r|
        module main (f)
        source Py from "f.py"
          ( "h"
          --' Add two
          , "g"
          )
        g :: Int -> Int
        h :: Int -> Int
        f :: Int -> Int
        f x = g (h x)
        |]
    , reject
        "spec-doc-1-3"
        [r|
        module main (f)
        source Py from "f.py"
          ( "h"
          --' Add two
          ,
          --' Add three
          "g"
          )
        g :: Int -> Int
        h :: Int -> Int
        f :: Int -> Int
        f x = g (h x)
        |]
    ]
