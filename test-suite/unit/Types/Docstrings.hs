{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Types.Docstrings
  ( datafileTests
  , epilogueDocstringTests
  , parseDocstringTests
  , withDocstringTests
  , removedDirectiveTests
  ) where

import Text.RawString.QQ

import qualified Data.Text as MT
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit
import Types.Common
import Morloc.Frontend.Desugar (processArgDocLines)
import Morloc.Namespace.Type (ArgDocVars (..), SourceAtom (..))

-- | `@literal` is gone; `@source inline` is its spelling. The old directive
-- is an unrecognized one: it sets nothing and its line stays description.
removedDirectiveTests :: TestTree
removedDirectiveTests =
  testGroup "removed docstring directives"
    [ testCase "@literal is not recognized" $ do
        let (errs, ws, v) = processArgDocLines ["@literal true"]
        assertEqual "errors" [] errs
        assertEqual "one warning" 1 (length ws)
        assertEqual "kept as description" ["@literal true"] (docLines v)
        assertEqual "no source" Nothing (docSource v)
    , testCase "@source inline sets the source" $ do
        let (errs, _, v) = processArgDocLines ["@source inline"]
        assertEqual "errors" [] errs
        assertEqual "source" (Just SourceInline) (docSource v)
    ]

-- | @datafile is resolved at build time, so its argument must be a string
-- literal (directly, or through a name bound to one).
datafileTests :: TestTree
datafileTests =
  testGroup "@datafile path argument"
    [ expectPass "literal path"
        [r|
        module main (f)
        f :: Str
        f = @datafile "x.txt"
        |]
    , expectPass "where-bound literal path"
        [r|
        module main (f)
        f :: Str
        f = @datafile p where
          p = "x.txt"
        |]
    , expectError "parameter as path"
        [r|
        module main (f)
        f :: Str -> Str
        f q = @datafile q
        |]
    ]

-- | Placement of `@epilogue` in a term docstring. A command's epilogue
-- belongs on its signature preamble, like `@with`; anywhere else it has
-- no help screen to render on and is rejected rather than kept as prose.
epilogueDocstringTests :: TestTree
epilogueDocstringTests =
  localOption (mkTimeout 1000000) $ -- 1s
    testGroup
      "@epilogue placement"
      [ expectPass
          "@epilogue on a signature preamble"
          [r|
        module main (foo)
        --' Do a thing
        --' @epilogue
        --' Examples:
        --'   main foo 1
        foo :: Int -> Int
        foo x = x
          |]
      , expectError
          "@epilogue on an argument-level docstring is rejected"
          [r|
        module main (foo)
        foo ::
          --' @epilogue
          --' Examples:
          Int ->
          Int
        foo x = x
          |]
      , expectError
          "@epilogue on an export without a signature is rejected"
          [r|
        module main (foo)
        --' @epilogue
        --' Examples:
        foo = 42
          |]
      ]

-- | Frontend validation of `@parse` on command arguments. Each case varies
-- one part of a small command whose argument is read by `readInts`.
parseDocstringTests :: TestTree
parseDocstringTests =
  localOption (mkTimeout 2000000) $ -- 2s
    testGroup
      "parse: docstring validation"
      [ expectPass "a format with extensions" (parseProg "@parse ints=readInts .ints .ints.gz")
      , expectPass "two formats" (parseProg "@parse ints=readInts .ints\n  --' @parse csv=readInts .csv")
      , expectPass "a format with no extensions" (parseProg "@parse ints=readInts")
      , expectError "missing `=`" (parseProg "@parse ints readInts")
      , expectError "missing handler" (parseProg "@parse ints=")
      , expectError "uppercase format name" (parseProg "@parse Ints=readInts")
      , expectError "format name with `_`" (parseProg "@parse my_ints=readInts")
      , expectError "reserved format name `morloc`" (parseProg "@parse morloc=readInts")
      , expectError "duplicate format name" (parseProg "@parse ints=readInts\n  --' @parse ints=readInts .x")
      , expectError "extension without a leading dot" (parseProg "@parse ints=readInts ints")
      , expectError "uppercase extension" (parseProg "@parse ints=readInts .INTS")
      , expectError "extension claimed by two formats" (parseProg "@parse ints=readInts .txt\n  --' @parse csv=readInts .txt")
      , expectError "extension repeated in one format" (parseProg "@parse ints=readInts .txt .txt")
      , expectError "`@parse` in a signature preamble"
          [r|
        module main (count)
        effect IO
        source Py from "m.py" ("readInts", "size")
        readInts :: Str -> <IO> [Int]
        size :: [Int] -> Int
        --' @parse ints=readInts
        count :: [Int] -> Int
        count xs = size xs
          |]
      , expectError "`@parse` on a `Str` argument" (parseProgT "Str" "@parse s=readStr" "x")
      , expectError "`@parse` on an alias of `Str`" (parseProgT "Path" "@parse s=readStr" "x")
      , expectError "`@parse` on an argument whose type has a type variable"
          [r|
        module main (count)
        effect IO
        data Try e a = Err e | Ok a
        source Py from "m.py" ("readAny", "size")
        readAny :: Str -> <IO> [a]
        size :: [a] -> Int
        count a ::
          --' @parse any=readAny
          [a] ->
          Int
        count xs = size xs
          |]
      , expectError "a handler of the wrong type" (parseProg "@parse ints=size")
      , expectError "an unbound handler" (parseProg "@parse ints=noSuchReader")
      , expectError "`@parse` with `@many`" (parseProg "@parse ints=readInts\n  --' @many")
      , expectError "`@parse` with `@unroll`" (parseProg "@parse ints=readInts\n  --' @unroll")
      , expectError "`@parse` on an `OStream` argument" (parseProgT "OStream Int" "@parse s=readInts" "0")
      , expectError "`@parse` on an `IFile` that is not a list" (parseProgT "IFile Int" "@parse s=readInts" "0")
      , expectError "a stream handler that is not a producer" (parseProgT "IStream Int" "@parse s=readInts" "0")
      , expectPass "a stream producer on an `IStream` argument" (parseProgT "IStream Int" "@parse s=produceInts .txt" "0")
      , expectPass "a stream producer on an `IFile` list argument" (parseProgT "IFile [Int]" "@parse s=produceInts .txt" "0")
      , expectError "a command returning the stream it parses" (parseProgT "IStream Int" "@parse s=produceInts .txt" "x")
      , expectPass "an optional argument" (parseProgT "?[Int]" "@parse ints=readInts .csv" "0")
      , expectError "a user identifier with the reserved `mlcq_` prefix"
          [r|
        module main (mlcq_count)
        mlcq_count :: Int -> Int
        mlcq_count x = x
          |]
      , expectError "`@parse` on a type alias"
          [r|
        module main (count)
        effect IO
        source Py from "m.py" ("readInts", "size")
        readInts :: Str -> <IO> [Int]
        size :: [Int] -> Int
        --' @parse ints=readInts
        type Ints = [Int]
        count :: Ints -> Int
        count xs = size xs
          |]
      ]
  where
    -- one argument of type `t`, read with `directive`; the body returns `body`
    parseProgT :: MT.Text -> MT.Text -> MT.Text -> MT.Text
    parseProgT t directive body = MT.unlines
      [ "module main (cmd)"
      , "effect IO"
      , "data Try e a = Err e | Ok a"
      , "type Path = Str"
      , "source Py from \"m.py\" (\"readInts\", \"readStr\", \"produceInts\")"
      , "readInts :: Str -> <IO> [Int]"
      , "readStr :: Str -> <IO> Str"
      , "produceInts :: Str -> ([Int] -> <IO> ()) -> <IO> ()"
      , "cmd ::"
      , "  --' " <> directive
      , "  " <> t <> " ->"
      , "  " <> (if body == "x" then t else "Int")
      , "cmd x = " <> body
      ]

    parseProg :: MT.Text -> MT.Text
    parseProg directive = MT.unlines
      [ "module main (count)"
      , "effect IO"
      , "data Try e a = Err e | Ok a"
      , "source Py from \"m.py\" (\"readInts\", \"size\")"
      , "readInts :: Str -> <IO> [Int]"
      , "size :: [Int] -> Int"
      , "count ::"
      , "  --' " <> directive
      , "  [Int] ->"
      , "  Int"
      , "count xs = size xs"
      ]

-- | Frontend validation of `--' @with` docstring atoms (terminal
-- actions on CLI-exported commands). One positive sanity check plus
-- coverage of the rejection paths in Frontend/Desugar.hs. Type-level
-- shape is verified by the golden test-suite; here we only care that
-- a legal shape passes and every illegal shape fails.
withDocstringTests :: TestTree
withDocstringTests =
  localOption (mkTimeout 1000000) $ -- 1s
    testGroup
      "with: docstring validation"
      [ -- The whole-list gather materializes the stream and applies the
        -- handler to it once. Only the producer says what the stream holds,
        -- so a handler whose receiver disagrees has to be rejected here --
        -- the gathered bytes would otherwise be read as the wrong type.
        expectError
          "whole-form handler receiver disagreeing with the stream element type"
          [r|
        module main (stream)
        effect IO
        data Try e a = Err e | Ok a
        mk :: Int -> [Int]
        mk _ = [1]
        showStr :: Str -> Str
        showStr s = s
        produce :: ([Int] -> <IO> ()) -> <IO> ()
        produce sink = sink (mk 0)
        --' @render -p/--plain=showStr
        stream :: <IO> (Collect Int)
        stream = @collect produce
          |]

        -- A structurally different disagreement: a tuple where the stream
        -- is a list. This is the shape that reads six list elements as a
        -- two-slot tuple instead of failing.
      , expectError
          "whole-form handler taking a tuple where the stream is a list"
          [r|
        module main (stream)
        effect IO
        data Try e a = Err e | Ok a
        mk :: Int -> [Int]
        mk _ = [1]
        showPair :: (Int, [Int]) -> Str
        showPair _ = "x"
        produce :: ([Int] -> <IO> ()) -> <IO> ()
        produce sink = sink (mk 0)
        --' @render -p/--pair=showPair
        stream :: <IO> (Collect Int)
        stream = @collect produce
          |]

        -- The agreeing handler must still be accepted: the check has to
        -- constrain the gather, not reject it.
      , expectPass
          "whole-form handler agreeing with the stream element type"
          [r|
        module main (stream)
        effect IO
        data Try e a = Err e | Ok a
        mk :: Int -> [Int]
        mk _ = [1]
        showInts :: [Int] -> Str
        showInts _ = "x"
        produce :: ([Int] -> <IO> ()) -> <IO> ()
        produce sink = sink (mk 0)
        --' @render -p/--plain=showInts
        stream :: <IO> (Collect Int)
        stream = @collect produce
          |]

        -- A polymorphic handler is not a disagreement: it accepts the
        -- stream's element type like any other polymorphic function.
      , expectPass
          "polymorphic whole-form handler"
          [r|
        module main (stream)
        effect IO
        data Try e a = Err e | Ok a
        mk :: Int -> [Int]
        mk _ = [1]
        countAny :: [a] -> Int
        countAny _ = 0
        produce :: ([Int] -> <IO> ()) -> <IO> ()
        produce sink = sink (mk 0)
        --' @with -n/--count=countAny
        stream :: <IO> (Collect Int)
        stream = @collect produce
          |]

        -- Sanity: a legal single-atom `@with` composes cleanly.
      , expectPass
          "single with: on effectful command with matching formatter"
          [r|
        module main (foo)
        effect IO
        fmt :: Int -> <IO> ()
        fmt x = fmt x
        --' @with -l/--lines=fmt
        foo :: Int -> <IO> Int
        foo x = do x
          |]

        -- Multiple `@with` lines with distinct short/long are legal.
      , expectPass
          "two with: lines with distinct short and long"
          [r|
        module main (foo)
        effect IO
        fmt :: Int -> <IO> ()
        fmt x = fmt x
        alt :: Int -> <IO> ()
        alt x = alt x
        --' @with -l/--lines=fmt
        --' @with -a/--alt=alt
        foo :: Int -> <IO> Int
        foo x = do x
          |]

        -- A pure terminal (no `<IO>`) on an effectful parent is legal.
        -- The synthesized entry inherits the parent's effect via the
        -- do-block; the terminal itself contributes no effect.
      , expectPass
          "pure terminal on effectful parent"
          [r|
        module main (foo)
        effect IO
        showInt :: Int -> Str
        showInt x = showInt x
        --' @with -s/--show=showInt
        foo :: Int -> <IO> Int
        foo x = do x
          |]

        -- A pure terminal on a pure parent is legal. Composed entry is
        -- pure `Int -> Str`; no effect is invented.
      , expectPass
          "pure terminal on pure parent"
          [r|
        module main (foo)
        showInt :: Int -> Str
        showInt x = showInt x
        --' @with -s/--show=showInt
        foo :: Int -> Int
        foo x = x
          |]

        -- Pure terminal returning () is fine too -- the "sink" shape
        -- doesn't require `<IO>` in the signature; a pure `A -> ()` is
        -- an unusual choice but should typecheck against any parent.
      , expectPass
          "pure terminal returning ()"
          [r|
        module main (foo)
        drop :: Int -> ()
        drop x = ()
        --' @with -d/--drop=drop
        foo :: Int -> Int
        foo x = x
          |]

        -- Duplicate long -> reject.
      , expectError
          "duplicate with: long name"
          [r|
        module main (foo)
        fmt :: Int -> <IO> ()
        fmt x = fmt x
        --' @with -l/--lines=fmt
        --' @with -a/--lines=fmt
        foo :: Int -> <IO> Int
        foo x = do x
          |]

        -- Duplicate short -> reject.
      , expectError
          "duplicate with: short name"
          [r|
        module main (foo)
        fmt :: Int -> <IO> ()
        fmt x = fmt x
        --' @with -l/--lines=fmt
        --' @with -l/--alt=fmt
        foo :: Int -> <IO> Int
        foo x = do x
          |]

        -- Short-only spec (`-l` without `--long`) -> reject.
      , expectError
          "with: short-only spec is rejected"
          [r|
        module main (foo)
        fmt :: Int -> <IO> ()
        fmt x = fmt x
        --' @with -l=fmt
        foo :: Int -> <IO> Int
        foo x = do x
          |]

        -- Reserved long `--help` -> reject.
      , expectError
          "with: --help collides with reserved flag"
          [r|
        module main (foo)
        fmt :: Int -> <IO> ()
        fmt x = fmt x
        --' @with --help=fmt
        foo :: Int -> <IO> Int
        foo x = do x
          |]

        -- Reserved short `-h` -> reject.
      , expectError
          "with: -h collides with reserved flag"
          [r|
        module main (foo)
        fmt :: Int -> <IO> ()
        fmt x = fmt x
        --' @with -h/--halt=fmt
        foo :: Int -> <IO> Int
        foo x = do x
          |]

        -- `@with` on argument-level docstring -> reject (command-only).
      , expectError
          "with: on argument-level docstring is rejected"
          [r|
        module main (foo)
        fmt :: Int -> <IO> ()
        fmt x = fmt x
        foo ::
          --' @with -l/--lines=fmt
          Int ->
          <IO> Int
        foo x = x
          |]

        -- `@with` on export without a signature -> reject.
      , expectError
          "with: on export without signature is rejected"
          [r|
        module main (foo)
        fmt :: Int -> <IO> ()
        fmt x = fmt x
        --' @with -l/--lines=fmt
        foo = 42
          |]

        -- Malformed value (missing `=` between flag and term) -> reject.
      , expectError
          "with: missing = separator is rejected"
          [r|
        module main (foo)
        fmt :: Int -> <IO> ()
        fmt x = fmt x
        --' @with -l/--lines fmt
        foo :: Int -> <IO> Int
        foo x = do x
          |]

        -- Empty term after `=` -> reject.
      , expectError
          "with: empty term name is rejected"
          [r|
        module main (foo)
        --' @with -l/--lines=
        foo :: Int -> <IO> Int
        foo x = do x
          |]

        -- Long form must be lowercase-kebab. UPPER-cased long is rejected.
      , expectError
          "with: uppercased long name is rejected"
          [r|
        module main (foo)
        fmt :: Int -> <IO> ()
        fmt x = fmt x
        --' @with --Lines=fmt
        foo :: Int -> <IO> Int
        foo x = do x
          |]

        -- Digit short is rejected (would collide with negative-number args).
      , expectError
          "with: digit short is rejected"
          [r|
        module main (foo)
        fmt :: Int -> <IO> ()
        fmt x = fmt x
        --' @with -1/--lines=fmt
        foo :: Int -> <IO> Int
        foo x = do x
          |]

        -- `@with` long clashes with an argument-declared long. Both
        -- would land on the same clap Command; clap panics at build
        -- time. Frontend must reject first.
      , expectError
          "with: long collides with arg-declared long"
          [r|
        module main (foo)
        fmt :: Int -> <IO> ()
        fmt x = fmt x
        --' @with --lines=fmt
        foo ::
          --' @arg --lines
          Int ->
          <IO> Int
        foo x = x
          |]

        -- Same for short.
      , expectError
          "with: short collides with arg-declared short"
          [r|
        module main (foo)
        fmt :: Int -> <IO> ()
        fmt x = fmt x
        --' @with -x/--lines=fmt
        foo ::
          --' @arg -x/--count
          Int ->
          <IO> Int
        foo x = x
          |]

        -- Two `@with` long flags that mangle to the same internal name
        -- (hyphen -> underscore collapse) must be caught, or the
        -- synthesized bindings would clash silently.
      , expectError
          "with: two flags mangling to the same internal name"
          [r|
        module main (foo)
        fmt :: Int -> <IO> ()
        fmt x = fmt x
        --' @with --bar-baz=fmt
        --' @with --bar_baz=fmt
        foo :: Int -> <IO> Int
        foo x = do x
          |]

        -- Synthesized name collides with a user-defined top-level
        -- identifier in the same module.
      , expectError
          "with: mangled entry collides with user identifier"
          [r|
        module main (foo, mlcp_foo_lines)
        fmt :: Int -> <IO> ()
        fmt x = fmt x
        mlcp_foo_lines :: Int -> Int
        mlcp_foo_lines n = n
        --' @with -l/--lines=fmt
        foo :: Int -> <IO> Int
        foo x = do x
          |]

        -- The replay entries synthesized for terminal actions own the
        -- `mlcr_` prefix.
      , expectError
          "with: an identifier may not use the reserved mlcr_ prefix"
          [r|
        module main (foo, mlcr_bar)
        mlcr_bar :: Int -> Int
        mlcr_bar n = n
        foo :: Int -> Int
        foo x = x
          |]

        -- Every command with terminal actions takes `--no-stdout`, so
        -- neither an action nor an argument may use the name.
      , expectError
          "with: --no-stdout is a reserved action name"
          [r|
        module main (foo)
        fmt :: Int -> Str
        --' @render --no-stdout=fmt
        foo :: Int -> Int
        foo x = x
          |]
      , expectError
          "with: --no-stdout is a reserved argument name on a command with actions"
          [r|
        module main (foo)
        fmt :: Int -> Str
        --' @render -F/--fmt=fmt
        foo ::
          --' @arg --no-stdout
          Int ->
          Int
        foo x = x
          |]

        -- A terminal action applies to what a streaming command streams,
        -- so the command may not also return a value.
      , expectError
          "with: an action on a streaming command that also returns a value"
          [r|
        module main (foo)
        effect IO
        produce :: ([Int] -> <IO> ()) -> <IO> ()
        fmt :: [Int] -> Str
        --' @render -a/--all=fmt
        foo :: Int -> <IO> Int
        foo n = do
          @collect produce
          n
          |]

        -- An action applies to the whole output, whatever the shape of the
        -- body: a `@collect` in each branch of a guard builds.
      , expectPass
          "with: a gathering action on a @collect in each branch"
          [r|
        module main (foo)
        effect IO
        produce :: ([Int] -> <IO> ()) -> <IO> ()
        nop :: <IO> ()
        fmt :: [Int] -> Str
        --' @render -a/--all=fmt
        foo :: Bool -> <IO> (Collect Int)
        foo b = ? b = @collect produce : @collect (\k -> nop)
          |]

        -- A leading-underscore long flag name is rejected: those names are
        -- reserved for compiler-generated argument identifiers (the MCP
        -- backend names positionals `_1`, `_2`, ...), so a flag must not be
        -- able to produce one.
      , expectError
          "arg: long flag may not begin with an underscore"
          [r|
        module main (foo)
        foo ::
          --' @arg --_hidden
          --' @default 0
          Int ->
          Int
        foo x = x
          |]

        -- The streaming command duplicates the parent's body with its
        -- parameters renamed. A multi-binding `let` is sequential, so `y`
        -- names the `let`-bound `x` (a Str), never the Int parameter.
      , expectPass
          "with: a let binder shadowing a parameter stays in scope for later bindings"
          [r|
        module main (stream)
        effect IO
        data Try e a = Err e | Ok a
        mk :: Str -> [Int]
        mk _ = [1]
        showInts :: [Int] -> Str
        showInts _ = "x"
        produce :: Str -> ([Int] -> <IO> ()) -> <IO> ()
        produce s sink = sink (mk s)
        --' @render -p/--plain=showInts
        stream :: Int -> <IO> (Collect Int)
        stream x =
          let x = "s"
              y = x
          in @collect (produce y)
          |]

        -- The streaming command names the handler inside the duplicated body,
        -- where a local binder of the same name must not capture it.
      , expectPass
          "with: a local binder named like the handler does not capture it"
          [r|
        module main (stream)
        effect IO
        data Try e a = Err e | Ok a
        mk :: Int -> [Int]
        mk _ = [1]
        showInts :: [Int] -> Str
        showInts _ = "x"
        produce :: Int -> ([Int] -> <IO> ()) -> <IO> ()
        produce n sink = sink (mk n)
        --' @render -p/--plain=showInts
        stream :: Int -> <IO> (Collect Int)
        stream n = let showInts = 5 in @collect (produce showInts)
          |]

        -- A stdout source ends the body itself, never a where-binding
        -- (STRM-4).
      , expectError
          "with: a command may not stream from a where-binding"
          [r|
        module main (stream)
        effect IO
        data Try e a = Err e | Ok a
        mk :: Int -> [Int]
        mk _ = [1]
        render2 :: Int -> [Int] -> Str
        render2 _ _ = "x"
        produce :: Int -> ([Int] -> <IO> ()) -> <IO> ()
        produce n sink = sink (mk n)
        --' @render -r/--text=render2($1)
        stream :: Int -> Int -> <IO> (Collect Int)
        stream x y = go
          where
            go = @collect (produce x)
          |]

        -- A `@stdout` command runs its actions on its staged output, where
        -- `$2` is the second argument the stage saved, even when a lambda
        -- in the body binds it.
      , testCase "with: `$N` naming a body lambda's parameter of a staged command" $ do
          result <- runFront [r|
        module main (stream)
        effect IO
        data Try e a = Err e | Ok a
        mk :: Int -> [Int]
        mk _ = [1]
        render2 :: Int -> [Int] -> Str
        render2 _ _ = "x"
        produce :: Int -> ([Int] -> <IO> ()) -> <IO> ()
        produce n sink = sink (mk n)
        --' @render -r/--text=render2($2)
        stream :: Int -> Int -> <IO> (Collect Int)
        stream x = \y -> @stdout (\o -> @write 0 o (mk x))
          |]
          case result of
            Right _ -> return ()
            Left e -> assertFailure (show e)
      ]
