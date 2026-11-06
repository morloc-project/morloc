# Host calls

- **A point-free reader built from a Rust function fails when a Python
  caller both collects it and writes it.** [reproduced] Violates WIRE-6.
  With bio-rust installed, `readE = mapReader gffEntries readGff3` (no
  parameters; `mapReader f r path sink = r path (\xs -> sink (f xs))`) and
  a Python function `do ms <- collectSource (readE p); w o (readE q); idpy
  (length ms)` where `w path src = writeGff3 path (mapSource entryRoots
  src)` stops with "cannot send a function value that morloc did not
  create" (rustmorloc fv.rs). Each statement alone works; so does
  `readE path = mapReader gffEntries readGff3 path`, a lambda-defined
  reader, `mapReader (\xs -> xs)` in place of `gffEntries`, and the same
  body with no Python. biolib's bio defines its readers with the path
  argument for this reason. [traced 2026-11-02, briefs/A7-deep.md] The
  refusal follows the term's use count, not placement: with two uses
  Share.hs binds the function-valued constant once, HostProvenance sees an
  application of a let binder, and its `AppS` arm reports a stray before
  looking at the head and never passes the demand on. Fix there (pass a
  positional demand to the let binder), not by eta-expanding, which would
  undo the sharing EVAL-3 requires.

- **A point-free source built from a list of sources stops the build when a
  Python call follows its use.** [reproduced, compiler 4443cc47] Internal
  error "a lambda value is realized in rust inside an expression realized in
  py" (CodeGenerator/Express.hs, the lambda-language check), located at
  `src (writeBatch level o)` in biolib stream's `saveSource`. With stream
  and stream-rust installed:

      source Py from "h.py" ("tmp_path" as tmpPath, "touch" as touch)
      numbers :: Source Int
      numbers = concatSources [listSource [1, 2], listSource [3]]
      go :: Int -> <IO> Int
      go n = do
        p <- tmpPath "a.mlc"
        saveSource 0 p numbers
        touch p
        n

  Builds: `numbers sink = concatSources [...] sink`, `numbers = listSource
  [1, 2, 3]`, the list inlined at the call, and dropping `touch p`.
  `concatSources` written with a recursive `mapE_` fails the same way. The
  same program shape built before the compiler of 2026-10-19 17:27.
  On 2026-10-31, with stream-py also installed, the program builds and
  runs because every call is placed in Python; the Rust path was not
  exercised.
- **A call that stops at an `rsize` group boundary with more than one group
  left is called flat.** [read, 2026-10-27] Violates HOST-3. With `--' @rsize 
  1 1` on `f :: Int -> Int -> Int -> Int`, `let h = f 1 in h 2 3` calls the
  function the host returns once with both 2 and 3 (Python: TypeError);
  the host returns a function taking one argument that returns another.
  Provenance (HostProvenance `sourcedApp`), the call site and Rust
  `lcSourcedResult` (`IntoFnValueN` with N = the arguments left, not the
  next group's size) all read the arguments left as one group. A wrapper
  applying the groups in turn needs the curried host type in the typed
  pools. Not reproduced 2026-10-31 in Python: `let h = f 1 in h 2 x`,
  `twice (f 1) x` with `twice g x = g 2 x + g 3 x`, and `f 1` passed to a
  Python function calling it with two arguments all give the right value,
  since each call is eta-expanded to `f(1)(b)(c)`; the flat path may be
  reachable only through Rust `lcSourcedResult`.
- **The advice to choose a source can name sources of other instances.**
  [read, 2026-11-06] HostProvenance.hs `chooseSourceHint` finds the term
  of the source that made a function by value (`src elem srcs`), and a
  provenance (`MadeBy`) holds no index, so when that source is a method's
  and two instances source the same function, the advice lists the
  languages of both instances' sources, though only the selected
  instance's can be chosen (HOST-9). Not reproduced.
