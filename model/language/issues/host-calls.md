# Host calls

- **A function a host returns keeps its declared grouping only when an
  effect forces an adapter.** [read] Gap in HOST-4. A source `mk :: Int ->
  (Int -> (Int -> Int))` whose result is called with two arguments: with no
  effect in the result, `needsAdapt` (CodeGenerator/EffectBoundary.hs) is
  False and nothing regroups the call, so morloc calls the host's
  one-argument function with two. With an effect in the tail the inbound
  adapter splits at the declared grouping (`calledAs`). Not reproduced.

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
  argument for this reason.

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
- **An invalid `rsize` is accepted until the term is called.**
  [reproduced, 2026-10-21] Violates HOST-3. `--' rsize: 2` on `shift ::
  Real -> [Real] -> [Real]` builds when `shift` is never called;
  Express.hs:561-581 checks at the call site, though the arity is known
  from the signature. Repro: /work/plans/model-audit-2026-10-21/repro/F/host/h3b.loc.
- **Two spellings of source directives.** [reproduced, 2026-10-21] HOST-3
  and the manual write `--' rsize: 1`; goldens spec-host-7-1/8-1 write
  `--' @rsize 1`, and the parse error suggests `@rsize 1`
  (Desugar.hs:918). Both build; no item names the canonical form (same for
  `name:`/`@name`).
