# Effects

Suspensions, `do`, `!` and effect rows (EFF) against the code. Reproduced
on 0.109.0 (dev 3a238d96) unless marked.

- **A function that puts a suspension into a record is rejected.**
  [reproduced, 2026-10-21] Violates EFF-2. With `record Box where act ::
  <IO> Int`, `mk :: <IO> Int -> Box; mk t = {act = t}` fails "Inescapable
  effect 'IO' appear(s) in an argument but not in the result row"; the
  same through a tuple alias or a list is accepted. Cause [read]:
  `checkDeclaredEffects.inescapableLeak` (Frontend/Treeify.hs:427-436)
  collects result labels syntactically and does not look inside a named
  record, `data` or newtype. Repro: /work/plans/model-audit-2026-10-21/repro/D/effleak/m2.loc, m3.loc.
- **A `do` block's suspension tail is run or returned by expected type.**
  [reproduced, 2026-10-21] Violates EFF-4. With `net :: Int -> <Net> Int`,
  `g :: Int -> <IO> (<Net> Int); g x = do (net x)` is accepted and returns
  the suspension unrun; `j :: Int -> <Net> Int; j x = do (net x)` runs it.
  Under EFF-4 `g`'s body has type `<Net> Int` and should be rejected. EFF-4
  and EFF-9 then need to say how `<E> (<E'> T)` is built (only through a
  let-bound or sourced value?). Repro: /work/plans/model-audit-2026-10-21/repro/D/effneg/n9.
- **`!` outside a do-block: EFF-7 against the manual and the code.**
  [reproduced, 2026-10-21] EFF-7: a `!` with no enclosing do-block is a
  type error. `g :: Int -> <IO> Int; g x = !(say x) + 1` and
  `pr x = (!(say x), !(say (x+1)))` build and run, as features-effects.asc:
  415-440 documents. Candidates: (a) EFF-7 as written; (b) a `!` outside a
  do-block wraps the nearest enclosing body. The manual's rejected
  positions (`x <- !e`, bare `!e`, `let x = !e`) are in no item. Repro:
  /work/plans/model-audit-2026-10-21/repro/D/bang.
- **Effect rules the manual and code apply with no item.** [reproduced,
  2026-10-21]
  - A bare statement whose result is an `Err` stops the block, and
    `Ok p <- e` throws on a mismatch (features-effects.asc:244-332). EFF-6
    says only that a bare statement runs `t`. Which result types are
    checked, and is the failure recoverable under FAIL-4 (caught by
    `@try`)?
  - A signature whose parameter rows hold an inescapable label absent from
    the result is rejected (`ign :: <IO> Int -> Int; ign t = 1`;
    features-effects.asc:647-670). Only EFF-12 (open) mentions handlers.
  - EFF-12 names the handler shape `<E, e> a -> <e> a`; the manual and
    code accept `withSeed :: Int -> <Rand> a -> a` (features-effects.asc:
    681-700).
  - A row may hold at most one variable (`<Rand,e,f>` rejected), and two
    modules may not declare one label with different escapability
    (features-effects.asc:160, :613). EFF-8 states neither.
  - EFF-11 says every row but a source's is inferred; EFF-10 lets a
    morloc signature declare labels its body does not run, and intrinsics
    such as `@savej` carry `<IO>` without a source.
- **A malformed foreign `Try` is decoded silently.** [reproduced,
  2026-10-21] A Python source declared `-> <IO> (Try Str ())` returning
  `{"Ok": [None]}` (the native form is `("Ok", (None,))`) made a bare
  statement fail "Error: run failed / 0"; nothing names the bad value.
