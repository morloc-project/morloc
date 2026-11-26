# Typecheck time grows faster than linearly

Child of the typecheck time line in runtime/issues/LIST.md. Bench:
benchmarks/typechecking.

[reproduced, 2026-11-26, compiler 6c1b7ab9c+dirty, load under 1] One
`morloc typecheck` per point, from the bench's generators. The fixed cost
t0 (TYCK-1) is 95 ms. Each line gives t - t0 at three sizes, each double
the last, and the growth per doubling (2 is linear, 4 is quadratic). Every
item named has `Limit: <series>.k <= 1.3`, which these break.

- **Composition chains (TYCK-9).** compose-poly (unsigned `id_`), n =
  160, 320, 640: 200, 690, 3860 ms, x3.4 then x5.6. compose (signed): 40,
  160, 950 ms, x4 then x6. compose-partial: 110, 300, 1050 ms, x2.7 then
  x3.5. Growth itself grows, so this is worse than quadratic in the chain.
- **Signed definitions nested in `where` (TYCK-10).** where-chain, d = 128,
  256, 512: 50, 184, 833 ms, x3.7 then x4.5. Peak RSS 48, 72, 173 MiB.
- **Parenthesized nesting (TYCK-8).** tuple-nested, n = 400, 800, 1600: 80,
  230, 800 ms, x2.9 then x3.5. apply-annotated: 50, 140, 460 ms, x2.8 then
  x3.3. apply-nested (x2.7, x3) and apply-multiarg are milder.
- **Method uses in one definition (TYCK-6, TYCK-7).** method-gt and
  method-ne, n = 1000, 2000: 400 then 1200 ms, x3.
- Milder, around x2.3 per doubling: chain (TYCK-2, n = 200..800),
  ladder-indexed (TYCK-4, d = 64..256), stuck-uses (TYCK-11, n = 1024 to
  2048: 1056 to 2595 ms).

The cause has not been looked for. Which phase (parse, generalize,
validate, specialize) holds the time is the first question: compose-poly at
640 and where-chain at 512 are the cheapest witnesses.
