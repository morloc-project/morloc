# Performance items out of date with their data

Found by the 2026-10-21 audit; full notes in
/work/plans/model-audit-2026-10-21/K-performance.md.

- **ZIP-5's three limits fail in the recorded runs.** [measured]
  results/2026-10-14-1725 (load 1.38) and -1430 (0.46), str-fastq, 128
  MiB, level 3: write speedup at 6 workers 2.25x (limit >= 3), read
  1.37-1.52x (>= 2), `taskset -c 0-3` over 4 workers 1.22x (<= 1.15);
  u64-small likewise. The fitted serial part is 0.79 s of 0.79 + 1.79/w,
  so the miss is serial work. The data predates c75b14d0, 3d38ea85,
  95709b1b: re-run `./run.sh compression` before attributing. Reproduce:
  `./compare.py 2026-10-14-1430-7306021b+dirty-baseline-before-decode-ahead
  2026-10-14-1725-7306021b+dirty-decode-ahead --item ZIP-5`.
- **`Measured: pending` though citable runs exist.** ZIP-1..7 and ZIP-10
  have results/2026-10-14-1725; XL-1 has results/2026-10-16-2156 (which
  XL-2 cites): all six pairs miss the 20 us goal (22-38 us) and cpp->py,
  rs->py sit within 10% of the 40 us limit. ENC-2 cites a run directory
  that does not exist.
- **ZIP-5/ZIP-6 count frames with F = 16 MiB; ZIP-9 cuts 2 MiB frames at
  levels 1-6.** bench.py:13 `FRAME = 16 * MiB`, :174 `k = ceil(r /
  FRAME)`. At level 3, 128 MiB is 64 frames, not 8, and ZIP-6's `h` (fit
  -1.24 KiB, wall r2 0.41) is per write buffer. Candidates: restate per
  sub-packet plus a 2 MiB frame-edge experiment; or parameterize F by
  level.
- **ZIP-4's open question is answered by the data.** `.long` = 1.000 at
  z8/z9 for every corpus and 0.94-1.00 at z7; z8 and z9 give identical
  ratios on 5 of 7 corpora. Needs a ruling on long mode at 7-9 and on
  whether 8 and 9 should differ.
- **ZIP-8's title calls level 3 "the default"; the CLI default is 0.**
  The ruling says level 3 is the usual (recommended) level. Retitle, or
  make 3 the default.
- **COPY-3 and COPY-6 prose is stale.** Rust per-step copies are 0 since
  f8736a0a (COPY-3 says "rs unchanged", "q = 2n"); the COPY-6 golden
  shows 0, not 198.
- **stream-compression-pipeline.md misstates the depth default.** It says
  4 (2 with fewer threads); the code is `frame_workers().clamp(2, 4)`.
