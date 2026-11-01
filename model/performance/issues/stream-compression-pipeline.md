# Compressed streams: which process grows

Status: open. Parent: runtime/issues/encode-throughput.md section 3.
Source: lib/biolib/runtime-bench.

- **Unmeasured: which process grows.** [measured 2026-10-14, 600 MiB of
  `[Vector U8]`] A level-3 write without flush peaks at 211 MiB (C++)
  against 110 at level 0. bioprojects' tools/measure.py, which
  lib/biolib/runtime-bench uses, reports the largest process's VmHWM,
  which counts shared pages it touched; per-process RssShmem and RssAnon
  would locate the growth.
