# Tables and tensors

- Bad table cell kills the pool: see failure-classification.md.
- [reproduced] Undeclared CSV column typed from the first 100 rows
  (morloc-runtime/src/arrow_ipc_reader.rs:667); a later mismatch kills the
  pool.
- [reproduced] Undeclared JSON column typed from its first value:
  `[{"u":1},{"u":2.5}]` -> "Expected integer" (arrow_ffi.rs:640-648).
- [reproduced] NaN/Inf cells print `nan`/`inf`, invalid JSON
  (arrow_ffi.rs:349-350).
- [reproduced] `?(?Int)` column compiles, fails at run time (Serial.hs:748
  vs arrow_shm.rs:212-221).
- **Tensor dims unchecked.** [reproduced] Violates DIM-3. Only the flat
  length is checked against a static product. `Matrix 2 2` accepts dims
  (1,4); C++ wraps negative dims (lib/stdlib tensor-cpp/tensor.hpp:328,
  :366); Python reshape infers -1 (tensor-py/tensor.py:66-68); R `array()`
  recycles or truncates (tensor-r/R/tensor.R:38-41).
- Contradicts TAB-1: only the row count is erased; columns travel and are
  checked. TAB-5/TAB-6 open: `morloc typecheck` accepts a list column that
  `make` rejects; row counts are never checked.
- [read] Arrow block doc (arrow_shm.rs) describes a metadata section never
  written.
- [speculative] pymorloc.c:2591-2593 leaks a moved schema on failure.
