# Tables and tensors

- [reproduced] Undeclared CSV column typed from the first 100 rows
  (morloc-runtime/src/arrow_ipc_reader.rs:698); a later mismatch fails the
  call on valid data.
- [reproduced] Undeclared JSON column typed from its first value:
  `[{"u":1},{"u":2.5}]` -> "Expected integer" (arrow_ffi.rs
  `discover_json_columns` keeps the first sample).
- [reproduced] `?(?Int)` column compiles, fails at run time (Serial.hs:830-831
  `isArrowColumn` vs arrow_shm.rs:217-226 `declared_inner`).
- **Tensor dims unchecked.** [reproduced] Violates DIM-3. Only the flat
  length is checked against a static product. `Matrix 2 2` accepts dims
  (1,4); C++ wraps negative dims (lib/stdlib tensor-cpp/tensor.hpp:333,
  :371); Python reshape infers -1 (tensor-py/tensor.py:70-72); R `array()`
  recycles or truncates (tensor-r/R/tensor.R:36-39).
- Contradicts TAB-1: only the row count is erased; columns travel and are
  checked. TAB-5/TAB-6 open: `morloc typecheck` accepts a list column that
  `make` rejects; row counts are never checked.
  [reproduced 2026-11-02] A `?(?Int)` column, or `?O` with `type O = ?Int`,
  passes typecheck and make and fails at run time ("Unsupported column type
  ... Optional"): Serial.hs `isArrowColumn` recurses through optionals.
  What a column may hold is decided in four places that disagree
  (`isArrowColumn`, Nexus.hs `tableFormProblem`, arrow_shm.rs `declared_*`,
  delimited.rs `ColType`); briefs/B12-B13.md.
- [speculative] pymorloc.c:2591-2593 leaks a moved schema on failure.

CSV input to a `Table` argument (reader: arrow_ipc_reader.rs
`delimited_batch`, rules CLI-13..CLI-19).

- **A bad table cell fails the call after the command starts.** [reproduced
  2026-10-29] CLI-20 asks for the error before the command runs; a `Table`
  is read by the pool that holds it, so the nexus checks only the header and
  the cell error comes back as the call's failure (golden
  bad-table-cell-fails-the-call).
- [speculative, 2026-11-11] A Rust `@write` of a batch of tables would
  size the list through the voidstar size walk, which aborts for a table
  (rustmorloc `RecordBatch::shm_size`, morloc_infra_abort); the runtime
  refuses table elements in place, so the copy path is the only route.
  Not reproduced: whether codegen ever gives a Rust pool an `OStream Table`
  write is unchecked. SLOT-35 / TAB gap.
