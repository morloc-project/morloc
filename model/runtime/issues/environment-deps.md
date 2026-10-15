# Declared environment dependencies

- **A declared tool is found through whatever PATH the program inherits.**
  [read] Gap: no spec item says where a pool finds the executables of a
  module's `tool-deps`. Pools inherit the nexus's environment unchanged
  (morloc-nexus/src/process.rs, the spawn copies `vars_os()`), so a
  program built in an environment and started from a shell without the
  environment's bin dir on PATH runs whatever `STAR` comes first, or fails
  when it reaches the call. `mim run`, serve and frozen images put the
  environment's bin first. Interpreters have the same exposure (python3 from
  PATH). Design open: the nexus could prepend the bin dir of the
  environment a program was built in, or check the declared tools at
  startup as `validate_pools` checks interpreters; a startup check needs
  the executable names, which the conda package name does not give (star ->
  STAR, trim-galore -> trim_galore), so `tool-deps` would need an optional
  list of executables.

- **Unknown package.yaml keys are ignored.** [read] Gap: no spec item for
  package.yaml validation. `FromJSON PackageMeta`
  (library/Morloc/Namespace/State.hs) reads only the keys it knows, so a
  misspelled `tools-deps` or `py-dep`, or `tool-deps` read by a compiler
  older than the key, drops the dependencies without a word and the
  program fails later at run time.
