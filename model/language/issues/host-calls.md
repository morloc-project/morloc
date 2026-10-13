# Host calls

- **A function a host returns keeps its declared grouping only when an
  effect forces an adapter.** [read] Gap in HOST-4. A source `mk :: Int ->
  (Int -> (Int -> Int))` whose result is called with two arguments: with no
  effect in the result, `needsAdapt` (CodeGenerator/EffectBoundary.hs) is
  False and nothing regroups the call, so morloc calls the host's
  one-argument function with two. With an effect in the tail the inbound
  adapter splits at the declared grouping (`calledAs`). Not reproduced.
