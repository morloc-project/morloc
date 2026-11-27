# Round-trip helpers for the [inline-threshold] daemon test. Each call
# sweeps a payload of a known size through the daemon -> R -> daemon
# packet pipeline so the harness can verify byte-perfect transit on
# both sides of the inline-vs-RPTR cutoff.

# Echo a list of integers unchanged. With N integers the flat voidstar
# size is roughly 16 + 8*N bytes; N around 8190 straddles the 64 KB
# inline threshold.
echoInts <- function(xs) as.integer(xs)

# Reduce a list of integers to a single Int. Used to verify the small-
# result direction: a 100k-element input is RPTR-shipped to R, but the
# scalar return inlines.
sumInts <- function(xs) {
  # Use sum() with a numeric coercion to dodge integer overflow for
  # large N; cast back to integer at the end. The harness picks
  # element values that keep the total within Int range.
  as.integer(sum(as.numeric(xs)))
}

# Generate a fresh sequence 0..N-1. Tiny scalar input, arbitrary-size
# result -- exercises pool -> daemon RPTR-ship for big results.
firstN <- function(n) {
  if (n <= 0L) integer(0) else seq.int(0L, as.integer(n) - 1L)
}
