# Returns a list of n strings, each ~256 characters wide. Used by the
# [pool-recovery-large] daemon test to verify pool-crash recovery
# reclaims large RPTR-shipped payloads.
bigList <- function(n) {
  one <- paste(rep("x", 256), collapse = "")
  rep(one, n)
}
