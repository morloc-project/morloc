r_abort <- function(x) {
  morloc_mlc_internal_abort("a runtime invariant failed")
  x
}
