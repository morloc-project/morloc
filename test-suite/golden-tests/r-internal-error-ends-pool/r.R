r_abort <- function(x) {
  .__morloc$morloc_mlc_internal_abort("a runtime invariant failed")
  x
}
