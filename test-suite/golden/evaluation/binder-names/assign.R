r_assign <- function(x) {
  assign(".__mm_1", function(...) 0L, envir = globalenv())
  x
}
