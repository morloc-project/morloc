use_ops <- function(ops, x) {
  ops[["inc"]](x) + ops[["scale"]](x)
}

use_list <- function(fs, x) {
  fs[[1]](x) + fs[[2]](x)
}

use_tuple <- function(t, x) {
  t[[1]](x) + t[[2]](x)
}

use_opt <- function(f, x) {
  if (is.null(f)) x else f(x)
}
