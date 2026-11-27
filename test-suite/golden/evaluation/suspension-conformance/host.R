tick <- function(path) {
  cat("x", file = path, append = TRUE)
  as.integer(file.info(path)$size)
}

mark <- function(s) s

ident <- function(x) as.integer(x)

take_thunk <- function(t) {
  t()
  t()
  as.integer(t())
}

use_record <- function(h) {
  h[["run"]]()
  h[["run"]]()
  as.integer(h[["run"]]())
}

use_list <- function(xs) {
  xs[[1]]()
  xs[[1]]()
  as.integer(xs[[1]]())
}

use_callback <- function(f, x) {
  f(x)
  f(x)
  as.integer(f(x))
}

use_pinned <- function(c, x) {
  c[["inc"]](x)
  c[["inc"]](x)
  as.integer(c[["inc"]](x))
}
