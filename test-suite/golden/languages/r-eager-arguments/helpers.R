leaf <- function(x) {
  if (sys.nframe() > 400) stop("call depth ", sys.nframe())
  x + 1L
}
first <- function(a, b) a
boom <- function(x) stop("boom")
