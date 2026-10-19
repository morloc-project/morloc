# Prints the deepest n for which this recursion, written natively in R,
# returns n on the current stack.
depth <- function(n) if (n == 0) 0 else 1 + depth(n - 1)
reaches <- function(n) isTRUE(tryCatch(depth(n) == n, error = function(e) FALSE))
lo <- 1
hi <- 100000
while (hi - lo > 1) {
  mid <- (lo + hi) %/% 2
  if (reaches(mid)) lo <- mid else hi <- mid
}
cat(lo, "\n")
