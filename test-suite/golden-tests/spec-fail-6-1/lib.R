guard <- function(f, x) tryCatch(f(x), error = function(e) -1L)
