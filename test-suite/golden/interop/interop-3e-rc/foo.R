rneg <- function(x) (-1) * x
radd <- function(x, y) x + y
rmul <- function(x, y) x * y
rmap <- function(f, xs) sapply(xs, f)
rzipWith <- function(f, xs, ys) sapply(seq_along(xs), function(i) f(xs[i], ys[i]))
