rneg <- function(x) (-1) * x
radd <- function(x, y) x + y
rmul <- function(x, y) x * y
rmap <- function(f, xs) sapply(xs, f)
rfold <- function(f, b, xs) Reduce(f=f, x=xs, init=b)
