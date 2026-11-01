apply2 <- function(k, a, b) k(a, b)
feed <- function(k) k(function(a, b) a - b)
make_sub <- function(n) function(a, b) (a - b) * n
first2 <- function(fs, x) fs[[1]](x, x)
feed_split <- function(k) k(function(a) function(y) a * 10L + y)
