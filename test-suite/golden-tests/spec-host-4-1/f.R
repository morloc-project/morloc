scale <- function(k, xs) lapply(xs, function(x) k * x)
scale_r <- function(k) function(xs) scale(k, xs)
mk3 <- function(a, b, c) a * 100L + b * 10L + c
mk_r <- function(a) function(b, c) 1000L + a * 100L + b * 10L + c
