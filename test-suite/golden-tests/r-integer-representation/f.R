as_i64 <- function(x) bit64::as.integer64(x)
i64_big <- function(x) bit64::as.integer64(x) * bit64::as.integer64(1000000)
neg_u64 <- function(x) bit64::as.integer64(-1)
half <- function(x) x / 2
ident <- function(x) x
