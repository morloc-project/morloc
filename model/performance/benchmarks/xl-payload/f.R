mk_reals <- function(n) as.numeric(seq_len(as.integer(n)) - 1L)

len_reals <- function(xs) bit64::as.integer64(length(xs))

mk_str <- function(n) strrep("x", as.integer(n))

len_str <- function(s) bit64::as.integer64(nchar(s, type = "bytes"))

mk_strs <- function(n) rep("abcdefgh", as.integer(n))

len_strs <- function(xs) bit64::as.integer64(length(xs))
