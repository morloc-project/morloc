gen_reals <- function(n) as.numeric(seq_len(n)) * 0.5
gen_ints <- function(n) seq_len(n) %% 1000L
gen_bools <- function(n) seq_len(n) %% 3L == 0L
len <- function(xs) length(xs)
