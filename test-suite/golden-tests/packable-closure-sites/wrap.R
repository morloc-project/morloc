pack_wrap_r <- function(x) structure(list(v = x), class = "WrapR")
unpack_wrap_r <- function(w) w$v
key_r <- function(w) w$v
r_mk <- function(x) pack_wrap_r(x)
