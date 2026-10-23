total <- function(xs){ sum(sapply(xs, function(x) x$src + x$dst)) }
