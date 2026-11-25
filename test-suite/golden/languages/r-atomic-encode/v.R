ints <- function(k) if (k == 0) c(0L, 1L, 127L) else if (k == 1) c(1L, NA) else c(1L, -1L)
dbls <- function(k) if (k == 0) c(0, 1.5, 126.25) else if (k == 1) c(1, NaN) else c(1, 5e9)
lgls <- function(k) if (k == 0) c(TRUE, FALSE, TRUE) else c(TRUE, NA)
