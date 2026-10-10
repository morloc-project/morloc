inc <- function(x) x + 1L

add <- function(x, y) x + y

join2 <- function(x, y) paste0(x, y)

count2 <- function(xs) length(xs)

ident <- function(xs) xs

mknames <- function(n) paste0("n", seq_len(n) - 1L)
