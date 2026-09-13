mkFrame <- function(n) data.frame(x = c(1L, 2L, 3L), y = c("a", "b", "c"))
kindOfR <- function(t) class(t)[1]
addRowR <- function(t) rbind(t, data.frame(x = 4L, y = "d"))
