# R factor codes are 1-based, so the complement of code k is 5 - k.
complement_all <- function(xs) {
    levs <- levels(xs)
    factor(levs[5L - as.integer(xs)], levels = levs)
}
