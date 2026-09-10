# DNA arrives as an R factor: integer codes plus a levels attribute, which
# is R's native enum. Factor codes are 1-based, so this indexes with
# 5L - as.integer(x) where the wire ordinal would want 3 - tag.
complement <- function(x) {
    levs <- levels(x)
    factor(levs[5L - as.integer(x)], levels = levs)
}

# A [DNA] arrives as one factor VECTOR: the levels sit on the vector, not on
# each cell, so an element pulled out of it is a bare integer. Both an
# answer given as a factor and one given as a character vector of
# constructor names are legal.
complement_all <- function(xs) {
    levs <- levels(xs)
    factor(levs[5L - as.integer(xs)], levels = levs)
}

names_of <- function(xs) as.character(xs)
