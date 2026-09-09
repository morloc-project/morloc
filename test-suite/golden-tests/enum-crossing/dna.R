# DNA arrives as an R factor: integer codes plus a levels attribute, which
# is R's native enum. Factor codes are 1-based, so this indexes with
# 5L - as.integer(x) where the wire ordinal would want 3 - tag.
complement <- function(x) {
    levs <- levels(x)
    factor(levs[5L - as.integer(x)], levels = levs)
}
