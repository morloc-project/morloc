ident <- function(x) x

# The class of the value as R sees it. A literal written into the pool and a
# value read off the wire must give the same answer, or the same morloc value
# has two R types and comparison sees the mismatch.
form <- function(x) paste(class(x), collapse = " ")
