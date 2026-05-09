## Returns R's NA_integer_, regardless of input. The morloc R bridge must
## refuse to pack NA into a fixed-width integer slot rather than silently
## casting NaN to INT*_MIN.
returnNAInt8 <- function(x) {
  NA_integer_
}
