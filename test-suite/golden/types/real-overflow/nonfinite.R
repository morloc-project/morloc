## Producers of and arithmetic on R's IEEE-754 non-finite values. R
## distinguishes NA (missing) from NaN (not-a-number) and Inf (infinity);
## only the latter two map to morloc Real. NA is excluded here -- see
## int-overflow/naSrc.R for the NA-rejection case.
rInf <- function(x) Inf
rNegInf <- function(x) -Inf
rNaN <- function(x) NaN

add <- function(a, b) a + b
sub <- function(a, b) a - b
mul <- function(a, b) a * b
neg <- function(a) -a
