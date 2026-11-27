## Real 10-element baseline
rMakeReal10 <- function() as.double(1:10)
rSumReal10  <- function(v) sum(v)

## Permutation probe: powers of two; element at index 3 (R: index 4, since 1-indexed)
## must equal 8.
rMakePerm <- function() as.integer(c(1, 2, 4, 8, 16, 32, 64, 128))
rAtIdx3   <- function(v) as.integer(v[4])

## Big: 50000 doubles = 400 KB; values 0..49999.
rMakeBig <- function() as.double(0:49999)
rSumBig  <- function(v) sum(v)

## Edge cases
rEmpty       <- function() double(0)
rSingle      <- function() as.double(42)
rSumEmpty    <- function(v) sum(v)
rSumSingle   <- function(v) sum(v)

## Polymorphic-in-size sum (for literal-input tests).
rSumVecPoly  <- function(v) sum(v)
