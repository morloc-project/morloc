## ---------------------------------------------------------------------------
## Round-trip baselines.
## ---------------------------------------------------------------------------

## Matrix 3x4 Real, row-major values 1..12 at the wire boundary.
## The pack/unpack pair in tensor-r reverses axis order, so after unpack
## a wire row-major fill of 1..12 produces an R matrix whose [i,j] matches
## numpy/Cpp [i-1, j-1] under the row-major semantic indexing.
rMakeMat34 <- function() {
  ## Build a row-major-equivalent 3x4 by filling column-by-column and transposing.
  matrix(as.double(1:12), nrow = 3, ncol = 4, byrow = TRUE)
}

rSumMat34 <- function(m) sum(m)

## Tensor3 2x3x4 Real, row-major values 0..23 at the wire boundary.
## R stores column-major; the unpack converts via aperm so the semantic
## value at index (i,j,k) (zero-based) is i*12 + j*4 + k.
## To produce that semantic content from R, we fill the array with values
## in the order R stores them, then aperm to match the semantic layout.
rMakeT3 <- function() {
  ## We want r[i+1, j+1, k+1] == i*12 + j*4 + k (zero-based semantics).
  ## Easiest: build the row-major sequence then permute axes.
  ## After aperm with perm=c(3,2,1), an R array of dims=c(4,3,2) filled
  ## column-major with seq 0..23 gives an array of dims=c(2,3,4) where
  ## the original linear order maps to the row-major semantics.
  aperm(array(as.double(0:23), dim = c(4, 3, 2)), c(3, 2, 1))
}

rSumT3 <- function(t) sum(t)

## Tensor4 2x3x2x2 Real, row-major values 0..23.
rMakeT4 <- function() {
  aperm(array(as.double(0:23), dim = c(2, 2, 3, 2)), c(4, 3, 2, 1))
}

rSumT4 <- function(t) sum(t)

## Tensor5 2x2x2x2x2 Real, row-major values 0..31.
rMakeT5 <- function() {
  aperm(array(as.double(0:31), dim = c(2, 2, 2, 2, 2)), c(5, 4, 3, 2, 1))
}

rSumT5 <- function(t) sum(t)

## ---------------------------------------------------------------------------
## matmul consumer (Matrix 2 2 Real -> Real).
## ---------------------------------------------------------------------------
rSumMat22 <- function(m) sum(m)

## ---------------------------------------------------------------------------
## Corner extraction. Uses semantic (zero-based) indexing then converts
## to R's one-based. Expected result for the rMakeT3 fixture: c(0,12,11,23).
## ---------------------------------------------------------------------------
rCornersT3 <- function(t) {
  c(t[1, 1, 1],   # (0,0,0)
    t[2, 1, 1],   # (1,0,0)
    t[1, 3, 4],   # (0,2,3)
    t[2, 3, 4])   # (1,2,3)
}

## ---------------------------------------------------------------------------
## Big matrix for SHM-path zero-copy probe.
## ---------------------------------------------------------------------------
rMakeMatBig <- function() {
  matrix(1.0, nrow = 200, ncol = 100)
}

## Polymorphic-in-shape sum for literal-input tests.
rSumMatPoly <- function(m) sum(m)
