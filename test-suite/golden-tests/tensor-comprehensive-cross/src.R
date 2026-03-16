# --- 2D Real (3x4, values 1..12) ---
rMakeMat <- function() {
  matrix(as.double(1:12), nrow=3, ncol=4, byrow=TRUE)
}

rSumMat <- function(m) {
  sum(m)
}

# --- 3D Real (2x3x4, values 1..24 but R is 1-indexed)
rMake3d <- function() {
  array(as.double(1:24), dim=c(2, 3, 4))
}

# --- 1D Real (10 elements, values 1..10) ---
rMakeVec <- function() {
  array(as.double(1:10), dim=c(10))
}

rSumVec <- function(v) {
  sum(v)
}

# --- 4D Real (2x3x2x2, values 0..23) ---
rMake4d <- function() {
  array(as.double(0:23), dim=c(2, 3, 2, 2))
}

rSum4d <- function(t) {
  sum(t)
}

# --- 1D Int (8 elements, values 10..17) ---
rMakeIntVec <- function() {
  array(as.integer(10:17), dim=c(8))
}

rSumIntVec <- function(v) {
  as.integer(sum(v))
}

# --- 1D Bool (6 elements: T,F,T,T,F,T) ---
rMakeBoolVec <- function() {
  array(c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE), dim=c(6))
}

rCountTrue <- function(v) {
  as.integer(sum(v))
}

# --- Empty tensor (0 elements) ---
rMakeEmpty <- function() {
  array(double(0), dim=c(0))
}

rSumEmpty <- function(v) {
  sum(v)
}

# --- Single element (value 42) ---
rMakeSingle <- function() {
  array(42.0, dim=c(1))
}

rSumSingle <- function(v) {
  sum(v)
}

# --- Large tensor (5000 doubles, values 0..4999) ---
rMakeLarge <- function() {
  array(as.double(0:4999), dim=c(5000))
}

rSumLarge <- function(v) {
  sum(v)
}

# --- Very large tensor (50000 doubles, crosses SHM threshold) ---
rMakeHuge <- function() {
  array(as.double(0:49999), dim=c(50000))
}

rSumHuge <- function(v) {
  sum(v)
}
