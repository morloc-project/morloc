upper <- function(raw_vec) {
  # Create a mask for lowercase ASCII values (a-z)
  mask <- raw_vec >= as.raw(0x61) & raw_vec <= as.raw(0x7A)
  
  # Apply the bitwise XOR operation only to lowercase letters
  raw_vec[mask] <- as.raw(bitwXor(as.integer(raw_vec[mask]), as.integer(0x20)))
  
  raw_vec
}
