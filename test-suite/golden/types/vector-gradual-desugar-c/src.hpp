#ifndef __SRC_HPP__
#define __SRC_HPP__
#include <vector>
#include <cstdint>

// Return a std::vector<uint8_t> of length n. Morloc-side type is
// gradual (Vector U8, no Nat annotation); the runtime shape is
// determined by n.
inline std::vector<uint8_t> mk_vec(int n) {
    return std::vector<uint8_t>(static_cast<size_t>(n), static_cast<uint8_t>(0));
}

// Return a length-3 std::vector<uint8_t>. Morloc-side type is
// concrete (Vector 3 U8); runtime length must match.
inline std::vector<uint8_t> mk_three() {
    return std::vector<uint8_t>{1, 2, 3};
}

#endif
