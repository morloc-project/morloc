#pragma once
#include <cstdint>

// A user-mapped `data Cpp => DNA = "DNA"` supplies its own definition, the
// same way `record Cpp => X = "..."` does. One byte, ordinals in
// declaration order.
enum class DNA : uint8_t { A = 0, C = 1, G = 2, T = 3 };

inline DNA complement(DNA x) {
    return static_cast<DNA>(3 - static_cast<uint8_t>(x));
}
