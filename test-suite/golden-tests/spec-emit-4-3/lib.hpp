#pragma once
#include <cstdint>
#include <stdexcept>

inline bool step(int64_t k, int64_t i) {
  if (i > k) throw std::runtime_error("an operand after the deciding one ran");
  return i != k;
}

inline bool hit(int64_t k, int64_t i) {
  if (i > k) throw std::runtime_error("an operand after the deciding one ran");
  return i == k;
}
