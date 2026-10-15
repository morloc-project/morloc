#pragma once
#include <cstdint>
#include <vector>
#include <stdexcept>

std::vector<uint8_t> mk(uint64_t n) { return std::vector<uint8_t>(n, 7); }
uint64_t len(const std::vector<uint8_t>& xs) { return xs.size(); }
uint64_t add(uint64_t a, uint64_t b) { return a + b; }
std::vector<uint8_t> fail(uint64_t n) { throw std::runtime_error("no batch " + std::to_string(n)); }
