#pragma once
#include <vector>
#include <cstdint>
inline std::vector<uint8_t> toBytes(std::vector<int> xs){ std::vector<uint8_t> r; for(int x:xs) r.push_back((uint8_t)x); return r; }
