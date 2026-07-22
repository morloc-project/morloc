#pragma once
#include <vector>
#include <cstdint>
inline std::vector<int> mk(int i){ return {i*10+1, i*10}; }
// offset handler: prepend the running element offset
inline std::vector<int> tagOff(uint64_t off, std::vector<int> xs){ xs.insert(xs.begin(), (int)off); return xs; }
// IFile handler: element count
