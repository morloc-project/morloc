#pragma once
#include <vector>
#include <cstdint>
inline std::vector<int> mk(int i){ return {i*10+1, i*10}; }
// per-batch handler: append the batch size so a tagged batch is distinguishable
// from an untagged one in the output.
inline std::vector<int> tag(std::vector<int> xs){ xs.push_back((int)xs.size()); return xs; }
