#pragma once
#include <vector>
#include <string>
#include <cstdint>

inline std::vector<int> mk(int i){ return {i * 10, i * 10 + 1}; }

// whole-form `render:` handler -- gather the complete [Int] stream, emit a
// text rendering verbatim (the nexus forces `-f raw` for render terminals).
inline std::string joinAll(std::vector<int> xs){
  std::string s;
  for (size_t i = 0; i < xs.size(); ++i) { if (i) s += ","; s += std::to_string(xs[i]); }
  s += "\n";
  return s;
}

// whole-form BINARY `render:` handler -- gather the complete stream, emit raw
// bytes (Vector U8) verbatim, with no textual framing.
inline std::vector<uint8_t> toBytes(std::vector<int> xs){
  std::vector<uint8_t> r;
  for (int x : xs) r.push_back((uint8_t) x);
  return r;
}
