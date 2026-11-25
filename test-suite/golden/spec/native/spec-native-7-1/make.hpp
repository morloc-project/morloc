#include <cstdint>
#include <vector>
inline std::vector<uint8_t> cpp_bytes(int n) { std::vector<uint8_t> v; for (int i = 0; i < n; i++) v.push_back((uint8_t)(i + 1)); return v; }
template <class D> std::vector<D> cpp_dirs(const std::vector<D>& xs) { return xs; }
