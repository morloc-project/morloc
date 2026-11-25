#pragma once
#include <cstdint>
#include <string>

struct MlcDate { int64_t v; };

inline int64_t& unpackCount() { static int64_t n = 0; return n; }

inline MlcDate packDate(const std::string& s) { return MlcDate{std::stoll(s)}; }
inline std::string unpackDate(const MlcDate& d) { unpackCount()++; return std::to_string(d.v); }
inline MlcDate dateOf(int x) { return MlcDate{int64_t(x) * 100}; }
template <class T> int unpacksAfter(const T&) { return int(unpackCount()); }
#include <deque>
#include <vector>
template <class T> std::deque<T> dq(const std::vector<T>& xs) { return std::deque<T>(xs.begin(), xs.end()); }
