#pragma once
#include <cstdint>
#include <stdexcept>
#include <string>

struct MlcDate { int64_t v; bool bad; };

inline int64_t& unpackCount() { static int64_t n = 0; return n; }

inline MlcDate packDate(const std::string& s) { return MlcDate{std::stoll(s), false}; }
inline std::string unpackDate(const MlcDate& d) {
    if (d.bad) throw std::runtime_error("this date cannot be written");
    unpackCount()++;
    return std::to_string(d.v);
}
inline MlcDate dateOf(int x) { return MlcDate{int64_t(x) * 10, false}; }
template <class T> int unpacksAfter(const T&) { return int(unpackCount()); }
inline MlcDate badDate(int x) { return MlcDate{int64_t(x), true}; }
#include <vector>
template <class T> struct MlcBag { std::vector<T> items; };
template <class T> MlcBag<T> packBag(const std::vector<T>& xs) { return MlcBag<T>{xs}; }
template <class T> std::vector<T> unpackBag(const MlcBag<T>& b) { return b.items; }
template <class T> MlcBag<T> bagOf(const std::vector<T>& xs) { return MlcBag<T>{xs}; }
inline int64_t toI64(int x) { return x; }
