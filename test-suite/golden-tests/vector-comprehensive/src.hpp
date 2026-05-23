#ifndef __VECTOR_COMPREHENSIVE_SRC_HPP__
#define __VECTOR_COMPREHENSIVE_SRC_HPP__

#include <climits>
#include <cstdint>
#include <vector>

// ---------------------------------------------------------------------------
// Real 10-element baseline: [1.0, 2.0, ..., 10.0]; sum = 55.0
// ---------------------------------------------------------------------------
inline std::vector<double> cppMakeReal10() {
    std::vector<double> v(10);
    for (int i = 0; i < 10; i++) v[i] = i + 1;
    return v;
}
inline double cppSumReal10(const std::vector<double>& v) {
    double s = 0;
    for (double x : v) s += x;
    return s;
}

// ---------------------------------------------------------------------------
// Element types. Each fixture packs the type's extremes so any sign or
// width-loss bug surfaces in the sum.
// ---------------------------------------------------------------------------
inline std::vector<int8_t>  cppMakeI8()  { return {INT8_MIN,  -1, 0, INT8_MAX}; }
inline std::vector<int16_t> cppMakeI16() { return {INT16_MIN, -1, 0, INT16_MAX}; }
inline std::vector<int32_t> cppMakeI32() { return {INT32_MIN, -1, 0, INT32_MAX}; }
// Avoid INT64_MIN: morloc's Int sums into a signed 64-bit slot, and
// INT64_MIN + INT64_MAX would overflow the difference.
inline std::vector<int64_t> cppMakeI64() { return {INT64_MIN + 1, -1, 0, INT64_MAX - 1}; }

inline std::vector<uint8_t>  cppMakeU8()  { return {0, 1, 127, UINT8_MAX}; }
inline std::vector<uint16_t> cppMakeU16() { return {0, 1, 32767, UINT16_MAX}; }
inline std::vector<uint32_t> cppMakeU32() { return {0, 1, 65535, UINT32_MAX}; }
// Cap at UINT32_MAX so the sum fits in a signed 64-bit Int.
inline std::vector<uint64_t> cppMakeU64() { return {0, 1, 65535, UINT32_MAX}; }

inline std::vector<float>  cppMakeF32() { return {0.5f, 1.5f, 2.5f, 3.5f}; }
inline std::vector<double> cppMakeF64() { return {0.5,  1.5,  2.5,  3.5}; }

template <class T>
inline int64_t sum_int(const std::vector<T>& v) {
    int64_t s = 0;
    for (auto x : v) s += x;
    return s;
}
inline int64_t cppSumI8 (const std::vector<int8_t>&  v) { return sum_int(v); }
inline int64_t cppSumI16(const std::vector<int16_t>& v) { return sum_int(v); }
inline int64_t cppSumI32(const std::vector<int32_t>& v) { return sum_int(v); }
inline int64_t cppSumI64(const std::vector<int64_t>& v) { return sum_int(v); }
inline int64_t cppSumU8 (const std::vector<uint8_t>&  v) { return sum_int(v); }
inline int64_t cppSumU16(const std::vector<uint16_t>& v) { return sum_int(v); }
// U32/U64 sums cross 2^31, so return uint64_t to match the morloc UInt64
// signature and avoid platform-int truncation at the wire.
inline uint64_t cppSumU32(const std::vector<uint32_t>& v) {
    uint64_t s = 0;
    for (auto x : v) s += x;
    return s;
}
inline uint64_t cppSumU64(const std::vector<uint64_t>& v) {
    uint64_t s = 0;
    for (auto x : v) s += x;
    return s;
}

inline double cppSumF32(const std::vector<float>&  v) { double s = 0; for (auto x : v) s += x; return s; }
inline double cppSumF64(const std::vector<double>& v) { double s = 0; for (auto x : v) s += x; return s; }

// Width-aliasing probes
inline std::vector<int8_t>  cppMakeI8Min() { return {INT8_MIN}; }
inline std::vector<uint8_t> cppMakeU8Max() { return {UINT8_MAX}; }
inline int64_t cppSumI8Min(const std::vector<int8_t>&  v)  { return v.empty() ? 0 : v[0]; }
inline int64_t cppSumU8Max(const std::vector<uint8_t>& v)  { return v.empty() ? 0 : v[0]; }

// ---------------------------------------------------------------------------
// Permutation probe: powers of two [1,2,4,8,16,32,64,128]; index 3 == 8.
// ---------------------------------------------------------------------------
inline std::vector<int32_t> cppMakePerm() { return {1, 2, 4, 8, 16, 32, 64, 128}; }
inline int64_t cppAtIdx3(const std::vector<int32_t>& v) { return v.size() > 3 ? v[3] : -1; }

// ---------------------------------------------------------------------------
// Big / threshold-straddle / edges
// ---------------------------------------------------------------------------
inline std::vector<double> cppMakeBig() {
    std::vector<double> v(50000);
    for (size_t i = 0; i < v.size(); i++) v[i] = (double)i;
    return v;
}
inline double cppSumBig(const std::vector<double>& v) {
    double s = 0;
    for (double x : v) s += x;
    return s;
}

inline std::vector<double> cppMakeUnder() { return std::vector<double>(8000, 0.0); }
inline std::vector<double> cppMakeOver()  { return std::vector<double>(8200, 0.0); }

inline std::vector<double> cppEmpty()  { return {}; }
inline std::vector<double> cppSingle() { return {42.0}; }
inline double cppSumEmpty (const std::vector<double>& v) { double s = 0; for (double x : v) s += x; return s; }
inline double cppSumSingle(const std::vector<double>& v) { double s = 0; for (double x : v) s += x; return s; }

// ---------------------------------------------------------------------------
// Polymorphic-in-size sum helpers for the literal-input tests.
// ---------------------------------------------------------------------------
inline double  cppSumVecPoly(const std::vector<double>&  v) { double s = 0; for (double x : v) s += x; return s; }
inline int64_t cppSumI32Poly(const std::vector<int32_t>& v) { return sum_int(v); }

#endif
