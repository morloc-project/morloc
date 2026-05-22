#ifndef __VECTOR_COMPREHENSIVE_SRC_HPP__
#define __VECTOR_COMPREHENSIVE_SRC_HPP__

#include <cstdint>
#include <numeric>
#include <vector>

// ---------------------------------------------------------------------------
// Real 10-element baseline: [1.0, 2.0, ..., 10.0]; sum = 55.0
// ---------------------------------------------------------------------------
inline std::vector<double> cppMakeReal10() {
    std::vector<double> v(10);
    for (int i = 0; i < 10; i++) v[i] = (double)(i + 1);
    return v;
}
inline double cppSumReal10(const std::vector<double>& v) {
    double s = 0;
    for (double x : v) s += x;
    return s;
}

// ---------------------------------------------------------------------------
// Element types (each extreme + zero + one to detect sign/width loss)
// ---------------------------------------------------------------------------
inline std::vector<int8_t> cppMakeI8()   { return {(int8_t)-128, (int8_t)-1, (int8_t)0, (int8_t)127}; }
inline int64_t              cppSumI8(const std::vector<int8_t>& v)   { int64_t s = 0; for (auto x : v) s += (int64_t)x; return s; }

inline std::vector<int16_t> cppMakeI16()  { return {(int16_t)-32768, (int16_t)-1, (int16_t)0, (int16_t)32767}; }
inline int64_t              cppSumI16(const std::vector<int16_t>& v) { int64_t s = 0; for (auto x : v) s += (int64_t)x; return s; }

inline std::vector<int32_t> cppMakeI32()  { return {(int32_t)-2147483648LL, (int32_t)-1, (int32_t)0, (int32_t)2147483647}; }
inline int64_t              cppSumI32(const std::vector<int32_t>& v) { int64_t s = 0; for (auto x : v) s += (int64_t)x; return s; }

inline std::vector<int64_t> cppMakeI64() {
    return {(int64_t)-9223372036854775807LL, (int64_t)-1, (int64_t)0, (int64_t)9223372036854775806LL};
}
inline int64_t              cppSumI64(const std::vector<int64_t>& v) { int64_t s = 0; for (auto x : v) s += x; return s; }

inline std::vector<uint8_t> cppMakeU8()   { return {(uint8_t)0, (uint8_t)1, (uint8_t)127, (uint8_t)255}; }
inline int64_t              cppSumU8(const std::vector<uint8_t>& v)  { int64_t s = 0; for (auto x : v) s += (int64_t)x; return s; }

inline std::vector<uint16_t> cppMakeU16() { return {(uint16_t)0, (uint16_t)1, (uint16_t)32767, (uint16_t)65535}; }
inline int64_t               cppSumU16(const std::vector<uint16_t>& v) { int64_t s = 0; for (auto x : v) s += (int64_t)x; return s; }

inline std::vector<uint32_t> cppMakeU32() { return {(uint32_t)0, (uint32_t)1, (uint32_t)65535, (uint32_t)4294967295U}; }
inline int64_t               cppSumU32(const std::vector<uint32_t>& v) { int64_t s = 0; for (auto x : v) s += (int64_t)x; return s; }

inline std::vector<uint64_t> cppMakeU64() { return {(uint64_t)0, (uint64_t)1, (uint64_t)65535, (uint64_t)4294967295ULL}; }
inline int64_t               cppSumU64(const std::vector<uint64_t>& v) { int64_t s = 0; for (auto x : v) s += (int64_t)x; return s; }

inline std::vector<float>  cppMakeF32()  { return {0.5f, 1.5f, 2.5f, 3.5f}; }
inline double              cppSumF32(const std::vector<float>& v)   { double s = 0; for (float x : v) s += (double)x; return s; }

inline std::vector<double> cppMakeF64()  { return {0.5, 1.5, 2.5, 3.5}; }
inline double              cppSumF64(const std::vector<double>& v)  { double s = 0; for (double x : v) s += x; return s; }

inline std::vector<int8_t>  cppMakeI8Min()  { return {(int8_t)-128}; }
inline int64_t              cppSumI8Min(const std::vector<int8_t>& v)  { return v.empty() ? 0 : (int64_t)v[0]; }

inline std::vector<uint8_t> cppMakeU8Max() { return {(uint8_t)255}; }
inline int64_t              cppSumU8Max(const std::vector<uint8_t>& v) { return v.empty() ? 0 : (int64_t)v[0]; }

// ---------------------------------------------------------------------------
// Permutation probe: powers of two [1,2,4,8,16,32,64,128]; index 3 == 8.
// ---------------------------------------------------------------------------
inline std::vector<int32_t> cppMakePerm() { return {1, 2, 4, 8, 16, 32, 64, 128}; }
inline int64_t              cppAtIdx3(const std::vector<int32_t>& v) { return v.size() > 3 ? (int64_t)v[3] : -1; }

// ---------------------------------------------------------------------------
// Big / threshold-straddle / edges
// ---------------------------------------------------------------------------
inline std::vector<double> cppMakeBig() {
    std::vector<double> v(50000);
    for (size_t i = 0; i < 50000; i++) v[i] = (double)i;
    return v;
}
inline double cppSumBig(const std::vector<double>& v) {
    double s = 0;
    for (double x : v) s += x;
    return s;
}

inline std::vector<double> cppMakeUnder() { return std::vector<double>(8000, 0.0); }
inline std::vector<double> cppMakeOver()  { return std::vector<double>(8200, 0.0); }

inline std::vector<double> cppEmpty()  { return std::vector<double>(); }
inline std::vector<double> cppSingle() { return std::vector<double>{42.0}; }
inline double              cppSumEmpty(const std::vector<double>& v)  { double s = 0; for (double x : v) s += x; return s; }
inline double              cppSumSingle(const std::vector<double>& v) { double s = 0; for (double x : v) s += x; return s; }

// ---------------------------------------------------------------------------
// Polymorphic-in-size sum helpers for the literal-input tests.
// The morloc-level Vector n Real / Vector n Int32 maps to std::vector<T>
// regardless of n, so these are just plain std::vector consumers.
// ---------------------------------------------------------------------------
inline double  cppSumVecPoly(const std::vector<double>& v)  { double s = 0; for (double x : v) s += x; return s; }
inline int64_t cppSumI32Poly(const std::vector<int32_t>& v) { int64_t s = 0; for (auto x : v) s += (int64_t)x; return s; }

#endif
