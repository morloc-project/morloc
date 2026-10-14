#pragma once
#include <cstdint>
#include <cstring>
#include <vector>

std::vector<uint8_t> mk_u8(uint64_t n) {
    std::vector<uint8_t> v(n);
    for (uint64_t i = 0; i < n; i++) v[i] = (uint8_t)(i * 7 % 251);
    return v;
}
std::vector<int32_t> mk_i32(uint64_t n) {
    std::vector<int32_t> v(n);
    for (uint64_t i = 0; i < n; i++) v[i] = (int32_t)i - 500000;
    return v;
}
std::vector<float> mk_f32(uint64_t n) {
    std::vector<float> v(n);
    for (uint64_t i = 0; i < n; i++) v[i] = (float)(i % 1000) * 0.5f;
    return v;
}
std::vector<double> mk_real(uint64_t n) {
    std::vector<double> v(n);
    for (uint64_t i = 0; i < n; i++) v[i] = (double)i * 0.25;
    return v;
}
std::vector<bool> mk_bool(uint64_t n) {
    std::vector<bool> v(n);
    for (uint64_t i = 0; i < n; i++) v[i] = i % 3 == 0;
    return v;
}

uint64_t sum_u8(const std::vector<uint8_t>& xs) {
    uint64_t s = 0;
    for (size_t i = 0; i < xs.size(); i++) s += (i % 13 + 1) * (uint64_t)xs[i];
    return s;
}
int64_t sum_i32(const std::vector<int32_t>& xs) {
    int64_t s = 0;
    for (size_t i = 0; i < xs.size(); i++) s += (int64_t)(i % 13 + 1) * xs[i];
    return s;
}
double sum_f32(const std::vector<float>& xs) {
    double s = 0;
    for (size_t i = 0; i < xs.size(); i++) s += (double)(i % 13 + 1) * xs[i];
    return s;
}
double sum_real(const std::vector<double>& xs) {
    double s = 0;
    for (size_t i = 0; i < xs.size(); i++) s += (double)(i % 13 + 1) * xs[i];
    return s;
}
uint64_t count_bool(const std::vector<bool>& xs) {
    uint64_t s = 0;
    for (size_t i = 0; i < xs.size(); i++) if (xs[i]) s += i % 13 + 1;
    return s;
}

float snan(uint64_t n) {
    uint32_t b = 0x7f800000u | ((uint32_t)n & 0x3fffffu);
    float f;
    std::memcpy(&f, &b, 4);
    return f;
}
std::vector<float> snan_vec(uint64_t n) {
    std::vector<float> v;
    for (uint64_t i = 1; i <= n; i++) v.push_back(snan(i));
    return v;
}
uint32_t bits(float x) {
    uint32_t b;
    std::memcpy(&b, &x, 4);
    return b;
}
std::vector<uint32_t> bits_vec(const std::vector<float>& xs) {
    std::vector<uint32_t> v;
    for (float x : xs) v.push_back(bits(x));
    return v;
}
