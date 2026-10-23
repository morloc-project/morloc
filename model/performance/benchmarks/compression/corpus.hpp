#pragma once
// Benchmark corpora. Element i depends only on (kind, i), so a corpus is
// the same bytes however it is split into batches.
#include <algorithm>
#include <cstdint>
#include <string>
#include <vector>

inline uint64_t mix(uint64_t x) {
  x += 0x9e3779b97f4a7c15ull;
  x = (x ^ (x >> 30)) * 0xbf58476d1ce4e5b9ull;
  x = (x ^ (x >> 27)) * 0x94d049bb133111ebull;
  return x ^ (x >> 31);
}

// kind 0 uniform 64-bit, 1 below 2^20, 2 sorted with deltas 0..15, 3 zero
std::vector<uint64_t> gen_u64(int64_t kind, int64_t start, int64_t n) {
  std::vector<uint64_t> v(n);
  for (int64_t j = 0; j < n; j++) {
    uint64_t i = start + j;
    switch (kind) {
      case 0: v[j] = mix(i); break;
      case 1: v[j] = mix(i) & 0xFFFFF; break;
      case 2: v[j] = 8 * i + (mix(i) & 15); break;
      default: v[j] = 0;
    }
  }
  return v;
}

// A random walk sampled with noise: what a measurement series looks like.
std::vector<double> gen_real(int64_t start, int64_t n) {
  std::vector<double> v(n);
  for (int64_t j = 0; j < n; j++) {
    uint64_t i = start + j;
    double trend = 0.001 * (double)i;
    double noise = (double)(mix(i) >> 11) / 9007199254740992.0 - 0.5;
    v[j] = trend + noise;
  }
  return v;
}

// kind 0: 150-base DNA reads; kind 1: FASTQ lines (header, bases, '+',
// qualities), one line per element.
std::vector<std::string> gen_str(int64_t kind, int64_t start, int64_t n) {
  static const char bases[] = "ACGT";
  std::vector<std::string> v(n);
  for (int64_t j = 0; j < n; j++) {
    uint64_t i = start + j;
    uint64_t r = i / 4;
    int line = kind == 0 ? 1 : (int)(i % 4);
    std::string s;
    if (line == 0) {
      s = "@SRR000001." + std::to_string(r) + " " + std::to_string(r) + "/1";
    } else if (line == 2) {
      s = "+";
    } else {
      s.resize(150);
      uint64_t h = 0;
      for (int k = 0; k < 150; k++) {
        if (k % 32 == 0) h = mix(r * 8 + k / 32 + (line == 3 ? 4 : 0));
        if (line == 1) {
          s[k] = bases[h & 3];
          h >>= 2;
        } else {
          int q = 40 - (int)(h & 7) - k / 15;
          s[k] = (char)(33 + std::max(q, 2));
          h >>= 3;
        }
      }
    }
    v[j] = s;
  }
  return v;
}

int64_t batch_len(int64_t i, int64_t n, int64_t b) { return std::min(b, n - i); }

template <class T>
int64_t count(const std::vector<T>& xs) { return (int64_t)xs.size(); }
