#pragma once
#include <cstdint>
#include <string>
#include <vector>
#include "mlc_arrow.hpp"

struct gene_t {
  std::string name;
  std::string chrom;
  int64_t start;
  int64_t end;
  double score;
  bool keep;
};

template <class T>
int64_t count(const std::vector<T>& xs) { return (int64_t)xs.size(); }

inline int64_t table_rows(const mlc::ArrowTable& t) { return t.n_rows(); }

template <class T>
T ident(const T& x) { return x; }
