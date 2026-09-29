#pragma once
#include <cstdint>
#include <fstream>
#include <string>
#include <sys/stat.h>

inline int tick(const std::string& path) {
  {
    std::ofstream f(path, std::ios::app | std::ios::binary);
    f << "x";
  }
  struct stat st;
  stat(path.c_str(), &st);
  return static_cast<int>(st.st_size);
}

inline int count(const std::string& path) {
  struct stat st;
  return stat(path.c_str(), &st) == 0 ? static_cast<int>(st.st_size) : 0;
}

template <class F, class A>
auto h_pass(F f, A x) { return f(x); }
