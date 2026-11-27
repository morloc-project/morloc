#pragma once
#include <fstream>
#include <functional>
#include <string>
#include <sys/stat.h>
#include <vector>

struct Holder {
  std::function<int()> run;
};

struct Caller {
  std::function<int(int)> inc;
};

inline int tick(std::string path) {
  {
    std::ofstream f(path, std::ios::app | std::ios::binary);
    f << "x";
  }
  struct stat st;
  stat(path.c_str(), &st);
  return static_cast<int>(st.st_size);
}

inline std::string mark(std::string s) { return s; }

inline int ident(int x) { return x; }

inline int take_thunk(std::function<int()> t) {
  t();
  t();
  return t();
}

inline int use_record(Holder h) {
  h.run();
  h.run();
  return h.run();
}

inline int use_list(std::vector<std::function<int()>> xs) {
  xs[0]();
  xs[0]();
  return xs[0]();
}

inline int use_callback(std::function<int(int)> f, int x) {
  f(x);
  f(x);
  return f(x);
}

inline int use_pinned(Caller c, int x) {
  c.inc(x);
  c.inc(x);
  return c.inc(x);
}
