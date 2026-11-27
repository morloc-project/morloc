#pragma once
#include <fstream>
#include <functional>
#include <optional>
#include <stdexcept>
#include <string>
#include <sys/stat.h>
#include <tuple>
#include <vector>

struct Holder {
  std::function<int()> run;
  int tag;
};

struct Caller {
  std::function<int(int)> inc;
};

struct Purer {
  std::function<int(int)> apply;
};


inline int home_ident(int x) { return x; }
inline int away_ident(int x) { return x; }

inline int home_tick(std::string path) {
  {
    std::ofstream f(path, std::ios::app | std::ios::binary);
    f << "x";
  }
  struct stat st;
  stat(path.c_str(), &st);
  return static_cast<int>(st.st_size);
}

inline std::string home_mark(std::string s) { return s; }
inline int home_str_len(std::string s) { return static_cast<int>(s.size()); }
inline int home_add(int x, int y) { return x + y; }
inline int home_fail(std::string path) { throw std::runtime_error("failHome ran: " + path); }
inline std::function<int(int)> home_mk(int k) { return [k](int x) { return x + k; }; }
inline std::vector<int> home_big(int n) {
  std::vector<int> v;
  for (int i = 0; i < n; i++) v.push_back(i);
  return v;
}
inline int home_use_pure(std::function<int(int)> f, int x) { return f(x); }
inline int home_take_thunk(std::function<int()> t) {
  t();
  t();
  return t();
}



inline int away_bump(std::string path, int) {
  {
    std::ofstream f(path, std::ios::app | std::ios::binary);
    f << "x";
  }
  struct stat st;
  stat(path.c_str(), &st);
  return static_cast<int>(st.st_size);
}

inline int away_take_thunk(std::function<int()> t) {
  t();
  t();
  return t();
}

inline int away_drop_thunk(std::function<int()>) { return 0; }

inline int away_use_record(Holder h) {
  h.run();
  h.run();
  return h.run() + h.tag;
}

inline int away_use_list(std::vector<std::function<int()>> xs) {
  xs[0]();
  xs[0]();
  return xs[0]();
}

inline int away_use_tuple(std::tuple<std::function<int()>, int> t) {
  std::get<0>(t)();
  std::get<0>(t)();
  return std::get<0>(t)() + std::get<1>(t);
}

inline int away_use_opt(std::optional<std::function<int()>> t) {
  if (!t.has_value()) return 0;
  (*t)();
  (*t)();
  return (*t)();
}

inline int away_use_nested(std::vector<Holder> hs) {
  hs[0].run();
  hs[0].run();
  return hs[0].run();
}

inline int away_use_callback(std::function<int(int)> f, int x) {
  f(x);
  f(x);
  return f(x);
}

inline int away_use_pure(std::function<int(int)> f, int x) { return f(x); }
inline int away_use_arity2(std::function<int(int, int)> f, int x, int y) { return f(x, y); }

inline int away_use_pinned(Caller c, int x) {
  c.inc(x);
  c.inc(x);
  return c.inc(x);
}

inline int away_use_hof(std::function<int(std::function<int(int)>)> f, std::function<int(int)> g) { return f(g); }

inline int away_use_list_pure(std::vector<std::function<int(int)>> fs, int x) {
  int s = 0;
  for (auto& f : fs) s += f(x);
  return s;
}

inline int away_use_mk(std::function<std::function<int(int)>(int)> f, int a, int b) { return f(a)(b); }

inline int away_use_thunk2(std::function<std::function<int()>()> t) {
  auto inner = t();
  inner();
  inner();
  return inner();
}

inline int away_use_thunk_fn(std::function<std::function<int(int)>()> t, int x) { return t()(x); }

inline int away_use_fn_thunk(std::function<int(std::function<int()>)> f, std::function<int()> t) { return f(t); }

inline std::function<int(int)> away_pass_through(std::function<int(int)> f) { return f; }

template <class F>
std::vector<int> away_map_away(F f, const std::vector<int>& xs) {
  std::vector<int> ys;
  for (int x : xs) ys.push_back(f(x));
  return ys;
}

inline int away_use_hof_eff(std::function<int(std::function<int(int)>)> f, std::function<int(int)> g) { return f(g); }

inline int away_use_hof2(std::function<int(std::function<int(std::function<int(int)>)>)> f) {
  return f([](std::function<int(int)> g) { return g(3); });
}

inline int away_two(std::string a, std::string b, int) {
  {
    std::ofstream f(a, std::ios::app | std::ios::binary);
    f << "x";
  }
  struct stat st;
  stat(a.c_str(), &st);
  return static_cast<int>(st.st_size) + static_cast<int>(b.size());
}

inline int away_mix(int x, std::string a) {
  {
    std::ofstream f(a, std::ios::app | std::ios::binary);
    f << "x";
  }
  struct stat st;
  stat(a.c_str(), &st);
  return static_cast<int>(st.st_size) + x;
}
