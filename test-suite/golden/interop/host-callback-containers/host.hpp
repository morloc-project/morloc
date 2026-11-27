#pragma once
#include <cstdint>
#include <fstream>
#include <functional>
#include <map>
#include <optional>
#include <string>
#include <sys/stat.h>
#include <tuple>
#include <vector>

struct Ops {
  std::function<int(int)> get;
};

struct Wrap {
  Ops inner;
};

inline int tick(const std::string& path) {
  {
    std::ofstream f(path, std::ios::app | std::ios::binary);
    f << "x";
  }
  struct stat st;
  stat(path.c_str(), &st);
  return static_cast<int>(st.st_size);
}

inline std::string ok_(int x) { return "ok " + std::to_string(x); }

template <class F>
std::string h_apply(F f, int n) { return ok_(f(n)); }

inline std::string h_ops(const Ops& o, int n) { return ok_(o.get(n)); }

inline std::string h_wrap(const Wrap& w, int n) { return ok_(w.inner.get(n)); }

inline std::string h_tup(const std::tuple<std::function<int(int)>, int>& p) {
  return ok_(std::get<0>(p)(std::get<1>(p)));
}

inline std::string h_list(const std::vector<std::function<int(int)>>& fs, int n) {
  return ok_(fs[0](n) + fs[1](n));
}

inline std::string h_opt(const std::optional<std::function<int(int)>>& f, int n) {
  return f ? ok_((*f)(n)) : std::string("none");
}

inline std::string h_map(const std::map<std::string, std::function<int(int)>>& m, int n) {
  return ok_(m.at("a")(n));
}

template <class S>
std::string h_susp_ops(S s, int n) { return ok_(s().get(n)); }

inline Ops h_make_ops(int k) {
  return Ops{[k](int i) { return i + k; }};
}

inline std::vector<std::function<int(int)>> h_make_list(int k) {
  return {[k](int i) { return i + k; }, [k](int i) { return i * k; }};
}

template <class F>
std::string h_cb_ret(F mk, int n) { return ok_(mk(n).get(n)); }

template <class F>
std::string h_cb_param(F cb) {
  return ok_(cb(Ops{[](int i) { return i * 10; }}));
}
