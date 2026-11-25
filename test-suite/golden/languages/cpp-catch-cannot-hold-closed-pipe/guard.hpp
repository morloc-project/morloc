#include <cstdio>
#include <functional>
#include <stdexcept>
inline int guarded(std::function<int()> t) {
  try { return t(); } catch (const std::exception&) { std::fprintf(stderr, "CAUGHT\n"); return -1; }
}
