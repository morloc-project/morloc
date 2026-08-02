#pragma once
#include <functional>

// A user-mapped record `record Cpp => Ops = "Ops"` whose fields are runtime
// closures (std::function); morloc builds the struct and reifies each field on
// serialization.
struct Ops {
  std::function<int(int)> inc;
  std::function<int(int)> scale;
};

inline int tick() { return 0; }
inline int add(int a, int b) { return a + b; }
inline int mul(int a, int b) { return a * b; }
