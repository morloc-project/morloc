#pragma once
#include <functional>

inline int take_thunk(std::function<int()> t) { return t(); }
