#pragma once
#include <memory>
#include <string>
#include <vector>
#include "mlc/types/_local/main.hpp"
template <class A> A same(const A& x) { return x; }
inline int firstVal(const mlc::types::_local::main::Container<std::vector<mlc::types::_local::main::Container<int>>>& c) { return c->val.at(0)->val; }
