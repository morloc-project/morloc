#pragma once
#include <memory>
#include <string>
#include "mlc/types/_local/main.hpp"
template <class A> A same(const A& x) { return x; }
inline int innerVal(const mlc::types::_local::main::Container<mlc::types::_local::main::Container<int>>& c) { return c->val->val; }
