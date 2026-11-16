#pragma once
#include "mlc/types/_local/main.hpp"
using mlc::types::_local::main::Container;
inline int innerCI(const Container<Container<int>>& c) { return c->val->val; }
inline int innerFoo(const Container<Container<int>>& c) { return c->val->val; }
