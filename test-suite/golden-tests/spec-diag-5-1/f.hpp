#pragma once
#include <stdexcept>
int boom(int x) { throw std::runtime_error("boom"); }
