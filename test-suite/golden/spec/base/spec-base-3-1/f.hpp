#pragma once
#include <stdexcept>
bool boom(int x) { throw std::runtime_error("evaluated"); }
