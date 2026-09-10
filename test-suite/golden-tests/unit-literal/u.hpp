#pragma once
#include <cstdint>
#include <iostream>

inline int64_t ident(int64_t x) { return x; }

inline void shout(int64_t x) { std::cout << "cpp shout " << x << std::endl; }
