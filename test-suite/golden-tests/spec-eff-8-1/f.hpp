#include <iostream>
#include <cstdint>

void say(int64_t x){ std::cerr << "cpp " << x << std::endl; }

int64_t tick(int64_t x){ std::cerr << "cpp tick " << x << std::endl; return x + 1; }
