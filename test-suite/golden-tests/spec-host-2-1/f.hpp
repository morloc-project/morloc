#include <iostream>
#include <vector>

template <class F>
void emit(int a, int b, F sink){ sink(std::vector<int>{a, b}); }

void say(const std::vector<int>& xs){ std::cerr << "cpp " << xs[0] << " " << xs[1] << std::endl; }
