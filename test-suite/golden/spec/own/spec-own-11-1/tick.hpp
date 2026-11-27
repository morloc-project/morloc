#pragma once
#include <stdexcept>
#include <tuple>

static int tick_count = 0;

int tick(int x) { tick_count++; return x; }
int boom(int x) { throw std::runtime_error("boom " + std::to_string(x)); }
std::tuple<int, int> ticks(int x) { return std::make_tuple(x, tick_count); }
int bump(int x) { tick_count++; return x; }
