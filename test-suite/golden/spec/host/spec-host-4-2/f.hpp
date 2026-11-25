#include <cstdio>
#include <string>
#include <vector>
template <class S> void feed(int k, const std::string& p, S sink) { sink(std::vector<int>{k, (int)p.size()}); }
inline void put(const std::vector<int>& xs) { std::printf("%d\n", xs[0] + xs[1]); }
template <class S> void feed_all(const std::string& p, S sink) { sink(std::vector<int>{1, (int)p.size()}); }
