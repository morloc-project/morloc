#pragma once
#include <cstdint>
#include <cstdlib>
#include <filesystem>
#include <string>
#include <vector>

// 1000 plus the number of morloc-gather-* files under the run's temp dir,
// then 2 and 3.
template <typename F> void produce_route(F sink) {
    const char* dir = std::getenv("MORLOC_TMPDIR");
    int64_t n = 0;
    std::error_code ec;
    for (auto it = std::filesystem::recursive_directory_iterator(dir, ec);
         dir != nullptr && it != std::filesystem::recursive_directory_iterator(); it.increment(ec)) {
        if (ec) break;
        if (it->path().filename().string().rfind("morloc-gather-", 0) == 0) n++;
    }
    sink(std::vector<int64_t>{1000 + n});
    sink(std::vector<int64_t>{2});
    sink(std::vector<int64_t>{3});
}
inline int add_sum(int acc, const std::vector<int64_t>& xs) { for (auto x : xs) acc += (int)x; return acc; }
inline int merge(int a, int b) { return a + b; }
inline std::string show_n(int n) { return "cpp n=" + std::to_string(n) + "\n"; }
