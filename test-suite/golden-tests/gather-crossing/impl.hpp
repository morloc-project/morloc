#pragma once
#include <cstdint>
#include <cstdlib>
#include <filesystem>
#include <string>
#include <vector>
template <typename F> void produce(F sink) { sink(std::vector<int64_t>{2}); }
inline std::string show_ints(const std::vector<int64_t>& xs) { std::string o="cpp"; for (auto x: xs) o += " " + std::to_string(x); return o + "\n"; }
inline int64_t dec(int64_t n) { return n - 1; }
inline bool is_zero(int64_t n) { return n == 0; }

// 1000 plus the number of morloc-gather-* files under the run's temp dir.
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
}
