#pragma once
#include <cstdint>
#include <cstdlib>
#include <filesystem>
#include <map>
#include <string>
#include <vector>
#include <sys/wait.h>
#include <unistd.h>

inline int64_t gather_files() {
    const char* dir = std::getenv("MORLOC_TMPDIR");
    if (dir == nullptr) return -1;
    int64_t n = 0;
    std::error_code ec;
    for (auto it = std::filesystem::recursive_directory_iterator(dir, ec);
         it != std::filesystem::recursive_directory_iterator(); it.increment(ec)) {
        if (ec) break;
        if (it->path().filename().string().rfind("morloc-gather-", 0) == 0) n++;
    }
    return n;
}

template <typename F>
void produce_route(F sink) {
    sink(std::vector<int64_t>{0, 1});
    sink(std::vector<int64_t>{});
    sink(std::vector<int64_t>{2, 3, 4});
    sink(std::vector<int64_t>{1000 + gather_files()});
}

template <typename F>
void produce_forks(F sink) {
    sink(std::vector<int64_t>{0});
    for (int64_t c = 1; c <= 3; c++) {
        pid_t pid = fork();
        if (pid == 0) {
            sink(std::vector<int64_t>{10 * c + 1});
            sink(std::vector<int64_t>{10 * c + 2});
            sink(std::vector<int64_t>{10 * c + 3});
            _exit(0);
        }
        int status = 0;
        waitpid(pid, &status, 0);
        if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) sink(std::vector<int64_t>{-1});
        sink(std::vector<int64_t>{50 + c});
    }
    sink(std::vector<int64_t>{99});
}

inline std::string show_ints(const std::vector<int64_t>& xs) {
    std::string out = "cpp";
    for (auto x : xs) out += " " + std::to_string(x);
    return out + "\n";
}

inline int add_batch(int acc, const std::vector<int64_t>& xs) { return acc + (int)xs.size(); }
inline int merge(int a, int b) { return a + b; }
inline std::string show_n(int n) { return "cpp n=" + std::to_string(n) + "\n"; }

using smap = std::map<std::string, int64_t>;

template <typename F>
void produce_maps(F sink) {
    sink(std::vector<smap>{smap{{"a", 1}}, smap{}});
    sink(std::vector<smap>{smap{{"b", 2}, {"c", 3}}});
}

template <typename F>
void produce_map_forks(F sink) {
    sink(std::vector<smap>{smap{{"a", 1}}});
    pid_t pid = fork();
    if (pid == 0) {
        try {
            sink(std::vector<smap>{smap{{"b", 2}}});
        } catch (...) {
            _exit(3);
        }
        _exit(0);
    }
    int status = 0;
    waitpid(pid, &status, 0);
    sink(std::vector<smap>{smap{{"c", 3}}});
}

inline std::string show_maps(const std::vector<smap>& xs) {
    std::string out = "cpp";
    for (const auto& m : xs) out += " " + std::to_string(m.size());
    return out + "\n";
}

inline int add_maps(int acc, const std::vector<smap>& xs) { return acc + (int)xs.size(); }
