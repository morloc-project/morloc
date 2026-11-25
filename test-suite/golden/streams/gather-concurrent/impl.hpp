#pragma once
#include <algorithm>
#include <cstdint>
#include <map>
#include <set>
#include <string>
#include <thread>
#include <vector>
#include <sys/wait.h>
#include <unistd.h>

template <typename F>
void produce_threads(int64_t nthreads, int64_t nbatches, F sink) {
    std::vector<std::thread> ts;
    for (int64_t t = 0; t < nthreads; t++) {
        ts.emplace_back([&, t] {
            for (int64_t b = 0; b < nbatches; b++) {
                int64_t v = t * 1000 + b * 4;
                sink(std::vector<int64_t>{v, v + 1, v + 2, v + 3});
            }
        });
    }
    for (auto& t : ts) t.join();
}

template <typename F>
void produce_forks(int64_t nchildren, int64_t big, F sink) {
    sink(std::vector<int64_t>{0});
    std::vector<pid_t> pids;
    for (int64_t c = 1; c <= nchildren; c++) {
        pid_t pid = fork();
        if (pid == 0) {
            sink(std::vector<int64_t>{10 * c + 1, 10 * c + 2});
            sink(std::vector<int64_t>{10 * c + 3});
            if (big > 0) sink(std::vector<int64_t>(big, 7));
            _exit(0);
        }
        pids.push_back(pid);
    }
    for (pid_t pid : pids) {
        int status = 0;
        waitpid(pid, &status, 0);
        if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) sink(std::vector<int64_t>{-1});
    }
    sink(std::vector<int64_t>{99});
}

inline std::string tally(const std::vector<int64_t>& xs) {
    int64_t sum = 0;
    for (auto x : xs) sum += x;
    bool whole = xs.size() % 4 == 0;
    for (size_t i = 0; whole && i < xs.size(); i += 4)
        for (size_t k = 0; k < 4; k++)
            if (xs[i] % 4 != 0 || xs[i + k] != xs[i] + (int64_t)k) whole = false;
    std::map<int64_t, int64_t> last;
    bool ordered = true;
    for (auto x : xs) {
        auto it = last.find(x / 1000);
        if (it != last.end() && it->second >= x) ordered = false;
        last[x / 1000] = x;
    }
    std::set<int64_t> d(xs.begin(), xs.end());
    return "cpp n=" + std::to_string(xs.size()) + " distinct=" + std::to_string(d.size()) +
           " sum=" + std::to_string(sum) + " whole=" + (whole ? "yes" : "no") +
           " ordered=" + (ordered ? "yes" : "no") + "\n";
}

inline std::string sorted(const std::vector<int64_t>& xs) {
    std::vector<int64_t> ys = xs;
    std::sort(ys.begin(), ys.end());
    std::string out = "cpp";
    for (auto y : ys) out += " " + std::to_string(y);
    return out + "\n";
}

inline std::string count(const std::vector<int64_t>& xs) {
    return "cpp n=" + std::to_string(xs.size()) + "\n";
}
