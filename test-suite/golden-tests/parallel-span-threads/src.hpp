#pragma once
#include <cstdint>
#include <cstdio>
#include <string>
#include <vector>
#include <sys/wait.h>
#include <unistd.h>
#include "mlc_arrow.hpp"
#include <nanoarrow/nanoarrow.h>

inline double tableSum(const mlc::ArrowTable& table) {
    struct ArrowArrayView view;
    ArrowArrayViewInitFromSchema(&view, table.schema(), nullptr);
    ArrowArrayViewSetArray(&view, table.array(), nullptr);
    double s = 0;
    for (int64_t i = 0; i < table.n_rows(); ++i) s += ArrowArrayViewGetDoubleUnsafe(view.children[1], i);
    ArrowArrayViewReset(&view);
    return s;
}

inline double listSum(const std::vector<double>& xs) {
    double s = 0;
    for (double x : xs) s += x;
    return s;
}

inline void logLine(const std::string& s) {
    FILE* f = std::fopen("calls.log", "a");
    std::fprintf(f, "%s\n", s.c_str());
    std::fclose(f);
}

// Each element forks; the child returns as user code that forgot to exit
// would, and logs any element it is handed after that.
inline int64_t forkEach(int64_t x) {
    static thread_local bool forked_child = false;
    logLine((forked_child ? "child ran " : "ran ") + std::to_string(x));
    usleep(20000);
    pid_t pid = fork();
    if (pid == 0) {
        forked_child = true;
        return x * 10;
    }
    int st = 0;
    waitpid(pid, &st, 0);
    logLine(WIFEXITED(st) ? "child exit " + std::to_string(WEXITSTATUS(st)) : "child killed");
    return x * 10;
}
