#pragma once
#include <sys/wait.h>
#include <unistd.h>
#include <stdexcept>
#include <vector>

template <typename F>
void seq_p(F sink) {
    sink(std::vector<int64_t>{1, 2});
    pid_t pid = fork();
    if (pid == 0) {
        sink(std::vector<int64_t>{5});
        _exit(0);
    }
    waitpid(pid, nullptr, 0);
    sink(std::vector<int64_t>{3});
}

template <typename F>
void returns_p(F sink) {
    sink(std::vector<int64_t>{1});
    pid_t pid = fork();
    if (pid == 0) {
        sink(std::vector<int64_t>{5});
        return;
    }
    waitpid(pid, nullptr, 0);
    sink(std::vector<int64_t>{3});
}

template <typename F>
void raises_p(F sink) {
    sink(std::vector<int64_t>{1});
    pid_t pid = fork();
    if (pid == 0) {
        sink(std::vector<int64_t>{5});
        throw std::runtime_error("the child raises");
    }
    waitpid(pid, nullptr, 0);
    sink(std::vector<int64_t>{3});
}
