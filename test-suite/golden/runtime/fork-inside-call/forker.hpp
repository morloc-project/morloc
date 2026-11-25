#pragma once
#include <string>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

std::string cppForkReturns(int x) {
    (void)x;
    pid_t pid = fork();
    if (pid == 0) {
        return "child";
    }
    int status = 0;
    waitpid(pid, &status, 0);
    int code = WIFEXITED(status) ? WEXITSTATUS(status) : 100 + WTERMSIG(status);
    return "parent:" + std::to_string(code);
}
