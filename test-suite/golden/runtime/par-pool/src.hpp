#pragma once
#include <atomic>
#include <cstdint>
#include <cstdio>
#include <dirent.h>
#include <pthread.h>
#include <signal.h>
#include <stdexcept>
#include <string>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#include <mutex>
#include <vector>
#ifdef __APPLE__
#include <mach/mach.h>
#endif

inline bool onHelper() {
    char name[16] = {0};
    pthread_getname_np(pthread_self(), name, sizeof(name));
    return std::string(name) == "morloc-par";
}

inline int64_t boomOnHelper(int64_t x) {
    if (onHelper()) throw std::runtime_error("boom on a helper");
    usleep(30000);
    return x;
}

inline std::string readStatus(const std::string& path, const std::string& key) {
    FILE* f = std::fopen(path.c_str(), "r");
    if (!f) return "";
    char line[256];
    std::string out;
    while (std::fgets(line, sizeof(line), f)) {
        std::string l(line);
        if (l.rfind(key, 0) == 0) out = l.substr(key.size());
    }
    std::fclose(f);
    while (!out.empty() && (out.back() == '\n' || out.front() == '\t' || out.front() == ' ')) {
        if (out.back() == '\n') out.pop_back(); else out.erase(0, 1);
    }
    return out;
}

inline std::string maskOf(const sigset_t& m) {
    std::string out;
    for (int s = 1; s < NSIG; ++s) out += sigismember(&m, s) ? '1' : '0';
    return out;
}

inline std::string myMask() {
    sigset_t m;
    pthread_sigmask(SIG_BLOCK, nullptr, &m);
    return maskOf(m);
}

#ifdef __APPLE__
// The morloc-par threads of this process; macOS cannot read another
// thread's mask, so a helper records its own as it runs (noteMask).
inline std::vector<std::string> helperMasks() {
    std::vector<std::string> out;
    thread_act_array_t threads;
    mach_msg_type_number_t n = 0;
    if (task_threads(mach_task_self(), &threads, &n) != KERN_SUCCESS) return out;
    for (mach_msg_type_number_t i = 0; i < n; ++i) {
        char name[64] = {0};
        pthread_t t = pthread_from_mach_thread_np(threads[i]);
        if (t && pthread_getname_np(t, name, sizeof(name)) == 0 && std::string(name) == "morloc-par") out.push_back("");
        mach_port_deallocate(mach_task_self(), threads[i]);
    }
    vm_deallocate(mach_task_self(), (vm_address_t)threads, n * sizeof(thread_act_t));
    return out;
}
#else
// The morloc-par threads of this process, and their SigBlk masks.
inline std::vector<std::string> helperMasks() {
    std::vector<std::string> out;
    DIR* d = opendir("/proc/self/task");
    if (!d) return out;
    while (dirent* e = readdir(d)) {
        if (e->d_name[0] == '.') continue;
        std::string task = std::string("/proc/self/task/") + e->d_name;
        if (readStatus(task + "/status", "Name:") == "morloc-par") out.push_back(readStatus(task + "/status", "SigBlk:"));
    }
    closedir(d);
    return out;
}
#endif

struct ForkFailCtx {
    pthread_t caller;
    std::atomic<int> held{0};
    std::string seen;
};
inline ForkFailCtx* forkFailCtx = nullptr;

inline void forkFailProbe() {
    if (!pthread_equal(pthread_self(), forkFailCtx->caller)) {
        forkFailCtx->held = 1;
        usleep(300000);
    }
}

// A helper's span fails and holds the job's lock while the caller's span
// forks; the child's span then fails too, and must exit before it waits on
// that lock, which no thread of the child will release.
inline void noWork(void*, size_t);

inline void forkFailBody(void* p, size_t) {
    ForkFailCtx* c = static_cast<ForkFailCtx*>(p);
    if (!pthread_equal(pthread_self(), c->caller)) throw std::runtime_error("helper span failed");
    // A nested call wakes the helpers of this call first (WPOOL-12).
    mlc::par_run(2, 2, noWork, nullptr);
    for (int k = 0; k < 5000 && !c->held; ++k) usleep(1000);
    if (!c->held) { c->seen = "no helper held the lock"; return; }
    pid_t pid = fork();
    if (pid == 0) throw std::runtime_error("child span failed");
    int st = 0;
    for (int k = 0; k < 10000; ++k) {
        if (waitpid(pid, &st, WNOHANG) == pid) {
            c->seen = WIFEXITED(st) ? "child exit " + std::to_string(WEXITSTATUS(st)) : "child signalled";
            return;
        }
        usleep(1000);
    }
    kill(pid, SIGKILL);
    waitpid(pid, &st, 0);
    c->seen = "child hung";
}

inline std::string forkFailUnderLock(int64_t) {
    ForkFailCtx c;
    c.caller = pthread_self();
    forkFailCtx = &c;
    mlc::par_failure_probe = forkFailProbe;
    std::string raised;
    try {
        mlc::par_run(2, 2, forkFailBody, &c);
    } catch (const std::runtime_error& e) {
        raised = e.what();
    }
    mlc::par_failure_probe = nullptr;
    return c.seen + "; raised: " + raised;
}

inline std::mutex seenMasksLock;
inline std::vector<std::string> seenMasks;

inline void noteMask(void*, size_t) {
    if (onHelper()) {
        std::lock_guard<std::mutex> g(seenMasksLock);
        seenMasks.push_back(myMask());
    }
    usleep(20000);
}

inline std::string helperMask(int64_t) {
#ifdef __APPLE__
    std::string mine = myMask();
#else
    std::string mine = readStatus("/proc/thread-self/status", "SigBlk:");
#endif
    sigset_t extra, old;
    sigemptyset(&extra);
    sigaddset(&extra, SIGUSR1);
    sigaddset(&extra, SIGHUP);
    pthread_sigmask(SIG_BLOCK, &extra, &old);
    mlc::par_run(8, 4, noteMask, nullptr);
    pthread_sigmask(SIG_SETMASK, &old, nullptr);
#ifdef __APPLE__
    std::vector<std::string> masks = seenMasks;
#else
    std::vector<std::string> masks = helperMasks();
#endif
    if (masks.empty()) return "no helpers";
    for (const std::string& m : masks) {
        if (m != mine) return "helper mask " + m + " differs from the worker's " + mine;
    }
    return "helpers have the worker's mask";
}

inline void noWork(void*, size_t) {
    timespec t0, t;
    clock_gettime(CLOCK_MONOTONIC, &t0);
    do clock_gettime(CLOCK_MONOTONIC, &t);
    while ((t.tv_sec - t0.tv_sec) * 1000000000L + (t.tv_nsec - t0.tv_nsec) < 2000);
}

inline void someWork(void*, size_t) {
    for (int k = 0; k < 10; ++k) noWork(nullptr, 0);
}

inline std::string helperCount(int64_t) {
    size_t most = 0;
    for (int k = 0; k < 20; ++k) {
        mlc::par_run(400, 400, someWork, nullptr);
        most = std::max(most, helperMasks().size());
    }
    if (most == 0) return "no helpers grew";
    if (most > 255) return "too many helpers: " + std::to_string(most);
    size_t keep = morloc_available_cpus() - 1;
    for (int k = 0; k < 5000 && helperMasks().size() > keep; ++k) usleep(1000);
    size_t left = helperMasks().size();
    if (left > keep) return std::to_string(left) + " helpers stayed past " + std::to_string(keep);
    return "helpers within the ceiling, and within the CPUs once idle";
}
