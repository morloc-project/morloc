// Member-agnostic pool host for the CAbi family.
//
// This translation unit owns main() and drives pool_main (the language-agnostic
// socket/worker loop in libmorloc). It knows nothing about C++ specifically: it
// parses argv, asks each member to register into the pool_config_t via an
// extern registration hook, then hands control to pool_main. The C++ member
// (pool.cpp) provides cpp_register.
//
// One member (C++) today; when a second CAbi member is added, this host
// aggregates their dispatch by manifold id before calling pool_main.

#include <string>
#include <iostream>
#include <cstring>
#include <cstdlib>
#include <cstdio>
#include <csignal>
#include <execinfo.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/prctl.h>
#endif

#include "morloc.h"

// Provided by the C++ member translation unit (pool.cpp).
extern void cpp_register(pool_config_t* config, const char* tmpdir);

// On a fatal signal, dump a native backtrace to stderr (captured in the test
// obs.err) so a C++ pool crash shows WHERE it died -- the nexus caller only sees
// "Connection closed by peer". Then restore the default handler and re-raise so
// the exit status still reflects the signal. backtrace_symbols_fd is
// async-signal-safe.
extern "C" void mlc_pool_crash_handler(int sig) {
    void* frames[64];
    int n = backtrace(frames, 64);
    char hdr[64];
    int len = std::snprintf(hdr, sizeof(hdr), "\n=== C++ pool fatal signal %d ===\n", sig);
    if (len > 0) { ssize_t w = ::write(2, hdr, (size_t)len); (void)w; }
    backtrace_symbols_fd(frames, n, 2);
    std::signal(sig, SIG_DFL);
    std::raise(sig);
}

int main(int argc, char* argv[]) {
    // Line-buffer stderr so diagnostic output is not lost on pool shutdown.
    // stdout is left fully buffered for performance (genome-scale piping)
    // and flushed after each job by pool_main.
    setvbuf(stderr, NULL, _IOLBF, 0);

    // Pre-warm backtrace() so the signal handler does not pay its first-call
    // cost inside a crash. glibc backtrace() lazily dlopen's the libgcc unwinder
    // and may malloc on first use; doing that from a handler entered on heap
    // corruption (holding the malloc lock) can deadlock. Resolving it now, while
    // the process is healthy, makes the in-handler backtrace_symbols_fd path
    // rely only on already-initialized state.
    {
        void* warm[4];
        (void) backtrace(warm, 4);
    }

    // Print a backtrace to stderr on a fatal signal (see mlc_pool_crash_handler).
    std::signal(SIGSEGV, mlc_pool_crash_handler);
    std::signal(SIGABRT, mlc_pool_crash_handler);
    std::signal(SIGBUS,  mlc_pool_crash_handler);
    std::signal(SIGILL,  mlc_pool_crash_handler);
    std::signal(SIGFPE,  mlc_pool_crash_handler);

    // Request SIGTERM when the parent (nexus) dies. Without this,
    // SIGKILL on the nexus leaves pool processes orphaned with
    // leaked SHM segments in /dev/shm.
#ifdef __linux__
    prctl(PR_SET_PDEATHSIG, SIGTERM);
#endif

    // Health check: confirm binary links and print version
    if (argc == 2 && std::string(argv[1]) == "--health") {
        std::cout << "{\"status\":\"ok\",\"version\":\"__MORLOC_VERSION__\"}" << std::endl;
        return 0;
    }

    if (argc != 4) {
        std::cerr << "Usage: " << argv[0] << " <socket_path> <tmpdir> <shm_basename>\n";
        return 1;
    }

    pool_config_t config = {};
    cpp_register(&config, argv[2]);

    return pool_main(argc, argv, &config);
}
