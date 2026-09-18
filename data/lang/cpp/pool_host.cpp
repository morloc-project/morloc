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
#include <unistd.h>
#ifdef __linux__
#include <sys/prctl.h>
#endif

#include "morloc.h"

// Provided by the C++ member translation unit (pool.cpp).
extern void cpp_register(pool_config_t* config, const char* tmpdir);

// The manifold this thread is executing, kept by the C++ member (cppmorloc)
// for the crash report.
extern "C" const char* mlc_current_frame(size_t* len);

int main(int argc, char* argv[]) {
    // Line-buffer stderr so diagnostic output is not lost on pool shutdown.
    // stdout is left fully buffered for performance (genome-scale piping)
    // and flushed after each job by pool_main.
    setvbuf(stderr, NULL, _IOLBF, 0);

    // On a fatal signal, report the signal, the executing manifold and a
    // backtrace, then die of the signal (see morloc_install_crash_handler).
    morloc_install_crash_handler("cpp", mlc_current_frame);

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
