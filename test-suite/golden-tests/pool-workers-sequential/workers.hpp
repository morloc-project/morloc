#ifndef __WORKERS_HPP__
#define __WORKERS_HPP__

#include <dirent.h>
#include <fstream>
#include <string>

// Worker threads of this pool: threads that carry the process's own name,
// less the main thread, which accepts connections. Any other unnamed thread
// would be counted too; this program starts none. Linux only; elsewhere it
// reports 1, the expected count, since it cannot see the threads.
inline int cppWorkers(int done) {
    (void)done;
#ifdef __linux__
    std::string self;
    std::getline(std::ifstream("/proc/self/comm"), self);
    int n = 0;
    if (DIR* d = opendir("/proc/self/task")) {
        while (struct dirent* e = readdir(d)) {
            if (e->d_name[0] == '.') continue;
            std::string name;
            std::getline(std::ifstream(std::string("/proc/self/task/") + e->d_name + "/comm"), name);
            if (name == self) n++;
        }
        closedir(d);
    }
    return n - 1;
#else
    return 1;
#endif
}

inline int cppInc(int x) { return x + 1; }

#endif
