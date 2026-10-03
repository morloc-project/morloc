#ifndef __WORKERS_HPP__
#define __WORKERS_HPP__

#include <stdexcept>
#include <string>

#if defined(__linux__)
#include <dirent.h>
#include <fstream>
#elif defined(__APPLE__)
#include <mach/mach.h>
#endif

// Worker threads of this pool: the threads the runtime leaves unnamed, less
// the main thread, which accepts connections. The runtime names its helper
// threads; any other unnamed thread would be counted too, and this program
// starts none. On Linux an unnamed thread carries the process's name.
inline int cppWorkers(int done) {
    (void)done;
    int n = 0;
#if defined(__linux__)
    std::string self;
    std::getline(std::ifstream("/proc/self/comm"), self);
    if (DIR* d = opendir("/proc/self/task")) {
        while (struct dirent* e = readdir(d)) {
            if (e->d_name[0] == '.') continue;
            std::string name;
            std::getline(std::ifstream(std::string("/proc/self/task/") + e->d_name + "/comm"), name);
            if (name == self) n++;
        }
        closedir(d);
    }
#elif defined(__APPLE__)
    thread_act_array_t threads;
    mach_msg_type_number_t count = 0;
    if (task_threads(mach_task_self(), &threads, &count) != KERN_SUCCESS) {
        throw std::runtime_error("task_threads failed");
    }
    for (mach_msg_type_number_t i = 0; i < count; i++) {
        thread_extended_info_data_t info;
        mach_msg_type_number_t info_count = THREAD_EXTENDED_INFO_COUNT;
        if (thread_info(threads[i], THREAD_EXTENDED_INFO, (thread_info_t)&info, &info_count) == KERN_SUCCESS
            && info.pth_name[0] == '\0') {
            n++;
        }
        mach_port_deallocate(mach_task_self(), threads[i]);
    }
    vm_deallocate(mach_task_self(), (vm_address_t)threads, count * sizeof(thread_act_t));
#else
    throw std::runtime_error("cannot list this pool's threads on this platform");
#endif
    return n - 1;
}

inline int cppInc(int x) { return x + 1; }

#endif
