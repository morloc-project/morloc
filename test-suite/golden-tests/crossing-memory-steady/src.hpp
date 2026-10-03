#ifndef __SRC_HPP__
#define __SRC_HPP__

#include <cstdint>
#include <cstdio>
#include <fstream>
#include <stdexcept>
#include <string>
#include <vector>

#if defined(__APPLE__)
#include <malloc/malloc.h>
#else
#include <malloc.h>
#endif

inline long long total(const std::vector<int>& xs) {
    long long s = 0;
    for (auto x : xs) s += x;
    return s;
}

// Bytes of shared memory mapped into this process (Linux; elsewhere 0).
inline long long shmemBytes() {
    std::ifstream f("/proc/self/status");
    std::string line;
    while (std::getline(f, line)) {
        if (line.rfind("RssShmem:", 0) == 0) return std::stoll(line.substr(9)) * 1024;
    }
    return 0;
}

// Bytes this process holds: live allocations plus mapped shared memory.
inline long long heldBytes() {
#if defined(__APPLE__)
    long long heap = (long long)mstats().bytes_used;
#elif defined(__GLIBC__) && (__GLIBC__ > 2 || __GLIBC_MINOR__ >= 33)
    struct mallinfo2 mi = mallinfo2();
    long long heap = (long long)mi.uordblks + (long long)mi.hblkhd;
#else
    struct mallinfo mi = mallinfo();
    long long heap = (long long)(unsigned)mi.uordblks + (long long)(unsigned)mi.hblkhd;
#endif
    return heap + shmemBytes();
}

inline long long& markedBytes() {
    static long long b = -1;
    return b;
}

inline long long mark(long long i, long long x) {
    if (i == 1000) markedBytes() = heldBytes();
    return x;
}

// Thread count and huge-page use, for reading a failure against the
// machine it ran on.
inline void reportLayout() {
    std::ifstream status("/proc/self/status");
    std::string line, header;
    while (std::getline(status, line)) {
        if (line.rfind("Threads:", 0) == 0 || line.rfind("Rss", 0) == 0) {
            fprintf(stderr, "%s\n", line.c_str());
        }
    }
    std::ifstream smaps("/proc/self/smaps");
    while (std::getline(smaps, line)) {
        if (!line.empty() && line.find(':') > line.find(' ')) {
            header = line;
        } else if (line.rfind("AnonHugePages:", 0) == 0 && line.find(" 0 kB") == std::string::npos) {
            fprintf(stderr, "%s | %s\n", header.c_str(), line.c_str());
        }
    }
}

// KiB held now beyond what was held at the mark. The argument is the run's
// result: it is what makes the measurement happen after the run.
inline long long grownKiB(long long done) {
    (void)done;
    reportLayout();
    if (markedBytes() < 0) throw std::runtime_error("the run ended before the mark");
    return (heldBytes() - markedBytes()) / 1024;
}

#endif
