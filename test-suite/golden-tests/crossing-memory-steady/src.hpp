#ifndef __SRC_HPP__
#define __SRC_HPP__

#include <sys/resource.h>
#include <cstdint>
#include <vector>

inline long long total(const std::vector<int>& xs) {
    long long s = 0;
    for (auto x : xs) s += x;
    return s;
}

// The most memory this pool has ever held. The argument is the run's
// result: it is what makes the measurement happen after the run.
inline long long peakKiB(long long done) {
    (void)done;
    struct rusage ru;
    getrusage(RUSAGE_SELF, &ru);
    // ru_maxrss is kibibytes on Linux and bytes on macOS.
#ifdef __APPLE__
    return (long long)ru.ru_maxrss / 1024;
#else
    return (long long)ru.ru_maxrss;
#endif
}

#endif
