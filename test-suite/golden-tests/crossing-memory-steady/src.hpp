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
inline long long peakMiB(long long done) {
    (void)done;
    struct rusage ru;
    getrusage(RUSAGE_SELF, &ru);
    // ru_maxrss is kibibytes on Linux and bytes on macOS; the comparison
    // this feeds is between two runs of the same binary, so either does.
    return (long long)(ru.ru_maxrss >> 10);
}

#endif
