#ifndef __ISTREAM_PAGES_RELEASED_HPP__
#define __ISTREAM_PAGES_RELEASED_HPP__

#include <sys/resource.h>
#include <vector>

// The most memory this pool has ever held, in KiB. The argument is the
// drain's result: it is what makes the measurement happen after the drain.
inline int peakKiB(int done) {
    (void)done;
    struct rusage ru;
    getrusage(RUSAGE_SELF, &ru);
    return (int)ru.ru_maxrss;
}

#endif
