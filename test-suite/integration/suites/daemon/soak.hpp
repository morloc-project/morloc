#ifndef MORLOC_SOAK_HPP
#define MORLOC_SOAK_HPP

#include <vector>

// Identity over a list. The caller chooses the length, so a payload can be
// pushed past the size at which a value stops being carried inside the packet
// and starts being handed over in shared memory -- which is the allocation
// path a concurrent soak needs to put under pressure.
//
// Templated on the element type rather than naming one: this file only cares
// that the list arrives and leaves unchanged, and the concrete C++ type behind
// a morloc Int is the code generator's business, not this test's.
template <typename T>
inline std::vector<T> cppEcho(const std::vector<T>& xs) {
    return xs;
}

#endif
