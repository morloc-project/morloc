#ifndef __MORLOC_TYPE_IDENTITIES_TYPES_H__
#define __MORLOC_TYPE_IDENTITIES_TYPES_H__

#include <cstdint>

// packSizeT   Cpp :: pack   => Int -> SizeT
size_t packSizeT(int x) {
    return static_cast<size_t>(x);
}

// unpackSizeT Cpp :: unpack => SizeT -> Int
int unpackSizeT(size_t x) {
    return static_cast<int>(x);
}

// packLong   Cpp :: pack   => Int -> Long
int64_t packLong(int x) {
    return static_cast<int64_t>(x);
}

// unpackLong Cpp :: unpack => Long -> Int
int unpackLong(int64_t x) {
    return static_cast<int>(x);
}

#endif
