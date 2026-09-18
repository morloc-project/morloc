#ifndef __SRC_HPP__
#define __SRC_HPP__

#include "mlc_arrow.hpp"
#include <nanoarrow/nanoarrow.h>
#include <cstring>
#include <string>

// A decimal128 value is 16 bytes of two's-complement little-endian; read
// it as a scaled integer, which is all this needs of it.
inline std::string price(const mlc::ArrowTable& t) {
    const ArrowArray* col = t.array()->children[1];
    int64_t scaled = 0;
    memcpy(&scaled, (const uint8_t*)col->buffers[1], sizeof(scaled));
    char buf[32];
    snprintf(buf, sizeof(buf), "%lld.%02lld", (long long)(scaled / 100), (long long)(scaled % 100));
    return std::string(buf) + " " + t.schema()->children[1]->format;
}

// A string view's first 4 bytes are the length; a value this short is
// stored inline in the 12 that follow.
inline std::string label(const mlc::ArrowTable& t) {
    const ArrowArray* col = t.array()->children[1];
    const uint8_t* view = (const uint8_t*)col->buffers[1];
    int32_t len = 0;
    memcpy(&len, view, sizeof(len));
    return std::string((const char*)(view + 4), (size_t)len)
        + " " + t.schema()->children[1]->format;
}

#endif
