#ifndef MAPPINGS_SRC_HPP
#define MAPPINGS_SRC_HPP

// A minimal C++ describer: "name:format:[values]" per column, for the
// flat types this test produces. Its job is to be a consumer in another
// language, so the table crosses a pool boundary.

#include "mlc_arrow.hpp"
#include <nanoarrow/nanoarrow.h>
#include <cstdio>
#include <stdexcept>
#include <string>

inline std::string describeCpp(const mlc::ArrowTable& t) {
    ArrowArrayView view;
    ArrowError err;
    if (ArrowArrayViewInitFromSchema(&view, t.schema(), &err) != NANOARROW_OK
        || ArrowArrayViewSetArray(&view, t.array(), &err) != NANOARROW_OK) {
        throw std::runtime_error("nanoarrow view failed");
    }
    std::string out;
    for (int64_t j = 0; j < t.schema()->n_children; ++j) {
        const ArrowSchema* cs = t.schema()->children[j];
        const ArrowArrayView* cv = view.children[j];
        std::string f = cs->format;
        if (j > 0) out += "; ";
        out += std::string(cs->name) + ":" + f + ":[";
        for (int64_t r = 0; r < cv->length; ++r) {
            if (r > 0) out += ", ";
            if (ArrowArrayViewIsNull(cv, r)) { out += "null"; continue; }
            char buf[64];
            if (f == "l" || f == "i" || f == "s" || f == "c") {
                snprintf(buf, sizeof buf, "%lld", (long long)ArrowArrayViewGetIntUnsafe(cv, r)); out += buf;
            } else if (f == "g" || f == "f") {
                snprintf(buf, sizeof buf, "%g", ArrowArrayViewGetDoubleUnsafe(cv, r)); out += buf;
            } else if (f == "u" || f == "U") {
                ArrowStringView sv = ArrowArrayViewGetStringUnsafe(cv, r);
                out += "'" + std::string(sv.data, (size_t)sv.size_bytes) + "'";
            } else {
                out += "?";
            }
        }
        out += "]";
    }
    ArrowArrayViewReset(&view);
    return out;
}
#endif
