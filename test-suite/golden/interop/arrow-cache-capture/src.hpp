#ifndef CACHE_CAPTURE_SRC_HPP
#define CACHE_CAPTURE_SRC_HPP

#include "mlc_arrow.hpp"
#include <nanoarrow/nanoarrow.h>
#include <cstdio>
#include <functional>
#include <stdexcept>
#include <string>

// "name:format:[values]" per column, for the flat types this test makes.
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
            if (f == "l") {
                snprintf(buf, sizeof buf, "%lld", (long long)ArrowArrayViewGetIntUnsafe(cv, r)); out += buf;
            } else if (f == "u") {
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

// Two rows, built with nanoarrow.
inline mlc::ArrowTable mkCpp(int64_t n) {
    struct ArrowSchema schema;
    struct ArrowArray array;
    ArrowSchemaInit(&schema);
    ArrowSchemaSetTypeStruct(&schema, 2);
    ArrowSchemaSetFormat(schema.children[0], "l");
    ArrowSchemaSetName(schema.children[0], "x");
    ArrowSchemaSetFormat(schema.children[1], "u");
    ArrowSchemaSetName(schema.children[1], "y");
    ArrowArrayInitFromSchema(&array, &schema, nullptr);
    ArrowArrayStartAppending(&array);
    for (int64_t i = 0; i < n; ++i) {
        ArrowArrayAppendInt(array.children[0], 10 + i);
        ArrowArrayAppendString(array.children[1], ArrowCharView(i % 2 == 0 ? "even" : "odd"));
        ArrowArrayFinishElement(&array);
    }
    ArrowArrayFinishBuildingDefault(&array, nullptr);
    return mlc::ArrowTable(&schema, &array);
}

inline std::string rowAtCpp(const mlc::ArrowTable& t, int64_t k) {
    ArrowArrayView view;
    ArrowError err;
    if (ArrowArrayViewInitFromSchema(&view, t.schema(), &err) != NANOARROW_OK
        || ArrowArrayViewSetArray(&view, t.array(), &err) != NANOARROW_OK) {
        throw std::runtime_error("nanoarrow view failed");
    }
    ArrowStringView sv = ArrowArrayViewGetStringUnsafe(view.children[1], k);
    std::string out = "x=" + std::to_string(ArrowArrayViewGetIntUnsafe(view.children[0], k))
        + " y=" + std::string(sv.data, (size_t)sv.size_bytes);
    ArrowArrayViewReset(&view);
    return out;
}

inline std::string applyIt(std::function<std::string(int64_t)> f, int64_t k) { return f(k); }
inline int64_t applyIntCpp(std::function<int64_t(int64_t)> f, int64_t k) { return f(k); }
inline int64_t tickCpp() { return 0; }

#endif
