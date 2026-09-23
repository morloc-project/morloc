#ifndef TABLE_CROSSPOOL_SUBEXPRESSION_SRC_HPP
#define TABLE_CROSSPOOL_SUBEXPRESSION_SRC_HPP

#include "mlc_arrow.hpp"
#include <nanoarrow/nanoarrow.h>
#include <vector>

inline std::vector<double> dbl(const mlc::ArrowTable& t) {
    struct ArrowArrayView v;
    ArrowArrayViewInitFromSchema(&v, t.schema(), nullptr);
    ArrowArrayViewSetArray(&v, t.array(), nullptr);
    std::vector<double> out((size_t)t.n_rows());
    for (int64_t i = 0; i < t.n_rows(); ++i) {
        out[(size_t)i] = 2.0 * (double)ArrowArrayViewGetIntUnsafe(v.children[0], i);
    }
    ArrowArrayViewReset(&v);
    return out;
}

#endif
