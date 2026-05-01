#ifndef __SRC_HPP__
#define __SRC_HPP__

#include "mlc_arrow.hpp"
#include <nanoarrow/nanoarrow.h>

double lookupValue(const mlc::ArrowTable& table, int i) {
    struct ArrowArrayView view;
    ArrowArrayViewInitFromSchema(&view, table.schema(), nullptr);
    ArrowArrayViewSetArray(&view, table.array(), nullptr);

    double val = ArrowArrayViewGetDoubleUnsafe(view.children[1], (int64_t)i);

    ArrowArrayViewReset(&view);
    return val;
}

#endif
