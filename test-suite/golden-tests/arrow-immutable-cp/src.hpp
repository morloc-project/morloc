#ifndef __SRC_HPP__
#define __SRC_HPP__

#include "mlc_arrow.hpp"
#include <nanoarrow/nanoarrow.h>

mlc::ArrowTable makeLargeTable(int n) {
    struct ArrowSchema schema;
    struct ArrowArray array;

    ArrowSchemaInit(&schema);
    ArrowSchemaSetTypeStruct(&schema, 2);

    ArrowSchemaSetFormat(schema.children[0], "i");
    ArrowSchemaSetName(schema.children[0], "idx");

    ArrowSchemaSetFormat(schema.children[1], "g");
    ArrowSchemaSetName(schema.children[1], "value");

    ArrowArrayInitFromSchema(&array, &schema, nullptr);
    ArrowArrayStartAppending(&array);

    for (int i = 0; i < n; i++) {
        ArrowArrayAppendInt(array.children[0], i);
        ArrowArrayAppendDouble(array.children[1], (double)i * 0.5);
        ArrowArrayFinishElement(&array);
    }

    ArrowArrayFinishBuildingDefault(&array, nullptr);
    return mlc::ArrowTable(std::move(schema), std::move(array));
}

std::vector<int> makeIndices(int n) {
    std::vector<int> v(n);
    for (int i = 0; i < n; i++) v[i] = i;
    return v;
}

double sumReals(const std::vector<double>& xs) {
    double s = 0.0;
    for (double x : xs) s += x;
    return s;
}

#endif
