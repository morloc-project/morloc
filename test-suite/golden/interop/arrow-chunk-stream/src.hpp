#ifndef __SRC_HPP__
#define __SRC_HPP__

#include "mlc_arrow.hpp"
#include <nanoarrow/nanoarrow.h>
#include <string>

// One 4 MiB Int64 column.
static const int64_t CHUNK_ROWS = 524288;

inline mlc::ArrowTable mkChunk(int64_t i) {
    struct ArrowSchema schema;
    struct ArrowArray array;
    ArrowSchemaInit(&schema);
    ArrowSchemaSetTypeStruct(&schema, 1);
    ArrowSchemaSetFormat(schema.children[0], "l");
    ArrowSchemaSetName(schema.children[0], "x");
    ArrowArrayInitFromSchema(&array, &schema, nullptr);
    ArrowArrayStartAppending(&array);
    int64_t base = i * CHUNK_ROWS;
    for (int64_t k = 0; k < CHUNK_ROWS; k++) {
        ArrowArrayAppendInt(array.children[0], base + k);
        ArrowArrayFinishElement(&array);
    }
    ArrowArrayFinishBuildingDefault(&array, nullptr);
    return mlc::ArrowTable(&schema, &array);
}

inline int64_t nRows(const mlc::ArrowTable& t) { return t.n_rows(); }

// The most shared memory this run has needed at once. Volumes only grow,
// so this is a high-water mark. The argument is the run's result: it is
// what makes the measurement happen after the run.
inline int64_t highWaterMiB(int64_t rows) {
    (void)rows;
    return (int64_t)(total_shm_size() / (1024 * 1024));
}

#endif
