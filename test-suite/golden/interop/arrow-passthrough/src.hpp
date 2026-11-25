#ifndef PASSTHROUGH_SRC_HPP
#define PASSTHROUGH_SRC_HPP

// Three ways a C++ pool can hand a received table back: unchanged, with a
// column renamed over the same buffers, and as a slice of the same buffers.
// Only the first may be passed through without a copy; the other two must
// arrive changed.

#include "mlc_arrow.hpp"
#include <nanoarrow/nanoarrow.h>
#include <stdexcept>
#include <string>

inline mlc::ArrowTable idCpp(const mlc::ArrowTable& t) {
    return t;
}

inline mlc::ArrowTable renameFirst(const mlc::ArrowTable& t) {
    struct ArrowSchema s;
    struct ArrowArray a;
    t.lend(&s, &a);
    struct ArrowSchema copy;
    if (ArrowSchemaDeepCopy(&s, &copy) != NANOARROW_OK) throw std::runtime_error("schema copy failed");
    if (ArrowSchemaSetName(copy.children[0], "renamed") != NANOARROW_OK) throw std::runtime_error("rename failed");
    return t.derive(copy, a);
}

inline mlc::ArrowTable dropFirstRow(const mlc::ArrowTable& t) {
    struct ArrowSchema s;
    struct ArrowArray a;
    t.lend(&s, &a);
    a.offset += 1;
    a.length -= 1;
    return t.derive(s, a);
}
#endif
