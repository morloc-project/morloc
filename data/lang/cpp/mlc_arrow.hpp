#ifndef MLC_ARROW_HPP
#define MLC_ARROW_HPP

// mlc_arrow.hpp -- thin RAII wrapper around Arrow C Data Interface structs
// for use in morloc C++ pools.  Holds ArrowSchema + ArrowArray as a single
// move-only value.  The pool template dispatches arrow-hinted schemas to
// arrow_to_shm / arrow_from_shm (in libmorloc.so) via this type.
//
// User code should include <nanoarrow/nanoarrow.h> to build and read columns.

#include "morloc.h"
#include <cstring>
#include <stdexcept>
#include <utility>

namespace mlc {

class ArrowTable {
public:
    // Construct from moved-in C Data Interface structs.
    // Takes ownership of release callbacks.
    ArrowTable(struct ArrowSchema schema, struct ArrowArray array)
        : schema_(schema), array_(array)
    {
        // Zero the source structs so the caller does not double-release
        memset(&schema, 0, sizeof(schema));
        memset(&array, 0, sizeof(array));
    }

    ~ArrowTable() {
        if (array_.release) array_.release(&array_);
        if (schema_.release) schema_.release(&schema_);
    }

    // Move-only
    ArrowTable(ArrowTable&& other) noexcept
        : schema_(other.schema_), array_(other.array_)
    {
        memset(&other.schema_, 0, sizeof(other.schema_));
        memset(&other.array_, 0, sizeof(other.array_));
    }

    ArrowTable& operator=(ArrowTable&& other) noexcept {
        if (this != &other) {
            if (array_.release) array_.release(&array_);
            if (schema_.release) schema_.release(&schema_);
            schema_ = other.schema_;
            array_ = other.array_;
            memset(&other.schema_, 0, sizeof(other.schema_));
            memset(&other.array_, 0, sizeof(other.array_));
        }
        return *this;
    }

    ArrowTable(const ArrowTable&) = delete;
    ArrowTable& operator=(const ArrowTable&) = delete;

    // Accessors (const -- arrow data is immutable)
    const struct ArrowSchema* schema() const { return &schema_; }
    const struct ArrowArray*  array()  const { return &array_; }
    int64_t n_columns() const { return schema_.n_children; }
    int64_t n_rows()    const { return array_.length; }

    // Build from shared memory header (zero-copy import)
    static ArrowTable from_shm(const arrow_shm_header_t* hdr) {
        struct ArrowSchema as;
        struct ArrowArray aa;
        char* err = nullptr;
        arrow_from_shm(hdr, &as, &aa, &err);
        if (err) {
            std::string msg(err);
            free(err);
            throw std::runtime_error(msg);
        }
        return ArrowTable(std::move(as), std::move(aa));
    }

    // Move table data to shared memory: copies buffers into a contiguous SHM
    // block, frees the original heap buffers, then repoints this table's
    // internal ArrowSchema/ArrowArray into the SHM block (zero-copy).
    // After this call the table is still usable but backed by SHM.
    // Returns relptr to the SHM block for use in packets.
    relptr_t move_to_shm() {
        // Step 1: copy all column data into contiguous SHM
        char* copy_err = nullptr;
        relptr_t rp = arrow_to_shm(&array_, &schema_, &copy_err);
        if (copy_err) {
            std::string msg(copy_err);
            free(copy_err);
            throw std::runtime_error(msg);
        }

        // Step 2: release heap-backed structs (frees all original buffers)
        if (array_.release) array_.release(&array_);
        if (schema_.release) schema_.release(&schema_);
        memset(&schema_, 0, sizeof(schema_));
        memset(&array_, 0, sizeof(array_));

        // Step 3: resolve SHM pointer and rebuild structs pointing into it
        char* resolve_err = nullptr;
        void* abs = rel2abs(rp, &resolve_err);
        if (resolve_err) {
            std::string msg(resolve_err);
            free(resolve_err);
            throw std::runtime_error(msg);
        }

        char* shm_err = nullptr;
        arrow_from_shm((const arrow_shm_header_t*)abs, &schema_, &array_, &shm_err);
        if (shm_err) {
            std::string msg(shm_err);
            free(shm_err);
            throw std::runtime_error(msg);
        }

        return rp;
    }

private:
    struct ArrowSchema schema_;
    struct ArrowArray array_;
};

} // namespace mlc

#endif // MLC_ARROW_HPP
