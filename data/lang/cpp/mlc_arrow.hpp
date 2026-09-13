#ifndef MLC_ARROW_HPP
#define MLC_ARROW_HPP

// mlc_arrow.hpp -- shared-ownership handle on an Arrow C Data Interface pair
// (ArrowSchema + ArrowArray) for use in morloc C++ pools. Copying a table
// shares it; the structs are released when the last copy goes away. A table
// derived from another (a renamed or sliced view over the same buffers)
// keeps its source alive for as long as it lives, so a view never outlives
// what it points into. The pool template moves tables between SHM and this
// type through arrow_to_shm / arrow_from_shm in libmorloc.so.
//
// User code reads and builds columns with <nanoarrow/nanoarrow.h>.

#include "morloc.h"
#include <cstring>
#include <memory>
#include <stdexcept>
#include <string>
#include <utility>

namespace mlc {

class ArrowTable {
public:
    // Take ownership of C Data Interface structs (and their release
    // callbacks). The sources are zeroed so the caller cannot release
    // them a second time.
    ArrowTable(struct ArrowSchema schema, struct ArrowArray array)
        : impl_(std::make_shared<Impl>(schema, array, nullptr))
    {
        memset(&schema, 0, sizeof(schema));
        memset(&array, 0, sizeof(array));
    }

    ArrowTable(const ArrowTable&) = default;
    ArrowTable(ArrowTable&&) noexcept = default;
    ArrowTable& operator=(const ArrowTable&) = default;
    ArrowTable& operator=(ArrowTable&&) noexcept = default;

    // Arrow data is immutable; views are read-only.
    const struct ArrowSchema* schema() const { return &impl_->schema; }
    const struct ArrowArray*  array()  const { return &impl_->array; }
    int64_t n_columns() const { return impl_->schema.n_children; }
    int64_t n_rows()    const { return impl_->array.length; }

    // Zero-copy view over a table block in SHM. The block must outlive
    // the returned table.
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
        return ArrowTable(as, aa);
    }

    // Copies of this table's structs whose release does nothing, for
    // handing to a consumer that expects to own its input. Valid only
    // while this table (or a table derived from it) is alive.
    void lend(struct ArrowSchema* schema, struct ArrowArray* array) const {
        *schema = impl_->schema;
        *array = impl_->array;
        schema->release = lent_schema_release;
        array->release = lent_array_release;
    }

    // A table over structs that alias this table's buffers -- typically
    // built from lend() copies with a name or a slice changed. The result
    // owns the given structs and keeps this table alive as long as it
    // lives, so the aliased memory stays valid.
    ArrowTable derive(struct ArrowSchema schema, struct ArrowArray array) const {
        return ArrowTable(std::make_shared<Impl>(schema, array, impl_));
    }

    // Move this table's data into a fresh SHM block (or, when it is a
    // table this pool received and returns unchanged, pass that block
    // through), bringing it into agreement with the declared morloc
    // column schema when one is given, and repoint this table at the
    // block. Returns the block's relptr for use in packets; the block is
    // the caller's to track and release.
    relptr_t move_to_shm(const Schema* declared = nullptr) {
        // The structs are lent, never consumed: other copies of this
        // table may still be using them, and the consumer's release is
        // then a no-op on the copies while `impl_` keeps them alive.
        struct ArrowSchema s;
        struct ArrowArray a;
        lend(&s, &a);
        char* err = nullptr;
        relptr_t rp = arrow_to_shm_typed(&a, &s, declared, &err);
        if (err) {
            std::string msg(err);
            free(err);
            throw std::runtime_error(msg);
        }
        *this = from_shm(resolve(rp));
        return rp;
    }

private:
    struct Impl {
        struct ArrowSchema schema;
        struct ArrowArray array;
        std::shared_ptr<Impl> parent;

        Impl(struct ArrowSchema s, struct ArrowArray a, std::shared_ptr<Impl> p)
            : schema(s), array(a), parent(std::move(p)) {}
        ~Impl() {
            if (array.release) array.release(&array);
            if (schema.release) schema.release(&schema);
        }
        Impl(const Impl&) = delete;
        Impl& operator=(const Impl&) = delete;
    };

    explicit ArrowTable(std::shared_ptr<Impl> impl) : impl_(std::move(impl)) {}

    static const arrow_shm_header_t* resolve(relptr_t rp) {
        char* err = nullptr;
        void* abs = rel2abs(rp, &err);
        if (err) {
            std::string msg(err);
            free(err);
            throw std::runtime_error(msg);
        }
        return (const arrow_shm_header_t*)abs;
    }

    static void lent_schema_release(struct ArrowSchema* s) { s->release = nullptr; }
    static void lent_array_release(struct ArrowArray* a) { a->release = nullptr; }

    std::shared_ptr<Impl> impl_;
};

} // namespace mlc

#endif // MLC_ARROW_HPP
