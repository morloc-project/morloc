#ifndef SRC_HPP
#define SRC_HPP
#include "mlc_arrow.hpp"

inline mlc::ArrowTable idCpp(const mlc::ArrowTable& t) {
    // Round-trip through SHM to produce an independent owned copy.
    // Stage 1 MVP: validates that the cross-pool Table flow works; future
    // stages will share buffers zero-copy via direct ArrowArray fields.
    char* err = nullptr;
    relptr_t rp = arrow_to_shm(const_cast<ArrowArray*>(t.array()),
                                const_cast<ArrowSchema*>(t.schema()), &err);
    if (err) { std::string s(err); free(err); throw std::runtime_error(s); }
    char* rerr = nullptr;
    void* abs = rel2abs(rp, &rerr);
    if (rerr) { free(rerr); }
    struct ArrowSchema out_schema;
    struct ArrowArray  out_array;
    char* ferr = nullptr;
    arrow_from_shm((const arrow_shm_header_t*)abs, &out_schema, &out_array, &ferr);
    if (ferr) { std::string s(ferr); free(ferr); throw std::runtime_error(s); }
    return mlc::ArrowTable(std::move(out_schema), std::move(out_array));
}
#endif
