#include <stdexcept>
#include <string>
#include <cstdlib>
#include <cstring>
#include <cstdio>
#include <unistd.h>
#include <pthread.h>
#include <sys/mman.h>
#include <atomic>
#include <new>

#include "cppmorloc.hpp"

// Under --debug, the innermost manifold this thread is executing, for the
// crash handler's report. The slot lives in the pool binary, so a handler on
// the faulting thread reads it from the static thread-local block without
// allocating.
static thread_local const char* mlc_frame_ = nullptr;

extern "C" const char* mlc_frame_enter(const char* frame) {
    const char* prev = mlc_frame_;
    mlc_frame_ = frame;
    return prev;
}

extern "C" void mlc_frame_leave(const char* prev) {
    mlc_frame_ = prev;
}

namespace mlc {
// STATE-5. Constant-initialised and trivially destructible, so generated
// code reads it from the static thread-local block with no init check; the
// entries live in a mapping a thread-exit key unmaps.
thread_local constinit frame_stack frames_ = {nullptr, 0, 0};

static pthread_key_t frames_key_;
static pthread_once_t frames_key_once_ = PTHREAD_ONCE_INIT;

static size_t frames_bytes(size_t cap) {
    return cap * sizeof(const char*) + static_cast<size_t>(sysconf(_SC_PAGESIZE));
}

static void frames_free(void* base) {
    if (frames_.base == base) {
        size_t cap = frames_.cap;
        frames_.base = nullptr;
        frames_.cap = 0;
        frames_.depth = 0;
        std::atomic_signal_fence(std::memory_order_seq_cst);
        munmap(base, frames_bytes(cap));
    }
}

static void frames_key_make() {
    if (pthread_key_create(&frames_key_, frames_free) != 0) {
        std::abort();
    }
}

// The bytes of machine stack the calling thread has, or 0 if unknown.
static size_t thread_stack_bytes() {
#if defined(__APPLE__)
    return pthread_get_stacksize_np(pthread_self());
#else
    size_t n = 0;
    pthread_attr_t attr;
    if (pthread_getattr_np(pthread_self(), &attr) == 0) {
        pthread_attr_getstacksize(&attr, &n);
        pthread_attr_destroy(&attr);
    }
    return n;
#endif
}

// Lazily committed address space for 2^28 calls, or eight per 16 bytes of
// the thread's machine stack if that is more: more calls than the machine
// stack holds however many the optimizer inlines into one frame. Where the
// system will not reserve that much, the eight per 16 bytes alone. Its
// untouched pages are no part of a core dump.
static void* frames_map(size_t bytes) {
    size_t page = static_cast<size_t>(sysconf(_SC_PAGESIZE));
    int flags = MAP_PRIVATE | MAP_ANONYMOUS;
#ifdef MAP_NORESERVE
    flags |= MAP_NORESERVE;
#endif
    void* p = mmap(nullptr, bytes + page, PROT_READ | PROT_WRITE, flags, -1, 0);
#ifdef MADV_DONTDUMP
    if (p != MAP_FAILED) madvise(p, bytes + page, MADV_DONTDUMP);
#endif
    return p;
}

static void* frames_reserve(size_t& bytes) {
    size_t page = static_cast<size_t>(sysconf(_SC_PAGESIZE));
    size_t stack = thread_stack_bytes();
    if (stack == 0) stack = size_t(64) << 20;
    size_t needed = ((stack / 16 * 8 + 2) * sizeof(const char*) + page - 1) / page * page;
    bytes = (size_t(1) << 28) * sizeof(const char*);
    if (bytes < needed) bytes = needed;
    void* p = frames_map(bytes);
    if (p != MAP_FAILED || bytes == needed) return p;
    bytes = needed;
    return frames_map(bytes);
}

void frame_init() {
    size_t page = static_cast<size_t>(sysconf(_SC_PAGESIZE));
    size_t bytes = 0;
    void* p = frames_reserve(bytes);
    if (p == MAP_FAILED) throw std::bad_alloc();
    if (mprotect(static_cast<char*>(p) + bytes, page, PROT_NONE) != 0) {
        munmap(p, bytes + page);
        throw std::bad_alloc();
    }
    pthread_once(&frames_key_once_, frames_key_make);
    if (pthread_setspecific(frames_key_, p) != 0) {
        munmap(p, bytes + page);
        throw std::bad_alloc();
    }
    frames_.cap = bytes / sizeof(const char*);
    std::atomic_signal_fence(std::memory_order_seq_cst);
    frames_.base = static_cast<const char**>(p);
}

// STATE-8
thread_local constinit int arm_depth_ = 0;

struct arm_job {
    void (*free_fn)(void*);
    void* p;
};
struct arm_jobs {
    arm_job* items;
    size_t len;
    size_t cap;
};
static thread_local constinit arm_jobs* arm_jobs_ = nullptr;
static pthread_key_t arm_jobs_key_;
static pthread_once_t arm_jobs_key_once_ = PTHREAD_ONCE_INIT;

static void arm_jobs_free(void* jobs) {
    arm_jobs* j = static_cast<arm_jobs*>(jobs);
    if (arm_jobs_ == j) arm_jobs_ = nullptr;
    std::free(j->items);
    std::free(j);
}

static void arm_jobs_key_make() {
    if (pthread_key_create(&arm_jobs_key_, arm_jobs_free) != 0) std::abort();
}

bool arm_defer(void (*free_fn)(void*), void* p) noexcept {
    arm_jobs* j = arm_jobs_;
    if (j == nullptr) {
        j = static_cast<arm_jobs*>(std::calloc(1, sizeof(arm_jobs)));
        if (j == nullptr) return false;
        pthread_once(&arm_jobs_key_once_, arm_jobs_key_make);
        if (pthread_setspecific(arm_jobs_key_, j) != 0) {
            std::free(j);
            return false;
        }
        arm_jobs_ = j;
    }
    if (j->len == j->cap) {
        size_t cap = j->cap ? j->cap * 2 : 256;
        void* items = std::realloc(j->items, cap * sizeof(arm_job));
        if (items == nullptr) return false;
        j->items = static_cast<arm_job*>(items);
        j->cap = cap;
    }
    j->items[j->len++] = arm_job{free_fn, p};
    return true;
}

void arm_drain() noexcept {
    arm_jobs* j = arm_jobs_;
    while (j != nullptr && j->len > 0) {
        arm_job job = j->items[--j->len];
        job.free_fn(job.p);
        j = arm_jobs_;
    }
}

std::string frame_take(size_t saved) {
    frame_stack& s = frames_;
    size_t depth = s.depth;
    s.depth = saved;
    std::string out;
    for (size_t k = depth; k > saved && k < s.cap; k--) out += s.base[k];
    return out;
}
}

// The frame a crash report names: under --debug the marked manifold, else
// the top of the frame stack (STATE-5) without its trace-line prefix.
extern "C" const char* mlc_current_frame(size_t* len) {
    const char* f = mlc_frame_;
    if (f == nullptr) {
        size_t depth = mlc::frames_.depth;
        std::atomic_signal_fence(std::memory_order_seq_cst);
        if (depth > 0 && depth < mlc::frames_.cap) {
            f = mlc::frames_.base[depth];
            static const char prefix[] = "\n  at ";
            if (strncmp(f, prefix, sizeof(prefix) - 1) == 0) f += sizeof(prefix) - 1;
        }
    }
    *len = f ? strlen(f) : 0;
    return f;
}

[[noreturn]] void mlc_runtime_defect(const std::string& msg) {
    morloc_exit_if_shared_memory_lost(msg.c_str());
    std::fprintf(stderr, "morloc internal error (C++ pool): %s\n", msg.c_str());
    std::fflush(stderr);
    // PANIC-5
    _exit(70);
}

[[noreturn]] void mlc_defect_take(char* errmsg) {
    std::string msg(errmsg ? errmsg : "a runtime call failed without giving a reason");
    free(errmsg);
    mlc_runtime_defect(msg);
}

// PANIC-13: both resolve pointers into blocks this pool allocated.
absptr_t rel2abs_cpp(relptr_t ptr){
    char* errmsg = NULL;
    absptr_t absptr = rel2abs(ptr, &errmsg);
    if(errmsg != NULL) mlc_defect_take(errmsg);
    return absptr;
}

relptr_t abs2rel_cpp(absptr_t ptr){
    char* errmsg = NULL;
    relptr_t relptr = abs2rel(ptr, &errmsg);
    if(errmsg != NULL) mlc_defect_take(errmsg);
    return relptr;
}

bool shfree_cpp(absptr_t ptr){
    char* errmsg = NULL;
    bool success = shfree(ptr, &errmsg);
    if(errmsg != NULL) mlc_defect_take(errmsg);
    return success;
}

Schema* parse_schema_cpp(const char* schema_ptr){
    char* errmsg = NULL;
    Schema* schema = parse_schema(schema_ptr, &errmsg);
    if(errmsg != NULL){
        std::string msg(errmsg); free(errmsg);
        throw std::runtime_error(msg);
    }
    return schema;
}

void* shmalloc_cpp(size_t size){
    char* errmsg = NULL;
    void* new_ptr = shmalloc(size, &errmsg);
    if(errmsg != NULL){
        std::string msg(errmsg); free(errmsg);
        throw std::runtime_error(msg);
    }
    return new_ptr;
}

int pack_with_schema_cpp(const void* mlc, const Schema* schema, char** mpk, size_t* mpk_size){
    char* errmsg = NULL;
    int exitcode = pack_with_schema(mlc, schema, mpk, mpk_size, &errmsg);
    if(errmsg != NULL){
        std::string msg(errmsg); free(errmsg);
        throw std::runtime_error(msg);
    }
    return exitcode;
}

int unpack_with_schema_cpp(const char* mgk, size_t mgk_size, const Schema* schema, void** mlcptr){
    char* errmsg = NULL;
    int exitcode = unpack_with_schema(mgk, mgk_size, schema, mlcptr, &errmsg);
    if(errmsg != NULL){
        std::string msg(errmsg); free(errmsg);
        throw std::runtime_error(msg);
    }
    return exitcode;
}
