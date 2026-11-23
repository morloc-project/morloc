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
#include <exception>
#include <deque>
#include <mutex>
#include <vector>

#include "morloc_pch.hpp"
#include "cppmorloc.hpp"

// LIFE-6: the pool's libmorloc against the nexus's, when the pool loads;
// compiled-in code records no build id.
__attribute__((constructor)) static void mlc_require_build() {
    morloc_require_build(0);
}

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

bool arm_idle() noexcept {
    return arm_depth_ == 0 && (arm_jobs_ == nullptr || arm_jobs_->len == 0);
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

std::atomic<uint64_t> fork_count{0};
static const int fork_count_hook_ = pthread_atfork(nullptr, nullptr, [] { fork_count.fetch_add(1, std::memory_order_relaxed); });

struct closure_trace {
    std::exception_ptr failure;
    std::string labels;
    pthread_t thread;
    size_t depth;
    // WPOOL-3: kept for a parallel call's caller; no thread's any more.
    bool detached;
};

// WPOOL-3: this thread added or took over a trace since its last job-end hook.
static thread_local constinit bool traces_added_ = false;

// STATE-6, STATE-5
static std::mutex* closure_traces_lock_ = new std::mutex();
static std::deque<closure_trace>* closure_traces_ = new std::deque<closure_trace>();
static const int closure_traces_reset_ = pthread_atfork(nullptr, nullptr, [] {
    closure_traces_lock_ = new std::mutex();
    closure_traces_ = new std::deque<closure_trace>();
});

constexpr size_t closure_traces_cap = 256;

// STATE-6: the same propagation, when the failure left a closure on this
// thread and nothing has been taken from the stack since, or came from
// another thread.
static bool continues(const closure_trace& t, size_t depth) {
    return t.detached || !pthread_equal(t.thread, pthread_self()) || t.depth == depth;
}

void closure_trace_add(size_t saved) noexcept {
    std::exception_ptr failure = std::current_exception();
    std::deque<closure_trace> evicted;
    try {
        size_t depth = frame_depth();
        std::string labels = frame_take(saved);
        if (!failure || labels.empty()) return;
        std::lock_guard<std::mutex> g(*closure_traces_lock_);
        std::deque<closure_trace>& kept = *closure_traces_;
        for (closure_trace& t : kept) {
            if (t.failure == failure) {
                if (continues(t, depth)) t.labels += labels; else t.labels = std::move(labels);
                t.thread = pthread_self();
                t.depth = saved;
                t.detached = false;
                traces_added_ = true;
                return;
            }
        }
        if (kept.size() >= closure_traces_cap) {
            evicted.push_back(std::move(kept.front()));
            kept.pop_front();
        }
        kept.push_back(closure_trace{failure, std::move(labels), pthread_self(), saved, false});
        traces_added_ = true;
    } catch (...) {
        frame_restore(saved);
    }
}

static std::string closure_labels_locked(const std::exception_ptr& failure) {
    std::lock_guard<std::mutex> g(*closure_traces_lock_);
    std::deque<closure_trace>& kept = *closure_traces_;
    for (auto it = kept.begin(); it != kept.end(); ++it) {
        if (it->failure == failure) {
            std::string out = continues(*it, frame_depth()) ? std::move(it->labels) : std::string();
            kept.erase(it);
            return out;
        }
    }
    return std::string();
}

std::string closure_labels() noexcept {
    std::exception_ptr failure = std::current_exception();
    if (!failure) return std::string();
    try {
        return closure_labels_locked(failure);
    } catch (...) {
        return std::string();
    }
}

std::string failure_trace(size_t saved) noexcept {
    std::string out = closure_labels();
    try {
        out += frame_take(saved);
    } catch (...) {
        frame_restore(saved);
    }
    return out;
}

void closure_traces_drop_thread() noexcept {
    std::deque<closure_trace> dropped;
    try {
        std::lock_guard<std::mutex> g(*closure_traces_lock_);
        std::deque<closure_trace>& kept = *closure_traces_;
        for (auto it = kept.begin(); it != kept.end();) {
            if (!it->detached && pthread_equal(it->thread, pthread_self())) {
                dropped.push_back(std::move(*it));
                it = kept.erase(it);
            } else {
                ++it;
            }
        }
        traces_added_ = false;
    } catch (...) {
    }
}

// WPOOL-3: under the trace lock, the trace of `keep` is detached from this
// thread and every other trace of this thread is erased.
static void closure_traces_end_job(const std::exception_ptr& keep) {
    std::deque<closure_trace> dropped;
    std::lock_guard<std::mutex> g(*closure_traces_lock_);
    std::deque<closure_trace>& kept = *closure_traces_;
    for (auto it = kept.begin(); it != kept.end();) {
        if (it->detached || !pthread_equal(it->thread, pthread_self())) {
            ++it;
        } else if (keep && it->failure == keep) {
            it->detached = true;
            ++it;
        } else {
            dropped.push_back(std::move(*it));
            it = kept.erase(it);
        }
    }
    traces_added_ = false;
}

// WPOOL-4: the trace of the failure a parallel call raises again is this
// thread's once more, at the depth the call left.
static void closure_trace_attach(const std::exception_ptr& failure) {
    std::lock_guard<std::mutex> g(*closure_traces_lock_);
    for (closure_trace& t : *closure_traces_) {
        if (t.failure == failure) {
            t.thread = pthread_self();
            t.depth = frame_depth();
            t.detached = false;
            traces_added_ = true;
            return;
        }
    }
}

// WPOOL-3
static std::atomic<const par_tracker_ops*> par_ops_{nullptr};

void par_register(const par_tracker_ops* ops) {
    const par_tracker_ops* none = nullptr;
    if (!par_ops_.compare_exchange_strong(none, ops) && none != ops) {
        mlc_runtime_defect("two C++ pool translation units registered parallel tracker hooks");
    }
}

void (*par_failure_probe)() = nullptr;

struct par_job {
    void (*body)(void*, size_t);
    void* ctx;
    uint64_t generation;
    // WPOOL-1: guards `first` and `handed`; never held across user code.
    std::mutex failure_lock;
    std::exception_ptr first;
    std::vector<absptr_t> handed;
};

// WPOOL-4: each span is a trace boundary.
static int par_span(void* p, size_t i) noexcept {
    par_job* job = static_cast<par_job*>(p);
    size_t saved = frame_depth();
    try {
        saved = frame_enter();
        job->body(job->ctx, i);
        return 0;
    } catch (...) {
        // WPOOL-5: before the job's lock, which a forked child may find held.
        mlc_exit_if_forked_since(job->generation);
        const par_tracker_ops* ops = par_ops_.load(std::memory_order_acquire);
        if (ops != nullptr && ops->pipe_closed()) {
            frame_restore(saved);
        } else {
            closure_trace_add(saved);
        }
        std::lock_guard<std::mutex> g(job->failure_lock);
        if (void (*probe)() = par_failure_probe) probe();
        if (!job->first) job->first = std::current_exception();
        return 1;
    }
}

// WPOOL-3: on a helper, after the spans of a pick, before it leaves the job.
static void par_end_job(par_job* job) {
    const par_tracker_ops* ops = par_ops_.load(std::memory_order_acquire);
    bool entries = ops != nullptr && ops->size() > 0;
    if (entries || traces_added_) {
        std::lock_guard<std::mutex> g(job->failure_lock);
        if (entries) ops->move_to(job->handed);
        if (traces_added_) closure_traces_end_job(job->first);
    }
    if (frame_depth() != 0 || !arm_idle() || !recur_env().empty()
        || (ops != nullptr && ops->reading_runtime_value())) {
        mlc_runtime_defect("a parallel helper ended a job with runtime state left on its thread");
    }
}

static void par_end(void* p) noexcept {
    try {
        par_end_job(static_cast<par_job*>(p));
    } catch (...) {
        // PANIC-13
        mlc_runtime_defect("the job-end hook of a parallel helper failed");
    }
}

bool par_enabled() noexcept {
    return morloc_par_pool_enabled() != 0;
}

void par_run(size_t nspans, size_t workers, void (*body)(void*, size_t), void* ctx) {
    par_job job;
    job.body = body;
    job.ctx = ctx;
    job.generation = morloc_fork_generation();
    int rc = morloc_par_run(nspans, workers, par_span, par_end, &job);
    if (!job.handed.empty()) {
        const par_tracker_ops* ops = par_ops_.load(std::memory_order_acquire);
        if (ops == nullptr) mlc_runtime_defect("a parallel helper handed over tracker entries with no tracker registered");
        ops->add(job.handed);
    }
    if (rc != 0) {
        if (!job.first) mlc_runtime_defect("a parallel call failed without a failure");
        closure_trace_attach(job.first);
        std::rethrow_exception(job.first);
    }
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

// PANIC-6: an exception no handler holds ends the pool, on a thread user
// code started or anywhere else std::terminate is reached.
[[noreturn]] void mlc_on_terminate() {
    const char* what = "an exception with no message";
    std::string labels = mlc::closure_labels();
    if (std::exception_ptr e = std::current_exception()) {
        try {
            std::rethrow_exception(e);
        } catch (const std::exception& x) {
            what = x.what();
        } catch (...) {
        }
    } else {
        what = "std::terminate was called with no exception";
    }
    size_t len = 0;
    const char* frame = mlc_current_frame(&len);
    if (frame != nullptr && len > 0) {
        std::fprintf(stderr, "error with no handler to hold it (C++ pool): %s%s\n  at %.*s\n", what, labels.c_str(), (int)len, frame);
    } else {
        std::fprintf(stderr, "error with no handler to hold it (C++ pool): %s%s\n", what, labels.c_str());
    }
    std::fflush(stderr);
    // PANIC-1
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
