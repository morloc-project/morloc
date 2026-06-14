#define PY_SSIZE_T_CLEAN
#include "morloc.h"
#include "Python.h"
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>

// boilerplate for numpy support
#define PY_ARRAY_UNIQUE_SYMBOL MORLOC_ARRAY_API
#define NPY_NO_DEPRECATED_API NPY_1_7_API_VERSION
#include <numpy/arrayobject.h>

// SHM tracker for _put_value allocations (deferred cleanup)
#define SHM_TRACKER_INIT_CAP 16
typedef struct {
    absptr_t ptr;
    Schema* schema;
} shm_entry_t;
static shm_entry_t* shm_tracker = NULL;
static size_t shm_tracker_count = 0;
static size_t shm_tracker_cap = 0;

static void shm_tracker_push(absptr_t ptr, Schema* schema) {
    if (shm_tracker_count >= shm_tracker_cap) {
        size_t new_cap = shm_tracker_cap ? shm_tracker_cap * 2 : SHM_TRACKER_INIT_CAP;
        shm_entry_t* new_buf = (shm_entry_t*)realloc(shm_tracker, new_cap * sizeof(shm_entry_t));
        if (!new_buf) return;
        shm_tracker = new_buf;
        shm_tracker_cap = new_cap;
    }
    shm_tracker[shm_tracker_count].ptr = ptr;
    shm_tracker[shm_tracker_count].schema = schema;
    shm_tracker_count++;
}

static void flush_shm_tracker(void) {
    for (size_t i = 0; i < shm_tracker_count; i++) {
        char* err = NULL;
        // shm::shfree decrements the refcount and zeros the block on final
        // ref-drop, so no separate metadata-zeroing pass is needed here.
        shfree(shm_tracker[i].ptr, &err);
        if (err) { free(err); }
        if (shm_tracker[i].schema) {
            free_schema(shm_tracker[i].schema);
        }
    }
    shm_tracker_count = 0;
}

// Drop one tracker entry matching ptr (swap-with-last), shfree the
// block, and free its schema. Used by release_packet_shm to free
// a put_value-tracked packet's SHM as soon as its codegen-determined
// scope ends, rather than waiting for the next dispatch flush.
static bool shm_tracker_release_one(absptr_t ptr) {
    for (size_t i = 0; i < shm_tracker_count; i++) {
        if (shm_tracker[i].ptr == ptr) {
            Schema* schema = shm_tracker[i].schema;
            shm_tracker[i] = shm_tracker[shm_tracker_count - 1];
            shm_tracker_count--;
            char* err = NULL;
            shfree(ptr, &err);
            if (err) { free(err); }
            if (schema) { free_schema(schema); }
            return true;
        }
    }
    return false;
}

#define NOTHING

#define MAYFAIL \
    char* child_errmsg_ = NULL; \

// Returns a strdup'd string that the caller must free, or NULL.
char* get_prior_err(){
    char* prior_err = NULL;
    if (PyErr_Occurred()) {
        // Fetch existing exception
        PyObject *type, *value, *traceback;
        PyErr_Fetch(&type, &value, &traceback);

        // Extract error message
        PyObject* str = PyObject_Str(value);  // Convert exception to string
        if (str) {
            const char* raw = PyUnicode_AsUTF8(str);
            if (raw) {
                prior_err = strdup(raw);
            }
            Py_DECREF(str);
        }
        Py_XDECREF(type);
        Py_XDECREF(value);
        Py_XDECREF(traceback);
    }
    return prior_err;
}


#define PyTRY(fun, ...) \
    fun(__VA_ARGS__ __VA_OPT__(,) &child_errmsg_); \
    if(child_errmsg_ != NULL){ \
        char* prior_err = get_prior_err(); \
        if(prior_err == NULL){ \
            PyErr_Format(PyExc_RuntimeError, "Error (%s:%d in %s):\n%s", __FILE__, __LINE__, __func__, child_errmsg_); \
        } else { \
            PyErr_Format(PyExc_RuntimeError, "%s\nError (%s:%d in %s):\n%s", prior_err, __FILE__, __LINE__, __func__, child_errmsg_); \
            free(prior_err); \
        } \
        goto error; \
    }

#define PyRAISE(msg, ...) { \
    char* prior_err_ = get_prior_err(); \
    if(prior_err_ == NULL){ \
        PyErr_Format(PyExc_RuntimeError, "Error (%s:%d in %s):\n" msg "\n", __FILE__, __LINE__, __func__, ##__VA_ARGS__); \
    } else { \
        PyErr_Format(PyExc_RuntimeError, "%s\nError (%s:%d in %s):\n" msg "\n", prior_err_, __FILE__, __LINE__, __func__, ##__VA_ARGS__); \
        free(prior_err_); \
    } \
    goto error; \
    }

#define PyTRACE(cond) \
    if(cond){ \
        char* prior_err = get_prior_err(); \
        if(prior_err != NULL){ \
            PyErr_Format(PyExc_TypeError, "Error (%s:%d in %s):\n%s", __FILE__, __LINE__, __func__, prior_err); \
            free(prior_err); \
            goto error; \
        } \
    }

PyObject* numpy_module = NULL;


// This function will be called to import numpy if, and only if, a numpy feature
// is used. This avoids the agonizingly long numpy import time.
void* import_numpy() {
    numpy_module = PyImport_ImportModule("numpy");
    if(numpy_module == NULL){
        PyRAISE("NumPy is not available");
    }

    import_array();

error:
    return NULL;
}



// ── Recursive-record env (named-schema stack) ─────────────────────────────
//
// Schemas built from wire forms like `&4Treem25valuej8childrena^4Tree`
// carry a `name` on the outer (`Tree`) declaration and on every Recur
// back-reference. While walking the schema we maintain a stack of
// (name, schema) entries: push on entering a declaration, pop on exit.
// A Recur node looks up its target by scanning the stack top-down.
//
// __thread keeps the stack per-thread so concurrent walks (e.g. the
// daemon dispatcher and a foreign-call serializer running on another
// thread) do not stomp on each other's environments.
typedef struct {
    const char* name;
    const Schema* schema;
} recur_env_entry_t;

#define RECUR_ENV_MAX 64
static __thread recur_env_entry_t recur_env_stack[RECUR_ENV_MAX];
static __thread int recur_env_depth = 0;

static int recur_env_push(const Schema* schema) {
    // Only schemas with a non-null name and not Recur-typed are
    // declarations. Recur nodes share the `name` field as a lookup key,
    // not a binding site.
    if (schema == NULL || schema->name == NULL || schema->type == MORLOC_RECUR) {
        return 0;
    }
    if (recur_env_depth >= RECUR_ENV_MAX) {
        // Stack overflow indicates pathological nesting; bail rather
        // than silently dropping a declaration. The caller will see a
        // failed lookup downstream.
        return 0;
    }
    recur_env_stack[recur_env_depth].name = schema->name;
    recur_env_stack[recur_env_depth].schema = schema;
    recur_env_depth++;
    return 1;
}

static void recur_env_pop(int pushed) {
    if (pushed && recur_env_depth > 0) recur_env_depth--;
}

static const Schema* recur_env_lookup(const char* name) {
    if (name == NULL) return NULL;
    for (int i = recur_env_depth - 1; i >= 0; i--) {
        const recur_env_entry_t* e = &recur_env_stack[i];
        if (e->name != NULL && strcmp(e->name, name) == 0) {
            return e->schema;
        }
    }
    return NULL;
}

// Map morloc schema element type to numpy type number
static int schema_to_npy_type(morloc_serial_type type) {
    switch (type) {
        case MORLOC_BOOL:    return NPY_BOOL;
        case MORLOC_SINT8:   return NPY_INT8;
        case MORLOC_SINT16:  return NPY_INT16;
        case MORLOC_SINT32:  return NPY_INT32;
        case MORLOC_SINT64:  return NPY_INT64;
        case MORLOC_UINT8:   return NPY_UINT8;
        case MORLOC_UINT16:  return NPY_UINT16;
        case MORLOC_UINT32:  return NPY_UINT32;
        case MORLOC_UINT64:  return NPY_UINT64;
        case MORLOC_FLOAT32: return NPY_FLOAT32;
        case MORLOC_FLOAT64: return NPY_FLOAT64;
        default:             return -1;
    }
}

PyObject* fromAnything(const Schema* schema, const void* data, const void* base_ptr){ MAYFAIL

    PyObject* obj = NULL;
    // Push the schema's name (if it is a `&<name>X` declaration) onto
    // the recursive-env stack before descending. Pop on the way out via
    // the goto-error path or normal return. Recur lookups inside the
    // body resolve against this stack.
    int _recur_pushed = recur_env_push(schema);
    switch (schema->type) {
        case MORLOC_NIL:
            recur_env_pop(_recur_pushed);
            Py_RETURN_NONE;
        case MORLOC_BOOL:
            obj = PyBool_FromLong(*(bool*)data);
            break;
        case MORLOC_SINT8:
            obj = PyLong_FromLong(*(int8_t*)data);
            break;
        case MORLOC_SINT16:
            obj = PyLong_FromLong(*(int16_t*)data);
            break;
        case MORLOC_SINT32:
            obj = PyLong_FromLong(*(int32_t*)data);
            break;
        case MORLOC_SINT64:
            obj = PyLong_FromLongLong(*(int64_t*)data);
            break;
        case MORLOC_UINT8:
            obj = PyLong_FromUnsignedLong(*(uint8_t*)data);
            break;
        case MORLOC_UINT16:
            obj = PyLong_FromUnsignedLong(*(uint16_t*)data);
            break;
        case MORLOC_UINT32:
            obj = PyLong_FromUnsignedLong(*(uint32_t*)data);
            break;
        case MORLOC_UINT64:
            obj = PyLong_FromUnsignedLongLong(*(uint64_t*)data);
            break;
        case MORLOC_FLOAT32:
            obj = PyFloat_FromDouble(*(float*)data);
            break;
        case MORLOC_FLOAT64:
            obj = PyFloat_FromDouble(*(double*)data);
            break;
        case MORLOC_INT: {
            // Inline BigInt: [size:i64, value_or_relptr:i64]
            int64_t* fields = (int64_t*)data;
            int64_t bigint_size = fields[0];
            if (bigint_size <= 1) {
                // Inline: second field is the value directly
                int64_t val = (bigint_size == 0) ? 0 : fields[1];
                obj = PyLong_FromLongLong(val);
            } else {
                // Overflow: second field is relptr to limb array
                void* limb_ptr = resolve_relptr(*(relptr_t*)&fields[1], base_ptr, NULL);
                obj = _PyLong_FromByteArray(
                    (const unsigned char*)limb_ptr,
                    bigint_size * sizeof(uint64_t),
                    1, 1  // little-endian, signed
                );
            }
            break;
        }
        case MORLOC_STRING: {
            Array* str_array = (Array*)data;
            void* tmp_ptr = NULL;

            if (str_array->size != 0) {
                tmp_ptr = PyTRY(resolve_relptr, str_array->data, base_ptr);
            }

            if (schema->hint != NULL && strcmp(schema->hint, "bytes") == 0) {
                // load binary data as a python bytes object
                if (str_array->size == 0) {
                    obj = PyBytes_FromStringAndSize("", 0);  // empty bytes object
                } else {
                    obj = PyBytes_FromStringAndSize(tmp_ptr, str_array->size);
                }
                if (!obj) {
                    PyRAISE("Failed to parse data as bytes");
                }
            } else if (schema->hint != NULL && strcmp(schema->hint, "bytearray") == 0) {
                // load binary data as a python bytearray object
                if (str_array->size == 0) {
                    obj = PyByteArray_FromStringAndSize("", 0);  // empty bytearray object
                } else {
                    obj = PyByteArray_FromStringAndSize(tmp_ptr, str_array->size);
                }
                if (!obj) {
                    PyRAISE("Failed to parse data as bytearray");
                }
            } else {
                // otherwise, load this as a str type
                if (str_array->size == 0) {
                    obj = PyUnicode_New(0, 127);  // empty string object
                } else {
                    obj = PyUnicode_FromStringAndSize(tmp_ptr, str_array->size);
                }
                if (!obj) {
                    PyRAISE("Failed to parse data as string");
                }
            }
            break;
        }
        case MORLOC_ARRAY: {
            Array* array = (Array*)data;
            // The "numpy.ndarray" hint is authoritative: the user wants a
            // NumPy array, regardless of element type. For fixed-width
            // primitive elements we use the natural dtype (zero-copy / fast
            // memcpy). For everything else (MORLOC_INT BigInt, MORLOC_STRING,
            // nested tuples/arrays/records, MORLOC_NIL, MORLOC_OPTIONAL) we
            // build an `np.empty(n, dtype=object)` and fill via recursive
            // fromAnything per element. This preserves Functor's container
            // invariance: `map asString ([1,2,3] :: Vector 3 Int)` stays a
            // NumPy ndarray on both sides, the dtype just shifts from int64
            // to object.
            int numpy_type_num = -1;
            bool numpy_hint = (schema->hint != NULL && strcmp(schema->hint, "numpy.ndarray") == 0);
            if (numpy_hint) {
                Schema* element_schema = schema->parameters[0];
                switch (element_schema->type) {
                    case MORLOC_BOOL:    numpy_type_num = NPY_BOOL; break;
                    case MORLOC_SINT8:   numpy_type_num = NPY_INT8; break;
                    case MORLOC_SINT16:  numpy_type_num = NPY_INT16; break;
                    case MORLOC_SINT32:  numpy_type_num = NPY_INT32; break;
                    case MORLOC_SINT64:  numpy_type_num = NPY_INT64; break;
                    case MORLOC_UINT8:   numpy_type_num = NPY_UINT8; break;
                    case MORLOC_UINT16:  numpy_type_num = NPY_UINT16; break;
                    case MORLOC_UINT32:  numpy_type_num = NPY_UINT32; break;
                    case MORLOC_UINT64:  numpy_type_num = NPY_UINT64; break;
                    case MORLOC_FLOAT32: numpy_type_num = NPY_FLOAT32; break;
                    case MORLOC_FLOAT64: numpy_type_num = NPY_FLOAT64; break;
                    default: numpy_type_num = NPY_OBJECT; break;  // boxed dtype=object path below
                }
            }
            if (numpy_type_num == NPY_OBJECT) {
                // Boxed path: build dtype=object array and fill via
                // recursive fromAnything. Each slot stores a PyObject*
                // pointer; SETITEM handles INCREF/DECREF correctly.
                import_numpy();
                npy_intp dims[] = {array->size};
                obj = PyArray_SimpleNew(1, dims, NPY_OBJECT);
                if (obj == NULL) {
                    PyRAISE("Failed to allocate numpy object array");
                }
                if (array->size > 0) {
                    char* start = (char*) PyTRY(resolve_relptr, array->data, base_ptr);
                    size_t width = schema->parameters[0]->width;
                    Schema* element_schema = schema->parameters[0];
                    PyArrayObject* arr = (PyArrayObject*)obj;
                    for (size_t i = 0; i < array->size; i++) {
                        PyObject* item = fromAnything(element_schema, start + width * i, base_ptr);
                        if (!item) {
                            PyRAISE("Failed to convert element for numpy object array");
                        }
                        // PyArray_SETITEM copies the PyObject* into the slot
                        // and bumps its refcount on the way in; we drop our
                        // local reference afterwards.
                        if (PyArray_SETITEM(arr, PyArray_GETPTR1(arr, i), item) < 0) {
                            Py_DECREF(item);
                            PyRAISE("Failed to set element in numpy object array");
                        }
                        Py_DECREF(item);
                    }
                }
            } else if (numpy_type_num >= 0) {
                import_numpy();
                npy_intp dims[] = {array->size};
                void* absptr = PyTRY(resolve_relptr, array->data, base_ptr);
                if (base_ptr != NULL) {
                    // Inline packet: `absptr` points into a libc-malloc'd
                    // packet buffer that is freed shortly after get_value
                    // returns. We must own the data, not view it -- otherwise
                    // any later read (e.g. inside a Python comparator like
                    // numpy.allclose) would see stale or recycled memory.
                    obj = PyArray_SimpleNew(1, dims, numpy_type_num);
                    if (obj == NULL) {
                        PyRAISE("Failed to allocate numpy array");
                    }
                    if (array->size > 0) {
                        size_t nbytes = (size_t)array->size *
                                        (size_t)PyArray_ITEMSIZE((PyArrayObject*)obj);
                        memcpy(PyArray_DATA((PyArrayObject*)obj), absptr, nbytes);
                    }
                } else {
                    // SHM-backed packet: the caller has bumped the SHM refcount
                    // and registered a deferred decref in shm_tracker. The
                    // backing memory outlives this numpy array, so a view is
                    // safe and avoids a copy of potentially-large arrays.
                    obj = PyArray_SimpleNewFromData(1, dims, numpy_type_num, absptr);
                    if(obj == NULL) {
                        PyRAISE("Failed to parse data");
                    }
                }
                // Note that we do not want to give ownership to Python.
                // This is shared memory, which means python should not mutate it.
            } else if (schema->hint != NULL && strcmp(schema->hint, "list") == 0) {
                // Explicit "list" hint takes precedence over the UInt8 fast-path
                // below: the user declared `type Py => (List a) = "list" a`
                // (or similar), so honour that representation even for UInt8
                // elements. Without this, a [UInt8] packet would silently
                // arrive as `bytes` and mismatch a Python-pool list literal
                // of the same morloc type.
                obj = PyList_New(array->size);
                if(obj == NULL){
                    PyRAISE("Failed to allocate list");
                }
                if(array->size > 0){
                    char* start = (char*) PyTRY(resolve_relptr, array->data, base_ptr);
                    size_t width = schema->parameters[0]->width;
                    Schema* element_schema = schema->parameters[0];
                    for (size_t i = 0; i < array->size; i++) {
                        PyObject* item = fromAnything(element_schema, start + width * i, base_ptr);
                        if (!item || PyList_SetItem(obj, i, item) < 0) {
                            PyRAISE("Failed to access element in list")
                        }
                    }
                }
            } else if (schema->hint != NULL && strcmp(schema->hint, "bytearray") == 0) {
                // Create a Python bytearray object
                void* absptr = PyTRY(resolve_relptr, array->data, base_ptr);
                obj = PyByteArray_FromStringAndSize((const char*)absptr, array->size);
                if (!obj) {
                    PyErr_SetString(PyExc_TypeError, "Failed to create bytearray");
                    goto error;
                }
                // Note: Similar to the numpy case, we don't want to give ownership to Python.
                // The bytearray is created from a copy of the data, so no additional handling is needed.
            } else if (schema->parameters[0]->type == MORLOC_UINT8) {
                // Default for UInt8 arrays when hint is "bytes" or absent.
                void* tmp_ptr = PyTRY(resolve_relptr, array->data, base_ptr);
                obj = PyBytes_FromStringAndSize((const char*)tmp_ptr, array->size);
                if (obj == NULL) {
                    PyRAISE("Failed to one bytes")
                }
            } else if (schema->hint == NULL) {
                // No hint, non-UInt8: default to a Python list
                obj = PyList_New(array->size);
                if(obj == NULL){
                    PyRAISE("Failed to allocate list");
                }
                if(array->size > 0){
                    char* start = (char*) PyTRY(resolve_relptr, array->data, base_ptr);
                    size_t width = schema->parameters[0]->width;
                    Schema* element_schema = schema->parameters[0];
                    for (size_t i = 0; i < array->size; i++) {
                        PyObject* item = fromAnything(element_schema, start + width * i, base_ptr);
                        if (!item || PyList_SetItem(obj, i, item) < 0) {
                            PyRAISE("Failed to access element in list");
                        }
                    }
                }
            } else {
                PyRAISE("Unexpected array hint");
            }
            break;
        }
        case MORLOC_TUPLE: {
            obj = PyTuple_New(schema->size);
            if(obj == NULL){
                PyRAISE("Failed in tuple");
            }
            for (size_t i = 0; i < schema->size; i++) {
                void* item_ptr = (char*)data + schema->offsets[i];
                PyObject* item = fromAnything(schema->parameters[i], item_ptr, base_ptr);
                if (!item || PyTuple_SetItem(obj, i, item) < 0) {
                    PyRAISE("Failed to access tuple element");
                }
            }
            break;
        }
        case MORLOC_MAP: {
            obj = PyDict_New();
            if(obj == NULL){
                PyRAISE("Failed in map");
            }
            for (size_t i = 0; i < schema->size; i++) {
                void* item_ptr = (char*)data + schema->offsets[i];
                PyObject* value = fromAnything(schema->parameters[i], item_ptr, base_ptr);
                PyObject* key = PyUnicode_FromString(schema->keys[i]);
                if (!value || !key || PyDict_SetItem(obj, key, value) < 0) {
                    Py_XDECREF(value);
                    Py_XDECREF(key);
                    PyRAISE("Failed to access map element");
                }
                Py_DECREF(key);
                Py_DECREF(value);
            }
            break;
        }
        case MORLOC_OPTIONAL: {
            // The Optional slot is a relptr (RELNULL = absent). Resolve and
            // recurse into the inner T's body when present. base_ptr is
            // set when the data lives inline in a packet payload (relptrs
            // are payload-relative); otherwise we go through SHM.
            relptr_t relptr = *(const relptr_t*)data;
            if (relptr == RELNULL) {
                recur_env_pop(_recur_pushed);
                Py_RETURN_NONE;
            }
            const void* inner_abs;
            if (base_ptr) {
                inner_abs = (const char*)base_ptr + relptr;
            } else {
                char* errmsg_resolve = NULL;
                inner_abs = rel2abs(relptr, &errmsg_resolve);
                if (errmsg_resolve) {
                    PyErr_SetString(PyExc_RuntimeError, errmsg_resolve);
                    free(errmsg_resolve);
                    goto error;
                }
            }
            obj = fromAnything(schema->parameters[0], inner_abs, base_ptr);
            if (!obj) {
                PyRAISE("Failed to deserialize optional inner value");
            }
            break;
        }
        case MORLOC_RECUR: {
            // Back-reference: resolve to the named declaration on the
            // env stack and deserialise as if we were already inside
            // that schema. The data shape at this slot is whatever the
            // declaration body specifies; the env carries the matching
            // Schema pointer so the recursive call can navigate.
            const Schema* target = recur_env_lookup(schema->name);
            if (target == NULL) {
                PyRAISE("Recur back-reference to undeclared schema name '%s'",
                        schema->name ? schema->name : "?");
            }
            obj = fromAnything(target, data, base_ptr);
            if (!obj) {
                PyRAISE("Failed to deserialize recursive value");
            }
            break;
        }
        default:
            PyRAISE("Unsupported schema type %d in fromAnything", (int)schema->type);
    }

    recur_env_pop(_recur_pushed);
    return obj;

error:
    recur_env_pop(_recur_pushed);
    Py_XDECREF(obj);
    return NULL;
}


#define HANDLE_SINT_TYPE(CTYPE, PYLONG_FUNC, MIN, MAX) \
    do { \
        if (!PyLong_Check(obj)) { \
            PyErr_Format(PyExc_TypeError, "Expected int for %s, but got %s", #CTYPE, Py_TYPE(obj)->tp_name); \
            goto error; \
        } \
        long long value = PYLONG_FUNC(obj); \
        if (value < MIN || value > MAX || PyErr_Occurred()) { \
            PyErr_Format(PyExc_OverflowError, "Integer overflow for %s", #CTYPE); \
            goto error; \
        } \
        *(CTYPE*)dest = (CTYPE)value; \
    } while(0)

#define HANDLE_UINT_TYPE(CTYPE, PYLONG_FUNC, MAX) \
    do { \
        if (!PyLong_Check(obj)) { \
            PyErr_Format(PyExc_TypeError, "Expected int for %s, but got %s", #CTYPE, Py_TYPE(obj)->tp_name); \
            goto error; \
        } \
        unsigned long long value = PYLONG_FUNC(obj); \
        if (value > MAX || PyErr_Occurred()) { \
            PyErr_Format(PyExc_OverflowError, "Integer overflow for %s", #CTYPE); \
            goto error; \
        } \
        *(CTYPE*)dest = (CTYPE)value; \
    } while(0)



static ssize_t get_shm_size_inner(const Schema* schema, PyObject* obj);

// Wrap get_shm_size_inner so the recursive-env stack is maintained at
// every entry. Inner code calls get_shm_size (this wrapper), which
// pushes the schema's declaration name (if any) before delegating to
// _inner. Recur nodes themselves don't push (recur_env_push skips them)
// so a back-ref resolution does not pollute the stack.
ssize_t get_shm_size(const Schema* schema, PyObject* obj) {
    int pushed = recur_env_push(schema);
    ssize_t r = get_shm_size_inner(schema, obj);
    recur_env_pop(pushed);
    return r;
}

static ssize_t get_shm_size_inner(const Schema* schema, PyObject* obj) {
    switch (schema->type) {
        case MORLOC_NIL:
        case MORLOC_BOOL:
        case MORLOC_SINT8:
        case MORLOC_SINT16:
        case MORLOC_SINT32:
        case MORLOC_SINT64:
        case MORLOC_UINT8:
        case MORLOC_UINT16:
        case MORLOC_UINT32:
        case MORLOC_UINT64:
        case MORLOC_FLOAT32:
        case MORLOC_FLOAT64:
            return schema->width;
        case MORLOC_INT: {
            // Inline BigInt: 16 bytes for common case, more for overflow
            if (!PyLong_Check(obj)) {
                PyRAISE("Expected int for MORLOC_INT, but got %s", Py_TYPE(obj)->tp_name);
            }
            size_t nbits = _PyLong_NumBits(obj);
            if (nbits == (size_t)-1 && PyErr_Occurred()) return -1;
            size_t nbytes = (nbits + 8) / 8;
            size_t nlimbs = (nbytes + 7) / 8;
            if (nlimbs <= 1) return 16;  // inline
            return 16 + _Alignof(uint64_t) - 1 + nlimbs * sizeof(uint64_t);
        }
        case MORLOC_STRING:
        case MORLOC_ARRAY:
            if (schema->type == MORLOC_STRING && !(PyUnicode_Check(obj) || PyBytes_Check(obj) || PyByteArray_Check(obj) )) {
                PyRAISE("Expected str or bytes for MORLOC_STRING, but got %s", Py_TYPE(obj)->tp_name);
            }
            if (schema->type == MORLOC_ARRAY && !(PyList_Check(obj) || PyBytes_Check(obj) || PyByteArray_Check(obj) || PyObject_HasAttrString(obj, "__array_interface__"))) {
                PyRAISE("Expected list, bytes, bytearray, or numpy array for MORLOC_ARRAY, but got %s", Py_TYPE(obj)->tp_name);
            }
        
            {
                ssize_t required_size = 0;
                // worst-case cursor alignment padding for element data.
                // String stays at natural element alignment (1 byte for chars);
                // Array bumps to 64 for primitive numeric elements (SIMD/BLAS).
                size_t buf_align = (schema->type == MORLOC_STRING)
                    ? schema_alignment(schema->parameters[0])
                    : array_data_alignment(schema->parameters[0]);
                required_size += (ssize_t)(buf_align - 1);

                if (PyList_Check(obj)) {
                    Py_ssize_t list_size = PyList_Size(obj);
                    size_t element_width = schema->parameters[0]->width;
                    switch(schema->parameters[0]->type){
                        case MORLOC_NIL:
                        case MORLOC_BOOL:
                        case MORLOC_SINT8:
                        case MORLOC_SINT16:
                        case MORLOC_SINT32:
                        case MORLOC_SINT64:
                        case MORLOC_UINT8:
                        case MORLOC_UINT16:
                        case MORLOC_UINT32:
                        case MORLOC_UINT64:
                        case MORLOC_FLOAT32:
                        case MORLOC_FLOAT64:
                            required_size += list_size * element_width;
                            break;
                        case MORLOC_INT:
                        case MORLOC_STRING:
                        case MORLOC_ARRAY:
                        case MORLOC_TUPLE:
                        case MORLOC_MAP:
                        case MORLOC_OPTIONAL:
                            for(size_t i = 0; i < (size_t)list_size; i++){
                               required_size += get_shm_size(schema->parameters[0], PyList_GetItem(obj, i));
                            }
                            break;
                    }
                } else if (PyObject_HasAttrString(obj, "__array_interface__")) {
                    import_numpy();
                    PyArrayObject *arr = (PyArrayObject *)obj;
                    npy_intp *dims = PyArray_DIMS(arr);
                    int ndim = PyArray_NDIM(arr);
                    size_t total_elements = 1;
                    for (int i = 0; i < ndim; i++) {
                        total_elements *= dims[i];
                    }
                    // Per-element sizing required when (a) dtype=object
                    // (slots are PyObject*) or (b) the morloc element
                    // schema is variable-width -- numpy's flat inline
                    // storage does not match morloc's wire layout (e.g.
                    // numpy int64 is 8 bytes per slot but morloc Int is
                    // a 16-byte BigInt header plus optional limb tail).
                    if (PyArray_TYPE(arr) == NPY_OBJECT
                        || !schema_is_fixed_width(schema->parameters[0])) {
                        Schema* element_schema = schema->parameters[0];
                        for (size_t i = 0; i < total_elements; i++) {
                            PyObject* item = PyArray_GETITEM(arr, PyArray_GETPTR1(arr, i));
                            if (!item) {
                                PyRAISE("Failed to read element from numpy array");
                            }
                            ssize_t element_size = get_shm_size(element_schema, item);
                            Py_DECREF(item);
                            if (element_size == -1) {
                                return -1;
                            }
                            required_size += element_size;
                        }
                    } else {
                        required_size += total_elements * schema->parameters[0]->width;
                    }
                } else if (PyBytes_Check(obj)) {
                    required_size += (ssize_t)PyBytes_GET_SIZE(obj);
                } else if (PyByteArray_Check(obj)) {
                    required_size += (ssize_t)PyByteArray_GET_SIZE(obj);
                } else if (PyUnicode_Check(obj)) {
                    PyUnicode_AsUTF8AndSize(obj, &required_size);
                } else {
                    PyRAISE("Unsupported data type");
                }

                required_size += sizeof(Array);
                return required_size;
            }

        case MORLOC_TUPLE:
            if (!PyTuple_Check(obj) && !PyList_Check(obj)) {
                PyRAISE("Expected tuple or list for MORLOC_TUPLE, but got %s", Py_TYPE(obj)->tp_name);
            }

            {
                Py_ssize_t size = PyTuple_Check(obj) ? PyTuple_Size(obj) : PyList_Size(obj);
                if ((size_t)size != schema->size) {
                    PyRAISE("Tuple/List size mismatch");
                }

                size_t required_size = schema->width;

                for (Py_ssize_t i = 0; i < size; ++i) {
                    PyObject* item = PyTuple_Check(obj) ? PyTuple_GetItem(obj, i) : PyList_GetItem(obj, i);
                    ssize_t element_size = get_shm_size(schema->parameters[i], item);
                    if(element_size != -1){
                        if ((size_t)element_size > schema->parameters[i]->width) {
                            required_size += (size_t)element_size - schema->parameters[i]->width;
                        }
                    } else {
                        return -1;
                    }
                }
                return (ssize_t)required_size;
            }

        case MORLOC_MAP:
            if (!PyDict_Check(obj)) {
                PyRAISE("Expected dict for MORLOC_MAP, but got %s", Py_TYPE(obj)->tp_name);
            }

            {
                size_t required_size = schema->width;
                for (size_t i = 0; i < schema->size; ++i) {
                    PyObject* key = PyUnicode_FromString(schema->keys[i]);
                    PyObject* value = PyDict_GetItem(obj, key);
                    Py_DECREF(key);
                    if (value) {
                        ssize_t element_size = get_shm_size(schema->parameters[i], value);
                        if(element_size != -1){
                            if ((size_t)element_size > schema->parameters[i]->width) {
                                required_size += (size_t)element_size - schema->parameters[i]->width;
                            }
                        } else {
                            return -1;
                        }
                    }
                }
                return (ssize_t)required_size;
            }

        case MORLOC_OPTIONAL:
            // Slot is sizeof(relptr) (= schema->width). Absent → just the slot.
            // Present → slot + worst-case alignment padding for the inner T +
            // T's own total size (which already includes inner.width and any
            // variable extras T contributes).
            if (obj == Py_None) {
                return (ssize_t)schema->width;
            }
            {
                ssize_t inner_size = get_shm_size(schema->parameters[0], obj);
                if (inner_size == -1) return -1;
                size_t inner_align = schema_alignment(schema->parameters[0]);
                if (inner_align == 0) inner_align = 1;
                return (ssize_t)schema->width + (ssize_t)(inner_align - 1) + inner_size;
            }

        case MORLOC_RECUR: {
            // Resolve and recurse on the target. Note this calls the
            // _inner helper directly so the Recur node itself does not
            // get pushed (it isn't a declaration); the target's own
            // declaration is already on the stack from the enclosing
            // walk.
            const Schema* target = recur_env_lookup(schema->name);
            if (target == NULL) {
                PyRAISE("Recur back-reference to undeclared schema name '%s'",
                        schema->name ? schema->name : "?");
            }
            return get_shm_size_inner(target, obj);
        }

        default:
            PyRAISE("Unsupported schema type %d in calc_required_size", (int)schema->type);
    }

    PyRAISE("Reached the unreachable");

error:
    return -1;
}



static int to_voidstar_r_inner(void* dest, void** cursor, const Schema* schema, PyObject* obj);

// Public entry point: push the schema's declaration name (if any) and
// delegate to the inner walker. The push/pop discipline lets Recur arms
// inside the inner walker resolve via the env stack.
int to_voidstar_r(void* dest, void** cursor, const Schema* schema, PyObject* obj) {
    int pushed = recur_env_push(schema);
    int r = to_voidstar_r_inner(dest, cursor, schema, obj);
    recur_env_pop(pushed);
    return r;
}

static int to_voidstar_r_inner(void* dest, void** cursor, const Schema* schema, PyObject* obj) { MAYFAIL
    switch (schema->type) {
        case MORLOC_NIL:
            if (obj != Py_None) {
                PyRAISE("Expected None for MORLOC_NIL, but got %s", Py_TYPE(obj)->tp_name);
            }
            *((int8_t*)dest) = (int8_t)0;
            break;

        case MORLOC_BOOL:
            if (!PyBool_Check(obj)) {
                PyRAISE("Expected bool for MORLOC_BOOL, but got %s", Py_TYPE(obj)->tp_name);
            }
            *((bool*)dest) = (obj == Py_True);
            break;

        case MORLOC_SINT8:
            HANDLE_SINT_TYPE(int8_t, PyLong_AsLongLong, INT8_MIN, INT8_MAX);
            break;
        case MORLOC_SINT16:
            HANDLE_SINT_TYPE(int16_t, PyLong_AsLongLong, INT16_MIN, INT16_MAX);
            break;
        case MORLOC_SINT32:
            HANDLE_SINT_TYPE(int32_t, PyLong_AsLongLong, INT32_MIN, INT32_MAX);
            break;
        case MORLOC_SINT64:
            HANDLE_SINT_TYPE(int64_t, PyLong_AsLongLong, INT64_MIN, INT64_MAX);
            break;
        case MORLOC_UINT8:
            HANDLE_UINT_TYPE(uint8_t, PyLong_AsUnsignedLongLong, UINT8_MAX);
            break;
        case MORLOC_UINT16:
            HANDLE_UINT_TYPE(uint16_t, PyLong_AsUnsignedLongLong, UINT16_MAX);
            break;
        case MORLOC_UINT32:
            HANDLE_UINT_TYPE(uint32_t, PyLong_AsUnsignedLongLong, UINT32_MAX);
            break;
        case MORLOC_UINT64:
            HANDLE_UINT_TYPE(uint64_t, PyLong_AsUnsignedLongLong, UINT64_MAX);
            break;

        case MORLOC_FLOAT32:
            if (!PyFloat_Check(obj)) {
                PyRAISE("Expected float for MORLOC_FLOAT32, but got %s", Py_TYPE(obj)->tp_name);
            }
            *((float*)dest) = (float)PyFloat_AsDouble(obj);
            break;

        case MORLOC_FLOAT64:
            if(PyFloat_Check(obj))
            {
                *((double*)dest) = PyFloat_AsDouble(obj);
            } else if(PyLong_Check(obj)){
                *((double*)dest) = (double)PyLong_AsLongLong(obj);
            } else {
                PyRAISE("Expected float or int for MORLOC_FLOAT64, but got %s", Py_TYPE(obj)->tp_name);
            }
            break;

        case MORLOC_INT: {
            // Inline BigInt: [size:i64, value_or_relptr:i64]
            if (!PyLong_Check(obj)) {
                PyRAISE("Expected int for MORLOC_INT, but got %s", Py_TYPE(obj)->tp_name);
            }
            size_t nbits = _PyLong_NumBits(obj);
            if (nbits == (size_t)-1 && PyErr_Occurred()) { goto error; }
            size_t nbytes = (nbits + 8) / 8;
            size_t nlimbs = (nbytes + 7) / 8;
            if (nlimbs == 0) nlimbs = 1;

            int64_t* fields = (int64_t*)dest;
            if (nlimbs <= 1) {
                // Inline: write value directly
                fields[0] = 1;
                int overflow = 0;
                long long val = PyLong_AsLongLongAndOverflow(obj, &overflow);
                if (overflow || PyErr_Occurred()) {
                    PyErr_Clear();
                    // Shouldn't happen for nlimbs<=1, but fallback
                    fields[1] = 0;
                } else {
                    fields[1] = (int64_t)val;
                }
            } else {
                // Overflow: allocate limb array, store relptr
                fields[0] = (int64_t)nlimbs;
                *cursor = (void*)ALIGN_UP((uintptr_t)*cursor, _Alignof(uint64_t));
                {
                    char* rel_err = NULL;
                    *(relptr_t*)&fields[1] = abs2rel(*cursor, &rel_err);
                    if (rel_err) { free(rel_err); goto error; }
                }
                memset(*cursor, 0, nlimbs * sizeof(uint64_t));
                if (_PyLong_AsByteArray((PyLongObject*)obj,
                                        (unsigned char*)*cursor,
                                        nlimbs * sizeof(uint64_t),
                                        1, 1) < 0) {
                    goto error;
                }
                *cursor = (char*)*cursor + nlimbs * sizeof(uint64_t);
            }
            break;
        }

        case MORLOC_STRING:
        case MORLOC_ARRAY:
            if (schema->type == MORLOC_STRING && !(PyUnicode_Check(obj) || PyBytes_Check(obj)  || PyByteArray_Check(obj))) {
                PyRAISE("Expected str or bytes for MORLOC_STRING, but got %s", Py_TYPE(obj)->tp_name);
            }
    
            if (schema->type == MORLOC_ARRAY && !(PyList_Check(obj) || PyBytes_Check(obj) || PyByteArray_Check(obj) || PyObject_HasAttrString(obj, "__array_interface__"))) { 
                PyRAISE("Expected list, bytes, bytearray, or numpy array for MORLOC_ARRAY, but got %s", Py_TYPE(obj)->tp_name);
            }
    
            {
                Py_ssize_t size;

                // "bytes" type is mutable, so it exposes a non-const pointer 
                char* mutable_data = NULL;

                // strings type are immutable, so const
                const char* immutable_data = NULL; 

                // Distinguish a numpy array we can memcpy bulk off
                // PyArray_DATA from one we must walk per-element via
                // PyArray_GETITEM. We must walk per-element when (a)
                // dtype=object (PyArray_DATA exposes PyObject* pointer
                // bytes, not payloads) or (b) the morloc element
                // schema is variable-width (its wire layout has a
                // header / sub-data that numpy's flat dtype storage
                // does not provide).
                bool numpy_per_element = false;
                if (PyList_Check(obj)) {
                    size = PyList_Size(obj);
                } else if (PyBytes_Check(obj)) {
                    // This needs non-const data
                    PyBytes_AsStringAndSize(obj, &mutable_data, &size);
                } else if (PyByteArray_Check(obj)) {
                    mutable_data = PyByteArray_AS_STRING(obj);
                    size = PyByteArray_GET_SIZE(obj);
                } else if (schema->type == MORLOC_ARRAY && PyObject_HasAttrString(obj, "__array_interface__")) { // check if it is a numpy array
                    import_numpy();
                    PyArrayObject* arr = (PyArrayObject*)obj;
                    size = PyArray_SIZE(arr);

                    // Force per-element path when the morloc element
                    // schema is variable-width. The memcpy fast-path
                    // assumes numpy's flat element storage matches the
                    // wire layout; for Int (16-byte BigInt vs 8-byte
                    // int64) and other variable-width schemas it does
                    // not, and memcpy would read past numpy's buffer
                    // and write corrupt headers into SHM.
                    if (PyArray_TYPE(arr) == NPY_OBJECT
                        || !schema_is_fixed_width(schema->parameters[0])) {
                        // Boxed numpy array OR variable-width element
                        // schema: leave immutable_data NULL and dispatch
                        // to the per-element path below.
                        numpy_per_element = true;
                    } else {
                        // Fixed-width primitive numpy array.
                        immutable_data = PyArray_DATA(arr);
                        if (!PyArray_ISCONTIGUOUS(arr)) {
                            PyRAISE("NumPy array must be contiguous");
                        }
                    }
                } else {
                    immutable_data = PyUnicode_AsUTF8AndSize(obj, &size);
                }
    
                Array* result = (Array*)dest;
                result->size = (size_t)size;

                if(result->size == 0){
                    result->data = RELNULL;
                    break;
                }

                // align cursor for element data placement.
                // String stays at natural element alignment (1 byte for chars);
                // Array bumps to 64 for primitive numeric elements (SIMD/BLAS).
                {
                    size_t buf_align = (schema->type == MORLOC_STRING)
                        ? schema_alignment(schema->parameters[0])
                        : array_data_alignment(schema->parameters[0]);
                    *cursor = (void*)ALIGN_UP((uintptr_t)*cursor, buf_align);
                }

                result->data = PyTRY(abs2rel, *cursor);

                if (PyList_Check(obj) || numpy_per_element) {
                    // Per-element recursion: works for Python lists and
                    // numpy arrays that need per-element walking
                    // (dtype=object, or any variable-width element
                    // schema where numpy's flat storage cannot stand in
                    // for the wire layout). Element access differs
                    // (PyList_GetItem vs PyArray_GETITEM), but the
                    // wire-layout and recursion shape are identical.
                    size_t width = schema->parameters[0]->width;

                    // Move the cursor to the location immediately after the
                    // fixed sized elements
                    *cursor = (void*)(*(char**)cursor + size * width);

                    char* start = (char*) PyTRY(rel2abs, result->data);
                    Schema* element_schema = schema->parameters[0];
                    PyArrayObject* arr = numpy_per_element ? (PyArrayObject*)obj : NULL;
                    for (Py_ssize_t i = 0; i < size; i++) {
                        PyObject* item;
                        if (numpy_per_element) {
                            // PyArray_GETITEM returns a NEW reference.
                            item = PyArray_GETITEM(arr, PyArray_GETPTR1(arr, i));
                            if (!item) {
                                goto error;
                            }
                        } else {
                            // PyList_GetItem returns a BORROWED reference.
                            item = PyList_GetItem(obj, i);
                        }
                        int rc = to_voidstar_r(start + width * i, cursor, element_schema, item);
                        if (numpy_per_element) {
                            Py_DECREF(item);
                        }
                        if (rc != 0) {
                            goto error;
                        }
                    }

                } else if (PyBytes_Check(obj) || PyByteArray_Check(obj)){
                    absptr_t tmp_ptr = PyTRY(rel2abs, result->data);
                    memcpy(tmp_ptr, mutable_data, size);
                    // move cursor to the location after the copied data
                    *cursor = (void*)(*(char**)cursor + size);
                }
                else{
                    size_t width = schema->parameters[0]->width;

                    absptr_t tmp_ptr = PyTRY(rel2abs, result->data);
                    memcpy(tmp_ptr, immutable_data, size * width);

                    // Move the cursor to the location immediately after the
                    // fixed sized elements
                    *cursor = (void*)(*(char**)cursor + size * width);
                }
            }
            break;


        case MORLOC_TUPLE:
            if (!PyTuple_Check(obj) && !PyList_Check(obj)) {
                PyRAISE("Expected tuple or list for MORLOC_TUPLE, but got %s", Py_TYPE(obj)->tp_name);
            }

            {
                Py_ssize_t size = PyTuple_Check(obj) ? PyTuple_Size(obj) : PyList_Size(obj);
                if ((size_t)size != schema->size) {
                    PyRAISE("Tuple/List size mismatch");
                }
                for (Py_ssize_t i = 0; i < size; ++i) {
                    PyObject* item = PyTuple_Check(obj) ? PyTuple_GetItem(obj, i) : PyList_GetItem(obj, i);
                    if (to_voidstar_r((char*)dest + schema->offsets[i], cursor, schema->parameters[i], item) != 0) {
                        goto error;
                    }
                }
            }
            break;

        case MORLOC_MAP:
            if (!PyDict_Check(obj)) {
                PyRAISE("Expected dict for MORLOC_MAP, but got %s", Py_TYPE(obj)->tp_name);
            }

            {
                for (size_t i = 0; i < schema->size; ++i) {
                    PyObject* key = PyUnicode_FromString(schema->keys[i]);
                    PyObject* value = PyDict_GetItem(obj, key);
                    Py_DECREF(key);
                    if (value) {
                        if (to_voidstar_r((char*)dest + schema->offsets[i], cursor, schema->parameters[i], value) != 0) {
                            goto error;
                        }
                    }
                }
            }
            break;

        case MORLOC_OPTIONAL:
            // The slot is a relptr. Absent → write RELNULL. Present →
            // align the cursor for the inner T, write the inner's relptr
            // into the slot, advance the cursor past T's width, then
            // recurse to fill T's body (T may push the cursor further
            // for its own variable-length payload).
            if (obj == Py_None) {
                *((relptr_t*)dest) = RELNULL;
            } else {
                const Schema* inner_schema = schema->parameters[0];
                size_t inner_align = schema_alignment(inner_schema);
                if (inner_align == 0) inner_align = 1;
                *cursor = (void*)ALIGN_UP((uintptr_t)*cursor, inner_align);
                {
                    char* rel_err = NULL;
                    *(relptr_t*)dest = abs2rel(*cursor, &rel_err);
                    if (rel_err) { free(rel_err); goto error; }
                }
                void* inner_dest = *cursor;
                *cursor = (void*)((char*)*cursor + inner_schema->width);
                if (to_voidstar_r(inner_dest, cursor, inner_schema, obj) != 0) {
                    goto error;
                }
            }
            break;

        case MORLOC_RECUR: {
            // Resolve and dispatch on the named declaration. We call
            // the inner function directly because the Recur node is
            // not itself a declaration and the target's declaration is
            // already on the stack from an outer push.
            const Schema* target = recur_env_lookup(schema->name);
            if (target == NULL) {
                PyRAISE("Recur back-reference to undeclared schema name '%s'",
                        schema->name ? schema->name : "?");
            }
            if (to_voidstar_r_inner(dest, cursor, target, obj) != 0) {
                goto error;
            }
            break;
        }

        default:
            PyRAISE("Unsupported schema type %d in to_voidstar_r", (int)schema->type);
    }

    return 0;

error:
    return -1;
}

void* to_voidstar(const Schema* schema, PyObject* obj){ MAYFAIL
  void* dest = NULL;

  // calculate the required size of the shared memory object
  ssize_t shm_size = get_shm_size(schema, obj);
  if(shm_size == -1){
      PyRAISE("Schema does not match object");
  }

  // allocate the required memory as a single block
  dest = PyTRY(shmalloc, (size_t)shm_size);

  // set the write location of variable size chunks
  void* cursor = (void*)((char*)dest + schema->width);

  // write the data to the block
  int result = to_voidstar_r(dest, &cursor, schema, obj);
  if (result != 0) {
      goto error;
  }

  return dest;

error:
  if (dest != NULL) {
      char* free_errmsg = NULL;
      shfree(dest, &free_errmsg);
      free(free_errmsg);
  }
  return NULL;
}


// ── log emission bridge to libmorloc.so ──────────────────────────────────

static PyObject* pybinding__log_next_id(PyObject* self, PyObject* args) {
    (void)self; (void)args;
    return PyLong_FromUnsignedLongLong((unsigned long long)morloc_log_next_id());
}

static PyObject* pybinding__log_emit(PyObject* self, PyObject* args) {
    const char* tmpl;
    const char* group;
    double runtime;
    unsigned long long call_id;
    // group accepts None as well as an empty string -- both mean "skip tee".
    if (!PyArg_ParseTuple(args, "szdK", &tmpl, &group, &runtime, &call_id)) {
        return NULL;
    }
    morloc_log_emit(tmpl, group, runtime, (uint64_t)call_id);
    Py_RETURN_NONE;
}


// ── cache bridge to libmorloc.so ─────────────────────────────────────────

static PyObject* pybinding__pool_hash(PyObject* self, PyObject* args) {
    (void)self; (void)args;
    return PyLong_FromUnsignedLongLong((unsigned long long)morloc_pool_hash());
}

static PyObject* pybinding__cache_path(PyObject* self, PyObject* args) {
    const char* label;
    if (!PyArg_ParseTuple(args, "z", &label)) {
        return NULL;
    }
    char* errmsg = NULL;
    char* path = morloc_cache_path(label, &errmsg);
    if (!path) {
        if (errmsg) {
            PyErr_SetString(PyExc_RuntimeError, errmsg);
            free(errmsg);
        } else {
            PyErr_SetString(PyExc_RuntimeError, "morloc_cache_path failed");
        }
        return NULL;
    }
    PyObject* result = PyUnicode_FromString(path);
    free(path);
    return result;
}

static PyObject* pybinding__cache_lookup(PyObject* self, PyObject* args) {
    unsigned long long key;
    const char* label;
    if (!PyArg_ParseTuple(args, "Ks", &key, &label)) {
        return NULL;
    }
    size_t size = 0;
    char* errmsg = NULL;
    uint8_t* data = morloc_cache_lookup((uint64_t)key, label, &size, &errmsg);
    if (!data) {
        if (errmsg) {
            PyErr_SetString(PyExc_RuntimeError, errmsg);
            free(errmsg);
            return NULL;
        }
        Py_RETURN_NONE; // cache miss
    }
    PyObject* result = PyBytes_FromStringAndSize((const char*)data, (Py_ssize_t)size);
    free(data);
    return result;
}

static PyObject* pybinding__cache_store(PyObject* self, PyObject* args) {
    unsigned long long key;
    const char* label;
    Py_buffer buf;
    const char* schema_str;
    if (!PyArg_ParseTuple(args, "Ksy*s", &key, &label, &buf, &schema_str)) {
        return NULL;
    }
    char* errmsg = NULL;
    bool ok = morloc_cache_store(
        (uint64_t)key, label,
        (const uint8_t*)buf.buf, (size_t)buf.len,
        schema_str,
        &errmsg
    );
    PyBuffer_Release(&buf);
    if (!ok) {
        if (errmsg) {
            PyErr_SetString(PyExc_RuntimeError, errmsg);
            free(errmsg);
        } else {
            PyErr_SetString(PyExc_RuntimeError, "morloc_cache_store failed");
        }
        return NULL;
    }
    Py_RETURN_NONE;
}

static PyObject* pybinding__cache_key_compute(PyObject* self, PyObject* args) {
    unsigned int midx;
    PyObject* bytes_list;
    PyObject* schema_list;
    if (!PyArg_ParseTuple(args, "IOO", &midx, &bytes_list, &schema_list)) {
        return NULL;
    }
    if (!PyList_Check(bytes_list) && !PyTuple_Check(bytes_list)) {
        PyErr_SetString(PyExc_TypeError,
            "cache_key_compute: first arg must be a list/tuple of packet bytes");
        return NULL;
    }
    if (!PyList_Check(schema_list) && !PyTuple_Check(schema_list)) {
        PyErr_SetString(PyExc_TypeError,
            "cache_key_compute: second arg must be a list/tuple of schema strings");
        return NULL;
    }
    Py_ssize_t n_args = PySequence_Fast_GET_SIZE(bytes_list);
    if (PySequence_Fast_GET_SIZE(schema_list) != n_args) {
        PyErr_SetString(PyExc_ValueError,
            "cache_key_compute: packet list and schema list must have equal length");
        return NULL;
    }

    // Build parallel arrays of packet pointers + schema cstrings. Hold
    // Py_buffer views for the lifetime of the call so the byte pointers
    // stay live; schema cstrings are owned by their PyObjects in
    // schema_list.
    Py_buffer* views = (Py_buffer*)calloc((size_t)n_args, sizeof(Py_buffer));
    const uint8_t** packet_ptrs = (const uint8_t**)calloc((size_t)n_args, sizeof(uint8_t*));
    const char** schema_ptrs = (const char**)calloc((size_t)n_args, sizeof(char*));
    if (!views || !packet_ptrs || !schema_ptrs) {
        free(views); free(packet_ptrs); free(schema_ptrs);
        PyErr_NoMemory();
        return NULL;
    }
    Py_ssize_t acquired = 0;
    for (Py_ssize_t i = 0; i < n_args; i++) {
        PyObject* packet_item = PySequence_Fast_GET_ITEM(bytes_list, i);
        PyObject* schema_item = PySequence_Fast_GET_ITEM(schema_list, i);

        if (PyObject_GetBuffer(packet_item, &views[i], PyBUF_SIMPLE) != 0) {
            for (Py_ssize_t j = 0; j < acquired; j++) {
                PyBuffer_Release(&views[j]);
            }
            free(views); free(packet_ptrs); free(schema_ptrs);
            return NULL;
        }
        acquired++;
        packet_ptrs[i] = (const uint8_t*)views[i].buf;

        const char* schema_cstr = PyUnicode_AsUTF8(schema_item);
        if (!schema_cstr) {
            for (Py_ssize_t j = 0; j < acquired; j++) {
                PyBuffer_Release(&views[j]);
            }
            free(views); free(packet_ptrs); free(schema_ptrs);
            return NULL;
        }
        schema_ptrs[i] = schema_cstr;
    }

    char* errmsg = NULL;
    uint64_t key = morloc_cache_key_compute(
        (uint32_t)midx, packet_ptrs, schema_ptrs, (size_t)n_args, &errmsg
    );

    for (Py_ssize_t i = 0; i < acquired; i++) {
        PyBuffer_Release(&views[i]);
    }
    free(views); free(packet_ptrs); free(schema_ptrs);

    if (errmsg) {
        PyErr_SetString(PyExc_RuntimeError, errmsg);
        free(errmsg);
        return NULL;
    }

    return PyLong_FromUnsignedLongLong((unsigned long long)key);
}

static PyObject* pybinding__cache_record_hit(PyObject* self, PyObject* args) {
    (void)self; (void)args;
    morloc_cache_record_hit();
    Py_RETURN_NONE;
}

static PyObject* pybinding__cache_record_miss(PyObject* self, PyObject* args) {
    (void)self; (void)args;
    morloc_cache_record_miss();
    Py_RETURN_NONE;
}

static PyObject* pybinding__cache_record_store(PyObject* self, PyObject* args) {
    (void)self; (void)args;
    morloc_cache_record_store();
    Py_RETURN_NONE;
}

static PyObject* pybinding__wait_for_client(PyObject* self, PyObject* args) { MAYFAIL
    PyObject* daemon_capsule;

    if (!PyArg_ParseTuple(args, "O", &daemon_capsule)) {
        PyRAISE("Failed to parse arguments");
    }

    language_daemon_t* daemon = (language_daemon_t*)PyCapsule_GetPointer(daemon_capsule, "language_daemon_t");

    int client_fd = PyTRY(wait_for_client, daemon);

    return PyLong_FromLong((long)client_fd);

error:
    return NULL;
}

static PyObject* pybinding__start_daemon(PyObject* self, PyObject* args) { MAYFAIL
    const char* socket_path;
    const char* tmpdir;
    const char* shm_basename;
    size_t shm_default_size;
    language_daemon_t* daemon = NULL;

    if (!PyArg_ParseTuple(args, "sssk", &socket_path, &tmpdir, &shm_basename, &shm_default_size)) {
      goto error;
    }

    daemon = PyTRY(
        start_daemon,
        socket_path,
        tmpdir,
        shm_basename,
        shm_default_size
    );

    return PyCapsule_New(daemon, "language_daemon_t", NULL);

error:
    FREE(daemon)
    return NULL;
}


static PyObject* pybinding__close_daemon(PyObject* self, PyObject* args) {
    PyObject* daemon_capsule;

    if (!PyArg_ParseTuple(args, "O", &daemon_capsule)) {
        PyRAISE("Failed to parse arguments");
    }

    language_daemon_t* daemon = (language_daemon_t*)PyCapsule_GetPointer(daemon_capsule, "language_daemon_t");

    if(daemon != NULL){
        close_daemon(&daemon);
    }

    Py_RETURN_NONE;

error:
    return NULL;
}


static PyObject*  pybinding__read_morloc_call_packet(PyObject* self, PyObject* args){ MAYFAIL
    char* packet;
    size_t packet_size;
    morloc_call_t* call_packet = NULL;
    PyObject* py_tuple = NULL;
    PyObject* py_args = NULL;
    PyObject* py_mid = NULL;

    if (!PyArg_ParseTuple(args, "y#", &packet, &packet_size)) {
        PyRAISE("Failed to parse arguments");
    }
    call_packet = PyTRY(read_morloc_call_packet, (const uint8_t*)packet);

    py_tuple = PyTuple_New(2);
    if (!py_tuple) { PyRAISE("Allocation failed"); }
    py_args = PyList_New(call_packet->nargs);
    if (!py_args) { PyRAISE("Allocation failed"); }
    py_mid = PyLong_FromLong((long)call_packet->midx);
    if (!py_mid) { PyRAISE("Allocation failed"); }
    for(size_t i = 0; i < call_packet->nargs; i++){
        size_t arg_packet_size = PyTRY(morloc_packet_size, call_packet->args[i]);
        PyObject* py_arg = PyBytes_FromStringAndSize(
            (char*)call_packet->args[i],
            arg_packet_size
        );
        PyList_SetItem(py_args, i, py_arg);
    }

    PyTuple_SetItem(py_tuple, 0, py_mid);
    PyTuple_SetItem(py_tuple, 1, py_args);
    py_mid = NULL;  // stolen by PyTuple_SetItem
    py_args = NULL;  // stolen by PyTuple_SetItem

    free_morloc_call(call_packet);

    return py_tuple;

error:
    if (call_packet) free_morloc_call(call_packet);
    Py_XDECREF(py_mid);
    Py_XDECREF(py_args);
    Py_XDECREF(py_tuple);
    return NULL;
}

static PyObject*  pybinding__send_packet_to_foreign_server(PyObject* self, PyObject* args){ MAYFAIL
    int client_fd = 0;
    uint8_t* packet = NULL;
    size_t packet_size = 0;

    if (!PyArg_ParseTuple(args, "iy#", &client_fd, &packet, &packet_size)) {
        PyRAISE("Failed to parse arguments");
    }

    size_t bytes_sent = PyTRY(send_packet_to_foreign_server, client_fd, packet);

    return PyLong_FromSize_t(bytes_sent);

error:
    return NULL;
}


static PyObject*  pybinding__stream_from_client(PyObject* self, PyObject* args){ MAYFAIL
    int client_fd = 0;
    uint8_t* packet = NULL;

    if (!PyArg_ParseTuple(args, "i", &client_fd)) {
        PyRAISE("Failed to parse arguments");
    }

    packet = PyTRY(stream_from_client, client_fd);

    size_t packet_size = PyTRY(morloc_packet_size, packet);

    PyObject* retval = PyBytes_FromStringAndSize((char*)packet, packet_size);

    free(packet);

    return retval;

error:
    FREE(packet)
    return NULL;
}



static PyObject*  pybinding__close_socket(PyObject* self, PyObject* args){
    int socket_id = 0;

    if (!PyArg_ParseTuple(args, "i", &socket_id)) {
        PyRAISE("Failed to parse arguments");
    }

    close_socket(socket_id);

    Py_RETURN_NONE;

error:
    return NULL;
}

// Transforms a value into a message ready for the socket
static PyObject* pybinding__put_value(PyObject* self, PyObject* args){ MAYFAIL
    uint8_t* packet = NULL;
    Schema* schema = NULL;
    void* voidstar = NULL;
    size_t packet_size = 0;
    bool tracked = false;

    PyObject* obj;
    const char* schema_str;

    if (!PyArg_ParseTuple(args, "Os", &obj, &schema_str)) {
        PyRAISE("Failed to parse arguments");
    }

    schema = PyTRY(parse_schema, schema_str);

    // Arrow dispatch: schema marker `T` (MORLOC_TABLE) routes through the
    // Arrow C Data Interface. The legacy `<arrow>` hint has been retired;
    // the schema type itself now signals the dispatch.
    if (schema->type == MORLOC_TABLE) {
        // Export pyarrow object via C Data Interface -> copy to shm -> packet
        struct ArrowSchema arrow_schema;
        struct ArrowArray arrow_array;

        // Call obj._export_to_c(arrow_array_ptr, arrow_schema_ptr)
        PyObject* export_result = PyObject_CallMethod(
            obj, "_export_to_c",
            "nn", (Py_ssize_t)&arrow_array, (Py_ssize_t)&arrow_schema);
        if (!export_result) {
            free_schema(schema);
            PyRAISE("Failed to export pyarrow object via C Data Interface");
        }
        Py_DECREF(export_result);

        char* errmsg = NULL;
        relptr_t relptr = arrow_to_shm(&arrow_array, &arrow_schema, &errmsg);

        // Release the exported C Data Interface structs
        if (arrow_schema.release) arrow_schema.release(&arrow_schema);
        if (arrow_array.release) arrow_array.release(&arrow_array);

        if (errmsg) {
            free_schema(schema);
            PyErr_SetString(PyExc_RuntimeError, errmsg);
            free(errmsg);
            return NULL;
        }

        packet = make_arrow_data_packet(relptr, schema);
        if (!packet) {
            free_schema(schema);
            PyRAISE("Failed to create arrow data packet");
        }

        // Track shm for cleanup
        char* resolve_err = NULL;
        void* shm_ptr = rel2abs(relptr, &resolve_err);
        if (resolve_err) { free(resolve_err); }
        if (shm_ptr) {
            shm_tracker_push((absptr_t)shm_ptr, NULL);
            tracked = true;
        }

        packet_size = PyTRY(morloc_packet_size, packet);
        PyObject* retval = PyBytes_FromStringAndSize((char*)packet, packet_size);
        free(packet);
        free_schema(schema);
        return retval;
    }

    voidstar = to_voidstar(schema, obj);
    PyTRACE(voidstar == NULL)

    // convert to a relative pointer conserved between language servers
    relptr_t relptr = PyTRY(abs2rel, voidstar);

    packet = PyTRY(make_data_packet_auto, voidstar, relptr, schema);

    {
        const morloc_packet_header_t* hdr = (const morloc_packet_header_t*)packet;
        if (hdr->command.data.source == PACKET_SOURCE_RPTR) {
            // SHM referenced by packet -- track for deferred cleanup
            shm_tracker_push((absptr_t)voidstar, schema);
            tracked = true;
        } else {
            // Data inlined in packet -- free SHM immediately. shfree zeros
            // the block on final ref-drop, so no metadata-walk needed.
            char* free_err = NULL;
            shfree((absptr_t)voidstar, &free_err);
            if (free_err) { free(free_err); }
            voidstar = NULL;
        }
    }

    packet_size = PyTRY(morloc_packet_size, packet);

    {
        PyObject* retval = PyBytes_FromStringAndSize((char*)packet, packet_size);
        free(packet);
        if (!tracked) {
            free_schema(schema);
        }
        return retval;
    }

error:
    FREE(packet)
    if (!tracked) {
        if (voidstar && schema) {
            char* free_err = NULL;
            shfree((absptr_t)voidstar, &free_err);
            if (free_err) { free(free_err); }
        }
        free_schema(schema);
    }
    return NULL;
}


// Use a key to retrieve a value
static PyObject* pybinding__get_value(PyObject* self, PyObject* args){ MAYFAIL
    uint8_t* voidstar = NULL;
    Schema* schema = NULL;
    PyObject* obj = NULL;
    bool tracked = false;

    const char* packet;
    size_t packet_size;
    const char* schema_str;

    if (!PyArg_ParseTuple(args, "y#s", &packet, &packet_size, &schema_str)) {
        PyRAISE("Failed to parse arguments");
    }

    const morloc_packet_header_t* header = (const morloc_packet_header_t*)packet;
    uint8_t source = header->command.data.source;
    uint8_t format = header->command.data.format;

    schema = PyTRY(parse_schema, schema_str)

    // Arrow dispatch: if packet format is Arrow, import via C Data Interface
    if (format == PACKET_FORMAT_ARROW) {
        voidstar = PyTRY(get_morloc_data_packet_value, (uint8_t*)packet, schema);

        const arrow_shm_header_t* arrow_hdr = (const arrow_shm_header_t*)voidstar;

        struct ArrowSchema arrow_schema;
        struct ArrowArray arrow_array;
        char* arrow_err = NULL;
        arrow_from_shm(arrow_hdr, &arrow_schema, &arrow_array, &arrow_err);
        if (arrow_err) {
            free_schema(schema);
            PyErr_SetString(PyExc_RuntimeError, arrow_err);
            free(arrow_err);
            return NULL;
        }

        // Import via pyarrow RecordBatch.from_buffers or _import_from_c
        PyObject* pyarrow_mod = PyImport_ImportModule("pyarrow");
        if (!pyarrow_mod) {
            if (arrow_schema.release) arrow_schema.release(&arrow_schema);
            if (arrow_array.release) arrow_array.release(&arrow_array);
            free_schema(schema);
            PyRAISE("pyarrow is required for arrow-typed data");
        }

        PyObject* rb_class = PyObject_GetAttrString(pyarrow_mod, "RecordBatch");
        Py_DECREF(pyarrow_mod);
        if (!rb_class) {
            if (arrow_schema.release) arrow_schema.release(&arrow_schema);
            if (arrow_array.release) arrow_array.release(&arrow_array);
            free_schema(schema);
            PyRAISE("Failed to get pyarrow.RecordBatch");
        }

        // Use RecordBatch._import_from_c(array_ptr, schema_ptr)
        obj = PyObject_CallMethod(rb_class, "_import_from_c",
            "nn", (Py_ssize_t)&arrow_array, (Py_ssize_t)&arrow_schema);
        Py_DECREF(rb_class);

        // Incref shm so it stays alive while pyarrow references the buffers
        char* incref_err = NULL;
        shincref((absptr_t)voidstar, &incref_err);
        if (incref_err) { free(incref_err); }
        shm_tracker_push((absptr_t)voidstar, NULL);

        free_schema(schema);
        if (!obj) return NULL;
        return obj;
    }

    // Fast path: inline voidstar -- read directly from packet, no SHM needed
    if (source == PACKET_SOURCE_MESG && format == PACKET_FORMAT_VOIDSTAR) {
        const uint8_t* payload = (const uint8_t*)packet + sizeof(morloc_packet_header_t) + header->offset;
        obj = fromAnything(schema, (const void*)payload, (const void*)payload);
        PyTRACE(obj == NULL)
        free_schema(schema);
        return obj;
    }

    // SHM paths (RPTR or MESG+MSGPACK)
    bool is_rptr = (source == PACKET_SOURCE_RPTR);

    voidstar = PyTRY(get_morloc_data_packet_value, (uint8_t*)packet, schema);

    // For RPTR data, increment refcount so the owner's tracker flush
    // won't destroy data we may still need (e.g. forwarded packets).
    if (is_rptr) {
        char* incref_err = NULL;
        shincref((absptr_t)voidstar, &incref_err);
        if (incref_err) { free(incref_err); }
        // Track for deferred decref (tracker takes schema ownership)
        shm_tracker_push((absptr_t)voidstar, schema);
        tracked = true;
    }

    obj = fromAnything(schema, voidstar, NULL);
    PyTRACE(obj == NULL)

    if (!tracked) {
        free_schema(schema);
    }

    return obj;

error:
    if (!tracked) {
        free_schema(schema);
    }
    return NULL;
}


// Free tracked SHM allocations from put_value calls.
// Called at dispatch start to free result SHM from previous dispatch.
static PyObject* pybinding__flush_shm_tracker(PyObject* self, PyObject* args) {
    (void)self; (void)args;
    flush_shm_tracker();
    Py_RETURN_NONE;
}


// Debug-trace bindings -- forwarded straight to the Rust runtime in
// libmorloc.so. The pool's run_job and per-manifold catch blocks call
// these via the morloc Python module.
extern void morloc_debug_record_frame(
    uint32_t midx,
    const uint8_t** packets,
    const char** schemas,
    size_t n);
extern char* morloc_debug_drain_frames(void);
extern void morloc_debug_flush_dispatch(void);

// debug_record_frame(midx, packets_list, schemas_list) -- packets is a
// Python list of bytes objects (each a serialized morloc packet);
// schemas is a Python list of str. The codegen-emitted catch block
// builds both lists from the per-arg serialization results.
static PyObject* pybinding__debug_record_frame(PyObject* self, PyObject* args) {
    (void)self;
    unsigned long midx_arg;
    PyObject* packets_list;
    PyObject* schemas_list;
    if (!PyArg_ParseTuple(args, "kOO", &midx_arg, &packets_list, &schemas_list)) {
        return NULL;
    }
    if (!PyList_Check(packets_list) || !PyList_Check(schemas_list)) {
        PyErr_SetString(PyExc_TypeError, "packets and schemas must be lists");
        return NULL;
    }
    Py_ssize_t n_pkts = PyList_Size(packets_list);
    Py_ssize_t n_sch = PyList_Size(schemas_list);
    if (n_pkts != n_sch) {
        PyErr_SetString(PyExc_ValueError,
            "packets and schemas must have the same length");
        return NULL;
    }
    if (n_pkts == 0) {
        morloc_debug_record_frame((uint32_t)midx_arg, NULL, NULL, 0);
        Py_RETURN_NONE;
    }
    const uint8_t** pkt_arr = (const uint8_t**)calloc((size_t)n_pkts, sizeof(uint8_t*));
    const char** sch_arr = (const char**)calloc((size_t)n_pkts, sizeof(char*));
    if (!pkt_arr || !sch_arr) {
        free(pkt_arr); free(sch_arr);
        PyErr_NoMemory();
        return NULL;
    }
    for (Py_ssize_t i = 0; i < n_pkts; i++) {
        PyObject* p = PyList_GetItem(packets_list, i);
        PyObject* s = PyList_GetItem(schemas_list, i);
        if (!PyBytes_Check(p) || !PyUnicode_Check(s)) {
            free(pkt_arr); free(sch_arr);
            PyErr_SetString(PyExc_TypeError,
                "packets must be bytes and schemas must be str");
            return NULL;
        }
        pkt_arr[i] = (const uint8_t*)PyBytes_AsString(p);
        sch_arr[i] = PyUnicode_AsUTF8(s);
    }
    morloc_debug_record_frame((uint32_t)midx_arg, pkt_arr, sch_arr, (size_t)n_pkts);
    free(pkt_arr);
    free(sch_arr);
    Py_RETURN_NONE;
}

// debug_drain_frames() -> Optional[str] -- returns None when no frames
// were recorded (clean run or --debug not compiled in).
static PyObject* pybinding__debug_drain_frames(PyObject* self, PyObject* args) {
    (void)self; (void)args;
    char* trace = morloc_debug_drain_frames();
    if (trace == NULL) {
        Py_RETURN_NONE;
    }
    PyObject* result = PyUnicode_FromString(trace);
    free(trace);
    return result;
}

// Reset per-dispatch debug state (recursion counters, write counter,
// frame stack). Called by the pool's run_job at the start of each
// new top-level call.
static PyObject* pybinding__debug_flush_dispatch(PyObject* self, PyObject* args) {
    (void)self; (void)args;
    morloc_debug_flush_dispatch();
    Py_RETURN_NONE;
}


// Release the SHM ref owned by a put_value-produced packet. The codegen
// inserts this call at the end of a serialize let's scope so the tracker
// entry is dropped as soon as the packet is no longer needed, instead of
// accumulating until the dispatch boundary. No-op for inline packets
// (those don't carry an SHM relptr), so callers can invoke unconditionally.
static PyObject* pybinding__release_packet_shm(PyObject* self, PyObject* args) { MAYFAIL
    const char* packet;
    Py_ssize_t packet_size;

    if (!PyArg_ParseTuple(args, "y#", &packet, &packet_size)) {
        PyRAISE("Failed to parse arguments");
    }

    if ((size_t)packet_size < sizeof(morloc_packet_header_t)) {
        Py_RETURN_NONE;
    }

    const morloc_packet_header_t* hdr = (const morloc_packet_header_t*)packet;
    if (hdr->command.data.source != PACKET_SOURCE_RPTR) {
        Py_RETURN_NONE;
    }

    size_t relptr = *(size_t*)((const uint8_t*)packet
        + sizeof(morloc_packet_header_t) + hdr->offset);
    char* resolve_err = NULL;
    void* voidstar = rel2abs(relptr, &resolve_err);
    if (resolve_err) { free(resolve_err); }
    if (voidstar) {
        shm_tracker_release_one((absptr_t)voidstar);
    }

    Py_RETURN_NONE;

error:
    return NULL;
}


// Make a foreign call
//
// Arguments:
//   1. socket path
//   2. midx
//   3. list of arguments, each is bytestring packet
static PyObject* pybinding__foreign_call(PyObject* self, PyObject* args) { MAYFAIL
    char* socket_path;
    int mid;
    PyObject* py_args;
    const uint8_t** arg_packets = NULL;
    Py_ssize_t nargs;
    Py_ssize_t i;
    uint8_t* packet = NULL;
    uint8_t* result = NULL;
    size_t result_length = 0;

    // Parse arguments: string, integer, and sequence
    if (!PyArg_ParseTuple(args, "siO", &socket_path, &mid, &py_args)) {
        PyRAISE("Failed to parse argument")
    }

    // Verify third argument is a sequence
    if (!PySequence_Check(py_args)) {
        PyRAISE("Third argument must be a sequence");
    }

    // Get sequence size and allocate C arrays
    nargs = PySequence_Size(py_args);
    arg_packets = (const uint8_t**)calloc(nargs, sizeof(uint8_t*));
    if (!arg_packets) {
        PyErr_NoMemory();
        goto error;
    }

    // Convert Python bytes to C buffers
    for (i = 0; i < nargs; i++) {
        PyObject* item = PySequence_GetItem(py_args, i);
        if (!PyBytes_Check(item)) {
            Py_DECREF(item);
            free(arg_packets);
            arg_packets = NULL;
            PyRAISE("All arguments must be bytes objects");
        }
        arg_packets[i] = (const uint8_t*)PyBytes_AsString(item);
        Py_DECREF(item);
    }

    packet = PyTRY(make_morloc_local_call_packet, (uint32_t)mid, arg_packets, (size_t)nargs);

    free(arg_packets);
    arg_packets = NULL;

    result = PyTRY(send_and_receive_over_socket, socket_path, packet);
    free(packet);
    packet = NULL;

    // If the foreign pool returned a fail packet, surface it as a Python
    // exception. Without this the raw fail-packet bytes get returned to
    // the autogen caller which then tries to deserialize them as data,
    // producing a confusing downstream error or crashing the worker.
    {
        char* fail_check_err = NULL;
        char* fail_msg = get_morloc_data_packet_error_message(
            (const uint8_t*)result, &fail_check_err);
        if (fail_check_err != NULL) { free(fail_check_err); }
        if (fail_msg != NULL) {
            PyErr_Format(PyExc_RuntimeError, "%s", fail_msg);
            free(fail_msg);
            free(result);
            result = NULL;
            goto error;
        }
    }

    // Incref the result's SHM so the callee's tracker flush won't destroy
    // data we may still need (e.g. forwarded result packets).
    {
        const morloc_packet_header_t* res_header = (const morloc_packet_header_t*)result;
        if (res_header->command.data.source == PACKET_SOURCE_RPTR) {
            size_t relptr = *(size_t*)((uint8_t*)result + res_header->offset + sizeof(morloc_packet_header_t));
            char* resolve_err = NULL;
            void* res_voidstar = rel2abs(relptr, &resolve_err);
            if (resolve_err) { free(resolve_err); resolve_err = NULL; }
            if (res_voidstar) {
                char* incref_err = NULL;
                shincref((absptr_t)res_voidstar, &incref_err);
                if (incref_err) { free(incref_err); }
                shm_tracker_push((absptr_t)res_voidstar, NULL);
            }
        }
    }

    result_length = PyTRY(morloc_packet_size, result);

    PyObject* retval = PyBytes_FromStringAndSize((char*)result, result_length);

    free(result);

    return retval;

error:
    FREE(arg_packets)
    FREE(packet)
    return NULL;
}


static PyObject* pybinding__remote_call(PyObject* self, PyObject* args) { MAYFAIL
    int midx;
    char* socket_base;
    char* cache_path;
    PyObject* res_struct; // python struct that is converted to a resource_t struct
    PyObject* arg_packets_obj; // python list of bytes types
    const uint8_t** arg_packets = NULL;
    uint8_t* result = NULL;

    if (!PyArg_ParseTuple(args, "issOO", &midx, &socket_base, &cache_path, &res_struct, &arg_packets_obj)) {
        PyRAISE("Failed to parse arguments");
    }

    if (!PyBytes_Check(res_struct)) {
        PyRAISE("res_struct must be a bytes object from struct.pack()");
    }

    // Ensure the resources struct is the right size
    if (PyBytes_Size(res_struct) != sizeof(resources_t)) {
        PyRAISE("Struct size mismatch");
    }

    resources_t* res = (resources_t*)PyBytes_AsString(res_struct);
    PyTRACE(res == NULL)

    Py_ssize_t nargs = PyList_Size(arg_packets_obj);

    arg_packets = calloc(nargs, sizeof(uint8_t*));
    if (arg_packets == NULL) {
        PyRAISE("Memory allocation failed");
    }

    for (Py_ssize_t i = 0; i < nargs; i++) {
        PyObject* packet_obj = PyList_GetItem(arg_packets_obj, i);
        if (!PyBytes_Check(packet_obj)) {
            PyRAISE("Packets must be bytes");
        }
        arg_packets[i] = (uint8_t*)PyBytes_AsString(packet_obj);
    }

    result = PyTRY(
        remote_call,
        midx,
        socket_base,
        cache_path,
        res,
        arg_packets,
        (size_t)nargs
    );

    free(arg_packets);

    if (result == NULL) Py_RETURN_NONE;
    size_t result_length = PyTRY(morloc_packet_size, result);
    PyObject* py_result = PyBytes_FromStringAndSize((char*)result, result_length);
    free(result);
    return py_result;

error:
    if (result != NULL){
        free(result);
    }
    if (arg_packets != NULL){
        // The elements are handled by Python and should not be freed
        free(arg_packets);
    }
    return NULL;
}


static PyObject* pybinding__is_ping(PyObject* self, PyObject* args) { MAYFAIL
    char* packet;
    size_t packet_size;

    if (!PyArg_ParseTuple(args, "y#", &packet, &packet_size)) {
        PyRAISE("Failed to parse arguments");
    }

    bool is_ping = PyTRY(packet_is_ping, (uint8_t*)packet);

    PyObject* obj = PyBool_FromLong((long)is_ping);

    return obj;

error:
    return NULL;
}


static PyObject* pybinding__is_local_call(PyObject* self, PyObject* args) { MAYFAIL
    char* packet;
    size_t packet_size;

    if (!PyArg_ParseTuple(args, "y#", &packet, &packet_size)) {
        PyRAISE("Failed to parse arguments");
    }

    bool is_local_call = PyTRY(packet_is_local_call, (uint8_t*)packet);

    PyObject* obj = PyBool_FromLong((long)is_local_call);

    return obj;

error:
    return NULL;
}

static PyObject* pybinding__is_remote_call(PyObject* self, PyObject* args) { MAYFAIL
    char* packet;
    size_t packet_size;

    if (!PyArg_ParseTuple(args, "y#", &packet, &packet_size)) {
        PyRAISE("Failed to parse arguments");
    }

    bool is_remote_call = PyTRY(packet_is_remote_call, (uint8_t*)packet);

    PyObject* obj = PyBool_FromLong((long)is_remote_call);

    return obj;

error:
    return NULL;
}


static PyObject* pybinding__pong(PyObject* self, PyObject* args) { MAYFAIL
    char* packet;
    size_t packet_size;
    uint8_t* pong = NULL;

    if (!PyArg_ParseTuple(args, "y#", &packet, &packet_size)) {
        PyRAISE("Failed to parse arguments");
    }

    pong = PyTRY(return_ping, (uint8_t*)packet);

    size_t pong_size = PyTRY(morloc_packet_size, pong);

    {
        PyObject* retval = PyBytes_FromStringAndSize((char*)pong, pong_size);
        free(pong);
        return retval;
    }

error:
    FREE(pong)
    return NULL;
}

static PyObject* pybinding__set_fallback_dir(PyObject* self, PyObject* args) {
    const char* dir;
    if (!PyArg_ParseTuple(args, "s", &dir)) {
        return NULL;
    }
    shm_set_fallback_dir(dir);
    Py_RETURN_NONE;
}

static PyObject* pybinding__shinit(PyObject* self, PyObject* args) { MAYFAIL
    shm_t* shm = NULL;
    
    const char* shm_basename;
    size_t volume_index;
    size_t shm_default_size;

    if (!PyArg_ParseTuple(args, "skk", &shm_basename, &volume_index, &shm_default_size)) {
        PyRAISE("Failed to parse arguments");
    }

    shm = PyTRY(
        shinit,
        shm_basename,
        volume_index,
        shm_default_size
    );

    return PyCapsule_New(shm, "shm_t", NULL);

error:
    FREE(shm)
    return NULL;
}


static PyObject* pybinding__make_fail_packetg(PyObject* self, PyObject* args) { MAYFAIL
    const char* packet_errmsg;
    uint8_t* packet = NULL;

    if (!PyArg_ParseTuple(args, "s", &packet_errmsg)) {
        PyRAISE("Failed to parse arguments");
    }

    packet = make_fail_packet(packet_errmsg);

    size_t packet_size = PyTRY(morloc_packet_size, packet);

    {
        PyObject* retval = PyBytes_FromStringAndSize((char*)packet, packet_size);
        free(packet);
        return retval;
    }

error:
    FREE(packet)
    return NULL;
}

static PyObject* pybinding__mlc_hash(PyObject* self, PyObject* args) { MAYFAIL
    PyObject* obj;
    const char* schema_str;
    Schema* schema = NULL;
    void* voidstar = NULL;
    char* hex = NULL;

    if (!PyArg_ParseTuple(args, "Os", &obj, &schema_str)) {
        PyRAISE("Failed to parse arguments");
    }

    schema = PyTRY(parse_schema, schema_str);

    voidstar = to_voidstar(schema, obj);
    PyTRACE(voidstar == NULL)

    hex = PyTRY(mlc_hash, voidstar, schema);

    {
        char* shfree_errmsg = NULL;
        shfree(voidstar, &shfree_errmsg);
        free(shfree_errmsg);
    }
    free_schema(schema);

    {
        PyObject* retval = PyUnicode_FromString(hex);
        free(hex);
        return retval;
    }

error:
    if (voidstar) {
        char* shfree_errmsg = NULL;
        shfree(voidstar, &shfree_errmsg);
        free(shfree_errmsg);
    }
    free_schema(schema);
    FREE(hex)
    return NULL;
}

static PyObject* pybinding__mlc_save(PyObject* self, PyObject* args) { MAYFAIL
    PyObject* obj;
    const char* schema_str;
    long long level_ll;
    const char* path;
    Schema* schema = NULL;
    void* voidstar = NULL;

    // Args: (value, schema, level, path). The level is accepted here
    // for ABI uniformity with mlc_save_voidstar; the runtime ignores it
    // for the msgpack format (not a packet file).
    if (!PyArg_ParseTuple(args, "OsLs", &obj, &schema_str, &level_ll, &path)) {
        PyRAISE("Failed to parse arguments");
    }
    uint8_t level = (uint8_t)level_ll;

    schema = PyTRY(parse_schema, schema_str);

    voidstar = to_voidstar(schema, obj);
    PyTRACE(voidstar == NULL)

    PyTRY(mlc_save, voidstar, schema, level, path);

    {
        char* shfree_errmsg = NULL;
        shfree(voidstar, &shfree_errmsg);
        free(shfree_errmsg);
    }
    free_schema(schema);
    Py_RETURN_NONE;

error:
    if (voidstar) {
        char* shfree_errmsg = NULL;
        shfree(voidstar, &shfree_errmsg);
        free(shfree_errmsg);
    }
    free_schema(schema);
    return NULL;
}

static PyObject* pybinding__mlc_save_voidstar(PyObject* self, PyObject* args) { MAYFAIL
    PyObject* obj;
    const char* schema_str;
    long long level_ll;
    const char* path;
    Schema* schema = NULL;
    void* voidstar = NULL;

    // Args: (value, schema, level, path). level is the zstd preset
    // (0 = uncompressed, 1-9 = increasing ratio).
    if (!PyArg_ParseTuple(args, "OsLs", &obj, &schema_str, &level_ll, &path)) {
        PyRAISE("Failed to parse arguments");
    }
    uint8_t level = (uint8_t)level_ll;

    schema = PyTRY(parse_schema, schema_str);

    voidstar = to_voidstar(schema, obj);
    PyTRACE(voidstar == NULL)

    PyTRY(mlc_save_voidstar, voidstar, schema, level, path);

    {
        char* shfree_errmsg = NULL;
        shfree(voidstar, &shfree_errmsg);
        free(shfree_errmsg);
    }
    free_schema(schema);
    Py_RETURN_NONE;

error:
    if (voidstar) {
        char* shfree_errmsg = NULL;
        shfree(voidstar, &shfree_errmsg);
        free(shfree_errmsg);
    }
    free_schema(schema);
    return NULL;
}

static PyObject* pybinding__mlc_save_json(PyObject* self, PyObject* args) { MAYFAIL
    PyObject* obj;
    const char* schema_str;
    long long level_ll;
    const char* path;
    Schema* schema = NULL;
    void* voidstar = NULL;

    // Args: (value, schema, level, path). level accepted for ABI uniformity.
    if (!PyArg_ParseTuple(args, "OsLs", &obj, &schema_str, &level_ll, &path)) {
        PyRAISE("Failed to parse arguments");
    }
    uint8_t level = (uint8_t)level_ll;

    schema = PyTRY(parse_schema, schema_str);

    voidstar = to_voidstar(schema, obj);
    PyTRACE(voidstar == NULL)

    PyTRY(mlc_save_json, voidstar, schema, level, path);

    {
        char* shfree_errmsg = NULL;
        shfree(voidstar, &shfree_errmsg);
        free(shfree_errmsg);
    }
    free_schema(schema);
    Py_RETURN_NONE;

error:
    if (voidstar) {
        char* shfree_errmsg = NULL;
        shfree(voidstar, &shfree_errmsg);
        free(shfree_errmsg);
    }
    free_schema(schema);
    return NULL;
}

static PyObject* pybinding__mlc_show(PyObject* self, PyObject* args) { MAYFAIL
    PyObject* obj;
    const char* schema_str;
    Schema* schema = NULL;
    void* voidstar = NULL;
    char* json = NULL;

    if (!PyArg_ParseTuple(args, "Os", &obj, &schema_str)) {
        PyRAISE("Failed to parse arguments");
    }

    schema = PyTRY(parse_schema, schema_str);

    voidstar = to_voidstar(schema, obj);
    PyTRACE(voidstar == NULL)

    json = PyTRY(mlc_show, voidstar, schema);

    {
        char* shfree_errmsg = NULL;
        shfree(voidstar, &shfree_errmsg);
        free(shfree_errmsg);
    }
    free_schema(schema);

    {
        PyObject* retval = PyUnicode_FromString(json);
        free(json);
        return retval;
    }

error:
    if (voidstar) {
        char* shfree_errmsg = NULL;
        shfree(voidstar, &shfree_errmsg);
        free(shfree_errmsg);
    }
    free_schema(schema);
    FREE(json)
    return NULL;
}

static PyObject* pybinding__mlc_read(PyObject* self, PyObject* args) { MAYFAIL
    const char* schema_str;
    const char* json_str;
    Schema* schema = NULL;
    void* voidstar = NULL;

    if (!PyArg_ParseTuple(args, "ss", &schema_str, &json_str)) {
        PyRAISE("Failed to parse arguments");
    }

    schema = PyTRY(parse_schema, schema_str);

    {
        char* errmsg = NULL;
        voidstar = mlc_read(json_str, schema, &errmsg);
        if (errmsg != NULL) {
            free(errmsg);
        }
    }

    if (voidstar == NULL) {
        free_schema(schema);
        Py_RETURN_NONE;
    }

    {
        // The numpy fast-path in fromAnything (base_ptr == NULL) returns a
        // PyArray view of the SHM block via PyArray_SimpleNewFromData -- the
        // backing memory must outlive the view. Defer the shfree via
        // shm_tracker so the dispatch's flush releases the block (and the
        // schema) once the view is no longer in scope.
        PyObject* obj = fromAnything(schema, voidstar, NULL);
        if (obj == NULL) {
            char* shfree_errmsg = NULL;
            shfree(voidstar, &shfree_errmsg);
            free(shfree_errmsg);
            free_schema(schema);
            return NULL;
        }
        shm_tracker_push((absptr_t)voidstar, schema);
        return obj;
    }

error:
    if (voidstar) {
        char* shfree_errmsg = NULL;
        shfree(voidstar, &shfree_errmsg);
        free(shfree_errmsg);
    }
    free_schema(schema);
    return NULL;
}

static PyObject* pybinding__mlc_load(PyObject* self, PyObject* args) { MAYFAIL
    const char* schema_str;
    const char* path;
    Schema* schema = NULL;
    void* voidstar = NULL;

    if (!PyArg_ParseTuple(args, "ss", &schema_str, &path)) {
        PyRAISE("Failed to parse arguments");
    }

    schema = PyTRY(parse_schema, schema_str);

    voidstar = PyTRY(mlc_load, path, schema);

    if (voidstar == NULL) {
        free_schema(schema);
        Py_RETURN_NONE;
    }

    {
        // The numpy fast-path in fromAnything (base_ptr == NULL) returns a
        // PyArray view of the SHM block via PyArray_SimpleNewFromData -- the
        // backing memory must outlive the view. Defer the shfree via
        // shm_tracker so the dispatch's flush releases the block (and the
        // schema) once the view is no longer in scope.
        PyObject* obj = fromAnything(schema, voidstar, NULL);
        if (obj == NULL) {
            char* shfree_errmsg = NULL;
            shfree(voidstar, &shfree_errmsg);
            free(shfree_errmsg);
            free_schema(schema);
            return NULL;
        }
        shm_tracker_push((absptr_t)voidstar, schema);
        return obj;
    }

error:
    if (voidstar) {
        char* shfree_errmsg = NULL;
        shfree(voidstar, &shfree_errmsg);
        free(shfree_errmsg);
    }
    free_schema(schema);
    return NULL;
}

static PyMethodDef Methods[] = {
    {"log_next_id", pybinding__log_next_id, METH_NOARGS, "Allocate a fresh log call id"},
    {"log_emit", pybinding__log_emit, METH_VARARGS, "Emit a formatted log line via libmorloc"},
    {"pool_hash", pybinding__pool_hash, METH_NOARGS, "Return the pool's source fingerprint"},
    {"cache_path", pybinding__cache_path, METH_VARARGS, "Resolve per-label cache directory"},
    {"cache_lookup", pybinding__cache_lookup, METH_VARARGS, "Cache lookup; returns bytes or None"},
    {"cache_store", pybinding__cache_store, METH_VARARGS, "Atomically store packet bytes in cache"},
    {"cache_key_compute", pybinding__cache_key_compute, METH_VARARGS, "Compute cache key from midx + per-arg bytes"},
    {"cache_record_hit", pybinding__cache_record_hit, METH_NOARGS, "Increment the cache hit counter"},
    {"cache_record_miss", pybinding__cache_record_miss, METH_NOARGS, "Increment the cache miss counter"},
    {"cache_record_store", pybinding__cache_record_store, METH_NOARGS, "Increment the cache store counter"},
    {"set_fallback_dir", pybinding__set_fallback_dir, METH_VARARGS, "Set fallback directory for file-backed shared memory"},
    {"shinit", pybinding__shinit, METH_VARARGS, "Open the shared memory pool"},
    {"start_daemon", pybinding__start_daemon, METH_VARARGS, "Initialize the shared memory and socket for the python daemon"},
    {"close_daemon", pybinding__close_daemon, METH_VARARGS, "Banish the daemon back to the abyss from whence it came"},
    {"wait_for_client", pybinding__wait_for_client, METH_VARARGS, "Listen over a pipe until a client packet arrives"},
    {"read_morloc_call_packet", pybinding__read_morloc_call_packet, METH_VARARGS, "Parse a morloc call packet"},
    {"send_packet_to_foreign_server", pybinding__send_packet_to_foreign_server, METH_VARARGS, "Send data to a foreign server"},
    {"stream_from_client", pybinding__stream_from_client, METH_VARARGS, "Stream data from the client"},
    {"close_socket", pybinding__close_socket, METH_VARARGS, "Close the socket"},
    {"flush_shm_tracker", pybinding__flush_shm_tracker, METH_NOARGS, "Free tracked SHM allocations from put_value calls"},
    {"release_packet_shm", pybinding__release_packet_shm, METH_VARARGS, "Release the SHM ref owned by a put_value-produced packet"},
    {"debug_record_frame", pybinding__debug_record_frame, METH_VARARGS, "Append a manifold's args to the debug-trace stack"},
    {"debug_drain_frames", pybinding__debug_drain_frames, METH_NOARGS, "Drain the debug-trace stack and return as a string, or None"},
    {"debug_flush_dispatch", pybinding__debug_flush_dispatch, METH_NOARGS, "Reset per-dispatch debug state (recursion counters etc.)"},
    {"foreign_call", pybinding__foreign_call, METH_VARARGS, "Send a call packet to a foreign pool"},
    {"get_value", pybinding__get_value, METH_VARARGS, "Convert a packet to a Python value"},
    {"put_value", pybinding__put_value, METH_VARARGS, "Convert a Python value to a packet"},
    {"is_ping", pybinding__is_ping, METH_VARARGS, "Packet is a ping"},
    {"is_local_call", pybinding__is_local_call, METH_VARARGS, "Packet is a local call"},
    {"is_remote_call", pybinding__is_remote_call, METH_VARARGS, "Packet is a remote call"},
    {"pong", pybinding__pong, METH_VARARGS, "Return a ping"},
    {"make_fail_packet", pybinding__make_fail_packetg, METH_VARARGS, "Create a fail packet from an error message"},
    {"remote_call", pybinding__remote_call, METH_VARARGS, "Make a call to a remote cluster"},
    {"mlc_hash", pybinding__mlc_hash, METH_VARARGS, "Hash a value using xxhash"},
    {"mlc_save", pybinding__mlc_save, METH_VARARGS, "Save a value to file in msgpack format"},
    {"mlc_save_voidstar", pybinding__mlc_save_voidstar, METH_VARARGS, "Save a value to file in flat voidstar binary format"},
    {"mlc_save_json", pybinding__mlc_save_json, METH_VARARGS, "Save a value to file in JSON format"},
    {"mlc_load", pybinding__mlc_load, METH_VARARGS, "Load a value from file"},
    {"mlc_show", pybinding__mlc_show, METH_VARARGS, "Serialize a value to JSON string"},
    {"mlc_read", pybinding__mlc_read, METH_VARARGS, "Deserialize a JSON string to a value"},
    {NULL, NULL, 0, NULL} // this is a sentinel value
};

static struct PyModuleDef pymorloc = {
    PyModuleDef_HEAD_INIT,
    "pymorloc",
    "Python interface to Morloc binary and MessagePack data",
    -1,
    Methods
};

PyMODINIT_FUNC PyInit_pymorloc(void) {
    return PyModule_Create(&pymorloc);
}
