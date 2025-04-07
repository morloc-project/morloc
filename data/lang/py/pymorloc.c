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

#define NOTHING

#define MAYFAIL \
    char* child_errmsg_ = NULL; \
    void* return_value_ = NULL;

#define MAYFAIL_VOID \
    char* child_errmsg_ = NULL;

#define MAYFAIL_WITH(type, value) \
    char* child_errmsg_ = NULL; \
    type return_value_ = value;


#define PyTRY(fun, ...) \
    fun(__VA_ARGS__ __VA_OPT__(,) &child_errmsg_); \
    if(child_errmsg_ != NULL){ \
        PyErr_Format(PyExc_RuntimeError, "Error (%s:%d in %s):\n%s", __FILE__, __LINE__, __func__, child_errmsg_); \
        return return_value_; \
    }

#define PyTRY_VOID(fun, ...) \
    fun(__VA_ARGS__ __VA_OPT__(,) &child_errmsg_); \
    if(child_errmsg_ != NULL){ \
        PyErr_Format(PyExc_RuntimeError, "Error (%s:%d in %s):\n%s", __FILE__, __LINE__, __func__, child_errmsg_); \
        return; \
    }

# define PyTRY_WITH(clean, fun, ...) \
    fun(__VA_ARGS__ __VA_OPT__(,) &child_errmsg_); \
    if(child_errmsg_ != NULL){ \
        clean; \
        PyErr_Format(PyExc_RuntimeError, "Error (%s:%d in %s):\n%s", __FILE__, __LINE__, __func__, child_errmsg_); \
        return return_value_; \
    }


PyObject* numpy_module = NULL;


// This function will be called to import numpy if, and only if, a numpy feature
// is used. This avoids the agonizingly long numpy import time (and fuck you
// very much if you think 100ms is agonizingly long).
void* import_numpy() {
    numpy_module = PyImport_ImportModule("numpy");
    if (numpy_module == NULL) {
        PyErr_SetString(PyExc_ImportError, "NumPy is not available");
        return NULL;
    }

    import_array();
}



PyObject* fromAnything(const Schema* schema, const void* data){
    MAYFAIL

    PyObject* obj = NULL;
    switch (schema->type) {
        case MORLOC_NIL:
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
        case MORLOC_STRING: {
            Array* str_array = (Array*)data;
            absptr_t tmp_ptr = PyTRY(rel2abs, str_array->data);
            obj = PyUnicode_FromStringAndSize(tmp_ptr, str_array->size);
            if (!obj) {
                fprintf(stderr, "Failed to extract string from voidstar\n");
                PyErr_SetString(PyExc_TypeError, "Failed to parse data");
                goto error;
            }
            break;
        }
        case MORLOC_ARRAY: {
            Array* array = (Array*)data;
            if (schema->hint != NULL && strcmp(schema->hint, "numpy.ndarray") == 0) {
                import_numpy();
                Schema* element_schema = schema->parameters[0];
                npy_intp dims[] = {array->size};
                int nd = 1; // number of dimensions
                int type_num;
                // Determine the NumPy type number based on the element schema
                switch (element_schema->type) {
                    case MORLOC_BOOL:    type_num = NPY_BOOL; break;
                    case MORLOC_SINT8:   type_num = NPY_INT8; break;
                    case MORLOC_SINT16:  type_num = NPY_INT16; break;
                    case MORLOC_SINT32:  type_num = NPY_INT32; break;
                    case MORLOC_SINT64:  type_num = NPY_INT64; break;
                    case MORLOC_UINT8:   type_num = NPY_UINT8; break;
                    case MORLOC_UINT16:  type_num = NPY_UINT16; break;
                    case MORLOC_UINT32:  type_num = NPY_UINT32; break;
                    case MORLOC_UINT64:  type_num = NPY_UINT64; break;
                    case MORLOC_FLOAT32: type_num = NPY_FLOAT32; break;
                    case MORLOC_FLOAT64: type_num = NPY_FLOAT64; break;
                    default:
                        PyErr_SetString(PyExc_TypeError, "Unsupported element type for NumPy array");
                        goto error;
                }

                void* absptr = PyTRY(rel2abs, array->data);

                // Create the NumPy array
                obj = PyArray_SimpleNewFromData(nd, dims, type_num, absptr);

                if (!obj) {
                    PyErr_SetString(PyExc_TypeError, "Failed to parse data");
                    goto error;
                }

                // Note that we do not want to give ownership to Python
                // This is shared memory, which means, python should not mutate
                // it.

            } else if (schema->parameters[0]->type == MORLOC_UINT8) {
                // Create a Python bytes object for UINT8 arrays
                absptr_t tmp_ptr = PyTRY(rel2abs, array->data);
                obj = PyBytes_FromStringAndSize((const char*)tmp_ptr, array->size);
                if (!obj) {
                    PyErr_SetString(PyExc_TypeError, "Failed to one bytes");
                    goto error;
                }
            } else if (schema->hint == NULL || (schema->hint != NULL && strcmp(schema->hint, "list") == 0)) {
                // For other types, create a standard list
                obj = PyList_New(array->size);
                if (!obj) {
                    PyErr_SetString(PyExc_TypeError, "Failed to one string");
                    goto error;
                }
                char* start = (char*) PyTRY(rel2abs, array->data);
                size_t width = schema->parameters[0]->width;
                Schema* element_schema = schema->parameters[0];
                for (size_t i = 0; i < array->size; i++) {
                    PyObject* item = fromAnything(element_schema, start + width * i);
                    if (!item || PyList_SetItem(obj, i, item) < 0) {
                        Py_XDECREF(item);
                        PyErr_SetString(PyExc_TypeError, "Failed to access element in list");
                        goto error;
                    }
                }
            } else {
                PyErr_SetString(PyExc_TypeError, "Unexpected array hint");
                goto error;
            }
            break;
        }
        case MORLOC_TUPLE: {
            obj = PyTuple_New(schema->size);
            if (!obj) {
                PyErr_SetString(PyExc_TypeError, "Failed in tuple");
                goto error;
            }
            for (size_t i = 0; i < schema->size; i++) {
                void* item_ptr = (char*)data + schema->offsets[i];
                PyObject* item = fromAnything(schema->parameters[i], item_ptr);
                if (!item || PyTuple_SetItem(obj, i, item) < 0) {
                    Py_XDECREF(item);
                    PyErr_SetString(PyExc_TypeError, "Failed to access tuple element");
                    goto error;
                }
            }
            break;
        }
        case MORLOC_MAP: {
            obj = PyDict_New();
            if (!obj) {
                PyErr_SetString(PyExc_TypeError, "Failed in map");
                goto error;
            }
            for (size_t i = 0; i < schema->size; i++) {
                void* item_ptr = (char*)data + schema->offsets[i];
                PyObject* value = fromAnything(schema->parameters[i], item_ptr);
                PyObject* key = PyUnicode_FromString(schema->keys[i]);
                if (!value || !key || PyDict_SetItem(obj, key, value) < 0) {
                    Py_XDECREF(value);
                    Py_XDECREF(key);
                    PyErr_SetString(PyExc_TypeError, "Failed to access map element");
                    goto error;
                }
                Py_DECREF(key);
                Py_DECREF(value);
            }
            break;
        }
        default:
            PyErr_SetString(PyExc_TypeError, "Unsupported schema type");
            goto error;
    }

    return obj;

error:
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



ssize_t get_shm_size(const Schema* schema, PyObject* obj) {
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
        case MORLOC_STRING:
        case MORLOC_ARRAY:
            if (schema->type == MORLOC_STRING && !(PyUnicode_Check(obj) || PyBytes_Check(obj))) {
                PyErr_Format(PyExc_TypeError, "Expected str or bytes for MORLOC_STRING, but got %s", Py_TYPE(obj)->tp_name);
                goto error;
            }
            if (schema->type == MORLOC_ARRAY && !(PyList_Check(obj) || PyBytes_Check(obj) || PyObject_HasAttrString(obj, "__array_interface__"))) {
                PyErr_Format(PyExc_TypeError, "Expected list for MORLOC_ARRAY, but got %s", Py_TYPE(obj)->tp_name);
                goto error;
            }

            {
                size_t required_size = sizeof(Array);

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
                        case MORLOC_STRING:
                        case MORLOC_ARRAY:
                        case MORLOC_TUPLE:
                        case MORLOC_MAP:
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
                    required_size += total_elements * PyArray_ITEMSIZE(arr);
                } else if (PyBytes_Check(obj)) {
                    required_size += (size_t)PyBytes_GET_SIZE(obj);
                } else if (PyUnicode_Check(obj)) {
                    required_size += (size_t)PyUnicode_GET_LENGTH(obj);
                } else {
                    PyErr_SetString(PyExc_TypeError, "Unsupported data type");
                    return -1;
                }

                return required_size;
            }

        case MORLOC_TUPLE:
            if (!PyTuple_Check(obj) && !PyList_Check(obj)) {
                PyErr_Format(PyExc_TypeError, "Expected tuple or list for MORLOC_TUPLE, but got %s", Py_TYPE(obj)->tp_name);
                goto error;
            }

            {
                Py_ssize_t size = PyTuple_Check(obj) ? PyTuple_Size(obj) : PyList_Size(obj);
                if ((size_t)size != schema->size) {
                    PyErr_SetString(PyExc_ValueError, "Tuple/List size mismatch");
                    goto error;
                }

                size_t required_size = 0;

                for (Py_ssize_t i = 0; i < size; ++i) {
                    PyObject* item = PyTuple_Check(obj) ? PyTuple_GetItem(obj, i) : PyList_GetItem(obj, i);
                    ssize_t element_size = get_shm_size(schema->parameters[i], item);
                    if(element_size != -1){
                        required_size += element_size;
                    } else {
                        return -1;
                    }
                }
                return required_size;
            }

        case MORLOC_MAP:
            if (!PyDict_Check(obj)) {
                PyErr_Format(PyExc_TypeError, "Expected dict for MORLOC_MAP, but got %s", Py_TYPE(obj)->tp_name);
                goto error;
            }

            {
                size_t required_size = 0;
                for (size_t i = 0; i < schema->size; ++i) {
                    PyObject* key = PyUnicode_FromString(schema->keys[i]);
                    PyObject* value = PyDict_GetItem(obj, key);
                    Py_DECREF(key);
                    if (value) {
                        ssize_t element_size = get_shm_size(schema->parameters[i], value);
                        if(element_size != -1){
                            required_size += element_size;
                        } else {
                            return -1;
                        }
                    }
                }
                return required_size;
            }

        default:
            PyErr_SetString(PyExc_TypeError, "Unsupported schema type");
            goto error;
    }

    PyErr_SetString(PyExc_TypeError, "Impossible error");

error:
    return -1;
}



int to_voidstar_r(void* dest, void** cursor, const Schema* schema, PyObject* obj) {
    MAYFAIL_WITH(int, -1)
    switch (schema->type) {
        case MORLOC_NIL:
            /* if (obj != Py_None) {                                                                                 */
            /*     PyErr_Format(PyExc_TypeError, "Expected None for MORLOC_NIL, but got %s", Py_TYPE(obj)->tp_name); */
            /*     goto error;                                                                                       */
            /* }                                                                                                     */
            *((int8_t*)dest) = (int8_t)0;
            break;

        case MORLOC_BOOL:
            /* if (!PyBool_Check(obj)) {                                                                              */
            /*     PyErr_Format(PyExc_TypeError, "Expected bool for MORLOC_BOOL, but got %s", Py_TYPE(obj)->tp_name); */
            /*     goto error;                                                                                        */
            /* }                                                                                                      */
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
            /* if (!PyFloat_Check(obj)) {                                                                                 */
            /*     PyErr_Format(PyExc_TypeError, "Expected float for MORLOC_FLOAT32, but got %s", Py_TYPE(obj)->tp_name); */
            /*     goto error;                                                                                            */
            /* }                                                                                                          */
            *((float*)dest) = (float)PyFloat_AsDouble(obj);
            break;

        case MORLOC_FLOAT64:
            if(PyFloat_Check(obj))
            {
                *((double*)dest) = PyFloat_AsDouble(obj);
            } else if(PyLong_Check(obj)){
                *((double*)dest) = (double)PyLong_AsLongLong(obj);
            } else {
                PyErr_Format(PyExc_TypeError, "Expected float or int for MORLOC_FLOAT64, but got %s", Py_TYPE(obj)->tp_name);
                goto error;
            }
            break;

        case MORLOC_STRING:
        case MORLOC_ARRAY:
            if (schema->type == MORLOC_STRING && !(PyUnicode_Check(obj) || PyBytes_Check(obj))) {
                PyErr_Format(PyExc_TypeError, "Expected str or bytes for MORLOC_STRING, but got %s", Py_TYPE(obj)->tp_name);
                goto error;
            }

            if (schema->type == MORLOC_ARRAY && !(PyList_Check(obj) || PyBytes_Check(obj) || PyObject_HasAttrString(obj, "__array_interface__"))) { 
                PyErr_Format(PyExc_TypeError, "Expected list for MORLOC_ARRAY, but got %s", Py_TYPE(obj)->tp_name);
                goto error;
            }

            {
                Py_ssize_t size;
                char* data = NULL; // Initialize data to NULL

                if (PyList_Check(obj)) {
                    size = PyList_Size(obj);
                } else if (PyBytes_Check(obj)) {
                    PyBytes_AsStringAndSize(obj, &data, &size);
                } else if (schema->type == MORLOC_ARRAY && PyObject_HasAttrString(obj, "__array_interface__")) { // check if it is a numpy array
                    import_numpy();
                    PyArrayObject* arr = (PyArrayObject*)obj;
                    size = PyArray_SIZE(arr);
                    data = (char*)PyArray_DATA(arr); // Get the data pointer

                    // Verify that the array is contiguous
                    if (!PyArray_ISCONTIGUOUS(arr)) {
                        PyErr_SetString(PyExc_ValueError, "NumPy array must be contiguous");
                        goto error;
                    }
                } else {
                    data = PyUnicode_AsUTF8AndSize(obj, &size);
                }

                Array* result = (Array*)dest;
                result->size = (size_t)size;
                result->data = PyTRY(abs2rel, *cursor);

                if (PyList_Check(obj)) {
                    // Fixed size width of each element (variable size data will
                    // be written to the cursor location)
                    size_t width = schema->parameters[0]->width;

                    // Move the cursor to the location immediately after the
                    // fixed sized elements
                    *cursor = (void*)(*(char**)cursor + size * width);

                    char* start = (char*) PyTRY(rel2abs, result->data);
                    Schema* element_schema = schema->parameters[0];
                    for (Py_ssize_t i = 0; i < size; i++) {
                        PyObject* item = PyList_GetItem(obj, i);
                        to_voidstar_r(start + width * i, cursor, element_schema, item);
                    }
                } else if (PyBytes_Check(obj)){
                    absptr_t tmp_ptr = PyTRY(rel2abs, result->data);
                    memcpy(tmp_ptr, data, size);

                    // move cursor to the location after the copied data
                    *cursor = (void*)(*(char**)cursor + size);
                }
                else{
                    size_t width = schema->parameters[0]->width;

                    absptr_t tmp_ptr = PyTRY(rel2abs, result->data);
                    memcpy(tmp_ptr, data, size * width);

                    // Move the cursor to the location immediately after the
                    // fixed sized elements
                    *cursor = (void*)(*(char**)cursor + size * width);
                }
            }
            break;

        case MORLOC_TUPLE:
            if (!PyTuple_Check(obj) && !PyList_Check(obj)) {
                PyErr_Format(PyExc_TypeError, "Expected tuple or list for MORLOC_TUPLE, but got %s", Py_TYPE(obj)->tp_name);
                goto error;
            }

            {
                Py_ssize_t size = PyTuple_Check(obj) ? PyTuple_Size(obj) : PyList_Size(obj);
                if ((size_t)size != schema->size) {
                    PyErr_SetString(PyExc_ValueError, "Tuple/List size mismatch");
                    goto error;
                }
                for (Py_ssize_t i = 0; i < size; ++i) {
                    PyObject* item = PyTuple_Check(obj) ? PyTuple_GetItem(obj, i) : PyList_GetItem(obj, i);
                    to_voidstar_r((char*)dest + schema->offsets[i], cursor, schema->parameters[i], item);
                }
            }
            break;

        case MORLOC_MAP:
            if (!PyDict_Check(obj)) {
                PyErr_Format(PyExc_TypeError, "Expected dict for MORLOC_MAP, but got %s", Py_TYPE(obj)->tp_name);
                goto error;
            }

            {
                for (size_t i = 0; i < schema->size; ++i) {
                    PyObject* key = PyUnicode_FromString(schema->keys[i]);
                    PyObject* value = PyDict_GetItem(obj, key);
                    Py_DECREF(key);
                    if (value) {
                        to_voidstar_r((char*)dest + schema->offsets[i], cursor, schema->parameters[i], value);
                    }
                }
            }
            break;

        default:
            PyErr_SetString(PyExc_TypeError, "Unsupported schema type");
            goto error;
    }

    return 0;

error:
    return -1;
}

void* to_voidstar(const Schema* schema, PyObject* obj){ MAYFAIL
  // calculate the required size of the shared memory object
  ssize_t shm_size = get_shm_size(schema, obj);
  if(shm_size == -1){
      PyErr_SetString(PyExc_TypeError, "Schema does not match object");
      return NULL;
  }

  // allocate the required memory as a single block
  void* dest = PyTRY(shmalloc, (size_t)shm_size);

  // set the write location of variable size chunks
  void* cursor = (void*)((char*)dest + schema->width);

  // write the data to the block
  int result = to_voidstar_r(dest, &cursor, schema, obj);

  if(result == 0){
      return dest;
  } else {
      PyErr_SetString(PyExc_TypeError, "Failed to write data to shared memory pool");
      return NULL;
  }
}


static PyObject* pybinding__wait_for_client(PyObject* self, PyObject* args) { MAYFAIL
    PyObject* daemon_capsule;

    if (!PyArg_ParseTuple(args, "O", &daemon_capsule)) {
        PyErr_SetString(PyExc_TypeError, "Failed to parse arguments");
        return NULL;
    }

    language_daemon_t* daemon = (language_daemon_t*)PyCapsule_GetPointer(daemon_capsule, "language_daemon_t");

    int client_fd = PyTRY(wait_for_client, daemon);

    return PyLong_FromLong((long)client_fd);
}

static PyObject* pybinding__start_daemon(PyObject* self, PyObject* args) { MAYFAIL
    const char* socket_path;
    const char* tmpdir;
    const char* shm_basename;
    size_t shm_default_size;
 
    if (!PyArg_ParseTuple(args, "sssk", &socket_path, &tmpdir, &shm_basename, &shm_default_size)) {
      return NULL;
    }
 
    language_daemon_t* daemon = PyTRY(
        start_daemon,
        socket_path,
        tmpdir,
        shm_basename,
        shm_default_size
    );
 
    return PyCapsule_New(daemon, "language_daemon_t", NULL);
}


static PyObject* pybinding__close_daemon(PyObject* self, PyObject* args) {
    PyObject* daemon_capsule;

    if (!PyArg_ParseTuple(args, "O", &daemon_capsule)) {
        PyErr_SetString(PyExc_TypeError, "Failed to parse arguments");
        return NULL;
    }

    language_daemon_t* daemon = (language_daemon_t*)PyCapsule_GetPointer(daemon_capsule, "language_daemon_t");

    close_daemon(daemon);

    Py_RETURN_NONE;
}


static PyObject*  pybinding__read_morloc_call_packet(PyObject* self, PyObject* args){ MAYFAIL
    char* packet;
    size_t packet_size;
  
    if (!PyArg_ParseTuple(args, "y#", &packet, &packet_size)) {
        PyErr_SetString(PyExc_TypeError, "Failed to parse arguments");
        return NULL;
    }
    morloc_call_t* call_packet = PyTRY(read_morloc_call_packet, packet);

    PyObject* py_tuple = PyTuple_New(2);
    PyObject* py_args = PyList_New(call_packet->nargs);
    PyObject* py_mid = PyLong_FromLong((long)call_packet->midx);
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

    return py_tuple;
}

static PyObject*  pybinding__send_packet_to_foreign_server(PyObject* self, PyObject* args){ MAYFAIL
    int client_fd = 0;
    uint8_t* packet = NULL;
    size_t packet_size = 0;

    if (!PyArg_ParseTuple(args, "iy#", &client_fd, &packet, &packet_size)) {
        PyErr_SetString(PyExc_TypeError, "Failed to parse arguments");
        return NULL;
    }

    size_t bytes_sent = PyTRY(send_packet_to_foreign_server, client_fd, packet);

    return PyLong_FromSize_t(bytes_sent);
}


static PyObject*  pybinding__stream_from_client(PyObject* self, PyObject* args){ MAYFAIL
    int client_fd = 0;

    if (!PyArg_ParseTuple(args, "i", &client_fd)) {
        PyErr_SetString(PyExc_TypeError, "Failed to parse arguments");
        return NULL;
    }

    uint8_t* packet = PyTRY(stream_from_client, client_fd);

    size_t packet_size = PyTRY(morloc_packet_size, packet);

    return PyBytes_FromStringAndSize((char*)packet, packet_size);
}



static PyObject*  pybinding__socket_close(PyObject* self, PyObject* args){
    int socket_id = 0;

    if (!PyArg_ParseTuple(args, "i", &socket_id)) {
        PyErr_SetString(PyExc_TypeError, "Failed to parse arguments");
        return NULL;
    }

    socket_close(socket_id);

    Py_RETURN_NONE;
}

// Transforms a value into a message ready for the socket
static PyObject* pybinding__put_value(PyObject* self, PyObject* args){ MAYFAIL
    PyObject* obj;
    const char* schema_str;
  
    if (!PyArg_ParseTuple(args, "Os", &obj, &schema_str)) {
        return NULL;
    }
  
    Schema* schema = PyTRY(parse_schema, &schema_str);
  
    void* voidstar = to_voidstar(schema, obj);
  
    if(!voidstar){
        return NULL;
    }

    // convert to a relative pointer conserved between language servers
    relptr_t relptr = PyTRY(abs2rel, voidstar);

    uint8_t* packet = make_relptr_data_packet(relptr);

    size_t packet_size = PyTRY(morloc_packet_size, packet);

    return PyBytes_FromStringAndSize((char*)packet, packet_size);
}


// Use a key to retrieve a value
static PyObject* pybinding__get_value(PyObject* self, PyObject* args){ MAYFAIL
    const char* packet;
    size_t packet_size;
    const char* schema_str;
  
    if (!PyArg_ParseTuple(args, "y#s", &packet, &packet_size, &schema_str)) {
        return NULL;
    }

    Schema* schema = PyTRY(parse_schema, &schema_str)

    uint8_t* voidstar = PyTRY(get_morloc_data_packet_value, (uint8_t*)packet, schema);

    PyObject* obj = fromAnything(schema, voidstar);
    if (obj == NULL) {
        free_schema(schema);
        return NULL;
    }

    free_schema(schema);

    return obj;
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
    
    // Parse arguments: string, integer, and sequence
    if (!PyArg_ParseTuple(args, "siO", &socket_path, &mid, &py_args)) {
        return NULL;
    }

    // Verify third argument is a sequence
    if (!PySequence_Check(py_args)) {
        PyErr_SetString(PyExc_TypeError, "Third argument must be a sequence");
        return NULL;
    }

    // Get sequence size and allocate C arrays
    nargs = PySequence_Size(py_args);
    arg_packets = (const uint8_t**)calloc(nargs, sizeof(uint8_t*));
    if (!arg_packets) {
        free(arg_packets);
        PyErr_NoMemory();
        return NULL;
    }

    // Convert Python bytes to C buffers
    for (i = 0; i < nargs; i++) {
        PyObject* item = PySequence_GetItem(py_args, i);
        if (!PyBytes_Check(item)) {
            Py_DECREF(item);
            free(arg_packets);
            PyErr_SetString(PyExc_TypeError, "All arguments must be bytes objects");
            return NULL;
        }
        arg_packets[i] = (const uint8_t*)PyBytes_AsString(item);
        Py_DECREF(item);
    }

    uint8_t* packet = PyTRY_WITH(free(arg_packets), make_morloc_call_packet, (uint32_t)mid, arg_packets, (size_t)nargs);

    free(arg_packets);
    
    uint8_t* result = PyTRY_WITH(free(packet), send_and_receive_over_socket, socket_path, packet);
    free(packet);

    size_t result_length = PyTRY(morloc_packet_size, result);

    return PyBytes_FromStringAndSize((char*)result, result_length);
}


static PyObject* pybinding__is_ping(PyObject* self, PyObject* args) { MAYFAIL
    char* packet;
    size_t packet_size;
  
    if (!PyArg_ParseTuple(args, "y#", &packet, &packet_size)) {
        PyErr_SetString(PyExc_TypeError, "Failed to parse arguments");
        return NULL;
    }

    bool is_ping = PyTRY(packet_is_ping, (uint8_t*)packet);

    PyObject* obj = PyBool_FromLong((long)is_ping);
    
    return obj;
}


static PyObject* pybinding__is_call(PyObject* self, PyObject* args) { MAYFAIL
    char* packet;
    size_t packet_size;
  
    if (!PyArg_ParseTuple(args, "y#", &packet, &packet_size)) {
        PyErr_SetString(PyExc_TypeError, "Failed to parse arguments");
        return NULL;
    }

    bool is_call = PyTRY(packet_is_call, (uint8_t*)packet);

    PyObject* obj = PyBool_FromLong((long)is_call);
    
    return obj;
}


static PyObject* pybinding__pong(PyObject* self, PyObject* args) { MAYFAIL
    char* packet;
    size_t packet_size;
  
    if (!PyArg_ParseTuple(args, "y#", &packet, &packet_size)) {
        PyErr_SetString(PyExc_TypeError, "Failed to parse arguments");
        return NULL;
    }

    const uint8_t* pong = PyTRY(return_ping, (uint8_t*)packet);

    size_t pong_size = PyTRY(morloc_packet_size, pong);

    return PyBytes_FromStringAndSize((char*)pong, pong_size);
}


static PyMethodDef Methods[] = {
    {"start_daemon", pybinding__start_daemon, METH_VARARGS, "Initialize the shared memory and socket for the python daemon"},
    {"close_daemon", pybinding__close_daemon, METH_VARARGS, "Banish the daemon back to the abyss from whence it came"},
    {"wait_for_client", pybinding__wait_for_client, METH_VARARGS, "Listen over a pipe until a client packet arrives"},
    {"read_morloc_call_packet", pybinding__read_morloc_call_packet, METH_VARARGS, "Parse a morloc call packet"},
    {"send_packet_to_foreign_server", pybinding__send_packet_to_foreign_server, METH_VARARGS, "Send data to a foreign server"},
    {"stream_from_client", pybinding__stream_from_client, METH_VARARGS, "Stream data from the client"},
    {"socket_close", pybinding__socket_close, METH_VARARGS, "Close a fucking socket"},
    {"foreign_call", pybinding__foreign_call, METH_VARARGS, "Close a fucking socket"},
    {"get_value", pybinding__get_value, METH_VARARGS, "Convert a packet to a Python value"},
    {"put_value", pybinding__put_value, METH_VARARGS, "Convert a Python value to a packet"},
    {"is_ping", pybinding__is_ping, METH_VARARGS, "Packet is a ping"},
    {"is_call", pybinding__is_call, METH_VARARGS, "Packet is a call"},
    {"pong", pybinding__pong, METH_VARARGS, "Return a ping"},
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
