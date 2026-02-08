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

const char* get_prior_err(){
    const char* prior_err = NULL;
    if (PyErr_Occurred()) {
        // Fetch existing exception
        PyObject *type, *value, *traceback;
        PyErr_Fetch(&type, &value, &traceback);

        // Extract error message
        PyObject* str = PyObject_Str(value);  // Convert exception to string
        prior_err = PyUnicode_AsUTF8(str);

        // Cleanup
        Py_XDECREF(str);
        Py_XDECREF(type);
        Py_XDECREF(value);
        Py_XDECREF(traceback);
    }
    return prior_err;
}


#define PyTRY(fun, ...) \
    fun(__VA_ARGS__ __VA_OPT__(,) &child_errmsg_); \
    if(child_errmsg_ != NULL){ \
        const char* prior_err = get_prior_err(); \
        if(prior_err == NULL){ \
            PyErr_Format(PyExc_RuntimeError, "Error (%s:%d in %s):\n%s", __FILE__, __LINE__, __func__, child_errmsg_); \
        } else { \
            PyErr_Format(PyExc_RuntimeError, "%s\nError (%s:%d in %s):\n%s", prior_err, __FILE__, __LINE__, __func__, child_errmsg_); \
        } \
        goto error; \
    }

#define PyRAISE(msg, ...) \
    const char* prior_err = get_prior_err(); \
    if(prior_err == NULL){ \
        PyErr_Format(PyExc_RuntimeError, "Error (%s:%d in %s):\n" msg "\n", __FILE__, __LINE__, __func__, ##__VA_ARGS__); \
    } else { \
        PyErr_Format(PyExc_RuntimeError, "%s\nError (%s:%d in %s):\n" msg "\n", prior_err, __FILE__, __LINE__, __func__, ##__VA_ARGS__); \
    } \
    goto error;

#define PyTRACE(cond) \
    if(cond){ \
        const char* prior_err = get_prior_err(); \
        if(prior_err != NULL){ \
            PyErr_Format(PyExc_TypeError, "Error (%s:%d in %s):\n%s", __FILE__, __LINE__, __func__, prior_err); \
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



PyObject* fromAnything(const Schema* schema, const void* data){ MAYFAIL

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
            absptr_t tmp_ptr = NULL;
            
            if (str_array->size != 0) {
                tmp_ptr = PyTRY(rel2abs, str_array->data);
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
            if (schema->hint != NULL && strcmp(schema->hint, "numpy.ndarray") == 0) {
                import_numpy();
                Schema* element_schema = schema->parameters[0];
                npy_intp dims[] = {array->size};
                void* absptr = NULL;
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
                        PyRAISE("Unsupported element type for NumPy array");
                }

                absptr = PyTRY(rel2abs, array->data);

                // Create the NumPy array
                obj = PyArray_SimpleNewFromData(nd, dims, type_num, absptr);

                if(obj == NULL) {
                    PyRAISE("Failed to parse data");
                }

                // Note that we do not want to give ownership to Python
                // This is shared memory, which means, python should not mutate
                // it.

            } else if (schema->hint != NULL && strcmp(schema->hint, "bytearray") == 0) {
                // Create a Python bytearray object
                absptr_t absptr = PyTRY(rel2abs, array->data);
                obj = PyByteArray_FromStringAndSize((const char*)absptr, array->size);
                if (!obj) {
                    PyErr_SetString(PyExc_TypeError, "Failed to create bytearray");
                    goto error;
                }
                // Note: Similar to the numpy case, we don't want to give ownership to Python.
                // The bytearray is created from a copy of the data, so no additional handling is needed.
            } else if (schema->parameters[0]->type == MORLOC_UINT8) {
                // Create a Python bytes object for UINT8 arrays
                absptr_t tmp_ptr = PyTRY(rel2abs, array->data);
                obj = PyBytes_FromStringAndSize((const char*)tmp_ptr, array->size);
                if (obj == NULL) {
                    PyRAISE("Failed to one bytes")
                }
            } else if (schema->hint == NULL || (schema->hint != NULL && strcmp(schema->hint, "list") == 0)) {
                // For other types, create a standard list
                obj = PyList_New(array->size);
                if(obj == NULL){
                    PyRAISE("Failed to one string");
                }
                if(array->size > 0){
                    char* start = (char*) PyTRY(rel2abs, array->data);
                    size_t width = schema->parameters[0]->width;
                    Schema* element_schema = schema->parameters[0];
                    for (size_t i = 0; i < array->size; i++) {
                        PyObject* item = fromAnything(element_schema, start + width * i);
                        if (!item || PyList_SetItem(obj, i, item) < 0) {
                            Py_XDECREF(item);
                            PyRAISE("Failed to access element in list")
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
                PyObject* item = fromAnything(schema->parameters[i], item_ptr);
                if (!item || PyTuple_SetItem(obj, i, item) < 0) {
                    Py_XDECREF(item);
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
                PyObject* value = fromAnything(schema->parameters[i], item_ptr);
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
        default:
            PyRAISE("Unsupported schema type");
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
            if (schema->type == MORLOC_STRING && !(PyUnicode_Check(obj) || PyBytes_Check(obj) || PyByteArray_Check(obj) )) {
                PyRAISE("Expected str or bytes for MORLOC_STRING, but got %s", Py_TYPE(obj)->tp_name);
            }
            if (schema->type == MORLOC_ARRAY && !(PyList_Check(obj) || PyBytes_Check(obj) || PyByteArray_Check(obj) || PyObject_HasAttrString(obj, "__array_interface__"))) {
                PyRAISE("Expected list, bytes, bytearray, or numpy array for MORLOC_ARRAY, but got %s", Py_TYPE(obj)->tp_name);
            }
        
            {
                ssize_t required_size = 0;

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
                PyRAISE("Expected dict for MORLOC_MAP, but got %s", Py_TYPE(obj)->tp_name);
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
            PyRAISE("Unsupported schema type");
    }

    PyRAISE("Reached the unreachable");

error:
    return -1;
}



int to_voidstar_r(void* dest, void** cursor, const Schema* schema, PyObject* obj) { MAYFAIL
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
                    // This needs const data
                    immutable_data = PyArray_DATA(arr); // Get the data pointer

                    // Verify that the array is contiguous
                    if (!PyArray_ISCONTIGUOUS(arr)) {
                        PyRAISE("NumPy array must be contiguous");
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
                    to_voidstar_r((char*)dest + schema->offsets[i], cursor, schema->parameters[i], item);
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
                        to_voidstar_r((char*)dest + schema->offsets[i], cursor, schema->parameters[i], value);
                    }
                }
            }
            break;

        default:
            PyRAISE("Unsupported schema type");
    }

    return 0;

error:
    return -1;
}

void* to_voidstar(const Schema* schema, PyObject* obj){ MAYFAIL
  // calculate the required size of the shared memory object
  ssize_t shm_size = get_shm_size(schema, obj);
  if(shm_size == -1){
      PyRAISE("Schema does not match object");
  }

  // allocate the required memory as a single block
  void* dest = PyTRY(shmalloc, (size_t)shm_size);

  // set the write location of variable size chunks
  void* cursor = (void*)((char*)dest + schema->width);

  // write the data to the block
  int result = to_voidstar_r(dest, &cursor, schema, obj);
  PyTRACE(result != 0)

  return dest;

error:
  return NULL;
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

    if (!PyArg_ParseTuple(args, "y#", &packet, &packet_size)) {
        PyRAISE("Failed to parse arguments");
    }
    morloc_call_t* call_packet = PyTRY(read_morloc_call_packet, (const uint8_t*)packet);

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

error:
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

    if (!PyArg_ParseTuple(args, "i", &client_fd)) {
        PyRAISE("Failed to parse arguments");
    }

    uint8_t* packet = PyTRY(stream_from_client, client_fd);

    size_t packet_size = PyTRY(morloc_packet_size, packet);

    PyObject* retval = PyBytes_FromStringAndSize((char*)packet, packet_size);

    free(packet);

    return retval;

error:
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

    PyObject* obj;
    const char* schema_str;

    if (!PyArg_ParseTuple(args, "Os", &obj, &schema_str)) {
        PyRAISE("Failed to parse arguments");
    }

    schema = PyTRY(parse_schema, schema_str);

    voidstar = to_voidstar(schema, obj);
    PyTRACE(voidstar == NULL)

    // convert to a relative pointer conserved between language servers
    relptr_t relptr = PyTRY(abs2rel, voidstar);

    packet = make_standard_data_packet(relptr, schema);

    packet_size = PyTRY(morloc_packet_size, packet);

    return PyBytes_FromStringAndSize((char*)packet, packet_size);

error:
    FREE(packet)
    free_schema(schema);
    return NULL;
}


// Use a key to retrieve a value
static PyObject* pybinding__get_value(PyObject* self, PyObject* args){ MAYFAIL
    uint8_t* voidstar = NULL;
    Schema* schema = NULL;
    PyObject* obj = NULL;

    const char* packet;
    size_t packet_size;
    const char* schema_str;

    if (!PyArg_ParseTuple(args, "y#s", &packet, &packet_size, &schema_str)) {
        PyRAISE("Failed to parse arguments");
    }

    schema = PyTRY(parse_schema, schema_str)

    voidstar = PyTRY(get_morloc_data_packet_value, (uint8_t*)packet, schema);

    obj = fromAnything(schema, voidstar);
    PyTRACE(obj == NULL)

    free_schema(schema);

    return obj;

error:
    free_schema(schema);
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
        free(arg_packets);
        PyErr_NoMemory();
        goto error;
    }

    // Convert Python bytes to C buffers
    for (i = 0; i < nargs; i++) {
        PyObject* item = PySequence_GetItem(py_args, i);
        if (!PyBytes_Check(item)) {
            Py_DECREF(item);
            free(arg_packets);
            PyRAISE("All arguments must be bytes objects");
        }
        arg_packets[i] = (const uint8_t*)PyBytes_AsString(item);
        Py_DECREF(item);
    }

    packet = PyTRY(make_morloc_local_call_packet, (uint32_t)mid, arg_packets, (size_t)nargs);

    free(arg_packets);

    result = PyTRY(send_and_receive_over_socket, socket_path, packet);
    free(packet);

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
    PyObject* py_result = PyBytes_FromString((char*)result);
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

    if (!PyArg_ParseTuple(args, "y#", &packet, &packet_size)) {
        PyRAISE("Failed to parse arguments");
    }

    const uint8_t* pong = PyTRY(return_ping, (uint8_t*)packet);

    size_t pong_size = PyTRY(morloc_packet_size, pong);

    return PyBytes_FromStringAndSize((char*)pong, pong_size);

error:
    return NULL;
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

    if (!PyArg_ParseTuple(args, "s", &packet_errmsg)) {
        PyRAISE("Failed to parse arguments");
    }

    uint8_t* packet = make_fail_packet(packet_errmsg);

    size_t packet_size = PyTRY(morloc_packet_size, packet);

    return PyBytes_FromStringAndSize((char*)packet, packet_size);

error:
    return NULL;
}

static PyMethodDef Methods[] = {
    {"shinit", pybinding__shinit, METH_VARARGS, "Open the shared memory pool"},
    {"start_daemon", pybinding__start_daemon, METH_VARARGS, "Initialize the shared memory and socket for the python daemon"},
    {"close_daemon", pybinding__close_daemon, METH_VARARGS, "Banish the daemon back to the abyss from whence it came"},
    {"wait_for_client", pybinding__wait_for_client, METH_VARARGS, "Listen over a pipe until a client packet arrives"},
    {"read_morloc_call_packet", pybinding__read_morloc_call_packet, METH_VARARGS, "Parse a morloc call packet"},
    {"send_packet_to_foreign_server", pybinding__send_packet_to_foreign_server, METH_VARARGS, "Send data to a foreign server"},
    {"stream_from_client", pybinding__stream_from_client, METH_VARARGS, "Stream data from the client"},
    {"close_socket", pybinding__close_socket, METH_VARARGS, "Close the socket"},
    {"foreign_call", pybinding__foreign_call, METH_VARARGS, "Send a call packet to a foreign pool"},
    {"get_value", pybinding__get_value, METH_VARARGS, "Convert a packet to a Python value"},
    {"put_value", pybinding__put_value, METH_VARARGS, "Convert a Python value to a packet"},
    {"is_ping", pybinding__is_ping, METH_VARARGS, "Packet is a ping"},
    {"is_local_call", pybinding__is_local_call, METH_VARARGS, "Packet is a local call"},
    {"is_remote_call", pybinding__is_remote_call, METH_VARARGS, "Packet is a remote call"},
    {"pong", pybinding__pong, METH_VARARGS, "Return a ping"},
    {"make_fail_packet", pybinding__make_fail_packetg, METH_VARARGS, "Create a fail packet from an error message"},
    {"remote_call", pybinding__remote_call, METH_VARARGS, "Make a call to a remote cluster"},
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
