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



// Exported prototypes
static PyObject* to_mesgpack(PyObject* self, PyObject* args);
static PyObject* from_mesgpack(PyObject* self, PyObject* args);
static PyObject* to_voidstar(PyObject* self, PyObject* args);
static PyObject* from_voidstar(PyObject* self, PyObject* args);
static PyObject* py_to_mesgpack(PyObject* self, PyObject* args);
static PyObject* mesgpack_to_py(PyObject* self, PyObject* args);


// convert voidstar to MessagePack
static PyObject* to_mesgpack(PyObject* self, PyObject* args) {
    PyObject* voidstar_capsule;
    char* schema;
    char* msgpck_data = NULL;
    size_t msgpck_data_len = 0;

    if (!PyArg_ParseTuple(args, "Os", &voidstar_capsule, &schema)) {
        PyErr_SetString(PyExc_TypeError, "Failed to parse arguments");
        return NULL;
    }

    void* voidstar = PyCapsule_GetPointer(voidstar_capsule, "absptr_t");

    if (voidstar == NULL) {
        PyErr_SetString(PyExc_TypeError, "Invalid voidstar capsule");
        return NULL;
    }

    int exitcode = pack(voidstar, schema, &msgpck_data, &msgpck_data_len);

    if (exitcode != 0 || !msgpck_data) {
        PyErr_SetString(PyExc_RuntimeError, "Packing failed");
        return NULL;
    }

    // TODO: avoid memory copying here
    PyObject* mesgpack_bytes = PyBytes_FromStringAndSize(msgpck_data, msgpck_data_len);
    free(msgpck_data);

    return mesgpack_bytes;
}


// destroy the voidstar
static void voidstar_destructor(PyObject *capsule) {
    void *voidstar = PyCapsule_GetPointer(capsule, "absptr_t");
    if (voidstar) {
        shfree(voidstar);
    }
}

// convert MessagePack to voidstar
static PyObject* from_mesgpack(PyObject* self, PyObject* args) {
    const char* msgpck_data;
    Py_ssize_t msgpck_data_len;
    const char* schema;
    void* voidstar = NULL;

    if (!PyArg_ParseTuple(args, "y#s", &msgpck_data, &msgpck_data_len, &schema)) {
        return NULL;
    }

    int exitcode = unpack(msgpck_data, msgpck_data_len, schema, &voidstar);
    if (exitcode != 0) {
        PyErr_Format(PyExc_RuntimeError, "Unpacking failed with exit code %d", exitcode);
        return NULL;
    }

    PyObject* voidstar_capsule = PyCapsule_New(voidstar, "absptr_t", voidstar_destructor);
    if (!voidstar_capsule) {
        shfree(voidstar);  // Or use appropriate deallocation function
        return NULL;
    }

    return voidstar_capsule;
}


PyObject* fromAnything(const Schema* schema, const void* data){
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
            obj = PyUnicode_FromStringAndSize(rel2abs(str_array->data), str_array->size);
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

                void* absptr = rel2abs(array->data);

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
                obj = PyBytes_FromStringAndSize((const char*)rel2abs(array->data), array->size);
                if (!obj) {
                    PyErr_SetString(PyExc_TypeError, "Failed to one bytes");
                    goto error;
                }
            } else if (schema->hint != NULL && strcmp(schema->hint, "list") == 0) {
                // For other types, create a standard list
                obj = PyList_New(array->size);
                if (!obj) {
                    PyErr_SetString(PyExc_TypeError, "Failed to one string");
                    goto error;
                }
                char* start = (char*)rel2abs(array->data);
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


// convert voidstar to PyObject
static PyObject* from_voidstar(PyObject* self, PyObject* args) {
    PyObject* voidstar_capsule;
    const char* schema_str;

    if (!PyArg_ParseTuple(args, "Os", &voidstar_capsule, &schema_str)) {
        PyErr_SetString(PyExc_TypeError, "Failed to parse input");
        return NULL;
    }

    void* voidstar = PyCapsule_GetPointer(voidstar_capsule, "absptr_t");

    if (voidstar == NULL) {
        PyErr_SetString(PyExc_TypeError, "Invalid voidstar capsule");
        return NULL;
    }

    Schema* schema = parse_schema(&schema_str);
    if (!schema) {
        PyErr_SetString(PyExc_ValueError, "Failed to parse schema");
        return NULL;
    }

    PyObject* obj = fromAnything(schema, voidstar);
    if (obj == NULL) {
        free_schema(schema);
        PyErr_SetString(PyExc_TypeError, "fromAnything returned NULL");
        return NULL;
    }

    free_schema(schema);

    return obj;
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
                result->data = abs2rel(*cursor);

                if (PyList_Check(obj)) {
                    // Fixed size width of each element (variable size data will
                    // be written to the cursor location)
                    size_t width = schema->parameters[0]->width;

                    // Move the cursor to the location immediately after the
                    // fixed sized elements
                    *cursor = (void*)(*(char**)cursor + size * width);

                    char* start = (char*)rel2abs(result->data);
                    Schema* element_schema = schema->parameters[0];
                    for (Py_ssize_t i = 0; i < size; i++) {
                        PyObject* item = PyList_GetItem(obj, i);
                        to_voidstar_r(start + width * i, cursor, element_schema, item);
                    }
                } else if (PyBytes_Check(obj)){
                    memcpy(rel2abs(result->data), data, size);

                    // move cursor to the location after the copied data
                    *cursor = (void*)(*(char**)cursor + size);
                }
                else{
                    size_t width = schema->parameters[0]->width;

                    memcpy(rel2abs(result->data), data, size * width);

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

void* to_voidstar_c(const Schema* schema, PyObject* obj){
  // calculate the required size of the shared memory object
  ssize_t shm_size = get_shm_size(schema, obj);
  if(shm_size == -1){
      PyErr_SetString(PyExc_TypeError, "Schema does not match object");
      return NULL;
  }

  // allocate the required memory as a single block
  void* dest = shmalloc((size_t)shm_size);

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


// convert PyObject to voidstar
static PyObject* to_voidstar(PyObject* self, PyObject* args){
  PyObject* obj;
  const char* schema_str;

  if (!PyArg_ParseTuple(args, "Os", &obj, &schema_str)) {
      return NULL;
  }

  Schema* schema = parse_schema(&schema_str);

  void* voidstar = to_voidstar_c(schema, obj);

  if(!voidstar){
      return NULL;
  }

  // Note that there is no destructor. This memory is managed by the custom
  // memory allocator. I will eventually add some means of reference
  // counting. When I do, the destructor will decrement this count.
  PyObject* voidstar_capsule = PyCapsule_New(voidstar, "absptr_t", NULL);

  free_schema(schema);

  return voidstar_capsule;
}


static PyObject* py_to_mesgpack(PyObject* self, PyObject* args) {
  PyObject* obj;
  const char* schema_str;

  if (!PyArg_ParseTuple(args, "Os", &obj, &schema_str)) {
      PyErr_SetString(PyExc_ValueError, "py_to_mesgpack: Failed to parse arguments");
      return NULL;
  }

  Schema* schema = parse_schema(&schema_str);
  if (!schema) {
      PyErr_SetString(PyExc_ValueError, "py_to_mesgpack: Failed to parse schema");
      return NULL;
  }

  void* voidstar = to_voidstar_c(schema, obj);

  if (!voidstar && PyErr_Occurred()) {
      free_schema(schema);
      PyErr_SetString(PyExc_ValueError, "py_to_mesgpack: Failed to yield voidstar");
      return NULL;
  }

  char* msgpck_data = NULL;
  size_t msgpck_data_len = 0;

  int exitcode = pack_with_schema(voidstar, schema, &msgpck_data, &msgpck_data_len);
  if (exitcode != 0 || !msgpck_data) {
      PyErr_SetString(PyExc_RuntimeError, "py_to_mesgpack: Packing failed");
      free(msgpck_data);
      free_schema(schema);
      return NULL;
  }

  // TODO: avoid memory copying here
  PyObject* mesgpack_bytes = PyBytes_FromStringAndSize(msgpck_data, msgpck_data_len);
  free_schema(schema);
  free(msgpck_data);

  // free voidstar, we shan't be needing it now

  return mesgpack_bytes;
}


static PyObject* mesgpack_to_py(PyObject* self, PyObject* args) {
    const char* msgpck_data;
    Py_ssize_t msgpck_data_len;
    const char* schema_str;
    void* voidstar = NULL;

    if (!PyArg_ParseTuple(args, "y#s", &msgpck_data, &msgpck_data_len, &schema_str)) {
        return NULL;
    }

    Schema* schema = parse_schema(&schema_str);

    int exitcode = unpack_with_schema(msgpck_data, msgpck_data_len, schema, &voidstar);
    if(exitcode != 0){
        PyErr_SetString(PyExc_TypeError, "unpack_with_schema failed in mesgpack_to_py");
        return NULL;
    }

    PyObject* obj = fromAnything(schema, voidstar);
    if (obj == NULL) {
        PyErr_SetString(PyExc_TypeError, "fromAnything returned NULL");
        return NULL;
    }

    free_schema(schema);

    return obj;
}



// initialize a shared memory pool with one volume
static PyObject* shm_start(PyObject* self, PyObject* args) {
    char* basename;
    size_t shm_desired_size;
    shm_t* shm;

    if (!PyArg_ParseTuple(args, "sk", &basename, &shm_desired_size)) {
        PyErr_SetString(PyExc_TypeError, "Failed to parse arguments");
        return NULL;
    }

    shm = shinit(basename, 0, shm_desired_size);
    if (shm == NULL) {
        PyErr_SetString(PyExc_RuntimeError, "Failed to initialize shared memory");
        return NULL;
    }

    return PyCapsule_New(shm, "shm_t", NULL);
}

static PyObject* shm_rel2abs(PyObject* self, PyObject* args) {
    size_t relptr;
    if (!PyArg_ParseTuple(args, "k", &relptr)) {
        return NULL;
    }

    absptr_t absptr = rel2abs((relptr_t)relptr);
    return PyCapsule_New((void*)absptr, "absptr_t", NULL);
}

static PyObject* shm_abs2rel(PyObject* self, PyObject* args) {
    PyObject* capsule;
    if (!PyArg_ParseTuple(args, "O", &capsule)) {
        return NULL;
    }

    absptr_t absptr = (absptr_t)PyCapsule_GetPointer(capsule, "absptr_t");
    if (absptr == NULL) {
        return NULL;  // PyCapsule_GetPointer sets the error
    }

    relptr_t relptr = abs2rel(absptr);
    return PyLong_FromSize_t(relptr);
}


static PyObject* shm_close(PyObject* self, PyObject* args) {
    shclose();
    Py_RETURN_NONE;
}

// Take a python object and a schema, convert it to voidstar, write the
// voidstar to the shared memory pool, and return the relative pointer as an
// integer.
static PyObject* to_shm(PyObject* self, PyObject* args) {
  PyObject* obj;
  const char* schema_str;

  if (!PyArg_ParseTuple(args, "Os", &obj, &schema_str)) {
      return NULL;
  }

  Schema* schema = parse_schema(&schema_str);

  void* voidstar = to_voidstar_c(schema, obj);

  free_schema(schema);

  relptr_t relptr = abs2rel(voidstar);
  return PyLong_FromSize_t(relptr);
}

// Takes a relative pointer as an integer and a schema, looks up the voidstar in
// the shared memory pool, convert it to a python object, and return it
static PyObject* from_shm(PyObject* self, PyObject* args) {
  size_t relptr;
  const char* schema_str;

  if (!PyArg_ParseTuple(args, "ks", &relptr, &schema_str)) {
      return NULL;
  }

  Schema* schema = parse_schema(&schema_str);

  absptr_t voidstar = rel2abs(relptr);

  PyObject* obj = fromAnything(schema, voidstar);
  if (obj == NULL) {
      free_schema(schema);
      PyErr_SetString(PyExc_TypeError, "fromAnything returned NULL");
      return NULL;
  }

  free_schema(schema);

  return obj;
}


static  PyObject* write_voidstar_as_json(PyObject* self, PyObject* args) {
  size_t relptr;
  const char* schema_str;
  if (!PyArg_ParseTuple(args, "ks", &relptr, &schema_str)) {
    return NULL;
  }

  Schema* schema = parse_schema(&schema_str);
  absptr_t voidstar = rel2abs(relptr);
  print_voidstar(voidstar, schema);
  free_schema(schema);
  Py_RETURN_NONE;
}

static PyObject* write_msgpack_as_json(PyObject* self, PyObject* args) {
  const char* msgpck_data;
  Py_ssize_t msgpck_data_len;
  const char* schema_str;
  void* voidstar = NULL;

  if (!PyArg_ParseTuple(args, "y#s", &msgpck_data, &msgpck_data_len, &schema_str)) {
    return NULL;
  }

  Schema* schema = parse_schema(&schema_str);

  int exitcode = unpack_with_schema(msgpck_data, msgpck_data_len, schema, &voidstar);
  if (exitcode != 0) {
    PyErr_Format(PyExc_RuntimeError, "Unpacking failed with exit code %d", exitcode);
    return NULL;
  }

  print_voidstar(voidstar, schema);

  free_schema(schema);

  Py_RETURN_NONE;
}

static PyObject* write_msgpack_file_as_json(PyObject* self, PyObject* args) {
  char* filename;
  const char* schema_str;

  if (!PyArg_ParseTuple(args, "ss", &filename, &schema_str)) {
    return NULL;
  }

  Schema* schema = parse_schema(&schema_str);

  // Get file size
  struct stat st;
  if (stat(filename, &st) != 0) {
    PyErr_SetFromErrno(PyExc_IOError);
    free_schema(schema);
    return NULL;
  }
  size_t file_size = st.st_size;

  // Allocate memory for file contents
  char* msgpack_data = (char*)malloc(file_size);
  if (msgpack_data == NULL) {
    PyErr_NoMemory();
    free_schema(schema);
    return NULL;
  }

  // Open and read file
  FILE* file = fopen(filename, "rb");
  if (file == NULL) {
    PyErr_SetFromErrno(PyExc_IOError);
    free(msgpack_data);
    free_schema(schema);
    return NULL;
  }

  size_t bytes_read = fread(msgpack_data, 1, file_size, file);
  fclose(file);

  if (bytes_read != file_size) {
    PyErr_Format(PyExc_IOError, "Failed to read entire file. Expected %zu bytes, read %zu bytes", file_size, bytes_read);
    free(msgpack_data);
    free_schema(schema);
    return NULL;
  }

  void* voidstar;
  int exitcode = unpack_with_schema(msgpack_data, file_size, schema, &voidstar);
  if (exitcode != 0) {
    PyErr_Format(PyExc_RuntimeError, "Unpacking failed with exit code %d", exitcode);
    free(msgpack_data);
    free_schema(schema);
    return NULL;
  }

  print_voidstar(voidstar, schema);

  free(msgpack_data);
  free_schema(schema);

  Py_RETURN_NONE;
}


static PyMethodDef Methods[] = {
    {"to_mesgpack", to_mesgpack, METH_VARARGS, "Serialize a voidstar to MessagePack data"},
    {"from_mesgpack", from_mesgpack, METH_VARARGS, "Deserialize MessagePack data to voidstar"},
    {"to_voidstar", to_voidstar, METH_VARARGS, "Convert python data to voidstar"},
    {"from_voidstar", from_voidstar, METH_VARARGS, "Convert voidstar to python data"},
    {"py_to_mesgpack", py_to_mesgpack, METH_VARARGS, "Convert python data to mesgpack"},
    {"mesgpack_to_py", mesgpack_to_py, METH_VARARGS, "Convert mesgpack to python data"},
    {"shm_rel2abs", shm_rel2abs, METH_VARARGS, "Convert a relative shared memory pointer to an absolute pointer to process memory"},
    {"shm_abs2rel", shm_abs2rel, METH_VARARGS, "Convert an absolute pointer to process memory to a relative shared memory pointer"},
    {"shm_start", shm_start, METH_VARARGS, "Initialize the shared memory pool"},
    {"shm_close", shm_close, METH_VARARGS, "Close shared memory pool"},
    {"to_shm", to_shm, METH_VARARGS, "Write python object to memory pool and return a relative pointer"},
    {"from_shm", from_shm, METH_VARARGS, "Create a python object from a memory pool relative pointer"},
    {"write_msgpack_as_json", write_msgpack_as_json, METH_VARARGS, "Given MessagePack data, write JSON"},
    {"write_msgpack_file_as_json", write_msgpack_file_as_json, METH_VARARGS, "Given a MessagePack filename, write JSON"},
    {"write_voidstar_as_json", write_voidstar_as_json, METH_VARARGS, "Given a relative pointer to voidstar data, write JSON"},
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
