#define PY_SSIZE_T_CLEAN
#include "mlcmpack.h"
#include "Python.h"


// Exported prototypes
static PyObject* to_msgpack(PyObject* self, PyObject* args);
static PyObject* from_msgpack(PyObject* self, PyObject* args);
static PyObject* to_voidstar(PyObject* self, PyObject* args);
static PyObject* from_voidstar(PyObject* self, PyObject* args);


// convert voidstar to MessagePack
static PyObject* to_msgpack(PyObject* self, PyObject* args) {
    PyObject* voidstar_capsule;
    char* schema;
    char* msgpck_data = NULL;
    size_t msgpck_data_len = 0;

    if (!PyArg_ParseTuple(args, "Os", &voidstar_capsule, &schema)) {
        PyErr_SetString(PyExc_TypeError, "Failed to parse arguments");
        return NULL;
    }

    void* voidstar = PyCapsule_GetPointer(voidstar_capsule, "VoidStar");

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
    PyObject* msgpack_bytes = PyBytes_FromStringAndSize(msgpck_data, msgpck_data_len);
    free(msgpck_data);

    return msgpack_bytes;
}


// destroy the voidstar
static void voidstar_destructor(PyObject *capsule) {
    void *voidstar = PyCapsule_GetPointer(capsule, "VoidStar");
    if (voidstar) {
        free(voidstar);
    }
}

// convert MessagePack to voidstar
static PyObject* from_msgpack(PyObject* self, PyObject* args) {
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

    PyObject* voidstar_capsule = PyCapsule_New(voidstar, "VoidStar", voidstar_destructor);
    if (!voidstar_capsule) {
        free(voidstar);  // Or use appropriate deallocation function
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
            obj = PyUnicode_FromStringAndSize(str_array->data, str_array->size);
            break;
        }
        case MORLOC_ARRAY: {
            Array* array = (Array*)data;
            if (schema->parameters[0]->type == MORLOC_UINT8) {
                // Create a Python bytes object for UINT8 arrays
                obj = PyBytes_FromStringAndSize((const char*)array->data, array->size);
                if (!obj) goto error;
            } else {
                // For other types, create a list as before
                obj = PyList_New(array->size);
                if (!obj) goto error;
                char* start = (char*)array->data;
                size_t width = schema->parameters[0]->width;
                Schema* element_schema = schema->parameters[0];
                for (size_t i = 0; i < array->size; i++) {
                    PyObject* item = fromAnything(element_schema, start + width * i);
                    if (!item || PyList_SetItem(obj, i, item) < 0) {
                        Py_XDECREF(item);
                        goto error;
                    }
                }
            }
            break;
        }
        case MORLOC_TUPLE: {
            obj = PyTuple_New(schema->size);
            if (!obj) goto error;
            for (size_t i = 0; i < schema->size; i++) {
                void* item_ptr = (char*)data + schema->offsets[i];
                PyObject* item = fromAnything(schema->parameters[i], item_ptr);
                if (!item || PyTuple_SetItem(obj, i, item) < 0) {
                    Py_XDECREF(item);
                    goto error;
                }
            }
            break;
        }
        case MORLOC_MAP: {
            obj = PyDict_New();
            if (!obj) goto error;
            for (size_t i = 0; i < schema->size; i++) {
                void* item_ptr = (char*)data + schema->offsets[i];
                PyObject* value = fromAnything(schema->parameters[i], item_ptr);
                PyObject* key = PyUnicode_FromString(schema->keys[i]);
                if (!value || !key || PyDict_SetItem(obj, key, value) < 0) {
                    Py_XDECREF(value);
                    Py_XDECREF(key);
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

    void* voidstar = PyCapsule_GetPointer(voidstar_capsule, "VoidStar");

    if (voidstar == NULL) {
        PyErr_SetString(PyExc_TypeError, "Invalid voidstar capsule");
        return NULL;
    }

    const Schema* schema = parse_schema(&schema_str);
    if (!schema) {
        PyErr_SetString(PyExc_ValueError, "Failed to parse schema");
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


void* toAnything(void* dest, Schema* schema, PyObject* obj) {
    if (!dest) {
        dest = get_ptr(schema);
    }

    switch (schema->type) {
        case MORLOC_NIL:
            if (obj != Py_None) {
                PyErr_Format(PyExc_TypeError, "Expected None for MORLOC_NIL, but got %s", Py_TYPE(obj)->tp_name);
                goto error;
            }
            *((int8_t*)dest) = (int8_t)0;
            break;

        case MORLOC_BOOL:
            if (!PyBool_Check(obj)) {
                PyErr_Format(PyExc_TypeError, "Expected bool for MORLOC_BOOL, but got %s", Py_TYPE(obj)->tp_name);
                goto error;
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
                PyErr_Format(PyExc_TypeError, "Expected float for MORLOC_FLOAT32, but got %s", Py_TYPE(obj)->tp_name);
                goto error;
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
            if (schema->type == MORLOC_ARRAY && !(PyList_Check(obj) || PyBytes_Check(obj))) {
                PyErr_Format(PyExc_TypeError, "Expected list for MORLOC_ARRAY, but got %s", Py_TYPE(obj)->tp_name);
                goto error;
            }

            {
                Py_ssize_t size;
                char* data;

                if (PyList_Check(obj)) {
                    size = PyList_Size(obj);
                } else if (PyBytes_Check(obj)) {
                    PyBytes_AsStringAndSize(obj, &data, &size);
                } else { // PyUnicode_Check(obj)
                    data = PyUnicode_AsUTF8AndSize(obj, &size);
                }

                Array* result = array_data(dest, schema->parameters[0]->width, size);


                if (PyList_Check(obj)) {
                    size_t width = schema->parameters[0]->width;
                    char* start = (char*)result->data;
                    Schema* element_schema = schema->parameters[0];
                    for (Py_ssize_t i = 0; i < size; i++) {
                        PyObject* item = PyList_GetItem(obj, i);
                        toAnything(start + width * i, element_schema, item);
                    }
                } else {
                    memcpy(result->data, data, size);
                }

                dest = (void*)result;
            }
            break;

        case MORLOC_TUPLE:
            if (!PyTuple_Check(obj)) {
                PyErr_Format(PyExc_TypeError, "Expected tuple for MORLOC_TUPLE, but got %s", Py_TYPE(obj)->tp_name);
                goto error;
            }

            {
                Py_ssize_t size = PyTuple_Size(obj);
                if ((size_t)size != schema->size) {
                    PyErr_SetString(PyExc_ValueError, "Tuple size mismatch");
                    goto error;
                }
                for (Py_ssize_t i = 0; i < size; ++i) {
                    PyObject* item = PyTuple_GetItem(obj, i);
                    toAnything((char*)dest + schema->offsets[i], schema->parameters[i], item);
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
                        toAnything((char*)dest + schema->offsets[i], schema->parameters[i], value);
                    }
                }
            }
            break;

        default:
            PyErr_SetString(PyExc_TypeError, "Unsupported schema type");
            goto error;
    }

    return dest;

error:
    return NULL;
}


// convert PyObject to voidstar
static PyObject* to_voidstar(PyObject* self, PyObject* args){
  PyObject* obj;
  const char* schema_str;

  if (!PyArg_ParseTuple(args, "Os", &obj, &schema_str)) {
      return NULL;
  }

  Schema* schema = parse_schema(&schema_str);

  void* voidstar = toAnything(NULL, schema, obj);

  if(!voidstar){
      PyErr_SetString(PyExc_TypeError, "toAnything failed");
      return NULL;
  }

  // Note that there is no destructor. This memory is managed by the custom
  // memory allocator. I will eventually add some means of reference
  // counting. When I do, the destructor will decrement this count.
  PyObject* voidstar_capsule = PyCapsule_New(voidstar, "VoidStar", NULL);

  free_schema(schema);

  return voidstar_capsule;
}


static PyMethodDef Methods[] = {
    {"to_msgpack",    to_msgpack,    METH_VARARGS, "Serialize a voidstar to MessagePack data"},
    {"from_msgpack",  from_msgpack,  METH_VARARGS, "Deserialize MessagePack data to voidstar"},
    {"to_voidstar",   to_voidstar,   METH_VARARGS, "Convert python data to voidstar"},
    {"from_voidstar", from_voidstar, METH_VARARGS, "Convert voidstar to python data"},
    {NULL, NULL, 0, NULL} // this is a sentinel value
};


static struct PyModuleDef pympack = {
    PyModuleDef_HEAD_INIT,
    "pympack",
    "Python interface to Morloc binary and MessagePack data",
    -1,
    Methods
};

PyMODINIT_FUNC PyInit_pympack(void) {
    return PyModule_Create(&pympack);
}
