#define PY_SSIZE_T_CLEAN
#include "morloc.h"
#include "Python.h"


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
            break;
        }
        case MORLOC_ARRAY: {
            Array* array = (Array*)data;
            if (schema->parameters[0]->type == MORLOC_UINT8) {
                // Create a Python bytes object for UINT8 arrays
                obj = PyBytes_FromStringAndSize((const char*)rel2abs(array->data), array->size);
                if (!obj) goto error;
            } else {
                // For other types, create a list as before
                obj = PyList_New(array->size);
                if (!obj) goto error;
                char* start = (char*)rel2abs(array->data);
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
            if (schema->type == MORLOC_ARRAY && !(PyList_Check(obj) || PyBytes_Check(obj))) {
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
                } else {
                    memcpy(rel2abs(result->data), data, size);

                    // move cursor to the location after the copied data
                    *cursor = (void*)(*(char**)cursor + size);
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

  free_schema(schema);
    
  return obj;
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
