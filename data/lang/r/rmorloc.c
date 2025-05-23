#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Arith.h>

#include <stdint.h>
#include <stdbool.h>
#include <limits.h>
#include <string.h>

#include "morloc.h"

// {{{ macros

#define MAYFAIL char* child_errmsg_ = NULL;

#define R_TRY(fun, ...) \
    fun(__VA_ARGS__ __VA_OPT__(,) &child_errmsg_); \
    if(child_errmsg_ != NULL){ \
        error("Error (%s:%d in %s):\n%s", __FILE__, __LINE__, __func__, child_errmsg_); \
    }

# define R_TRY_WITH(clean, fun, ...) \
    fun(__VA_ARGS__ __VA_OPT__(,) &child_errmsg_); \
    if(child_errmsg_ != NULL){ \
        clean; \
        error("Error (%s:%d in %s):\n%s", __FILE__, __LINE__, __func__, child_errmsg_); \
    }

/// }}}

// {{{ to_voidstar

static size_t get_shm_size(const Schema* schema, SEXP obj) {
    size_t size = 0;
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
            {
                size_t length = (size_t)LENGTH(obj);
                size = sizeof(Array);
                const char* str;

                switch (TYPEOF(obj)) {
                    case CHARSXP:
                        str = CHAR(obj);
                        size += (size_t)strlen(str);  // Do not include null terminator
                        break;
                    case STRSXP:
                        if (LENGTH(obj) == 1) {
                            str = CHAR(STRING_ELT(obj, 0));
                            size += (size_t)strlen(str);  // Do not include null terminator
                        } else {
                            if(schema->parameters[0]->type == MORLOC_STRING){
                                for(size_t i = 0; i < length; i++){
                                    size += get_shm_size(schema->parameters[0], STRING_ELT(obj, i));
                                }
                            } else {
                                error("Expected character vector of length 1, but got length %zu", length);
                            }
                        }
                        break;
                    case VECSXP:  // This handles lists
                        for (int i = 0; i < length; i++) {
                            size += get_shm_size(schema->parameters[0], VECTOR_ELT(obj, i));
                        }
                        break;
                    case LGLSXP:
                    case INTSXP:
                    case REALSXP:
                    case RAWSXP:
                        size += length * schema->parameters[0]->width;
                        break;
                    default:
                        error("Unsupported type in get_shm_size array: %s", type2char(TYPEOF(obj)));
                }
                return size;
            }

        case MORLOC_TUPLE:
            if (!isVectorList(obj)) {
                error("Expected list for MORLOC_TUPLE, but got %s", type2char(TYPEOF(obj)));
            }

            {
                size_t array_size = (size_t)xlength(obj);
                if (array_size != schema->size) {
                    error("Expected tuple of length %zu, but found list of length %zu", schema->size, size);
                }
                for (R_xlen_t i = 0; i < array_size; ++i) {
                    SEXP item = VECTOR_ELT(obj, i);
                    size += get_shm_size(schema->parameters[i], item);
                }
                return size;
            }

        case MORLOC_MAP:
            {
                if (isNewList(obj)) {
                    // Handle named list
                    size = 0;
                    SEXP names = getAttrib(obj, R_NamesSymbol);
                    if (names == R_NilValue) {
                        error("List must have names for MORLOC_MAP");
                    }
                    for (size_t i = 0; i < schema->size; ++i) {
                        SEXP key = PROTECT(mkChar(schema->keys[i]));
                        int index = -1;
                        for (int j = 0; j < length(obj); j++) {
                            if (strcmp(CHAR(STRING_ELT(names, j)), CHAR(key)) == 0) {
                                index = j;
                                break;
                            }
                        }
                        if (index != -1) {
                            SEXP value = VECTOR_ELT(obj, index);
                            size += get_shm_size(schema->parameters[i], value);
                        }
                        UNPROTECT(1);
                    }
                    return size;
                } else {
                    error("Expected a named list for MORLOC_MAP");
                }
            }

        default:
            error("Unhandled schema type");
            break;
    }

    return size;
}


#define HANDLE_SINT_TYPE(CTYPE, MIN, MAX) \
    do { \
        if (!(isInteger(obj) || isReal(obj))) { \
            error("Expected integer for %s, but got %s", #CTYPE, type2char(TYPEOF(obj))); \
        } \
        double value = asReal(obj); \
        if (value < MIN || value > MAX) { \
            error("Integer overflow for %s", #CTYPE); \
        } \
        *(CTYPE*)dest = (CTYPE)value; \
    } while(0)

#define HANDLE_UINT_TYPE(CTYPE, MAX) \
    do { \
        if (!(isInteger(obj) || isReal(obj))) { \
            error("Expected integer for %s, but got %s", #CTYPE, type2char(TYPEOF(obj))); \
        } \
        double value = asReal(obj); \
        if (value < 0 || value > MAX) { \
            error("Integer overflow for %s", #CTYPE); \
        } \
        *(CTYPE*)dest = (CTYPE)value; \
    } while(0)

static void* to_voidstar_r(void* dest, void** cursor, SEXP obj, const Schema* schema){
    MAYFAIL

    switch (schema->type) {
        case MORLOC_NIL:
            if (obj != R_NilValue) {
                error("Expected NULL for MORLOC_NIL, but got %s", type2char(TYPEOF(obj)));
            }
            *((int8_t*)dest) = (int8_t)0;
            break;
        case MORLOC_BOOL:
            if (!isLogical(obj)) {
                error("Expected logical for MORLOC_BOOL, but got %s", type2char(TYPEOF(obj)));
            }
            *((uint8_t*)dest) = (uint8_t)((LOGICAL(obj)[0] == TRUE) ? 1 : 0);
            break;
        case MORLOC_SINT8:
            HANDLE_SINT_TYPE(int8_t, INT8_MIN, INT8_MAX);
            break;
        case MORLOC_SINT16:
            HANDLE_SINT_TYPE(int16_t, INT16_MIN, INT16_MAX);
            break;
        case MORLOC_SINT32:
            HANDLE_SINT_TYPE(int32_t, INT32_MIN, INT32_MAX);
            break;
        case MORLOC_SINT64:
            HANDLE_SINT_TYPE(int64_t, INT64_MIN, INT64_MAX);
            break;
        case MORLOC_UINT8:
            HANDLE_UINT_TYPE(uint8_t, UINT8_MAX);
            break;
        case MORLOC_UINT16:
            HANDLE_UINT_TYPE(uint16_t, UINT16_MAX);
            break;
        case MORLOC_UINT32:
            HANDLE_UINT_TYPE(uint32_t, UINT32_MAX);
            break;
        case MORLOC_UINT64:
            HANDLE_UINT_TYPE(uint64_t, UINT64_MAX);
            break;
        case MORLOC_FLOAT32:
            if (!(isReal(obj) || isInteger(obj))) {
                error("Expected numeric for MORLOC_FLOAT32, but got %s", type2char(TYPEOF(obj)));
            }
            *((float*)dest) = (float)asReal(obj);
            break;

        case MORLOC_FLOAT64:
            if (!(isReal(obj) || isInteger(obj))) {
                error("Expected numeric for MORLOC_FLOAT64, but got %s", type2char(TYPEOF(obj)));
            }
            *((double*)dest) = asReal(obj);
            break;
        case MORLOC_STRING:
            {
                const char* str = NULL;
                size_t length = 0;
                switch(TYPEOF(obj)){
                    case CHARSXP:
                        str = CHAR(obj);
                        length = (size_t)strlen(str);
                        break;
                    case STRSXP:
                        if (LENGTH(obj) == 1) {
                            str = CHAR(STRING_ELT(obj, 0));
                            length = (size_t)strlen(str);
                        } else {
                            error("Expected character of length 1");
                        }
                        break;
                    case RAWSXP:
                        str = RAW(obj);
                        length = LENGTH(obj);
                        break;
                    default:
                      error("Expected a character type");
                      break;
                }
                Array* array = (Array*)dest;
                array->size = length;  // Do not include null terminator
                if(length > 0){
                    array->data = R_TRY(abs2rel, *cursor);
                    absptr_t tmp_ptr = R_TRY(rel2abs, array->data);
                    memcpy(tmp_ptr, str, array->size);
                } else {
                    array->data = RELNULL;
                }

                // move cursor to the location after the copied data
                *cursor = (void*)(*(char**)cursor + array->size);
            }
            break;
        case MORLOC_ARRAY:
            Array* array = (Array*)dest;
            array->size = (size_t)length(obj);
            if(array->size == 0){
                array->data = RELNULL;
                break;
            }

            array->data = R_TRY(abs2rel, *cursor);
            Schema* element_schema = schema->parameters[0];
            char* start;

            switch (TYPEOF(obj)) {
                case STRSXP:
                    {
                        if(element_schema->type == MORLOC_STRING){
                            // set the cursor the the location after the array headers
                            *cursor = (void*)(*(char**)cursor + array->size * element_schema->width);
                            start = R_TRY(rel2abs, array->data);
                            for(size_t i = 0; i < array->size; i++){
                                SEXP elem = STRING_ELT(obj, i);
                                to_voidstar_r(start + i * element_schema->width, cursor, elem, element_schema);
                            }
                        } else {
                            error("Expected character vector of length 1, but got length %ld", array->size);
                        }
                    }
                    break;
                case RAWSXP:  // Raw vectors
                    if (element_schema->type != MORLOC_UINT8) {
                        error("Expected MORLOC_UINT8 for raw vector");
                    }
                    absptr_t tmp_ptr = R_TRY(rel2abs, array->data);
                    memcpy(tmp_ptr, RAW(obj), array->size * sizeof(uint8_t));
                    *cursor = (void*)(*(char**)cursor + array->size * sizeof(uint8_t));
                    break;
                case VECSXP:  // This handles lists
                    *cursor = (void*)(*(char**)cursor + array->size * element_schema->width);
                    start = R_TRY(rel2abs, array->data);
                    for (int i = 0; i < array->size; i++) {
                        SEXP elem = VECTOR_ELT(obj, i);
                        to_voidstar_r(start + i * element_schema->width, cursor, elem, element_schema);
                    }
                    break;
                case LGLSXP:
                    *cursor = (void*)(*(char**)cursor + array->size * element_schema->width);
                    start = R_TRY(rel2abs, array->data);
                    for (int i = 0; i < array->size; i++) {
                        SEXP elem = PROTECT(ScalarLogical(LOGICAL(obj)[i]));
                        to_voidstar_r(start + i, cursor, elem, element_schema);
                        UNPROTECT(1);
                    }
                    break;
                case INTSXP:
                    *cursor = (void*)(*(char**)cursor + array->size * element_schema->width);
                    start = R_TRY(rel2abs, array->data);
                    for (int i = 0; i < array->size; i++) {
                        SEXP elem = PROTECT(ScalarInteger(INTEGER(obj)[i]));
                        to_voidstar_r(start + i * element_schema->width, cursor, elem, element_schema);
                        UNPROTECT(1);
                    }
                    break;
                case REALSXP:
                    *cursor = (void*)(*(char**)cursor + array->size * element_schema->width);
                    start = R_TRY(rel2abs, array->data);
                    for (int i = 0; i < array->size; i++) {
                        SEXP elem = PROTECT(ScalarReal(REAL(obj)[i]));
                        to_voidstar_r(start + i * element_schema->width, cursor, elem, element_schema);
                        UNPROTECT(1);
                    }
                    break;
                default:
                    error("Unsupported type in to_voidstar array: %s", type2char(TYPEOF(obj)));
            }
            break;



        case MORLOC_TUPLE:
            if (!isVectorList(obj)) {
                error("Expected list for MORLOC_TUPLE, but got %s", type2char(TYPEOF(obj)));
            }

            {
                R_xlen_t size = xlength(obj);
                if ((size_t)size != schema->size) {
                    error("Expected tuple of length %zu, but found list of length %zu", schema->size, size);
                }
                for (R_xlen_t i = 0; i < size; ++i) {
                    SEXP item = VECTOR_ELT(obj, i);
                    to_voidstar_r(dest + schema->offsets[i], cursor, item, schema->parameters[i]);
                }
            }
            break;

        case MORLOC_MAP:
            {
                if (isNewList(obj)) {
                    // Handle named list
                    SEXP names = getAttrib(obj, R_NamesSymbol);
                    if (names == R_NilValue) {
                        error("List must have names for MORLOC_MAP");
                    }
                    for (size_t i = 0; i < schema->size; ++i) {
                        SEXP key = PROTECT(mkChar(schema->keys[i]));
                        int index = -1;
                        for (int j = 0; j < length(obj); j++) {
                            if (strcmp(CHAR(STRING_ELT(names, j)), CHAR(key)) == 0) {
                                index = j;
                                break;
                            }
                        }
                        if (index != -1) {
                            SEXP value = VECTOR_ELT(obj, index);
                            to_voidstar_r(dest + schema->offsets[i], cursor, value, schema->parameters[i]);
                        }
                        UNPROTECT(1);
                    }
                } else {
                    error("Expected a named list for MORLOC_MAP");
                }
            }
            break;

        default:
            error("Unhandled schema type");
            break;
    }

    return dest;

}


static void* to_voidstar(SEXP obj, const Schema* schema) {
    MAYFAIL

    size_t total_size = get_shm_size(schema, obj);

    void* dest = R_TRY(shmalloc, total_size);

    void* cursor = (void*)((char*)dest + schema->width);

    return to_voidstar_r(dest, &cursor, obj, schema);
}

// }}} to_voidstar

// {{{ from_voidstar

static SEXP from_voidstar(const void* data, const Schema* schema) {
    MAYFAIL

    if(data == NULL){
        error("NULL data (%s:%d in %s)", __FILE__, __LINE__, __func__);
    }

    if(schema == NULL){
        error("NULL schema (%s:%d in %s)", __FILE__, __LINE__, __func__);
    }

    SEXP obj = R_NilValue;
    switch (schema->type) {
        case MORLOC_NIL:
            return R_NilValue;
        case MORLOC_BOOL:
            obj = ScalarLogical((bool)*(uint8_t*)data);
            break;
        case MORLOC_SINT8:
            obj = ScalarInteger((int)(*(int8_t*)data));
            break;
        case MORLOC_SINT16:
            obj = ScalarInteger((int)(*(int16_t*)data));
            break;
        case MORLOC_SINT32:
            obj = ScalarInteger(*(int32_t*)data);
            break;
        case MORLOC_SINT64:
            obj = ScalarReal((double)(*(int64_t*)data));
            break;
        case MORLOC_UINT8:
            obj = ScalarInteger((int)(*(uint8_t*)data));
            break;
        case MORLOC_UINT16:
            obj = ScalarInteger((int)(*(uint16_t*)data));
            break;
        case MORLOC_UINT32:
            obj = ScalarReal((double)(*(uint32_t*)data));
            break;
        case MORLOC_UINT64:
            obj = ScalarReal((double)(*(uint64_t*)data));
            break;
        case MORLOC_FLOAT32:
            obj = ScalarReal((double)(*(float*)data));
            break;
        case MORLOC_FLOAT64:
            obj = ScalarReal(*(double*)data);
            break;
        case MORLOC_STRING: {
                if (schema->hint != NULL && strcmp(schema->hint, "raw") == 0){
                    Array* raw_array = (Array*)data;
                    if(raw_array->size > 0){
                        absptr_t tmp_ptr = R_TRY(rel2abs, raw_array->data);
                        obj = PROTECT(allocVector(RAWSXP, raw_array->size));
                        memcpy(RAW(obj), tmp_ptr, raw_array->size);
                    } else {
                        obj = PROTECT(allocVector(RAWSXP, 0));
                    }
                    UNPROTECT(1);
                } else {
                    Array* str_array = (Array*)data;
                    if(str_array->size > 0){
                        absptr_t tmp_ptr = R_TRY(rel2abs, str_array->data);
                        SEXP chr = PROTECT(mkCharLen(tmp_ptr, str_array->size));
                        obj = PROTECT(ScalarString(chr));
                    } else {
                        SEXP chr = PROTECT(mkChar(""));
                        obj = PROTECT(ScalarString(chr));
                    }
                    UNPROTECT(2);
                }
            }
            break;
        case MORLOC_ARRAY:
            {
                Array* array = (Array*)data;
                Schema* element_schema = schema->parameters[0];
                char* start;

                switch(element_schema->type){
                    case MORLOC_BOOL:
                        obj = PROTECT(allocVector(LGLSXP, array->size));
                        if(array->size == 0) {
                            UNPROTECT(1);
                            break;
                        }
                        start = (char*)R_TRY(rel2abs, array->data);
                        for (size_t i = 0; i < array->size; i++) {
                            LOGICAL(obj)[i] = (bool)*(uint8_t*)(start + i) ? TRUE : FALSE;
                        }
                        UNPROTECT(1);
                        break;
                    case MORLOC_SINT8:
                        obj = PROTECT(allocVector(INTSXP, array->size));
                        if(array->size == 0) {
                            UNPROTECT(1);
                            break;
                        }
                        start = (char*)R_TRY(rel2abs, array->data);
                        for (size_t i = 0; i < array->size; i++) {
                            INTEGER(obj)[i] = (int)(*(int8_t*)(start + i * sizeof(int8_t)));
                        }
                        UNPROTECT(1);
                        break;
                    case MORLOC_SINT16:
                        obj = PROTECT(allocVector(INTSXP, array->size));
                        if(array->size == 0) {
                            UNPROTECT(1);
                            break;
                        }
                        start = (char*)R_TRY(rel2abs, array->data);
                        for (size_t i = 0; i < array->size; i++) {
                            INTEGER(obj)[i] = (int)(*(int16_t*)(start + i * sizeof(int16_t)));
                        }
                        UNPROTECT(1);
                        break;
                    case MORLOC_SINT32:
                        obj = PROTECT(allocVector(INTSXP, array->size));
                        if(array->size == 0) {
                            UNPROTECT(1);
                            break;
                        }
                        absptr_t tmp_ptr = R_TRY(rel2abs, array->data);
                        memcpy(INTEGER(obj), tmp_ptr, array->size * sizeof(int32_t));
                        UNPROTECT(1);
                        break;
                    case MORLOC_SINT64:
                        obj = PROTECT(allocVector(REALSXP, array->size));
                        if(array->size == 0) {
                            UNPROTECT(1);
                            break;
                        }
                        start = (char*)R_TRY(rel2abs, array->data);
                        for (size_t i = 0; i < array->size; i++) {
                            REAL(obj)[i] = (double)(*(int64_t*)(start + i * sizeof(int64_t)));
                        }
                        UNPROTECT(1);
                        break;
                    // Interpret the uint8 as a raw vector
                    case MORLOC_UINT8:
                        obj = PROTECT(allocVector(RAWSXP, array->size));
                        if(array->size == 0) {
                            UNPROTECT(1);
                            break;
                        }
                        start = (char*)R_TRY(rel2abs, array->data);
                        memcpy(RAW(obj), start, array->size * sizeof(uint8_t));
                        UNPROTECT(1);
                        break;
                    case MORLOC_UINT16:
                        obj = PROTECT(allocVector(INTSXP, array->size));
                        if(array->size == 0) {
                            UNPROTECT(1);
                            break;
                        }
                        start = (char*)R_TRY(rel2abs, array->data);
                        for (size_t i = 0; i < array->size; i++) {
                            INTEGER(obj)[i] = (int)(*(uint16_t*)(start + i * sizeof(uint16_t)));
                        }
                        UNPROTECT(1);
                        break;
                    case MORLOC_UINT32:
                        obj = PROTECT(allocVector(REALSXP, array->size));
                        if(array->size == 0) {
                            UNPROTECT(1);
                            break;
                        }
                        start = (char*)R_TRY(rel2abs, array->data);
                        for (size_t i = 0; i < array->size; i++) {
                            REAL(obj)[i] = (double)(*(uint32_t*)(start + i * sizeof(uint32_t)));
                        }
                        UNPROTECT(1);
                        break;
                    case MORLOC_UINT64:
                        // NOTE: the R integer cannot store a 64 bit int
                        obj = PROTECT(allocVector(REALSXP, array->size));
                        if(array->size == 0) {
                            UNPROTECT(1);
                            break;
                        }
                        start = (char*)R_TRY(rel2abs, array->data);
                        for (size_t i = 0; i < array->size; i++) {
                            REAL(obj)[i] = (double)(*(uint64_t*)(start + i * sizeof(uint64_t)));
                        }
                        UNPROTECT(1);
                        break;
                    case MORLOC_FLOAT32:
                        obj = PROTECT(allocVector(REALSXP, array->size));
                        if(array->size == 0) {
                            UNPROTECT(1);
                            break;
                        }
                        start = (char*)R_TRY(rel2abs, array->data);
                        for (size_t i = 0; i < array->size; i++) {
                            REAL(obj)[i] = (double)(*(float*)(start + i * sizeof(float)));
                        }
                        UNPROTECT(1);
                        break;
                    case MORLOC_FLOAT64:
                        obj = PROTECT(allocVector(REALSXP, array->size));
                        if(array->size == 0) {
                            UNPROTECT(1);
                            break;
                        }
                        start = (char*)R_TRY(rel2abs, array->data);
                        memcpy(REAL(obj), start, array->size * sizeof(double));
                        UNPROTECT(1);
                        break;
                    case MORLOC_STRING:
                        {
                            obj = PROTECT(allocVector(STRSXP, array->size));
                            if(array->size == 0) {
                                UNPROTECT(1);
                                break;
                            }
                            start = (char*)R_TRY(rel2abs, array->data);
                            size_t width = schema->width;
                            for (size_t i = 0; i < array->size; i++) {
                                Array* str_array = (Array*)(start + i * width);
                                SEXP item;
                                if(str_array->size == 0){
                                    item = PROTECT(mkCharLen("", 0));
                                } else {
                                    absptr_t str_ptr = R_TRY_WITH(UNPROTECT(1), rel2abs, str_array->data);
                                    item = PROTECT(mkCharLen(str_ptr, str_array->size));
                                }
                                UNPROTECT(1);
                                SET_STRING_ELT(obj, i, item);
                            }
                            UNPROTECT(1);
                        }
                        break;
                    default:
                        {
                            obj = PROTECT(allocVector(VECSXP, array->size));
                            if(array->size == 0) {
                                UNPROTECT(1);
                                break;
                            }
                            start = (char*)R_TRY(rel2abs, array->data);
                            size_t width = element_schema->width;
                            for (size_t i = 0; i < array->size; i++) {
                                SEXP item = from_voidstar(start + width * i, element_schema);
                                if (item == R_NilValue) {
                                    obj = R_NilValue;
                                    goto error;
                                }
                                SET_VECTOR_ELT(obj, i, item);
                            }
                            UNPROTECT(1);
                        }
                        break;
                }
            }
            break;
        case MORLOC_TUPLE: {
            obj = allocVector(VECSXP, schema->size);
            for (size_t i = 0; i < schema->size; i++) {
                void* item_ptr = (char*)data + schema->offsets[i];
                SEXP item = from_voidstar(item_ptr, schema->parameters[i]);
                if (item == R_NilValue) {
                    obj = R_NilValue;
                    goto error;
                }
                SET_VECTOR_ELT(obj, i, item);
            }
            break;
        }
        case MORLOC_MAP: {
            obj = allocVector(VECSXP, schema->size);
            SEXP names = allocVector(STRSXP, schema->size);
            for (size_t i = 0; i < schema->size; i++) {
                void* item_ptr = (char*)data + schema->offsets[i];
                SEXP value = from_voidstar(item_ptr, schema->parameters[i]);
                if (value == R_NilValue) {
                    obj = R_NilValue;
                    goto error;
                }
                SET_VECTOR_ELT(obj, i, value);
                SET_STRING_ELT(names, i, mkChar(schema->keys[i]));
            }
            setAttrib(obj, R_NamesSymbol, names);
            break;
        }
        default:
            error("Unsupported schema type");
            goto error;
    }

    return obj;

error:
    return R_NilValue;
}

// }}} from_voidstar

// {{{ exported morloc API functions

// Close the daemon when the R object dies
static void daemon_finalizer(SEXP ptr) {
    if (!R_ExternalPtrAddr(ptr)) return;
    language_daemon_t* daemon = (language_daemon_t*)R_ExternalPtrAddr(ptr);
    if(daemon != NULL){
        close_daemon(&daemon);
    }
    R_ClearExternalPtr(ptr);
}

SEXP morloc_start_daemon(
    SEXP socket_path_r,
    SEXP tmpdir_r,
    SEXP shm_basename_r,
    SEXP shm_default_size_r
){ MAYFAIL
    const char* socket_path = CHAR(STRING_ELT(socket_path_r, 0));
    const char* tmpdir = CHAR(STRING_ELT(tmpdir_r, 0));
    const char* shm_basename = CHAR(STRING_ELT(shm_basename_r, 0));
    size_t shm_default_size = (size_t)asInteger(shm_default_size_r);

    language_daemon_t* daemon = R_TRY(
        start_daemon,
        socket_path,
        tmpdir,
        shm_basename,
        shm_default_size
    );

    // Wrap pointer in external pointer
    SEXP result = PROTECT(R_MakeExternalPtr(daemon, R_NilValue, R_NilValue));

    // Register finalizer with wrapper
    R_RegisterCFinalizerEx(result, daemon_finalizer, TRUE);

    // Set class attribute
    SEXP class_name = PROTECT(mkString("language_daemon"));
    SET_CLASS(result, class_name);

    UNPROTECT(2);
    return result;
}



SEXP morloc_wait_for_client(SEXP daemon_r){ MAYFAIL
    if (!R_ExternalPtrAddr(daemon_r)) {
        error("Expected a daemon pointer");
    }
    language_daemon_t* daemon = (language_daemon_t*)R_ExternalPtrAddr(daemon_r);

    int client_fd = R_TRY(wait_for_client_with_timeout, daemon, 10000);

    return ScalarInteger(client_fd);
}


SEXP morloc_read_morloc_call_packet(SEXP packet_r) { MAYFAIL
    uint8_t* packet = RAW(packet_r);
    morloc_call_t* call_packet = R_TRY(read_morloc_call_packet, packet);

    // Create two element R list
    //  1: manifold id
    //  2: argument list of raw packets
    SEXP r_list = PROTECT(allocVector(VECSXP, 2));

    // Convert midx to R integer
    SEXP r_mid = PROTECT(ScalarInteger(call_packet->midx));

    // Create arguments list
    SEXP r_args = PROTECT(allocVector(VECSXP, call_packet->nargs));

    for(size_t i = 0; i < call_packet->nargs; i++) {
        size_t arg_packet_size = R_TRY(morloc_packet_size, call_packet->args[i]);
        SEXP r_arg = PROTECT(allocVector(RAWSXP, arg_packet_size));
        memcpy(RAW(r_arg), call_packet->args[i], arg_packet_size);
        SET_VECTOR_ELT(r_args, i, r_arg);
        UNPROTECT(1);  // r_arg
    }

    // Assemble final list
    SET_VECTOR_ELT(r_list, 0, r_mid);
    SET_VECTOR_ELT(r_list, 1, r_args);

    UNPROTECT(3);  // r_list, r_mid, r_args
    return r_list;
}


SEXP morloc_send_packet_to_foreign_server(SEXP client_fd_r, SEXP packet_r) { MAYFAIL
    if (TYPEOF(client_fd_r) != INTSXP || LENGTH(client_fd_r) != 1) {
        error("client_fd must be a single integer");
    }
    if (TYPEOF(packet_r) != RAWSXP) {
        error("packet must be a raw vector");
    }

    // Extract arguments
    int client_fd = INTEGER(client_fd_r)[0];
    uint8_t* packet = RAW(packet_r);
    size_t packet_size = (size_t)LENGTH(packet_r);

    // Call underlying implementation
    size_t bytes_sent = R_TRY(send_packet_to_foreign_server, client_fd, packet);

    // This could in theory be problematic, since int is smaller than size_t
    // In practice it should not be, since packets are typically small
    // However, if I refactor to send large packets in the future, this could be
    // problematic. Then I would need to convert to a double return.
    return ScalarInteger((int)bytes_sent);
}


// Read from socket returning raw vector of received data
SEXP morloc_stream_from_client(SEXP client_fd_r) { MAYFAIL
    if (TYPEOF(client_fd_r) != INTSXP || LENGTH(client_fd_r) != 1) {
        error("client_fd must be a single integer");
    }

    int client_fd = INTEGER(client_fd_r)[0];

    // Read packet from socket
    uint8_t* packet = R_TRY(stream_from_client, client_fd);

    // Read the packet size from the header
    size_t packet_size = R_TRY(morloc_packet_size, packet);

    // Create raw vector for result
    SEXP result = PROTECT(allocVector(RAWSXP, packet_size));
    memcpy(RAW(result), packet, packet_size);

    UNPROTECT(1);
    return result;
}


// close_socket
SEXP morloc_close_socket(SEXP socket_id_r) {
    if (TYPEOF(socket_id_r) != INTSXP || LENGTH(socket_id_r) != 1) {
        error("socket_id must be a single integer");
    }
    int socket_id = INTEGER(socket_id_r)[0];
    close_socket(socket_id);
    // Return invisible NULL
    return R_NilValue;
}


// put_value
SEXP morloc_put_value(SEXP obj_r, SEXP schema_str_r) { MAYFAIL
    if (TYPEOF(schema_str_r) != STRSXP || LENGTH(schema_str_r) != 1) {
        error("schema must be a single string");
    }

    const char* schema_str = CHAR(STRING_ELT(schema_str_r, 0));

    Schema* schema = R_TRY(parse_schema, &schema_str);

    void* voidstar = to_voidstar(obj_r, schema);
    if (!voidstar) {
        error("Failed to convert R object to internal representation");
    }

    relptr_t relptr = R_TRY(abs2rel, voidstar);

    uint8_t* packet = make_standard_data_packet(relptr, schema);

    size_t packet_size = R_TRY(morloc_packet_size, packet);

    SEXP result = PROTECT(allocVector(RAWSXP, packet_size));
    memcpy(RAW(result), packet, packet_size);

    UNPROTECT(1);
    return result;
}


SEXP morloc_get_value(SEXP packet_r, SEXP schema_str_r) { MAYFAIL
    if (TYPEOF(packet_r) != RAWSXP) {
        error("packet must be a raw vector");
    }
    if (TYPEOF(schema_str_r) != STRSXP || LENGTH(schema_str_r) != 1) {
        error("schema must be a single string");
    }

    // Extract arguments
    uint8_t* packet = RAW(packet_r);
    size_t packet_size = (size_t)LENGTH(packet_r);
    const char* schema_str = CHAR(STRING_ELT(schema_str_r, 0));

    Schema* schema = R_TRY(parse_schema, &schema_str);

    uint8_t* voidstar = R_TRY(get_morloc_data_packet_value, packet, schema);

    SEXP obj_r = from_voidstar(voidstar, schema);
    if (obj_r == NULL) {
        error("Failed to convert internal representation to R object");
    }

    free_schema(schema);

    return obj_r;
}


SEXP morloc_foreign_call(SEXP socket_path_r, SEXP mid_r, SEXP args_r) { MAYFAIL
    // Validate inputs
    if (TYPEOF(socket_path_r) != STRSXP || LENGTH(socket_path_r) != 1) {
        error("socket_path must be a single string");
    }
    if (TYPEOF(mid_r) != INTSXP || LENGTH(mid_r) != 1) {
        error("mid must be a single integer");
    }
    if (TYPEOF(args_r) != VECSXP) {
        error("args must be a list of raw vectors");
    }

    // Extract arguments
    const char* socket_path = CHAR(STRING_ELT(socket_path_r, 0));
    int mid = INTEGER(mid_r)[0];
    size_t nargs = (size_t)LENGTH(args_r);

    // Allocate temporary storage
    const uint8_t** arg_packets = (const uint8_t**)R_alloc(nargs, sizeof(uint8_t*));

    // Convert R raw vectors to C buffers
    for (size_t i = 0; i < nargs; i++) {
        SEXP arg = VECTOR_ELT(args_r, i);
        if (TYPEOF(arg) != RAWSXP) {
            error("All arguments must be raw vectors (argument %zu)", i+1);
        }
        arg_packets[i] = RAW(arg);
    }

    // Create call packet
    uint8_t* packet = R_TRY(
        make_morloc_local_call_packet,
        (uint32_t)mid,
        arg_packets,
        nargs
    );

    // Send/receive over socket
    uint8_t* result = R_TRY_WITH(free(packet),
        send_and_receive_over_socket,
        socket_path,
        packet
    );

    // Get result size
    size_t result_length = R_TRY(morloc_packet_size, result);

    // Create result raw vector
    SEXP result_r = PROTECT(allocVector(RAWSXP, result_length));
    memcpy(RAW(result_r), result, result_length);

    // Cleanup
    UNPROTECT(1);
    return result_r;
}


SEXP morloc_is_ping(SEXP packet_r) { MAYFAIL
    if (TYPEOF(packet_r) != RAWSXP) {
        error("packet must be a raw vector");
    }

    bool is_ping = R_TRY(packet_is_ping, RAW(packet_r));

    return ScalarLogical(is_ping);
}


SEXP morloc_is_local_call(SEXP packet_r) { MAYFAIL
    if (TYPEOF(packet_r) != RAWSXP) {
        error("packet must be a raw vector");
    }

    bool is_local_call = R_TRY(packet_is_local_call, RAW(packet_r));

    return ScalarLogical(is_local_call);
}


SEXP morloc_is_remote_call(SEXP packet_r) { MAYFAIL
    if (TYPEOF(packet_r) != RAWSXP) {
        error("packet must be a raw vector");
    }

    bool is_remote_call = R_TRY(packet_is_remote_call, RAW(packet_r));

    return ScalarLogical(is_remote_call);
}


SEXP morloc_pong(SEXP packet_r) { MAYFAIL
    if (TYPEOF(packet_r) != RAWSXP) {
        error("packet must be a raw vector");
    }

    // Generate a response to ping
    const uint8_t* pong = R_TRY(return_ping, RAW(packet_r));

    size_t pong_size = R_TRY(morloc_packet_size, pong);

    SEXP result_r = PROTECT(allocVector(RAWSXP, pong_size));
    memcpy(RAW(result_r), pong, pong_size);

    UNPROTECT(1);
    return result_r;
}


SEXP morloc_make_fail_packet(SEXP failure_message_r) { MAYFAIL
    const char* failure_message = CHAR(STRING_ELT(failure_message_r, 0));
    uint8_t* fail_packet = make_fail_packet(failure_message);

    size_t packet_size = R_TRY(morloc_packet_size, fail_packet);

    SEXP packet_r = PROTECT(allocVector(RAWSXP, packet_size));
    memcpy(RAW(packet_r), fail_packet, packet_size);

    UNPROTECT(1);
    return packet_r;
}


SEXP extract_element_by_name(SEXP list, const char* key) {
  // Ensure inputs are correct types
  if (TYPEOF(list) != VECSXP) error("Input must be a list");

  // Get list names attribute
  SEXP names = Rf_getAttrib(list, R_NamesSymbol);

  // Iterate through list elements
  for (int i = 0; i < Rf_length(list); i++) {
    const char *current_name = CHAR(STRING_ELT(names, i));

    if (strcmp(key, current_name) == 0) {
      return VECTOR_ELT(list, i);  // Return matching element
    }
  }

  return R_NilValue;  // Return NULL if name not found
}


SEXP morloc_remote_call(SEXP midx, SEXP socket_path, SEXP cache_path, SEXP resources, SEXP arg_packets) { MAYFAIL
    // Protect all R inputs immediately
    PROTECT(socket_path);
    PROTECT(cache_path);
    PROTECT(resources);
    PROTECT(arg_packets = coerceVector(arg_packets, VECSXP));

    // Convert basic parameters
    int c_midx = INTEGER(midx)[0];
    const char* c_socket_path = CHAR(STRING_ELT(socket_path, 0));
    const char* c_cache_path = CHAR(STRING_ELT(cache_path, 0));

    // Extract resources using safe macro
    resources_t c_resources;
    c_resources.memory = INTEGER(extract_element_by_name(resources, "memory"))[0];
    c_resources.time = INTEGER(extract_element_by_name(resources, "time"))[0];
    c_resources.cpus = INTEGER(extract_element_by_name(resources, "cpus"))[0];
    c_resources.gpus = INTEGER(extract_element_by_name(resources, "gpus"))[0];

    // Process argument packets with type checking
    size_t nargs = LENGTH(arg_packets);
    const uint8_t** c_arg_packets = (const uint8_t**) R_alloc(nargs, sizeof(uint8_t*));

    for(size_t i = 0; i < nargs; i++) {
        SEXP raw_vec = PROTECT(VECTOR_ELT(arg_packets, i));
        if(TYPEOF(raw_vec) != RAWSXP) {
            Rf_error("arg_packets must contain only raw vectors");
        }
        c_arg_packets[i] = (uint8_t*)RAW(raw_vec);
        UNPROTECT(1);
    }

    // Execute remote call
    uint8_t* result_packet = R_TRY(
        remote_call,
        c_midx,
        c_socket_path,
        c_cache_path,
        &c_resources,
        c_arg_packets,
        nargs
    );

    // Validate and copy result
    size_t packet_size = R_TRY(morloc_packet_size, result_packet);
    if(!result_packet || packet_size == 0) {
        if(result_packet) free(result_packet);
        error("Invalid result packet from remote call");
    }

    SEXP result_packet_r = PROTECT(allocVector(RAWSXP, packet_size));
    memcpy(RAW(result_packet_r), result_packet, packet_size);
    free(result_packet);

    // Cleanup and return
    UNPROTECT(4);  // socket_path, cache_path, resources, arg_packets
    UNPROTECT(1);  // result_packet_r
    return result_packet_r;
}


// }}} exported functions


void R_init_rmorloc(DllInfo *info) {
    R_CallMethodDef callMethods[] = {
        {"morloc_start_daemon", (DL_FUNC) &morloc_start_daemon, 4},
        {"morloc_wait_for_client", (DL_FUNC) &morloc_wait_for_client, 1},
        {"morloc_read_morloc_call_packet", (DL_FUNC) &morloc_read_morloc_call_packet, 1},
        {"morloc_send_packet_to_foreign_server", (DL_FUNC) &morloc_send_packet_to_foreign_server, 2},
        {"morloc_stream_from_client", (DL_FUNC) &morloc_stream_from_client, 1},
        {"morloc_close_socket", (DL_FUNC) &morloc_close_socket, 1},
        {"morloc_foreign_call", (DL_FUNC) &morloc_foreign_call, 3},
        {"morloc_get_value", (DL_FUNC) &morloc_get_value, 2},
        {"morloc_put_value", (DL_FUNC) &morloc_put_value, 2},
        {"morloc_is_ping", (DL_FUNC) &morloc_is_ping, 1},
        {"morloc_is_local_call", (DL_FUNC) &morloc_is_local_call, 1},
        {"morloc_is_remote_call", (DL_FUNC) &morloc_is_remote_call, 1},
        {"morloc_remote_call", (DL_FUNC) &morloc_is_remote_call, 1},
        {"morloc_pong", (DL_FUNC) &morloc_pong, 1},
        {"morloc_make_fail_packet", (DL_FUNC) &morloc_make_fail_packet, 1},
        {NULL, NULL, 0}
    };

    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
}
