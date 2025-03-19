#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Arith.h>

#include "morloc.h"

#include <stdint.h>
#include <stdbool.h>
#include <limits.h>

#define MAYFAIL char* child_errmsg_ = NULL;

#define TRY(fun, ...) \
    fun(__VA_ARGS__ __VA_OPT__(,) &child_errmsg_); \
    if(child_errmsg_ != NULL){ \
        error("Error (%s:%d in %s):\n%s", __FILE__, __LINE__, __func__, child_errmsg_); \
    }

# define TRY_WITH(clean, fun, ...) \
    fun(__VA_ARGS__ __VA_OPT__(,) &child_errmsg_); \
    if(child_errmsg_ != NULL){ \
        clean; \
        error("Error (%s:%d in %s):\n%s", __FILE__, __LINE__, __func__, child_errmsg_); \
    }

SEXP to_mesgpack(SEXP r_obj, SEXP r_schema_str);
SEXP from_mesgpack(SEXP r_packed, SEXP r_schema_str);
void* to_voidstar(SEXP obj, const Schema* schema);
SEXP from_voidstar(const void* data, const Schema* schema);

// Shared memory functions
SEXP shm_start(SEXP shm_basename_r, SEXP shm_size_r);
SEXP shm_close();
SEXP to_shm(SEXP obj, SEXP schema_str_r);
SEXP from_shm(SEXP relptr_r, SEXP schema_str_r);

size_t get_shm_size(const Schema* schema, SEXP obj) {
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
                        error("Unsupported type in to_voidstar array: %s", type2char(TYPEOF(obj)));
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

void* to_voidstar_r(void* dest, void** cursor, SEXP obj, const Schema* schema){
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
                array->data = TRY(abs2rel, *cursor); 

                absptr_t tmp_ptr = TRY(rel2abs, array->data);
                memcpy(tmp_ptr, str, array->size);

                // move cursor to the location after the copied data
                *cursor = (void*)(*(char**)cursor + array->size);
            }
            break;
        case MORLOC_ARRAY:
            Array* array = (Array*)dest; 
            array->size = (size_t)length(obj);
            array->data = TRY(abs2rel, *cursor);
            Schema* element_schema = schema->parameters[0];
            char* start;
          
            switch (TYPEOF(obj)) {
                case STRSXP:
                    {
                        if(element_schema->type == MORLOC_STRING){
                            // set the cursor the the location after the array headers
                            *cursor = (void*)(*(char**)cursor + array->size * element_schema->width); 
                            start = TRY(rel2abs, array->data);
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
                    absptr_t tmp_ptr = TRY(rel2abs, array->data);
                    memcpy(tmp_ptr, RAW(obj), array->size * sizeof(uint8_t));
                    *cursor = (void*)(*(char**)cursor + array->size * sizeof(uint8_t)); 
                    break;
                case VECSXP:  // This handles lists
                    *cursor = (void*)(*(char**)cursor + array->size * element_schema->width); 
                    start = TRY(rel2abs, array->data);
                    for (int i = 0; i < array->size; i++) {
                        SEXP elem = VECTOR_ELT(obj, i);
                        to_voidstar_r(start + i * element_schema->width, cursor, elem, element_schema);
                    }
                    break;

                case LGLSXP:
                    *cursor = (void*)(*(char**)cursor + array->size * element_schema->width); 
                    start = TRY(rel2abs, array->data);
                    for (int i = 0; i < array->size; i++) {
                        SEXP elem = PROTECT(ScalarLogical(LOGICAL(obj)[i]));
                        to_voidstar_r(start + i, cursor, elem, element_schema);
                        UNPROTECT(1);
                    }
                    break;
                case INTSXP:
                    *cursor = (void*)(*(char**)cursor + array->size * element_schema->width); 
                    start = TRY(rel2abs, array->data);
                    for (int i = 0; i < array->size; i++) {
                        SEXP elem = PROTECT(ScalarInteger(INTEGER(obj)[i]));
                        to_voidstar_r(start + i * element_schema->width, cursor, elem, element_schema);
                        UNPROTECT(1);
                    }
                    break;
                case REALSXP:
                    *cursor = (void*)(*(char**)cursor + array->size * element_schema->width); 
                    start = TRY(rel2abs, array->data);
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


void* to_voidstar(SEXP obj, const Schema* schema) {
    MAYFAIL

    size_t total_size = get_shm_size(schema, obj);

    void* dest = TRY(shmalloc, total_size);

    void* cursor = (void*)((char*)dest + schema->width);

    return to_voidstar_r(dest, &cursor, obj, schema);
}


SEXP from_voidstar(const void* data, const Schema* schema) {
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
                absptr_t tmp_ptr = TRY(rel2abs, raw_array->data);
                obj = PROTECT(allocVector(RAWSXP, raw_array->size));
                memcpy(RAW(obj), tmp_ptr, raw_array->size);
                UNPROTECT(1);
            } else {
                Array* str_array = (Array*)data;
                absptr_t tmp_ptr = TRY(rel2abs, str_array->data);
                SEXP chr = PROTECT(mkCharLen(tmp_ptr, str_array->size));
                obj = PROTECT(ScalarString(chr));
                UNPROTECT(2);
            }
            break;
        }
        case MORLOC_ARRAY:
            {
                Array* array = (Array*)data;
                Schema* element_schema = schema->parameters[0];
                char* start;
                
                switch(element_schema->type){
                    case MORLOC_BOOL:
                        start = (char*)TRY(rel2abs, array->data);
                        obj = PROTECT(allocVector(LGLSXP, array->size));
                        for (size_t i = 0; i < array->size; i++) {
                            LOGICAL(obj)[i] = (bool)*(uint8_t*)(start + i) ? TRUE : FALSE;
                        }
                        UNPROTECT(1);
                        break;
                    case MORLOC_SINT8:
                        start = (char*)TRY(rel2abs, array->data);
                        obj = PROTECT(allocVector(INTSXP, array->size));
                        for (size_t i = 0; i < array->size; i++) {
                            INTEGER(obj)[i] = (int)(*(int8_t*)(start + i * sizeof(int8_t)));
                        }
                        UNPROTECT(1);
                        break;
                    case MORLOC_SINT16:
                        start = (char*)TRY(rel2abs, array->data);
                        obj = PROTECT(allocVector(INTSXP, array->size));
                        for (size_t i = 0; i < array->size; i++) {
                            INTEGER(obj)[i] = (int)(*(int16_t*)(start + i * sizeof(int16_t)));
                        }
                        UNPROTECT(1);
                        break;
                    case MORLOC_SINT32:
                        absptr_t tmp_ptr = TRY(rel2abs, array->data);
                        obj = PROTECT(allocVector(INTSXP, array->size));
                        memcpy(INTEGER(obj), tmp_ptr, array->size * sizeof(int32_t));
                        UNPROTECT(1);
                        break;
                    case MORLOC_SINT64:
                        start = (char*)TRY(rel2abs, array->data);
                        obj = PROTECT(allocVector(REALSXP, array->size));
                        for (size_t i = 0; i < array->size; i++) {
                            REAL(obj)[i] = (double)(*(int64_t*)(start + i * sizeof(int64_t)));
                        }
                        UNPROTECT(1);
                        break;
                    // Interpret the uint8 as a raw vector
                    case MORLOC_UINT8:
                        start = (char*)TRY(rel2abs, array->data);
                        obj = PROTECT(allocVector(RAWSXP, array->size));
                        memcpy(RAW(obj), start, array->size * sizeof(uint8_t));
                        UNPROTECT(1);
                        break;
                    case MORLOC_UINT16:
                        start = (char*)TRY(rel2abs, array->data);
                        obj = PROTECT(allocVector(INTSXP, array->size));
                        for (size_t i = 0; i < array->size; i++) {
                            INTEGER(obj)[i] = (int)(*(uint16_t*)(start + i * sizeof(uint16_t)));
                        }
                        UNPROTECT(1);
                        break;
                    case MORLOC_UINT32:
                        start = (char*)TRY(rel2abs, array->data);
                        obj = PROTECT(allocVector(REALSXP, array->size));
                        for (size_t i = 0; i < array->size; i++) {
                            REAL(obj)[i] = (double)(*(uint32_t*)(start + i * sizeof(uint32_t)));
                        }
                        UNPROTECT(1);
                        break;
                    case MORLOC_UINT64:
                        start = (char*)TRY(rel2abs, array->data);
                        obj = PROTECT(allocVector(REALSXP, array->size));
                        for (size_t i = 0; i < array->size; i++) {
                            REAL(obj)[i] = (double)(*(uint64_t*)(start + i * sizeof(uint64_t)));
                        }
                        UNPROTECT(1);
                        break;
                    case MORLOC_FLOAT32:
                        start = (char*)TRY(rel2abs, array->data);
                        obj = PROTECT(allocVector(REALSXP, array->size));
                        for (size_t i = 0; i < array->size; i++) {
                            REAL(obj)[i] = (double)(*(float*)(start + i * sizeof(float)));
                        }
                        UNPROTECT(1);
                        break;
                    case MORLOC_FLOAT64:
                        start = (char*)TRY(rel2abs, array->data);
                        obj = PROTECT(allocVector(REALSXP, array->size));
                        memcpy(REAL(obj), start, array->size * sizeof(double));
                        UNPROTECT(1);
                        break;
                    case MORLOC_STRING:
                        {
                            start = (char*)TRY(rel2abs, array->data);
                            obj = PROTECT(allocVector(STRSXP, array->size));
                            size_t width = schema->width;
                            for (size_t i = 0; i < array->size; i++) {
                                Array* str_array = (Array*)(start + i * width);
                                absptr_t str_ptr = TRY_WITH(UNPROTECT(1), rel2abs, str_array->data);
                                SEXP item = PROTECT(mkCharLen(str_ptr, str_array->size));
                                UNPROTECT(1);
                                SET_STRING_ELT(obj, i, item);
                            }
                            UNPROTECT(1);
                        }
                        break;
                    default:
                        {
                            start = (char*)TRY(rel2abs, array->data);
                            obj = allocVector(VECSXP, array->size);
                            size_t width = element_schema->width;
                            for (size_t i = 0; i < array->size; i++) {
                                SEXP item = from_voidstar(start + width * i, element_schema);
                                if (item == R_NilValue) {
                                    obj = R_NilValue;
                                    goto error;
                                }
                                SET_VECTOR_ELT(obj, i, item);
                            }
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


SEXP to_mesgpack(SEXP r_obj, SEXP r_schema_str) {
    MAYFAIL

    PROTECT(r_obj);
    PROTECT(r_schema_str);
    char* child_errmsg = NULL;

    const char* schema_str = CHAR(STRING_ELT(r_schema_str, 0));
    Schema* schema = TRY_WITH(UNPROTECT(2), parse_schema, &schema_str);

    void* data = to_voidstar(r_obj, schema);

    if (!data) {
        free_schema(schema);
        UNPROTECT(2);
        error("Failed to convert R object to Anything");
    }

    char* packed_data = NULL;
    size_t packed_size = 0;
    int result = TRY_WITH(
        UNPROTECT(2); free_schema(schema),
        pack_with_schema,
        data,
        schema,
        &packed_data,
        &packed_size
    );

    SEXP r_packed = PROTECT(allocVector(RAWSXP, packed_size));
    memcpy(RAW(r_packed), packed_data, packed_size);

    // Clean up
    /* free(packed_data); */
    free_schema(schema);

    UNPROTECT(3);
    return r_packed;
}



// R-callable function to unpack to R object
SEXP from_mesgpack(SEXP r_packed, SEXP r_schema_str) {
    MAYFAIL

    PROTECT(r_packed);
    PROTECT(r_schema_str);
    
    const char* schema_str = CHAR(STRING_ELT(r_schema_str, 0));
    Schema* schema = TRY_WITH(UNPROTECT(2), parse_schema, &schema_str);

    const char* packed_data = (const char*)RAW(r_packed);
    size_t packed_size = LENGTH(r_packed);

    void* unpacked_data = NULL;
    int result = TRY_WITH(
        UNPROTECT(2); free_schema(schema),
        unpack_with_schema,
        packed_data,
        packed_size,
        schema,
        &unpacked_data
    );

    SEXP r_unpacked = PROTECT(from_voidstar(unpacked_data, schema));
    
    // Assuming unpack_with_schema allocates memory for unpacked_data
    /* free(unpacked_data); */
    free_schema(schema);

    UNPROTECT(3);
    return r_unpacked;
}


SEXP r_to_mesgpack(SEXP r_obj, SEXP r_schema_str){
    MAYFAIL

    PROTECT(r_obj);
    PROTECT(r_schema_str);
    const char* schema_str = CHAR(STRING_ELT(r_schema_str, 0));
    Schema* schema = TRY_WITH(UNPROTECT(2), parse_schema, &schema_str);

    void* voidstar = to_voidstar(r_obj, schema);
    if (!voidstar) {
        UNPROTECT(2);
        free_schema(schema);
        error("Failed to convert R object to Anything");
    }

    char* packed_data = NULL;
    size_t packed_size = 0;
    int result = TRY_WITH(
        UNPROTECT(2); free_schema(schema),
        pack_with_schema,
        voidstar,
        schema,
        &packed_data,
        &packed_size
    );

    SEXP r_packed = PROTECT(allocVector(RAWSXP, packed_size));
    memcpy(RAW(r_packed), packed_data, packed_size);

    // Clean up
    free(packed_data);
    free_schema(schema);

    UNPROTECT(3);
    return r_packed;
}

SEXP mesgpack_to_r(SEXP r_mesgpack, SEXP r_schema_str){
    MAYFAIL
    
    PROTECT(r_mesgpack);
    PROTECT(r_schema_str);
    
    const char* schema_str = CHAR(STRING_ELT(r_schema_str, 0));
    Schema* schema = TRY_WITH(UNPROTECT(2), parse_schema, &schema_str);
    if (!schema) {
        UNPROTECT(2);
        error("Failed to parse schema");
    }

    const char* packed_data = (const char*)RAW(r_mesgpack);
    size_t packed_size = LENGTH(r_mesgpack);

    void* voidstar = NULL;
    int result = TRY_WITH(
        UNPROTECT(2); free_schema(schema),
        unpack_with_schema,
        packed_data,
        packed_size,
        schema,
        &voidstar
    );

    SEXP obj = from_voidstar(voidstar, schema);

    UNPROTECT(2);
    return obj;
}


SEXP shm_start(SEXP shm_basename_r, SEXP shm_size_r) {
    MAYFAIL

    const char* shm_basename = CHAR(STRING_ELT(shm_basename_r, 0));
    size_t shm_size = (size_t)asInteger(shm_size_r);

    shm_t* shm = TRY(shinit, shm_basename, 0, shm_size);

    if (shm) {
        return R_NilValue; // Return NULL, representing success
    } else {
        error("Failed to open shared memory pool");
    }
}


SEXP shm_close() {
    MAYFAIL
    TRY(shclose)
    return R_NilValue; // Return NULL
}

SEXP to_shm(SEXP obj, SEXP schema_str_r) {
    MAYFAIL

    const char* schema_str = CHAR(STRING_ELT(schema_str_r, 0));

    Schema* schema = TRY(parse_schema, &schema_str);

    absptr_t voidstar = to_voidstar(obj, schema);

    free_schema(schema);

    // relptr_t type is the integer representation of a pointer, so a 64bit integer
    relptr_t relptr = TRY(abs2rel, voidstar);

    // Return relptr as a numeric scalar
    // Casting a pointer to a double is disturbing, but the 32bit R integers to
    // small to represent reasonable possible allocations.
    return ScalarReal((double)relptr);
}


SEXP from_shm(SEXP relptr_r, SEXP schema_str_r) {
    MAYFAIL

    relptr_t relptr = (relptr_t)asReal(relptr_r);
    const char* schema_str = CHAR(STRING_ELT(schema_str_r, 0));
    char* child_errmsg = NULL;

    Schema* schema = TRY(parse_schema, &schema_str);

    absptr_t voidstar = TRY(rel2abs, relptr);

    SEXP obj = from_voidstar(voidstar, schema);

    free_schema(schema);

    return obj;
}


void R_init_rmorloc(DllInfo *info) {
    R_CallMethodDef callMethods[] = {
        {"to_voidstar", (DL_FUNC) &to_voidstar, 2},
        {"from_voidstar", (DL_FUNC) &from_voidstar, 2},
        {"to_mesgpack", (DL_FUNC) &to_mesgpack, 2},
        {"from_mesgpack", (DL_FUNC) &from_mesgpack, 2},
        {"mesgpack_to_r", (DL_FUNC) &mesgpack_to_r, 2},
        {"r_to_mesgpack", (DL_FUNC) &r_to_mesgpack, 2},
        {"shm_start", (DL_FUNC) &shm_start, 2},
        {"shm_close", (DL_FUNC) &shm_close, 0},
        {"to_shm", (DL_FUNC) &to_shm, 2},
        {"from_shm", (DL_FUNC) &from_shm, 2},
        {NULL, NULL, 0}
    };

    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
}
