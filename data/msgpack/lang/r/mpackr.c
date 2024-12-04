#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Arith.h>

#include "mlcmpack.h"

#include <stdint.h>
#include <stdbool.h>
#include <limits.h>


SEXP to_mesgpack(SEXP r_obj, SEXP r_schema_str);
SEXP from_mesgpack(SEXP r_packed, SEXP r_schema_str);
void* to_voidstar(void* dest, SEXP obj, const Schema* schema);
SEXP from_voidstar(const void* data, const Schema* schema);


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

void* to_voidstar(void* dest, SEXP obj, const Schema* schema) {
    if(!dest){
      dest = get_ptr(schema);
    }

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
            *((bool*)dest) = (LOGICAL(obj)[0] == TRUE);
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
            switch(TYPEOF(obj)){
                case CHARSXP:
                    const char* str = CHAR(obj);
                    size_t str_len = strlen(str);  // Do not include null terminator
                    Array* array = array_data(dest, schema->parameters[0]->width, str_len);
                    memcpy(array->data, str, str_len);
                    break;
                case STRSXP:
                    if (LENGTH(obj) == 1) {
                        SEXP elem = STRING_ELT(obj, 0);
                        const char* str = CHAR(elem);
                        size_t str_len = strlen(str);  // Do not include null terminator
                        Array* array = array_data(dest, schema->parameters[0]->width, str_len);
                        memcpy(array->data, str, str_len);
                    } else {
                        error("Expected character of length 1");
                    }
                    break;
              default:
                error("Expected a character type");
                break;
            }
            break;
        case MORLOC_ARRAY:
            Array* array = array_data(dest, schema->parameters[0]->width, LENGTH(obj));
            int length = LENGTH(obj);
          
            switch (TYPEOF(obj)) {
                case STRSXP:
                    {
                        if(schema->parameters[0]->type == MORLOC_STRING){
                            Schema* element_schema = schema->parameters[0];
                            for(size_t i = 0; i < array->size; i++){
                                SEXP elem = STRING_ELT(obj, i);
                                to_voidstar(array->data + i * element_schema->width, elem, element_schema); 
                            }
                        } else {
                            error("Expected character vector of length 1, but got length %d", length);
                        }
                    }
                    break;
                case VECSXP:  // This handles lists
                    for (int i = 0; i < length; i++) {
                        SEXP elem = VECTOR_ELT(obj, i);
                        void* element_ptr = (char*)array->data + i * schema->parameters[0]->width;
                        to_voidstar(element_ptr, elem, schema->parameters[0]);
                    }
                    break;
                case LGLSXP:
                    for (int i = 0; i < length; i++) {
                        SEXP elem = PROTECT(ScalarLogical(LOGICAL(obj)[i]));
                        void* element_ptr = (char*)array->data + i * schema->parameters[0]->width;
                        to_voidstar(element_ptr, elem, schema->parameters[0]);
                        UNPROTECT(1);
                    }
                    break;
                case INTSXP:
                    for (int i = 0; i < length; i++) {
                        SEXP elem = PROTECT(ScalarInteger(INTEGER(obj)[i]));
                        void* element_ptr = (char*)array->data + i * schema->parameters[0]->width;
                        to_voidstar(element_ptr, elem, schema->parameters[0]);
                        UNPROTECT(1);
                    }
                    break;
                case REALSXP:
                    for (int i = 0; i < length; i++) {
                        SEXP elem = PROTECT(ScalarReal(REAL(obj)[i]));
                        void* element_ptr = (char*)array->data + i * schema->parameters[0]->width;
                        to_voidstar(element_ptr, elem, schema->parameters[0]);
                        UNPROTECT(1);
                    }
                    break;
                case RAWSXP:  // Raw vectors
                    if (schema->parameters[0]->type != MORLOC_UINT8) {
                        error("Expected MORLOC_UINT8 for raw vector");
                    }
                    memcpy(array->data, RAW(obj), length * sizeof(uint8_t));
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
                    to_voidstar(dest + schema->offsets[i], item, schema->parameters[i]);
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
                            to_voidstar(dest + schema->offsets[i], value, schema->parameters[i]);
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



SEXP from_voidstar(const void* data, const Schema* schema) {
    SEXP obj = R_NilValue;
    switch (schema->type) {
        case MORLOC_NIL:
            return R_NilValue;
        case MORLOC_BOOL:
            obj = ScalarLogical(*(bool*)data);
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
            Array* str_array = (Array*)data;
            SEXP chr = PROTECT(mkCharLen(str_array->data, str_array->size));
            obj = PROTECT(ScalarString(chr));
            UNPROTECT(2);
            break;
        }
        case MORLOC_ARRAY:
            {
                Array* array = (Array*)data;
                Schema* element_schema = schema->parameters[0];
                
                switch(element_schema->type){
                    case MORLOC_BOOL:
                        obj = PROTECT(allocVector(LGLSXP, array->size));
                        for (size_t i = 0; i < array->size; i++) {
                            LOGICAL(obj)[i] = *(bool*)((char*)array->data + i * sizeof(bool)) ? TRUE : FALSE;
                        }
                        UNPROTECT(1);
                        break;
                    case MORLOC_SINT8:
                        obj = PROTECT(allocVector(INTSXP, array->size));
                        for (size_t i = 0; i < array->size; i++) {
                            INTEGER(obj)[i] = (int)(*(int8_t*)((char*)array->data + i * sizeof(int8_t)));
                        }
                        UNPROTECT(1);
                        break;
                    case MORLOC_SINT16:
                        obj = PROTECT(allocVector(INTSXP, array->size));
                        for (size_t i = 0; i < array->size; i++) {
                            INTEGER(obj)[i] = (int)(*(int16_t*)((char*)array->data + i * sizeof(int16_t)));
                        }
                        UNPROTECT(1);
                        break;
                    case MORLOC_SINT32:
                        obj = PROTECT(allocVector(INTSXP, array->size));
                        memcpy(INTEGER(obj), array->data, array->size * sizeof(int32_t));
                        UNPROTECT(1);
                        break;
                    case MORLOC_SINT64:
                        obj = PROTECT(allocVector(REALSXP, array->size));
                        for (size_t i = 0; i < array->size; i++) {
                            REAL(obj)[i] = (double)(*(int64_t*)((char*)array->data + i * sizeof(int64_t)));
                        }
                        UNPROTECT(1);
                        break;
                    // Interpret the uint8 as a raw vector
                    case MORLOC_UINT8:
                        obj = PROTECT(allocVector(RAWSXP, array->size));
                        memcpy(RAW(obj), array->data, array->size * sizeof(uint8_t));
                        UNPROTECT(1);
                        break;
                    case MORLOC_UINT16:
                        obj = PROTECT(allocVector(INTSXP, array->size));
                        for (size_t i = 0; i < array->size; i++) {
                            INTEGER(obj)[i] = (int)(*(uint16_t*)((char*)array->data + i * sizeof(uint16_t)));
                        }
                        UNPROTECT(1);
                        break;
                    case MORLOC_UINT32:
                        obj = PROTECT(allocVector(REALSXP, array->size));
                        for (size_t i = 0; i < array->size; i++) {
                            REAL(obj)[i] = (double)(*(uint32_t*)((char*)array->data + i * sizeof(uint32_t)));
                        }
                        UNPROTECT(1);
                        break;
                    case MORLOC_UINT64:
                        obj = PROTECT(allocVector(REALSXP, array->size));
                        for (size_t i = 0; i < array->size; i++) {
                            REAL(obj)[i] = (double)(*(uint64_t*)((char*)array->data + i * sizeof(uint64_t)));
                        }
                        UNPROTECT(1);
                        break;
                    case MORLOC_FLOAT32:
                        obj = PROTECT(allocVector(REALSXP, array->size));
                        for (size_t i = 0; i < array->size; i++) {
                            REAL(obj)[i] = (double)(*(float*)((char*)array->data + i * sizeof(float)));
                        }
                        UNPROTECT(1);
                        break;
                    case MORLOC_FLOAT64:
                        obj = PROTECT(allocVector(REALSXP, array->size));
                        memcpy(REAL(obj), array->data, array->size * sizeof(double));
                        UNPROTECT(1);
                        break;
                    case MORLOC_STRING:
                        {
                            obj = PROTECT(allocVector(STRSXP, array->size));
                            char* start = (char*)array->data;
                            size_t width = schema->width;
                            for (size_t i = 0; i < array->size; i++) {
                                Array* str_array = (Array*)(start + i * width);
                                SEXP item = PROTECT(mkCharLen(str_array->data, str_array->size));
                                UNPROTECT(1);
                                SET_STRING_ELT(obj, i, item);
                            }
                            UNPROTECT(1);
                        }
                        break;
                    default:
                        {
                            obj = allocVector(VECSXP, array->size);
                            char* start = (char*)array->data;
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
    PROTECT(r_obj);
    PROTECT(r_schema_str);

    const char* schema_str = CHAR(STRING_ELT(r_schema_str, 0));
    Schema* schema = parse_schema(&schema_str);

    if (!schema) {
        UNPROTECT(2);
        error("Failed to parse schema");
    }

    void* data = to_voidstar(NULL, r_obj, schema);

    if (!data) {
        free_schema(schema);
        UNPROTECT(2);
        error("Failed to convert R object to Anything");
    }

    char* packed_data = NULL;
    size_t packed_size = 0;
    int result = pack_with_schema(data, schema, &packed_data, &packed_size);

    if (result != 0 || !packed_data) {
        free_schema(schema);
        UNPROTECT(2);
        error("Packing failed");
    }

    SEXP r_packed = PROTECT(allocVector(RAWSXP, packed_size));
    memcpy(RAW(r_packed), packed_data, packed_size);

    // Clean up
    free(packed_data);
    free_schema(schema);

    UNPROTECT(3);
    return r_packed;
}



// R-callable function to unpack to R object
SEXP from_mesgpack(SEXP r_packed, SEXP r_schema_str) {
    PROTECT(r_packed);
    PROTECT(r_schema_str);
    
    const char* schema_str = CHAR(STRING_ELT(r_schema_str, 0));
    Schema* schema = parse_schema(&schema_str);
    if (!schema) {
        UNPROTECT(2);
        error("Failed to parse schema");
    }

    const char* packed_data = (const char*)RAW(r_packed);
    size_t packed_size = LENGTH(r_packed);

    void* unpacked_data = NULL;
    int result = unpack_with_schema(packed_data, packed_size, schema, &unpacked_data);

    if (result != 0 || !unpacked_data) {
        free_schema(schema);
        UNPROTECT(2);
        error("Unpacking failed");
    }

    SEXP r_unpacked = PROTECT(from_voidstar(unpacked_data, schema));
    
    // Assuming unpack_with_schema allocates memory for unpacked_data
    free(unpacked_data);
    free_schema(schema);

    UNPROTECT(3);
    return r_unpacked;
}


SEXP r_to_mesgpack(SEXP r_obj, SEXP r_schema_str){
    PROTECT(r_obj);
    PROTECT(r_schema_str);
    const char* schema_str = CHAR(STRING_ELT(r_schema_str, 0));
    Schema* schema = parse_schema(&schema_str);

    void* voidstar = to_voidstar(NULL, r_obj, schema);
    if (!voidstar) {
        UNPROTECT(2);
        free_schema(schema);
        error("Failed to convert R object to Anything");
    }

    char* packed_data = NULL;
    size_t packed_size = 0;
    int result = pack_with_schema(voidstar, schema, &packed_data, &packed_size);
    if (result != 0 || !packed_data) {
        UNPROTECT(2);
        free_schema(schema);
        error("Packing failed");
    }

    SEXP r_packed = PROTECT(allocVector(RAWSXP, packed_size));
    memcpy(RAW(r_packed), packed_data, packed_size);

    // Clean up
    free(packed_data);
    free_schema(schema);

    UNPROTECT(3);
    return r_packed;
}

SEXP mesgpack_to_r(SEXP r_mesgpack, SEXP r_schema_str){
    PROTECT(r_mesgpack);
    PROTECT(r_schema_str);
    
    const char* schema_str = CHAR(STRING_ELT(r_schema_str, 0));
    Schema* schema = parse_schema(&schema_str);
    if (!schema) {
        UNPROTECT(2);
        error("Failed to parse schema");
    }

    const char* packed_data = (const char*)RAW(r_mesgpack);
    size_t packed_size = LENGTH(r_mesgpack);

    void* voidstar = NULL;
    int result = unpack_with_schema(packed_data, packed_size, schema, &voidstar);

    SEXP obj = from_voidstar(voidstar, schema);
    if (result != 0 || !packed_data) {
        UNPROTECT(2);
        free_schema(schema);
        error("Packing failed");
    }

    UNPROTECT(2);
    return obj;
}

void R_init_mlcmpack(DllInfo *info) {
    R_CallMethodDef callMethods[] = {
        {"to_voidstar", (DL_FUNC) &to_voidstar, 2},
        {"from_voidstar", (DL_FUNC) &from_voidstar, 2},
        {"to_mesgpack", (DL_FUNC) &to_mesgpack, 2},
        {"from_mesgpack", (DL_FUNC) &from_mesgpack, 2},
        {"mesgpack_to_r", (DL_FUNC) &mesgpack_to_r, 2},
        {"r_to_mesgpack", (DL_FUNC) &r_to_mesgpack, 2},
        {NULL, NULL, 0}
    };

    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
}
