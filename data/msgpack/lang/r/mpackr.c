#include <R.h>
#include <Rinternals.h>
#include "mlcmpack.h"

Anything* r_to_anything(SEXP obj, const Schema* schema) {
  Anything* result = (Anything*)malloc(sizeof(Anything));
  if (!result) {
    error("Failed to allocate memory for Anything");
  }
  result->type = schema->type;
  result->key = NULL;

  switch(schema->type){
    case MORLOC_NIL:
      result->size = 0;
      result->data.nil_val = 0x00;
      break;
    case MORLOC_BOOL:
      if (TYPEOF(obj) != LGLSXP) {
        free(result);
        error("Expected logical, got %s", type2char(TYPEOF(obj)));
      }
      result->size = 0;
      result->data.bool_val = LOGICAL(obj)[0] == NA_LOGICAL ? false : LOGICAL(obj)[0];
      break;
    case MORLOC_INT:
      result->size = 0;
      if (TYPEOF(obj) == INTSXP) {
        result->data.int_val = INTEGER(obj)[0];
      } else if (TYPEOF(obj) == REALSXP) {
        double val = REAL(obj)[0];
        if (val > INT_MAX || val < INT_MIN) {
          free(result);
          error("Double value %f out of range for integer", val);
        }
        result->data.int_val = (int)val;
      } else {
        free(result);
        error("Expected integer or double, got %s", type2char(TYPEOF(obj)));
      }
      break;
    case MORLOC_FLOAT:
      if (TYPEOF(obj) != REALSXP) {
        free(result);
        error("Expected double, got %s", type2char(TYPEOF(obj)));
      }
      result->size = 0;
      result->data.double_val = REAL(obj)[0];
      break;
    case MORLOC_STRING:
      if (TYPEOF(obj) != STRSXP) {
        free(result);
        error("Expected string, got %s", type2char(TYPEOF(obj)));
      }
      {
        const char* str = CHAR(STRING_ELT(obj, 0));
        result->size = strlen(str);
        result->data.char_arr = strdup(str);
        if (!result->data.char_arr) {
          free(result);
          error("Failed to allocate memory for string");
        }
      }
      break;
    case MORLOC_BINARY:
      if (TYPEOF(obj) != RAWSXP) {
        free(result);
        error("Expected raw vector, got %s", type2char(TYPEOF(obj)));
      }
      result->size = LENGTH(obj);
      result->data.char_arr = (char*)malloc(result->size);
      if (!result->data.char_arr) {
        free(result);
        error("Failed to allocate memory for binary data");
      }
      memcpy(result->data.char_arr, RAW(obj), result->size);
      break;
    case MORLOC_ARRAY:
      if (TYPEOF(obj) != VECSXP) {
        free(result);
        error("Expected list, got %s", type2char(TYPEOF(obj)));
      }
      {
        const Schema* element_schema = schema->parameters[0];
        result->size = LENGTH(obj);
        result->data.obj_arr = (Anything**)malloc(result->size * sizeof(Anything*));
        if (!result->data.obj_arr) {
          free(result);
          error("Failed to allocate memory for array");
        }
        for (size_t i = 0; i < result->size; i++) {
          result->data.obj_arr[i] = r_to_anything(VECTOR_ELT(obj, i), element_schema);
        }
      }
      break;
    case MORLOC_TUPLE:
      if (TYPEOF(obj) != VECSXP) {
        free(result);
        error("Expected list, got %s", type2char(TYPEOF(obj)));
      }
      {
        result->size = LENGTH(obj);
        if (result->size != schema->size) {
          free(result);
          error("Tuple size mismatch: expected %zu, got %zu", schema->size, result->size);
        }
        result->data.obj_arr = (Anything**)malloc(result->size * sizeof(Anything*));
        if (!result->data.obj_arr) {
          free(result);
          error("Failed to allocate memory for tuple");
        }
        for (size_t i = 0; i < result->size; i++) {
          result->data.obj_arr[i] = r_to_anything(VECTOR_ELT(obj, i), schema->parameters[i]);
        }
      }
      break;
    case MORLOC_MAP:
      if (TYPEOF(obj) != VECSXP) {
        free(result);
        error("Expected list, got %s", type2char(TYPEOF(obj)));
      }
      {
        result->size = LENGTH(obj);
        result->data.obj_arr = (Anything**)malloc(result->size * sizeof(Anything*));
        if (!result->data.obj_arr) {
          free(result);
          error("Failed to allocate memory for map");
        }
        SEXP names = getAttrib(obj, R_NamesSymbol);
        if (names == R_NilValue) {
          free(result->data.obj_arr);
          free(result);
          error("Expected named list for map");
        }
        for (size_t i = 0; i < result->size; i++) {
          result->data.obj_arr[i] = r_to_anything(VECTOR_ELT(obj, i), schema->parameters[i]);
          result->data.obj_arr[i]->key = strdup(CHAR(STRING_ELT(names, i)));
          if (!result->data.obj_arr[i]->key) {
            // Clean up previously allocated memory
            for (size_t j = 0; j < i; j++) {
              free(result->data.obj_arr[j]->key);
              free(result->data.obj_arr[j]);
            }
            free(result->data.obj_arr);
            free(result);
            error("Failed to allocate memory for map key");
          }
        }
      }
      break;
    case MORLOC_BOOL_ARRAY:
      if (TYPEOF(obj) != LGLSXP) {
        free(result);
        error("Expected logical vector, got %s", type2char(TYPEOF(obj)));
      }
      {
        result->size = LENGTH(obj);
        result->data.bool_arr = (bool*)malloc(result->size * sizeof(bool));
        if (!result->data.bool_arr) {
          free(result);
          error("Failed to allocate memory for boolean array");
        }
        for (size_t i = 0; i < result->size; i++) {
          result->data.bool_arr[i] = LOGICAL(obj)[i] == NA_LOGICAL ? false : LOGICAL(obj)[i];
        }
      }
      break;
    case MORLOC_INT_ARRAY:
      if (TYPEOF(obj) != INTSXP) {
        free(result);
        error("Expected integer vector, got %s", type2char(TYPEOF(obj)));
      }
      {
        result->size = LENGTH(obj);
        result->data.int_arr = (int*)malloc(result->size * sizeof(int));
        if (!result->data.int_arr) {
          free(result);
          error("Failed to allocate memory for integer array");
        }
        memcpy(result->data.int_arr, INTEGER(obj), result->size * sizeof(int));
      }
      break;
    case MORLOC_FLOAT_ARRAY:
      if (TYPEOF(obj) != REALSXP) {
        free(result);
        error("Expected double vector, got %s", type2char(TYPEOF(obj)));
      }
      {
        result->size = LENGTH(obj);
        result->data.float_arr = (double*)malloc(result->size * sizeof(double));
        if (!result->data.float_arr) {
          free(result);
          error("Failed to allocate memory for float array");
        }
        memcpy(result->data.float_arr, REAL(obj), result->size * sizeof(double));
      }
      break;
    case MORLOC_EXT:
      if (TYPEOF(obj) != RAWSXP) {
        free(result);
        error("Expected raw vector for EXT type, got %s", type2char(TYPEOF(obj)));
      }
      {
        result->size = LENGTH(obj);
        result->data.char_arr = (char*)malloc(result->size);
        if (!result->data.char_arr) {
          free(result);
          error("Failed to allocate memory for EXT data");
        }
        memcpy(result->data.char_arr, RAW(obj), result->size);
      }
      break;
    default:
      free(result);
      error("Unsupported schema type");
  }
  return result;
}






// Helper function to convert Anything to R object
SEXP anything_to_r(const Anything* data, const Schema* schema) {
  SEXP result;
  switch(schema->type){
    case MORLOC_NIL:
      return R_NilValue;
    case MORLOC_BOOL:
      return ScalarLogical(data->data.bool_val);
    case MORLOC_INT:
      return ScalarInteger(data->data.int_val);
    case MORLOC_FLOAT:
      return ScalarReal(data->data.double_val);
    case MORLOC_STRING:
      return mkString(data->data.char_arr);
    case MORLOC_BINARY:
      {
        result = PROTECT(allocVector(RAWSXP, data->size));
        memcpy(RAW(result), data->data.char_arr, data->size);
        UNPROTECT(1);
      }
      break;
    case MORLOC_ARRAY:
      {
        result = PROTECT(allocVector(VECSXP, data->size));
        const Schema* element_schema = schema->parameters[0];
        for (size_t i = 0; i < data->size; i++) {
          SET_VECTOR_ELT(result, i, anything_to_r(data->data.obj_arr[i], element_schema));
        }
        UNPROTECT(1);
      }
      break;
    case MORLOC_TUPLE:
      {
        result = PROTECT(allocVector(VECSXP, data->size));
        for (size_t i = 0; i < data->size; i++) {
          SET_VECTOR_ELT(result, i, anything_to_r(data->data.obj_arr[i], schema->parameters[i]));
        }
        UNPROTECT(1);
      }
      break;
    case MORLOC_MAP:
      {
        result = PROTECT(allocVector(VECSXP, data->size));
        SEXP names = PROTECT(allocVector(STRSXP, data->size));
        for (size_t i = 0; i < data->size; i++) {
          SET_VECTOR_ELT(result, i, anything_to_r(data->data.obj_arr[i], schema->parameters[i]));
          SET_STRING_ELT(names, i, mkChar(data->data.obj_arr[i]->key));
        }
        setAttrib(result, R_NamesSymbol, names);
        UNPROTECT(2);
      }
      break;
    case MORLOC_BOOL_ARRAY:
      {
        result = PROTECT(allocVector(LGLSXP, data->size));
        for (size_t i = 0; i < data->size; i++) {
          LOGICAL(result)[i] = data->data.bool_arr[i];
        }
        UNPROTECT(1);
      }
      break;
    case MORLOC_INT_ARRAY:
      {
        result = PROTECT(allocVector(INTSXP, data->size));
        memcpy(INTEGER(result), data->data.int_arr, data->size * sizeof(int));
        UNPROTECT(1);
      }
      break;
    case MORLOC_FLOAT_ARRAY:
      {
        result = PROTECT(allocVector(REALSXP, data->size));
        memcpy(REAL(result), data->data.float_arr, data->size * sizeof(double));
        UNPROTECT(1);
      }
      break;
    case MORLOC_EXT:
      // Assuming EXT is handled as raw bytes in R
      {
        result = PROTECT(allocVector(RAWSXP, data->size));
        memcpy(RAW(result), data->data.char_arr, data->size);
        UNPROTECT(1);
      }
      break;
    default:
      error("Unsupported Anything type");
      return R_NilValue;
  }
  return result;
}


// R-callable function to pack R object
SEXP _mlcmpack_r_pack(SEXP r_obj, SEXP r_schema_str) {
    const char* schema_str = CHAR(STRING_ELT(r_schema_str, 0));
    Schema* schema = parse_schema(&schema_str);

    Anything* data = r_to_anything(r_obj, schema);

    char* packed_data;
    size_t packed_size;
    int result = pack_with_schema(data, schema, &packed_data, &packed_size);

    free_parsed_data(data);

    if (result != 0) {
        error("Packing failed");
    }

    SEXP r_packed = PROTECT(allocVector(RAWSXP, packed_size));
    memcpy(RAW(r_packed), packed_data, packed_size);
    free(packed_data);
    free_schema(schema);

    UNPROTECT(1);
    return r_packed;
}

// R-callable function to unpack to R object
SEXP _mlcmpack_r_unpack(SEXP r_packed, SEXP r_schema_str) {
    const char* schema_str = CHAR(STRING_ELT(r_schema_str, 0));
    Schema* schema = parse_schema(&schema_str);

    const char* packed_data = (const char*)RAW(r_packed);
    size_t packed_size = LENGTH(r_packed);

    Anything* unpacked_data;
    int result = unpack_with_schema(packed_data, packed_size, schema, &unpacked_data);

    if (result != 0) {
        error("Unpacking failed");
    }

    SEXP r_unpacked = anything_to_r(unpacked_data, schema);
    free_parsed_data(unpacked_data);
    free_schema(schema);

    return r_unpacked;
}


void R_init_mlcmpack(DllInfo *info) {
    R_CallMethodDef callMethods[] = {
        {"_mlcmpack_r_pack", (DL_FUNC) &_mlcmpack_r_pack, 2},
        {"_mlcmpack_r_unpack", (DL_FUNC) &_mlcmpack_r_unpack, 2},
        {NULL, NULL, 0}
    };

    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
}
