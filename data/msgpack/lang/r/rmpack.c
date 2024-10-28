#include <R.h>
#include <Rinternals.h>
#include "mlcmpack.h"

// Helper function to convert R object to Anything
Anything* r_to_anything(SEXP obj, const Schema* schema) {
  Anything* result = (Anything*)malloc(sizeof(Anything));
  result->type = schema->type;
  result->key = NULL;

  switch(schema->type){
    case MORLOC_NIL:
      result->size = 0;
      result->data.nil_val = 0x00;
      break;
    case MORLOC_BOOL:
      result->size = 0;
      result->data.bool_val = LOGICAL(obj)[0];
      break;
    case MORLOC_INT:
      result->size = 0;
      result->data.int_val = INTEGER(obj)[0];
      break;
    case MORLOC_FLOAT:
      result->size = 0;
      result->data.double_val = REAL(obj)[0];
      break;
    case MORLOC_STRING:
      {
        const char* str = CHAR(STRING_ELT(obj, 0));
        result->size = strlen(str);
        result->data.char_arr = strdup(str);
      }
      break;
    case MORLOC_BINARY:
      {
        result->size = LENGTH(obj);
        result->data.char_arr = (char*)malloc(result->size);
        memcpy(result->data.char_arr, RAW(obj), result->size);
      }
      break;
    case MORLOC_ARRAY:
      {
        const Schema* element_schema = schema->parameters[0];
        result->size = LENGTH(obj);
        result->data.obj_arr = (Anything**)malloc(result->size * sizeof(Anything*));
        for (size_t i = 0; i < result->size; i++) {
          result->data.obj_arr[i] = r_to_anything(VECTOR_ELT(obj, i), element_schema);
        }
      }
      break;
    case MORLOC_TUPLE:
      {
        result->size = LENGTH(obj);
        result->data.obj_arr = (Anything**)malloc(result->size * sizeof(Anything*));
        for (size_t i = 0; i < result->size; i++) {
          result->data.obj_arr[i] = r_to_anything(VECTOR_ELT(obj, i), schema->parameters[i]);
        }
      }
      break;
    case MORLOC_MAP:
      {
        result->size = LENGTH(obj);
        result->data.obj_arr = (Anything**)malloc(result->size * sizeof(Anything*));
        SEXP names = getAttrib(obj, R_NamesSymbol);
        for (size_t i = 0; i < result->size; i++) {
          result->data.obj_arr[i] = r_to_anything(VECTOR_ELT(obj, i), schema->parameters[i]);
          result->data.obj_arr[i]->key = strdup(CHAR(STRING_ELT(names, i)));
        }
      }
      break;
    case MORLOC_BOOL_ARRAY:
      {
        result->size = LENGTH(obj);
        result->data.bool_arr = (bool*)malloc(result->size * sizeof(bool));
        for (size_t i = 0; i < result->size; i++) {
          result->data.bool_arr[i] = LOGICAL(obj)[i];
        }
      }
      break;
    case MORLOC_INT_ARRAY:
      {
        result->size = LENGTH(obj);
        result->data.int_arr = (int*)malloc(result->size * sizeof(int));
        memcpy(result->data.int_arr, INTEGER(obj), result->size * sizeof(int));
      }
      break;
    case MORLOC_FLOAT_ARRAY:
      {
        result->size = LENGTH(obj);
        result->data.float_arr = (double*)malloc(result->size * sizeof(double));
        memcpy(result->data.float_arr, REAL(obj), result->size * sizeof(double));
      }
      break;
    case MORLOC_EXT:
      // Assuming EXT is handled as raw bytes in R
      {
        result->size = LENGTH(obj);
        result->data.char_arr = (char*)malloc(result->size);
        memcpy(result->data.char_arr, RAW(obj), result->size);
      }
      break;
    default:
      free(result);
      error("Unsupported schema type");
      return NULL;
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
