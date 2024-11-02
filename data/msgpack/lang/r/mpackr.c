#include <R.h>
#include <Rinternals.h>
#include "mlcmpack.h"


Anything* r_to_anything(SEXP obj, const Schema* schema);

Anything* r_to_anything_nil(Anything* result, SEXP obj, const Schema* schema) {
  result->size = 0;
  result->data.nil_val = 0x00;
  return result;
}

Anything* r_to_anything_bool(Anything* result, SEXP obj, const Schema* schema) {
  if (TYPEOF(obj) != LGLSXP) {
    free(result);
    error("Expected logical, got %s", type2char(TYPEOF(obj)));
  }
  result->size = 0;
  result->data.bool_val = LOGICAL(obj)[0] == NA_LOGICAL ? false : LOGICAL(obj)[0];
  return result;
}

Anything* r_to_anything_int(Anything* result, SEXP obj, const Schema* schema) {
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
  return result;
}

Anything* r_to_anything_float(Anything* result, SEXP obj, const Schema* schema) {
  if (TYPEOF(obj) != REALSXP) {
    free(result);
    error("Expected double, got %s", type2char(TYPEOF(obj)));
  }
  result->size = 0;
  result->data.double_val = REAL(obj)[0];
  return result;
}

Anything* r_to_anything_binary(Anything* result, SEXP obj, const Schema* schema) {
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
  return result;
}

Anything* r_to_anything_string(Anything* result, SEXP obj, const Schema* schema) {

    if (TYPEOF(obj) != STRSXP || LENGTH(obj) != 1) {
        free(result);
        error("Expected a single-element character vector, got %s with length %d", 
              type2char(TYPEOF(obj)), LENGTH(obj));
    }

    result->type = MORLOC_STRING;
    result->data.char_arr = (char*)CHAR(STRING_ELT(obj, 0));
    result->size = strlen(result->data.char_arr);

    return result;
}


Anything* r_to_anything_string_list(Anything* result, SEXP obj, const Schema* schema) {
    for (size_t i = 0; i < result->size; i++) {
        SEXP str = VECTOR_ELT(obj, i);
        if (str == NA_STRING) {
            error("NA strings are not supported");
        } else {
          result->data.obj_arr[i] = (Anything*)malloc(sizeof(Anything));
          result->data.obj_arr[i] = r_to_anything_string(
            result->data.obj_arr[i],
            str,
            schema
          );
       }
    }
    return result;
}


Anything* r_to_anything_string_vector(Anything* result, SEXP obj, const Schema* schema) {
    for (size_t i = 0; i < result->size; i++) {
        SEXP str = STRING_ELT(obj, i);
        if (str == NA_STRING) {
            error("NA strings are not supported");
        } else {
            const char* c_str = CHAR(str);
            result->data.obj_arr[i] = (Anything*)malloc(sizeof(Anything));
            result->data.obj_arr[i]->type = MORLOC_STRING;
            result->data.obj_arr[i]->size = strlen(c_str);
            result->data.obj_arr[i]->data.char_arr = strdup(c_str);
        }
    }
    return result;
}

Anything* r_to_anything_array(Anything* result, SEXP obj, const Schema* schema) {

   if (! (TYPEOF(obj) == VECSXP || TYPEOF(obj) == STRSXP)) {
     free(result);
     error("Expected list, got %s", type2char(TYPEOF(obj)));
   }

  const Schema* element_schema = schema->parameters[0];

  result->type = MORLOC_ARRAY;
  result->size = LENGTH(obj);
  result->data.obj_arr = (Anything**)malloc(result->size * sizeof(Anything*));
  if (!result->data.obj_arr) {
    free(result);
    error("Failed to allocate memory for array");
  }

  if (element_schema->type == MORLOC_STRING){
    if (TYPEOF(obj) == VECSXP) {
      return r_to_anything_string_list(result, obj, schema);
    } else if (TYPEOF(obj) == STRSXP) {
      return r_to_anything_string_vector(result, obj, schema);
    }
  }

  for (size_t i = 0; i < result->size; i++) {
    result->data.obj_arr[i] = r_to_anything(VECTOR_ELT(obj, i), element_schema);
  }
  return result;
}

Anything* r_to_anything_tuple(Anything* result, SEXP obj, const Schema* schema) {
  if (TYPEOF(obj) != VECSXP) {
    free(result);
    error("Expected list, got %s", type2char(TYPEOF(obj)));
  }
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
  return result;
}

Anything* r_to_anything_map(Anything* result, SEXP obj, const Schema* schema) {
  if (TYPEOF(obj) != VECSXP) {
    free(result);
    error("Expected list, got %s", type2char(TYPEOF(obj)));
  }
  
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
  return result;
}


Anything* r_to_anything_bool_array(Anything* result, SEXP obj, const Schema* schema) {
    result->size = 0;
    result->data.bool_arr = NULL;

    if (TYPEOF(obj) == LGLSXP) {
        // Handle logical vector
        result->size = LENGTH(obj);
        result->data.bool_arr = (bool*)malloc(result->size * sizeof(bool));
        if (!result->data.bool_arr) {
            free(result);
            error("Failed to allocate memory for boolean array");
        }
        for (size_t i = 0; i < result->size; i++) {
            result->data.bool_arr[i] = LOGICAL(obj)[i] == NA_LOGICAL ? false : LOGICAL(obj)[i];
        }
    } else if (TYPEOF(obj) == VECSXP) {
        // Handle list of logical values
        result->size = LENGTH(obj);
        result->data.bool_arr = (bool*)malloc(result->size * sizeof(bool));
        if (!result->data.bool_arr) {
            free(result);
            error("Failed to allocate memory for boolean array");
        }
        for (size_t i = 0; i < result->size; i++) {
            SEXP elem = VECTOR_ELT(obj, i);
            if (TYPEOF(elem) != LGLSXP || LENGTH(elem) != 1) {
                free(result->data.bool_arr);
                free(result);
                error("Element %zu in list is not a single logical value", i + 1);
            }
            result->data.bool_arr[i] = LOGICAL(elem)[0] == NA_LOGICAL ? false : LOGICAL(elem)[0];
        }
    } else {
        free(result);
        error("Expected logical vector or list of logical values, got %s", type2char(TYPEOF(obj)));
    }

    return result;
}


Anything* r_to_anything_int_array(Anything* result, SEXP obj, const Schema* schema) {
    result->size = 0;
    result->data.int_arr = NULL;

    if (TYPEOF(obj) == INTSXP || TYPEOF(obj) == REALSXP) {
        // Handle integer or double vector
        result->size = LENGTH(obj);
        result->data.int_arr = (int*)malloc(result->size * sizeof(int));
        if (!result->data.int_arr) {
            free(result);
            error("Failed to allocate memory for integer array");
        }
        if (TYPEOF(obj) == INTSXP) {
            memcpy(result->data.int_arr, INTEGER(obj), result->size * sizeof(int));
        } else {
            for (size_t i = 0; i < result->size; i++) {
                double val = REAL(obj)[i];
                if (val > INT_MAX || val < INT_MIN) {
                    free(result->data.int_arr);
                    free(result);
                    error("Double value %f at index %zu is out of range for integer", val, i);
                }
                result->data.int_arr[i] = (int)val;
            }
        }
    } else if (TYPEOF(obj) == VECSXP) {
        // Handle list of integers or doubles
        result->size = LENGTH(obj);
        result->data.int_arr = (int*)malloc(result->size * sizeof(int));
        if (!result->data.int_arr) {
            free(result);
            error("Failed to allocate memory for integer array");
        }
        for (size_t i = 0; i < result->size; i++) {
            SEXP elem = VECTOR_ELT(obj, i);
            if (TYPEOF(elem) == INTSXP && LENGTH(elem) == 1) {
                result->data.int_arr[i] = INTEGER(elem)[0];
            } else if (TYPEOF(elem) == REALSXP && LENGTH(elem) == 1) {
                double val = REAL(elem)[0];
                if (val > INT_MAX || val < INT_MIN) {
                    free(result->data.int_arr);
                    free(result);
                    error("Double value %f at index %zu is out of range for integer", val, i);
                }
                result->data.int_arr[i] = (int)val;
            } else {
                free(result->data.int_arr);
                free(result);
                error("Element %zu in list is not a single integer or double", i + 1);
            }
        }
    } else {
        free(result);
        error("Expected integer or double vector, or list of integers or doubles, got %s", type2char(TYPEOF(obj)));
    }

    return result;
}


Anything* r_to_anything_float_array(Anything* result, SEXP obj, const Schema* schema) {
    result->size = 0;
    result->data.float_arr = NULL;

    if (TYPEOF(obj) == REALSXP) {
        // Handle double vector
        result->size = LENGTH(obj);
        result->data.float_arr = (double*)malloc(result->size * sizeof(double));
        if (!result->data.float_arr) {
            free(result);
            error("Failed to allocate memory for float array");
        }
        memcpy(result->data.float_arr, REAL(obj), result->size * sizeof(double));
    } else if (TYPEOF(obj) == VECSXP) {
        // Handle list of doubles
        result->size = LENGTH(obj);
        result->data.float_arr = (double*)malloc(result->size * sizeof(double));
        if (!result->data.float_arr) {
            free(result);
            error("Failed to allocate memory for float array");
        }
        for (size_t i = 0; i < result->size; i++) {
            SEXP elem = VECTOR_ELT(obj, i);
            if (TYPEOF(elem) != REALSXP || LENGTH(elem) != 1) {
                free(result->data.float_arr);
                free(result);
                error("Element %zu in list is not a single double", i + 1);
            }
            result->data.float_arr[i] = REAL(elem)[0];
        }
    } else {
        free(result);
        error("Expected double vector or list of doubles, got %s", type2char(TYPEOF(obj)));
    }

    return result;
}



Anything* r_to_anything_ext(Anything* result, SEXP obj, const Schema* schema) {
  if (TYPEOF(obj) != RAWSXP) {
    free(result);
    error("Expected raw vector for EXT type, got %s", type2char(TYPEOF(obj)));
  }

  result->size = LENGTH(obj);
  result->data.char_arr = (char*)malloc(result->size);
  if (!result->data.char_arr) {
    free(result);
    error("Failed to allocate memory for EXT data");
  }
  memcpy(result->data.char_arr, RAW(obj), result->size);

  return result;
}


Anything* r_to_anything(SEXP obj, const Schema* schema) {
  Anything* result = (Anything*)malloc(sizeof(Anything));
  if (!result) {
    error("Failed to allocate memory for Anything");
  }
  result->type = schema->type;
  result->key = NULL;
  switch(schema->type){
    case MORLOC_NIL:          return r_to_anything_nil(result,          obj, schema);
    case MORLOC_BOOL:         return r_to_anything_bool(result,         obj, schema);
    case MORLOC_INT:          return r_to_anything_int(result,          obj, schema);
    case MORLOC_FLOAT:        return r_to_anything_float(result,        obj, schema);
    case MORLOC_STRING:       return r_to_anything_string(result,       obj, schema);
    case MORLOC_BINARY:       return r_to_anything_binary(result,       obj, schema);
    case MORLOC_ARRAY:        return r_to_anything_array(result,        obj, schema);
    case MORLOC_TUPLE:        return r_to_anything_tuple(result,        obj, schema);
    case MORLOC_MAP:          return r_to_anything_map(result,          obj, schema);
    case MORLOC_BOOL_ARRAY:   return r_to_anything_bool_array(result,   obj, schema);
    case MORLOC_INT_ARRAY:    return r_to_anything_int_array(result,    obj, schema);
    case MORLOC_FLOAT_ARRAY:  return r_to_anything_float_array(result,  obj, schema);
    case MORLOC_EXT:          return r_to_anything_ext(result,          obj, schema);
    default:
      free(result);
      error("Unsupported schema type");
  }
  return NULL;
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
          switch(schema->parameters[0]->type) {
              case MORLOC_BOOL:
              {
                  result = PROTECT(allocVector(LGLSXP, data->size));
                  for (size_t i = 0; i < data->size; i++) {
                      LOGICAL(result)[i] = data->data.obj_arr[i]->data.bool_val ? TRUE : FALSE;
                  }
                  UNPROTECT(1);
              }
              break;
    
              case MORLOC_INT:
              {
                  result = PROTECT(allocVector(INTSXP, data->size));
                  for (size_t i = 0; i < data->size; i++) {
                      INTEGER(result)[i] = data->data.obj_arr[i]->data.int_val;
                  }
                  UNPROTECT(1);
              }
              break;
    
              case MORLOC_FLOAT:
              {
                  result = PROTECT(allocVector(REALSXP, data->size));
                  for (size_t i = 0; i < data->size; i++) {
                      REAL(result)[i] = data->data.obj_arr[i]->data.double_val;
                  }
                  UNPROTECT(1);
              }
              break;
    
              case MORLOC_STRING:
              {
                  result = PROTECT(allocVector(STRSXP, data->size));
                  for (size_t i = 0; i < data->size; i++) {
                      SET_STRING_ELT(result, i, mkChar(data->data.obj_arr[i]->data.char_arr));
                  }
                  UNPROTECT(1);
              }
              break;
    
              default:
              {
                  result = PROTECT(allocVector(VECSXP, data->size));
                  const Schema* element_schema = schema->parameters[0];
                  for (size_t i = 0; i < data->size; i++) {
                      SET_VECTOR_ELT(result, i, anything_to_r(data->data.obj_arr[i], element_schema));
                  }
                  UNPROTECT(1);
              }
              break;
          }
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
      return R_NilValue;
      error("Unsupported Anything type");
  }
  return result;
}


SEXP _mlcmpack_r_pack(SEXP r_obj, SEXP r_schema_str) {
    PROTECT(r_obj);
    PROTECT(r_schema_str);

    const char* schema_str = CHAR(STRING_ELT(r_schema_str, 0));
    Schema* schema = parse_schema(&schema_str);
    if (!schema) {
        UNPROTECT(2);
        error("Failed to parse schema");
    }

    Anything* data = r_to_anything(r_obj, schema);

    if (!data) {
        free_schema(schema);
        UNPROTECT(2);
        error("Failed to convert R object to Anything");
    }

    char* packed_data = NULL;
    size_t packed_size = 0;
    int result = pack_with_schema(data, schema, &packed_data, &packed_size);

    /* free_parsed_data(data); */

    if (result != 0 || !packed_data) {
        free_schema(schema);
        UNPROTECT(2);
        error("Packing failed");
    }

    SEXP r_packed = PROTECT(allocVector(RAWSXP, packed_size));
    memcpy(RAW(r_packed), packed_data, packed_size);

    if (packed_data)
      free(packed_data);

    if (schema)
      free_schema(schema);

    UNPROTECT(3);
    return r_packed;

    return r_schema_str;
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
