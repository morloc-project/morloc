#include "morloc.h"

// This type should act as a string-based dictionary
// The current implementation as a linked list is slow and can be replaced later
// with a tree structure, but it suffices for the current small use case.
typedef struct dict_s dict_t;

typedef struct dict_s {
    char* name;
    void* thing;
    dict_t* next;
} dict_t;

// Forward declarations
static absptr_t morloc_eval_r(morloc_expression_t*, absptr_t, size_t, dict_t*, ERRMSG);
static absptr_t apply_setter(absptr_t, Schema*, morloc_pattern_t*, Schema*, absptr_t, Schema**, absptr_t*, ERRMSG);
static absptr_t apply_setter_copy(absptr_t, Schema*, morloc_pattern_t*, Schema*, absptr_t, ERRMSG);
static absptr_t apply_setter_set(absptr_t, Schema*, morloc_pattern_t*, Schema*, absptr_t, Schema**, absptr_t*, size_t*, ERRMSG);
static absptr_t apply_getter(absptr_t, size_t*, Schema*, morloc_pattern_t*, Schema*, absptr_t, ERRMSG);

// add an element to the beginning of a named linked list
// inserting an element into an empty dict creates a dict singleton
static dict_t* dict_insert(char* name, void* thing, dict_t* dict){
    dict_t* dict_new = (dict_t*)malloc(sizeof(dict_t));
    dict_new->name = strdup(name);
    dict_new->thing = thing;
    dict_new->next = dict;
    return dict_new;
}

// O(n) for this linked list implementation
static void* dict_lookup(char* name, dict_t* ll){
    for(dict_t* n = ll; n != NULL; n = n->next){
        if(strcmp(n->name, name) == 0){
            return n->thing;
        }
    }
    return NULL;
}

// As currently implemented, the dictionary may have synonyms. This function
// deletes (and frees) all entries with the same name.
static dict_t* dict_delete(char* name, dict_t* dict){
    dict_t* ptr;
    // remove initial values with name
    while(dict != NULL && strcmp(dict->name, name) == 0){
        ptr = dict->next;
        free(dict->name);
        free(dict);
        dict = ptr;
    }

    // remove internal/final values with name
    ptr = dict;
    while(ptr != NULL){
        if(ptr->next != NULL && strcmp(ptr->next->name, name) == 0){
            dict_t* to_delete = ptr->next;
            ptr->next = to_delete->next;
            free(to_delete->name);
            free(to_delete);
            // Don't advance ptr -- check the new ptr->next
        } else {
            ptr = ptr->next;
        }
    }
    return dict;
}

// Free the list spine AND the names, but not the elements
__attribute__((unused))
static void dict_free(dict_t* list){
    while(list != NULL){
        dict_t* temp = list;
        list = list->next;
        free(temp->name);
        free(temp);
    }
}

morloc_expression_t* make_morloc_bound_var(const char* schema_str, char* varname, ERRMSG){
    PTR_RETURN_SETUP(morloc_expression_t)
    Schema* schema = TRY(parse_schema, schema_str);

    morloc_expression_t* expr = (morloc_expression_t*)calloc(1, sizeof(morloc_expression_t));
    expr->type = MORLOC_X_BND;
    expr->schema = schema;
    expr->expr.bnd_expr = varname;

    return expr;
}

morloc_expression_t* make_morloc_literal(
  const char* schema_str,
  primitive_t lit,
  ERRMSG
){
    PTR_RETURN_SETUP(morloc_expression_t)
    Schema* schema = TRY(parse_schema, schema_str);

    morloc_data_t* data = (morloc_data_t*)malloc(sizeof(morloc_data_t));
    data->is_voidstar = false;
    data->data.lit_val = lit;

    morloc_expression_t* expr = (morloc_expression_t*)malloc(sizeof(morloc_expression_t));
    expr->schema = schema;
    expr->type = MORLOC_X_DAT;
    expr->expr.data_expr = data;
    return expr;
}

morloc_expression_t* make_morloc_container(
  const char* schema_str,
  ERRMSG,
  size_t nargs,
  ... // list of container elements
){
    PTR_RETURN_SETUP(morloc_expression_t)
    va_list value_list;
    va_start(value_list, nargs);
    Schema* schema = parse_schema(schema_str, &CHILD_ERRMSG);
    if(CHILD_ERRMSG != NULL){
        va_end(value_list);
        RAISE("\n%s", CHILD_ERRMSG)
    }

    morloc_data_t* data = (morloc_data_t*)malloc(sizeof(morloc_data_t));
    data->is_voidstar = false;

    morloc_expression_t** values = (morloc_expression_t**)calloc(nargs, sizeof(morloc_expression_t*));
    for(size_t i = 0; i < nargs; i++){
        values[i] = va_arg(value_list, morloc_expression_t*);
    }

    // Set the appropriate container field based on schema type
    switch(schema->type) {
        case MORLOC_ARRAY: {
            morloc_data_array_t* array = (morloc_data_array_t*)malloc(sizeof(morloc_data_array_t));
            array->schema = schema->parameters[0];
            array->size = nargs;
            array->values = values;
            data->data.array_val = array;
            break;
        }
        case MORLOC_TUPLE:
        case MORLOC_MAP:
            data->data.tuple_val = values;
            break;
        default:
            va_end(value_list);
            free(values);
            free(data);
            free_schema(schema);
            RAISE("Schema type is not a container type")
    }
    morloc_expression_t* expr = (morloc_expression_t*)malloc(sizeof(morloc_expression_t));
    expr->type = MORLOC_X_DAT;
    expr->schema = schema;
    expr->expr.data_expr = data;

    va_end(value_list);

    return expr;
}

morloc_expression_t* make_morloc_app(
  const char* schema_str,
  morloc_expression_t* func,
  ERRMSG,
  size_t nargs,
  ... // list of input arguments
){
    PTR_RETURN_SETUP(morloc_expression_t)
    va_list args;
    va_start(args, nargs);

    Schema* schema = parse_schema(schema_str, &CHILD_ERRMSG);
    if(CHILD_ERRMSG != NULL){
        va_end(args);
        RAISE("\n%s", CHILD_ERRMSG)
    }

    morloc_app_expression_t* app = (morloc_app_expression_t*)malloc(sizeof(morloc_app_expression_t));

    // Determine application type based on func
    switch(func->type) {
        case MORLOC_X_PAT:
            app->type = APPLY_PATTERN;
            app->function.pattern = func->expr.pattern_expr;
            break;
        case MORLOC_X_LAM:
            app->type = APPLY_LAMBDA;
            app->function.lambda = func->expr.lam_expr;
            break;
        case MORLOC_X_FMT:
            app->type = APPLY_FORMAT;
            app->function.fmt = func->expr.interpolation;
            break;
        default:
            va_end(args);
            free(app);
            free_schema(schema);
            RAISE("Can only apply pattern, lambda, or format")
    }

    app->args = (morloc_expression_t**)calloc(nargs, sizeof(morloc_expression_t*));
    for(size_t i = 0; i < nargs; i++){
        app->args[i] = va_arg(args, morloc_expression_t*);
    }
    app->nargs = nargs;

    morloc_expression_t* expr = (morloc_expression_t*)malloc(sizeof(morloc_expression_t));
    expr->type = MORLOC_X_APP;
    expr->schema = schema;
    expr->expr.app_expr = app;

    va_end(args);

    return expr;
}

morloc_expression_t* make_morloc_lambda(
  morloc_expression_t* body,
  size_t nvars,
  ... // list of input variable names
){
    va_list var_list;
    va_start(var_list, nvars);

    char** vars = (char**)calloc(nvars, sizeof(char*));
    for(size_t i = 0; i < nvars; i++){
        vars[i] = va_arg(var_list, char*);
    }

    morloc_lam_expression_t* lam = (morloc_lam_expression_t*)malloc(sizeof(morloc_lam_expression_t));
    lam->nargs = nvars;
    lam->args = vars;
    lam->body = body;

    va_end(var_list);

    morloc_expression_t* lam_expr = (morloc_expression_t*)calloc(1, sizeof(morloc_expression_t));
    lam_expr->type = MORLOC_X_LAM;
    lam_expr->schema = NULL;
    lam_expr->expr.lam_expr = lam;

    return lam_expr;
}

morloc_expression_t* make_morloc_interpolation(const char* schema_str, ERRMSG, size_t nargs, ...){
    PTR_RETURN_SETUP(morloc_expression_t)
    Schema* schema = TRY(parse_schema, schema_str);

    va_list var_list;
    va_start(var_list, nargs);

    char** strings = (char**)calloc(nargs+1, sizeof(char*)); // include terminal NULL

    for(size_t i = 0; i < nargs; i++){
        strings[i] = va_arg(var_list, char*);
    }

    va_end(var_list);

    morloc_expression_t* expr = (morloc_expression_t*)calloc(1, sizeof(morloc_expression_t));
    expr->type = MORLOC_X_FMT;
    expr->schema = schema;
    expr->expr.interpolation = strings;
    return expr;
}

morloc_expression_t* make_morloc_pattern(const char* schema_str, morloc_pattern_t* pattern, ERRMSG){
    PTR_RETURN_SETUP(morloc_expression_t)
    Schema* schema = TRY(parse_schema, schema_str);

    morloc_expression_t* expr = (morloc_expression_t*)calloc(1, sizeof(morloc_expression_t));
    expr->type = MORLOC_X_PAT;
    expr->schema = schema;
    expr->expr.pattern_expr = pattern;
    return expr;
}

morloc_pattern_t* make_morloc_pattern_end(){
    morloc_pattern_t* pattern = (morloc_pattern_t*)calloc(1, sizeof(morloc_pattern_t));
    pattern->type = SELECT_END;
    pattern->size = 0;
    pattern->fields.indices = NULL;
    pattern->selectors = NULL;
    return pattern;
}

morloc_pattern_t* make_morloc_pattern_idx(size_t nargs, ...){
    size_t* indices = (size_t*)calloc(nargs, sizeof(size_t));
    morloc_pattern_t** selectors = (morloc_pattern_t**)calloc(nargs, sizeof(morloc_pattern_t*));

    va_list var_list;
    va_start(var_list, nargs);

    for(size_t i = 0; i < (2 * nargs); i++){
        if(i % 2 == 0){
            indices[i / 2] = va_arg(var_list, size_t);
        } else {
            selectors[i / 2] = va_arg(var_list, morloc_pattern_t*);
        }
    }

    va_end(var_list);

    morloc_pattern_t* pattern = (morloc_pattern_t*)calloc(1, sizeof(morloc_pattern_t));
    pattern->type = SELECT_BY_INDEX;
    pattern->size = nargs;
    pattern->fields.indices = indices;
    pattern->selectors = selectors;

    return pattern;
}

morloc_pattern_t* make_morloc_pattern_key(size_t nargs, ...){
    char** keys = (char**)calloc(nargs, sizeof(char*));
    morloc_pattern_t** selectors = (morloc_pattern_t**)calloc(nargs, sizeof(morloc_pattern_t*));

    va_list var_list;
    va_start(var_list, nargs);

    for(size_t i = 0; i < (2 * nargs); i++){
        if(i % 2 == 0){
            keys[i / 2] = va_arg(var_list, char*);
        } else {
            selectors[i / 2] = va_arg(var_list, morloc_pattern_t*);
        }
    }

    va_end(var_list);

    morloc_pattern_t* pattern = (morloc_pattern_t*)calloc(1, sizeof(morloc_pattern_t));
    pattern->type = SELECT_BY_KEY;
    pattern->size = nargs;
    pattern->fields.keys = keys;
    pattern->selectors = selectors;

    return pattern;
}

absptr_t morloc_eval(
  morloc_expression_t* expr,
  Schema* return_schema,
  uint8_t** arg_voidstar, // voidstar data
  Schema** arg_schemas, // argument schema strings
  size_t nargs,
  ERRMSG
) {
    PTR_RETURN_SETUP(absptr_t)
    morloc_expression_t* new_expr = NULL;

    // If the top expression is a lambda, then the arguments must be the user
    // provided arguments. The arguments are given as voidstar values. We need
    // to make a new application expression that wraps all the voidstar values
    // and applies them to the lambda function.
    if (expr->type == MORLOC_X_LAM) {
        morloc_expression_t** arg_exprs = (morloc_expression_t**)calloc(nargs, sizeof(morloc_expression_t*));
        for(size_t i = 0; i < nargs; i++){
            arg_exprs[i] = (morloc_expression_t*)calloc(1, sizeof(morloc_expression_t));
            arg_exprs[i]->type = MORLOC_X_DAT;
            arg_exprs[i]->schema = arg_schemas[i];
            arg_exprs[i]->expr.data_expr = (morloc_data_t*)calloc(1, sizeof(morloc_data_t));
            arg_exprs[i]->expr.data_expr->is_voidstar = true;
            arg_exprs[i]->expr.data_expr->data.voidstar = arg_voidstar[i];
        }

        morloc_app_expression_t* app_expr = (morloc_app_expression_t*)calloc(1, sizeof(morloc_app_expression_t));
        app_expr->type = APPLY_LAMBDA;
        app_expr->function.lambda = expr->expr.lam_expr;
        app_expr->args = arg_exprs;
        app_expr->nargs = nargs;

        new_expr = (morloc_expression_t*)calloc(1, sizeof(morloc_expression_t));
        new_expr->type = MORLOC_X_APP;
        new_expr->schema = return_schema;
        new_expr->expr.app_expr = app_expr;
    }
    // If we are not dealing with a lambda, we should instead directly evaluate
    // the input expression
    else {
        new_expr = expr;
    }

    absptr_t result = morloc_eval_r(new_expr, NULL, 0, NULL, &CHILD_ERRMSG);

    // Free the wrapper expression nodes allocated above (not the original lambda)
    if (expr->type == MORLOC_X_LAM && new_expr != NULL) {
        morloc_app_expression_t* app_expr = new_expr->expr.app_expr;
        for (size_t i = 0; i < nargs; i++) {
            free(app_expr->args[i]->expr.data_expr);
            free(app_expr->args[i]);
        }
        free(app_expr->args);
        free(app_expr);
        free(new_expr);
    }

    RAISE_IF(CHILD_ERRMSG != NULL, "\n%s", CHILD_ERRMSG)

    return result;
}

static absptr_t morloc_eval_r(morloc_expression_t* expr, absptr_t dest, size_t width, dict_t* bndvars, ERRMSG) {
    PTR_RETURN_SETUP(void)

    RAISE_IF(!expr, "Empty expression")

    Schema* schema = expr->schema;

    if (dest == NULL){
        width = schema->width;
        dest = TRY(shcalloc, 1, width);
    } else {
        RAISE_IF(width != schema->width, "Unexpected data size")
    }

    switch(expr->type) {
        case MORLOC_X_DAT: {
            morloc_data_t* data = expr->expr.data_expr;

            // directly return voidstar data
            if (data->is_voidstar) {
                return data->data.voidstar;
            }

            switch(schema->type){
                 case MORLOC_NIL:
                     memcpy(dest, (void*)(&data->data.lit_val.z), width);
                     break;
                 case MORLOC_BOOL:
                     memcpy(dest, (void*)(&data->data.lit_val.b), width);
                     break;
                 case MORLOC_SINT8:
                     memcpy(dest, (void*)(&data->data.lit_val.i1), width);
                     break;
                 case MORLOC_SINT16:
                     memcpy(dest, (void*)(&data->data.lit_val.i2), width);
                     break;
                 case MORLOC_SINT32:
                     memcpy(dest, (void*)(&data->data.lit_val.i4), width);
                     break;
                 case MORLOC_SINT64:
                     memcpy(dest, (void*)(&data->data.lit_val.i8), width);
                     break;
                 case MORLOC_UINT8:
                     memcpy(dest, (void*)(&data->data.lit_val.u1), width);
                     break;
                 case MORLOC_UINT16:
                     memcpy(dest, (void*)(&data->data.lit_val.u2), width);
                     break;
                 case MORLOC_UINT32:
                     memcpy(dest, (void*)(&data->data.lit_val.u4), width);
                     break;
                 case MORLOC_UINT64:
                     memcpy(dest, (void*)(&data->data.lit_val.u8), width);
                     break;
                 case MORLOC_FLOAT32:
                     memcpy(dest, (void*)(&data->data.lit_val.f4), width);
                     break;
                 case MORLOC_FLOAT64:
                     memcpy(dest, (void*)(&data->data.lit_val.f8), width);
                     break;
                 case MORLOC_STRING:
                     {
                         char* str = data->data.lit_val.s;
                         size_t str_size = strlen(str);
                         relptr_t str_relptr = -1;
                         if(str_size > 0){
                             absptr_t str_absptr = TRY(shmemcpy, (void*)str, str_size);
                             str_relptr = TRY(abs2rel, str_absptr);
                         }
                         Array str_array;
                         str_array.size = str_size;
                         str_array.data = str_relptr;
                         memcpy(dest, (void*)(&str_array), width);
                     }
                     break;
                 case MORLOC_ARRAY:
                     {
                         morloc_data_array_t* arr = data->data.array_val;
                         size_t arr_size = arr->size;
                         size_t element_width = arr->schema->width;
                         relptr_t arr_reldata = -1;
                         if(arr_size > 0){
                             absptr_t arr_data = TRY(shcalloc, arr_size, element_width);
                             for(size_t i = 0; i < arr_size; i++){
                                 TRY(morloc_eval_r, arr->values[i], (void*)((char*)arr_data + i * element_width), element_width, bndvars)
                             }
                             arr_reldata = TRY(abs2rel, arr_data)
                         }
                         Array array;
                         array.size = arr_size;
                         array.data = arr_reldata;
                         memcpy(dest, (void*)(&array), width);
                     }
                     break;
                 case MORLOC_TUPLE:
                 case MORLOC_MAP:
                     {
                         size_t element_width = 0;
                         for(size_t i = 0; i < schema->size; i++){
                             element_width = schema->parameters[i]->width;
                             morloc_expression_t* element = data->data.tuple_val[i];
                             absptr_t element_dest = (void*)((char*)dest + schema->offsets[i]);
                             TRY(morloc_eval_r, element, element_dest, element_width, bndvars)
                         }
                     }
                     break;

                 default:
                     RAISE("Illegal value in enum")
            }

        } break;

        case MORLOC_X_APP: {
            // Application: apply function to arguments
            morloc_app_expression_t* app = expr->expr.app_expr;

            absptr_t* arg_results = (absptr_t*)calloc(app->nargs, sizeof(absptr_t));
            // evaluate all arguments outside the new lambda scope
            for(size_t i = 0; i < app->nargs; i++){
                arg_results[i] = TRY_WITH(free(arg_results), morloc_eval_r, app->args[i], NULL, 0, bndvars);
            }

            switch(app->type) {
                case APPLY_PATTERN: {
                    if(app->nargs == 1){
                        size_t return_index = 0;
                        TRY_WITH(free(arg_results), apply_getter, dest, &return_index, schema, app->function.pattern, app->args[0]->schema, arg_results[0])
                    } else if(app->nargs > 1) {
                        Schema** arg_schemas = (Schema**)calloc(app->nargs-1, sizeof(Schema*));
                        for(size_t i = 1; i < app->nargs; i++){
                            arg_schemas[i-1] = app->args[i]->schema;
                        }
                        TRY_WITH(
                          (free(arg_schemas), free(arg_results)),
                          apply_setter,
                          dest,
                          schema,
                          app->function.pattern,
                          app->args[0]->schema,
                          arg_results[0],
                          arg_schemas,
                          arg_results + 1
                        )
                        free(arg_schemas);
                    } else {
                        RAISE_WITH(free(arg_results), "No arguments provided to pattern, this should be unreachable")
                    }
                } break;

                case APPLY_LAMBDA: {
                    morloc_lam_expression_t* lam = app->function.lambda;
                    // remove shadowed values from the variable table
                    for(size_t i = 0; i < app->nargs; i++){
                        bndvars = dict_delete(lam->args[i], bndvars);
                    }

                    // add evaluated arguments to the variable table
                    for(size_t i = 0; i < app->nargs; i++){
                        bndvars = dict_insert(lam->args[i], arg_results[i], bndvars);
                    }

                    // evaluate the lambda body with the new variable table
                    TRY_WITH(free(arg_results), morloc_eval_r, lam->body, dest, width, bndvars);

                    // clean up bindings added in this scope
                    for(size_t i = 0; i < app->nargs; i++){
                        bndvars = dict_delete(lam->args[i], bndvars);
                    }
                } break;

                case APPLY_FORMAT: {
                    char** strings = app->function.fmt;
                    size_t* string_lengths = (size_t*)calloc(app->nargs+1, sizeof(size_t));

                    size_t result_size = 0;

                    for(size_t i = 0; i < (app->nargs + 1); i++){
                        string_lengths[i] = strlen(strings[i]);
                        result_size += string_lengths[i];
                    }

                    for(size_t i = 0; i < app->nargs; i++){
                        Array* arr = (Array*)arg_results[i];
                        result_size += arr->size;
                    }

                    absptr_t new_string = TRY_WITH((free(string_lengths), free(arg_results)), shmalloc, result_size);
                    Array* result_array = (Array*)dest;
                    result_array->size = result_size;
                    result_array->data = TRY_WITH((free(string_lengths), free(arg_results)), abs2rel, new_string);

                    char* cursor = (char*)new_string;
                    for(size_t i = 0; i < (app->nargs + 1); i++){
                        memcpy((void*)cursor, strings[i], string_lengths[i]);
                        cursor += string_lengths[i];
                        if(i < app->nargs){
                            Array* arr = (Array*)arg_results[i];
                            absptr_t arr_data = TRY_WITH((free(string_lengths), free(arg_results)), rel2abs, arr->data);
                            memcpy(cursor, arr_data, arr->size);
                            cursor += arr->size;
                        }
                    }

                    free(string_lengths);

                } break;

                default:
                    RAISE_WITH(free(arg_results), "Invalid functional term")
            }
            free(arg_results);
        } break;

        case MORLOC_X_BND: {
            char* varname = expr->expr.bnd_expr;
            absptr_t bnd_value = dict_lookup(varname, bndvars);
            RAISE_IF(bnd_value == NULL, "Unbound variable %s", varname);
            memcpy(dest, bnd_value, schema->width);
        } break;

        default:
            RAISE("Illegal top expression");
    }

    return dest;
}

static bool convert_keys_to_indices(morloc_pattern_t* pattern, Schema* schema, ERRMSG) {
    BOOL_RETURN_SETUP

    if(schema->size > 1){
        for(size_t i = 0; i < pattern->size; i++){
            TRY(convert_keys_to_indices, pattern->selectors[i], schema->parameters[i]);
        }
    }

    if(pattern->type == SELECT_BY_KEY){
        size_t* indices = (size_t*)calloc(schema->size, sizeof(size_t));
        for(size_t i = 0; i < pattern->size; i++){
            char* pattern_key = pattern->fields.keys[i];
            bool found = false;
            for(size_t j = 0; j < schema->size; j++){
                char* record_key = schema->keys[j];
                if(strcmp(record_key, pattern_key) == 0){
                    found = true;
                    indices[i] = j;
                    break;
                }
            }
            if(!found){
                RAISE_WITH((free(pattern_key), free(indices)), "Pattern contains key that is missing in schema: %s", pattern_key)
            }
            free(pattern_key);
        }
        pattern->type = SELECT_BY_INDEX;
        free(pattern->fields.keys);
        pattern->fields.indices = indices;
    }

    return true;
}

static absptr_t apply_getter(absptr_t dest, size_t* return_index, Schema* return_schema, morloc_pattern_t* pattern, Schema* value_schema, absptr_t value, ERRMSG) {
    PTR_RETURN_SETUP(void)
    if (!return_schema || !pattern || !value_schema || !value) {
        RAISE("Required field is NULL")
    }

    if(dest == NULL){
        dest = TRY(shcalloc, 1, return_schema->width);
    }

    switch(pattern->type) {
        case SELECT_BY_INDEX:
            for(size_t i = 0; i < pattern->size; i++){
                size_t index = pattern->fields.indices[i];
                TRY(
                  apply_getter,
                  dest,
                  return_index,
                  return_schema,
                  pattern->selectors[i],
                  value_schema->parameters[index],
                  (void*)((char*)value + value_schema->offsets[index])
                )
            }
            break;

        case SELECT_BY_KEY:
            // if selecting by key, use the schema to translate key-based
            // lookups to index-based lookups
            TRY(convert_keys_to_indices, pattern, value_schema);
            // re-call as an index-based pattern
            TRY(apply_getter, dest, return_index, return_schema, pattern, value_schema, value)
            break;

        case SELECT_END:
            size_t element_width;
            absptr_t element_dest;
            if(return_schema->size > 1){
                element_dest = (void*)((char*)dest + return_schema->offsets[*return_index]);
                element_width = return_schema->parameters[*return_index]->width;
            } else {
                element_dest = dest;
                element_width = return_schema->width;
            }
            *return_index = *return_index + 1;
            memcpy(element_dest, value, element_width);
            break;

        default:
            RAISE("Illegal pattern enum value");
    }

    return dest;
}

static absptr_t apply_setter(
  absptr_t dest,
  Schema* return_schema,
  morloc_pattern_t* pattern,
  Schema* value_schema,
  absptr_t value,
  Schema** set_value_schemas,
  absptr_t* set_values,
  ERRMSG
) {
    PTR_RETURN_SETUP(void)
    if (!return_schema || !pattern || !value_schema || !value || !set_value_schemas || !set_values) {
        RAISE("Required value is NULL")
    }

    TRY(apply_setter_copy, dest, return_schema, pattern, value_schema, value);

    size_t set_idx = 0;
    TRY(apply_setter_set, dest, return_schema, pattern, value_schema, value, set_value_schemas, set_values, &set_idx);

    return dest;
}

static absptr_t apply_setter_copy(
  absptr_t dest,
  Schema* return_schema,
  morloc_pattern_t* pattern,
  Schema* value_schema,
  absptr_t value,
  ERRMSG
) {
    PTR_RETURN_SETUP(void)
    switch(pattern->type) {
        case SELECT_BY_KEY: {
            // if selecting by key, use the schema to translate key-based
            // lookups to index-based lookups
            TRY(convert_keys_to_indices, pattern, value_schema);

            // re-call as an index-based pattern
            TRY(apply_setter_copy, dest, return_schema, pattern, value_schema, value)
        } break;

        case SELECT_BY_INDEX: {
            RAISE_IF(value_schema->size != return_schema->size, "Expected setter return and input sizes to be the same")
            for(size_t i = 0; i < value_schema->size; i++){
                bool changed = false;
                absptr_t new_dest = (void*)((char*)dest + return_schema->offsets[i]);
                absptr_t new_value = (void*)((char*)value + value_schema->offsets[i]);
                for(size_t j = 0; j < pattern->size; j++){
                    if(i == pattern->fields.indices[j]){
                        TRY(
                          apply_setter_copy,
                          new_dest,
                          return_schema->parameters[i],
                          pattern->selectors[j],
                          value_schema->parameters[i],
                          new_value
                        )
                        changed = true;
                        break;
                    }
                }
                if(!changed){
                    memcpy(new_dest, new_value, value_schema->parameters[i]->width);
                }
            }
        } break;

        case SELECT_END:
            break;

        default:
            RAISE("Invalid pattern type");
    }
    return dest;
}

static absptr_t apply_setter_set(
  absptr_t dest,
  Schema* return_schema,
  morloc_pattern_t* pattern,
  Schema* value_schema,
  absptr_t value,
  Schema** set_value_schemas,
  absptr_t* set_values,
  size_t* set_idx,
  ERRMSG
) {
    PTR_RETURN_SETUP(void)
    switch(pattern->type) {
        case SELECT_BY_INDEX: {
            for(size_t pat_idx = 0; pat_idx < pattern->size; pat_idx++){
                size_t data_idx = pattern->fields.indices[pat_idx];
                absptr_t new_dest = (void*)((char*)dest + return_schema->offsets[data_idx]);
                absptr_t new_value = (void*)((char*)value + value_schema->offsets[data_idx]);
                TRY(
                  apply_setter_set,
                  new_dest,
                  return_schema->parameters[data_idx],
                  pattern->selectors[pat_idx],
                  value_schema->parameters[data_idx],
                  new_value,
                  set_value_schemas,
                  set_values,
                  set_idx
                )
            }
        } break;

        case SELECT_END: {
            memcpy(dest, set_values[*set_idx], return_schema->width);
            *set_idx = *set_idx + 1;
        } break;

        case SELECT_BY_KEY:
            RAISE("No key enums should be present, they should have been resolved in the copy step");

        default:
            RAISE("Invalid pattern type");
    }
    return dest;
}
