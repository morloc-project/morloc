#include "morloc.h"
#include "json.h"

// ======================================================================
// Internal JSON value types
// ======================================================================

typedef enum { JV_NULL = 0, JV_BOOL, JV_NUM, JV_STR, JV_ARR, JV_OBJ } jtype_t;

typedef struct jval_s jval_t;
typedef struct { char* key; jval_t* val; } jpair_t;

struct jval_s {
    jtype_t type;
    union {
        bool b;
        double n;
        char* s;
        struct { jval_t** items; size_t len; } arr;
        struct { jpair_t* pairs; size_t len; } obj;
    };
};

// Forward declarations
static jval_t* jparse(const char** p, ERRMSG);
static void jfree(jval_t* v);

// ======================================================================
// JSON accessors
// ======================================================================

static jval_t* jget(const jval_t* obj, const char* key) {
    if (!obj || obj->type != JV_OBJ) return NULL;
    for (size_t i = 0; i < obj->obj.len; i++) {
        if (strcmp(obj->obj.pairs[i].key, key) == 0)
            return obj->obj.pairs[i].val;
    }
    return NULL;
}

static const char* jstr(const jval_t* v) {
    return (v && v->type == JV_STR) ? v->s : NULL;
}

static double jnum(const jval_t* v) {
    return (v && v->type == JV_NUM) ? v->n : 0;
}

static bool jbool(const jval_t* v) {
    return (v && v->type == JV_BOOL) ? v->b : false;
}

static size_t jlen(const jval_t* v) {
    return (v && v->type == JV_ARR) ? v->arr.len : 0;
}

static jval_t* jidx(const jval_t* v, size_t i) {
    return (v && v->type == JV_ARR && i < v->arr.len) ? v->arr.items[i] : NULL;
}

static bool jnull(const jval_t* v) {
    return !v || v->type == JV_NULL;
}

// Convenience: get string member of object, or NULL
static const char* jgets(const jval_t* obj, const char* key) {
    return jstr(jget(obj, key));
}

// ======================================================================
// JSON parser
// ======================================================================

static void jskip(const char** p) {
    while (**p == ' ' || **p == '\t' || **p == '\n' || **p == '\r') (*p)++;
}

static jval_t* jmake(jtype_t type) {
    jval_t* v = (jval_t*)calloc(1, sizeof(jval_t));
    v->type = type;
    return v;
}

static char* jparse_str_raw(const char** p, ERRMSG) {
    PTR_RETURN_SETUP(char)
    RAISE_IF(**p != '"', "Expected '\"' at position: %.20s", *p)
    (*p)++;

    // Find closing quote to determine max length
    const char* scan = *p;
    while (*scan && *scan != '"') {
        if (*scan == '\\') { scan++; if (*scan) scan++; }
        else scan++;
    }
    size_t maxlen = (size_t)(scan - *p);

    char* s = (char*)calloc(maxlen + 1, 1);
    size_t j = 0;

    while (**p != '"') {
        RAISE_IF_WITH(**p == '\0', free(s), "Unterminated string")
        if (**p == '\\') {
            (*p)++;
            switch (**p) {
                case '"':  s[j++] = '"'; break;
                case '\\': s[j++] = '\\'; break;
                case '/':  s[j++] = '/'; break;
                case 'b':  s[j++] = '\b'; break;
                case 'f':  s[j++] = '\f'; break;
                case 'n':  s[j++] = '\n'; break;
                case 'r':  s[j++] = '\r'; break;
                case 't':  s[j++] = '\t'; break;
                case 'u':
                    // Skip \uXXXX (manifest strings are ASCII)
                    (*p)++;
                    for (int k = 0; k < 4 && **p && **p != '"'; k++) (*p)++;
                    continue; // skip the (*p)++ below
                default: s[j++] = **p; break;
            }
        } else {
            s[j++] = **p;
        }
        (*p)++;
    }
    (*p)++; // consume closing '"'
    s[j] = '\0';
    return s;
}

static jval_t* jparse_arr(const char** p, ERRMSG) {
    PTR_RETURN_SETUP(jval_t)
    (*p)++; // consume '['
    jskip(p);

    jval_t* v = jmake(JV_ARR);
    if (**p == ']') { (*p)++; return v; }

    size_t cap = 8;
    v->arr.items = (jval_t**)calloc(cap, sizeof(jval_t*));

    while (1) {
        jskip(p);
        if (v->arr.len >= cap) {
            cap *= 2;
            v->arr.items = (jval_t**)realloc(v->arr.items, cap * sizeof(jval_t*));
        }
        v->arr.items[v->arr.len] = TRY_WITH(jfree(v), jparse, p);
        v->arr.len++;
        jskip(p);
        if (**p == ',') { (*p)++; continue; }
        if (**p == ']') { (*p)++; break; }
        RAISE_WITH(jfree(v), "Expected ',' or ']' in array")
    }
    return v;
}

static jval_t* jparse_obj(const char** p, ERRMSG) {
    PTR_RETURN_SETUP(jval_t)
    (*p)++; // consume '{'
    jskip(p);

    jval_t* v = jmake(JV_OBJ);
    if (**p == '}') { (*p)++; return v; }

    size_t cap = 8;
    v->obj.pairs = (jpair_t*)calloc(cap, sizeof(jpair_t));

    while (1) {
        jskip(p);
        if (v->obj.len >= cap) {
            cap *= 2;
            v->obj.pairs = (jpair_t*)realloc(v->obj.pairs, cap * sizeof(jpair_t));
        }
        jpair_t* pair = &v->obj.pairs[v->obj.len];
        pair->key = TRY_WITH(jfree(v), jparse_str_raw, p);
        v->obj.len++; // increment now so jfree cleans up partial pair
        jskip(p);
        RAISE_IF_WITH(**p != ':', jfree(v), "Expected ':' in object")
        (*p)++;
        pair->val = TRY_WITH(jfree(v), jparse, p);
        jskip(p);
        if (**p == ',') { (*p)++; continue; }
        if (**p == '}') { (*p)++; break; }
        RAISE_WITH(jfree(v), "Expected ',' or '}' in object")
    }
    return v;
}

static jval_t* jparse(const char** p, ERRMSG) {
    PTR_RETURN_SETUP(jval_t)
    jskip(p);

    switch (**p) {
        case '"': {
            jval_t* v = jmake(JV_STR);
            v->s = TRY(jparse_str_raw, p);
            return v;
        }
        case '{': return TRY(jparse_obj, p);
        case '[': return TRY(jparse_arr, p);
        case 't':
            RAISE_IF(strncmp(*p, "true", 4) != 0, "Invalid token at: %.20s", *p)
            *p += 4;
            { jval_t* v = jmake(JV_BOOL); v->b = true; return v; }
        case 'f':
            RAISE_IF(strncmp(*p, "false", 5) != 0, "Invalid token at: %.20s", *p)
            *p += 5;
            { jval_t* v = jmake(JV_BOOL); v->b = false; return v; }
        case 'n':
            RAISE_IF(strncmp(*p, "null", 4) != 0, "Invalid token at: %.20s", *p)
            *p += 4;
            return jmake(JV_NULL);
        default: {
            if (**p == '-' || (**p >= '0' && **p <= '9')) {
                char* end;
                double n = strtod(*p, &end);
                RAISE_IF(*p == end, "Invalid number at: %.20s", *p)
                *p = end;
                jval_t* v = jmake(JV_NUM);
                v->n = n;
                return v;
            }
            RAISE("Unexpected character '%c' at: %.20s", **p, *p)
        }
    }
}

static void jfree(jval_t* v) {
    if (!v) return;
    switch (v->type) {
        case JV_STR: free(v->s); break;
        case JV_ARR:
            for (size_t i = 0; i < v->arr.len; i++) jfree(v->arr.items[i]);
            free(v->arr.items);
            break;
        case JV_OBJ:
            for (size_t i = 0; i < v->obj.len; i++) {
                free(v->obj.pairs[i].key);
                jfree(v->obj.pairs[i].val);
            }
            free(v->obj.pairs);
            break;
        default: break;
    }
    free(v);
}

// ======================================================================
// Helper: read a JSON array of strings into a NULL-terminated char**
// ======================================================================

static char** read_str_array(const jval_t* jarr) {
    size_t n = jlen(jarr);
    char** arr = (char**)calloc(n + 1, sizeof(char*));
    for (size_t i = 0; i < n; i++) {
        const char* s = jstr(jidx(jarr, i));
        arr[i] = s ? strdup(s) : strdup("");
    }
    arr[n] = NULL;
    return arr;
}

// ======================================================================
// Expression tree builder
// ======================================================================

static morloc_pattern_t* build_pattern(const jval_t* jp, ERRMSG) {
    PTR_RETURN_SETUP(morloc_pattern_t)
    const char* type = jgets(jp, "type");
    RAISE_IF(!type, "Pattern missing 'type' field")

    if (strcmp(type, "end") == 0) {
        return make_morloc_pattern_end();
    }

    const jval_t* sels = jget(jp, "selectors");
    size_t n = jlen(sels);

    morloc_pattern_t* pat = (morloc_pattern_t*)calloc(1, sizeof(morloc_pattern_t));
    pat->size = n;
    pat->selectors = (morloc_pattern_t**)calloc(n, sizeof(morloc_pattern_t*));

    if (strcmp(type, "idx") == 0) {
        pat->type = SELECT_BY_INDEX;
        pat->fields.indices = (size_t*)calloc(n, sizeof(size_t));
        for (size_t i = 0; i < n; i++) {
            const jval_t* sel = jidx(sels, i);
            pat->fields.indices[i] = (size_t)jnum(jget(sel, "index"));
            pat->selectors[i] = TRY(build_pattern, jget(sel, "sub"));
        }
    } else if (strcmp(type, "key") == 0) {
        pat->type = SELECT_BY_KEY;
        pat->fields.keys = (char**)calloc(n, sizeof(char*));
        for (size_t i = 0; i < n; i++) {
            const jval_t* sel = jidx(sels, i);
            pat->fields.keys[i] = strdup(jgets(sel, "key"));
            pat->selectors[i] = TRY(build_pattern, jget(sel, "sub"));
        }
    } else {
        free(pat->selectors);
        free(pat);
        RAISE("Unknown pattern type: %s", type)
    }

    return pat;
}

static morloc_expression_t* build_expr(const jval_t* je, ERRMSG) {
    PTR_RETURN_SETUP(morloc_expression_t)
    RAISE_IF(!je, "NULL expression node")
    const char* tag = jgets(je, "tag");
    RAISE_IF(!tag, "Expression missing 'tag' field")

    if (strcmp(tag, "lit") == 0) {
        const char* schema = jgets(je, "schema");
        const char* lt = jgets(je, "lit_type");
        const char* val = jgets(je, "value");
        primitive_t prim = {0};

        if      (strcmp(lt, "f4") == 0) prim.f4 = (float)atof(val);
        else if (strcmp(lt, "f8") == 0) prim.f8 = atof(val);
        else if (strcmp(lt, "i1") == 0) prim.i1 = (int8_t)atoi(val);
        else if (strcmp(lt, "i2") == 0) prim.i2 = (int16_t)atoi(val);
        else if (strcmp(lt, "i4") == 0) prim.i4 = (int32_t)atoi(val);
        else if (strcmp(lt, "i8") == 0) prim.i8 = (int64_t)atoll(val);
        else if (strcmp(lt, "u1") == 0) prim.u1 = (uint8_t)strtoul(val, NULL, 10);
        else if (strcmp(lt, "u2") == 0) prim.u2 = (uint16_t)strtoul(val, NULL, 10);
        else if (strcmp(lt, "u4") == 0) prim.u4 = (uint32_t)strtoul(val, NULL, 10);
        else if (strcmp(lt, "u8") == 0) prim.u8 = (uint64_t)strtoull(val, NULL, 10);
        else if (strcmp(lt, "b") == 0)  prim.b = atoi(val) != 0;
        else if (strcmp(lt, "z") == 0)  prim.z = 0;
        else { RAISE("Unknown lit_type: %s", lt) }

        return TRY(make_morloc_literal, schema, prim);
    }

    if (strcmp(tag, "str") == 0) {
        const char* schema = jgets(je, "schema");
        const char* val = jgets(je, "value");
        primitive_t prim = {0};
        prim.s = strdup(val);
        return TRY(make_morloc_literal, schema, prim);
    }

    if (strcmp(tag, "container") == 0) {
        const char* schema_str = jgets(je, "schema");
        const jval_t* elems = jget(je, "elements");
        size_t n = jlen(elems);

        Schema* schema = TRY(parse_schema, schema_str);
        morloc_expression_t** values = (morloc_expression_t**)calloc(n, sizeof(morloc_expression_t*));
        for (size_t i = 0; i < n; i++) {
            values[i] = TRY(build_expr, jidx(elems, i));
        }

        morloc_data_t* data = (morloc_data_t*)calloc(1, sizeof(morloc_data_t));
        data->is_voidstar = false;

        switch (schema->type) {
            case MORLOC_ARRAY: {
                morloc_data_array_t* arr = (morloc_data_array_t*)calloc(1, sizeof(morloc_data_array_t));
                arr->schema = schema->parameters[0];
                arr->size = n;
                arr->values = values;
                data->data.array_val = arr;
                break;
            }
            case MORLOC_TUPLE:
            case MORLOC_MAP:
                data->data.tuple_val = values;
                break;
            default:
                free(values);
                free(data);
                free_schema(schema);
                RAISE("Container schema is not a container type")
        }

        morloc_expression_t* expr = (morloc_expression_t*)calloc(1, sizeof(morloc_expression_t));
        expr->type = MORLOC_X_DAT;
        expr->schema = schema;
        expr->expr.data_expr = data;
        return expr;
    }

    if (strcmp(tag, "app") == 0) {
        const char* schema_str = jgets(je, "schema");
        const jval_t* jargs = jget(je, "args");
        size_t n = jlen(jargs);

        Schema* schema = TRY(parse_schema, schema_str);
        morloc_expression_t* func = TRY(build_expr, jget(je, "func"));

        morloc_expression_t** args = (morloc_expression_t**)calloc(n, sizeof(morloc_expression_t*));
        for (size_t i = 0; i < n; i++) {
            args[i] = TRY(build_expr, jidx(jargs, i));
        }

        morloc_app_expression_t* app = (morloc_app_expression_t*)calloc(1, sizeof(morloc_app_expression_t));
        switch (func->type) {
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
                free(args);
                free(app);
                free_schema(schema);
                RAISE("Invalid function in app expression (type=%d)", func->type)
        }
        app->args = args;
        app->nargs = n;

        morloc_expression_t* expr = (morloc_expression_t*)calloc(1, sizeof(morloc_expression_t));
        expr->type = MORLOC_X_APP;
        expr->schema = schema;
        expr->expr.app_expr = app;
        return expr;
    }

    if (strcmp(tag, "lambda") == 0) {
        const jval_t* jvars = jget(je, "vars");
        size_t n = jlen(jvars);

        morloc_expression_t* body = TRY(build_expr, jget(je, "body"));

        char** vars = (char**)calloc(n, sizeof(char*));
        for (size_t i = 0; i < n; i++) {
            vars[i] = strdup(jstr(jidx(jvars, i)));
        }

        morloc_lam_expression_t* lam = (morloc_lam_expression_t*)calloc(1, sizeof(morloc_lam_expression_t));
        lam->nargs = n;
        lam->args = vars;
        lam->body = body;

        morloc_expression_t* expr = (morloc_expression_t*)calloc(1, sizeof(morloc_expression_t));
        expr->type = MORLOC_X_LAM;
        expr->schema = NULL;
        expr->expr.lam_expr = lam;
        return expr;
    }

    if (strcmp(tag, "bound") == 0) {
        return TRY(make_morloc_bound_var,
            jgets(je, "schema"), strdup(jgets(je, "var")));
    }

    if (strcmp(tag, "interpolation") == 0) {
        const char* schema_str = jgets(je, "schema");
        const jval_t* jstrs = jget(je, "strings");
        size_t n = jlen(jstrs);

        Schema* schema = TRY(parse_schema, schema_str);

        char** strings = (char**)calloc(n + 1, sizeof(char*));
        for (size_t i = 0; i < n; i++) {
            strings[i] = strdup(jstr(jidx(jstrs, i)));
        }

        morloc_expression_t* expr = (morloc_expression_t*)calloc(1, sizeof(morloc_expression_t));
        expr->type = MORLOC_X_FMT;
        expr->schema = schema;
        expr->expr.interpolation = strings;
        return expr;
    }

    if (strcmp(tag, "pattern") == 0) {
        const char* schema_str = jgets(je, "schema");
        morloc_pattern_t* pat = TRY(build_pattern, jget(je, "pattern"));
        return TRY(make_morloc_pattern, schema_str, pat);
    }

    RAISE("Unknown expression tag: %s", tag)
}

// Public wrapper: parse a JSON expression string into a morloc_expression_t
morloc_expression_t* build_manifest_expr(const char* json_str, ERRMSG) {
    PTR_RETURN_SETUP(morloc_expression_t)
    const char* p = json_str;
    jval_t* jv = TRY(jparse, &p);
    morloc_expression_t* expr = build_expr(jv, &CHILD_ERRMSG);
    jfree(jv);
    RAISE_IF(CHILD_ERRMSG != NULL, "\n%s", CHILD_ERRMSG)
    return expr;
}

// ======================================================================
// Manifest reader
// ======================================================================

static char* nullable_strdup(const char* s) {
    return s ? strdup(s) : NULL;
}

static manifest_arg_t read_arg(const jval_t* ja, ERRMSG) {
    VAL_RETURN_SETUP(manifest_arg_t, ((manifest_arg_t){0}))
    const char* kind = jgets(ja, "kind");
    RAISE_IF(!kind, "Argument missing 'kind' field")

    manifest_arg_t arg = {0};
    arg.desc = read_str_array(jget(ja, "desc"));

    if (strcmp(kind, "pos") == 0) {
        arg.kind = MARG_POS;
        arg.metavar = nullable_strdup(jgets(ja, "metavar"));
        arg.type_desc = nullable_strdup(jgets(ja, "type_desc"));
        arg.quoted = jbool(jget(ja, "quoted"));
    } else if (strcmp(kind, "opt") == 0) {
        arg.kind = MARG_OPT;
        arg.metavar = nullable_strdup(jgets(ja, "metavar"));
        arg.type_desc = nullable_strdup(jgets(ja, "type_desc"));
        arg.quoted = jbool(jget(ja, "quoted"));
        const char* s = jgets(ja, "short");
        arg.short_opt = (s && s[0]) ? s[0] : '\0';
        const char* l = jgets(ja, "long");
        arg.long_opt = l ? strdup(l) : NULL;
        arg.default_val = nullable_strdup(jgets(ja, "default"));
    } else if (strcmp(kind, "flag") == 0) {
        arg.kind = MARG_FLAG;
        const char* s = jgets(ja, "short");
        arg.short_opt = (s && s[0]) ? s[0] : '\0';
        const char* l = jgets(ja, "long");
        arg.long_opt = l ? strdup(l) : NULL;
        const char* lr = jgets(ja, "long_rev");
        arg.long_rev = lr ? strdup(lr) : NULL;
        arg.default_val = nullable_strdup(jgets(ja, "default"));
    } else if (strcmp(kind, "grp") == 0) {
        arg.kind = MARG_GRP;
        arg.metavar = nullable_strdup(jgets(ja, "metavar"));
        const jval_t* gopt = jget(ja, "group_opt");
        if (!jnull(gopt)) {
            const char* gs = jgets(gopt, "short");
            arg.grp_short = (gs && gs[0]) ? gs[0] : '\0';
            const char* gl = jgets(gopt, "long");
            arg.grp_long = gl ? strdup(gl) : NULL;
        }
        const jval_t* entries = jget(ja, "entries");
        arg.n_entries = jlen(entries);
        arg.entries = (manifest_grp_entry_t*)calloc(arg.n_entries, sizeof(manifest_grp_entry_t));
        for (size_t i = 0; i < arg.n_entries; i++) {
            const jval_t* entry = jidx(entries, i);
            arg.entries[i].key = strdup(jgets(entry, "key"));
            manifest_arg_t* sub = (manifest_arg_t*)calloc(1, sizeof(manifest_arg_t));
            *sub = TRY(read_arg, jget(entry, "arg"));
            arg.entries[i].arg = sub;
        }
    } else {
        RAISE("Unknown arg kind: %s", kind)
    }

    return arg;
}

static manifest_pool_t read_pool(const jval_t* jp) {
    manifest_pool_t pool = {0};
    pool.lang = strdup(jgets(jp, "lang"));
    pool.exec = read_str_array(jget(jp, "exec"));
    pool.socket = strdup(jgets(jp, "socket"));
    return pool;
}

static manifest_command_t read_command(const jval_t* jc, ERRMSG) {
    VAL_RETURN_SETUP(manifest_command_t, ((manifest_command_t){0}))
    manifest_command_t cmd = {0};

    cmd.name = strdup(jgets(jc, "name"));
    const char* type = jgets(jc, "type");
    cmd.is_pure = (strcmp(type, "pure") == 0);

    if (!cmd.is_pure) {
        cmd.mid = (uint32_t)jnum(jget(jc, "mid"));
        cmd.pool_index = (size_t)jnum(jget(jc, "pool"));
        const jval_t* np = jget(jc, "needed_pools");
        cmd.n_needed_pools = jlen(np);
        cmd.needed_pools = (size_t*)calloc(cmd.n_needed_pools, sizeof(size_t));
        for (size_t i = 0; i < cmd.n_needed_pools; i++) {
            cmd.needed_pools[i] = (size_t)jnum(jidx(np, i));
        }
    }

    cmd.arg_schemas = read_str_array(jget(jc, "arg_schemas"));
    cmd.return_schema = strdup(jgets(jc, "return_schema"));
    cmd.desc = read_str_array(jget(jc, "desc"));
    cmd.return_type = strdup(jgets(jc, "return_type"));
    cmd.return_desc = read_str_array(jget(jc, "return_desc"));

    const jval_t* jargs = jget(jc, "args");
    cmd.n_args = jlen(jargs);
    cmd.args = (manifest_arg_t*)calloc(cmd.n_args, sizeof(manifest_arg_t));
    for (size_t i = 0; i < cmd.n_args; i++) {
        cmd.args[i] = TRY(read_arg, jidx(jargs, i));
    }

    if (cmd.is_pure) {
        cmd.expr = TRY(build_expr, jget(jc, "expr"));
    }

    return cmd;
}

manifest_t* parse_manifest(const char* text, ERRMSG) {
    PTR_RETURN_SETUP(manifest_t)

    const char* p = text;
    jval_t* root = jparse(&p, &CHILD_ERRMSG);
    if (CHILD_ERRMSG != NULL) {
        RAISE("Failed to parse manifest JSON:\n%s", CHILD_ERRMSG)
    }

    manifest_t* m = (manifest_t*)calloc(1, sizeof(manifest_t));
    m->version = (int)jnum(jget(root, "version"));
    m->name = nullable_strdup(jgets(root, "name"));
    m->build_dir = nullable_strdup(jgets(root, "build_dir"));

    const jval_t* jpools = jget(root, "pools");
    m->n_pools = jlen(jpools);
    m->pools = (manifest_pool_t*)calloc(m->n_pools, sizeof(manifest_pool_t));
    for (size_t i = 0; i < m->n_pools; i++) {
        m->pools[i] = read_pool(jidx(jpools, i));
    }

    const jval_t* jcmds = jget(root, "commands");
    m->n_commands = jlen(jcmds);
    m->commands = (manifest_command_t*)calloc(m->n_commands, sizeof(manifest_command_t));
    for (size_t i = 0; i < m->n_commands; i++) {
        m->commands[i] = read_command(jidx(jcmds, i), &CHILD_ERRMSG);
        if (CHILD_ERRMSG != NULL) {
            jfree(root);
            RAISE("Failed to read command %zu: %s", i, CHILD_ERRMSG)
        }
    }

    jfree(root);
    return m;
}

manifest_t* read_manifest(const char* path, ERRMSG) {
    PTR_RETURN_SETUP(manifest_t)

    size_t file_size = 0;
    char* text = (char*)TRY(read_binary_file, path, &file_size);

    manifest_t* m = TRY(parse_manifest, text);
    free(text);
    return m;
}

void free_manifest(manifest_t* manifest) {
    if (!manifest) return;
    free(manifest->name);
    free(manifest->build_dir);
    for (size_t i = 0; i < manifest->n_pools; i++) {
        free(manifest->pools[i].lang);
        if (manifest->pools[i].exec) {
            for (size_t j = 0; manifest->pools[i].exec[j]; j++)
                free(manifest->pools[i].exec[j]);
            free(manifest->pools[i].exec);
        }
        free(manifest->pools[i].socket);
    }
    free(manifest->pools);
    // Commands own complex state; for now we don't free them since
    // the manifest lives for the program lifetime.
    free(manifest->commands);
    free(manifest);
}

char* manifest_to_discovery_json(const manifest_t* manifest) {
    json_buf_t* jb = json_buf_new();

    json_write_obj_start(jb);

    json_write_key(jb, "name");
    json_write_string(jb, manifest->name ? manifest->name : "unknown");

    json_write_key(jb, "version");
    json_write_int(jb, manifest->version);

    json_write_key(jb, "commands");
    json_write_arr_start(jb);

    for (size_t i = 0; i < manifest->n_commands; i++) {
        manifest_command_t* cmd = &manifest->commands[i];
        json_write_obj_start(jb);

        json_write_key(jb, "name");
        json_write_string(jb, cmd->name);

        json_write_key(jb, "type");
        json_write_string(jb, cmd->is_pure ? "pure" : "remote");

        json_write_key(jb, "return_type");
        json_write_string(jb, cmd->return_type);

        json_write_key(jb, "return_schema");
        json_write_string(jb, cmd->return_schema);

        // argument info
        json_write_key(jb, "args");
        json_write_arr_start(jb);
        for (size_t a = 0; a < cmd->n_args; a++) {
            manifest_arg_t* arg = &cmd->args[a];
            json_write_obj_start(jb);

            json_write_key(jb, "kind");
            switch (arg->kind) {
                case MARG_POS:  json_write_string(jb, "pos"); break;
                case MARG_OPT:  json_write_string(jb, "opt"); break;
                case MARG_FLAG: json_write_string(jb, "flag"); break;
                case MARG_GRP:  json_write_string(jb, "grp"); break;
            }

            if (arg->metavar) {
                json_write_key(jb, "metavar");
                json_write_string(jb, arg->metavar);
            }

            if (arg->type_desc) {
                json_write_key(jb, "type");
                json_write_string(jb, arg->type_desc);
            }

            // schema for positional/opt args
            if ((arg->kind == MARG_POS || arg->kind == MARG_OPT) &&
                cmd->arg_schemas && cmd->arg_schemas[a]) {
                json_write_key(jb, "schema");
                json_write_string(jb, cmd->arg_schemas[a]);
            }

            if (arg->default_val) {
                json_write_key(jb, "default");
                json_write_string(jb, arg->default_val);
            }

            if (arg->long_opt) {
                json_write_key(jb, "long");
                json_write_string(jb, arg->long_opt);
            }

            if (arg->short_opt) {
                char short_str[2] = { arg->short_opt, '\0' };
                json_write_key(jb, "short");
                json_write_string(jb, short_str);
            }

            if (arg->desc && arg->desc[0] && arg->desc[0][0]) {
                json_write_key(jb, "desc");
                json_write_string(jb, arg->desc[0]);
            }

            json_write_obj_end(jb);
        }
        json_write_arr_end(jb);

        // command description
        if (cmd->desc && cmd->desc[0] && cmd->desc[0][0]) {
            json_write_key(jb, "desc");
            json_write_string(jb, cmd->desc[0]);
        }

        json_write_obj_end(jb);
    }

    json_write_arr_end(jb);
    json_write_obj_end(jb);

    return json_buf_finish(jb);
}
