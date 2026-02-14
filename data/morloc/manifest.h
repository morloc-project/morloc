#ifndef __MORLOC_MANIFEST_H__
#define __MORLOC_MANIFEST_H__

#include "macros.h"
#include "eval.h"

// ---- Manifest data structures ----

typedef struct {
    char* lang;
    char** exec;      // NULL-terminated array (e.g. {"python3", "pool.py", NULL})
    char* socket;     // socket basename (e.g. "pipe-cpp")
} manifest_pool_t;

typedef enum {
    MARG_POS = 0,
    MARG_OPT,
    MARG_FLAG,
    MARG_GRP
} manifest_arg_kind_t;

typedef struct manifest_arg_s manifest_arg_t;

typedef struct {
    char* key;
    manifest_arg_t* arg;
} manifest_grp_entry_t;

struct manifest_arg_s {
    manifest_arg_kind_t kind;
    char** desc;           // NULL-terminated description lines
    // pos, opt, grp
    char* metavar;
    // pos, opt: display type string
    char* type_desc;
    // pos, opt: use quoted() instead of strdup() for string args
    bool quoted;
    // opt, flag: short option char, '\0' if none
    char short_opt;
    // opt, flag: long option name, NULL if none
    char* long_opt;
    // flag: reverse long option, NULL if none
    char* long_rev;
    // opt, flag: default value string, NULL if none
    char* default_val;
    // grp: group option short, '\0' if none
    char grp_short;
    // grp: group option long, NULL if none
    char* grp_long;
    // grp: sub-entries
    manifest_grp_entry_t* entries;
    size_t n_entries;
};

typedef struct {
    char* name;
    bool is_pure;
    // remote-only fields
    uint32_t mid;
    size_t pool_index;
    size_t* needed_pools;
    size_t n_needed_pools;
    // common fields
    char** arg_schemas;    // NULL-terminated
    char* return_schema;
    char** desc;           // NULL-terminated
    char* return_type;
    char** return_desc;    // NULL-terminated
    manifest_arg_t* args;
    size_t n_args;
    // pure-only field
    morloc_expression_t* expr;
} manifest_command_t;

typedef struct {
    int version;
    char* name;       // program name (may be NULL for older manifests)
    char* build_dir;
    manifest_pool_t* pools;
    size_t n_pools;
    manifest_command_t* commands;
    size_t n_commands;
} manifest_t;

// Serialize manifest to a JSON discovery response (caller must free result)
char* manifest_to_discovery_json(const manifest_t* manifest);

#endif
