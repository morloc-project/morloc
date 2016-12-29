#ifndef __ENTRY_H__
#define __ENTRY_H__

#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "util.h"

typedef struct Couplet{
    int uid;
    Class cls;
    union {
        void*  none;
        char*  string;
        struct Label* label;
        struct Path*  path;
        struct List*  list;
    } lhs;
    union {
        char* string;
        struct Table*    table;
        struct Manifold* manifold;
        struct Effect*   effect;
    } rhs;
} Entry;

Entry* entry_new(VType vtype, void* key, void* value);

/* get an exact copy of an entry */
Entry* entry_copy(const Entry* e);

/* copy an entry and unlink it */
Entry* entry_isolate(const Entry* e);

void entry_print(const Entry* e);

Entry* entry_from_lhs(Class cls, const char* s){

Entry* entry_add_rhs(Entry* entry, void* rhs){





// typedef struct Id{
//     char* name;
//     char* label;
//     int uid;
// } Id;
//
// Id* id_new();
//
// Id* id_clone(Id* id);
//
// Id* id_from_str(char* s);
//
// bool id_cmp(Id* a, Id* b);


#endif
