#ifndef __ENTRY_H__
#define __ENTRY_H__

#include <stdio.h>

#include "effect.h"
#include "manifold.h"
#include "id.h"


typedef enum {
    T_UNDEFINED=0,
    T_EFFECT,
    T_PATH,
    C_COMPOSON,
    C_MANIFOLD,
    C_NEST,
    C_GRPREF,
    C_POSITIONAL,
    C_DEREF
} TType;

typedef struct Entry{
    Id* id;
    TType type;
    struct Entry* next;
    union {
        Effect* effect;
        struct Table* table;
        char* string;
        Manifold* manifold;
    } value;
} Entry;

Entry* entry_new(Id* id, TType type, void* value);

/* get an exact copy of an entry */
Entry* entry_copy(const Entry* e);

/* copy an entry and unlink it */
Entry* entry_isolate(const Entry* e);

void entry_print(const Entry* e);

#endif
