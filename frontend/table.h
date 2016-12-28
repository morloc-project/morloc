#ifndef __TABLE_H__
#define __TABLE_H__

#include <stdlib.h>
#include <string.h>

#include "entry.h"

typedef struct Table{
    Entry* head;
    Entry* tail;
} Table;

/* Copies entry and removes its link */
Table* table_new(const Entry* entry);

/* Given a composon, get an ordered list of the functions that produce output */
Table* table_composon_outputs(const Entry* entry);

/* Given a composon, get a list of manifolds that receive input. This list is
 * ordered, but needn't be, since order doesn't matter: each manifold is linked
 * to each output of the following coposon. */
Table* table_composon_inputs(const Entry* entry);

/* Copies entry and removes its link */
Table* table_add(Table* table, const Entry* entry);

/* b is destroyed upon join */
Table* table_join(Table* a, Table* b);

Table* table_selection_get(const Table* table, Selection* name, TType type); 

Table* table_recursive_get(const Table* table, Id* id, TType type); 

Table* table_recursive_get_type(const Table* table, TType type);

#endif
