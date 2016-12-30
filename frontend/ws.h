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

/* Recursively copy the input table.
 *   - Names and labels in Id structs are copied.
 *   - Type is preserved
 *   - Group references are NOT resolved, but copied as strings
 *   - Entry and Manifold objects have new uids.
 *   - All manifold properties are cleared.
 *   - Only items in a PATH are cloned, anything else is an error
 */
Table* table_clone(const Table* table);

/* recursively print the contents of a table */
void table_dump(const Table* table);

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


/* ****** NOTE ***************************************************************
 * For all the *get* functions, the returned Entry's are copies.  Modification
 * of the returned Entry's will not affect the contents of the input table.
 * However, the values the Entry's contain are still pointers to the realities,
 * so they CAN be changed.
 * **************************************************************************/

Table* table_get(const Table* table, Id* id, TType type);

Table* table_selection_get(const Table* table, Selection* name, TType type); 

Table* table_recursive_get(const Table* table, Id* id, TType type); 

Table* table_recursive_get_type(const Table* table, TType type);

#endif
