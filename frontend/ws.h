#ifndef __WS_H__
#define __WS_H__

#include <stdlib.h>
#include <string.h>

#include "w.h"

typedef struct Ws{
    W* head;
    W* tail;
} Ws;

/* Copies entry and removes its link */
Ws* ws_new(const W* w);

/* If ws is NULL, this will create a new Ws.  This basically renders ws_new
 * unnecessary, although I keep it around for symmetry. Also, I do not allow
 * empty Ws. Having this default constructor take an element argument will
 * prevent me from coming in later ad breaking everything by added a empty
 * default constructor.
 */
Ws* ws_add(Ws* ws, const W* w);

/* b is destroyed upon join */
Ws* ws_join(Ws* a, Ws* b);

Ws* ws_add_val(Ws* ws, Class cls, void* v);

int ws_length(const Ws* ws);

void ws_print(const Ws* ws, Ws*(*recurse)(const W*));

// Recursively moves through a Ws, accumulating W that meet a criterion into a flat list
Ws* ws_rfilter(
    const Ws*,
    Ws*(*recurse)(const W*),
    bool(*criterion)(const W*)
);

// Parameterized version of ws_rfilter
Ws* ws_prfilter(
    const Ws*,
    const W*,
    Ws*(*recurse)(const W*, const W*),
    bool(*criterion)(const W*, const W*),
    W*(*nextval)(const W*)
);

Ws* ws_map(const Ws*, W*(*map)(const W*));

Ws* ws_map2(const Ws*, const Ws*, W*(*map)(const W*, const W*));

Ws* ws_map3(const Ws*, const Ws*, const Ws*, W*(*map)(const W*, const W*, const W*));

Ws* ws_filter_map(const Ws* top,
    Ws*(*xfilter)(const Ws*),
    W*(*map)(const W* x)
);

Ws* ws_filter_2map(const Ws* top,
    Ws*(*xfilter)(const Ws*),
    Ws*(*yfilter)(const Ws*),
    W*(*map)(const W* x, const W* y)
);

Ws* ws_filter_3map(const Ws* top,
    Ws*(*xfilter)(const Ws*),
    Ws*(*yfilter)(const Ws*),
    Ws*(*zfilter)(const Ws*),
    W*(*map)(const W* x, const W* y, const W* z)
);

// Removing nesting in a list (as specified by the recursion rule).
// This is just a wrapper for ws_rfilter, with criterion := w_keep_all.
Ws* ws_flatten(const Ws*, Ws*(*recurse)(const W*));

// recurse rules
Ws* ws_recurse_ws(const W*);   // recurse into V_WS
Ws* ws_recurse_most(const W*); // recurse into V_WS and V_COUPLET (but not manifolds)
Ws* ws_recurse_none(const W*); // no recursion
// parameterized recurse rules
Ws* ws_recurse_path(const W*, const W*);

// criteria functions
bool w_is_manifold(const W*);
bool w_keep_all(const W*);
// parameterized criteria functions
bool w_name_match(const W*, const W*);

// nextval functions
W* w_next(W*);
W* w_id(W*);


// ==== ASSIMILATE ME =================================================
// vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

// void w_free(W* o);
//
// W* w_clone(W* o);
//
// W* w_set_value(W* o, void* v);
//
// W* w_add_over(W* a, W* b);
//
// W* w_add_down(W* a, W* b);
//
// W* w_next(const W* o);
//
// W* w_down(const W* o);
//
// W* w_head(const W* o);
//
// W* w_tail(const W* o);
//
// W* w_map( const W* o, W*(*map)(W*) );
//
// W* w_dmap( const W* o, W*(*map)(W*), int depth );
//
// W* w_rmap( const W* o, W*(*map)(W*) );
//
/* Recursive map function with recursion rule (digger). The recursion rule
 * function takes a W* and derives the lower W* from the original value. */
// W* w_dig( const W* o, W*(*digger)(W*), W*(*map)(W*) );
//
// [> performs a pairwise function on two W's <]
// W* w_zmap( const W* a, const W* b, W*(*map)(W*, W*) );
//
// W* w_zdmap( const W* a, const W* b, W*(*map)(W*, W*), int depth );
//
// W* w_zrmap( const W* a, const W* b, W*(*map)(W*, W*) );
//
// W* w_filter( const W* o, bool(*filter)(W*) );
//
// W* w_flatten(const W* o);
//
// bool w_is_alone(const W* o);
//
// bool w_is_iterative(const W* o);
//
// bool w_is_recursive(const W* o);
//
// bool w_is_collective(const W* o);
//
// bool w_is_homogenous(const W* o);



// /* Recursively copy the input table.
//  *   - Names and labels in Id structs are copied.
//  *   - Type is preserved
//  *   - Group references are NOT resolved, but copied as strings
//  *   - Entry and Manifold objects have new uids.
//  *   - All manifold properties are cleared.
//  *   - Only items in a PATH are cloned, anything else is an error
//  */
// Table* table_clone(const Table* table);
// 
// /* recursively print the contents of a table */
// void table_dump(const Table* table);
// 
// /* Given a composon, get an ordered list of the functions that produce output */
// Table* table_composon_outputs(const Entry* entry);
// 
// /* Given a composon, get a list of manifolds that receive input. This list is
//  * ordered, but needn't be, since order doesn't matter: each manifold is linked
//  * to each output of the following coposon. */
// Table* table_composon_inputs(const Entry* entry);
// 
// /* Copies entry and removes its link */
// Table* table_add(Table* table, const Entry* entry);
// 
// /* b is destroyed upon join */
// Table* table_join(Table* a, Table* b);
// 
// 
// /* ****** NOTE ***************************************************************
//  * For all the *get* functions, the returned Entry's are copies.  Modification
//  * of the returned Entry's will not affect the contents of the input table.
//  * However, the values the Entry's contain are still pointers to the realities,
//  * so they CAN be changed.
//  * **************************************************************************/
// 
// Table* table_get(const Table* table, Id* id, TType type);
// 
// Table* table_selection_get(const Table* table, Selection* name, TType type); 
// 
// Table* table_recursive_get(const Table* table, Id* id, TType type); 
// 
// Table* table_recursive_get_type(const Table* table, TType type);

#endif
