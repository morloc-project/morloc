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

void ws_assert_class(const W*, Class);
void ws_assert_type(const W*, VType);

/* If ws is NULL, this will create a new Ws.  This basically renders ws_new
 * unnecessary, although I keep it around for symmetry. Also, I do not allow
 * empty Ws. Having this default constructor take an element argument will
 * prevent me from coming in later ad breaking everything by added a empty
 * default constructor.
 */
Ws* ws_add(Ws* ws, const W* w);

/* Make a new table that with the first element dropped */
Ws* ws_increment(const Ws* ws);

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
    const W*(*nextval)(const W*, const W*)
);


// like ws_prfilter, but modifies rather than filtering.
void ws_prmod(
    const Ws* ws,
    const W* p,
    Ws*(*recurse)(const W*, const W*),
    void(*mod)(const W*, const W*),
    const W*(*nextval)(const W*, const W*)
);

// maps ws_prmod over parameter list ps
void ws_map_pmod(Ws* xs, const Ws* ps, void(*pmod)(Ws*, const W*));

/* Turns one couplet into a list of couplets, each with a single path (lhs). */
Ws* ws_split_couplet(const W*);

/* A 'split' takes one thing and returns several:
 *
 * split :: a -> [b]
 *
 * map_split maps a split over a list and flattens the list:
 *
 * map_split :: [a] -> (a -> [b]) -> [b]
 *
 * Notice the flattening, the output isn't `[[b]]`
 *
 * contrast this to a simple map:
 *
 * map :: [a] -> (a -> b) -> [b]
 */
Ws* ws_map_split(const Ws* ws, Ws*(*split)(const W*));

void ws_mod(const Ws*, void(*mod)(const W*));

void ws_mod2(const Ws*, const Ws*, void(*mod)(const W*, const W*));

void ws_mod3(const Ws*, const Ws*, const Ws*, void(*mod)(const W*, const W*, const W*));

void ws_filter_mod(const Ws* top,
    Ws*(*xfilter)(const Ws*),
    void(*mod)(const W* x)
);

void ws_filter_2mod(const Ws* top,
    Ws*(*xfilter)(const Ws*),
    Ws*(*yfilter)(const Ws*),
    void(*mod)(const W* x, const W* y)
);

void ws_filter_3mod(const Ws* top,
    Ws*(*xfilter)(const Ws*),
    Ws*(*yfilter)(const Ws*),
    Ws*(*zfilter)(const Ws*),
    void(*mod)(const W* x, const W* y, const W* z)
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
const W* w_nextval_always(const W* p, const W* w);
const W* w_nextval_never(const W* p, const W* w);
const W* w_nextval_ifpath(const W* p, const W* w);

// Utilities
bool ws_cmp_lhs_to_label(W* c, Label* b);




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
// /* Given a composon, get an ordered list of the functions that produce output */
// Table* table_composon_outputs(const Entry* entry);
// 
// /* Given a composon, get a list of manifolds that receive input. This list is
//  * ordered, but needn't be, since order doesn't matter: each manifold is linked
//  * to each output of the following coposon. */
// Table* table_composon_inputs(const Entry* entry);
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
