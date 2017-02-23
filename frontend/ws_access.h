#ifndef __WS_ACCESS_H__
#define __WS_ACCESS_H__

#include "hof.h"

// Removing nesting in a list (as specified by the recursion rule).
// This is just a wrapper for ws_rfilter, with criterion := w_keep_all.
Ws* ws_flatten(Ws*, Ws*(*recurse)(W*));

Ws* get_manifolds(Ws* ws);
Ws* get_refers(Ws* ws);

// Get top-level paths, (e.g. the 'A' in `A :: f . g`, which is a list
// containing f and g)
Ws* get_tpaths(Ws* ws);

Ws* get_by_name(Ws* ws, W* p);

bool w_is_lang      ( W* );
bool w_is_export    ( W* );
bool w_is_source    ( W* );
bool w_is_manifold  ( W* );
bool w_is_grpref    ( W* );
bool w_is_argref    ( W* );
bool w_is_refer     ( W* );
bool w_is_label     ( W* );
bool w_is_deref     ( W* );
bool w_is_tpath     ( W* );
bool w_is_composon  ( W* );
bool w_is_recursive ( W* );
bool w_is_type      ( W* );

bool w_equal_lhs    ( W* a, W* b );
bool w_string_equal ( W* a, W* b );

/* Turns one couplet into a list of couplets, each with a single path (lhs). */
Ws* ws_split_couplet(W*);

// recurse rules
Ws* ws_recurse_ws(W*);   // recurse into V_WS
Ws* ws_recurse_most(W*); // recurse into V_WS and V_COUPLET (but not manifolds)
Ws* ws_recurse_none(W*); // no recursion
Ws* ws_recurse_composition(W*); // recurse into T_PATH and C_NEST
Ws* ws_recurse_section(W*); // recurse only into sections
// parameterized recurse rules
Ws* ws_recurse_path(W*, W*);

// criteria functions
bool w_keep_all(W*);

// nextval functions
W* w_nextval_always(W* p, W* w);
W* w_nextval_never(W* p, W* w);
W* w_nextval_ifpath(W* p, W* w);

#endif
