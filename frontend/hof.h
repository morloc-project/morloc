#ifndef __HOF_H__
#define __HOF_H__

#include "ws.h"

// Recursively moves through a Ws, accumulating W that meet a criterion into a flat list
Ws* ws_rfilter(
    const Ws*,
    Ws*(*recurse)(const W*),
    bool(*criterion)(const W*)
);

// Non-recursive filter
Ws* ws_filter(
    const Ws*,
    bool(*criterion)(const W*)
);

// Non-recursive parameterized filter
Ws* ws_pfilter(const Ws*, const W*, bool(*criterion)(const W*, const W*));

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
    bool(*criterion)(const W*, const W*),
    void(*mod)(const W*, const W*),
    const W*(*nextval)(const W*, const W*)
);

void ws_recursive_reduce_mod(
    const Ws* ws,
    Ws*(*recurse)(const W*),
    bool(*l_criterion)(const W*),
    bool(*r_criterion)(const W*),
    void(*mod)(const W*, const W*)
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

// Maps over 1, 2, or 3 variables. All combinations are considered, that is,
// ws_2mod is quadratic and ws_3mod is cubic.
void ws_mod(const Ws*, void(*mod)(const W*));
void ws_2mod(const Ws*, const Ws*, void(*mod)(const W*, const W*));
void ws_3mod(const Ws*, const Ws*, const Ws*, void(*mod)(const W*, const W*, const W*));

// calls mod(xs[i], ys[i]) for all i. If as and bs are of unequal length, scream.
void ws_zip_mod(const Ws* xs, const Ws* ys, void(*mod)(const W* x, const W* y));
// stateful zip apply
W* ws_szap(const Ws* xs, const Ws* ys, W* st, W*(*mod)(const W* x, const W* y, W* st));

// Recurse along ws according to `recurse`. Perform function `mod` on all w if
// `criterion`. ws in `mod` are processed in the context of `ps`, which may,
// for example, be a symbol table. 
void ws_ref_rmod(
    const Ws* ws,
    const Ws* ps,
    Ws*(*recurse)(const W*),
    bool(*criterion)(const W*),
    void(*mod)(W*, const Ws*)
);

// Recursive Conditional Modifier
void ws_rcmod(
    const Ws* ws,
    Ws*(*recurse)(const W*),
    bool(*criterion)(const W*),
    void(*mod)(W*)
);

// Stateful Conditional Recursive Apply
W* ws_scrap(
    const Ws* ws,
    W* st,
    Ws*(*recurse)(const W*),
    bool(*criterion)(const W*),
    W*(*mod)(W* w, W* st)
);

void ws_filter_mod(
    const Ws* top,
    Ws*(*xfilter)(const Ws*),
    void(*mod)(const W* x)
);

void ws_filter_2mod(
    const Ws* top,
    Ws*(*xfilter)(const Ws*),
    Ws*(*yfilter)(const Ws*),
    void(*mod)(const W* x, const W* y)
);

void ws_filter_3mod(
    const Ws* top,
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
Ws* ws_recurse_composition(const W*); // recurse into T_PATH and C_NEST
// parameterized recurse rules
Ws* ws_recurse_path(const W*, const W*);

// criteria functions
bool w_is_manifold(const W*);
bool w_is_type(const W*);
bool w_is_composon(const W*);
bool w_keep_all(const W*);

// nextval functions
const W* w_nextval_always(const W* p, const W* w);
const W* w_nextval_never(const W* p, const W* w);
const W* w_nextval_ifpath(const W* p, const W* w);


#endif
