#ifndef __HOF_H__
#define __HOF_H__

#include "ws.h"

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
    bool(*criterion)(const W*, const W*),
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
// parameterized recurse rules
Ws* ws_recurse_path(const W*, const W*);

// criteria functions
bool w_is_manifold(const W*);
bool w_keep_all(const W*);

// nextval functions
const W* w_nextval_always(const W* p, const W* w);
const W* w_nextval_never(const W* p, const W* w);
const W* w_nextval_ifpath(const W* p, const W* w);


#endif
