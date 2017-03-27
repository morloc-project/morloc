#ifndef __WS_H__
#define __WS_H__

#include <stdlib.h>
#include <string.h>

#include "w.h"

typedef struct Ws{
    W* head;
    W* last;
} Ws;

/* Copies entry and removes its link */
Ws* ws_new(W* w);

/* Creates new W containers, but preserves content */
Ws* ws_copy(Ws* ws);

/* Clone ws calling clone on each element. Elements with uid fields will have
 * unique uids, all pointers will be to new objects. Any changes to the clone,
 * any of its elements or sub-elements, will not affect the original.
 */
Ws* ws_clone(Ws* ws);
// clone the value of a W, this will recurse into and Ws
void w_clone_value(W* w);

/* If ws is NULL, this will create a new Ws.  This basically renders ws_new
 * unnecessary, although I keep it around for symmetry. Also, I do not allow
 * empty Ws. Having this default constructor take an element argument will
 * prevent me from coming in later ad breaking everything by added a empty
 * default constructor.
 */
Ws* ws_add(Ws* ws, W* w);
// add without copy
Ws* _ws_add(Ws* ws, W* w);

Ws* ws_add_val(Ws* ws, Class cls, void* v);

/* copies b (see ws_copy) and links it to a */
Ws* ws_join(Ws* a, Ws* b);

/* Make table xs[2..k], drop first element */
Ws* ws_tail(Ws* ws);
/* Make table xs[1..k-1], drop last element */
Ws* ws_init(Ws* ws);
/* Get first element of a table */
W* ws_head(Ws* ws);
/* Get last element of a table */
W* ws_last(Ws* ws);

int ws_length(Ws* ws);

void ws_print(Ws* ws, Ws*(*recurse)(W*));

/* This function is in ws.h instead of manifold.h because it
 * contains many Ws and W structures, but manifold.h does not
 * import the respective headers */
void manifold_print(Manifold* m);

char* w_str(W* w);


// The following functions are wrappers for Ws that are wrapped in a W.  They
// act on Ws in a W container. A Ws might be wrapped in a W when complete
// polymorphism is required. As in some of the higher-order functions.
W* wws_new(W*);
// Create new W->Ws but with Class other than P_WS
W* wws_new_cls(W*, Class ws_cls);
W* wws_clone(W*);
W* wws_add(W*, W*);
// add without copy
W* _wws_add(W*, W*);
W* wws_add_val(W*, Class cls, void* v);
W* wws_join(W*, W*);
W* wws_tail(W*);
W* wws_init(W*);
W* wws_head(W*);
W* wws_last(W*);
int wws_length(W*);
void wws_print(W*, Ws*(*recurse)(W*));


#endif
