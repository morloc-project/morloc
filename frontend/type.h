#ifndef __TYPE_H__
#define __TYPE_H__

#define __IO__ "NULL"

#include "ws.h"
#include "hof.h"

void set_default_types(Ws* ws);

Ws* type_infer(Ws* ws);

bool type_is_well(Ws* type);
bool type_is_pipe(Ws* type);
bool type_is_sink(Ws* type);
bool type_is_effectful(Ws* type);

/* - pairwise compare inputs and types
 * - warn of missing type
 * - check consistency of inferred types
 * - honor syntax for IO
 */
W* type_check(Ws* ws);

void print_error(W*);

#endif
