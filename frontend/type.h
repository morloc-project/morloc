#ifndef __TYPE_H__
#define __TYPE_H__

#define __IO__ "Void"
#define __MULTI__ "?"
#define __WILD__ "*"

#include "ws_access.h"

#define MAX_TYPE_LENGTH 1024

void set_default_types(Ws* ws);

// void infer_multi_types(Ws* ws);
// void infer_star_types(Ws* ws);
// void infer_generic_types(Ws* ws);
//
void all_io_types_are_compatible(Ws* ws_top);

char* type_str(W* w);

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
