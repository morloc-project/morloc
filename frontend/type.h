#ifndef __TYPE_H__
#define __TYPE_H__

#include "ws_access.h"
#include "type_util.h"
#include "type_GenericList.h"
#include "type_HomoSetList.h"
#include "type_ManifoldList.h"

void set_default_types(Ws* ws);

// void infer_multi_types(Ws* ws);
// void infer_star_types(Ws* ws);
// void infer_generic_types(Ws* ws);
//
void all_io_types_are_compatible(Ws* ws_top);
void test_ManifoldList(Ws* ws_top);

/* - pairwise compare inputs and types
 * - warn of missing type
 * - check consistency of inferred types
 * - honor syntax for IO
 */
W* type_check(Ws* ws);

void print_error(W*);

#endif
