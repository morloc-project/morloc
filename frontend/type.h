#ifndef __TYPE_H__
#define __TYPE_H__

#define __IO__ "NULL"

#include "ws.h"
#include "hof.h"

void set_default_types(const Ws* ws);

Ws* type_infer(const Ws* ws);

bool type_is_well(const Ws* type);
bool type_is_pipe(const Ws* type);
bool type_is_sink(const Ws* type);
bool type_is_effectful(const Ws* type);

/* - pairwise compare inputs and types
 * - warn of missing type
 * - check consistency of inferred types
 * - honor syntax for IO
 */
W* type_check(const Ws* ws);

#endif
