#ifndef __TYPE_H__
#define __TYPE_H__

#include "ws.h"
#include "hof.h"

void set_default_types(const Ws* ws);

Ws* infertype(const Ws* ws);

/* - pairwise compare inputs and types
 * - warn of missing type
 * - check consistency of inferred types
 * - honor syntax for IO
 */
void typecheck(const Ws* ws);

#endif
