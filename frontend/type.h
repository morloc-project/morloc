#ifndef __TYPE_H__
#define __TYPE_H__

#include "ws_access.h"
#include "type_util.h"
#include "type_GenericList.h"
#include "type_HomoSetList.h"
#include "type_ManifoldList.h"

void set_default_types(Ws* ws);

void print_type_debug_info(Ws* ws_top);

void print_error(W*);

#endif
