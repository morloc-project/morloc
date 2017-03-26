#ifndef __TYPE_HOMOSET_H__
#define __TYPE_HOMOSET_H__

#include "ws_access.h"
#include "type_util.h"


// LL of elements with equivalent type
// Reasons for joining
// 1. The elements are IO linked
// 2. The elements are stars at the same position within the same function,
// though different manifolds
typedef struct HomoSet{
   // Types a and b must be unified, of form FT_*
   W* type;
   // modifiers for accessing generics
   // they will have the value `(m->uid * 26) - 97`, thus adding the character
   // numeric value will result in a value of range 0-MAX_INT.
   int gmod;
   struct HomoSet* next;
   struct HomoSet* prev;
} HomoSet;

HomoSet* append_HomoSet(HomoSet* hs, W* type, Manifold* m);
void print_HomoSet(HomoSet* hs);

#endif
