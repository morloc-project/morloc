#ifndef __COUPLET_H__
#define __COUPLET_H__

#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

typedef struct Couplet{
    W* lhs;
    W* rhs;
} Couplet;

Couplet* entry_new(W* lhs, W* rhs);

/* get an exact copy of an entry */
Couplet* entry_copy(const Couplet* e);

/* copy an entry and unlink it */
Couplet* entry_isolate(const Couplet* e);

void entry_print(const Couplet* e);

Couplet* entry_from_lhs(Class cls, const char* s){

Couplet* entry_add_rhs(Couplet* entry, void* rhs){

#endif
