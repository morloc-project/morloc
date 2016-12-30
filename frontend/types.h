#ifndef __TYPES_H__
#define __TYPES_H__

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>

typedef struct Couplet{
    struct W* lhs;
    struct W* rhs;
} Couplet;

typedef struct Manifold {
    char* function;
    struct Ws* effects; // Couplet<char*>
    struct Ws* inputs;  // Couplet<Manifold*>
} Manifold;

typedef struct Label{
    char* name;
    char* label;
} Label;

Manifold* manifold_new();
Label* label_new();
Couplet* couplet_new();


// ==== ASSIMILATE ME =================================================
// vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

// Couplet* entry_new(W* lhs, W* rhs);
//
// [> get an exact copy of an entry <]
// Couplet* entry_copy(const Couplet* e);
//
// [> copy an entry and unlink it <]
// Couplet* entry_isolate(const Couplet* e);
//
// void entry_print(const Couplet* e);
//
// Couplet* entry_from_lhs(Class cls, const char* s){
//
// Couplet* entry_add_rhs(Couplet* entry, void* rhs){

// ====================================================================

#endif
