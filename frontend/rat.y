%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lex.yy.h"
#include "ril.h"

void yyerror (char* s);

RatStack* rs;
%}

%code requires{
#include "types.h"
}

/*
1st TODO:
 [x] make union
 [x] structure for the doc couplets
 [x] structure for the other couplets
 [x] extend this to
 [x] - export
 [x] - source
 [ ] make single-operator, nested, linked list
 [ ] make type and ontology
 [ ] make two-operator, nested, linked list
 [ ] make compositions and args
2nd TODO:
 [ ] replace output printing with manifold creation
 [ ] write print function for the manifolds
 [ ] add symbol table
 [ ] add label-distinguished variables
 [ ] add comma separated assignments
 [ ] add positionals and derefs
3rd TODO:
 [ ] figure out how to do loops
4th TODO:
 [ ] add error handling
 [ ] pass file and line number from lexer
 [ ] integrate file and line number into error messages
5th TODO:
 [ ] compile ontology, type and composition sections into Haskell (with undef as bodies)
 [ ] interface with ghc to typecheck
*/

%define api.value.type union

/* named tokens on the left side of an assignemt (e.g. x=1) */
%token <char*> IDENTIFIER

/* named tokens on the right side (or anywhere else) */
%token <char*> VARIABLE 
%token COMPOSON

%token <char*> STR

%token INT LOG DBL

%token AS

%token <char*> LANG LINE

%token SECTION_ONTOLOGY
%token SECTION_SOURCE
%token SECTION_TYPE
%token SECTION_ARG
%token SECTION_EXPORT
%token SECTION_PATH
%token SECTION_CHECK
%token SECTION_EFFECT
%token SECTION_DOC
%token SECTION_ALIAS
%token SECTION_CACHE
%token SECTION_PACK
%token SECTION_OPEN
%token SECTION_FAIL
%token SECTION_PASS

%type <List*> section_export
%type <List*> section_doc
%type <List*> section_alias
%type <List*> section_cache
%type <List*> section_pack
%type <List*> section_open
%type <List*> section_fail
%type <List*> section_pass
%type <Source*> section_source

%token COUPLE
%token RARR

%%

final
  : input { print_RIL(rs); }

input
  : %empty { rs = new_RatStack(); }
  | input section
;

/* TODO: allow multiple sections */
section
  : section_export    { rs->export = $1; }
  | section_path
  | section_doc       { rs->doc = $1; }
  | section_check
  | section_effect
  | section_alias     { rs->alias = $1; }
  | section_cache     { rs->cache = $1; }
  | section_pack      { rs->pack  = $1; }
  | section_open      { rs->open  = $1; }
  | section_fail      { rs->fail  = $1; }
  | section_pass      { rs->pass  = $1; }
  | section_type
  | section_source    { ADD(rs->source, $1, Source*); }
  | section_ontology
  | section_arg


section_export
  : SECTION_EXPORT { $$ = new_List(); }
  | section_export VARIABLE AS VARIABLE { $$ = $1; PUT_COUPLET($$, $2, $4); }

/* --- composition sections ----------------------------------------------- */
section_path
  : SECTION_PATH
  | section_path IDENTIFIER COUPLE composition

section_effect
  : SECTION_EFFECT
  | section_effect IDENTIFIER COUPLE composition

section_check
  : SECTION_CHECK
  | section_check IDENTIFIER COUPLE composition

composition
  : COMPOSON
  | '(' composition ')'
  | composition COMPOSON
  | composition '(' composition ')'
  | composition '.' COMPOSON
  | composition '.' '(' composition ')'
/* ------------------------------------------------------------------------ */


section_type
  : SECTION_TYPE
  | section_type IDENTIFIER COUPLE signature
signature
  : VARIABLE RARR VARIABLE
  | '(' signature ')'
  | signature RARR VARIABLE
  | signature RARR '(' signature ')'

section_arg
  : SECTION_ARG
  | section_arg IDENTIFIER COUPLE argument
  | section_arg argument
argument
  : IDENTIFIER '=' primitive
  | IDENTIFIER '=' array
array
  : '[' list ']'
  | '['      ']'
list
  : primitive
  | list ',' primitive
primitive
  : INT
  | DBL
  | STR
  | LOG

section_ontology
  : SECTION_ONTOLOGY
  | section_ontology construction
construction
  : IDENTIFIER COUPLE VARIABLE
  | construction '|' VARIABLE
  | construction VARIABLE

section_source
  : SECTION_SOURCE LANG { $$ = new_Source($2); }
  | section_source LINE { $$ = $1; ADD($1->lines,$2,char*); }

section_doc
  : SECTION_DOC { $$ = new_List(); }
  | section_doc IDENTIFIER COUPLE STR { $$ = $1; PUT_COUPLET($$, $2, $4); }

section_alias
  : SECTION_ALIAS { $$ = new_List(); }
  | section_alias IDENTIFIER COUPLE VARIABLE { $$ = $1; PUT_COUPLET($$, $2, $4); }

section_cache
  : SECTION_CACHE { $$ = new_List(); }
  | section_cache IDENTIFIER COUPLE VARIABLE { $$ = $1; PUT_COUPLET($$, $2, $4); }

section_pack
  : SECTION_PACK { $$ = new_List(); }
  | section_pack IDENTIFIER COUPLE VARIABLE { $$ = $1; PUT_COUPLET($$, $2, $4); }

section_open
  : SECTION_OPEN { $$ = new_List(); }
  | section_open IDENTIFIER COUPLE VARIABLE { $$ = $1; PUT_COUPLET($$, $2, $4); }

section_fail
  : SECTION_FAIL { $$ = new_List(); }
  | section_fail IDENTIFIER COUPLE VARIABLE { $$ = $1; PUT_COUPLET($$, $2, $4); }

section_pass
  : SECTION_PASS { $$ = new_List(); }
  | section_pass IDENTIFIER COUPLE VARIABLE { $$ = $1; PUT_COUPLET($$, $2, $4); }

%%

void yyerror (char* s){
  printf ("%s\n", s);
}
