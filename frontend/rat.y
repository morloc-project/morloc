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
 [ ] extend this to
 [ ] - export
 [ ] - source
 [ ] - doc
 [ ] make single-operator, nested, linked list
 [ ] make type and ontology
 [ ] make two-operator, nested, linked list
 [ ] make compositions and args
 [ ] add printing for everything
2nd TODO:
 [ ] add symbol table
 [ ] add label-distinguished variables
 [ ] add comma separated assignments
 [ ] add positionals and derefs
3rd TODO:
 [ ] figure out how to do loops
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

%token LANG LINE

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

%type <CoupletStack*> section_doc
%type <CoupletStack*> section_alias
%type <CoupletStack*> section_cache
%type <CoupletStack*> section_pack
%type <CoupletStack*> section_open
%type <CoupletStack*> section_fail
%type <CoupletStack*> section_pass

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
  : section_export
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
  | section_source
  | section_ontology
  | section_arg


section_export
  : SECTION_EXPORT
  | section_export VARIABLE AS VARIABLE

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
  : SECTION_SOURCE LANG
  | section_source LINE

section_doc
  : SECTION_DOC { $$ = new_CoupletStack(); }
  | section_doc IDENTIFIER COUPLE STR { $$ = $1; put_Couplet($$, $2, $4); }

section_alias
  : SECTION_ALIAS { $$ = new_CoupletStack(); }
  | section_alias IDENTIFIER COUPLE VARIABLE { $$ = $1; put_Couplet($$, $2, $4); }

section_cache
  : SECTION_CACHE { $$ = new_CoupletStack(); }
  | section_cache IDENTIFIER COUPLE VARIABLE { $$ = $1; put_Couplet($$, $2, $4); }

section_pack
  : SECTION_PACK { $$ = new_CoupletStack(); }
  | section_pack IDENTIFIER COUPLE VARIABLE { $$ = $1; put_Couplet($$, $2, $4); }

section_open
  : SECTION_OPEN { $$ = new_CoupletStack(); }
  | section_open IDENTIFIER COUPLE VARIABLE { $$ = $1; put_Couplet($$, $2, $4); }

section_fail
  : SECTION_FAIL { $$ = new_CoupletStack(); }
  | section_fail IDENTIFIER COUPLE VARIABLE { $$ = $1; put_Couplet($$, $2, $4); }

section_pass
  : SECTION_PASS { $$ = new_CoupletStack(); }
  | section_pass IDENTIFIER COUPLE VARIABLE { $$ = $1; put_Couplet($$, $2, $4); }

%%

void yyerror (char* s){
  printf ("%s\n", s);
}
