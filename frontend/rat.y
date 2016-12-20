%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lex.yy.h"
#include "ril.h"

void yyerror (char* s);

RatStack* rs;

#define BUFFER_SIZE 1028
char* get_str();
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
 [x] make type and ontology
 [ ] make compositions and args
 [ ] allow multiple occurances of any sections
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

%type <NamedList*> section_type
%type <char*>      signature

%type <NamedList*> section_ontology
%type <NamedList*> construction

%type <NamedList*> section_export
%type <NamedList*> section_doc
%type <NamedList*> section_alias
%type <NamedList*> section_cache
%type <NamedList*> section_pack
%type <NamedList*> section_open
%type <NamedList*> section_fail
%type <NamedList*> section_pass
%type <NamedList*> section_source

%token COUPLE
%left RARR

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
  | section_check
  | section_effect
  | section_arg
  | section_type     { rs->type     = $1; }
  | section_ontology { rs->ontology = $1; }
  | section_doc      { rs->doc      = $1; }
  | section_alias    { rs->alias    = $1; }
  | section_cache    { rs->cache    = $1; }
  | section_pack     { rs->pack     = $1; }
  | section_open     { rs->open     = $1; }
  | section_fail     { rs->fail     = $1; }
  | section_pass     { rs->pass     = $1; }
  | section_source   { rs->source   = $1; }


section_export
  : SECTION_EXPORT { $$ = new_NamedList(); }
  | section_export VARIABLE AS VARIABLE { $$ = $1; COUPLET($$, $2, $4); }

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

section_type
  : SECTION_TYPE { $$ = new_NamedList(); }
  | section_type IDENTIFIER COUPLE signature {
      char* sig = get_str();
      sprintf(sig, ":: %s", $4);
      COUPLET($1, $2, sig);
      $$ = $1;
  }
signature
  : VARIABLE                 { $$ = $1; }
  | '(' signature ')'        { $$ = get_str(); sprintf($$, "( %s )", $2); }
  | signature RARR signature { $$ = get_str(); sprintf($$, "%s -> %s", $1, $3); }

section_ontology
  : SECTION_ONTOLOGY { $$ = new_NamedList(); }
  | section_ontology construction { JOIN($1,$2); $$ = $1; }
construction
  : IDENTIFIER COUPLE VARIABLE {
      $$ = new_NamedList();
      char* buffer = (char*)malloc(4096 * sizeof(char));
      sprintf(buffer, "%s ::", $1);
      COUPLET($$, buffer, $3);
  }
  | construction '|' VARIABLE {
      char* s = (char*)$1->value;
      strcpy(s + strlen(s), " | ");
      strcpy(s + strlen(s), $3);
      $$ = $1;
  }
  | construction VARIABLE { 
      char* s = (char*)$1->value;
      strcpy(s + strlen(s), " ");
      strcpy(s + strlen(s), $2);
      $$ = $1;
  }

section_source
  : SECTION_SOURCE LANG { $$ = new_NamedList(); $$->name = $2; }
  | section_source LINE { COUPLET($1, $1->name, $2); $$ = $1; }

section_doc
  : SECTION_DOC { $$ = new_NamedList(); }
  | section_doc IDENTIFIER COUPLE STR { $$ = $1; COUPLET($$, $2, $4); }

section_alias
  : SECTION_ALIAS { $$ = new_NamedList(); }
  | section_alias IDENTIFIER COUPLE VARIABLE { $$ = $1; COUPLET($$, $2, $4); }

section_cache
  : SECTION_CACHE { $$ = new_NamedList(); }
  | section_cache IDENTIFIER COUPLE VARIABLE { $$ = $1; COUPLET($$, $2, $4); }

section_pack
  : SECTION_PACK { $$ = new_NamedList(); }
  | section_pack IDENTIFIER COUPLE VARIABLE { $$ = $1; COUPLET($$, $2, $4); }

section_open
  : SECTION_OPEN { $$ = new_NamedList(); }
  | section_open IDENTIFIER COUPLE VARIABLE { $$ = $1; COUPLET($$, $2, $4); }

section_fail
  : SECTION_FAIL { $$ = new_NamedList(); }
  | section_fail IDENTIFIER COUPLE VARIABLE { $$ = $1; COUPLET($$, $2, $4); }

section_pass
  : SECTION_PASS { $$ = new_NamedList(); }
  | section_pass IDENTIFIER COUPLE VARIABLE { $$ = $1; COUPLET($$, $2, $4); }

%%

void yyerror (char* s){
  printf ("%s\n", s);
}

char* get_str(){
    char* c = (char*)calloc(BUFFER_SIZE, sizeof(char));
    return c;
}
