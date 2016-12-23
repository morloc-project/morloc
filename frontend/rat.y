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
#include "composition.h"
#include "label.h"
#include "list.h"
#include "manifold.h"
#include "ril.h"
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
 [x] make compositions
 [x] add unique manifold ids
 [x] add Manifold structures to Composon
 [x] allow path specification
 [ ] pass all couplets down the tree
 [ ] make args
 [ ] allow multiple occurances of any sections
2nd TODO:
 [ ] write RIL print function for the manifolds
 [ ] add comma separated assignments
 [ ] add positionals and derefs
 [ ] add conditionals
3rd TODO:
 [ ] figure out how to do loops
4th TODO:
 [ ] add syntax error handling
 [ ] add checking of manifold builds
 [ ] pass file and line number from lexer
 [ ] integrate file and line number into error messages
5th TODO:
 [ ] compile ontology, type and composition sections into Haskell (with undef as bodies)
 [ ] interface with ghc to typecheck
*/

%define api.value.type union

/* named tokens on the left side of an assignemt (e.g. x=1) */
%token <Label*> IDENTIFIER
%type <Label*> manifold

/* named tokens on the right side (or anywhere else) */
%token <char*> VARIABLE 
%token <char*> GROUP 
%token <Composon*> COMPOSON

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

%type <List*> section_type
%type <char*> signature

%type <List*> section_ontology
%type <List*> construction

%type <NamedList*> section_export
%type <NamedList*> section_doc
%type <NamedList*> section_alias
%type <NamedList*> section_cache
%type <NamedList*> section_pack
%type <NamedList*> section_open
%type <NamedList*> section_fail
%type <NamedList*> section_pass
%type <NamedList*> section_source

%type <NamedList*> section_path
%type <NamedList*> section_effect
%type <NamedList*> section_check
%type <List*> composition /* list of Composon lists */

%token COUPLE
%left RARR

%left '.'
%precedence CONCAT

%%

final
  : input { print_RIL(rs); }

input
  : %empty { rs = ratstack_new(); }
  | input section
;

/* TODO: allow multiple sections */
section
  : section_export   { rs->export = $1; }
  | section_path     { rs->path = $1; }
  | section_check    { rs->check = $1; }
  | section_effect   { rs->effect = $1; }
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

/* --- composition sections ----------------------------------------------- */
section_path
  : SECTION_PATH { $$ = namedlist_new(); }
  | section_path IDENTIFIER COUPLE composition { $$ = $1; ADD($$, $2, $4, List*); }

section_effect
  : SECTION_EFFECT { $$ = namedlist_new(); }
  | section_effect manifold COUPLE composition { $$ = $1; ADD($$, $2, $4, List*); }

section_check
  : SECTION_CHECK { $$ = namedlist_new(); }
  | section_check manifold COUPLE composition { $$ = $1; ADD($$, $2, $4, List*); }

composition
  : COMPOSON                             { $$ = composition_new($1); }
  | '(' composition ')'                  { $$ = composition_new_nested($2); }
  | composition composition %prec CONCAT { $$ = composition_add_down($1, $2); }
  | composition '.' composition          { $$ = composition_compose($1, $3); }

/* --- argument section --------------------------------------------------- */
section_arg
  : SECTION_ARG
  | section_arg manifold COUPLE argument
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
/* ------------------------------------------------------------------------ */

section_export
  : SECTION_EXPORT { $$ = namedlist_new(); }
  | section_export IDENTIFIER AS VARIABLE { $$ = $1; COUPLET($$, $2, $4); }

section_type
  : SECTION_TYPE { $$ = list_new(); }
  | section_type IDENTIFIER COUPLE signature {
      List* l = list_new();
      l->value = get_str();
      sprintf(l->value, "%s :: %s", $2->name, $4);
      JOIN($$, l);
  }
signature
  : VARIABLE                 { $$ = $1; }
  | '(' signature ')'        { $$ = get_str(); sprintf($$, "( %s )", $2); }
  | signature RARR signature { $$ = get_str(); sprintf($$, "%s -> %s", $1, $3); }

section_ontology
  : SECTION_ONTOLOGY { $$ = list_new(); }
  | section_ontology construction { JOIN($1,$2); $$ = $1; }
construction
  : IDENTIFIER COUPLE VARIABLE {
      $$ = list_new();
      char* buffer = (char*)malloc(4096 * sizeof(char));
      sprintf(buffer, "%s :: %s", $1->name, $3);
      $$->value = buffer;
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
  : SECTION_SOURCE LANG { $$ = namedlist_new(); $$->name = label_new($2); }
  | section_source LINE { COUPLET($1, $1->name, $2); $$ = $1; }

section_doc
  : SECTION_DOC { $$ = namedlist_new(); }
  | section_doc manifold COUPLE STR { $$ = $1; COUPLET($$, $2, $4); }

section_alias
  : SECTION_ALIAS { $$ = namedlist_new(); }
  | section_alias manifold COUPLE VARIABLE { $$ = $1; COUPLET($$, $2, $4); }

section_cache
  : SECTION_CACHE { $$ = namedlist_new(); }
  | section_cache manifold COUPLE VARIABLE { $$ = $1; COUPLET($$, $2, $4); }

section_pack
  : SECTION_PACK { $$ = namedlist_new(); }
  | section_pack manifold COUPLE VARIABLE { $$ = $1; COUPLET($$, $2, $4); }

section_open
  : SECTION_OPEN { $$ = namedlist_new(); }
  | section_open manifold COUPLE VARIABLE { $$ = $1; COUPLET($$, $2, $4); }

section_fail
  : SECTION_FAIL { $$ = namedlist_new(); }
  | section_fail manifold COUPLE VARIABLE { $$ = $1; COUPLET($$, $2, $4); }

section_pass
  : SECTION_PASS { $$ = namedlist_new(); }
  | section_pass manifold COUPLE VARIABLE { $$ = $1; COUPLET($$, $2, $4); }


/* ------------------------------------------------------------------------ */
manifold
  : IDENTIFIER   { $$ = $1; }
  | IDENTIFIER '/' manifold { $$ = $1; $$->next = $3; }

%%

void yyerror (char* s){
  printf ("%s\n", s);
}

char* get_str(){
    char* c = (char*)calloc(BUFFER_SIZE, sizeof(char));
    return c;
}
