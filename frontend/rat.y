%{

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "rat.tab.h"
#include "lex.yy.h"

int yylex(void);
void yyerror (char* s);
int newfile(char *fn);
int popfile(void);
char* get_str();

%}

%define api.value.type {char*}


/* named tokens on the left side of an assignemt (e.g. x=1) */
%token IDENTIFIER
/* named tokens on the right side (or anywhere else) */
%token VARIABLE COMPOSON

%token INT DBL STR LOG

%token AS

%token LANG LINE

%token SECTION_ONTOLOGY
%token SECTION_SOURCE
%token SECTION_TYPE
%token SECTION_ARG
%token SECTION_EXPORT
%token SECTION_PATH
%token SECTION_ALIAS
%token SECTION_DOC
%token SECTION_CACHE
%token SECTION_CHECK
%token SECTION_EFFECT
%token SECTION_PACK
%token SECTION_OPEN
%token SECTION_FAIL
%token SECTION_PASS

%token COUPLE
%token RARR

%%

input
  : %empty
  | input section
;

section
  : section_export
  | section_path
  | section_alias
  | section_doc
  | section_cache
  | section_check
  | section_effect
  | section_pack
  | section_open
  | section_fail
  | section_pass
  | section_type
  | section_source
  | section_ontology
  | section_arg


section_export
  : SECTION_EXPORT
  | section_export VARIABLE AS VARIABLE { printf("EXPORT %s %s\n", $2, $4); }

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
  | section_arg IDENTIFIER COUPLE argument { $$ = $2; printf("ARG %s %s\n", $2, $4); }
  | section_arg argument            { $$ = $1; printf("ARG %s %s\n", $1, $2); }
argument
  : IDENTIFIER '=' primitive { $$ = get_str(); sprintf($$, "%s %s", $1, $3); }
  | IDENTIFIER '=' array     { $$ = get_str(); sprintf($$, "%s %s", $1, $3); }
array
  : '[' list ']'  { $$ = get_str(); sprintf($$, "array(%s)", $2); }
  | '['      ']'   { $$ = get_str(); sprintf($$, "array()", $2); }
list
  : primitive
  | list ',' primitive
primitive
  : INT { $$ = $1; }
  | DBL { $$ = $1; }
  | STR { $$ = $1; }
  | LOG { $$ = $1; }

section_ontology
  : SECTION_ONTOLOGY
  | section_ontology construction
construction
  : IDENTIFIER COUPLE VARIABLE
  | construction '|' VARIABLE
  | construction VARIABLE

section_source
  : SECTION_SOURCE LANG { $$ = $2; }
  | section_source LINE { $$ = $1; printf("SOURCE %s %s\n", $1, $2); }

section_doc
  : SECTION_DOC
  | section_doc IDENTIFIER COUPLE STR { printf("DOC %s %s\n", $2, $4); }

section_alias
  : SECTION_ALIAS
  | section_alias IDENTIFIER COUPLE VARIABLE { printf("ALIAS %s %s\n", $2, $4); }

section_cache
  : SECTION_CACHE
  | section_cache IDENTIFIER COUPLE VARIABLE { printf("CACHE %s %s\n", $2, $4); }

section_pack
  : SECTION_PACK
  | section_pack IDENTIFIER COUPLE VARIABLE { printf("PACK %s %s\n", $2, $4); }

section_open
  : SECTION_OPEN
  | section_open IDENTIFIER COUPLE VARIABLE { printf("OPEN %s %s\n", $2, $4); }

section_fail
  : SECTION_FAIL
  | section_fail IDENTIFIER COUPLE VARIABLE { printf("FAIL %s %s\n", $2, $4); }

section_pass
  : SECTION_PASS
  | section_pass IDENTIFIER COUPLE VARIABLE { printf("PASS %s %s\n", $2, $4); }

%%

char* get_str(){
    char* a = (char*)malloc(128 * sizeof(char));
    return a;
}

void yyerror (char* s){
  printf ("%s\n", s);
}

FILE* toklog;

int main(int argc, char ** argv){
    if(argc < 2){
        perror("Please provide a filename\n");
        return 1;
    }
    if(newfile(argv[1]))
        toklog = fopen("tok.log", "w");
        yyparse();
    return 0;
}
