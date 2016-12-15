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

%token VAR INT DBL STR EOS

%token BUILTIN GROUP

%token AS

%token LBRK RBRK LPAR RPAR SEP

/* TODO: incorporate all this new shit */
%token LANG LINE COND POS RARR BAR DEREF
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
%token COMPOSE
%token DEP 
%token EQUAL
%token LABEL
%token CONDITION

%%

/* a rat program consists of a series of sections*/
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


/* sections have their own strict rules on their contents */
section_export
  : SECTION_EXPORT
  | section_export lvar AS var { printf("EXPORT %s %s\n", $2, $4); }

section_path
  : SECTION_PATH
  | EOS
  | section_path named_path

section_alias
  : SECTION_ALIAS
  | section_alias var EQUAL var { printf("ALIAS %s %s\n", $2, $4); }

section_doc
  : SECTION_DOC
  | section_doc VAR COUPLE STR { printf("DOC %s %s\n", $2, $4); }

section_arg
  : SECTION_ARG
  | section_arg args_couplet { printf("ARG %s\n", $2); }

section_check
  : SECTION_CHECK
  | section_check var_couplet { printf("CHECK %s\n", $2); }

section_effect
  : SECTION_EFFECT
  | section_effect var_couplet { printf("EFFECT %s\n", $2); }

section_cache
  : SECTION_CACHE
  | section_cache var_couplet { printf("CACHE %s\n", $2); }

section_pack
  : SECTION_PACK
  | section_pack var_couplet { printf("PACK %s\n", $2); }

section_open
  : SECTION_OPEN
  | section_open var_couplet { printf("OPEN %s\n", $2); }

section_fail
  : SECTION_FAIL
  | section_fail var_couplet { printf("FAIL %s\n", $2); }

section_pass
  : SECTION_PASS
  | section_pass var_couplet { printf("PASS %s\n", $2); }


/* definitions for groups used directly by the section group above */
named_path
  : var COUPLE path { printf("PATH %s %s\n", $1, $3); }
;

path
  : source         { $$ = $1; }
  | path DEP var   { $$ = $3; printf("EMIT %s\n", $3); printf("LINK %s %s\n", $3, $1); }
  | path DEP con   { $$ = $3; printf("LINK %s %s\n", $3, $1); }
;
source
  : var { $$ = $1; printf("EMIT %s\n", $1); }
  | con { $$ = $1; }
  | source con { $$ = $2; }
  | source var { printf("EMIT %s\n", $2); $$ = get_str(); sprintf($$, "%s %s", $1, $2); }
  | LPAR path RPAR { $$ = $2; }
  | source LPAR path RPAR { $$ = get_str(); sprintf($$, "%s %s", $1, $3); }
;
con
  : LPAR var CONDITION lvar_list RPAR { $$ = $2; printf("COND %s %s\n", $2, $4); }
lvar_list
  : lvar SEP lvar      { $$ = get_str(); sprintf($$, "%s %s", $1, $3); }
  | lvar_list SEP lvar { $$ = get_str(); sprintf($$, "%s %s", $1, $3); }


composition
  : var COMPOSE var         { $$ = get_str(); sprintf($$, "%s %s", $1, $3); }
  | composition COMPOSE var { $$ = get_str(); sprintf($$, "%s %s", $1, $3); }
;

args_couplet
  : var COUPLE arg      { $$ = $1; printf("ARG %s %s\n", $1, $3); }
  | args_couplet SEP arg { $$ = $1; printf("ARG %s %s\n", $1, $3); }
;

var_couplet
  : var COUPLE var { $$ = get_str(); sprintf($$, "%s %s", $1, $3); }


/* groups used in defining argument lists */
arg
  : var EQUAL element { $$ = get_str(); sprintf($$, "%s %s", $1, $3); }
;

element
  : var       { $$ = $1; }
  | primitive { $$ = $1; }
  | array     { $$ = $1; }
;

array
  : LBRK list    RBRK { $$ = get_str(); sprintf($$, "array(%s)", $2); }
  | LBRK element RBRK { $$ = get_str(); sprintf($$, "array(%s)", $2); }
  | LBRK         RBRK { $$ = get_str(); sprintf($$, "array()", $2); }
;

list
  : element SEP element { $$ = get_str(); sprintf($$, "%s,%s", $1, $3); }
  | list SEP element    { $$ = get_str(); sprintf($$, "%s,%s", $1, $3); }
;

var
  : lvar    { $$ = $1; }
  | BUILTIN { $$ = $1; }
  | GROUP   { $$ = $1; }
;

lvar
  : VAR           { $$ = $1; }
  | VAR LABEL VAR { $$ = get_str(); sprintf($$, "%s:%s", $1, $3); printf("LABEL %s %s\n", $1, $3); }
  | VAR LABEL INT { $$ = get_str(); sprintf($$, "%s:%s", $1, $3); printf("LABEL %s %s\n", $1, $3); }
;

primitive
  : INT { $$ = $1; }
  | DBL { $$ = $1; }
  | STR { $$ = $1; }
;

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
