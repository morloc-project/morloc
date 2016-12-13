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

%token AS WITH USING SPLIT ON MERGE

%token LBRK RBRK LPAR RPAR SEP

%token SECTION_EXPORT
%token SECTION_PATH
%token SECTION_ACTION
%token SECTION_ALIAS
%token SECTION_DOC
%token SECTION_CACHE
%token SECTION_CHECK
%token SECTION_EFFECT
%token SECTION_PACK
%token SECTION_OPEN
%token SECTION_FAIL
%token SECTION_PASS
%token SECTION_LOOP

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
  | section_action
  | section_alias
  | section_doc
  | section_cache
  | section_check
  | section_effect
  | section_pack
  | section_open
  | section_fail
  | section_pass
  | section_loop


/* sections have their own strict rules on their contents */
/* TODO remove section_compose and section_arg, add section_doc and section_action */
section_export
  : SECTION_EXPORT
  | section_export lvar AS var { printf("EXPORT %s %s\n", $2, $4); }

section_path
  : SECTION_PATH
  | EOS
  | section_path named_path

section_compose
  : SECTION_COMPOSE
  | section_compose var EQUAL composition { printf("COMPOSE %s %s\n", $2, $4); }

section_alias
  : SECTION_ALIAS
  | section_alias var EQUAL var { printf("ALIAS %s %s\n", $2, $4); }

section_check
  : SECTION_CHECK
  | section_check check_couplet

section_effect
  : SECTION_EFFECT
  | section_effect effect_couplet

section_arg
  : SECTION_ARG
  | section_arg args_couplet

section_cache
  : SECTION_CACHE
  | section_cache cache_couplet

section_pack
  : SECTION_PACK
  | section_pack pack_couplet

section_open
  : SECTION_OPEN
  | section_open open_couplet

section_fail
  : SECTION_FAIL
  | section_fail fail_couplet

section_pass
  : SECTION_PASS
  | section_pass pass_couplet

section_loop
  : SECTION_LOOP
  | section_loop loop_spec


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

check_couplet
  : var COUPLE var { printf("CHECK %s %s\n", $1, $3); }
;

effect_couplet
  : var COUPLE var { printf("EFFECT %s %s\n", $1, $3); }
;

cache_couplet
  : var COUPLE var { printf("CACHE %s %s\n", $1, $3); }
;

pack_couplet
  : var COUPLE var { printf("PACK %s %s\n", $1, $3); }
;

open_couplet
  : var COUPLE var { printf("OPEN %s %s\n", $1, $3); }
;

fail_couplet
  : var COUPLE var { printf("FAIL %s %s\n", $1, $3); }
;

pass_couplet
  : var COUPLE var { printf("PASS %s %s\n", $1, $3); }
;

loop_spec
  : WITH var                  { $$ = $2; }
  | loop_spec SPLIT USING var { $$ = $1; printf("LOOP_SPLIT_USING %s %s\n", $1, $4); }
  | loop_spec MERGE USING var { $$ = $1; printf("LOOP_MERGE_USING %s %s\n", $1, $4); }
  | loop_spec arg_split       { $$ = $1; printf("LOOP_SPLIT_ON %s %s\n",    $1, $2); }
;
arg_split
  : SPLIT     var ON var EQUAL element { $$ = get_str(); sprintf($$, "[%s %s %s]", $2, $4, $6); }
  | arg_split var ON var EQUAL element { $$ = get_str(); sprintf($$, "%s [%s %s %s]", $1, $2, $4, $6); }
;


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

int main(int argc, char ** argv){
    if(argc < 2){
        perror("Please provide a filename\n");
        return 1;
    }
    if(newfile(argv[1]))
        yyparse();
    return 0;
}
