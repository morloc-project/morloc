%{
#include <stdio.h>
#include <ctype.h>

#include "loc.tab.h"

int newfile(char *fn);
int popfile(void);

FILE* toklog;

#define LOG(x) fprintf(toklog, x); fflush(toklog);
#define LOGV(x,a) fprintf(toklog, x, a); fflush(toklog);

#define RETURN(x, t) \
    LOG(#t " ");     \
    yylval.t = x;    \
    return t;

#define RETURN_TOKEN(t) \
    LOG(#t " ");        \
    return t;
    

// Removes first and last characters around a string
char* unquote(const char* in_str);
// Removes leading whitespace and trailing whitspace and c in in_str
// For example, `trim_back("x ::", ":") --> "x"`
char* trim_back(const char* in_str, const char* c);
// Same as `trim_back(x, "")`
char* trim(const char* in_str);
// Same as `trim_back(x, ":")`
char* trim_couple(const char* in_str);
// After trimming, it is often necessary to put characters back into the buffer:
// unput a specific string
#define UNPUT_S(x) for(int i = strlen(x)-1; i >= 0; i--){ unput(x[i]); }
// unput k characters
#define UNPUT_N(k) \
    char* zs = strdup(yytext); \
    int zn = strlen(yytext); \
    for(int zi = 0; zi < k; zi++){ unput(zs[zn - zi - 1]); } \
    free(zs);

%}

%option header-file="lex.yy.h"
%option noyywrap pointer yylineno

%s S_TYPE
%s S_ONTOLOGY
%s S_INCLUDE
%s S_EXPORT
%x X_COMMENT
%x X_SOURCE
%s S_LANG
%s S_PATH
%s S_SIDE
%s S_CPLT
%s S_ARG

ws        [ \t\n\r]
comment   #.*\n

sym       [a-zA-Z_][a-zA-Z0-9_]*
var       {sym}(:{sym})?
path      {var}({ws}*\/{ws}*{var})*
selection {path}(,{path})*{ws}*:[:=+-]

type {sym}|\[{sym}\]|\*

identifier {var}{ws}*:[:=+-]

parameter [a-zA-Z0-9_.-]+{ws}*=
flag [-][a-zA-Z0-9_.-]+

grpref     \*{var}
positional `[^`]+`
refer      <[^<]+>

str \'[^']*\'|\"[^"]*\"
int -?([0-9]|[1-9][0-9]+)
dbl -?{int}\.[0-9]+
lgc TRUE|FALSE|NULL 

couple ::
modify :=|:-|:\+

%%


{comment} { }

@comment {  BEGIN(X_COMMENT); }
@include {  BEGIN(S_INCLUDE); }

@path     { BEGIN(S_PATH)     ; RETURN_TOKEN ( SECTION_PATH     ) ; }
@effect   { BEGIN(S_SIDE)     ; RETURN_TOKEN ( SECTION_EFFECT   ) ; }
@hook     { BEGIN(S_SIDE)     ; RETURN_TOKEN ( SECTION_HOOK     ) ; }
@check    { BEGIN(S_SIDE)     ; RETURN_TOKEN ( SECTION_CHECK    ) ; }
@fail     { BEGIN(S_SIDE)     ; RETURN_TOKEN ( SECTION_FAIL     ) ; }
@source   { BEGIN(S_LANG)     ; RETURN_TOKEN ( SECTION_SOURCE   ) ; }
@type     { BEGIN(S_TYPE)     ; RETURN_TOKEN ( SECTION_TYPE     ) ; }
@ontology { BEGIN(S_ONTOLOGY) ; RETURN_TOKEN ( SECTION_ONTOLOGY ) ; }
@cache    { BEGIN(S_CPLT)     ; RETURN_TOKEN ( SECTION_CACHE    ) ; }
@open     { BEGIN(S_CPLT)     ; RETURN_TOKEN ( SECTION_OPEN     ) ; }
@pack     { BEGIN(S_CPLT)     ; RETURN_TOKEN ( SECTION_PACK     ) ; }
@pass     { BEGIN(S_CPLT)     ; RETURN_TOKEN ( SECTION_PASS     ) ; }
@alias    { BEGIN(S_CPLT)     ; RETURN_TOKEN ( SECTION_ALIAS    ) ; }
@lang     { BEGIN(S_CPLT)     ; RETURN_TOKEN ( SECTION_LANG     ) ; }
@doc      { BEGIN(S_CPLT)     ; RETURN_TOKEN ( SECTION_DOC      ) ; }
@arg      { BEGIN(S_ARG)      ; RETURN_TOKEN ( SECTION_ARG      ) ; }
@export   { BEGIN(S_EXPORT)   ; RETURN_TOKEN ( SECTION_EXPORT   ) ; }

{ws} { LOGV("%s", yytext); }

<S_LANG>[ ]+{var} {
    BEGIN X_SOURCE;
    W* w = w_new(P_STRING, trim(yytext));
    RETURN(w, STR);
  }
<X_SOURCE>^[ \t]*([^\n@].*)?\n {
    char* s = trim(yytext);
    W* w = w_new(P_STRING, s);
    RETURN(w, STR);
}
<X_SOURCE>@ { unput('@'); BEGIN INITIAL; } /* allow @ anywhere except ^ */


<X_COMMENT>@      { unput('@'); BEGIN INITIAL; }
<X_COMMENT>(.|\n) { /* toss bodies */ }
<X_COMMENT>\\@    { /* allow escaped @ */ }

<S_EXPORT>as { RETURN_TOKEN(AS); }
<S_EXPORT>{var} {
    W* w = w_new(P_STRING, strdup(yytext));
    RETURN(w, STR);
}

<S_INCLUDE>{str} {
    char* s = unquote(yytext);
    LOGV("including(%s)\n", s);
    if(!newfile(s)) yyterminate();
  }
<<EOF>> { if(!popfile()) yyterminate(); UNPUT_S("\n@include\n"); }

<S_PATH,S_SIDE,S_CPLT,S_ARG,S_TYPE,S_ONTOLOGY>{couple} { RETURN_TOKEN(COUPLE); }
<S_PATH,S_SIDE,S_CPLT,S_ARG,S_TYPE,S_ONTOLOGY>{modify} { RETURN_TOKEN(COUPLE); }

<S_PATH>{identifier} {
    W* w = label_from_str(trim_couple(yytext));
    UNPUT_N(2)
    RETURN(w, IDENTIFIER);
}


<S_PATH,S_SIDE>{var} {
    Manifold* m = manifold_new();
    W* l = label_from_str(yytext);
    W* r = w_new(P_MANIFOLD, m);
    Couplet* c = couplet_new(l, r); 
    W* wc = w_new(C_MANIFOLD, c);
    RETURN(wc, COMPOSON);
}
<S_PATH,S_SIDE>{grpref} {
    char* s = strdup(yytext + 1);
    W* w = w_new(C_GRPREF, s);
    RETURN(w, COMPOSON);
}
<S_PATH,S_SIDE>{positional} {
    W* w = w_new(C_POSITIONAL, unquote(yytext));
    RETURN(w, COMPOSON);
}
<S_PATH,S_SIDE>{refer} {
    Manifold* m = NULL;
    W* l = label_from_str(unquote(yytext));
    W* r = w_new(P_MANIFOLD, m);
    Couplet* c = couplet_new(l, r); 
    W* wc = w_new(C_REFER, c);
    RETURN(wc, COMPOSON);
}


<S_CPLT,S_ARG,S_SIDE>{selection} {
    W* w = list_from_str(trim_couple(yytext));
    UNPUT_N(2)
    RETURN(w, SELECTION);
}

<S_CPLT,S_PATH>{var} {
    W* w = w_new(P_STRING, strdup(yytext));
    RETURN(w, VARIABLE);
}

<S_CPLT>{str} {
    W* w = w_new(P_STRING, strdup(yytext));
    RETURN(w, STR);
}

<S_ARG>{parameter} {
    W* w = w_new(P_STRING, trim_back(yytext, "="));
    UNPUT_S("= ");
    RETURN(w, NAME);
}
<S_ARG>{int}|{dbl}|{lgc}|{sym}|{str}|{flag} {
    W* w = w_new(P_STRING, strdup(yytext));
    RETURN(w, PRIMITIVE);
}

<S_TYPE,S_ONTOLOGY>{sym}{ws}*:: {
    W* w = w_new(P_STRING, trim_couple(yytext));
    UNPUT_S("::")
    RETURN(w, NAME);
}
<S_TYPE>{type} {
    W* w = w_new(P_STRING, strdup(yytext));
    RETURN(w, TYPE);
}
<S_TYPE>-> { RETURN_TOKEN(ARROW); }

<S_ONTOLOGY>{sym} {
    W* w = w_new(P_STRING, strdup(yytext));
    RETURN(w, OTYPE);
}


. { LOGV("%c", yytext[0]); return yytext[0]; }

%%

#include "bufstack.h"

// Remove the first and last characters of a string
char* unquote(const char* in_str){
    // copy the section of the string between the quotation marks
    int N = strlen(in_str);
    char* s = (char*)calloc((N-1), sizeof(char));
    memcpy(s, in_str+1, (N-2)*sizeof(char));
    s[N-2] = '\0';
    return s;
}

char* trim_back(const char* in_str, const char* c) {
    char* s = strdup(in_str);
    int k = strlen(c);
    while(isspace(s[0])) s++;
    for(int i = strlen(s)-1; i >= 0; i--){
        if(isspace(s[i])){
            s[i] = '\0';
            continue;
        }
        for(int j = 0; j < k; j++){
            if(s[i] == c[j]){
                s[i] = '\0';
                goto keep_going;
            }
        }
        break;
        keep_going:;
    }
    return s;
}

char* trim(const char* in_str) {
    return trim_back(in_str, "");
}

char* trim_couple(const char* in_str) {
    return trim_back(in_str, ":+-=");
}


void print_usage_and_exit(int exit_status){
    fprintf(stderr,
"LOC compiler\n"
"Usage: loc [options] myfile.loc\n"
"Arguments:\n"
"  -c  run the typechecker on manifolds\n"
"  -t  print tokens\n"
"  -h  print this help message and exit\n"
"  -l  suppress normal LIL output\n"
"  -d  recursively dump symbol table\n"
    );
    exit(exit_status);
}

int main(int argc, char ** argv){

    // === flags ====================================================
    bool run_typechecker = false; // type check the manifolds
    bool log_tokens = false;      // print tokens
    bool suppress_lil = false;    // don't print LIL
    bool dump_table = false;      // recursively dump symbol table
    // --------------------------------------------------------------

    // has the loc source file been loaded successfully
    int file_loaded = 0;

    if(argc == 1){
        print_usage_and_exit(1);
    }

    // Process arguments
    //  - only one source file is allowed
    //  - flags may be bundled
    //  - flags and source files can be in any order
    do{
        if(argv[1][0] == '-'){
            while(++argv[1] && argv[1][0] != '\0'){
                switch(argv[1][0]){
                    case 'c':
                        run_typechecker = true; 
                        break;
                    case 't':
                        log_tokens = true;
                        break;
                    case 'l':
                        suppress_lil = true;
                        break;
                    case 'd':
                        dump_table = true;
                        break;
                    case 'h':
                        print_usage_and_exit(0);
                        break;
                    default:
                        fprintf(stderr, "Argument '%c' not supported\n", argv[1][0]); 
                        break;
                }
            }
        } else {
            if(file_loaded){
                fprintf(stderr, "Cannot process multiple files\n");            
                print_usage_and_exit(1);
            } else {
                file_loaded = newfile(argv[1]);
                if(!file_loaded){
                    fprintf(stderr, "Could not open file '%s', skipping\n", argv[1]);
                }
            }
        }
    } while(argc-- > 2 && argv++);

    if(!file_loaded) {
        fprintf(stderr, "No readable loc file found\n");
        print_usage_and_exit(1);
    }
    if(log_tokens){
        toklog = fopen("/dev/stderr", "w");
    } else {
        toklog = fopen("/dev/null", "w");
    }


    // parse the grammar
    int status = yyparse();

    // build manifolds
    //  - resolve group references
    //  - link manifolds to their modifiers
    //  - link manifolds inputs to manifold outputs 
    build_manifolds(global_table);

    // extract LIL as list of lists
    Ws* lil = build_lil(global_table);

    // check the types of all manifolds
    W* msg = type_check(global_table);


    // === print statements =========================================
    if(   dump_table      ) ws_print(global_table, ws_recurse_most);
    if( ! suppress_lil    ) print_lil(lil);
    if(   run_typechecker ) print_error(msg);
    // --------------------------------------------------------------


    fclose(toklog);

    return status;
}
