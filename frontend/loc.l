%{
#include <stdio.h>
#include <ctype.h>

#include "loc.tab.h"

int newfile(char *fn);
int popfile(void);

FILE* toklog;

char* current_lang = NULL;

#define LOG(x) fprintf(toklog, x); fflush(toklog);
#define LOGV(x,a) fprintf(toklog, x, a); fflush(toklog);

#define RETURN(x, t) \
    LOG(#t " ");     \
    yylval.t = x;      \
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
W* couplet_str_str(char* lhs, char* rhs, Class cls);
// After trimming, it is often necessary to put characters back into the buffer:
// unput a specific string
#define UNPUT_S(x) for(int i = strlen(x)-1; i >= 0; i--){ unput(x[i]); }
// unput k characters
#define UNPUT_N(k) \
    char* zs = strdup(yytext); \
    int zn = strlen(yytext); \
    for(int zi = 0; zi < k; zi++){ unput(zs[zn - zi - 1]); } \
    free(zs);

#define RETURN_SECTION(s)                                       \
    LOG(#s " ");                                                \
    char* name = (char*)calloc(16, sizeof(char));               \
    char* lang = (char*)calloc(16, sizeof(char));               \
    int n = sscanf(yytext, "@%s %s", name, lang);               \
    lang = (n == 2) ? lang : NULL;                              \
    Section* sec = section_new(name, lang);                     \
    Couplet* c = couplet_new(w_new(P_SECTION, sec), NULL, '='); \
    yylval.s = w_new(T_SECTION, c);                             \
    current_lang = lang;                                        \
    return s;

%}

%option header-file="lex.yy.h"
%option noyywrap noinput pointer yylineno

%s S_TYPE
%s S_ONTOLOGY
%s S_INCLUDE
%s S_EXPORT
%x X_COMMENT
%s X_SOURCE
%s S_PATH
%s S_SIDE
%s S_CPLT
%s S_ARG

ws        [ \t\n\r]
comment   #.*\n

sym       [a-zA-Z_][a-zA-Z0-9_.-]*
var       {sym}(:[a-zA-Z0-9_]+)?
path      {var}(\/{var})*
selection ({path}({ws}*,{ws}*{path})*|\*){ws}*:[:=+-]

type {sym}|\*|\?
list {sym}({ws}*,{ws}*{sym})*

identifier {var}{ws}*:[:=+-]

parameter [a-zA-Z0-9_.-]+{ws}*=
flag [-][a-zA-Z0-9_.-]+

str \'[^']*\'|\"[^"]*\"
bak `[^`]*`
int -?([0-9]|[1-9][0-9]+)
dbl -?{int}\.[0-9]+
lgc TRUE|FALSE 

file {sym}(\/{sym})*

grpref     \*{var}
argref     ${int}
refer      <{var}>

couple ::|:=|:-|:\+

lang ([ ]+[^ \t\n\r]+)?

%%


{comment} { }
^{ws}*----+{ws}*$ { }
; { }

@comment {  BEGIN(X_COMMENT); }
@include {  BEGIN(S_INCLUDE); }

@path{lang}     { BEGIN(S_PATH)     ; RETURN_SECTION ( SECTION_PATH     ) ; }
@check{lang}    { BEGIN(S_SIDE)     ; RETURN_SECTION ( SECTION_CHECK    ) ; }
@fail{lang}     { BEGIN(S_SIDE)     ; RETURN_SECTION ( SECTION_FAIL     ) ; }

@[0-9]{lang}    { BEGIN(S_SIDE)     ; RETURN_SECTION ( SECTION_HOOK     ) ; } 
@before{lang}   { BEGIN(S_SIDE)     ; RETURN_SECTION ( SECTION_HOOK     ) ; }
@after{lang}    { BEGIN(S_SIDE)     ; RETURN_SECTION ( SECTION_HOOK     ) ; }

@source{lang}   { BEGIN(X_SOURCE)   ; RETURN_SECTION ( SECTION_SOURCE   ) ; }
@type{lang}     { BEGIN(S_TYPE)     ; RETURN_SECTION ( SECTION_TYPE     ) ; }
@ontology{lang} { BEGIN(S_ONTOLOGY) ; RETURN_SECTION ( SECTION_ONTOLOGY ) ; }
@cache{lang}    { BEGIN(S_CPLT)     ; RETURN_SECTION ( SECTION_CACHE    ) ; }
@alias{lang}    { BEGIN(S_CPLT)     ; RETURN_SECTION ( SECTION_ALIAS    ) ; }
@lang{lang}     { BEGIN(S_CPLT)     ; RETURN_SECTION ( SECTION_LANG     ) ; }
@doc{lang}      { BEGIN(S_CPLT)     ; RETURN_SECTION ( SECTION_DOC      ) ; }
@arg{lang}      { BEGIN(S_ARG)      ; RETURN_SECTION ( SECTION_ARG      ) ; }
@export{lang}   { BEGIN(S_EXPORT)   ; RETURN_SECTION ( SECTION_EXPORT   ) ; }

<X_SOURCE>^[ \t]*([^\n@].*)?\n {
    char* s = strdup(yytext);
    W* w = w_new(P_STRING, s);
    RETURN(w, STR);
}
<X_SOURCE>@ { unput('@'); BEGIN INITIAL; } /* allow @ anywhere except ^ */

{ws}  { LOGV("%s", yytext); }
RESET { RETURN_TOKEN(RESET); } 

<X_COMMENT>@      { unput('@'); BEGIN INITIAL; }
<X_COMMENT>(.|\n) { /* toss bodies */ }
<X_COMMENT>\\@    { /* allow escaped @ */ }

<S_EXPORT>as { RETURN_TOKEN(AS); }
<S_EXPORT>^{ws}*{path} {
    W* w = path_from_str(trim_couple(yytext), current_lang);
    RETURN(w, PATH);
}

<S_INCLUDE>{file} {
    char* s = trim(yytext);
    char* filename = (char*)malloc((strlen(s) + 5) * sizeof(char));
    sprintf(filename, "%s.loc", s);
    LOGV("including(%s)\n", filename);
    if(!newfile(filename)) yyterminate();
  }
<<EOF>> { if(!popfile()) yyterminate(); UNPUT_S("\n@include\n"); }

<S_PATH,S_SIDE,S_CPLT,S_ARG,S_TYPE,S_ONTOLOGY>{couple} {
    char op = yytext[1];
    op = op == ':' ? '=' : op;
    Couplet* c = couplet_new(NULL, NULL, op);
    W* w = w_new(P_COUPLET, c);
    RETURN(w, COUPLE);
}

<S_PATH>{identifier} {
    W* w = label_from_str(trim_couple(yytext), current_lang);
    UNPUT_N(2)
    RETURN(w, IDENTIFIER);
}

<S_PATH,S_SIDE>{bak} {
    W* w = couplet_str_str("*", unquote(yytext), C_POSITIONAL);
    RETURN(w, COMPOSON);
}
<S_PATH,S_SIDE>{int} {
    W* w = couplet_str_str("Int", strdup(yytext), C_POSITIONAL);
    RETURN(w, COMPOSON);
}
<S_PATH,S_SIDE>{dbl} {
    W* w = couplet_str_str("Num", strdup(yytext), C_POSITIONAL);
    RETURN(w, COMPOSON);
}

<S_PATH,S_SIDE>{lgc} {
    char* s = (strcmp(yytext, "TRUE") == 0) ? "true" : "false";
    W* w = couplet_str_str("Bool", strdup(s), C_POSITIONAL);
    RETURN(w, COMPOSON);
}

<S_PATH,S_SIDE>{str} {
    W* w = couplet_str_str("String", strdup(yytext), C_POSITIONAL);
    RETURN(w, COMPOSON);
}

<S_PATH,S_SIDE>{var} {
    Manifold* m = manifold_new();
    W* l = label_from_str(yytext, current_lang);
    W* r = w_new(P_MANIFOLD, m);
    Couplet* c = couplet_new(l, r, '.'); 
    W* wc = w_new(C_MANIFOLD, c);
    RETURN(wc, COMPOSON);
}
<S_PATH,S_SIDE>{grpref} {
    char* s = strdup(yytext + 1);
    W* w = w_new(C_GRPREF, s);
    RETURN(w, COMPOSON);
}
<S_PATH,S_SIDE>{argref} {
    char* s = strdup(yytext + 1);
    W* w = w_new(C_ARGREF, s);
    RETURN(w, COMPOSON);
}

<S_PATH,S_SIDE>{refer} {
    Manifold* m = NULL;
    W* l = label_from_str(unquote(yytext), current_lang);
    W* r = w_new(P_MANIFOLD, m);
    Couplet* c = couplet_new(l, r, '.'); 
    W* wc = w_new(C_REFER, c);
    RETURN(wc, COMPOSON);
}


<S_CPLT,S_ARG,S_SIDE>{selection} {
    W* w = list_from_str(trim_couple(yytext), current_lang);
    UNPUT_N(2)
    RETURN(w, SELECTION);
}

<S_CPLT,S_PATH,S_EXPORT>{var} {
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
<S_ARG>{bak} {
    W* w = couplet_str_str("*", unquote(yytext), C_POSITIONAL);
    // TODO - this isn't primitive
    RETURN(w, PRIMITIVE);
}

<S_TYPE,S_ONTOLOGY>{list}{ws}*:: {
    W* w = list_from_str(trim_couple(yytext), current_lang);
    UNPUT_N(2)
    RETURN(w, NAMES);
}
<S_TYPE>{type} {
    W* w = NULL;

    // Types are considered to be generic if they begin with a lowercase letter
    // AND have a length of 1. The inferred type, on the lhs of the couplet,
    // defaults to the untyped FT_ATOMIC<'*'>. The explicit type may be
    // inferred later. 
    if(islower(yytext[0]) && strlen(yytext) == 1){
        W* lhs = w_new(P_STRING, strdup(yytext));
        W* rhs = w_new(FT_ATOMIC, "*");
        Couplet* c = couplet_new(lhs, rhs, '=');
        w = w_new(FT_GENERIC, c);
    // If the type symbol is not lower case, it is considered a primitive
    // type, e.g. Int, Bool, or Table.
    } else {
        w = w_new(FT_ATOMIC, strdup(yytext));
    }
    RETURN(w, TYPE);
}
<S_TYPE>-> { RETURN_TOKEN(ARROW); }

<S_ONTOLOGY>{sym} {
    W* w = w_new(P_STRING, strdup(yytext));
    RETURN(w, TYPE);
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

W* couplet_str_str(char* lhs, char* rhs, Class cls){
    Couplet* c = couplet_new(w_new(P_STRING, lhs), w_new(P_STRING, rhs), '=');
    W* w = w_new(cls, c);
    return w;
}

void print_usage_and_exit(int exit_status){
    fprintf(stderr,
"LOC compiler\n"
"Usage: loc [options] myfile.loc\n"
"Arguments:\n"
"  -c  run the typechecker on manifolds\n"
"  -z  print type debugging info"
"  -t  print tokens\n"
"  -h  print this help message and exit\n"
"  -l  suppress normal LIL output\n"
"  -d  recursively dump symbol table\n"
    );
    exit(exit_status);
}

int main(int argc, char ** argv){

    // === flags ====================================================
    bool run_typechecker = false;  // type check the manifolds
    bool print_type_debug = false; // print type debugging info
    bool log_tokens = false;       // print tokens
    bool suppress_lil = false;     // don't print LIL
    bool dump_table = false;       // recursively dump symbol table
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
                    case 'z':
                        print_type_debug = true;
                        break;
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
                        warn("Argument '%c' not supported\n", argv[1][0]); 
                        break;
                }
            }
        } else {
            if(file_loaded){
                warn("Cannot process multiple files\n");            
                print_usage_and_exit(1);
            } else {
                file_loaded = newfile(argv[1]);
                if(!file_loaded){
                    warn("Could not open file '%s', skipping\n", argv[1]);
                }
            }
        }
    } while(argc-- > 2 && argv++);

    if(!file_loaded) {
        warn("No readable loc file found\n");
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
    build_manifolds(global_table, print_type_debug);

    // extract LIL as list of lists
    Ws* lil = build_lil(global_table);

    // === print statements =========================================
    if(   dump_table      ) ws_print(global_table, ws_recurse_most); 
    if( ! suppress_lil    ) print_lil(lil);
    // --------------------------------------------------------------


    fclose(toklog);

    return status;
}
