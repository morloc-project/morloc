DBL [0-9]\.[0-9]+|[1-9][0-9]+\.[0-9]+
INT [0-9]|[1-9][0-9]+
STR \'[^']*\'|\"[^"]*\"

NIL NA
VAL [a-zA-Z_.][a-zA-Z0-9_.]*
EOL \n

DEP -->
ARW ->
EQU =
DEF ::
SEP ,
LAB :

GATE \?
LPAR \(
RPAR \)
LBRC \{
RBRC \}
LBRK \[
RBRK \]
LDGT <<
RDGT >>
LSLT <
RSLT >

%%

#.*|\/\/.* ;

{DBL}  { printf("DBL ");  }
{INT}  { printf("INT ");  }
{STR}  { printf("STR ");  }

{VAL}  { printf("VAL ");  }
{EOL}  { printf("\n");    }

{DEP}  { printf("DEP ");  }
{ARW}  { printf("ARW ");  }
{EQU}  { printf("EQU ");  }
{DEF}  { printf("DEF ");  }
{SEP}  { printf("SEP ");  }
{LAB}  { printf("LAB ");  }

{GATE} { printf("GATE "); }
{LPAR} { printf("LPAR "); }
{RPAR} { printf("RPAR "); }
{LBRC} { printf("LBRC "); }
{RBRC} { printf("RBRC "); }
{LBRK} { printf("LBRK "); }
{RBRK} { printf("RBRK "); }
{LDGT} { printf("LDGT "); }
{RDGT} { printf("RDGT "); }
{LSLT} { printf("LSLT "); }
{RSLT} { printf("RSLT "); }

%%

int main(int argc, char ** argv){
    yylex(); 
    return 0;
}
