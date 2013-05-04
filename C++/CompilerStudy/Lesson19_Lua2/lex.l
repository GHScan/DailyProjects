%{

#define YY_NO_UNISTD_H

#include "Any.h"
#include "AST.h"

struct TerminalSymbol {
    string lexem;
    int line;
    TerminalSymbol(): line(0){}
    TerminalSymbol(const string& _lexem, int _line): lexem(_lexem), line(_line){}
};
#define YYSTYPE Any

static void onTokenFound(bool isAttr);
int isatty(int );
int getTokenLine();

#include "yacc.h"

%}

Number [0-9]+
Number16 0x[0-9a-fA-F]+
Float {Number}?\.{Number}
NumberOrFloat ({Number}|{Float})
EFloat {NumberOrFloat}[eE]\-?{NumberOrFloat}

%%

"--"[^\n]*\n {onTokenFound(false);}

[ \t]+ {onTokenFound(false);}

"+" {onTokenFound(false); return OP_ADD; }
"-" {onTokenFound(false); return OP_SUB; }
"*" {onTokenFound(false); return OP_MUL; }
"/" {onTokenFound(false); return OP_DIV; }
"%" {onTokenFound(false); return OP_MOD; }
"^" {onTokenFound(false); return OP_POWER; }
"#" {onTokenFound(false); return OP_LEN; }
"==" {onTokenFound(false); return OP_EQUAL; }
"~=" {onTokenFound(false); return OP_NEQUAL; }
"<=" {onTokenFound(false); return OP_LEQUAL; }
">=" {onTokenFound(false); return OP_GEQUAL; }
"<" {onTokenFound(false); return OP_LESS; }
">" {onTokenFound(false); return OP_GREATER; }
"=" {onTokenFound(false); return OP_ASSIGN; }
".." {onTokenFound(false); return OP_CONCAT; }
"..." {onTokenFound(false); return OP_ARGS; }

"and" {onTokenFound(false);  return AND;}
"break" {onTokenFound(false);  return BREAK;}
"continue" {onTokenFound(false);  return CONTINUE;}
"do" {onTokenFound(false);  return DO;}
"else" {onTokenFound(false);  return ELSE;}
"elseif" {onTokenFound(false);  return ELSEIF;}
"end" {onTokenFound(false);  return END;}
"false" {onTokenFound(false);  return FALSE;}
"for" {onTokenFound(false);  return FOR;}
"function" {onTokenFound(false);  return FUNCTION;}
"if" {onTokenFound(false);  return IF;}
"in" {onTokenFound(false);  return IN;}
"local" {onTokenFound(false);  return LOCAL;}
"nil" {onTokenFound(false);  return NIL;}
"not" {onTokenFound(false);  return NOT;}
"or" {onTokenFound(false);  return OR;}
"repeat" {onTokenFound(false);  return REPEAT;}
"return" {onTokenFound(false);  return RETURN;}
"then" {onTokenFound(false);  return THEN;}
"true" {onTokenFound(false);  return TRUE;}
"until" {onTokenFound(false);  return UNTIL;}
"while" {onTokenFound(false);  return WHILE;}

\"([^\\\"]|\\.)*\" { onTokenFound(true);return LITERAL1;}
'([^\\']|\\.)*' { onTokenFound(true);return LITERAL2;} 
"[["([^\]]|\][^\]])*"]]" { onTokenFound(true);return LITERAL3;}

{Number16} { onTokenFound(true); return NUMBER1; } 
{NumberOrFloat} { onTokenFound(true); return NUMBER2; }
{EFloat} { onTokenFound(true); return NUMBER3; }

[_a-zA-Z][_a-zA-Z0-9]* { onTokenFound(true); return ID; }

\n+ {
    onTokenFound(false); 
}

. { onTokenFound(false); return yytext[0]; }

%%

static int g_line = 1;
static int g_column = 1;
static int g_parenthesisLevel = 0;
static int g_bracketLevel = 0;
static int g_braceLevel = 0;
static void onTokenFound(bool isAttr) {
    if (isAttr) yylval = TerminalSymbol(string(yytext, yytext + yyleng), g_line);
    g_column += yyleng;
    for (int i = 0; i < (int)yyleng; ++i) { 
        switch (yytext[i]) {
        case '\n':
            ++g_line;
            g_column = 1;
            break;
        case '[': ++g_bracketLevel; break;
        case ']': --g_bracketLevel; break;
        case '(': ++g_parenthesisLevel; break;
        case ')': --g_parenthesisLevel; break;
        case '{': ++g_braceLevel; break;
        case '}': --g_braceLevel; break;
        default: break;
        }
    }
}

int getTokenLine() {
    return g_line;
}

void yyerror(const char *msg) {
    printf("yyerror [%d-%d]: %s\n", g_line, g_column, msg);
}

int yywrap() {
    g_line = 1;
    g_column = 1;
    (void)yyunput;
    return 1;
}

int isatty(int ) {
    return 0;
}
