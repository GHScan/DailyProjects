%{
#define YY_NO_UNISTD_H

#include "Token.h"
#include "AST.h"
#include "Atom.h"

#ifndef _MSC_VER
#include <unistd.h>
#endif

union YYSType {
    Token token;
    IASTNode *node;

    YYSType(): node(nullptr) {
    }

    explicit YYSType(IASTNode *_node): node(_node) {
    }
};

#define YYSTYPE YYSType
#include "parser.yy.hpp"

extern int isatty(int n);
static void onFoundToken();

%}

Int [0-9]+
Int16 0x[0-9a-fA-F]+
Float {Int}?\.{Int}
IntOrFloat ({Int}|{Float})
EFloat {IntOrFloat}[eE]\-?{IntOrFloat}

%%

"//".*\n { onFoundToken(); }
"/*"([^\*]|\*[^/])*"*/" { onFoundToken(); }

[ \t]+ {onFoundToken();}

\n+ { onFoundToken(); return OP_LINE; }

"=>" { onFoundToken(); return OP_ARROW; }

"var" { onFoundToken(); return VAR; }
"func" { onFoundToken(); return FUNC; }
"class" { onFoundToken(); return CLASS; }

"if" { onFoundToken(); return IF; }
"elif" { onFoundToken(); return ELIF; }
"else" { onFoundToken(); return ELSE; }
"for" { onFoundToken(); return FOR; }
"ret" { onFoundToken(); return RET; }

"try" { onFoundToken(); return TRY; }
"throw" { onFoundToken(); return THROW; }
"catch" { onFoundToken(); return CATCH; }
"defer" { onFoundToken(); return DEFER; }

"=" { onFoundToken(); return OP_ASSIGN; }
"in" { onFoundToken(); return IN; }
"and" { onFoundToken(); return AND; }
"or" { onFoundToken(); return OR; }
"==" { onFoundToken(); return OP_EQUAL; }
"!=" { onFoundToken(); return OP_NEQUAL; }
"<" { onFoundToken(); return OP_LESS; }
"<=" { onFoundToken(); return OP_LESSEQUAL; }
">" { onFoundToken(); return OP_GREATER; }
">=" { onFoundToken(); return OP_GREATEREQUAL; }
"+" { onFoundToken(); return OP_ADD; }
"-" { onFoundToken(); return OP_SUB; }
"*" { onFoundToken(); return OP_MUL; }
"/" { onFoundToken(); return OP_DIV; }
"%" { onFoundToken(); return OP_MOD; }
"**" { onFoundToken(); return OP_POW; }
"+=" { onFoundToken(); return OP_IADD; }
"-=" { onFoundToken(); return OP_ISUB; }
"*=" { onFoundToken(); return OP_IMUL; }
"/=" { onFoundToken(); return OP_IDIV; }
"%=" { onFoundToken(); return OP_IMOD; }
"**=" { onFoundToken(); return OP_IPOW; }

"++" { onFoundToken(); return OP_INC; }
"--" { onFoundToken(); return OP_DEC; }
"not" { onFoundToken(); return NOT; }

"..." { onFoundToken(); return OP_DOT3; }


'([^\\']|\\.)*' { onFoundToken(); return STRING;}

{Int16} { onFoundToken(); return INT; }
{Int} { onFoundToken(); return INT; }
{Float} { onFoundToken(); return FLOAT; }
{EFloat} { onFoundToken(); return FLOAT; }

[_a-zA-Z][_a-zA-Z0-9]* { onFoundToken(); return ID; }

. { onFoundToken(); return yytext[0];}

%%

static int gLine = 1;
static int gColumn = 1;

static void onFoundToken() {
    yylval.token = Token{AtomPool::instance()->intern(yytext), gLine, gColumn};

    int len = strlen(yytext);
    for (int i = 0; i < len; ++i) {
        if (yytext[i] == '\n') {
            ++gLine;
            gColumn = 1;
        }
    }
}

#ifdef _MSC_VER
extern int isatty(int n) {
    return 0;
}
#endif

extern int yywrap() {
    gLine = 1;
    gColumn = 1;
    (void)yyunput;
    return 1;
}

extern void yyerror(const char *msg) {
    printf("syntax error (%d,%d) : %s\n", gLine, gColumn, msg);
}
