%{
#define YY_NO_UNISTD_H

struct SymbolAttribute
{
    string term;
    ExpNodePtr exp;
    StmtNodePtr stmt;
    SymbolAttribute(const char* _term): term(_term){}
    SymbolAttribute(IExpNode *node): exp(node){}
    SymbolAttribute(IStmtNode *node): stmt(node){}
    SymbolAttribute(){}
};
#define YYSTYPE SymbolAttribute
#include "syntax.yy.h"
#include "AST.h"

int isatty(int n);

#ifdef _MSC_VER
#pragma warning(disable : 4267)
#pragma warning(disable : 4244)
#endif

%}

%%

"//".*\n {
    yyset_lineno(yyget_lineno() + 1);
    //printf("comments : %s\n", yytext);
}
"/*"([^\*]|\*[^/])*"*/" {
    int lineN = 0;
    for (int i = 0; i < yyleng; ++i) lineN += yytext[i] == '\n';
    yyset_lineno(yyget_lineno() + lineN);

    //printf("comments : %s\n", yytext);
}

[ \t\n] {
    if (yytext[0] == '\n') {
        yyset_lineno(yyget_lineno() + 1);
    }
}

"if" {
    return IF;
}
"else" {
    return ELSE;
}
"for" {
    return FOR;
}
"while" {
    return WHILE;
}
"do" { 
    return DO;
}
"break" {
    return BREAK;
}
"continue" {
    return CONTINUE;
}
"return" {
    return RETURN;
}
"local" {
    return LOCAL;
}
"function" {
    return FUNCTION;
}

\'[^\']*\' {
    string s(yytext + 1, yytext + yyleng - 1);
    yylval.term.clear();
    for (int i = 0; i < (int)s.size(); ++i) {
        if (s[i] == '\\') {
            switch (s[i + 1]) {
                case 't':
                    ++i;
                    yylval.term.push_back('\t');
                    continue;
                case 'n':
                    ++i;
                    yylval.term.push_back('\n');
                    continue;
                default:
                    break;
            }
        }
        yylval.term.push_back(s[i]);
    }
    return STRING;
}
[0-9]+ {
    yylval = yytext;
    return INT;
}
[_a-zA-Z][_a-zA-Z0-9]* {
    yylval = yytext;
    return ID;
}

[\+\-\*\/\%]?\= {
    yylval = yytext;
    return ASSIGN_OP;
}
"&&"|"||" {
    yylval = yytext;
    return AND_OP;
}
\<|\>|"!="|"=="|"<="|">=" {
    yylval = yytext;
    return CMP_OP;
}
[\+\-] {
    yylval = yytext;
    return ADD_OP;
}
[\*\/\%] {
    yylval = yytext;
    return MUL_OP;
}
"++"|"--" {
    yylval = yytext;
    return INC_OP;
}
\! {
    yylval = yytext;
    return UNARY_OP;
}

. {
    return yytext[0];
}

%%

int isatty(int n)
{
    return 0;
}
int yywrap()
{
    return 1;
}
void yyerror(const char *msg)
{
    printf("sytnax error %d : %s\n", yyget_lineno(), msg);
}
