
#ifndef LEXGLOBAL_H
#define LEXGLOBAL_H

#ifndef YYSTYPE
#define YYSTYPE int
#endif 

extern const char *yytext;
extern int yyleng;
extern YYSTYPE yylval;
extern int yyget_lineno();
extern void yyset_lineno(int l);

extern YYSTYPE& LRProductionHead();
extern YYSTYPE* LRProductionBody();

#endif
