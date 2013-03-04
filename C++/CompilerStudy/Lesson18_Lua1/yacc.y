%token AND BREAK DO ELSE ELSEIF END FALSE 
%token FOR FUNCTION IF IN LOCAL NIL NOT 
%token OR REPEAT RETURN THEN TRUE UNTIL WHILE

%token OP_ARGS

%token LITERAL1 LITERAL2 LITERAL3
%token NUMBER1 NUMBER2 NUMBER3

%token ID

%left OP_CONCAT

%right OP_ASSIGN

%left OP_EQUAL OP_NEQUAL OP_LEQUAL OP_GEQUAL OP_LESS OP_GREATER
%left OP_PLUS OP_SUB
%left OP_MUL OP_DIV OP_MOD
%left OP_POWER

%nonassoc OP_LEN

%{

%}

%%

Program : LITERAL1;

%%

bool parseFile(const char *fname)
{
    bool succ = false;
    FILE *f = fopen(fname, "r");
    try
    {
        yyrestart(f);
        yyparse();
        succ = true;
    }
    catch(const exception& e) {
        printf("Catch parse exception: %s\n", e.what());
    }
    fclose(f);
    return succ;
}

