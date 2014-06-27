%{

#include "pch.h"
#include "tokenizer.yy.h"

%}

%token VAR FUNC CLASS
%token IF ELIF ELSE FOR RET
%token TRY CATCH THROW DEFER
%token INT FLOAT STRING ID

%token OP_LINE OP_ARROW

%right OP_ASSIGN
%right OP_IADD OP_ISUB OP_IMUL OP_IDIV OP_IMOD OP_IPOW
%left OP_DOT3
%left AND OR IN
%left OP_EQUAL OP_NEQUAL OP_LESS OP_LESSEQUAL OP_GREATER OP_GREATEREQUAL
%left OP_ADD OP_SUB
%left OP_MUL OP_DIV OP_MOD
%left OP_POW

%nonassoc OP_INC OP_DEC NOT

%%

Program
    : Opt_Statement_List
    ;

Block
    : '{' Opt_Statement_List '}'
    ;

Opt_Statement_List
    : Opt_Statement_Sep
    | Opt_Statement_Sep Statement_List Opt_Statement_Sep
    ;

Statement_List
    : Statement
    | Statement_List Statement_Sep Statement
    ;

Opt_Statement_Sep
    :
    | Statement_Sep
    ;

Statement_Sep
    : OP_LINE
    | ';'
    | Statement_Sep OP_LINE
    | Statement_Sep ';'
    ;

Statement
    : Declare_Vars
    | Assignments
    | Inplace_Assignment
    | For
    | If_Statement
    | RValue
    | RET RValue
    ;

Declare_Vars
    : VAR ID_Comma_List
    | VAR Initial_Assignments
    ;

For
    : FOR ID_Comma_List IN For_Source Block
    ;

For_Source
    : RValue OP_DOT3 RValue
    | Primary_Exp
    ;

If_Statement 
    : IF RValue Block Elif_List Else
    ;

Elif_List
    : 
    | Elif_List Elif
    ;

Elif
    : ELIF RValue Block
    ;

Else
    : ELSE Block
    |
    ;

Initial_Assignments
    : ID_Comma_List OP_ASSIGN RValue_Comma_List
    | ID_Func
    | ID_Class
    ;

Assignments
    : LValue_Comma_List OP_ASSIGN RValue_Comma_List
    | ID_Func
    | ID_Class
    ;

Inplace_Assignment
    : LValue Inplace_Ops RValue
    | OP_INC LValue
    | LValue OP_INC
    | OP_DEC LValue
    | LValue OP_DEC
    ;

Inplace_Ops
    : OP_IADD | OP_ISUB | OP_IMUL | OP_IDIV | OP_IMOD | OP_IPOW
    ;

Call
    : Primary_Exp '(' Opt_RValue_Comma_List ')'
    ;

Initializer 
    : '@' '{' Opt_Initializer_Cell_List '}'
    ;

Opt_Initializer_Cell_List
    :
    | Initializer_Cell_List
    ;

Initializer_Cell_List
    : Initializer_Cell
    | Initializer_Cell_List ',' Initializer_Cell
    ;

Initializer_Cell
    : RValue
    | ID OP_ASSIGN RValue
    | '[' RValue ']' OP_ASSIGN RValue
    ;

LValue_Comma_List
    : LValue
    | LValue_Comma_List ',' Opt_Line LValue
    ;

Opt_RValue_Comma_List
    :
    | RValue_Comma_List
    ;

RValue_Comma_List
    : RValue
    | RValue_Comma_List ',' Opt_Line RValue
    ;

Opt_ID_Comma_List
    :
    | ID_Comma_List
    ;

ID_Comma_List
    : ID 
    | ID_Comma_List ',' Opt_Line ID
    ;

ID_Func
    : FUNC ID '(' Opt_ID_Comma_List ')' Block
    ;

Func
    : FUNC '(' Opt_ID_Comma_List ')' Block
    ;

ID_Class
    : CLASS ID Block
    ;

Class
    : CLASS Block
    ;

RValue
    : Binary_Exp
    ;

Binary_Exp
    : Unary_Exp
    | Binary_Exp OP_ADD Opt_Line Binary_Exp
    | Binary_Exp OP_SUB Opt_Line Binary_Exp
    | Binary_Exp OP_MUL Opt_Line Binary_Exp
    | Binary_Exp OP_DIV Opt_Line Binary_Exp
    | Binary_Exp OP_MOD Opt_Line Binary_Exp
    | Binary_Exp OP_POW Opt_Line Binary_Exp
    | Binary_Exp OP_EQUAL Opt_Line Binary_Exp
    | Binary_Exp OP_NEQUAL Opt_Line Binary_Exp
    | Binary_Exp OP_LESS Opt_Line Binary_Exp
    | Binary_Exp OP_LESSEQUAL Opt_Line Binary_Exp
    | Binary_Exp OP_GREATER Opt_Line Binary_Exp
    | Binary_Exp OP_GREATEREQUAL Opt_Line Binary_Exp
    | Binary_Exp AND Opt_Line Binary_Exp
    | Binary_Exp OR Opt_Line Binary_Exp
    | Binary_Exp IN Opt_Line Primary_Exp
    ;

Unary_Exp
    : Primary_Exp
    | NOT Primary_Exp
    ;

Primary_Exp
    : Literal
    | LValue
    | '(' RValue ')'
    | Call
    | Call Initializer
    ;

Literal
    : INT
    | FLOAT
    | STRING
    | Func
    | Class
    ;

LValue
    : ID
    | Call Field_Or_Index
    | '(' RValue ')' Field_Or_Index
    | LValue Field_Or_Index
    ;

Field_Or_Index
    : '.' ID
    | '[' RValue ']'
    ;

Opt_Line
    :
    | OP_LINE
    ;

%%

void parseFile(const char *fname)
{
    FILE *f = fopen(fname, "r");
    ASSERT(f);
    try
    {
        yyrestart(f);
        yyparse();
    } 
    catch (const exception&) {
        fclose(f);
        throw;
    }
}

