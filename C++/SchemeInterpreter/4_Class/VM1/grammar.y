%{

#include "pch.h"
#include "tokenizer.yy.h"
#include "StackAllocator.h"

static StackAllocator *g_allocator;
static SExpression g_result;

%}

%token STRING INT FLOAT SYMBOL

%%


Program
    : SExpression {
        g_result = $1;
    }
    ;

SExpression
    : INT
    | FLOAT
    | STRING
    | SYMBOL
    | List
    ;

List
    : '(' Opt_SExpression_List ')' {
        $$ = $2;
    }
    ;

Opt_SExpression_List
    : {
        $$ = SExpression::createList(nullptr);
    }
    | SExpression_List
    ;

SExpression_List
    : SExpression {
        auto node = g_allocator->malloc<SExpressionListNode>();
        node->value = $1;
        node->next = nullptr;
        $$ = SExpression::createList(node);
    }
    | SExpression SExpression_List {
        auto node = g_allocator->malloc<SExpressionListNode>();
        node->value = $1;
        node->next = $2.getNode();
        $$ = SExpression::createList(node);
    }
    ;

%%

SExpression parseFile(const char *fname, StackAllocator *allocator) {
    g_allocator = allocator;

    FILE *f = fopen(fname, "r");
    ASSERT(f);

    yyrestart(f);
    yyparse();

    fclose(f);

    g_allocator = nullptr;

    return g_result;
}

