grammar CMinus;

options {
    language = Cpp;
    //backtrack = true;
    k = 2;
}

@lexer::header {
#include "pch.h"
}
@lexer::traits {
    class CMinusLexer; 
    class CMinusParser; 
    typedef antlr3::Traits< CMinusLexer, CMinusParser > CMinusLexerTraits;
    typedef CMinusLexerTraits CMinusParserTraits;
}

@parser::header {
#include "pch.h"
#include "CMinusLexer.hpp"
#include "AST.h"
#include "SourceFileProto.h"
}
@parser::members {

static string unEscape(const string& s) {
    string r;
    for (int i = 0; i < (int)s.size(); ++i) {
        if (s[i] == '\\') {
            switch (s[i + 1]) {
            case 'a': r.push_back('\a'); break;
            case 'n': r.push_back('\n'); break;
            case 'r': r.push_back('\r'); break;
            case 't': r.push_back('\t'); break;
            default: r.push_back(s[i + 1]); break;
            }
            ++i;
        } else {
            r.push_back(s[i]);
        }
    }
    return r;
}

}

// parser

program returns[SourceFileProtoPtr value = SourceFileProtoPtr(new SourceFileProto())]
    : top_level_declare[value.get()]*
    ;

top_level_declare [SourceFileProto *proto]
options{k=3;}
    : extern_function_declare [proto]
    | function_define [proto]
    | global_variable_define [proto]
    ;

global_variable_define [SourceFileProto *proto]
    : type id_comma_list ';' {
        for (auto name : $id_comma_list.values) {
            proto->globalVars.push_back(StmtNodePtr(new StmtNode_DefineVarable($type.text, name)));
        }
    }
    ;

extern_function_declare [SourceFileProto *proto]
scope {
    FunctionProtoPtr func;
}
@init {
    $extern_function_declare::func.reset(new FunctionProto());
}
    : 'extern' function_head [$extern_function_declare::func.get()] ';' {
        proto->externFuncs.push_back($extern_function_declare::func);
    }
    ;

function_head [FunctionProto *proto]
options {k=4;}
    : type ID '(' (varArgs='...')? ')' {
        proto->retType = $type.text;
        proto->name = $ID.text;
        proto->isVarArgs = $varArgs != NULL;
    }
    | type ID '(' type_id_comma_list (',' varArgs='...')? ')' {
        proto->retType = $type.text;
        proto->name = $ID.text;
        proto->argsTypeID = $type_id_comma_list.values;
        proto->isVarArgs = $varArgs != NULL;
    }
    ;

function_define [SourceFileProto *proto]
scope {
    FunctionProtoPtr func;
    StmtNodePtr block;
}
@init {
    $function_define::func.reset(new FunctionProto());
    $function_define::block.reset(new StmtNode_Block());
}
    : function_head[$function_define::func.get()] '{'
    (statement {
     if ($statement.value != NULL)
     static_cast<StmtNode_Block*>($function_define::block.get())->stmts.push_back($statement.value);
     })* '}' {
        proto->funcs.push_back($function_define::func);
        $function_define::func->body = $function_define::block;
    }
    ;

statement returns[StmtNodePtr value]
    : ';'

    | expression ';' { value.reset(new StmtNode_Expr($expression.value)); }

    | {
        value.reset(new StmtNode_Block());
    } '{' (body=statement{
            if ($body.value != NULL)
            static_cast<StmtNode_Block*>(value.get())->stmts.push_back($body.value);
            })* '}' 

    | type variable_init_comma_list ';' {
        auto stmts = new StmtNode_Stmts();
        for (auto idExpr : $variable_init_comma_list.values) {
            stmts->stmts.push_back(StmtNodePtr(new StmtNode_DefineVarable($type.text, idExpr.first)));
        }
        for (auto idExpr : $variable_init_comma_list.values) {
            stmts->stmts.push_back(StmtNodePtr(new
                        StmtNode_Expr(ExprNodePtr(new ExprNode_Assignment(idExpr.first, idExpr.second)))));
        }
        value.reset(stmts);
    }

    | 'continue' ';' { value.reset(new StmtNode_Continue()); }

    | 'break' ';' { value.reset(new StmtNode_Break()); }

    | 'return' expression? ';' { value.reset(new StmtNode_Return($expression.value)); }

    | 'if' '(' expression ')' thenStmt=statement (('else')=>'else' elseStmt=statement)?  {
        value.reset(new StmtNode_IfThenElse($expression.value, $thenStmt.value, $elseStmt.value));
    }

    | 'for' '(' for_first ';' (secondExpr=expression)? ';' (thirdExpr=expression)? ')' body=statement {
        value = $for_first.value;
        auto block = static_cast<StmtNode_Block*>(value.get());
        block->stmts.push_back(StmtNodePtr(new StmtNode_For(StmtNodePtr(), $secondExpr.value, $thirdExpr.value, $body.value)));
    }

    |  'while' '(' expression ')' body=statement {
        value.reset(new StmtNode_For(StmtNodePtr(), $expression.value, ExprNodePtr(), $body.value));
    }

    ;

for_first returns[StmtNodePtr value]
    : type variable_init_comma_list {
        auto block = new StmtNode_Block();
        for (auto idExpr : $variable_init_comma_list.values) {
            block->stmts.push_back(StmtNodePtr(new StmtNode_DefineVarable($type.text, idExpr.first)));
        }
        for (auto idExpr : $variable_init_comma_list.values) {
            block->stmts.push_back(StmtNodePtr(new
                        StmtNode_Expr(ExprNodePtr(new ExprNode_Assignment(idExpr.first, idExpr.second)))));
        }
        value.reset(block);
    }
    | expression {
        auto block = new StmtNode_Block();
        block->stmts.push_back(StmtNodePtr(new StmtNode_Expr($expression.value)));
        value.reset(block);
    }
    | { value.reset(new StmtNode_Block());}
    ;

id_comma_list returns[vector<string> values]
    : a=ID {
        values.push_back($a.text);
    } (',' b=ID {
            values.push_back($b.text);
            })*
    ;

variable_init_comma_list returns[vector<pair<string, ExprNodePtr> > values]
    : a=variable_init {
        values.push_back($a.value);
    } (',' b=variable_init {
        values.push_back($b.value);
            })*
    ;

variable_init returns[pair<string, ExprNodePtr> value]
    : ID { value.first = $ID.text; }
    | ID '=' assign_expression {
        value.first = $ID.text;
        value.second = $assign_expression.value;
    }
    ;

type_id_comma_list returns[vector<pair<string, string> > values]
    : a=type_id {
        values.push_back($a.value);
    } (',' b=type_id {
            values.push_back($b.value);
            })*
    ;

type_id returns[pair<string, string> value]
    : type ID {
        value.first = $type.text;
        value.second = $ID.text;
    }
    ;

type 
    : 'int' | 'void' | 'float' |'char*'
    ;

expression returns[ExprNodePtr value]
    : assign_expression { value = $assign_expression.value; }
    ;

assign_expression returns[ExprNodePtr value]
    : ID '=' right=assign_expression { value.reset(new ExprNode_Assignment($ID.text, $right.value)); }
    | ID COMBINE_ASSIGN_OP right=assign_expression {
        string op = $COMBINE_ASSIGN_OP.text;
        op = op.substr(0, op.size() - 1);
        value.reset(new ExprNode_Assignment($ID.text, 
                    ExprNodePtr(new ExprNode_BinaryOp(op, ExprNodePtr(new ExprNode_Variable($ID.text)), $right.value))));
    }
    | logic_expression { value = $logic_expression.value; }
    ;

logic_expression returns[ExprNodePtr value]
    : a=relat_expression { value = $a.value; } (LOGIC_OP b=relat_expression{
            value = ExprNodePtr(new ExprNode_BinaryOp($LOGIC_OP.text, value, $b.value));
            })*
    ;

relat_expression returns[ExprNodePtr value]
    : a=add_expression {value = $a.value;} (RELAT_OP b=add_expression {
            value = ExprNodePtr(new ExprNode_BinaryOp($RELAT_OP.text, value, $b.value));
            })*
    ;

add_expression returns[ExprNodePtr value]
    : a=mul_expression {value = $a.value;} (op=('+' | '-') b=mul_expression {
            value = ExprNodePtr(new ExprNode_BinaryOp($op.text, value, $b.value));
            })*
    ;

mul_expression returns[ExprNodePtr value]
    : a=unary_expression { value = $a.value;} (op=('*' | '/' | '%') b=unary_expression {
            value = ExprNodePtr(new ExprNode_BinaryOp($op.text, value, $b.value));
            })*
    ;

unary_expression returns[ExprNodePtr value]
    : (op=('-' | '!'))? primary_expression  {
        if ($op == NULL) value = $primary_expression.value;
        else {
            value = ExprNodePtr(new ExprNode_UnaryOp($op.text, $primary_expression.value));
        }
    }
    ;

primary_expression returns[ExprNodePtr value]
    : ID { value.reset(new ExprNode_Variable($ID.text)); }
    | STRING_LITERAL { value.reset(new ExprNode_StringLiteral(unEscape($STRING_LITERAL.text))); }
    | INT_LITERAL { 
        int i;
        sscanf($INT_LITERAL.text.c_str(), "\%d", &i);
        value.reset(new ExprNode_IntLiteral(i)); 
    }
    | FLOAT_LITERAL {
        float f;
        sscanf($FLOAT_LITERAL.text.c_str(), "\%f", &f);
        value.reset(new ExprNode_IntLiteral(f)); 
    }
    | 'true' { value.reset(new ExprNode_IntLiteral(1)); }
    | 'false' {  value.reset(new ExprNode_IntLiteral(0)); }
    | call_expression { value = $call_expression.value; }
    | '(' expression ')' { value = $expression.value;}
    | op=('++' | '--') ID {
        value.reset(new ExprNode_Assignment($ID.text, 
                    ExprNodePtr(new ExprNode_BinaryOp($op.text.substr(0, 1),
                            ExprNodePtr(new ExprNode_Variable($ID.text)), 
                            ExprNodePtr(new ExprNode_IntLiteral(1))))));
    }
    | '(' type ')' expr=primary_expression { value.reset(new ExprNode_TypeCast($type.text, $expr.value)); }
    ;

call_expression returns[ExprNodePtr value]
    : ID '(' expression_comma_list? ')' {
        value.reset(new ExprNode_Call($ID.text, $expression_comma_list.values));
    }
    ;

expression_comma_list returns[vector<ExprNodePtr> values]
    : a=expression {
        values.push_back($a.value);
    } (',' b=expression {
        values.push_back($b.value);
            })*
    ;

// lexer
COMBINE_ASSIGN_OP : '+=' | '-=' | '*=' | '/=' | '%=';
RELAT_OP : '==' | '!=' | '<' | '<=' | '>' | '>=';
LOGIC_OP : '&&' | '||';

ID : ID_HEAD_LETTER ID_BODY_LETTER*;
STRING_LITERAL : '"' (~('"' | '\\') | '\\' .)* '"';
INT_LITERAL : DIGIT+;
FLOAT_LITERAL : DIGIT* '.' DIGIT*;

COMMENT : '/*' (options{greedy=false;}:.)* '*/' {skip();};
LINE_COMMENT : '//' ~('\r'|'\n')* '\r'?'\n' {skip();};
WS : (' ' | '\t' | '\r'?'\n')+ {skip();};

fragment ID_HEAD_LETTER : ALPHA | '_';
fragment ID_BODY_LETTER : ID_HEAD_LETTER | DIGIT;
fragment ALPHA : 'a'..'z' | 'A'..'Z';
fragment DIGIT : '0' ..'9';
