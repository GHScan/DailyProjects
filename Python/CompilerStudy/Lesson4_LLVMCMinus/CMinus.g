grammar CMinus;

options {
    language = Python;
    //backtrack = true;
    k = 2;
}

@parser::header {
from ast import *
from sourceMeta import *
}

// parser

program returns[meta = SourceMeta()]
    : top_level_declare[meta]*
    ;

top_level_declare [meta]
options{k=3;}
    : extern_function_declare [meta]
    | function_define [meta]
    | global_variable_define [meta]
    ;

global_variable_define [meta]
    : type id_comma_list ';' {
        for name in $id_comma_list.values:
            meta.globalVars.append(($type.text, name))
    }
    ;

extern_function_declare [meta]
    : 'extern' function_head ';' {
        meta.externFuncs.append($function_head.value)
    }
    ;

function_head returns[value]
options {k=4;}
    : type ID '(' (isVarArg='...')? ')' {
        value = FunctionMeta($type.text, $ID.text, [], $isVarArg != None, None)
    }
    | type ID '(' type_id_comma_list (',' isVarArg='...')? ')' {
        value = FunctionMeta($type.text, $ID.text, $type_id_comma_list.values, $isVarArg != None, None)
    }
    ;

function_define [meta]
scope {
    block
}
@init {
    block = StmtNodeBlock([])
}
    : function_head '{'
    (statement {
     if $statement.value != None: block.stmts.append($statement.value)
     })* '}' {
        funcMeta = $function_head.value
        funcMeta.bodyStmt = $function_define::block
        meta.funcs.append(funcMeta)
    }
    ;

statement returns[value]
    : ';'

    | expression ';' { value = StmtNodeExpr($expression.value) }

    | {
        value = StmtNodeBlock([])
    } '{' (body=statement{
            if $body.value != None: value.stmts.append($body.value)
            })* '}' 

    | type variable_init_comma_list ';' {
        value = StmtNodeStmts([])
        for name, initExpr in $variable_init_comma_list.values:
            value.stmts.append(StmtNodeDefineVariable($type.text, name))
        for name, initExpr in $variable_init_comma_list.values:
            value.stmts.append(StmtNodeExpr(ExprNodeAssignment(name, initExpr)))
    }

    | 'continue' ';' { value = StmtNodeContinue() }

    | 'break' ';' { value = StmtNodeBreak() }

    | 'return' expression? ';' { value = StmtNodeReturn($expression.value) }

    | 'if' '(' expression ')' thenStmt=statement (('else')=>'else' elseStmt=statement)?  {
        value = StmtNodeIfThenElse($expression.value, $thenStmt.value, $elseStmt.value)
    }

    | 'for' '(' for_first ';' (secondExpr=expression)? ';' (thirdExpr=expression)? ')' body=statement {
        value = $for_first.value;
        value.stmts.append(StmtNodeFor(None, $secondExpr.value, $thirdExpr.value, $body.value))
    }

    |  'while' '(' expression ')' body=statement {
        value = StmtNodeFor(None, $expression.value, None, $body.value)
    }

    ;

for_first returns[StmtNodePtr value]
    : type variable_init_comma_list {
        value = StmtNodeBlock([])
        for name, initExpr in $variable_init_comma_list.values:
            value.stmts.append(StmtNodeDefineVariable($type.text, name))
        for name, initExpr in $variable_init_comma_list.values:
            value.stmts.append(StmtNodeExpr(ExprNodeAssignment(name, initExpr)))
    }
    | expression {
        value = StmtNodeBlock([])
        value.stmts.append(StmtNodeExpr($expression.value))
    }
    | { value = StmtNodeBlock([])}
    ;

id_comma_list returns[values]
    : a=ID { values = [$a.text] } 
        (',' b=ID { values.append($b.text) })*
    ;

variable_init_comma_list returns[values]
    : a=variable_init { values = [$a.value] } 
        (',' b=variable_init { values.append($b.value) })*
    ;

variable_init returns[value]
    : ID { value = ($ID.text, None) }
    | ID '=' assign_expression { value = ($ID.text, $assign_expression.value) }
    ;

type_id_comma_list returns[values]
    : a=type_id { values = [$a.value] } 
        (',' b=type_id { values.append($b.value) })*
    ;

type_id returns[ value]
    : type ID { value = ($type.text, $ID.text) }
    ;

type 
    : 'int' | 'void' | 'float' | 'char*'
    ;

expression returns[value]
    : assign_expression { value = $assign_expression.value }
    ;

assign_expression returns[value]
    : ID '=' right=assign_expression { value = ExprNodeAssignment($ID.text, $right.value) }
    | ID COMBINE_ASSIGN_OP right=assign_expression {
        value = ExprNodeAssignment($ID.text, 
                ExprNodeBinaryOp($COMBINE_ASSIGN_OP.text[:-1], ExprNodeVariable($ID.text), $right.value))
    }
    | logic_expression { value = $logic_expression.value }
    ;

logic_expression returns[value]
    : a=relat_expression { value = $a.value } (LOGIC_OP b=relat_expression{
            value = ExprNodeBinaryOp($LOGIC_OP.text, value, $b.value)
            })*
    ;

relat_expression returns[value]
    : a=add_expression {value = $a.value} (RELAT_OP b=add_expression {
            value = ExprNodeBinaryOp($RELAT_OP.text, value, $b.value)
            })*
    ;

add_expression returns[value]
    : a=mul_expression {value = $a.value} (op=('+' | '-') b=mul_expression {
            value = ExprNodeBinaryOp($op.text, value, $b.value)
            })*
    ;

mul_expression returns[value]
    : a=unary_expression { value = $a.value} (op=('*' | '/' | '%') b=unary_expression {
            value = ExprNodeBinaryOp($op.text, value, $b.value)
            })*
    ;

unary_expression returns[value]
    : op=('-' | '!') right=unary_expression  { value = ExprNodeUnaryOp($op.text, $right.value) }
    |  primary_expression { value = $primary_expression.value }
    ;

primary_expression returns[value]
    : ID { value = ExprNodeVariable($ID.text) }
    | STRING_LITERAL { value = ExprNodeStringLiteral(eval($STRING_LITERAL.text)) }
    | INT_LITERAL { value = ExprNodeIntLiteral(int($INT_LITERAL.text)) }
    | FLOAT_LITERAL { value = ExprNodeFloatLiteral(float($FLOAT_LITERAL.text)) }
    | 'true' { value = ExprNodeIntLiteral(1) }
    | 'false' {  value = ExprNodeIntLiteral(0) }
    | call_expression { value = $call_expression.value }
    | '(' expression ')' { value = $expression.value }
    | op=('++' | '--') ID {
        value = ExprNodeAssignment($ID.text, 
                ExprNodeBinaryOp($op.text[0:1], ExprNodeVariable($ID.text), ExprNodeIntLiteral(1)))
    }
    | '(' type ')' expr=primary_expression { value = ExprNodeTypeCast($type.text, $expr.value) }
    ;

call_expression returns[value]
    : ID '(' expression_comma_list? ')' {
        value = ExprNodeCall($ID.text, $expression_comma_list.values)
    }
    ;

expression_comma_list returns[values]
    : a=expression { values = [$a.value] } 
        (',' b=expression { values.append($b.value) })*
    ;

// lexer
COMBINE_ASSIGN_OP : '+=' | '-=' | '*=' | '/=' | '%=';
RELAT_OP : '==' | '!=' | '<' | '<=' | '>' | '>=';
LOGIC_OP : '&&' | '||';

ID : ID_HEAD_LETTER ID_BODY_LETTER*;
STRING_LITERAL : '"' (~('"' | '\\') | '\\' .)* '"';
INT_LITERAL : DIGIT+;
FLOAT_LITERAL : DIGIT* '.' DIGIT*;

COMMENT : '/*' (options{greedy=false;}:.)* '*/' {self.skip()};
LINE_COMMENT : '//' ~('\r'|'\n')* '\r'?'\n' {self.skip()};
WS : (' ' | '\t' | '\r'?'\n')+ {self.skip()};

fragment ID_HEAD_LETTER : ALPHA | '_';
fragment ID_BODY_LETTER : ID_HEAD_LETTER | DIGIT;
fragment ALPHA : 'a'..'z' | 'A'..'Z';
fragment DIGIT : '0' ..'9';
