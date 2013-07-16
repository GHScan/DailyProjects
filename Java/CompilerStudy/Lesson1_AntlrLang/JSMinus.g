
grammar JSMinus;

options {
    //backtrack = true;
    k = 1;
}

@lexer::header {
}

@parser::header {
import java.util.ArrayList;
import java.util.HashMap;
}
@parser::members {

static String unEscape(String s) {
    StringBuilder builder = new StringBuilder();
    for (int i = 0; i < s.length(); ++i) {
        if (s.charAt(i) == '\\') {
            ++i;
            switch (s.charAt(i)) { 
                case 't':
                    builder.append('\t');
                    break;
                case 'n':
                    builder.append('\n');
                    break;
                case 'r':
                    builder.append('\r');
                    break;
                default:
                    builder.append(s.charAt(i));
                    break;
            }
        } else {    
            builder.append(s.charAt(i));
        }
    }
    return builder.toString();
}

}


// syntax

public
program returns[HashMap<String, FuncMeta> value]
scope {
    HashMap<String, FuncMeta> funcs;
    StmtNode_Block block;
}
    : {
        $program::block = new StmtNode_Block(1);
        $program::funcs = new HashMap<String, FuncMeta>();
    } (statement{
            if ($statement.value != null) {
                $program::block.stmts.add($statement.value);
            }
            })*  EOF {
        $program::funcs.put("__main", new FuncMeta("__main", 0, $program::block));
        value = $program::funcs;
    };

statement returns[StmtNode value]
    : (leftValue assignOp)=>assignment { value = $assignment.value;}
    | op=('++' | '--') leftValue {
        int line = $op.line;
        value = new StmtNode_Assign(line, $leftValue.value, 
                new ExprNode_BinaryOp(line, $op.text.substring(0, 1),
                    $leftValue.value, new ExprNode_ConstNumber(line, 1)));
    }
    | ('function')=>funcDefine 
    | ('var' ID P_ASSIGN_OP)=>varDefine { value = $varDefine.value ;}
    | rtTypeAtom // '(' exprList? ')' 
    {
        value = new StmtNode_Call($rtTypeAtom.value.line, (ExprNode_Call)$rtTypeAtom.value);
        assert $rtTypeAtom.value instanceof ExprNode_Call;
    }
    | varDeclares { value = $varDeclares.value; }
    |  lb='{' {
        value = new StmtNode_Block($lb.line);
    } (a=statement{
            if ($a.value != null) {
                ((StmtNode_Block)value).stmts.add($a.value);
            }
            })* '}' 
    | if_='if' '(' expr ')' ifStmt=statement (('else')=>'else' elseStmt=statement)?  {
        value = new StmtNode_IfElse($if_.line, $expr.value, $ifStmt.value, $elseStmt.value);
    }
    | for_='for'  '(' forStart? ';' expr? ';' third=statement? ')' bodyStmt=statement {
        value = new StmtNode_For($for_.line, $forStart.value, $expr.value, $third.value, $bodyStmt.value);
        StmtNode_Block block = new StmtNode_Block($for_.line);
        block.stmts.add(value);
        value = block;
    }
    | while_='while' '(' expr ')' bodyStmt=statement {
        value = new StmtNode_For($while_.line, null, $expr.value, null, $bodyStmt.value);
    }
    | break_='break' { value = new StmtNode_Break($break_.line); }
    | continue_='continue' { value = new StmtNode_Continue($continue_.line); }
    | return_='return' ((expr)=>expr)? { value = new StmtNode_Return($return_.line, $expr.value); }
    | ';'
    ;

varDeclares returns[StmtNode value]
    : var_='var' idList {
        StmtNode_Stmts stmts = new StmtNode_Stmts($var_.line);
        for (String name : $idList.value) {
            stmts.stmts.add(new StmtNode_DeclareLocal($var_.line, name));
        }
        value = stmts;
    }
    ;
varDefine returns[StmtNode value]
    : 'var' ID P_ASSIGN_OP expr {
        StmtNode_Stmts stmts = new StmtNode_Stmts($ID.line);
        stmts.stmts.add(new StmtNode_DeclareLocal($ID.line, $ID.text));
        stmts.stmts.add(new StmtNode_Assign($ID.line, new ExprNode_ID($ID.line, $ID.text), $expr.value));
        value = stmts;
    }
    ;

funcDefine 
scope {
    StmtNode_Block block;
}
    : 'function' ID '(' idList? ')' {
        StmtNode_Block block = new StmtNode_Block($ID.line);
        if ($idList.value != null) {
            for (String name : $idList.value) {
                block.stmts.add(new StmtNode_DeclareArg($ID.line, name));
            }
        }
        $funcDefine::block = block;
    } '{' (statement {
            if ($statement.value != null) {
                $funcDefine::block.stmts.add($statement.value);
            }
            })* '}' {
        $program::funcs.put($ID.text, new FuncMeta($ID.text, $idList.value == null ? 0 : $idList.value.size(), $funcDefine::block));
    }
    ;

forStart returns[StmtNode value]
    : assignment { value = $assignment.value;}
    | varDefine { value = $varDefine.value;}
    ;

idList returns[ArrayList<String> value]
    : a=ID { 
        value = new ArrayList<String>(); 
        value.add($a.text);
    } 
    (',' b=ID {
        value.add($b.text);
            })*
    ;

assignment returns[StmtNode value]
    : leftValue assignOp expr {
        ExprNode rexpr = $expr.value;
        String op = $assignOp.text;
        if (!op.equals("=")) {
            rexpr = new ExprNode_BinaryOp($leftValue.value.line, op.substring(0, 1), $leftValue.value, rexpr);
        }
        value = new StmtNode_Assign($leftValue.value.line, $leftValue.value, rexpr);
    }
    ;

leftValue returns[ExprNode value]
    //: (ID assignOp)=>ID
    : rtTypeAtom //'[' expr ']' 
    {
        value = $rtTypeAtom.value;
        assert (value instanceof ExprNode_ID) || (value instanceof ExprNode_IndexOf);
    }
    ;

arrayExpr returns[ExprNode value]
    : lp='[' exprList ']' { value = new ExprNode_ArrayConstructor($lp.line, $exprList.value);}
    ;

exprList returns[ArrayList<ExprNode> value]
    : a=expr {
        value = new ArrayList<ExprNode>();
        value.add($a.value);
    }
    (',' b=expr { value.add($b.value); })*
    |
    ;
expr returns[ExprNode value]
    : logicExpr { value = $logicExpr.value;}
    ;

logicExpr returns[ExprNode value]
    : a=relatExpr {
        value = $a.value;
    } (op=('&&' | '||') b=relatExpr {
            value = new ExprNode_BinaryOp(value.line, $op.text, value, $b.value);
            } )*
    ;
    
relatExpr returns[ExprNode value]
    : a=addExpr {
        value = $a.value;
    } (op=('<' | '<=' | '>' | '>=' | '==' | '!=') b=addExpr{
            value = new ExprNode_BinaryOp(value.line, $op.text, value, $b.value);
            } )*
    ;

addExpr returns[ExprNode value]
    : a=mulExpr  {
        value = $a.value;
    } (op=('+' | '-') b=mulExpr {
            value = new ExprNode_BinaryOp(value.line, $op.text, value, $b.value);
            })*
    ;

mulExpr returns[ExprNode value]
    : a=unaryExpr {
        value = $a.value;
    } (op=('*' | '/' | '%' | '^') b=unaryExpr {
            value = new ExprNode_BinaryOp(value.line, $op.text, value, $b.value);
            })*
    ; 

unaryExpr returns[ExprNode value]
    : (op=('not' | '-' | '#'))? atom {
        value = $atom.value;
        if ($op != null) value = new ExprNode_UnaryOp(value.line, $op.text, value);
    }
    ;

atom returns[ExprNode value]
    : literalAtom { value = $literalAtom.value;}
    | rtTypeAtom { value = $rtTypeAtom.value; }
    | '(' expr ')' { value = $expr.value;}
    ;

literalAtom returns[ExprNode value]
    : NUMBER_LITERAL { value = new ExprNode_ConstNumber($NUMBER_LITERAL.line, Double.parseDouble($NUMBER_LITERAL.text)); }
    | STRING_LITERAL { value = new
        ExprNode_ConstString($STRING_LITERAL.line,
                unEscape($STRING_LITERAL.text.substring(1, $STRING_LITERAL.text.length() - 1))); }
    | arrayExpr { value = $arrayExpr.value; }
    | lex='true' { value = new ExprNode_ConstNumber($lex.line, 1);}
    | lex='false' { value = new ExprNode_ConstNumber($lex.line, 0);}
    | lex='nil' { value = new ExprNode_ConstString($lex.line, null);}
    ;

rtTypeAtomHead returns[ExprNode value]
    : ID { value = new ExprNode_ID($ID.line, $ID.text);}
    ;
rtTypeAtom returns[ExprNode value]
    : rtTypeAtomHead { value = $rtTypeAtomHead.value; }  
    ( '(' exprList ')'  { value = new ExprNode_Call(value.line, value, $exprList.value); }
            | '[' expr ']' { value = new ExprNode_IndexOf(value.line, value, $expr.value); }
            )* 
    ;

assignOp
    : P_ASSIGN_OP | C_ASSIGN_OP
    ;
// lex

P_ASSIGN_OP: '=';
C_ASSIGN_OP: '-=' | '+=' | '*=' | '/=' | '%=';
 
ID : ID_START ID_LETTER*;
STRING_LITERAL : '\'' (~('\'' | '\\') | '\\' .)* '\'';
NUMBER_LITERAL : DIGIT+ ('.' DIGIT+)?;

WS : (' ' | '\t' | '\n' | '\r')+ {skip();};
LINE_COMMENT : '//' ~('\r' | '\n')* '\r'? '\n' {skip();};
COMMENT : '/*' (options{greedy=false;}:.)* '*/' {skip();};

fragment ID_START : ALPHA | '_';
fragment ID_LETTER : ID_START | DIGIT;
fragment ALPHA : 'a' .. 'z' | 'A' .. 'Z';
fragment DIGIT : '0' .. '9';
