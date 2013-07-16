
grammar JSMinus;

options {
    language = CSharp3;
    //backtrack = true;
    k = 1;
}

@lexer::header {
}

@parser::header {
using System.Text;
using System.Diagnostics;
}
@parser::members {

static string unEscape(string s) {
    var builder = new StringBuilder();
    for (var i = 0; i < s.Length; ++i) {
        if (s[i] == '\\') {
            ++i;
            switch (s[i]) { 
                case 't':
                    builder.Append('\t');
                    break;
                case 'n':
                    builder.Append('\n');
                    break;
                case 'r':
                    builder.Append('\r');
                    break;
                default:
                    builder.Append(s[i]);
                    break;
            }
        } else {    
            builder.Append(s[i]);
        }
    }
    return builder.ToString();
}

}


// syntax

public
program returns[Dictionary<string, FuncMeta> values]
scope {
    Dictionary<string, FuncMeta> funcs;
    StmtNode_Block block;
}
    : {
        $program::block = new StmtNode_Block(1);
        $program::funcs = new Dictionary<string, FuncMeta>();
    } (statement{
            if ($statement.value != null) {
                $program::block.Stmts.Add($statement.value);
            }
            })*  EOF {
        $program::funcs["Main"] = new FuncMeta("Main", 0, $program::block);
        values = $program::funcs;
    };

statement returns[StmtNode value]
    : (leftValue assignOp)=>assignment { value = $assignment.value;}
    | op=('++' | '--') leftValue {
        int line = $op.line;
        value = new StmtNode_Assign(line, $leftValue.value, 
                new ExprNode_BinaryOp(line, $op.text.Substring(0, 1),
                    $leftValue.value, new ExprNode_ConstNumber(line, 1)));
    }
    | ('function')=>funcDefine 
    | ('var' ID P_ASSIGN_OP)=>varDefine { value = $varDefine.value ;}
    | rtTypeAtom // '(' exprList? ')' 
    {
        value = new StmtNode_Call($rtTypeAtom.value.Line, $rtTypeAtom.value as ExprNode_Call);
        Trace.Assert($rtTypeAtom.value is ExprNode_Call);
    }
    | varDeclares { value = $varDeclares.value; }
    |  lb='{' {
        value = new StmtNode_Block($lb.line);
    } (a=statement{
            if ($a.value != null) {
                (value as StmtNode_Block).Stmts.Add($a.value);
            }
            })* '}' 
    | if_='if' '(' expr ')' ifStmt=statement (('else')=>'else' elseStmt=statement)?  {
        value = new StmtNode_IfElse($if_.line, $expr.value, $ifStmt.value, $elseStmt.value);
    }
    | for_='for'  '(' forStart? ';' expr? ';' third=statement? ')' bodyStmt=statement {
        value = new StmtNode_For($for_.line, $forStart.value, $expr.value, $third.value, $bodyStmt.value);
        var block = new StmtNode_Block($for_.line);
        block.Stmts.Add(value);
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
        var stmts = new StmtNode_Stmts($var_.line);
        foreach(var name in $idList.values) {
            stmts.Stmts.Add(new StmtNode_DeclareLocal($var_.line, name));
        }
        value = stmts;
    }
    ;
varDefine returns[StmtNode value]
    : 'var' ID P_ASSIGN_OP expr {
        var stmts = new StmtNode_Stmts($ID.line);
        stmts.Stmts.Add(new StmtNode_DeclareLocal($ID.line, $ID.text));
        stmts.Stmts.Add(new StmtNode_Assign($ID.line, new ExprNode_ID($ID.line, $ID.text), $expr.value));
        value = stmts;
    }
    ;

funcDefine 
scope {
    StmtNode_Block block;
}
    : 'function' ID '(' idList? ')' {
        var block = new StmtNode_Block($ID.line);
        if ($idList.values != null) {
            foreach(var name in $idList.values) {
                block.Stmts.Add(new StmtNode_DeclareArg($ID.line, name));
            }
        }
        $funcDefine::block = block;
    } '{' (statement {
            if ($statement.value != null) {
                $funcDefine::block.Stmts.Add($statement.value);
            }
            })* '}' {
        $program::funcs[$ID.text] = new FuncMeta($ID.text, $idList.values == null ? 0 : $idList.values.Count, $funcDefine::block);
    }
    ;

forStart returns[StmtNode value]
    : assignment { value = $assignment.value;}
    | varDefine { value = $varDefine.value;}
    ;

idList returns[List<string> values]
    : a=ID { values = new List<string>(){$a.text}; } 
    (',' b=ID {
        values.Add($b.text);
            })*
    ;

assignment returns[StmtNode value]
    : leftValue assignOp expr {
        ExprNode rexpr = $expr.value;
        string op = $assignOp.text;
        if (op != "=") {
            rexpr = new ExprNode_BinaryOp($leftValue.value.Line, op.Substring(0, 1), $leftValue.value, rexpr);
        }
        value = new StmtNode_Assign($leftValue.value.Line, $leftValue.value, rexpr);
    }
    ;

leftValue returns[ExprNode value]
    //: (ID assignOp)=>ID
    : rtTypeAtom //'[' expr ']' 
    {
        value = $rtTypeAtom.value;
        Trace.Assert((value is ExprNode_ID) || (value is ExprNode_IndexOf));
    }
    ;

arrayExpr returns[ExprNode value]
    : lp='[' exprList ']' { value = new ExprNode_ArrayConstructor($lp.line, $exprList.values);}
    ;

exprList returns[List<ExprNode> values]
    : a=expr {values = new List<ExprNode>(){$a.value };} 
    (',' b=expr { values.Add($b.value); })*
    |
    ;
expr returns[ExprNode value]
    : logicExpr { value = $logicExpr.value;}
    ;

logicExpr returns[ExprNode value]
    : a=relatExpr {
        value = $a.value;
    } (op=('&&' | '||') b=relatExpr {
            value = new ExprNode_BinaryOp(value.Line, $op.text, value, $b.value);
            } )*
    ;
    
relatExpr returns[ExprNode value]
    : a=addExpr {
        value = $a.value;
    } (op=('<' | '<=' | '>' | '>=' | '==' | '!=') b=addExpr{
            value = new ExprNode_BinaryOp(value.Line, $op.text, value, $b.value);
            } )*
    ;

addExpr returns[ExprNode value]
    : a=mulExpr  {
        value = $a.value;
    } (op=('+' | '-') b=mulExpr {
            value = new ExprNode_BinaryOp(value.Line, $op.text, value, $b.value);
            })*
    ;

mulExpr returns[ExprNode value]
    : a=unaryExpr {
        value = $a.value;
    } (op=('*' | '/' | '%' | '^') b=unaryExpr {
            value = new ExprNode_BinaryOp(value.Line, $op.text, value, $b.value);
            })*
    ; 

unaryExpr returns[ExprNode value]
    : (op=('not' | '-' | '#'))? atom {
        value = $atom.value;
        if ($op != null) value = new ExprNode_UnaryOp(value.Line, $op.text, value);
    }
    ;

atom returns[ExprNode value]
    : literalAtom { value = $literalAtom.value;}
    | rtTypeAtom { value = $rtTypeAtom.value; }
    | '(' expr ')' { value = $expr.value;}
    ;

literalAtom returns[ExprNode value]
    : NUMBER_LITERAL { value = new ExprNode_ConstNumber($NUMBER_LITERAL.line, double.Parse($NUMBER_LITERAL.text)); }
    | STRING_LITERAL { value = new
        ExprNode_ConstString($STRING_LITERAL.line,
                unEscape($STRING_LITERAL.text.Substring(1, $STRING_LITERAL.text.Length - 2))); }
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
    ( '(' exprList ')'  { value = new ExprNode_Call(value.Line, value, $exprList.values); }
            | '[' expr ']' { value = new ExprNode_IndexOf(value.Line, value, $expr.value); }
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

WS : (' ' | '\t' | '\n' | '\r')+ {$channel = Hidden;};
LINE_COMMENT : '//' ~('\r' | '\n')* '\r'? '\n' {$channel = Hidden;};
COMMENT : '/*' (options{greedy=false;}:.)* '*/' {$channel = Hidden;};

fragment ID_START : ALPHA | '_';
fragment ID_LETTER : ID_START | DIGIT;
fragment ALPHA : 'a' .. 'z' | 'A' .. 'Z';
fragment DIGIT : '0' .. '9';
