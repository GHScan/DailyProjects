
grammar JSMinus;

options {
    language = Cpp;
    //backtrack = true;
    k = 1;
}

@lexer::header {
#include "pch.h"
}
@lexer::traits {
    class JSMinusLexer; 
    class JSMinusParser; 
    typedef antlr3::Traits< JSMinusLexer, JSMinusParser > JSMinusLexerTraits;
    typedef JSMinusLexerTraits JSMinusParserTraits;
}

@parser::header {
#include "pch.h"
#include "JSMinusLexer.hpp"
#include "AST.h"
#include "SymbolTable.h"
#include "ByteCode.h"
#include "JSVM.h"
#include "JSFunction.h"
}
@parser::members {

static ExprNodePtr getExprNodeFromID(int line, const string& name) {
    int localIdx = SymbolTable::topTable()->getLocalIdx(name);
    if (localIdx == -1) {
        int constIdx = SymbolTable::topTable()->getMeta()->getConstIdx(JSValue::fromString(name.c_str()));
        return ExprNodePtr(new ExprNode_Global(line, constIdx));
    }
    else return ExprNodePtr(new ExprNode_Local(line, localIdx));
}
static ExprNodePtr getExprNodeFromConst(int line, const JSValue& cv) {
    int constIdx = SymbolTable::topTable()->getMeta()->getConstIdx(cv);
    return ExprNodePtr(new ExprNode_Const(line, constIdx));
}

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


// syntax

program [const char *fileName]returns[FuncMetaPtr value]: {
        value.reset(new FuncMeta(fileName));
        value->stmt.reset(new StmtNode_Block());
        SymbolTable::pushTable(value);
          }(statement {
                  if ($statement.value != NULL) {
                      static_cast<StmtNode_Block*>(value->stmt.get())->stmts.push_back($statement.value);
                  }
                  })*  EOF {
        value->localCount = SymbolTable::topTable()->getMaxLocalIdx();
        SymbolTable::popTable();
        emitCode(value);
          };

statement returns[StmtNodePtr value]
    : (leftValue assignOp)=>assignment { value = $assignment.value;}
    | op=('++' | '--') leftValue {
        value = StmtNodePtr(new StmtNode_Assign($leftValue.value->line,
                    $leftValue.value, 
                    ExprNodePtr(new
                        ExprNode_BinaryOp($leftValue.value->line,
                            $op.text == "++" ? "+" : "-",
                            $leftValue.value,
                            getExprNodeFromConst(0, JSValue::fromNumber(1))))));
    }
    | ('var'? 'function')=>funcDefine { value = $funcDefine.value; }
    | ('var' ID P_ASSIGN_OP)=>varDefine { value = $varDefine.value; }
    | rtTypeAtom // '(' exprList? ')'
    {
        ASSERT(dynamic_cast<ExprNode_Call*>($rtTypeAtom.value.get()));
        value = StmtNodePtr(new StmtNode_Call($rtTypeAtom.value->line, $rtTypeAtom.value));
    }
    | varDeclares
    | {
        value.reset(new StmtNode_Block());
    } '{' {
        SymbolTable::topTable()->pushBlock();
    } (a=statement{
            if ($a.value != NULL) {
                static_cast<StmtNode_Block*>(value.get())->stmts.push_back($a.value);
            }
            })* '}' {
        SymbolTable::topTable()->popBlock();
    }
    | if_='if' '(' expr ')' ifStmt=statement (('else')=>'else' elseStmt=statement)? {
        value = StmtNodePtr(new StmtNode_If($if_.line, $expr.value, $ifStmt.value, $elseStmt.value));
    }
    | for_='for' {
        SymbolTable::topTable()->pushBlock();
    } '(' forStart? ';' expr? ';' a=statement? ')' body=statement {
        SymbolTable::topTable()->popBlock();
        StmtNodePtr first, last;
        ExprNodePtr second;
        if ($forStart.value != NULL) first = $forStart.value;
        if ($expr.value != NULL) second = $expr.value;
        else second = getExprNodeFromConst(0, JSValue::TRUE);
        if ($a.value != NULL) last = $a.value;
        value = StmtNodePtr(new StmtNode_For($for_.line, first, second, last, $body.value));
    }
    | while_='while' '(' expr ')' a=statement {
        value = StmtNodePtr(new StmtNode_For($while_.line, StmtNodePtr(), $expr.value, StmtNodePtr(), $a.value));
    }
    | break_='break' {
        value = StmtNodePtr(new StmtNode_Break($break_.line));
    }
    | continue_='continue' {
        value = StmtNodePtr(new StmtNode_Continue($continue_.line));
    }
    | return_='return' ((expr)=>expr)? {
        ExprNodePtr e;
        if ($expr.value != NULL) e = $expr.value;
        value = StmtNodePtr(new StmtNode_Return($return_.line, e));
    }
    | ';'
    ;

varDeclares returns[StmtNodePtr value]
    : 'var' idList {
        for (auto &name : $idList.values) {
            SymbolTable::topTable()->declareLocal(name);
        }
    }
    ;
varDefine returns[StmtNodePtr value]
    : var='var' ID P_ASSIGN_OP expr {
        SymbolTable::topTable()->declareLocal($ID.text);
        value = StmtNodePtr(new StmtNode_Assign($var.line, getExprNodeFromID($ID.line, $ID.text), $expr.value));
    }
    ;

funcDefine returns[StmtNodePtr value]
    : var='var'? func='function' ID funcBody {
        if ($var != NULL) SymbolTable::topTable()->declareLocal($ID.text);
        value = StmtNodePtr(new StmtNode_Assign($func.line, getExprNodeFromID($ID.line, $ID.text), $funcBody.value));
    }
    ;

forStart returns[StmtNodePtr value]
    : assignment { value = $assignment.value; }
    | varDefine { value = $varDefine.value; }
    ;

idList returns[vector<string> values]
    : a=ID {
        values.push_back($a.text);
    } (',' b=ID {
        values.push_back($b.text);
            })*
    ;

assignment returns[StmtNodePtr value]
    : leftValue assignOp expr {
        auto rightExpr = $expr.value;
        if ($assignOp.text != "=") {
            rightExpr = ExprNodePtr(new ExprNode_BinaryOp($expr.value->line, $assignOp.text.substr(0, 1), $leftValue.value, rightExpr));
        }
        value = StmtNodePtr(new StmtNode_Assign($leftValue.value->line, $leftValue.value, rightExpr));
    }
    ;

funcBody returns[ExprNodePtr value]
scope {
    FuncMetaPtr meta;
}
    : lp='(' idList? ')' {
        $funcBody::meta.reset(new FuncMeta(SymbolTable::topTable()->getMeta()->fileName));
        auto meta = $funcBody::meta;
        SymbolTable::pushTable(meta);
        meta->stmt.reset(new StmtNode_Block());
        meta->argCount = (int)$idList.values.size();
        for (auto &name : $idList.values) {
            SymbolTable::topTable()->declareLocal(name);
        }
    } '{' (statement{
            if ($statement.value != NULL) {
                static_cast<StmtNode_Block*>($funcBody::meta->stmt.get())->stmts.push_back($statement.value);
            }
            })* '}' {
        auto meta = $funcBody::meta;
        meta->localCount = SymbolTable::topTable()->getMaxLocalIdx();
        SymbolTable::popTable();
        emitCode(meta);
        value = ExprNodePtr(new ExprNode_Lambda($lp.line, JSVM::instance()->getMetaIdx(meta)));
    }
    ;

leftValue returns[ExprNodePtr value]
    //: (ID assignOp)=>ID
    : rtTypeAtom //'[' expr ']' 
    {
        value = $rtTypeAtom.value;
        ASSERT(dynamic_cast<ExprNode_Global*>(value.get()) ||
                dynamic_cast<ExprNode_Local*>(value.get()) ||
                dynamic_cast<ExprNode_IndexOf*>(value.get()));
    }
    ;

arrayExpr returns[ExprNodePtr value]
    : lp='[' exprList ']' {
        value = ExprNodePtr(new ExprNode_ArrayConstructor($lp.line, $exprList.values));
    }
    ;
lambdaExpr returns[ExprNodePtr value]
    : 'function' funcBody { value = $funcBody.value; }
    ;

exprList returns[vector<ExprNodePtr> values]
    : a=expr {
        values.push_back($a.value);
        } (',' b=expr {
        values.push_back($b.value);
        })*
    |
    ;
expr returns [ExprNodePtr value]
    : logicExpr { value  = $logicExpr.value; }
    ;

logicExpr returns[ExprNodePtr value]
    : a=relatExpr {
        value = $a.value;
    } (op=('&&' | '||') b=relatExpr {
            value = ExprNodePtr(new ExprNode_BinaryOp(value->line, $op.text, value, $b.value));
            })*
    ;
    
relatExpr returns[ExprNodePtr value]
    : a=addExpr {
        value = $a.value;
    } (op=('<' | '<=' | '>' | '>=' | '==' | '!=') b=addExpr {
            value = ExprNodePtr(new ExprNode_BinaryOp(value->line, $op.text, value, $b.value));
            })*
    ;

addExpr returns[ExprNodePtr value]
    : a=mulExpr {
        value = $a.value;
    } (op=('+' | '-') b=mulExpr {
            value = ExprNodePtr(new ExprNode_BinaryOp(value->line, $op.text, value, $b.value));
            })*
    ;

mulExpr returns[ExprNodePtr value]
    : a=unaryExpr {
        value = $a.value;
    } (op=('*' | '/' | '%') b=unaryExpr {
            value = ExprNodePtr(new ExprNode_BinaryOp(value->line, $op.text, value, $b.value));
            })*
    ; 

unaryExpr returns[ExprNodePtr value]
    : (op=('not' | '-' | '#'))? atom {
        value = $atom.value;
        if ($op != NULL) {
            value = ExprNodePtr(new ExprNode_UnaryOp($op.line, $op.text, value));
        }
    }
    ;

atom returns[ExprNodePtr value]
    : literalAtom { $value = $literalAtom.value;}
    | rtTypeAtom { $value = $rtTypeAtom.value; }
    | '(' expr ')' { $value = $expr.value; }
    ;

literalAtom returns[ExprNodePtr value]
    : NUMBER_LITERAL {
        double num;
        sscanf($NUMBER_LITERAL.text.c_str(), "\%lf", &num);
        value = getExprNodeFromConst($NUMBER_LITERAL.line, JSValue::fromNumber(num));
    }
    | STRING_LITERAL { 
        string str = $STRING_LITERAL.text;
        str = unEscape(str.substr(1, str.size() - 2));
        value = getExprNodeFromConst($STRING_LITERAL.line, JSValue::fromString(str.c_str()));
    }
    | arrayExpr { value = $arrayExpr.value; }
    | lambdaExpr { value = $lambdaExpr.value; }
    | lit='true' { value = getExprNodeFromConst($lit.line, JSValue::TRUE);}
    | lit='false' { value = getExprNodeFromConst($lit.line, JSValue::FALSE);}
    | lit='null' { value = getExprNodeFromConst($lit.line, JSValue::NIL);}
    ;

rtTypeAtomHead returns[ExprNodePtr value]
    : ID { value = getExprNodeFromID($ID.line, $ID.text); }
    //| lambdaExpr '(' exprList? ')'
    ;
rtTypeAtom returns [ExprNodePtr value]
    : rtTypeAtomHead {
        value = $rtTypeAtomHead.value;
    } (
            '(' exprList  ')' {
                value = ExprNodePtr(new ExprNode_Call(value->line, value, $exprList.values));
            }
            | 
            '[' expr ']' {
                value = ExprNodePtr(new ExprNode_IndexOf(value->line, value, $expr.value));
            }
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
