
#include "pch.h"
#include "ByteCode.h"
#include "LuaVM.h"
#include "LuaStack.h"
#include "LuaFunction.h"
#include "LuaString.h"
#include "LuaTable.h"
#include "ByteCodeDefine.h"
#include "AST.h"

#define EMIT0(code) { m_codes.push_back(0); ByteCodeHandler<code>::emit(m_codes.back()); }
#define EMIT(code, ...) { m_codes.push_back(0); ByteCodeHandler<code>::emit(m_codes.back(), __VA_ARGS__); }
#define PRE_EMIT(off) { off = (int)m_codes.size(); m_codes.push_back(0); }
#define POST_EMIT0(off, code) { ByteCodeHandler<code>::emit(m_codes[off]); }
#define POST_EMIT(off, code, ...) {ByteCodeHandler<code>::emit(m_codes[off], __VA_ARGS__); }
#define EMIT_PUSH_CONST(v) {m_codes.push_back(0); ByteCodeHandler<BC_PushConst>::emit(m_codes.back(), m_meta->getConstIdx(v)); }
#define CUR_CODE_OFF int(m_codes.size())
#define EMIT_JUMPS(type) {\
    for (auto jump##type : m_jumps##type) {\
        POST_EMIT(jump##type, BC_Jump, CUR_CODE_OFF);\
    }\
    m_jumps##type.clear();\
}

class ExpNodeVisitor_CodeEmitor:
    public IExpNodeVisitor {
public:
    ExpNodeVisitor_CodeEmitor(LuaFunctionMeta* meta, const ExpNodePtr &exp):
        m_codes(meta->codes), m_meta(meta) {
        exp->acceptVisitor(this);
    }
private:
    virtual void visit(ExpNode_UnaryOp *node) {
        (ExpNodeVisitor_CodeEmitor(m_meta, node->exp));
        switch (node->op) {
            case ExpNode_UnaryOp::OP_Not: EMIT0(BC_Not); break;
            case ExpNode_UnaryOp::OP_Minus: EMIT0(BC_Minus); break;
            case ExpNode_UnaryOp::OP_Len: EMIT0(BC_Len); break;
            default: ASSERT(0);
        }
    }
    virtual void visit(ExpNode_BinaryOp *node) {
        (ExpNodeVisitor_CodeEmitor(m_meta, node->lexp));
        if (node->op == ExpNode_BinaryOp::OP_And || node->op == ExpNode_BinaryOp::OP_Or)  {
            /*
               push lval
               pushtop 0
               fjump label// tjump label
               popN 1
               push rval
label:
            */
            EMIT(BC_PushTop, 0);
            int label = 0;
            PRE_EMIT(label);
            EMIT(BC_PopN, 1);
            (ExpNodeVisitor_CodeEmitor(m_meta, node->rexp));
            if (node->op == ExpNode_BinaryOp::OP_Add) {
                POST_EMIT(label, BC_FalseJump, CUR_CODE_OFF);
            } else {
                POST_EMIT(label, BC_TrueJump, CUR_CODE_OFF);
            }
            return;
        }
        (ExpNodeVisitor_CodeEmitor(m_meta, node->rexp));
        switch (node->op) {
            case ExpNode_BinaryOp::OP_Less: EMIT0(BC_Less); break;
            case ExpNode_BinaryOp::OP_LessEq: EMIT0(BC_LessEq); break;
            case ExpNode_BinaryOp::OP_Greater: EMIT0(BC_Greater); break;
            case ExpNode_BinaryOp::OP_GreaterEq: EMIT0(BC_GreaterEq); break;
            case ExpNode_BinaryOp::OP_Equal: EMIT0(BC_Equal); break;
            case ExpNode_BinaryOp::OP_NEqual: EMIT0(BC_NEqual); break;
            case ExpNode_BinaryOp::OP_Add: EMIT0(BC_Add); break;
            case ExpNode_BinaryOp::OP_Sub: EMIT0(BC_Sub); break;
            case ExpNode_BinaryOp::OP_Mul: EMIT0(BC_Mul); break;
            case ExpNode_BinaryOp::OP_Div: EMIT0(BC_Div); break;
            case ExpNode_BinaryOp::OP_Mod: EMIT0(BC_Mod); break;
            case ExpNode_BinaryOp::OP_Pow: EMIT0(BC_Pow); break;
            case ExpNode_BinaryOp::OP_Concat: EMIT0(BC_Concat); break;
            default: ASSERT(0);
        }
    } 
    virtual void visit(ExpNode_Const *node) {
        EMIT(BC_PushConst, node->constIdx);
    }
    virtual void visit(ExpNode_LocalVar *node) {
        EMIT(BC_PushLocal, node->localIdx);
    }
    virtual void visit(ExpNode_UpValueVar *node) {
        EMIT(BC_PushUpValue, node->uvIdx);
    }
    virtual void visit(ExpNode_GlobalVar *node) {
        EMIT(BC_PushGlobal, node->constIdx);
    }
    virtual void visit(ExpNode_FieldAccess *node) {
        (ExpNodeVisitor_CodeEmitor(m_meta, node->table));
        (ExpNodeVisitor_CodeEmitor(m_meta, node->field));
        EMIT0(BC_GetTable);
    }
    virtual void visit(ExpNode_TableConstructor *node) {
        EMIT0(BC_PushNewTable);

        EMIT(BC_PushTop, 0);
        for (int i = 0; i < (int)node->array.size(); ++i) {
            (ExpNodeVisitor_CodeEmitor(m_meta, node->array[i]));
        }
        EMIT0(BC_ResizeTemp2Ext);
        EMIT(BC_PushAll2Table, (int)node->array.size());

        for (auto &pair : node->dict) {
            EMIT(BC_PushTop, 0);
            (ExpNodeVisitor_CodeEmitor(m_meta, pair.first));
            (ExpNodeVisitor_CodeEmitor(m_meta, pair.second));
            EMIT0(BC_SetTable);
        }
    }
    virtual void visit(ExpNode_Lambda *node) {
        int idx = LuaVM::instance()->getFunctionMetaIdx(node->meta);
        EMIT(BC_PushNewFunction, idx);
    }
    virtual void visit(ExpNode_Call *node) {
        (ExpNodeVisitor_CodeEmitor(m_meta, node->func));
        for (auto &param : node->params) {
            (ExpNodeVisitor_CodeEmitor(m_meta, param));
        }
        EMIT0(BC_ResizeTemp2Ext);
        EMIT(BC_Call, (int)node->params.size());
    }
    virtual void visit(ExpNode_Args *node) {
        EMIT0(BC_PushVArgs);
    }
private:
    vector<int> &m_codes;
    LuaFunctionMeta *m_meta;
};

class StmtNodeVisitor_CodeEmitor:
    public IStmtNodeVisitor {
public:
    StmtNodeVisitor_CodeEmitor(LuaFunctionMeta* meta, const StmtNodePtr& stmt):
        m_codes(meta->codes), m_meta(meta) {
        stmt->acceptVisitor(this);
        EMIT_JUMPS(Return);
    }
private:
    virtual void visit(StmtNode_Call *node) {
        (ExpNodeVisitor_CodeEmitor(m_meta, node->callExp));
        EMIT0(BC_PopTemps);
    }
    virtual void visit(StmtNode_Assign *node) {
        for (auto &exp : node->rvalues) {
            (ExpNodeVisitor_CodeEmitor(m_meta, exp));
        }
        EMIT(BC_ResizeTemp, (int)node->lvalues.size());

        for (int i = 0; i < (int)node->lvalues.size(); ++i) {
            if (auto localExp = dynamic_cast<ExpNode_LocalVar*>(node->lvalues[i].get())) {
                EMIT(BC_PushI, i);
                EMIT(BC_PopLocal, localExp->localIdx);
            } else if (auto uvExp = dynamic_cast<ExpNode_UpValueVar*>(node->lvalues[i].get())) {
                EMIT(BC_PushI, i);
                EMIT(BC_PopUpValue, uvExp->uvIdx);
            } else if (auto globalExp = dynamic_cast<ExpNode_GlobalVar*>(node->lvalues[i].get())) {
                EMIT(BC_PushI, i);
                EMIT(BC_PopGlobal, globalExp->constIdx);
            } else if (auto fieldExp = dynamic_cast<ExpNode_FieldAccess*>(node->lvalues[i].get())) {
                (ExpNodeVisitor_CodeEmitor(m_meta, fieldExp->table));
                (ExpNodeVisitor_CodeEmitor(m_meta, fieldExp->field));
                EMIT(BC_PushI, i);
                EMIT0(BC_SetTable);
            } else {
                ASSERT(0);
            }
        }
    }
    virtual void visit(StmtNode_Break *node) {
        int jump_break;
        PRE_EMIT(jump_break);
        m_jumpsBreak.push_back(jump_break);
    }
    virtual void visit(StmtNode_Continue *node) {
        int jump_continue;
        PRE_EMIT(jump_continue);
        m_jumpsBreak.push_back(jump_continue);
    }
    virtual void visit(StmtNode_Return *node) {
        for (auto &exp : node->exps) {
            (ExpNodeVisitor_CodeEmitor(m_meta, exp));
        }
        EMIT0(BC_ResizeTemp2Ext);
        EMIT0(BC_Return);
    }
    virtual void visit(StmtNode_Block *node) {
        for (int i = 0; i < (int)node->stmts.size(); ++i) {
            (StmtNodeVisitor_CodeEmitor(m_meta, node->stmts[i]));
        }
    }
    virtual void visit(StmtNode_IfElse *node) {
        /*
           jump case
label1:
            stmts1
            jump end
label2:
            stmts2
            jump end
            ...
else:
            elseStmt
            jump end

case:
           push exp1
           tjump label1
           push exp2
           tjump label2
           ...
           jump else
end:
         * */
        int jump_case;
        PRE_EMIT(jump_case);

        vector<int> labels, jumps_end;
        for (auto &pair : node->ifExpStmts) {
            labels.push_back(CUR_CODE_OFF);
            (StmtNodeVisitor_CodeEmitor(m_meta, pair.second));
            jumps_end.push_back(0);
            PRE_EMIT(jumps_end.back());
        }
        if (node->elseStmt != NULL) {
            labels.push_back(CUR_CODE_OFF);
            (StmtNodeVisitor_CodeEmitor(m_meta, node->elseStmt));
            jumps_end.push_back(0);
            PRE_EMIT(jumps_end.back());
        }

        POST_EMIT(jump_case, BC_Jump, CUR_CODE_OFF);

        for (int i = 0; i < (int)node->ifExpStmts.size(); ++i) {
            (ExpNodeVisitor_CodeEmitor(m_meta, node->ifExpStmts[i].first));
            EMIT(BC_TrueJump, labels[i]);
        }
        if (node->elseStmt != NULL) {
            EMIT(BC_Jump, labels.back());
        }

        for (auto &jump_end : jumps_end) {
            POST_EMIT(jump_end, BC_Jump, CUR_CODE_OFF);
        }
    }
    virtual void visit(StmtNode_RangeFor *node) {
        /*
           push step
           pushConst 0
           greater
           tjump positive
           push step
           pushConst 0
           less
           tjump negative
           jump end
negative:
           emitCode_RangeFor false
           jump end
positive:
           emitCode_RangeFor true
end:
          */
        (ExpNodeVisitor_CodeEmitor(m_meta, node->step));
        EMIT_PUSH_CONST(LuaValue(NumberType(0)));
        EMIT0(BC_Greater);
        int jump_positive;
        PRE_EMIT(jump_positive);

        (ExpNodeVisitor_CodeEmitor(m_meta, node->step));
        EMIT_PUSH_CONST(LuaValue(NumberType(0)));
        EMIT0(BC_Less);
        int jump_negative;
        PRE_EMIT(jump_negative);

        int jump_end;
        PRE_EMIT(jump_end);

        POST_EMIT(jump_negative, BC_TrueJump, CUR_CODE_OFF);
        emitCode_RangeFor(node, false);
        int jump_end2;
        PRE_EMIT(jump_end2);

        POST_EMIT(jump_positive, BC_TrueJump, CUR_CODE_OFF);
        emitCode_RangeFor(node, true);

        POST_EMIT(jump_end, BC_Jump, CUR_CODE_OFF);
        POST_EMIT(jump_end2, BC_Jump, CUR_CODE_OFF);
    }
    virtual void visit(StmtNode_LoopFor *node) {
        /*
           initStmt
loop:
           push exp
           fjump break
           bodyStmt
continue:
           jump loop
break:
         * */
        if (node->initStmt != NULL) {
            (StmtNodeVisitor_CodeEmitor(m_meta, node->initStmt));
        }

        int l_loop = CUR_CODE_OFF;
        (ExpNodeVisitor_CodeEmitor(m_meta, node->exp));
        int jump_break;
        PRE_EMIT(jump_break);
        if (node->bodyStmt != NULL) {
            (StmtNodeVisitor_CodeEmitor(m_meta, node->bodyStmt));
        }
        EMIT_JUMPS(Continue);
        EMIT(BC_Jump, l_loop);

        POST_EMIT(jump_break, BC_FalseJump, CUR_CODE_OFF);
        EMIT_JUMPS(Break);
    }
    virtual void visit(StmtNode_IteratorFor *node) {
        /*
           push iterExps
           resizeTemp 3
           popLocal var0
           popLocal __state
           popLocal __func
loop:
           pushLocal __func
           pushLocal __state
           pushLocal var0
           call 2
           resizeTemp N
           popLocal varN
           ...
           popLocal var0

           pushtop 0
           pushConst nil
           equal
           tjump break:

           stmt

continue:
           jump loop
break:
         * */
        vector<int> localVars;
        for (auto &var: node->vars) {
            assert(dynamic_cast<ExpNode_LocalVar*>(var.get()));
            localVars.push_back(static_cast<ExpNode_LocalVar*>(var.get())->localIdx);
        }
        ASSERT(!localVars.empty());

        for (auto &exp : node->iterExps) {
            (ExpNodeVisitor_CodeEmitor(m_meta, exp));
        }
        EMIT(BC_ResizeTemp, 3);
        EMIT(BC_PopLocal, localVars[0]);
        EMIT(BC_PopLocal, node->stateLocalIdx);
        EMIT(BC_PopLocal, node->funcLocalIdx);

        int l_loop = CUR_CODE_OFF;
        EMIT(BC_PushLocal, node->funcLocalIdx);
        EMIT(BC_PushLocal, node->stateLocalIdx);
        EMIT(BC_PushLocal, localVars[0]);
        EMIT(BC_Call, 2);
        EMIT(BC_ResizeTemp, localVars.size());
        for (auto riter = localVars.rbegin(); riter != localVars.rend(); ++riter) {
            EMIT(BC_PopLocal, *riter);
        }

        EMIT(BC_PushTop, 0);
        EMIT_PUSH_CONST(LuaValue::NIL);
        EMIT0(BC_Equal);
        int jump_break;
        PRE_EMIT(jump_break);

        if (node->stmt != NULL) {
            (StmtNodeVisitor_CodeEmitor(m_meta, node->stmt));
        }
        EMIT_JUMPS(Continue);
        EMIT(BC_Jump, l_loop);

        POST_EMIT(jump_break, BC_TrueJump, CUR_CODE_OFF);
        EMIT_JUMPS(Break);
    }
private:
    void emitCode_RangeFor(StmtNode_RangeFor *node, bool isPositive) {
        /*
            push last
            popLocal __last
            push step
            popLocal __step
            push first
            popLocal var0
loop:
            pushLocal var0
            pushLocal __last
            less // greater
            fjump break

            stmt

continue:
            pushLocal var0
            pushLocal __step
            add
            popLocal var0
            jump loop
break:
         * */
        assert(dynamic_cast<ExpNode_LocalVar*>(node->var.get()));
        int localIdx = static_cast<ExpNode_LocalVar*>(node->var.get())->localIdx;

        (ExpNodeVisitor_CodeEmitor(m_meta, node->last));
        EMIT(BC_PopLocal, node->lastLocalIdx);
        (ExpNodeVisitor_CodeEmitor(m_meta, node->step));
        EMIT(BC_PopLocal, node->stepLocalIdx);
        (ExpNodeVisitor_CodeEmitor(m_meta, node->first));
        EMIT(BC_PopLocal, localIdx);

        int l_loop = CUR_CODE_OFF;
        EMIT(BC_PushLocal, localIdx);
        EMIT(BC_PushLocal, node->lastLocalIdx);
        if (isPositive) EMIT0(BC_Less)
        else EMIT0(BC_Greater);
        int jump_break;
        PRE_EMIT(jump_break);

        if (node->stmt != NULL) {
            (StmtNodeVisitor_CodeEmitor(m_meta, node->stmt));
        }

        EMIT_JUMPS(Continue);
        EMIT(BC_PushLocal, localIdx);
        EMIT(BC_PushLocal, node->stepLocalIdx);
        EMIT0(BC_Add);
        EMIT(BC_PopLocal, localIdx);
        EMIT(BC_Jump, l_loop);

        POST_EMIT(jump_break, BC_FalseJump, CUR_CODE_OFF);
        EMIT_JUMPS(Break);
    }
private:
    vector<int> &m_codes;
    LuaFunctionMeta *m_meta;
    vector<int> m_jumpsContinue, m_jumpsReturn, m_jumpsBreak;
};

//====================
void execute(LuaStackFrame *stopFrame) {
    for (;;) {
        auto stack = LuaVM::instance()->getCurrentStack();
        auto frame = stack->topFrame();
        if (frame == stopFrame) break;
        auto lfunc = static_cast<LuaFunction*>(frame->func);
        if (frame->ip == (int)lfunc->meta->codes.size()) {
            stack->popFrame();
            continue;
        }
        int code = lfunc->meta->codes[frame->ip];
        switch (code) {
            case BC_PushLocal: ByteCodeHandler<BC_PushLocal>::execute(code, frame);
            case BC_PushUpValue: ByteCodeHandler<BC_PushUpValue>::execute(code, frame);
            case BC_PushGlobal: ByteCodeHandler<BC_PushGlobal>::execute(code, frame);
            case BC_PushConst: ByteCodeHandler<BC_PushConst>::execute(code, frame);
            case BC_PushVArgs: ByteCodeHandler<BC_PushVArgs>::execute(code, frame);
            case BC_PushNewFunction: ByteCodeHandler<BC_PushNewFunction>::execute(code, frame);
            case BC_PushNewTable: ByteCodeHandler<BC_PushNewTable>::execute(code, frame);
            case BC_PushI: ByteCodeHandler<BC_PushI>::execute(code, frame);
            case BC_PushTop: ByteCodeHandler<BC_PushTop>::execute(code, frame);
            case BC_PopLocal: ByteCodeHandler<BC_PopLocal>::execute(code, frame);
            case BC_PopUpValue: ByteCodeHandler<BC_PopUpValue>::execute(code, frame);
            case BC_PopGlobal: ByteCodeHandler<BC_PopGlobal>::execute(code, frame);
            case BC_PopN: ByteCodeHandler<BC_PopN>::execute(code, frame);
            case BC_PopTemps: ByteCodeHandler<BC_PopTemps>::execute(code, frame);
            case BC_ResizeTemp: ByteCodeHandler<BC_ResizeTemp>::execute(code, frame);
            case BC_ResizeTemp2Ext: ByteCodeHandler<BC_ResizeTemp2Ext>::execute(code, frame);
            case BC_Call: ByteCodeHandler<BC_Call>::execute(code, frame);
            case BC_Return: ByteCodeHandler<BC_Return>::execute(code, frame);
            case BC_Less: ByteCodeHandler<BC_Less>::execute(code, frame);
            case BC_LessEq: ByteCodeHandler<BC_LessEq>::execute(code, frame);
            case BC_Greater: ByteCodeHandler<BC_Greater>::execute(code, frame);
            case BC_GreaterEq: ByteCodeHandler<BC_GreaterEq>::execute(code, frame);
            case BC_Equal: ByteCodeHandler<BC_Equal>::execute(code, frame);
            case BC_NEqual: ByteCodeHandler<BC_NEqual>::execute(code, frame);
            case BC_Add: ByteCodeHandler<BC_Add>::execute(code, frame);
            case BC_Sub: ByteCodeHandler<BC_Sub>::execute(code, frame);
            case BC_Mul: ByteCodeHandler<BC_Mul>::execute(code, frame);
            case BC_Div: ByteCodeHandler<BC_Div>::execute(code, frame);
            case BC_Mod: ByteCodeHandler<BC_Mod>::execute(code, frame);
            case BC_Pow: ByteCodeHandler<BC_Pow>::execute(code, frame);
            case BC_Concat: ByteCodeHandler<BC_Concat>::execute(code, frame);
            case BC_Not: ByteCodeHandler<BC_Not>::execute(code, frame);
            case BC_Len: ByteCodeHandler<BC_Len>::execute(code, frame);
            case BC_Minus: ByteCodeHandler<BC_Minus>::execute(code, frame);
            case BC_GetTable: ByteCodeHandler<BC_GetTable>::execute(code, frame);
            case BC_SetTable: ByteCodeHandler<BC_SetTable>::execute(code, frame);
            case BC_PushAll2Table: ByteCodeHandler<BC_PushAll2Table>::execute(code, frame);
            case BC_Jump: ByteCodeHandler<BC_Jump>::execute(code, frame);
            case BC_TrueJump: ByteCodeHandler<BC_TrueJump>::execute(code, frame);
            case BC_FalseJump: ByteCodeHandler<BC_FalseJump>::execute(code, frame);
            default: ASSERT(0);
        }
    }
}

void disassemble(ostream& so, LuaFunctionMeta* meta) {
    auto &codes = meta->codes;
    for (int i = 0; i < (int)codes.size(); ++i) {
        int code = codes[i];
        so << tabString(meta->level) << format("%3d", i + 1);
        switch (code & 0xff) {
            case BC_PushLocal: ByteCodeHandler<BC_PushLocal>::disassemble(so, code, meta);
            case BC_PushUpValue: ByteCodeHandler<BC_PushUpValue>::disassemble(so, code, meta);
            case BC_PushGlobal: ByteCodeHandler<BC_PushGlobal>::disassemble(so, code, meta);
            case BC_PushConst: ByteCodeHandler<BC_PushConst>::disassemble(so, code, meta);
            case BC_PushVArgs: ByteCodeHandler<BC_PushVArgs>::disassemble(so, code, meta);
            case BC_PushNewFunction: ByteCodeHandler<BC_PushNewFunction>::disassemble(so, code, meta);
            case BC_PushNewTable: ByteCodeHandler<BC_PushNewTable>::disassemble(so, code, meta);
            case BC_PushI: ByteCodeHandler<BC_PushI>::disassemble(so, code, meta);
            case BC_PushTop: ByteCodeHandler<BC_PushTop>::disassemble(so, code, meta);
            case BC_PopLocal: ByteCodeHandler<BC_PopLocal>::disassemble(so, code, meta);
            case BC_PopUpValue: ByteCodeHandler<BC_PopUpValue>::disassemble(so, code, meta);
            case BC_PopGlobal: ByteCodeHandler<BC_PopGlobal>::disassemble(so, code, meta);
            case BC_PopN: ByteCodeHandler<BC_PopN>::disassemble(so, code, meta);
            case BC_PopTemps: ByteCodeHandler<BC_PopTemps>::disassemble(so, code, meta);
            case BC_ResizeTemp: ByteCodeHandler<BC_ResizeTemp>::disassemble(so, code, meta);
            case BC_ResizeTemp2Ext: ByteCodeHandler<BC_ResizeTemp2Ext>::disassemble(so, code, meta);
            case BC_Call: ByteCodeHandler<BC_Call>::disassemble(so, code, meta);
            case BC_Return: ByteCodeHandler<BC_Return>::disassemble(so, code, meta);
            case BC_Less: ByteCodeHandler<BC_Less>::disassemble(so, code, meta);
            case BC_LessEq: ByteCodeHandler<BC_LessEq>::disassemble(so, code, meta);
            case BC_Greater: ByteCodeHandler<BC_Greater>::disassemble(so, code, meta);
            case BC_GreaterEq: ByteCodeHandler<BC_GreaterEq>::disassemble(so, code, meta);
            case BC_Equal: ByteCodeHandler<BC_Equal>::disassemble(so, code, meta);
            case BC_NEqual: ByteCodeHandler<BC_NEqual>::disassemble(so, code, meta);
            case BC_Add: ByteCodeHandler<BC_Add>::disassemble(so, code, meta);
            case BC_Sub: ByteCodeHandler<BC_Sub>::disassemble(so, code, meta);
            case BC_Mul: ByteCodeHandler<BC_Mul>::disassemble(so, code, meta);
            case BC_Div: ByteCodeHandler<BC_Div>::disassemble(so, code, meta);
            case BC_Mod: ByteCodeHandler<BC_Mod>::disassemble(so, code, meta);
            case BC_Pow: ByteCodeHandler<BC_Pow>::disassemble(so, code, meta);
            case BC_Concat: ByteCodeHandler<BC_Concat>::disassemble(so, code, meta);
            case BC_Not: ByteCodeHandler<BC_Not>::disassemble(so, code, meta);
            case BC_Len: ByteCodeHandler<BC_Len>::disassemble(so, code, meta);
            case BC_Minus: ByteCodeHandler<BC_Minus>::disassemble(so, code, meta);
            case BC_GetTable: ByteCodeHandler<BC_GetTable>::disassemble(so, code, meta);
            case BC_SetTable: ByteCodeHandler<BC_SetTable>::disassemble(so, code, meta);
            case BC_PushAll2Table: ByteCodeHandler<BC_PushAll2Table>::disassemble(so, code, meta);
            case BC_Jump: ByteCodeHandler<BC_Jump>::disassemble(so, code, meta);
            case BC_TrueJump: ByteCodeHandler<BC_TrueJump>::disassemble(so, code, meta);
            case BC_FalseJump: ByteCodeHandler<BC_FalseJump>::disassemble(so, code, meta);
            default: ASSERT(0);
        }
        so << endl;
        if ((code & 0xff) == BC_PushNewFunction) {
            disassemble(so, LuaVM::instance()->getMeta(code >> 8).get());
        }
    }
}

void emitCode(LuaFunctionMeta* meta) {
    meta->codes.clear();
    (StmtNodeVisitor_CodeEmitor(meta, meta->ast));
}
