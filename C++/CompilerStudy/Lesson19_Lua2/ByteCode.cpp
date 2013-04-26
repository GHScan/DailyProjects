
#include "pch.h"
#include "ByteCode.h"
#include "LuaVM.h"
#include "LuaStack.h"
#include "LuaFunction.h"
#include "LuaString.h"
#include "LuaTable.h"
#include "AST.h"
#include "ByteCodeDefine.h"

#define EMIT0(code) { m_ip2line.push_back(node->line); m_codes.push_back(0); ByteCodeHandler<code>::emit(m_codes.back()); }
#define EMIT(code, ...) { m_ip2line.push_back(node->line); m_codes.push_back(0); ByteCodeHandler<code>::emit(m_codes.back(), __VA_ARGS__); }
#define PRE_EMIT(off) { EMIT0(BC_Nop); m_ip2line.push_back(node->line); off = (int)m_codes.size(); m_codes.push_back(0); }
#define POST_EMIT0(off, code) { ByteCodeHandler<code>::emit(m_codes[off]); }
#define POST_EMIT(off, code, ...) {ByteCodeHandler<code>::emit(m_codes[off], __VA_ARGS__); }
#define EMIT_PUSH_CONST(v) { m_ip2line.push_back(node->line); m_codes.push_back(0); ByteCodeHandler<BC_PushConst>::emit(m_codes.back(), m_meta->getConstIdx(v)); }
#define CUR_CODE_OFF int(m_codes.size())
#define EMIT_JUMPS(type) {\
    for (auto jump##type : m_jumps##type) {\
        POST_EMIT(jump##type, BC_Jump, CUR_CODE_OFF);\
    }\
    m_jumps##type.clear();\
}
#define EMIT_CLOSE_BLOCK_FOR_JUMPS(type, node) {\
    for (auto jump##type : m_jumps##type) {\
        int &code = m_codes[jump##type - 1];\
        if ((code & 0xff) == BC_Nop) {\
            ByteCodeHandler<BC_ExitBlock>::emit(code, node->localOff, node->localCount);\
        } else {\
            ByteCodeHandler<BC_ExitBlock>::emitOff(code, node->localOff);\
        }\
    }\
}

class LocalIdxAllocator {
public:
    LocalIdxAllocator(int localOff): m_initLocalOff(localOff), m_localOff(localOff), m_maxLocalOff(localOff) {}
    ~LocalIdxAllocator() {
        ASSERT(m_localOff == m_initLocalOff);
    }
    int alloc() {
        m_maxLocalOff = max(++m_localOff, m_maxLocalOff);
        return m_localOff;
    }
    void free(int idx) {
        ASSERT(idx == m_localOff);
        --m_localOff;
    }
    int getMaxLocalOff() const { return m_maxLocalOff; }
private:
    int m_initLocalOff, m_localOff, m_maxLocalOff;
};

class ExpNodeVisitor_CodeEmitor:
    public IExpNodeVisitor {
public:
    ExpNodeVisitor_CodeEmitor(LuaFunctionMeta* meta, const ExpNodePtr &exp, LocalIdxAllocator *idxAllocator, int varIdx = -1, bool isMulti = false):
        m_codes(meta->codes), m_ip2line(meta->ip2line), m_meta(meta), m_idxAllocator(idxAllocator), m_varIdx(varIdx), m_isAllocated(false), m_isMulti(isMulti) {
        exp->acceptVisitor(this);
    }
    ~ExpNodeVisitor_CodeEmitor() {
        if (m_isAllocated) {
            m_idxAllocator->free(m_varIdx);
        }
    }
    int getVarIdx() const {
        ASSERT(m_varIdx != -1);
        return m_varIdx;
    }
private:
    virtual void visit(ExpNode_UnaryOp *node) {
        makesureVarIdxValid();
        int nodeIdx = ExpNodeVisitor_CodeEmitor(m_meta, node->exp, m_idxAllocator).getVarIdx();
        switch (node->op) {
            case ExpNode_UnaryOp::OP_Not: EMIT(BC_Not, m_varIdx, nodeIdx); break;
            case ExpNode_UnaryOp::OP_Minus: EMIT(BC_Minus, m_varIdx, nodeIdx); break;
            case ExpNode_UnaryOp::OP_Len: EMIT(BC_Len, m_varIdx, nodeIdx); break;
            default: ASSERT(0);
        }
    }
    virtual void visit(ExpNode_BinaryOp *node) {
        makesureVarIdxValid();
        if (node->op == ExpNode_BinaryOp::OP_And || node->op == ExpNode_BinaryOp::OP_Or) {
            /*
               thisidx <- lexp
               fjump thisidx label // tjump
               thisidx <- rexp
label:
             * */
            ExpNodeVisitor_CodeEmitor(m_meta, node->lexp, m_idxAllocator, m_varIdx);
            int jump_label;
            PRE_EMIT(jump_label);
            ExpNodeVisitor_CodeEmitor(m_meta, node->rexp, m_idxAllocator, m_varIdx);
            if (node->op == ExpNode_BinaryOp::OP_And) {
                POST_EMIT(jump_label, BC_FalseJump, m_varIdx, CUR_CODE_OFF);
            } else {
                POST_EMIT(jump_label, BC_TrueJump, m_varIdx, CUR_CODE_OFF);
            }
            return;
        }

        ExpNodeVisitor_CodeEmitor lexp(m_meta, node->lexp, m_idxAllocator);
        int lIdx = lexp.getVarIdx();
        int rIdx = ExpNodeVisitor_CodeEmitor(m_meta, node->rexp, m_idxAllocator).getVarIdx();
        switch (node->op) {
            case ExpNode_BinaryOp::OP_Less: EMIT(BC_Less, m_varIdx, lIdx, rIdx); break;
            case ExpNode_BinaryOp::OP_LessEq: EMIT(BC_LessEq, m_varIdx, lIdx, rIdx); break;
            case ExpNode_BinaryOp::OP_Greater: EMIT(BC_Greater, m_varIdx, lIdx, rIdx); break;
            case ExpNode_BinaryOp::OP_GreaterEq: EMIT(BC_GreaterEq, m_varIdx, lIdx, rIdx); break;
            case ExpNode_BinaryOp::OP_Equal: EMIT(BC_Equal, m_varIdx, lIdx, rIdx); break;
            case ExpNode_BinaryOp::OP_NEqual: EMIT(BC_NEqual, m_varIdx, lIdx, rIdx); break;
            case ExpNode_BinaryOp::OP_Add: EMIT(BC_Add, m_varIdx, lIdx, rIdx); break;
            case ExpNode_BinaryOp::OP_Sub: EMIT(BC_Sub, m_varIdx, lIdx, rIdx); break;
            case ExpNode_BinaryOp::OP_Mul: EMIT(BC_Mul, m_varIdx, lIdx, rIdx); break;
            case ExpNode_BinaryOp::OP_Div: EMIT(BC_Div, m_varIdx, lIdx, rIdx); break;
            case ExpNode_BinaryOp::OP_Mod: EMIT(BC_Mod, m_varIdx, lIdx, rIdx); break;
            case ExpNode_BinaryOp::OP_Pow: EMIT(BC_Pow, m_varIdx, lIdx, rIdx); break;
            case ExpNode_BinaryOp::OP_Concat: EMIT(BC_Concat, m_varIdx, lIdx, rIdx); break;
            default: ASSERT(0);
        }
    } 
    virtual void visit(ExpNode_Const *node) {
        int nodeIdx = VarIndex::fromConst(node->constIdx).toInt();
        if (m_varIdx == -1) m_varIdx = nodeIdx;
        else EMIT(BC_Move, m_varIdx, nodeIdx);
    }
    virtual void visit(ExpNode_LocalVar *node) {
        int nodeIdx = VarIndex::fromLocal(node->localIdx).toInt();
        if (m_varIdx == -1) m_varIdx = nodeIdx;
        else EMIT(BC_Move, m_varIdx, nodeIdx);
    }
    virtual void visit(ExpNode_UpValueVar *node) {
        int nodeIdx = VarIndex::fromUpValue(node->uvIdx).toInt();
        if (m_varIdx == -1) m_varIdx = nodeIdx;
        else EMIT(BC_Move, m_varIdx, nodeIdx);
    }
    virtual void visit(ExpNode_GlobalVar *node) {
        makesureVarIdxValid();
        EMIT(BC_GetGlobal, m_varIdx, VarIndex::fromConst(node->constIdx).toInt());
    }
    virtual void visit(ExpNode_FieldAccess *node) {
        makesureVarIdxValid();
        ExpNodeVisitor_CodeEmitor tableExp(m_meta, node->table, m_idxAllocator);
        int fieldIdx = ExpNodeVisitor_CodeEmitor(m_meta, node->field, m_idxAllocator).getVarIdx();
        EMIT(BC_GetTable, m_varIdx, tableExp.getVarIdx(), fieldIdx);
    }
    virtual void visit(ExpNode_TableConstructor *node) {
        makesureVarIdxValid();
        EMIT(BC_NewTable, m_varIdx);
        for (auto &kv : node->dict) {
            ExpNodeVisitor_CodeEmitor kexp(m_meta, kv.first, m_idxAllocator);
            int vIdx = ExpNodeVisitor_CodeEmitor(m_meta, kv.second, m_idxAllocator).getVarIdx();
            EMIT(BC_SetTable, m_varIdx, kexp.getVarIdx(), vIdx);
        }
        // TODO
    }
    virtual void visit(ExpNode_Lambda *node) {
        makesureVarIdxValid();
        int metaIdx = LuaVM::instance()->getFunctionMetaIdx(node->meta);
        EMIT(BC_NewFunction, m_varIdx, metaIdx);
    }
    virtual void visit(ExpNode_Call *node) {
        if (m_isMulti) {
            ASSERT(m_varIdx == -1);
            makesureVarIdxValid();
            emitCode_Call(node, m_varIdx);
        } else {
            int varIdx = m_idxAllocator->alloc();
            emitCode_Call(node, varIdx);
            m_idxAllocator->free(varIdx);
            makesureVarIdxValid();
            if (m_varIdx != varIdx) {
                EMIT(BC_Move, m_varIdx, varIdx);
            }
        }
    }
    virtual void visit(ExpNode_Args *node) {
        if (m_isMulti) {
            ASSERT(m_varIdx == -1);
        }
        makesureVarIdxValid();
        EMIT(BC_LoadVArgs, m_varIdx, m_isMulti);
    }
private:
    void emitCode_Call(ExpNode_Call *node, int varIdx) {
        ExpNodeVisitor_CodeEmitor(m_meta, node->func, m_idxAllocator, varIdx);
        int paramIdx = 0;
        for (; paramIdx < (int)node->params.size() - 1; ++paramIdx) {
            ExpNodeVisitor_CodeEmitor(m_meta, node->params[paramIdx], m_idxAllocator, m_idxAllocator->alloc());
        }
        if (!node->params.empty()) {
            ExpNodeVisitor_CodeEmitor(m_meta, node->params.back(), m_idxAllocator, -1, true);
        }
        EMIT(BC_Call, varIdx, (int)node->params.size(), m_isMulti);
        for (int i = paramIdx - 1; i >= 0; --i) m_idxAllocator->free(varIdx + i + 1);
    }

    void makesureVarIdxValid() {
        if (m_varIdx == -1) {
            m_varIdx = m_idxAllocator->alloc();
            m_isAllocated = true;
        } 
    }
private:
    vector<int> &m_codes, &m_ip2line;
    LuaFunctionMeta *m_meta;
    LocalIdxAllocator *m_idxAllocator;
    int m_varIdx;
    bool m_isAllocated;
    bool m_isMulti;
};

class StmtNodeVisitor_CodeEmitor:
    public IStmtNodeVisitor {
public:
    StmtNodeVisitor_CodeEmitor(LuaFunctionMeta* meta, const StmtNodePtr& stmt):
        m_codes(meta->codes), m_ip2line(meta->ip2line), m_meta(meta), m_idxAllocator(meta->localCount) {
        stmt->acceptVisitor(this);
        EMIT_JUMPS(Return);
    }
private:
    virtual void visit(StmtNode_Call *node) {
        ExpNodeVisitor_CodeEmitor(m_meta, node->callExp, &m_idxAllocator);
    }
    virtual void visit(StmtNode_Assign *node) {
        for (int i = 0 ; i < (int)node->lvalues.size(); ++i) {
            if (i >= (int)node->rvalues.size()) {
                // TODO: from multi
            } else {
                bool shouldEvalMulti = i == (int)node->rvalues.size() - 1;
                if (auto localExp = dynamic_cast<ExpNode_LocalVar*>(node->lvalues[i].get())) {
                    ExpNodeVisitor_CodeEmitor(m_meta, node->rvalues[i], &m_idxAllocator, VarIndex::fromLocal(localExp->localIdx).toInt(), shouldEvalMulti);
                } else if (auto uvExp = dynamic_cast<ExpNode_UpValueVar*>(node->lvalues[i].get())) {
                    ExpNodeVisitor_CodeEmitor(m_meta, node->rvalues[i], &m_idxAllocator, VarIndex::fromUpValue(uvExp->uvIdx).toInt(), shouldEvalMulti);
                } else if (auto globalExp = dynamic_cast<ExpNode_GlobalVar*>(node->lvalues[i].get())) {
                    int vIdx = ExpNodeVisitor_CodeEmitor(m_meta, node->rvalues[i], &m_idxAllocator, -1, shouldEvalMulti).getVarIdx();
                    EMIT(BC_SetGlobal, VarIndex::fromConst(globalExp->constIdx).toInt(), vIdx);
                } else {
                    auto lexp = dynamic_cast<ExpNode_FieldAccess*>(node->lvalues[i].get());
                    ASSERT(lexp != NULL);
                    ExpNodeVisitor_CodeEmitor tableExp(m_meta, lexp->table, &m_idxAllocator, -1);
                    ExpNodeVisitor_CodeEmitor kExp(m_meta, lexp->field, &m_idxAllocator, -1);
                    int vIdx = ExpNodeVisitor_CodeEmitor(m_meta, node->rvalues[i], &m_idxAllocator, -1, shouldEvalMulti).getVarIdx();
                    EMIT(BC_SetTable, tableExp.getVarIdx(), kExp.getVarIdx(), vIdx);
                }
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
        m_jumpsContinue.push_back(jump_continue);
    }
    virtual void visit(StmtNode_Return *node) {
        // TODO
        int jump_return;
        PRE_EMIT(jump_return);
        m_jumpsReturn.push_back(jump_return);
    }
    virtual void visit(StmtNode_Block *node) {
        for (auto &stmt : node->stmts) {
            stmt->acceptVisitor(this);
        }
        EMIT_CLOSE_BLOCK_FOR_JUMPS(Break, node);
        EMIT_CLOSE_BLOCK_FOR_JUMPS(Continue, node);
        EMIT_CLOSE_BLOCK_FOR_JUMPS(Return, node);
        EMIT(BC_ExitBlock, node->localOff, node->localCount);
    }
    virtual void visit(StmtNode_IfElse *node) {
        /*
           fjump exp1 l_exp2
           stmt1
           jump l_end
l_exp2:
            ...
           fjump expn l_else
           stmtn
           jump l_end
l_else:
            stmtelse
l_end:
         * */
        vector<int> jumpsEnd;
        int jumpLast, expIdxLast;

        for (int i = 0; i < (int)node->ifExpStmts.size(); ++i) {
            if (i > 0) {
                POST_EMIT(jumpLast, BC_FalseJump, expIdxLast, CUR_CODE_OFF);
            }
            PRE_EMIT(jumpLast);
            auto& expStmt = node->ifExpStmts[i];
            expIdxLast = ExpNodeVisitor_CodeEmitor(m_meta, expStmt.first, &m_idxAllocator).getVarIdx();
            expStmt.second->acceptVisitor(this);
            int jump_end;
            PRE_EMIT(jump_end);
            jumpsEnd.push_back(jump_end);
        }

        POST_EMIT(jumpLast, BC_FalseJump, expIdxLast, CUR_CODE_OFF);
        if (node->elseStmt != NULL) {
            node->elseStmt->acceptVisitor(this);
        }

        for (int i = 0; i < (int)jumpsEnd.size(); ++i) {
            POST_EMIT(jumpsEnd[i], BC_Jump, CUR_CODE_OFF);
        }
    }
    virtual void visit(StmtNode_RangeFor *node) {
        /*
           less caseidx step 0
           tjump caseidx l_negative
           emitCode_RangeFor true
           jump l_end
l_negative:
           emitCode_RangeFor false
l_end:
         * */
        int caseIdx = m_idxAllocator.alloc();
        int stepIdx = ExpNodeVisitor_CodeEmitor(m_meta, node->step, &m_idxAllocator).getVarIdx();
        m_idxAllocator.free(caseIdx);
        int zeroIdx = VarIndex::fromConst(m_meta->getConstIdx(LuaValue(NumberType(0)))).toInt();
        EMIT(BC_Less, caseIdx, stepIdx, zeroIdx);
        int jump_negative;
        PRE_EMIT(jump_negative);

        emitCode_RangeFor(node, true);
        int jump_end;
        PRE_EMIT(jump_end);

        POST_EMIT(jump_negative, BC_TrueJump, caseIdx, CUR_CODE_OFF);
        emitCode_RangeFor(node, false);

        POST_EMIT(jump_end, BC_Jump, CUR_CODE_OFF);
    }
    virtual void visit(StmtNode_LoopFor *node) {
        /*
           initStmt
l_loop:
           fjump expidx l_break
           bodyStmt
l_continue:
           jump l_loop
l_break:
         * */
        if (node->initStmt != NULL) node->initStmt->acceptVisitor(this);
        int l_loop = CUR_CODE_OFF;
        int expIdx = ExpNodeVisitor_CodeEmitor(m_meta, node->exp, &m_idxAllocator).getVarIdx();
        int jump_break;
        PRE_EMIT(jump_break);

        if (node->bodyStmt != NULL) node->bodyStmt->acceptVisitor(this);

        EMIT_JUMPS(Continue);
        EMIT(BC_Jump, l_loop);

        POST_EMIT(jump_break, BC_FalseJump, expIdx, CUR_CODE_OFF);
        EMIT_JUMPS(Break);
    }
    virtual void visit(StmtNode_IteratorFor *node) {
        // TODO
    }
private:
    void emitCode_RangeFor(StmtNode_RangeFor *node, bool isPositive) {
        /*
           lastidx <- last
           stepidx <- step
           varidx <- first
l_loop:
           lessEq tempidx varidx lastidx // greaterEq
           fjump tempidx l_break
           stmt
l_continue:
           add varidx varidx stepidx
           jump l_loop
l_break:
         * */
        ExpNodeVisitor_CodeEmitor lastExp(m_meta, node->last, &m_idxAllocator);
        ExpNodeVisitor_CodeEmitor stepExp(m_meta, node->last, &m_idxAllocator);
        int varIdx = VarIndex::fromLocal(static_cast<ExpNode_LocalVar*>(node->var.get())->localIdx).toInt();
        ExpNodeVisitor_CodeEmitor(m_meta, node->first, &m_idxAllocator, varIdx);

        int l_loop = CUR_CODE_OFF;
        int tempIdx = m_idxAllocator.alloc();
        if (isPositive) {
            EMIT(BC_LessEq, tempIdx, varIdx, lastExp.getVarIdx());
        } else { 
            EMIT(BC_GreaterEq, tempIdx, varIdx, lastExp.getVarIdx());
        }
        m_idxAllocator.free(tempIdx);
        int jump_break;
        PRE_EMIT(jump_break);

        if (node->stmt != NULL) node->stmt->acceptVisitor(this);

        EMIT_JUMPS(Continue);
        EMIT(BC_Add, varIdx, varIdx, stepExp.getVarIdx());
        EMIT(BC_Jump, l_loop);

        POST_EMIT(jump_break, BC_FalseJump, tempIdx, CUR_CODE_OFF);
        EMIT_JUMPS(Break);
    }
private:
    vector<int> &m_codes, &m_ip2line;
    LuaFunctionMeta *m_meta;
    LocalIdxAllocator m_idxAllocator;
    vector<int> m_jumpsBreak, m_jumpsContinue, m_jumpsReturn;
};

//====================
static void return2PrevFrame(LuaStack* stack, LuaStackFrame* frame, bool isMulti) {
    auto lastFrame = stack->topFrame(-1);
    auto meta = frame->func->meta;
    LuaValue *destPtr = frame->varParamPtr - 1;
    if (!isMulti) frame->retN = min(frame->retN, 1);
    if (frame->retN == 0) {
        *destPtr = LuaValue::NIL;
    } else {
        LuaValue *srcPtr = frame->localPtr + meta->localCount;
        for (int i = 0; i < frame->retN; ++i) *destPtr++ = *srcPtr;
    }
    // FIXME: while call from lua ?
    lastFrame->setExtCount(frame->retN);
    stack->popFrame();
}
void execute(LuaStackFrame *stopFrame, bool isMulti) {
    auto stack = LuaVM::instance()->getCurrentStack();
    for (;;) {
        auto frame = stack->topFrame();
        if (frame == stopFrame) break;
        auto &codes = static_cast<LuaFunction*>(frame->func)->meta->codes;
        if (frame->ip == (int)codes.size()) {
            return2PrevFrame(stack, frame, isMulti);
            continue;
        }
        try {
            int code = codes[frame->ip];
            switch (code & 0xff) {
                // TODO
                case BC_Nop: ByteCodeHandler<BC_Nop>::execute(code, frame); break;
                default: ASSERT(0);
            }
            ++frame->ip;
        } catch(Exception& e) {
            for (; frame != stopFrame; frame = stack->topFrame()) {
                auto lfunc = static_cast<LuaFunction*>(frame->func);
                e.addLine(format("%s(%d):", lfunc->meta->fileName.c_str(), lfunc->meta->ip2line[frame->ip]));
                return2PrevFrame(stack, frame, isMulti);
            }
            throw;
        }
    }
}

void disassemble(ostream& so, LuaFunctionMeta* meta) {
    auto &codes = meta->codes;
    for (int i = 0; i < (int)codes.size(); ++i) {
        int code = codes[i];
        so << tabString(meta->level) << format("%3d : ", i + 1);
        switch (code & 0xff) {
            case BC_Nop: ByteCodeHandler<BC_Nop>::disassemble(so, code, meta); break;
            default: ASSERT(0);
        }
        so << endl;
    }
}

void emitCode(LuaFunctionMeta* meta) {
    meta->codes.clear();
    meta->ip2line.clear();
    (StmtNodeVisitor_CodeEmitor(meta, meta->ast));
    assert(meta->codes.size() == meta->ip2line.size());
}
