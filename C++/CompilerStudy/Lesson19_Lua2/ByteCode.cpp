
#include "pch.h"
#include "ByteCode.h"
#include "LuaVM.h"
#include "LuaStack.h"
#include "LuaFunction.h"
#include "LuaString.h"
#include "LuaTable.h"
#include "ByteCodeDefine.h"
#include "AST.h"

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
    LocalIdxAllocator(int localOff): m_localOff(localOff) {
    }
    ~LocalIdxAllocator() {
        for (auto b : m_idxsUsed) ASSERT(!b);
    }
    int alloc() {
        int i = 0;
        while (i < (int)m_idxsUsed.size() && m_idxsUsed[i]) ++i;
        if (i == (int)m_idxsUsed.size()) m_idxsUsed.push_back(false);
        m_idxsUsed[i] = true;
        return VarIndex::fromLocal(m_localOff + i).toInt();
    }
    void free(int idx) {
        m_idxsUsed[VarIndex(idx).getLocalIdx() - m_localOff] = false;
    }
    int allocMax() {
        if (m_idxsUsed.empty() || m_idxsUsed.back()) {
            m_idxsUsed.push_back(false);
        }
        int i = (int)m_idxsUsed.size() - 1;
        while (!m_idxsUsed[i]) --i;
        ++i;
        m_idxsUsed[i] = true;
        return VarIndex::fromLocal(m_localOff + i).toInt();
    }
    int getMaxLocalOff() const { return (int)m_idxsUsed.size() + m_localOff; }
private:
    int m_localOff;
    vector<bool> m_idxsUsed;
};

class ExpNodeVisitor_CodeEmitor:
    public IExpNodeVisitor {
public:
    ExpNodeVisitor_CodeEmitor(LuaFunctionMeta* meta, const ExpNodePtr &exp, LocalIdxAllocator *idxAllocator, int varIdx = - 1, bool isMultiValue = false):
        m_codes(meta->codes), m_ip2line(meta->ip2line), m_meta(meta), m_idxAllocator(idxAllocator), m_varIdx(varIdx), m_isAllocated(false), m_isMultiValue(isMultiValue) {
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
        int nodeIdx = ExpNodeVisitor_CodeEmitor(m_meta, node->exp, m_idxAllocator).getVarIdx();
        makesureVarIdxValid();
        switch (node->op) {
            case ExpNode_UnaryOp::OP_Not: EMIT(BC_Not, m_varIdx, nodeIdx); break;
            case ExpNode_UnaryOp::OP_Minus: EMIT(BC_Minus, m_varIdx, nodeIdx); break;
            case ExpNode_UnaryOp::OP_Len: EMIT(BC_Len, m_varIdx, nodeIdx); break;
            default: ASSERT(0);
        }
    }
    virtual void visit(ExpNode_BinaryOp *node) {
        ExpNodeVisitor_CodeEmitor lexp(m_meta, node->lexp, m_idxAllocator);
        int lIdx = lexp.getVarIdx();
        if (node->op == ExpNode_BinaryOp::OP_And || node->op == ExpNode_BinaryOp::OP_Or) {
            // TODO
            /*
               fjump lexp
             * */
            return;
        }

        int rIdx = ExpNodeVisitor_CodeEmitor(m_meta, node->rexp, m_idxAllocator).getVarIdx();
        makesureVarIdxValid();
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
        ExpNodeVisitor_CodeEmitor tableExp(m_meta, node->table, m_idxAllocator);
        int fieldIdx = ExpNodeVisitor_CodeEmitor(m_meta, node->field, m_idxAllocator).getVarIdx();
        makesureVarIdxValid();
        EMIT(BC_GetTable, m_varIdx, tableExp.getVarIdx(), fieldIdx);
    }
    virtual void visit(ExpNode_TableConstructor *node) {
        // TODO
    }
    virtual void visit(ExpNode_Lambda *node) {
        makesureVarIdxValid();
        int metaIdx = LuaVM::instance()->getFunctionMetaIdx(node->meta);
        EMIT(BC_NewFunction, m_varIdx, metaIdx);
    }
    virtual void visit(ExpNode_Call *node) {
        if (m_isMultiValue) {
            ASSERT(m_varIdx == -1);
            makesureVarIdxValid(true);
            emitCode_Call(node, m_varIdx);
        } else {
            int varIdx = m_idxAllocator->allocMax();
            emitCode_Call(node, varIdx);
            m_idxAllocator->free(varIdx);
            makesureVarIdxValid(false);
            if (m_varIdx != varIdx) {
                EMIT(BC_Move, m_varIdx, varIdx);
            }
        }
    }
    virtual void visit(ExpNode_Args *node) {
        if (m_isMultiValue) {
            ASSERT(m_varIdx == -1);
            makesureVarIdxValid(true);
        } else {
            makesureVarIdxValid(false);
        }
        EMIT(BC_LoadVArgs, m_varIdx, m_isMultiValue);
    }
private:
    void emitCode_Call(ExpNode_Call *node, int varIdx) {
        ExpNodeVisitor_CodeEmitor(m_meta, node->func, m_idxAllocator, varIdx);
        int paramIdx = 0;
        for (; paramIdx < (int)node->params.size() - 1; ++paramIdx) {
            ExpNodeVisitor_CodeEmitor(m_meta, node->params[paramIdx], m_idxAllocator, m_idxAllocator->allocMax());
        }
        if (!node->params.empty()) {
            ExpNodeVisitor_CodeEmitor(m_meta, node->params.back(), m_idxAllocator, -1, true);
        }
        EMIT(BC_Call, varIdx, (int)node->params.size(), m_isMultiValue);
        for (int i = 0; i < paramIdx; ++i) m_idxAllocator->free(m_varIdx + i + 1);
    }

    void makesureVarIdxValid(bool isMax = false) {
        if (m_varIdx == -1) {
            m_varIdx = isMax ? m_idxAllocator->allocMax() : m_idxAllocator->alloc();
            m_isAllocated = true;
        } 
    }
private:
    vector<int> &m_codes, &m_ip2line;
    LuaFunctionMeta *m_meta;
    LocalIdxAllocator *m_idxAllocator;
    int m_varIdx;
    bool m_isAllocated;
    bool m_isMultiValue;
};

class StmtNodeVisitor_CodeEmitor:
    public IStmtNodeVisitor {
public:
    StmtNodeVisitor_CodeEmitor(LuaFunctionMeta* meta, const StmtNodePtr& stmt):
        m_codes(meta->codes), m_ip2line(meta->ip2line), m_meta(meta) {
        stmt->acceptVisitor(this);
    }
private:
    virtual void visit(StmtNode_Call *node) {
    }
    virtual void visit(StmtNode_Assign *node) {
    }
    virtual void visit(StmtNode_Break *node) {
    }
    virtual void visit(StmtNode_Continue *node) {
    }
    virtual void visit(StmtNode_Return *node) {
    }
    virtual void visit(StmtNode_Block *node) {
    }
    virtual void visit(StmtNode_IfElse *node) {
    }
    virtual void visit(StmtNode_RangeFor *node) {
    }
    virtual void visit(StmtNode_LoopFor *node) {
    }
    virtual void visit(StmtNode_IteratorFor *node) {
    }
private:
    vector<int> &m_codes, &m_ip2line;
    LuaFunctionMeta *m_meta;
};

//====================
static void return2PrevFrame(LuaStack* stack, LuaStackFrame* frame) {
    // TODO: optimize
    auto lastFrame = stack->topFrame(-1);
    auto meta = static_cast<LuaFunction*>(frame->func)->meta;
    vector<LuaValue> rets;
    if (frame->tempCount > 0) rets.assign(&frame->temp(0), &frame->temp(0) + frame->tempExtCount);
    lastFrame->popTemps(frame->varParamBase - meta->argCount - 1 - lastFrame->tempBase);
    if (rets.empty()) lastFrame->pushTemp(LuaValue::NIL);
    else {
        lastFrame->pushTemp(rets[0]);
        for (int i = 1; i < (int)rets.size(); ++i) {
            lastFrame->pushExtTemp(rets[i]);
        }
    }
    stack->popFrame();
}
void execute(LuaStackFrame *stopFrame) {
    auto stack = LuaVM::instance()->getCurrentStack();
    for (;;) {
        auto frame = stack->topFrame();
        if (frame == stopFrame) break;
        auto &codes = static_cast<LuaFunction*>(frame->func)->meta->codes;
        if (frame->ip == (int)codes.size()) {
            return2PrevFrame(stack, frame);
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
                frame->popTemps(0);
                return2PrevFrame(stack, frame);
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
