
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
    int allocIdx() {
        int localOff = m_localOff;
        m_maxLocalOff = max(++m_localOff, m_maxLocalOff);
        return VarIndex::fromLocal(localOff).toInt();
    }
    void freeIdx(int idx) {
        ASSERT(VarIndex(idx).getLocalIdx() == m_localOff - 1);
        --m_localOff;
    }
    int getMaxLocalOff() const { return m_maxLocalOff; }
    int getLocalOff() const { return m_localOff; }
    int getNextIdx() const {
        return VarIndex::fromLocal(m_localOff).toInt();
    }
    vector<int> allocIdxList(int n) {
        vector<int> r;
        for (int i = 0; i < n; ++i) r.push_back(allocIdx());
        return r;
    }
    void freeIdxList(vector<int>& idxs) {
        while (!idxs.empty()) {
            freeIdx(idxs.back());
            idxs.pop_back();
        }
    }
private:
    int m_initLocalOff, m_localOff, m_maxLocalOff;
};

static void multiEval(LuaFunctionMeta *meta, LocalIdxAllocator *idxAllocator, const vector<ExpNodePtr>& exps);

class ExpNodeVisitor_CodeEmitor:
    public IExpNodeVisitor {
public:
    ExpNodeVisitor_CodeEmitor(LuaFunctionMeta* meta, const ExpNodePtr &node, LocalIdxAllocator *idxAllocator, int varIdx = -1, int requireRetN = 1):
        m_codes(meta->codes), m_ip2line(meta->ip2line), m_meta(meta), m_idxAllocator(idxAllocator), m_varIdx(varIdx), m_isAllocated(false), m_requireRetN(requireRetN) {
        node->acceptVisitor(this);

        if (m_requireRetN > 1) {
            ASSERT(varIdx != -1);
            int localIdx = VarIndex(varIdx).getLocalIdx();
            for (int i = 1; i < m_requireRetN; ++i) {
                EMIT(BC_Move, VarIndex::fromLocal(localIdx + i).toInt(), VarIndex::fromConst(m_meta->getConstIdx(LuaValue::NIL)).toInt());
            }
        }
    }
    ~ExpNodeVisitor_CodeEmitor() {
        if (m_isAllocated) {
            m_idxAllocator->freeIdx(m_varIdx);
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
        if (node->array.empty()) return;
        multiEval(m_meta, m_idxAllocator, node->array);
        EMIT(BC_PushValues2Table, m_varIdx, m_idxAllocator->getNextIdx(), (int)node->array.size());
    }
    virtual void visit(ExpNode_Lambda *node) {
        makesureVarIdxValid();
        int metaIdx = LuaVM::instance()->getFunctionMetaIdx(node->meta);
        EMIT(BC_NewFunction, m_varIdx, metaIdx);
    }
    virtual void visit(ExpNode_Call *node) {
        if (m_requireRetN != 1) {
            ASSERT(m_varIdx != -1);
            emitCode_Call(node, m_varIdx);
        } else {
            int varIdx = m_idxAllocator->allocIdx();
            emitCode_Call(node, varIdx);
            m_idxAllocator->freeIdx(varIdx);
            makesureVarIdxValid();
            if (m_varIdx != varIdx) {
                EMIT(BC_Move, m_varIdx, varIdx);
            }
        }
        m_requireRetN = 1;
    }
    virtual void visit(ExpNode_Args *node) {
        if (m_requireRetN != 1) {
            ASSERT(m_varIdx != -1);
        }
        makesureVarIdxValid();
        EMIT(BC_LoadVArgs, m_varIdx, m_requireRetN);
        m_requireRetN = 1;
    }
private:
    void emitCode_Call(ExpNode_Call *node, int varIdx) {
        ExpNodeVisitor_CodeEmitor(m_meta, node->func, m_idxAllocator, varIdx);
        multiEval(m_meta, m_idxAllocator, node->params);
        EMIT(BC_Call, varIdx, (int)node->params.size(), m_requireRetN);
    }

    void makesureVarIdxValid() {
        if (m_varIdx == -1) {
            m_varIdx = m_idxAllocator->allocIdx();
            m_isAllocated = true;
        } 
    }
private:
    vector<int> &m_codes, &m_ip2line;
    LuaFunctionMeta *m_meta;
    LocalIdxAllocator *m_idxAllocator;
    int m_varIdx;
    bool m_isAllocated;
    int m_requireRetN;
};

static void fixedMultiEval(LuaFunctionMeta *meta, LocalIdxAllocator *idxAllocator, int n, const vector<ExpNodePtr>& exps) {
    ASSERT(n >= (int)exps.size());
    vector<int> varIdxs(idxAllocator->allocIdxList((int)exps.size()));
    for (int i = 0; i < (int)exps.size(); ++i) {
        ExpNodeVisitor_CodeEmitor(meta, exps[i], idxAllocator, varIdxs[i], i == (int)exps.size() - 1 ? (1 + n - (int)exps.size()) : 1);
    }
    idxAllocator->freeIdxList(varIdxs);
}
static void multiEval(LuaFunctionMeta *meta, LocalIdxAllocator *idxAllocator, const vector<ExpNodePtr>& exps) {
    vector<int> varIdxs(idxAllocator->allocIdxList((int)exps.size()));
    for (int i = 0; i < (int)exps.size(); ++i) {
        ExpNodeVisitor_CodeEmitor(meta, exps[i], idxAllocator, varIdxs[i], i == (int)exps.size() - 1 ? 0 : 1);
    }
    idxAllocator->freeIdxList(varIdxs);
}

class StmtNodeVisitor_CodeEmitor:
    public IStmtNodeVisitor {
public:
    StmtNodeVisitor_CodeEmitor(LuaFunctionMeta* meta, const StmtNodePtr& stmt):
        m_codes(meta->codes), m_ip2line(meta->ip2line), m_meta(meta), m_idxAllocator(meta->localCount) {
        stmt->acceptVisitor(this);
        EMIT_JUMPS(Return);
        m_meta->tempCount = m_idxAllocator.getMaxLocalOff() - m_meta->localCount;
    }
private:
    virtual void visit(StmtNode_Call *node) {
        ExpNodeVisitor_CodeEmitor(m_meta, node->callExp, &m_idxAllocator);
    }
    virtual void visit(StmtNode_Assign *node) {
        ASSERT(!node->rvalues.empty() && !node->lvalues.empty());

        if (node->lvalues.size() == 1) {
            if (auto localExp = dynamic_cast<ExpNode_LocalVar*>(node->lvalues.front().get())) {
                ExpNodeVisitor_CodeEmitor(m_meta, node->rvalues.front(), &m_idxAllocator, VarIndex::fromLocal(localExp->localIdx).toInt());
            } else if (auto uvExp = dynamic_cast<ExpNode_UpValueVar*>(node->lvalues.front().get())) {
                ExpNodeVisitor_CodeEmitor(m_meta, node->rvalues.front(), &m_idxAllocator, VarIndex::fromUpValue(uvExp->uvIdx).toInt());
            } else if (auto globalExp = dynamic_cast<ExpNode_GlobalVar*>(node->lvalues.front().get())) {
                int vIdx = ExpNodeVisitor_CodeEmitor(m_meta, node->rvalues.front(), &m_idxAllocator).getVarIdx();
                EMIT(BC_SetGlobal, VarIndex::fromConst(globalExp->constIdx).toInt(), vIdx);
            } else {
                auto lexp = dynamic_cast<ExpNode_FieldAccess*>(node->lvalues.front().get());
                ASSERT(lexp != NULL);
                ExpNodeVisitor_CodeEmitor tableExp(m_meta, lexp->table, &m_idxAllocator);
                ExpNodeVisitor_CodeEmitor kExp(m_meta, lexp->field, &m_idxAllocator);
                int vIdx = ExpNodeVisitor_CodeEmitor(m_meta, node->rvalues.front(), &m_idxAllocator).getVarIdx();
                EMIT(BC_SetTable, tableExp.getVarIdx(), kExp.getVarIdx(), vIdx);
            }
            return;
        }

        fixedMultiEval(m_meta, &m_idxAllocator, (int)node->lvalues.size(), node->rvalues);
        EMIT(BC_SetExtCount, -1);

        vector<int>varIdxs = m_idxAllocator.allocIdxList((int)node->lvalues.size());
        for (int i = 0; i < (int)node->lvalues.size(); ++i) {
            if (auto localExp = dynamic_cast<ExpNode_LocalVar*>(node->lvalues[i].get())) {
                EMIT(BC_Move, VarIndex::fromLocal(localExp->localIdx).toInt(), varIdxs[i]);
            } else if (auto uvExp = dynamic_cast<ExpNode_UpValueVar*>(node->lvalues[i].get())) {
                EMIT(BC_Move, VarIndex::fromUpValue(uvExp->uvIdx).toInt(), varIdxs[i]);
            } else if (auto globalExp = dynamic_cast<ExpNode_GlobalVar*>(node->lvalues[i].get())) {
                EMIT(BC_SetGlobal, VarIndex::fromConst(globalExp->constIdx).toInt(), varIdxs[i]);
            } else {
                auto lexp = dynamic_cast<ExpNode_FieldAccess*>(node->lvalues[i].get());
                ASSERT(lexp != NULL);
                ExpNodeVisitor_CodeEmitor tableExp(m_meta, lexp->table, &m_idxAllocator);
                ExpNodeVisitor_CodeEmitor kExp(m_meta, lexp->field, &m_idxAllocator);
                EMIT(BC_SetTable, tableExp.getVarIdx(), kExp.getVarIdx(), varIdxs[i]);
            }
        }
        m_idxAllocator.freeIdxList(varIdxs);
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
        multiEval(m_meta, &m_idxAllocator, node->exps);
        EMIT(BC_ReturnN, (int)node->exps.size());

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
        int jumpLast = -1, expIdxLast = -1;

        for (int i = 0; i < (int)node->ifExpStmts.size(); ++i) {
            if (i > 0) {
                POST_EMIT(jumpLast, BC_FalseJump, expIdxLast, CUR_CODE_OFF);
            }
            auto& expStmt = node->ifExpStmts[i];
            expIdxLast = ExpNodeVisitor_CodeEmitor(m_meta, expStmt.first, &m_idxAllocator).getVarIdx();
            PRE_EMIT(jumpLast);
            if (expStmt.second != NULL) expStmt.second->acceptVisitor(this);
            int jump_end;
            PRE_EMIT(jump_end);
            jumpsEnd.push_back(jump_end);
        }

        ASSERT(jumpLast != -1 && expIdxLast != -1);
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
           var, lastIdx, stepIdx <- first, last, step
           less caseidx stepidx 0
           tjump caseidx l_negative
           emitCode_RangeFor true
           jump l_end
l_negative:
           emitCode_RangeFor false
l_end:
         * */
        ExpNodeVisitor_CodeEmitor(m_meta, node->first, &m_idxAllocator, 
                VarIndex::fromLocal(static_cast<ExpNode_LocalVar*>(node->var.get())->localIdx).toInt());
        ExpNodeVisitor_CodeEmitor(m_meta, node->last, &m_idxAllocator, VarIndex::fromLocal(node->lastLocalIdx).toInt());
        ExpNodeVisitor_CodeEmitor(m_meta, node->step, &m_idxAllocator, VarIndex::fromLocal(node->stepLocalIdx).toInt());

        int caseIdx = m_idxAllocator.getNextIdx();
        EMIT(BC_Less, caseIdx, 
                VarIndex::fromLocal(node->stepLocalIdx).toInt(), 
                VarIndex::fromConst(m_meta->getConstIdx(LuaValue(NumberType(0)))).toInt());
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
        /*
           move 3 nil
           eval iterExps
           func, state, k = 0, 1, 2
l_loop:
           move n nil
           call func state k
           equal tempidx k nil
           tjump tempidx l_break

           stmt
l_continue:
           jump l_loop
l_break:

         * */
        ASSERT(!node->vars.empty());
        vector<int> localIdxs;
        for (auto &exp : node->vars) {
            localIdxs.push_back(static_cast<ExpNode_LocalVar*>(exp.get())->localIdx);
        }

        fixedMultiEval(m_meta, &m_idxAllocator, 3, node->iterExps);
        EMIT(BC_SetExtCount, -1);

        vector<int> varIdxs(m_idxAllocator.allocIdxList(3));
        EMIT(BC_Move, VarIndex::fromLocal(node->funcLocalIdx).toInt(), varIdxs[0]);
        EMIT(BC_Move, VarIndex::fromLocal(node->stateLocalIdx).toInt(), varIdxs[1]);
        EMIT(BC_Move, VarIndex::fromLocal(localIdxs[0]).toInt(), varIdxs[2]);
        m_idxAllocator.freeIdxList(varIdxs);

        // 
        int l_loop = CUR_CODE_OFF;

        varIdxs = m_idxAllocator.allocIdxList(3);
        EMIT(BC_Move, varIdxs[0], VarIndex::fromLocal(node->funcLocalIdx).toInt());
        EMIT(BC_Move, varIdxs[1], VarIndex::fromLocal(node->stateLocalIdx).toInt());
        EMIT(BC_Move, varIdxs[2], VarIndex::fromLocal(localIdxs[0]).toInt());
        EMIT(BC_Call, varIdxs[0], 2, (int)localIdxs.size());
        EMIT(BC_SetExtCount, -1);
        m_idxAllocator.freeIdxList(varIdxs);

        varIdxs = m_idxAllocator.allocIdxList((int)localIdxs.size());
        for (int i = 0; i < (int)localIdxs.size(); ++i) {
            EMIT(BC_Move, VarIndex::fromLocal(localIdxs[i]).toInt(), varIdxs[i]);
        }
        m_idxAllocator.freeIdxList(varIdxs);

        int tempidx = m_idxAllocator.getNextIdx();
        EMIT(BC_Equal, tempidx, 
                VarIndex::fromLocal(localIdxs[0]).toInt(), 
                VarIndex::fromConst(m_meta->getConstIdx(LuaValue::NIL)).toInt());
        int jump_break;
        PRE_EMIT(jump_break);

        if (node->stmt != NULL) node->stmt->acceptVisitor(this);

        EMIT_JUMPS(Continue);
        EMIT(BC_Jump, l_loop);

        POST_EMIT(jump_break, BC_TrueJump, tempidx, CUR_CODE_OFF);
        EMIT_JUMPS(Break);
    }
private:
    void emitCode_RangeFor(StmtNode_RangeFor *node, bool isPositive) {
        /*
l_loop:
           lessEq tempidx varidx lastidx // greaterEq
           fjump tempidx l_break

           stmt
l_continue:
           add varidx varidx stepidx
           jump l_loop
l_break:
         * */
        int varIdx = VarIndex::fromLocal(static_cast<ExpNode_LocalVar*>(node->var.get())->localIdx).toInt();

        int l_loop = CUR_CODE_OFF;
        int tempIdx = m_idxAllocator.getNextIdx();
        if (isPositive) {
            EMIT(BC_LessEq, tempIdx, varIdx, VarIndex::fromLocal(node->lastLocalIdx).toInt());
        } else { 
            EMIT(BC_GreaterEq, tempIdx, varIdx, VarIndex::fromLocal(node->lastLocalIdx).toInt());
        }
        int jump_break;
        PRE_EMIT(jump_break);

        if (node->stmt != NULL) node->stmt->acceptVisitor(this);

        EMIT_JUMPS(Continue);
        EMIT(BC_Add, varIdx, varIdx, VarIndex::fromLocal(node->stepLocalIdx).toInt());
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
static void return2PrevFrame(LuaStack* stack, LuaStackFrame* frame) {
    auto lastFrame = stack->topFrame(-1);
    auto meta = frame->func->meta;
    LuaValue *destPtr = frame->varParamPtr - 1;
    LuaValue *srcPtr = frame->localPtr + meta->localCount;
    int n = frame->requireRetN > 0 ? frame->requireRetN : max(frame->retN, 1);
    stack->reserveValueSpace(int(destPtr - &stack->values()[0]) + n);
    for (int i = 0; i < n; ++i) {
        if (i < frame->retN) *destPtr++ = *srcPtr++;
        else *destPtr++ = LuaValue::NIL;
    }
    lastFrame->setExtCount(n);
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
                case BC_Move: ByteCodeHandler<BC_Move>::execute(code, frame); break;
                case BC_LoadVArgs: ByteCodeHandler<BC_LoadVArgs>::execute(code, frame); break;
                case BC_GetGlobal: ByteCodeHandler<BC_GetGlobal>::execute(code, frame); break;
                case BC_SetGlobal: ByteCodeHandler<BC_SetGlobal>::execute(code, frame); break;
                case BC_NewFunction: ByteCodeHandler<BC_NewFunction>::execute(code, frame); break;
                case BC_NewTable: ByteCodeHandler<BC_NewTable>::execute(code, frame); break;
                case BC_Call: ByteCodeHandler<BC_Call>::execute(code, frame); break;
                case BC_ExitBlock: ByteCodeHandler<BC_ExitBlock>::execute(code, frame); break;
                case BC_Less: ByteCodeHandler<BC_Less>::execute(code, frame); break;
                case BC_LessEq: ByteCodeHandler<BC_LessEq>::execute(code, frame); break;
                case BC_Greater: ByteCodeHandler<BC_Greater>::execute(code, frame); break;
                case BC_GreaterEq: ByteCodeHandler<BC_GreaterEq>::execute(code, frame); break;
                case BC_Equal: ByteCodeHandler<BC_Equal>::execute(code, frame); break;
                case BC_NEqual: ByteCodeHandler<BC_NEqual>::execute(code, frame); break;
                case BC_Add: ByteCodeHandler<BC_Add>::execute(code, frame); break;
                case BC_Sub: ByteCodeHandler<BC_Sub>::execute(code, frame); break;
                case BC_Mul: ByteCodeHandler<BC_Mul>::execute(code, frame); break;
                case BC_Div: ByteCodeHandler<BC_Div>::execute(code, frame); break;
                case BC_Mod: ByteCodeHandler<BC_Mod>::execute(code, frame); break;
                case BC_Pow: ByteCodeHandler<BC_Pow>::execute(code, frame); break;
                case BC_Concat: ByteCodeHandler<BC_Concat>::execute(code, frame); break;
                case BC_Not: ByteCodeHandler<BC_Not>::execute(code, frame); break;
                case BC_Len: ByteCodeHandler<BC_Len>::execute(code, frame); break;
                case BC_Minus: ByteCodeHandler<BC_Minus>::execute(code, frame); break;
                case BC_GetTable: ByteCodeHandler<BC_GetTable>::execute(code, frame); break;
                case BC_SetTable: ByteCodeHandler<BC_SetTable>::execute(code, frame); break;
                case BC_Jump: ByteCodeHandler<BC_Jump>::execute(code, frame); break;
                case BC_TrueJump: ByteCodeHandler<BC_TrueJump>::execute(code, frame); break;
                case BC_FalseJump: ByteCodeHandler<BC_FalseJump>::execute(code, frame); break;
                case BC_Nop: ByteCodeHandler<BC_Nop>::execute(code, frame); break;
                case BC_ReturnN: ByteCodeHandler<BC_ReturnN>::execute(code, frame); break;
                case BC_PushValues2Table: ByteCodeHandler<BC_PushValues2Table>::execute(code, frame); break;
                case BC_SetExtCount: ByteCodeHandler<BC_SetExtCount>::execute(code, frame); break;
                default: ASSERT(0);
            }
            ++frame->ip;
        } catch(Exception& e) {
            for (; frame != stopFrame; frame = stack->topFrame()) {
                auto lfunc = static_cast<LuaFunction*>(frame->func);
                e.addLine(format("%s(%d):", lfunc->meta->fileName.c_str(), lfunc->meta->ip2line[frame->ip]));
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
            case BC_Move: ByteCodeHandler<BC_Move>::disassemble(so, code, meta); break;
            case BC_LoadVArgs: ByteCodeHandler<BC_LoadVArgs>::disassemble(so, code, meta); break;
            case BC_GetGlobal: ByteCodeHandler<BC_GetGlobal>::disassemble(so, code, meta); break;
            case BC_SetGlobal: ByteCodeHandler<BC_SetGlobal>::disassemble(so, code, meta); break;
            case BC_NewFunction: ByteCodeHandler<BC_NewFunction>::disassemble(so, code, meta); break;
            case BC_NewTable: ByteCodeHandler<BC_NewTable>::disassemble(so, code, meta); break;
            case BC_Call: ByteCodeHandler<BC_Call>::disassemble(so, code, meta); break;
            case BC_ExitBlock: ByteCodeHandler<BC_ExitBlock>::disassemble(so, code, meta); break;
            case BC_Less: ByteCodeHandler<BC_Less>::disassemble(so, code, meta); break;
            case BC_LessEq: ByteCodeHandler<BC_LessEq>::disassemble(so, code, meta); break;
            case BC_Greater: ByteCodeHandler<BC_Greater>::disassemble(so, code, meta); break;
            case BC_GreaterEq: ByteCodeHandler<BC_GreaterEq>::disassemble(so, code, meta); break;
            case BC_Equal: ByteCodeHandler<BC_Equal>::disassemble(so, code, meta); break;
            case BC_NEqual: ByteCodeHandler<BC_NEqual>::disassemble(so, code, meta); break;
            case BC_Add: ByteCodeHandler<BC_Add>::disassemble(so, code, meta); break;
            case BC_Sub: ByteCodeHandler<BC_Sub>::disassemble(so, code, meta); break;
            case BC_Mul: ByteCodeHandler<BC_Mul>::disassemble(so, code, meta); break;
            case BC_Div: ByteCodeHandler<BC_Div>::disassemble(so, code, meta); break;
            case BC_Mod: ByteCodeHandler<BC_Mod>::disassemble(so, code, meta); break;
            case BC_Pow: ByteCodeHandler<BC_Pow>::disassemble(so, code, meta); break;
            case BC_Concat: ByteCodeHandler<BC_Concat>::disassemble(so, code, meta); break;
            case BC_Not: ByteCodeHandler<BC_Not>::disassemble(so, code, meta); break;
            case BC_Len: ByteCodeHandler<BC_Len>::disassemble(so, code, meta); break;
            case BC_Minus: ByteCodeHandler<BC_Minus>::disassemble(so, code, meta); break;
            case BC_GetTable: ByteCodeHandler<BC_GetTable>::disassemble(so, code, meta); break;
            case BC_SetTable: ByteCodeHandler<BC_SetTable>::disassemble(so, code, meta); break;
            case BC_Jump: ByteCodeHandler<BC_Jump>::disassemble(so, code, meta); break;
            case BC_TrueJump: ByteCodeHandler<BC_TrueJump>::disassemble(so, code, meta); break;
            case BC_FalseJump: ByteCodeHandler<BC_FalseJump>::disassemble(so, code, meta); break;
            case BC_Nop: ByteCodeHandler<BC_Nop>::disassemble(so, code, meta); break;
            case BC_ReturnN: ByteCodeHandler<BC_ReturnN>::disassemble(so, code, meta); break;
            case BC_PushValues2Table: ByteCodeHandler<BC_PushValues2Table>::disassemble(so, code, meta); break;
            case BC_SetExtCount: ByteCodeHandler<BC_SetExtCount>::disassemble(so, code, meta); break;
            default: ASSERT(0);
        }
        so << endl;

        if ((code & 0xff) == BC_NewFunction) {
            auto meta = ByteCodeHandler<BC_NewFunction>::getMetaFromCode(code);
            disassemble(so, meta);
        }
    }
}

void emitCode(LuaFunctionMeta* meta) {
    meta->codes.clear();
    meta->ip2line.clear();
    (StmtNodeVisitor_CodeEmitor(meta, meta->ast));
    assert(meta->codes.size() == meta->ip2line.size());
}
