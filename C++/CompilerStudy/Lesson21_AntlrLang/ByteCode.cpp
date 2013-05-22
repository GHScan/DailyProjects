
#include "pch.h"
#include "ByteCode.h"
#include "ByteCodeDefines.h"
#include "AST.h"

class LocalVarIDAllocator {
public:
    LocalVarIDAllocator(int initLocalIdx):
        m_initLocalIdx(initLocalIdx), m_curLocalIdx(initLocalIdx), m_maxLocalIdx(initLocalIdx){
    }
    int getMaxLocalIdx() { return m_maxLocalIdx; }
    int allocID() {
        int id = VarID::fromLocal(m_curLocalIdx).getInt();
        m_maxLocalIdx = max(++m_curLocalIdx, m_maxLocalIdx);
        return id;
    }
    void freeID(int id) {
        ASSERT(VarID(id).getLocal() == m_curLocalIdx - 1);
        --m_curLocalIdx;
    }
    vector<int> allocIDs(int n) {
        vector<int> r;
        while (--n >= 0) r.push_back(allocID());
        return r;
    }
    void freeIDs(vector<int>& ids) {
        for (auto iter = ids.rbegin(); iter != ids.rend(); ++iter) freeID(*iter);
        ids.clear();
    }
private: 
    int m_initLocalIdx;
    int m_curLocalIdx;
    int m_maxLocalIdx;
};

#define EMIT(codeType, ...) {m_ip2line.push_back(node->line); m_codes.push_back(0); ByteCodeHandler<codeType>::emitCode(m_codes.back(), __VA_ARGS__);}
#define PRE_EMIT(off)  {m_ip2line.push_back(node->line); off = CUR_CODE_OFF; m_codes.push_back(0);}
#define POST_EMIT(off, codeType, ...) { ByteCodeHandler<codeType>::emitCode(m_codes[off], __VA_ARGS__); }
#define CUR_CODE_OFF int(m_codes.size())

class ExprNodeVisitor_CodEmitor:
    public IExprNodeVisitor {
public:
    ExprNodeVisitor_CodEmitor(FuncMetaPtr& meta, LocalVarIDAllocator* allocator, const ExprNodePtr &expr, int varID = -1)
        : m_meta(meta), m_codes(meta->codes), m_ip2line(meta->ip2line), m_varID(varID), m_isAlloced(false), m_allocator(allocator) {
    }
    ~ExprNodeVisitor_CodEmitor() {
        if (m_isAlloced) {
            m_allocator->freeID(m_varID);
        }
    }
    int getVarID() {
        ASSERT(m_varID != -1);
        return m_varID;
    }
    bool isAllocated() const { return m_isAlloced; }
private:
    virtual void visit(ExprNode_Global* node) {
        makesureValidVarID();
        EMIT(BC_GetGlobal, m_varID, VarID::fromConst(node->constIdx).getInt());
    }
    virtual void visit(ExprNode_Local* node) {
        int localID = VarID::fromLocal(node->localIdx).getInt();
        if (m_varID == -1) m_varID = localID;
        else EMIT(BC_Move, m_varID, localID);
    }
    virtual void visit(ExprNode_Const* node) {
        int constID = VarID::fromConst(node->constIdx).getInt();
        if (m_varID == -1) m_varID = constID;
        else EMIT(BC_Move, m_varID, constID);
    }
    virtual void visit(ExprNode_Lambda* node) {
        makesureValidVarID();
        EMIT(BC_NewFunction, m_varID, node->metaIdx);
    }
    virtual void visit(ExprNode_ArrayConstructor* node) {
        makesureValidVarID();
        vector<int> ids;
        for (int i = 0; i < (int)node->exprs.size(); ++i) {
            ids.push_back(m_allocator->allocID());
            ExprNodeVisitor_CodEmitor(m_meta, m_allocator, node->exprs[i], ids.back());
        }
        if (ids.empty()) ids.push_back(m_allocator->allocID());
        EMIT(BC_NewArray, m_varID, ids.front(), (int)node->exprs.size());
        m_allocator->freeIDs(ids);
    }
    virtual void visit(ExprNode_IndexOf* node) {
        makesureValidVarID();
        ExprNodeVisitor_CodEmitor arrayExpr(m_meta, m_allocator, node->array);
        int kID = ExprNodeVisitor_CodEmitor(m_meta, m_allocator, node->index).getVarID();
        EMIT(BC_GetArray, m_varID, arrayExpr.getVarID(), kID);
    }
    virtual void visit(ExprNode_Call* node) {
        makesureValidVarID();
        ExprNodeVisitor_CodEmitor funcExpr(m_meta, m_allocator, node->func);
        vector<int> ids;
        for (int i = 0; i < (int)node->params.size(); ++i) {
            ids.push_back(m_allocator->allocID());
            ExprNodeVisitor_CodEmitor(m_meta, m_allocator, node->params[i], ids.back());
        }
        if (ids.empty()) ids.push_back(m_allocator->allocID());
        EMIT(BC_Call, funcExpr.getVarID(), ids.front(), (int)node->params.size());
        EMIT(BC_Move, m_varID, ids.front());
        m_allocator->freeIDs(ids);
    }
    virtual void visit(ExprNode_UnaryOp* node) {
        makesureValidVarID();
        int exprID = ExprNodeVisitor_CodEmitor(m_meta, m_allocator, node->expr).getVarID();
        switch (node->op) {
            case ExprNode_UnaryOp::OT_Not: EMIT(BC_Not, m_varID, exprID); break;
            case ExprNode_UnaryOp::OT_Minus: EMIT(BC_Minus, m_varID, exprID); break;
            default: ASSERT(0); break;
        }
    }
    virtual void visit(ExprNode_BinaryOp* node) {
        makesureValidVarID();
        if (node->op == ExprNode_BinaryOp::OT_And || node->op == ExprNode_BinaryOp::OT_Or) {
            /*
               m_varID<-lexpr
               fjump m_varID l_end // tjump m_varID l_end
               m_varID<-rexpr
l_end:
             * */
            ExprNodeVisitor_CodEmitor(m_meta, m_allocator, node->lexpr, m_varID);
            int jump_end;
            PRE_EMIT(jump_end);
            ExprNodeVisitor_CodEmitor(m_meta, m_allocator, node->rexpr, m_varID);
            if (node->op == ExprNode_BinaryOp::OT_And) {
                POST_EMIT(jump_end, BC_FalseJump, m_varID, CUR_CODE_OFF);
            } else {
                POST_EMIT(jump_end, BC_TrueJump, m_varID, CUR_CODE_OFF);
            }
            return;
        }

        ExprNodeVisitor_CodEmitor lexpr(m_meta, m_allocator, node->lexpr);
        int rexprID = ExprNodeVisitor_CodEmitor(m_meta, m_allocator, node->rexpr).getVarID();
        switch (node->op) {
            case ExprNode_BinaryOp::OT_And: ASSERT(0); break;
            case ExprNode_BinaryOp::OT_Or: ASSERT(0); break;
            case ExprNode_BinaryOp::OT_Less: EMIT(BC_Less, m_varID, lexpr.getVarID(), rexprID); break;
            case ExprNode_BinaryOp::OT_LessEq: EMIT(BC_LessEq, m_varID, lexpr.getVarID(), rexprID); break;
            case ExprNode_BinaryOp::OT_Greater: EMIT(BC_Greater, m_varID, lexpr.getVarID(), rexprID); break;
            case ExprNode_BinaryOp::OT_GreaterEq: EMIT(BC_GreaterEq, m_varID, lexpr.getVarID(), rexprID); break;
            case ExprNode_BinaryOp::OT_Equal: EMIT(BC_Equal, m_varID, lexpr.getVarID(), rexprID); break;
            case ExprNode_BinaryOp::OT_NEqual: EMIT(BC_NEqual, m_varID, lexpr.getVarID(), rexprID); break;
            case ExprNode_BinaryOp::OT_Add: EMIT(BC_Add, m_varID, lexpr.getVarID(), rexprID); break;
            case ExprNode_BinaryOp::OT_Sub: EMIT(BC_Sub, m_varID, lexpr.getVarID(), rexprID); break;
            case ExprNode_BinaryOp::OT_Mul: EMIT(BC_Mul, m_varID, lexpr.getVarID(), rexprID); break;
            case ExprNode_BinaryOp::OT_Div: EMIT(BC_Div, m_varID, lexpr.getVarID(), rexprID); break;
            case ExprNode_BinaryOp::OT_Mod: EMIT(BC_Mod, m_varID, lexpr.getVarID(), rexprID); break;
            case ExprNode_BinaryOp::OT_Pow: EMIT(BC_Pow, m_varID, lexpr.getVarID(), rexprID); break;
            default: ASSERT(0); break;
        }
    }
private:
    void makesureValidVarID() {
        if (m_varID == -1) {
            m_isAlloced = true;
            m_varID = m_allocator->allocID();
        }
    }
private:
    FuncMetaPtr m_meta;
    vector<int> &m_codes, &m_ip2line;
    int m_varID;
    bool m_isAlloced;
    LocalVarIDAllocator *m_allocator;
};

class StmtNodeVisitor_CodeEmitor:
    public IStmtNodeVisitor {
public:
private:
    virtual void visit(StmtNode_Assign *node) {
    }
    virtual void visit(StmtNode_Call *node) {
    }
    virtual void visit(StmtNode_Block *node) {
    }
    virtual void visit(StmtNode_If *node) {
    }
    virtual void visit(StmtNode_For *node) {
    }
    virtual void visit(StmtNode_Break *node) {
    }
    virtual void visit(StmtNode_Continue *node) {
    }
    virtual void visit(StmtNode_Return *node) {
    }
private:
};

void emitCode(FuncMeta *meta) {
}
void execute(StackFrame *stopFrame) {
}
void disassemble(FuncMeta *meta, int depth) {
}
