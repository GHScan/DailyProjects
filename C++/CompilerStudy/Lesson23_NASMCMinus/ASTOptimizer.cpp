
#include "pch.h"
#include "ASTOptimizer.h"
#include "SourceFileProto.h"
#include "AST.h"

class ASTOptimizerManager {
public:
    ASTOptimizerManager(ASTOptimizeType optTypeFlag): m_optTypeFlag(optTypeFlag) {
    }
    StmtNodePtr optimizeStmt(StmtNodePtr stmt);
    ExprNodePtr optimizeExpr(ExprNodePtr expr);
private:
    const ASTOptimizeType m_optTypeFlag;
};

class ExprNodeVisitor_ConstantFoldingOptimizer: public IExprNodeVisitor {
public:
    ExprNodeVisitor_ConstantFoldingOptimizer(ASTOptimizerManager *manager, ExprNodePtr expr):
        m_manager(manager), m_result(expr) {
        expr->acceptVisitor(this);
    }
    ExprNodePtr getResult() {
        return m_result;
    }
private:
    virtual void visit(ExprNode_StringLiteral *node) {
    }
    virtual void visit(ExprNode_IntLiteral *node) {
    }
    virtual void visit(ExprNode_FloatLiteral *node) {
    }
    virtual void visit(ExprNode_Variable *node) {
    }
    virtual void visit(ExprNode_Assignment *node) {
        node->right = ExprNodeVisitor_ConstantFoldingOptimizer(m_manager, node->right).getResult();
    }
    virtual void visit(ExprNode_BinaryOp *node) {
        node->left = ExprNodeVisitor_ConstantFoldingOptimizer(m_manager, node->left).getResult();
        node->right = ExprNodeVisitor_ConstantFoldingOptimizer(m_manager, node->right).getResult();
        if (node->op == ExprNode_BinaryOp::OT_And) {
            if (auto p = dynamic_cast<ExprNode_IntLiteral*>(node->left.get())) {
                if (p->number == 0) m_result = node->left;
                else {
                    if (auto p = dynamic_cast<ExprNode_IntLiteral*>(node->right.get())) {
                        m_result.reset(new ExprNode_IntLiteral(p->number != 0));
                    } else {
                        m_result.reset(new ExprNode_BinaryOp("!=", node->right, ExprNodePtr(new ExprNode_IntLiteral(0))));
                    }
                }
            }
            return;
        }
        if (node->op == ExprNode_BinaryOp::OT_Or) {
            if (auto p = dynamic_cast<ExprNode_IntLiteral*>(node->left.get())) {
                if (p->number == 0) {
                    if (auto p = dynamic_cast<ExprNode_IntLiteral*>(node->right.get())) {
                        m_result.reset(new ExprNode_IntLiteral(p->number != 0));
                    } else {
                        m_result.reset(new ExprNode_BinaryOp("!=", node->right, ExprNodePtr(new ExprNode_IntLiteral(0))));
                    }
                } else {
                    m_result.reset(new ExprNode_IntLiteral(1));
                }
            }
            return;
        }

        bool isLeftConst = dynamic_cast<ExprNode_IntLiteral*>(node->left.get()) || dynamic_cast<ExprNode_FloatLiteral*>(node->left.get());
        bool isRightConst = dynamic_cast<ExprNode_IntLiteral*>(node->right.get()) || dynamic_cast<ExprNode_FloatLiteral*>(node->right.get());
        if (!isLeftConst || !isRightConst) return;

        bool isIntResult = dynamic_cast<ExprNode_IntLiteral*>(node->left.get()) && dynamic_cast<ExprNode_IntLiteral*>(node->right.get());
        int leftI, rightI, resultI;
        float leftF, rightF, resultF;
        if (auto p = dynamic_cast<ExprNode_IntLiteral*>(node->left.get())) leftI = p->number, leftF = p->number;
        else leftF = dynamic_cast<ExprNode_FloatLiteral*>(node->left.get())->number;
        if (auto p = dynamic_cast<ExprNode_IntLiteral*>(node->right.get())) rightI = p->number, rightF = p->number;
        else rightF = dynamic_cast<ExprNode_FloatLiteral*>(node->right.get())->number;

        switch (node->op) {
            case ExprNode_BinaryOp::OT_Add:
                if (isIntResult) resultI = leftI + rightI;
                else resultF = leftF + rightF;
                break;
            case ExprNode_BinaryOp::OT_Sub:
                if (isIntResult) resultI = leftI - rightI;
                else resultF = leftF - rightF;
                break;
            case ExprNode_BinaryOp::OT_Mul:
                if (isIntResult) resultI = leftI * rightI;
                else resultF = leftF * rightF;
                break;
            case ExprNode_BinaryOp::OT_Div:
                if (isIntResult) resultI = leftI / rightI;
                else resultF = leftF / rightF;
                break;
            case ExprNode_BinaryOp::OT_Mod:
                if (isIntResult) resultI = leftI % rightI;
                else ASSERT(0);
                break;
            case ExprNode_BinaryOp::OT_Less:
                if (isIntResult) resultI = leftI < rightI;
                else resultI = leftF < rightF;
                isIntResult = true;
                break;
            case ExprNode_BinaryOp::OT_LessEq:
                if (isIntResult) resultI = leftI <= rightI;
                else resultI = leftF <= rightF;
                isIntResult = true;
                break;
            case ExprNode_BinaryOp::OT_Greater:
                if (isIntResult) resultI = leftI > rightI;
                else resultI = leftF > rightF;
                isIntResult = true;
                break;
            case ExprNode_BinaryOp::OT_GreaterEq:
                if (isIntResult) resultI = leftI >= rightI;
                else resultI = leftF >= rightF;
                isIntResult = true;
                break;
            case ExprNode_BinaryOp::OT_Equal:
                if (isIntResult) resultI = leftI == rightI;
                else resultI = leftF == rightF;
                isIntResult = true;
                break;
            case ExprNode_BinaryOp::OT_NEqual:
                if (isIntResult) resultI = leftI != rightI;
                else resultI = leftF != rightF;
                isIntResult = true;
                break;
            default: ASSERT(0); break;
        }

        if (isIntResult) m_result.reset(new ExprNode_IntLiteral(resultI));
        else m_result.reset(new ExprNode_FloatLiteral(resultF));
    }
    virtual void visit(ExprNode_UnaryOp *node) {
        node->expr = ExprNodeVisitor_ConstantFoldingOptimizer(m_manager, node->expr).getResult();
        if (auto p = dynamic_cast<ExprNode_IntLiteral*>(node->expr.get())) {
            switch (node->op) {
                case ExprNode_UnaryOp::OT_Minus:
                    m_result.reset(new ExprNode_IntLiteral(-p->number));
                    break;
                case ExprNode_UnaryOp::OT_Not:
                    m_result.reset(new ExprNode_IntLiteral(!p->number));
                    break;
                default: ASSERT(0); break;
            }
        } else if (auto p = dynamic_cast<ExprNode_FloatLiteral*>(node->expr.get())) {
            switch (node->op) {
                case ExprNode_UnaryOp::OT_Minus:
                    m_result.reset(new ExprNode_FloatLiteral(-p->number));
                    break;
                default: ASSERT(0); break;
            }
        } else {
            ASSERT(0);
        }
    }
    virtual void visit(ExprNode_TypeCast *node) {
        node->expr = ExprNodeVisitor_ConstantFoldingOptimizer(m_manager, node->expr).getResult();
    }
    virtual void visit(ExprNode_Call *node) {
        for (int i = 0; i < (int)node->args.size(); ++i) {
            node->args[i] = ExprNodeVisitor_ConstantFoldingOptimizer(m_manager, node->args[i]).getResult();
        }
    }
private:
    ASTOptimizerManager *m_manager;
    ExprNodePtr m_result;
};
class ExprNodeVisitor_ReductionInStrengthOptimizer: public IExprNodeVisitor {
public:
    ExprNodeVisitor_ReductionInStrengthOptimizer(ASTOptimizerManager *manager, ExprNodePtr expr):
        m_result(expr) {
        expr->acceptVisitor(this);
    }
    ExprNodePtr getResult() {
        return m_result;
    }
private:
    virtual void visit(ExprNode_StringLiteral *node) {
    }
    virtual void visit(ExprNode_IntLiteral *node) {
    }
    virtual void visit(ExprNode_FloatLiteral *node) {
    }
    virtual void visit(ExprNode_Variable *node) {
    }
    virtual void visit(ExprNode_Assignment *node) {
    }
    virtual void visit(ExprNode_BinaryOp *node) {
    }
    virtual void visit(ExprNode_UnaryOp *node) {
    }
    virtual void visit(ExprNode_TypeCast *node) {
    }
    virtual void visit(ExprNode_Call *node) {
    }
private:
    ExprNodePtr m_result;
};
class ExprNodeVisitor_AlgebraicIdentityOptimizer: public IExprNodeVisitor {
public:
    ExprNodeVisitor_AlgebraicIdentityOptimizer(ASTOptimizerManager *manager, ExprNodePtr expr):
        m_result(expr) {
        expr->acceptVisitor(this);
    }
    ExprNodePtr getResult() {
        return m_result;
    }
private:
    virtual void visit(ExprNode_StringLiteral *node) {
    }
    virtual void visit(ExprNode_IntLiteral *node) {
    }
    virtual void visit(ExprNode_FloatLiteral *node) {
    }
    virtual void visit(ExprNode_Variable *node) {
    }
    virtual void visit(ExprNode_Assignment *node) {
    }
    virtual void visit(ExprNode_BinaryOp *node) {
    }
    virtual void visit(ExprNode_UnaryOp *node) {
    }
    virtual void visit(ExprNode_TypeCast *node) {
    }
    virtual void visit(ExprNode_Call *node) {
    }
private:
    ExprNodePtr m_result;
};
class ExprNodeVisitor_ErshovNumberOptimizer: public IExprNodeVisitor {
public:
    ExprNodeVisitor_ErshovNumberOptimizer(ASTOptimizerManager *manager, ExprNodePtr expr):
        m_result(expr) {
        expr->acceptVisitor(this);
    }
    ExprNodePtr getResult() {
        return m_result;
    }
private:
    virtual void visit(ExprNode_StringLiteral *node) {
    }
    virtual void visit(ExprNode_IntLiteral *node) {
    }
    virtual void visit(ExprNode_FloatLiteral *node) {
    }
    virtual void visit(ExprNode_Variable *node) {
    }
    virtual void visit(ExprNode_Assignment *node) {
    }
    virtual void visit(ExprNode_BinaryOp *node) {
    }
    virtual void visit(ExprNode_UnaryOp *node) {
    }
    virtual void visit(ExprNode_TypeCast *node) {
    }
    virtual void visit(ExprNode_Call *node) {
    }
private:
    ExprNodePtr m_result;
};

class StmtNodeVisitor_ExprOptimizeDriver: public IStmtNodeVisitor {
public:
    StmtNodeVisitor_ExprOptimizeDriver(ASTOptimizerManager *manager, StmtNodePtr stmt):
        m_manager(manager), m_result(stmt) {
        stmt->acceptVisitor(this);
    }
    StmtNodePtr getResult() {
        return m_result;
    }
private:
    virtual void visit(StmtNode_Block *node) {
        for (auto stmt : node->stmts) stmt->acceptVisitor(this);
    }
    virtual void visit(StmtNode_Stmts *node) {
        for (auto stmt : node->stmts) stmt->acceptVisitor(this);
    }
    virtual void visit(StmtNode_Expr *node) {
        node->expr = m_manager->optimizeExpr(node->expr);
    }
    virtual void visit(StmtNode_DefineVariable *node) {
    }
    virtual void visit(StmtNode_Continue *node) {
    }
    virtual void visit(StmtNode_Break *node) {
    }
    virtual void visit(StmtNode_Return *node) {
        node->expr = m_manager->optimizeExpr(node->expr);
    }
    virtual void visit(StmtNode_IfThenElse *node) {
        node->cond = m_manager->optimizeExpr(node->cond);
        if (node->thenStmt != NULL) node->thenStmt->acceptVisitor(this);
        if (node->elseStmt != NULL) node->elseStmt->acceptVisitor(this);
    } 
    virtual void visit(StmtNode_For *node) {
        if (node->first != NULL) node->first->acceptVisitor(this);
        node->second = m_manager->optimizeExpr(node->second);
        node->third = m_manager->optimizeExpr(node->third);
        if (node->body != NULL) node->body->acceptVisitor(this);
    }
private:
    ASTOptimizerManager *m_manager;
    StmtNodePtr m_result;
};

StmtNodePtr ASTOptimizerManager::optimizeStmt(StmtNodePtr stmt) {
    if (stmt == NULL) return stmt;
    StmtNodeVisitor_ExprOptimizeDriver driver(this, stmt);
    // to do other optimize here
    return driver.getResult();
}
ExprNodePtr ASTOptimizerManager::optimizeExpr(ExprNodePtr expr) {
    if (expr == NULL) return expr;
    for (int i = 0; i < AOT_Count; ++i) {
        if (((1 << i) & m_optTypeFlag) == 0) continue;
        switch (1 << i) {
            case AOT_ConstantFolding: expr = ExprNodeVisitor_ConstantFoldingOptimizer(this, expr).getResult(); break;
            case AOT_ReductionInStrength: expr = ExprNodeVisitor_ReductionInStrengthOptimizer(this, expr).getResult(); break;
            case AOT_ErshovNumber: expr = ExprNodeVisitor_ErshovNumberOptimizer(this, expr).getResult(); break;
            case AOT_AlgebraicIdentity: expr = ExprNodeVisitor_AlgebraicIdentityOptimizer(this, expr).getResult(); break;
            default: ASSERT(0); break;
        }
    }
    return expr;
}

void optimizeAST(SourceFileProto *fileProto, ASTOptimizeType optTypeFlag) {
    ASTOptimizerManager manager(optTypeFlag);
    for (auto func : fileProto->funcs) {
        func->body = manager.optimizeStmt(func->body);
    }
}
