
#include "pch.h"
#include "BESymbolTable.h"
#include "BEType.h"
#include "BEConstant.h"
#include "BEStorage.h"
#include "BEx86CodeGenerator.h"
#include "BEx86FileBuilder.h"
#include "BEx86FunctionBuilder.h"
#include "SourceFileProto.h"
#include "AST.h"

class StmtNodeVisitor_CodeGenerator;
class ExprNodeVisitor_CodeGenerator: public IExprNodeVisitor {
public:
    ExprNodeVisitor_CodeGenerator(StmtNodeVisitor_CodeGenerator *stmt, ExprNodePtr expr);
    BEVariablePtr getVariable();
private:
    virtual void visit(ExprNode_StringLiteral *node);
    virtual void visit(ExprNode_IntLiteral *node);
    virtual void visit(ExprNode_FloatLiteral *node);
    virtual void visit(ExprNode_Variable *node);
    virtual void visit(ExprNode_Assignment *node);
    virtual void visit(ExprNode_BinaryOp *node);
    virtual void visit(ExprNode_UnaryOp *node);
    virtual void visit(ExprNode_TypeCast *node);
    virtual void visit(ExprNode_Call *node);
private:
    StmtNodeVisitor_CodeGenerator *m_stmt;
    BEx86FunctionBuilder *m_builder;
    BEVariablePtr m_var;
};

class StmtNodeVisitor_CodeGenerator : public IStmtNodeVisitor {
public:
    StmtNodeVisitor_CodeGenerator(BEx86FunctionBuilder *builder, FunctionProtoPtr func):
        m_builder(builder) {

        m_builder->beginBuild();
        for (auto typeID : func->argsTypeID) {
            const BEType *type = BETypeManager::instance()->getType(typeID.first);
            m_builder->declareArgVariable(typeID.second, type);
        }

        func->body->acceptVisitor(this);

        m_builder->endBuild();
    }
    BEx86FunctionBuilder* getBuilder() {
        return m_builder;
    }
private:
    virtual void visit(StmtNode_Block *node) {
    }
    virtual void visit(StmtNode_Stmts *node) {
    }
    virtual void visit(StmtNode_Expr *node) {
    }
    virtual void visit(StmtNode_DefineVariable *node) {
    }
    virtual void visit(StmtNode_Continue *node) {
    }
    virtual void visit(StmtNode_Break *node) {
    }
    virtual void visit(StmtNode_Return *node) {
    }
    virtual void visit(StmtNode_IfThenElse *node) {
    }
    virtual void visit(StmtNode_For *node) {
    }
private:
    BEx86FunctionBuilder *m_builder;
};

ExprNodeVisitor_CodeGenerator::ExprNodeVisitor_CodeGenerator(StmtNodeVisitor_CodeGenerator *stmt, ExprNodePtr expr):
    m_stmt(stmt), m_builder(stmt->getBuilder()) {
    expr->acceptVisitor(this);
}
BEVariablePtr ExprNodeVisitor_CodeGenerator::getVariable() {
    return m_var;
}
void ExprNodeVisitor_CodeGenerator::visit(ExprNode_StringLiteral *node) {
    m_var = m_builder->loadConstant(m_builder->getParent()->getConstantPool()->get(node->str));
}
void ExprNodeVisitor_CodeGenerator::visit(ExprNode_IntLiteral *node) {
    m_var = m_builder->loadConstant(m_builder->getParent()->getConstantPool()->get(node->number));
}
void ExprNodeVisitor_CodeGenerator::visit(ExprNode_FloatLiteral *node) {
    ASSERT(0);
}
void ExprNodeVisitor_CodeGenerator::visit(ExprNode_Variable *node) {
    m_var = m_builder->getLocalVariable(node->name);
    if (m_var == NULL) m_var = m_builder->getGlobalVariable(node->name);
    ASSERT(m_var != NULL);
}
void ExprNodeVisitor_CodeGenerator::visit(ExprNode_Assignment *node) {
    m_var = m_builder->getLocalVariable(node->left);
    if (m_var == NULL) m_var = m_builder->getGlobalVariable(node->left);
    ASSERT(m_var != NULL);
    m_var = m_builder->store(m_var, ExprNodeVisitor_CodeGenerator(m_stmt, node->right).getVariable());
}
void ExprNodeVisitor_CodeGenerator::visit(ExprNode_BinaryOp *node) {
    if (node->op == "&&" || node->op == "||") {
        // TODO
        return;
    }

    if (node->op == "+") {
        m_var = ExprNodeVisitor_CodeGenerator(m_stmt, node->left).getVariable();
        m_var = m_builder->createAdd(m_var, ExprNodeVisitor_CodeGenerator(m_stmt, node->right).getVariable());
    } else if (node->op == "-") {
        m_var = ExprNodeVisitor_CodeGenerator(m_stmt, node->left).getVariable();
        m_var = m_builder->createSub(m_var, ExprNodeVisitor_CodeGenerator(m_stmt, node->right).getVariable());
    } else if (node->op == "*") {
        m_var = ExprNodeVisitor_CodeGenerator(m_stmt, node->left).getVariable();
        m_var = m_builder->createMul(m_var, ExprNodeVisitor_CodeGenerator(m_stmt, node->right).getVariable());
    } else if (node->op == "/") {
        m_var = ExprNodeVisitor_CodeGenerator(m_stmt, node->left).getVariable();
        m_var = m_builder->createDiv(m_var, ExprNodeVisitor_CodeGenerator(m_stmt, node->right).getVariable());
    } else if (node->op == "%") {
        m_var = ExprNodeVisitor_CodeGenerator(m_stmt, node->left).getVariable();
        m_var = m_builder->createMod(m_var, ExprNodeVisitor_CodeGenerator(m_stmt, node->right).getVariable());
    } else if (node->op == "<") {
        m_var = ExprNodeVisitor_CodeGenerator(m_stmt, node->left).getVariable();
        m_var = m_builder->createLt(m_var, ExprNodeVisitor_CodeGenerator(m_stmt, node->right).getVariable());
    } else if (node->op == "<=") {
        m_var = ExprNodeVisitor_CodeGenerator(m_stmt, node->left).getVariable();
        m_var = m_builder->createLe(m_var, ExprNodeVisitor_CodeGenerator(m_stmt, node->right).getVariable());
    } else if (node->op == ">") {
        m_var = ExprNodeVisitor_CodeGenerator(m_stmt, node->left).getVariable();
        m_var = m_builder->createGt(m_var, ExprNodeVisitor_CodeGenerator(m_stmt, node->right).getVariable());
    } else if (node->op == ">=") {
        m_var = ExprNodeVisitor_CodeGenerator(m_stmt, node->left).getVariable();
        m_var = m_builder->createGe(m_var, ExprNodeVisitor_CodeGenerator(m_stmt, node->right).getVariable());
    } else if (node->op == "==") {
        m_var = ExprNodeVisitor_CodeGenerator(m_stmt, node->left).getVariable();
        m_var = m_builder->createEq(m_var, ExprNodeVisitor_CodeGenerator(m_stmt, node->right).getVariable());
    } else if (node->op == "!=") {
        m_var = ExprNodeVisitor_CodeGenerator(m_stmt, node->left).getVariable();
        m_var = m_builder->createNe(m_var, ExprNodeVisitor_CodeGenerator(m_stmt, node->right).getVariable());
    } else {
        ASSERT(0);
    }
}
void ExprNodeVisitor_CodeGenerator::visit(ExprNode_UnaryOp *node) {
    if (node->op == "-") {
        m_var = m_builder->loadConstant(m_builder->getParent()->getConstantPool()->get(0));
        m_var = m_builder->createSub(m_var, ExprNodeVisitor_CodeGenerator(m_stmt, node->expr).getVariable());
    } else if (node->op == "!") {
        m_var = ExprNodeVisitor_CodeGenerator(m_stmt, node->expr).getVariable();
        m_var = m_builder->createNot(m_var);
    } else {
        ASSERT(0);
    }
}
void ExprNodeVisitor_CodeGenerator::visit(ExprNode_TypeCast *node) {
    ASSERT(0);
}
void ExprNodeVisitor_CodeGenerator::visit(ExprNode_Call *node) {
    m_builder->beginCall((int)node->args.size());
    for (auto arg : node->args) {
        m_builder->createPush(ExprNodeVisitor_CodeGenerator(m_stmt, arg).getVariable());
    }
    BESymbol *funcSymbol = m_builder->getParent()->getGlobalSymbolTable()->get(node->funcName);
    m_builder->endCall(funcSymbol, (int)node->args.size());
}

BEx86FileBuilder* generatex86Code(SourceFileProto *fileProto) {
    BEx86FileBuilder *fileBuilder = new BEx86FileBuilder();

    for (auto func : fileProto->externFuncs) {
        const BEType *type = BETypeManager::instance()->getFuncType();
        fileBuilder->getGlobalSymbolTable()->declare(func->name, type);
        fileBuilder->setAsExternSymbol(func->name);
    }

    for (auto _stmt : fileProto->globalVars) {
        auto stmt = static_cast<StmtNode_DefineVariable*>(_stmt.get());
        const BEType *type = BETypeManager::instance()->getType(stmt->type);
        fileBuilder->getGlobalSymbolTable()->declare(stmt->name, type);
    }

    for (auto func : fileProto->funcs) {
        const BEType *type = BETypeManager::instance()->getFuncType();
        fileBuilder->getGlobalSymbolTable()->declare(func->name, type);
        BEx86FunctionBuilder *builder = fileBuilder->createFunctionBuilder(func->name);
        StmtNodeVisitor_CodeGenerator(builder, func);
    }

    return fileBuilder;
}
