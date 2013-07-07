
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
            const BEType *type = BETypeManager::instance()->get(typeID.first);
            m_builder->declareArgVariable(typeID.second, type);
        }

        m_builder->pushBasicBlock(m_builder->createBasicBlock("label_entry"));
        func->body->acceptVisitor(this);

        m_builder->endBuild();
    }
    BEx86FunctionBuilder* getBuilder() {
        return m_builder;
    }
private:
    virtual void visit(StmtNode_Block *node) {
        m_builder->beginScope();
        for (auto stmt : node->stmts) stmt->acceptVisitor(this);
        m_builder->endScope();
    }
    virtual void visit(StmtNode_Stmts *node) {
        for (auto stmt : node->stmts) stmt->acceptVisitor(this);
    }
    virtual void visit(StmtNode_Expr *node) {
        ExprNodeVisitor_CodeGenerator(this, node->expr);
    }
    virtual void visit(StmtNode_DefineVariable *node) {
        const BEType *type = BETypeManager::instance()->get(node->type);
        m_builder->declareLocalVariable(node->name, type);
    }
    virtual void visit(StmtNode_Continue *node) {
        m_builder->createJmp(m_continueBlocks.back());
    }
    virtual void visit(StmtNode_Break *node) {
        m_builder->createJmp(m_breakBlocks.back());
    }
    virtual void visit(StmtNode_Return *node) {
        if (node->expr != NULL) m_builder->createRet(ExprNodeVisitor_CodeGenerator(this, node->expr).getVariable());
        else m_builder->createRet();
    }
    virtual void visit(StmtNode_IfThenElse *node) {
        BEx86BasicBlock *thenBlock = m_builder->createBasicBlock("label_then");
        BEx86BasicBlock *elseBlock = m_builder->createBasicBlock("label_else");
        BEx86BasicBlock *endBlock = m_builder->createBasicBlock("label_endif");
        {
            BEVariablePtr temp(ExprNodeVisitor_CodeGenerator(this, node->cond).getVariable());
            m_builder->createCJmp(temp, thenBlock, elseBlock);
        }
        m_builder->pushBasicBlock(thenBlock);
        if (node->thenStmt != NULL) node->thenStmt->acceptVisitor(this);
        m_builder->createJmp(endBlock);
        m_builder->pushBasicBlock(elseBlock);
        if (node->elseStmt != NULL) node->elseStmt->acceptVisitor(this);
        m_builder->createJmp(endBlock);
        m_builder->pushBasicBlock(endBlock);
    }
    virtual void visit(StmtNode_For *node) {
        if (node->first != NULL) node->first->acceptVisitor(this);
        /*
label_loop:
           temp = visit node->second
           cjmp temp label_body label_break
label_body:
           visit node->body
           jmp label_continue
label_continue:
           visit node->third
           jmp label_loop
label_break:
         * */
        BEx86BasicBlock *loopBlock = m_builder->createBasicBlock("label_loop");
        BEx86BasicBlock *bodyBlock = m_builder->createBasicBlock("label_body");
        BEx86BasicBlock *continueBlock = m_builder->createBasicBlock("label_continue");
        BEx86BasicBlock *breakBlock = m_builder->createBasicBlock("label_break");
        m_continueBlocks.push_back(continueBlock);
        m_breakBlocks.push_back(breakBlock);

        m_builder->createJmp(loopBlock);
        m_builder->pushBasicBlock(loopBlock);
        {
            BEVariablePtr temp(m_builder->loadConstant(m_builder->getParent()->getConstantPool()->get(1)));
            if (node->second != NULL) temp = ExprNodeVisitor_CodeGenerator(this, node->second).getVariable();
            m_builder->createCJmp(temp, bodyBlock, breakBlock);
        }
        m_builder->pushBasicBlock(bodyBlock);
        if (node->body != NULL) node->body->acceptVisitor(this);
        m_builder->createJmp(continueBlock);
        m_builder->pushBasicBlock(continueBlock);
        if (node->third != NULL) ExprNodeVisitor_CodeGenerator(this, node->third);
        m_builder->createJmp(loopBlock);
        m_builder->pushBasicBlock(breakBlock);

        m_continueBlocks.pop_back();
        m_breakBlocks.pop_back();
    }
private:
    BEx86FunctionBuilder *m_builder;
    vector<BEx86BasicBlock*> m_breakBlocks, m_continueBlocks;
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
    if (node->op == ExprNode_BinaryOp::OT_And || node->op == ExprNode_BinaryOp::OT_Or) {
        /*
           temp = visit node->left
           temp = ne temp, 0
           cjmp cond label_logic_true label_logic_false
label_logic_true:
           temp = visit node->right
           temp = ne temp, 0
           store cond temp
           jmp label_logic_end
label_logic_false:
           jmp label_logic_end
label_logic_end:
            <- cond
         * */
        BEx86BasicBlock *trueBlock = m_builder->createBasicBlock("label_logic_true");
        BEx86BasicBlock *falseBlock = m_builder->createBasicBlock("label_logic_false");
        BEx86BasicBlock *endBlock = m_builder->createBasicBlock("label_logic_end");
        m_var = ExprNodeVisitor_CodeGenerator(m_stmt, node->left).getVariable();
        m_var = m_builder->createNe(m_var, m_builder->loadConstant(m_builder->getParent()->getConstantPool()->get(0)));
        {
            BEVariablePtr temp(m_var);
            if (node->op == ExprNode_BinaryOp::OT_And) m_builder->createCJmp(temp, trueBlock, falseBlock);
            else m_builder->createCJmp(temp, falseBlock, trueBlock);
        }
        m_builder->pushBasicBlock(trueBlock);
        {
            BEVariablePtr temp = ExprNodeVisitor_CodeGenerator(m_stmt, node->right).getVariable();
            temp = m_builder->createNe(temp, m_builder->loadConstant(m_builder->getParent()->getConstantPool()->get(0)));
            m_builder->store(m_var, temp);
        }
        m_builder->createJmp(endBlock);
        m_builder->pushBasicBlock(falseBlock);
        m_builder->createJmp(endBlock);
        m_builder->pushBasicBlock(endBlock);
        return;
    }

    switch (node->op) {
        case ExprNode_BinaryOp::OT_Add: {
                m_var = ExprNodeVisitor_CodeGenerator(m_stmt, node->left).getVariable();
                m_var = m_builder->createAdd(m_var, ExprNodeVisitor_CodeGenerator(m_stmt, node->right).getVariable());
            }
            break;
        case ExprNode_BinaryOp::OT_Sub: {
                m_var = ExprNodeVisitor_CodeGenerator(m_stmt, node->left).getVariable();
                m_var = m_builder->createSub(m_var, ExprNodeVisitor_CodeGenerator(m_stmt, node->right).getVariable());
            }
            break;
        case ExprNode_BinaryOp::OT_Mul: {
                m_var = ExprNodeVisitor_CodeGenerator(m_stmt, node->left).getVariable();
                m_var = m_builder->createMul(m_var, ExprNodeVisitor_CodeGenerator(m_stmt, node->right).getVariable());
            }
            break;
        case ExprNode_BinaryOp::OT_Div: {
                m_var = ExprNodeVisitor_CodeGenerator(m_stmt, node->left).getVariable();
                m_var = m_builder->createDiv(m_var, ExprNodeVisitor_CodeGenerator(m_stmt, node->right).getVariable());
            }
            break;
        case ExprNode_BinaryOp::OT_Mod: {
                m_var = ExprNodeVisitor_CodeGenerator(m_stmt, node->left).getVariable();
                m_var = m_builder->createMod(m_var, ExprNodeVisitor_CodeGenerator(m_stmt, node->right).getVariable());
            }
            break;
        case ExprNode_BinaryOp::OT_Less: {
                m_var = ExprNodeVisitor_CodeGenerator(m_stmt, node->left).getVariable();
                m_var = m_builder->createLt(m_var, ExprNodeVisitor_CodeGenerator(m_stmt, node->right).getVariable());
            }
            break;
        case ExprNode_BinaryOp::OT_LessEq: {
                m_var = ExprNodeVisitor_CodeGenerator(m_stmt, node->left).getVariable();
                m_var = m_builder->createLe(m_var, ExprNodeVisitor_CodeGenerator(m_stmt, node->right).getVariable());
            }
            break;
        case ExprNode_BinaryOp::OT_Greater: {
                m_var = ExprNodeVisitor_CodeGenerator(m_stmt, node->left).getVariable();
                m_var = m_builder->createGt(m_var, ExprNodeVisitor_CodeGenerator(m_stmt, node->right).getVariable());
            }
            break;
        case ExprNode_BinaryOp::OT_GreaterEq: {
                m_var = ExprNodeVisitor_CodeGenerator(m_stmt, node->left).getVariable();
                m_var = m_builder->createGe(m_var, ExprNodeVisitor_CodeGenerator(m_stmt, node->right).getVariable());
            }
            break;
        case ExprNode_BinaryOp::OT_Equal: {
                m_var = ExprNodeVisitor_CodeGenerator(m_stmt, node->left).getVariable();
                m_var = m_builder->createEq(m_var, ExprNodeVisitor_CodeGenerator(m_stmt, node->right).getVariable());
            }
            break;
        case ExprNode_BinaryOp::OT_NEqual: {
                m_var = ExprNodeVisitor_CodeGenerator(m_stmt, node->left).getVariable();
                m_var = m_builder->createNe(m_var, ExprNodeVisitor_CodeGenerator(m_stmt, node->right).getVariable());
            }
            break;
        default: ASSERT(0); break;
    }
}
void ExprNodeVisitor_CodeGenerator::visit(ExprNode_UnaryOp *node) {
    switch (node->op) {
        case ExprNode_UnaryOp::OT_Minus: {
                 m_var = m_builder->loadConstant(m_builder->getParent()->getConstantPool()->get(0));
                 m_var = m_builder->createSub(m_var, ExprNodeVisitor_CodeGenerator(m_stmt, node->expr).getVariable());
            }
            break;
        case ExprNode_UnaryOp::OT_Not: {
                m_var = ExprNodeVisitor_CodeGenerator(m_stmt, node->expr).getVariable();
                m_var = m_builder->createEq(m_var, m_builder->loadConstant(m_builder->getParent()->getConstantPool()->get(0)));
            }
            break;
        default: ASSERT(0); break;
    }
}
void ExprNodeVisitor_CodeGenerator::visit(ExprNode_TypeCast *node) {
    ASSERT(0);
}
void ExprNodeVisitor_CodeGenerator::visit(ExprNode_Call *node) {
    m_builder->beginCall((int)node->args.size());
    for (int i = (int)node->args.size() - 1; i >= 0; --i) {
        m_builder->createPush(ExprNodeVisitor_CodeGenerator(m_stmt, node->args[i]).getVariable());
    }
    BESymbol *funcSymbol = m_builder->getParent()->getGlobalSymbolTable()->get(node->funcName);
    m_var = m_builder->endCall(BETypeManager::instance()->get("int"), funcSymbol, (int)node->args.size());
}

BEx86FileBuilderPtr generatex86Code(SourceFileProto *fileProto) {
    BEx86FileBuilderPtr fileBuilder(new BEx86FileBuilder());

    for (auto func : fileProto->externFuncs) {
        const BEType *type = BETypeManager::instance()->getFunc();
        fileBuilder->getGlobalSymbolTable()->declare(func->name, type);
        fileBuilder->setAsExternSymbol(func->name);
    }

    for (auto _stmt : fileProto->globalVars) {
        auto stmt = static_cast<StmtNode_DefineVariable*>(_stmt.get());
        const BEType *type = BETypeManager::instance()->get(stmt->type);
        fileBuilder->getGlobalSymbolTable()->declare(stmt->name, type);
    }

    for (auto func : fileProto->funcs) {
        const BEType *type = BETypeManager::instance()->getFunc();
        fileBuilder->getGlobalSymbolTable()->declare(func->name, type);
        BEx86FunctionBuilder *builder = fileBuilder->createFunctionBuilder(func->name);
        StmtNodeVisitor_CodeGenerator(builder, func);
    }

    return fileBuilder;
}
