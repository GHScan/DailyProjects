
#include "pch.h"

#include "LLVMCompiler.h"
#include "AST.h"
#include "SourceFileProto.h"

class StmtNodeVisitor_CodeEmitor;
class LLVMCompilerImpl;

// ==============================
class ExprNodeVisitor_CodeEmitor: public IExprNodeVisitor {
public:
    ExprNodeVisitor_CodeEmitor(StmtNodeVisitor_CodeEmitor *parent, llvm::Type *destType, const ExprNodePtr &expr);
    llvm::Value* getValue() {
        ASSERT(m_value != NULL);
        return m_value;
    }
private: 
    void implicitTypeCast(llvm::Type *type);
    void explicitTypeCast(llvm::Type *type);
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
    StmtNodeVisitor_CodeEmitor *m_parent;
    llvm::Type *m_destType;
    llvm::Value *m_value;
    llvm::IRBuilder<> *m_builder;
};
// ==============================
class StmtNodeVisitor_CodeEmitor: public IStmtNodeVisitor {
public:
    StmtNodeVisitor_CodeEmitor(LLVMCompilerImpl *parent, const FunctionProtoPtr &proto);
    LLVMCompilerImpl* getParent() { return m_parent; }
    llvm::Value* getLocal(const string& name);
private:
    virtual void visit(StmtNode_Block *node);
    virtual void visit(StmtNode_Stmts *node);
    virtual void visit(StmtNode_Expr *node);
    virtual void visit(StmtNode_DefineVarable *node);
    virtual void visit(StmtNode_Continue *node);
    virtual void visit(StmtNode_Break *node);
    virtual void visit(StmtNode_Return *node);
    virtual void visit(StmtNode_IfThenElse *node);
    virtual void visit(StmtNode_For *node);
private:
    void tryInitArgs();
    void removeRedundantIns(llvm::Function *func);
private:
    LLVMCompilerImpl *m_parent;
    llvm::IRBuilder<> *m_builder;
    vector<map<string, llvm::Value*> > m_localVars;
    vector<llvm::BasicBlock*> m_continueLabels, m_breakLabels;
    bool m_isArgInited;
};
// ==============================
class LLVMCompilerImpl {
public:
    LLVMCompilerImpl(const SourceFileProtoPtr &proto);
    ~LLVMCompilerImpl();
    void compile(bool doOptimize);
    void print(const string& fileName);
    void run();

    llvm::Module* getModule() { return m_module; }
    llvm::IRBuilder<>* getBuilder() { return m_builder; }

    llvm::Type* getTypeByString(const string& s);
private:
    void emitGlobalVarables();
    void emitFunctions();
    void emitFunctionDeclare(const FunctionProtoPtr &proto);
    void optimize(llvm::PassManager *pm);

private:
    SourceFileProtoPtr m_proto;
    llvm::Module *m_module;
    llvm::IRBuilder<> *m_builder;
    llvm::ExecutionEngine *m_ee;
};
// ==============================
ExprNodeVisitor_CodeEmitor::ExprNodeVisitor_CodeEmitor(StmtNodeVisitor_CodeEmitor *parent, llvm::Type *destType, const ExprNodePtr &expr):
m_parent(parent), m_destType(destType), m_value(NULL), m_builder(NULL) {
    m_builder = m_parent->getParent()->getBuilder();

    expr->acceptVisitor(this);

    ASSERT(m_value != NULL);
    if (m_destType == NULL) m_destType = m_value->getType();
    implicitTypeCast(m_destType);
    ASSERT(m_value->getType() == m_destType);
}
void ExprNodeVisitor_CodeEmitor::implicitTypeCast(llvm::Type *type) {
    ASSERT(m_value != NULL);
    if (m_value->getType() == type) return;

    if (m_value->getType()->isIntegerTy() && type->isIntegerTy()) {
        if (m_value->getType()->getIntegerBitWidth() < type->getIntegerBitWidth()) {
            m_value = m_builder->CreateSExt(m_value, type);
        }
    }
}
void ExprNodeVisitor_CodeEmitor::explicitTypeCast(llvm::Type *type) {
    implicitTypeCast(type);

    ASSERT(m_value != NULL);
    if (m_value->getType() == type) return;

    if (m_value->getType()->isIntegerTy()) {
        if (type->isFloatTy()) {
            m_value = m_builder->CreateSIToFP(m_value, type);
        } else {
            ASSERT(0);
        }
    } else if (m_value->getType()->isFloatTy()) {
        if (type->isIntegerTy()) {
            m_value = m_builder->CreateFPToSI(m_value, type);
        } else {
            ASSERT(0);
        }
    } else {
        ASSERT(0);
    }
}
void ExprNodeVisitor_CodeEmitor::visit(ExprNode_StringLiteral *node) {
    // TODO:
    m_value = m_builder->CreateGlobalStringPtr(node->str);
}
void ExprNodeVisitor_CodeEmitor::visit(ExprNode_IntLiteral *node) {
    m_value = m_builder->getInt32(node->number);
}
void ExprNodeVisitor_CodeEmitor::visit(ExprNode_FloatLiteral *node) {
    m_value = llvm::ConstantFP::get(llvm::getGlobalContext(), llvm::APFloat(3.000000e+00f));
}
void ExprNodeVisitor_CodeEmitor::visit(ExprNode_Variable *node) {
    if ((m_value = m_parent->getLocal(node->name)) == NULL) {
        m_value = m_parent->getParent()->getModule()->getGlobalVariable(node->name);
    }
    ASSERT(m_value != NULL);
    m_value = m_builder->CreateLoad(m_value);
}
void ExprNodeVisitor_CodeEmitor::visit(ExprNode_Assignment *node) {
    llvm::Value *dest = m_parent->getLocal(node->left);
    if (dest == NULL) dest = m_parent->getParent()->getModule()->getGlobalVariable(node->left);
    ASSERT(dest != NULL);

    m_value = ExprNodeVisitor_CodeEmitor(m_parent, dest->getType()->getPointerElementType(), node->right).getValue();
    m_builder->CreateStore(m_value, dest);
}
void ExprNodeVisitor_CodeEmitor::visit(ExprNode_BinaryOp *node) {
    if (node->op == "&&" || node->op == "||") {
        /*
           lv = eval node->left
           cond = icmp ne lv 0
           br cond label_logicOpRight label_endLogicOp

label_logicOpRight:
           rv = eval node->right
           br label_endLogicOp

label_endLogicOp:
           v = phi [lv, label_old] [rv label_logicOpRight]
         * */
        llvm::Function* func = m_builder->GetInsertBlock()->getParent();
        llvm::BasicBlock *label_logicOpRight = llvm::BasicBlock::Create(llvm::getGlobalContext(), "logicOpRight");
        llvm::BasicBlock *label_endLogicOp = llvm::BasicBlock::Create(llvm::getGlobalContext(), "endLogicOp");


        llvm::Value *lv = ExprNodeVisitor_CodeEmitor(m_parent, m_builder->getInt32Ty(), node->left).getValue();
        llvm::BasicBlock* label_old = m_builder->GetInsertBlock();
        llvm::Value *cond = NULL;
        if (node->op == "&&") {
            cond = m_builder->CreateICmpNE(lv, m_builder->getInt32(0));
        } else {
            cond = m_builder->CreateICmpEQ(lv, m_builder->getInt32(0));
        }
        m_builder->CreateCondBr(cond, label_logicOpRight, label_endLogicOp);

        func->getBasicBlockList().push_back(label_logicOpRight);
        m_builder->SetInsertPoint(label_logicOpRight);
        llvm::Value *rv = ExprNodeVisitor_CodeEmitor(m_parent, m_builder->getInt32Ty(), node->right).getValue();
        label_logicOpRight = m_builder->GetInsertBlock();
        m_builder->CreateBr(label_endLogicOp);

        func->getBasicBlockList().push_back(label_endLogicOp);
        m_builder->SetInsertPoint(label_endLogicOp);

        llvm::PHINode *phi = m_builder->CreatePHI(m_builder->getInt32Ty(), 2);
        phi->addIncoming(lv, label_old);
        phi->addIncoming(rv, label_logicOpRight);
        m_value = phi;

        return;
    } 

    llvm::Value *lv = ExprNodeVisitor_CodeEmitor(m_parent, NULL, node->left).getValue();
    llvm::Value *rv = ExprNodeVisitor_CodeEmitor(m_parent, lv->getType(), node->right).getValue();
    ASSERT(lv->getType()->isFloatTy() || lv->getType()->isIntegerTy());
    if (node->op == "+") {
        if (lv->getType()->isFloatTy()) m_value = m_builder->CreateFAdd(lv, rv);
        else m_value = m_builder->CreateAdd(lv, rv);
    } else if (node->op == "-") {
        if (lv->getType()->isFloatTy()) m_value = m_builder->CreateFSub(lv, rv);
        else m_value = m_builder->CreateSub(lv, rv);
    } else if (node->op == "*") {
        if (lv->getType()->isFloatTy()) m_value = m_builder->CreateFMul(lv, rv);
        else m_value = m_builder->CreateMul(lv, rv);
    } else if (node->op == "/") {
        if (lv->getType()->isFloatTy()) m_value = m_builder->CreateFDiv(lv, rv);
        else m_value = m_builder->CreateSDiv(lv, rv);
    } else if (node->op == "%") {
        if (lv->getType()->isFloatTy()) m_value = m_builder->CreateFRem(lv, rv);
        else m_value = m_builder->CreateSRem(lv, rv);
    } else if (node->op == "<") {
        if (lv->getType()->isFloatTy()) m_value = m_builder->CreateFCmpOLT(lv, rv);
        else m_value = m_builder->CreateICmpSLT(lv, rv);
    } else if (node->op == "<=") {
        if (lv->getType()->isFloatTy()) m_value = m_builder->CreateFCmpOLE(lv, rv);
        else m_value = m_builder->CreateICmpSLE(lv, rv);
    } else if (node->op == ">") {
        if (lv->getType()->isFloatTy()) m_value = m_builder->CreateFCmpOGT(lv, rv);
        else m_value = m_builder->CreateICmpSGT(lv, rv);
    } else if (node->op == ">=") {
        if (lv->getType()->isFloatTy()) m_value = m_builder->CreateFCmpOGE(lv, rv);
        else m_value = m_builder->CreateICmpSGE(lv, rv);
    } else if (node->op == "==") {
        if (lv->getType()->isFloatTy()) m_value = m_builder->CreateFCmpOEQ(lv, rv);
        else m_value = m_builder->CreateICmpEQ(lv, rv);
    } else if (node->op == "!=") {
        if (lv->getType()->isFloatTy()) m_value = m_builder->CreateFCmpONE(lv, rv);
        else m_value = m_builder->CreateICmpNE(lv, rv);
    } else {
        ASSERT(0);
    }
}
void ExprNodeVisitor_CodeEmitor::visit(ExprNode_UnaryOp *node) {
    if (node->op == "-") {
        m_value = ExprNodeVisitor_CodeEmitor(m_parent, NULL, node->expr).getValue();
        if (m_value->getType()->isFloatTy()) {
            m_value = m_builder->CreateFNeg(m_value);
        } else if (m_value->getType()->isIntegerTy()) {
            m_value = m_builder->CreateNeg(m_value);
        } else {
            ASSERT(0);
        }
    } else if (node->op == "!") {
        m_value = ExprNodeVisitor_CodeEmitor(m_parent, m_builder->getInt32Ty(), node->expr).getValue();
        m_value = m_builder->CreateICmpEQ(m_value, m_builder->getInt32(0));
    } else {
        ASSERT(0);
    }
}
void ExprNodeVisitor_CodeEmitor::visit(ExprNode_TypeCast *node) {
    m_value = ExprNodeVisitor_CodeEmitor(m_parent, NULL, node->expr).getValue();
    explicitTypeCast(m_parent->getParent()->getTypeByString(node->destType));
}
void ExprNodeVisitor_CodeEmitor::visit(ExprNode_Call *node) {
    llvm::Function* func = m_parent->getParent()->getModule()->getFunction(node->funcName);
    vector<llvm::Value*> params;
    int i = 0;
    for (auto iter = func->arg_begin(); iter != func->arg_end(); ++iter, ++i) {
        params.push_back(ExprNodeVisitor_CodeEmitor(m_parent, iter->getType(), node->args[i]).getValue());
    }
    if (i < (int)node->args.size()) {
        ASSERT(func->isVarArg());
        for (; i < (int)node->args.size(); ++i) {
            params.push_back(ExprNodeVisitor_CodeEmitor(m_parent, NULL, node->args[i]).getValue());
        }
    }
    m_value = m_builder->CreateCall(func, params);
}
// ==============================
StmtNodeVisitor_CodeEmitor::StmtNodeVisitor_CodeEmitor(LLVMCompilerImpl *parent, const FunctionProtoPtr &proto):
    m_parent(parent), m_builder(parent->getBuilder()), m_isArgInited(false) {

    llvm::Function *func = m_parent->getModule()->getFunction(proto->name);
    llvm::BasicBlock* label_entry = llvm::BasicBlock::Create(llvm::getGlobalContext(), "entry", func);
    m_builder->SetInsertPoint(label_entry);

    proto->body->acceptVisitor(this);

    if (proto->retType == "void") m_builder->CreateRetVoid();

    removeRedundantIns(func);
}
void StmtNodeVisitor_CodeEmitor::removeRedundantIns(llvm::Function* func) {
    for (auto iter = func->begin(); iter != func->end(); ++iter) {
        auto& insList = iter->getInstList();
        auto iter2 = insList.begin();
        while (iter2 != insList.end() && !iter2->isTerminator()) ++iter2;
        if (iter2 != insList.end()) {
            insList.erase(++iter2, insList.end());
        }
    }
}
void StmtNodeVisitor_CodeEmitor::tryInitArgs() {
    if (m_isArgInited) return;
    m_isArgInited = true;

    llvm::Function* func = m_builder->GetInsertBlock()->getParent();
    for (auto iter = func->arg_begin(); iter != func->arg_end(); ++iter) {
        llvm::Value *var = m_builder->CreateAlloca(iter->getType(), NULL, iter->getName());
        m_localVars.back()[iter->getName()] = var;
        m_builder->CreateStore(iter, var);
    }
}
void StmtNodeVisitor_CodeEmitor::visit(StmtNode_Block *node) {
    m_localVars.push_back(map<string, llvm::Value*>());
    tryInitArgs();
    for (auto &stmt : node->stmts) stmt->acceptVisitor(this);
    m_localVars.pop_back();
}
void StmtNodeVisitor_CodeEmitor::visit(StmtNode_Stmts *node) {
    for (auto &stmt : node->stmts) stmt->acceptVisitor(this);
}
void StmtNodeVisitor_CodeEmitor::visit(StmtNode_Expr *node) {
    ExprNodeVisitor_CodeEmitor(this, NULL, node->expr);
}
void StmtNodeVisitor_CodeEmitor::visit(StmtNode_DefineVarable *node) {
    m_localVars.back()[node->name] = m_builder->CreateAlloca(m_parent->getTypeByString(node->type), NULL, node->name);
}
void StmtNodeVisitor_CodeEmitor::visit(StmtNode_Continue *node) {
    m_builder->CreateBr(m_continueLabels.back());
}
void StmtNodeVisitor_CodeEmitor::visit(StmtNode_Break *node) {
    m_builder->CreateBr(m_breakLabels.back());
}
void StmtNodeVisitor_CodeEmitor::visit(StmtNode_Return *node) {
    llvm::Function *func = m_builder->GetInsertBlock()->getParent();
    if (node->expr != NULL) m_builder->CreateRet(ExprNodeVisitor_CodeEmitor(this, func->getReturnType(), node->expr).getValue());
    else m_builder->CreateRetVoid();
}
void StmtNodeVisitor_CodeEmitor::visit(StmtNode_IfThenElse *node) {
    /*
       cond = eval node->cond
       cond = icmp ne cond 0
       br cond label_then label_else
label_then:
       emit node->thenStmt
       br label_endIf
label_else:
       emit node->elseStmt
       br label_endIf
label_endIf:
     * */
    llvm::Function *func = m_builder->GetInsertBlock()->getParent();
    llvm::BasicBlock *label_then = llvm::BasicBlock::Create(llvm::getGlobalContext(), "then");
    llvm::BasicBlock *label_else = llvm::BasicBlock::Create(llvm::getGlobalContext(), "else");
    llvm::BasicBlock *label_endIf = llvm::BasicBlock::Create(llvm::getGlobalContext(), "endIf");

    llvm::Value *cond = ExprNodeVisitor_CodeEmitor(this, m_builder->getInt32Ty(), node->cond).getValue();
    cond = m_builder->CreateICmpNE(cond, m_builder->getInt32(0));
    m_builder->CreateCondBr(cond, label_then, label_else);

    func->getBasicBlockList().push_back(label_then);
    m_builder->SetInsertPoint(label_then);
    if (node->thenStmt != NULL) node->thenStmt->acceptVisitor(this);
    m_builder->CreateBr(label_endIf);

    func->getBasicBlockList().push_back(label_else);
    m_builder->SetInsertPoint(label_else);
    if (node->elseStmt != NULL) node->elseStmt->acceptVisitor(this);
    m_builder->CreateBr(label_endIf);

    func->getBasicBlockList().push_back(label_endIf);
    m_builder->SetInsertPoint(label_endIf);
}
void StmtNodeVisitor_CodeEmitor::visit(StmtNode_For *node) {
    /*
       emit node->first
       br label_loop
label_loop:
       cond = eval node->second
       cond = icmp ne cond 0
       br cond label_body label_break

label_body:
        emit node->body
        br label_continue

label_continue:
        eval node->third
        br label_loop

label_break:
     * */
    llvm::Function *func = m_builder->GetInsertBlock()->getParent();
    llvm::BasicBlock *label_loop = llvm::BasicBlock::Create(llvm::getGlobalContext(), "loop");
    llvm::BasicBlock *label_body = llvm::BasicBlock::Create(llvm::getGlobalContext(), "body");
    llvm::BasicBlock *label_continue = llvm::BasicBlock::Create(llvm::getGlobalContext(), "continue");
    llvm::BasicBlock *label_break = llvm::BasicBlock::Create(llvm::getGlobalContext(), "break");

    m_continueLabels.push_back(label_continue);
    m_breakLabels.push_back(label_break);

    if (node->first != NULL) node->first->acceptVisitor(this);
    m_builder->CreateBr(label_loop);

    func->getBasicBlockList().push_back(label_loop);
    m_builder->SetInsertPoint(label_loop);
    llvm::Value *cond = NULL;
    if (node->second != NULL) {
        cond = ExprNodeVisitor_CodeEmitor(this, m_builder->getInt32Ty(), node->second).getValue();
    } else cond = m_builder->getInt32(0);
    cond = m_builder->CreateICmpNE(cond, m_builder->getInt32(0));
    m_builder->CreateCondBr(cond, label_body, label_break);

    func->getBasicBlockList().push_back(label_body);
    m_builder->SetInsertPoint(label_body);
    if (node->body != NULL) node->body->acceptVisitor(this);
    m_builder->CreateBr(label_continue);

    func->getBasicBlockList().push_back(label_continue);
    m_builder->SetInsertPoint(label_continue);
    if (node->third) ExprNodeVisitor_CodeEmitor(this, NULL, node->third);
    m_builder->CreateBr(label_loop);

    func->getBasicBlockList().push_back(label_break);
    m_builder->SetInsertPoint(label_break);

    m_continueLabels.pop_back();
    m_breakLabels.pop_back();
}
llvm::Value* StmtNodeVisitor_CodeEmitor::getLocal(const string& name) {
    for (auto iter = m_localVars.rbegin(); iter != m_localVars.rend(); ++iter) {
        if (iter->count(name) > 0) return (*iter)[name];
    }
    return NULL;
}
// ==============================
LLVMCompilerImpl::LLVMCompilerImpl(const SourceFileProtoPtr &proto):
    m_proto(proto) {

    llvm::InitializeNativeTarget();

    m_module = new llvm::Module("LLVM for CMinus", llvm::getGlobalContext());
    m_builder = new llvm::IRBuilder<>(llvm::getGlobalContext());

    string errStr;
    m_ee = llvm::EngineBuilder(m_module).setErrorStr(&errStr).create();
    if (m_ee == NULL) {
        printf("create jit engine failed : %s\n", errStr.c_str());
        ASSERT(0);
    }
}
LLVMCompilerImpl::~LLVMCompilerImpl() {
    delete m_ee;
    delete m_builder;
    // delete m_module;
}
void LLVMCompilerImpl::compile(bool doOptimize) {
    emitGlobalVarables();
    emitFunctions();

    llvm::PassManager pm;
    pm.add(llvm::createVerifierPass());
    if (doOptimize) {
        optimize(&pm);
        pm.add(llvm::createVerifierPass());
    }
    pm.run(*m_module);
}
void LLVMCompilerImpl::emitGlobalVarables() {
    for (auto &_stmt : m_proto->globalVars) {
        auto stmt = static_cast<StmtNode_DefineVarable*>(_stmt.get()); 
        auto type = getTypeByString(stmt->type);
        auto var = llvm::cast<llvm::GlobalVariable>(m_module->getOrInsertGlobal(stmt->name, type));
        var->setInitializer(llvm::Constant::getNullValue(type));
    }
}
void LLVMCompilerImpl::emitFunctionDeclare(const FunctionProtoPtr &proto) {
    vector<llvm::Type*> argTypes;
    for (auto &p : proto->argsTypeID) {
        argTypes.push_back(getTypeByString(p.first));
    }
    auto type = llvm::FunctionType::get(getTypeByString(proto->retType), argTypes, proto->isVarArgs);
    auto func = llvm::cast<llvm::Function>(m_module->getOrInsertFunction(proto->name, type));
    int i = 0;
    for (auto iter = func->arg_begin(); iter != func->arg_end(); ++iter, ++i) {
        iter->setName(proto->argsTypeID[i].second);
    }
}
void LLVMCompilerImpl::emitFunctions() {
    for (auto &proto : m_proto->externFuncs) emitFunctionDeclare(proto);
    for (auto &proto : m_proto->funcs) emitFunctionDeclare(proto);
    for (auto &proto : m_proto->funcs) StmtNodeVisitor_CodeEmitor(this, proto);
}
void LLVMCompilerImpl::optimize(llvm::PassManager *pm) {
    pm->add(llvm::createStripSymbolsPass(true));

    llvm::PassManagerBuilder pmBuilder;
    pmBuilder.Inliner = llvm::createFunctionInliningPass();
    pmBuilder.OptLevel = 3;
    pmBuilder.populateModulePassManager(*pm);
}
void LLVMCompilerImpl::print(const string& fileName) {
    string errStr;
    llvm::raw_fd_ostream os(fileName.c_str(), errStr);
    m_module->print(os, NULL);
}

void LLVMCompilerImpl::run() {
    llvm::Function *func = m_module->getFunction("main");
    auto mainFunc = (void(*)())m_ee->getPointerToFunction(func);
    mainFunc();
}
llvm::Type* LLVMCompilerImpl::getTypeByString(const string& s) {
    if (s == "int") return m_builder->getInt32Ty();
    else if (s == "float") return m_builder->getFloatTy();
    else if (s == "void") return m_builder->getVoidTy();
    else if (s == "char*") return m_builder->getInt8PtrTy();
    else {
        ASSERT(0);
        return NULL;
    }
}
// ==============================
LLVMCompiler::LLVMCompiler(const SourceFileProtoPtr &proto): 
    m_impl(new LLVMCompilerImpl(proto)){
}
LLVMCompiler::~LLVMCompiler() {
    delete m_impl;
}
void LLVMCompiler::compile(bool doOptimize) {
    m_impl->compile(doOptimize);
}
void LLVMCompiler::print(const string& fileName) {
    m_impl->print(fileName);
}
void LLVMCompiler::run() {
    m_impl->run();
}
