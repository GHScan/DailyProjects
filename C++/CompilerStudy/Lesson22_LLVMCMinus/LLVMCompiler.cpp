
#include "pch.h"

// TODO: 
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/Transforms/IPO/PassManagerBuilder.h>
#include <llvm/Transforms/IPO.h>

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
    void castToTargetType();
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
    LLVMCompilerImpl *m_parent;
    vector<map<string, llvm::Value*> > m_localVars;
};
// ==============================
class LLVMCompilerImpl {
public:
    LLVMCompilerImpl(const SourceFileProtoPtr &proto);
    ~LLVMCompilerImpl();
    void compile(bool doOptimize);
    void print();
    void run();

    llvm::Module* getModule() { return m_module; }
    llvm::IRBuilder<>* getBuilder() { return m_builder; }

    llvm::Type* getTypeByString(const string& s);
private:
    void emitGlobalVarables();
    void emitFunctions();
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
    castToTargetType();
}
void ExprNodeVisitor_CodeEmitor::castToTargetType() {
    ASSERT(m_value != NULL);
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
    m_value = m_parent->getLocal(node->name);
    if (m_value == NULL) {
        m_value = m_parent->getParent()->getModule()->getGlobalVariable(node->name);
    }
    ASSERT(m_value != NULL);
}
void ExprNodeVisitor_CodeEmitor::visit(ExprNode_Assignment *node) {
}
void ExprNodeVisitor_CodeEmitor::visit(ExprNode_BinaryOp *node) {
}
void ExprNodeVisitor_CodeEmitor::visit(ExprNode_UnaryOp *node) {
}
void ExprNodeVisitor_CodeEmitor::visit(ExprNode_TypeCast *node) {
}
void ExprNodeVisitor_CodeEmitor::visit(ExprNode_Call *node) {
}
// ==============================
StmtNodeVisitor_CodeEmitor::StmtNodeVisitor_CodeEmitor(LLVMCompilerImpl *parent, const FunctionProtoPtr &proto):
    m_parent(parent) {
    proto->body->acceptVisitor(this);
}
void StmtNodeVisitor_CodeEmitor::visit(StmtNode_Block *node) {
}
void StmtNodeVisitor_CodeEmitor::visit(StmtNode_Stmts *node) {
}
void StmtNodeVisitor_CodeEmitor::visit(StmtNode_Expr *node) {
}
void StmtNodeVisitor_CodeEmitor::visit(StmtNode_DefineVarable *node) {
}
void StmtNodeVisitor_CodeEmitor::visit(StmtNode_Continue *node) {
}
void StmtNodeVisitor_CodeEmitor::visit(StmtNode_Break *node) {
}
void StmtNodeVisitor_CodeEmitor::visit(StmtNode_Return *node) {
}
void StmtNodeVisitor_CodeEmitor::visit(StmtNode_IfThenElse *node) {
}
void StmtNodeVisitor_CodeEmitor::visit(StmtNode_For *node) {
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
    m_module = new llvm::Module("LLVM for CMinus", llvm::getGlobalContext());
    m_builder = new llvm::IRBuilder<>(llvm::getGlobalContext());
    m_ee = llvm::EngineBuilder(m_module).create();
}
LLVMCompilerImpl::~LLVMCompilerImpl() {
    delete m_ee;
    delete m_builder;
    delete m_module;
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
        m_module->getOrInsertGlobal(stmt->name, getTypeByString(stmt->type));
    }
}
void LLVMCompilerImpl::emitFunctions() {
    // for (auto &proto : m_proto->externFuncs) {

    for (auto &proto : m_proto->funcs) {
        vector<llvm::Type*> argTypes;
        for (auto &p : proto->argsTypeID) {
            argTypes.push_back(getTypeByString(p.first));
        }
        auto type = llvm::FunctionType::get(getTypeByString(proto->retType), argTypes, proto->isVarArgs);
        auto func = llvm::dyn_cast<llvm::Function>(m_module->getOrInsertFunction(proto->name, type));
        int i = 0;
        for (auto iter = func->arg_begin(); iter != func->arg_end(); ++iter, ++i) {
            iter->setName(proto->argsTypeID[i].second);
        }
    }
    for (auto &proto : m_proto->funcs) {
        new StmtNodeVisitor_CodeEmitor(this, proto);
    }
}
void LLVMCompilerImpl::optimize(llvm::PassManager *pm) {
    pm->add(llvm::createStripSymbolsPass(true));

    llvm::PassManagerBuilder pmBuilder;
    pmBuilder.Inliner = llvm::createFunctionInliningPass();
    pmBuilder.OptLevel = 3;
    pmBuilder.populateModulePassManager(*pm);
}
void LLVMCompilerImpl::print() {
    m_module->dump();
}
void LLVMCompilerImpl::run() {
    auto mainFunc = (void(*)())m_ee->getPointerToNamedFunction("main");
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
void LLVMCompiler::print() {
    m_impl->print();
}
void LLVMCompiler::run() {
    m_impl->run();
}
