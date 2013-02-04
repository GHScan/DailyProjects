
#include "pch.h"

#include "AST.h"
#include "TypeSystem.h"
#include "Runtime.h"

class ExpNodeVisitor_Eval:
    public IExpNodeVisitor
{
public:
    ExpNodeVisitor_Eval(Thread *thread, ExpNodePtr node)
    {
        node->acceptVisitor(this);
    }
private:
    virtual void visit(ExpNode_ConstantInt* node)
    {
    }
    virtual void visit(ExpNode_ConstantLiteral* node)
    {
    }
    virtual void visit(ExpNode_Variable* node)
    {
    }
    virtual void visit(ExpNode_Conversion* node)
    {
    }
    virtual void visit(ExpNode_BinaryOp* node)
    {
    }
    virtual void visit(ExpNode_UnaryOp* node)
    {
    }
    virtual void visit(ExpNode_Addr* node)
    {
    }
    virtual void visit(ExpNode_Unref* node)
    {
    }
    virtual void visit(ExpNode_Field* node)
    {
    }
    virtual void visit(ExpNode_ArrayElem* node)
    {
    }
    virtual void visit(ExpNode_Call* node)
    {
    }
    virtual void visit(ExpNode_Assign* node)
    {
    }
    virtual void visit(ExpNode_Sizeof* node)
    {
    }
private:
    Thread *m_thread;
};
class StmtNodeVisitor_Executor:
    public IStmtNodeVisitor
{
public:
    StmtNodeVisitor_Executor(Thread *thread, StmtNodePtr node)
    {
        node->acceptVisitor(this);
    }
private:
    virtual void visit(StmtNode_Exp* node)
    {
    }
    virtual void visit(StmtNode_Block* node)
    {
    }
    virtual void visit(StmtNode_DefineLocal* node)
    {
    }
    virtual void visit(StmtNode_Break* node)
    {
    }
    virtual void visit(StmtNode_Continue* node)
    {
    }
    virtual void visit(StmtNode_Return* node)
    {
    }
    virtual void visit(StmtNode_For* node)
    {
    }
    virtual void visit(StmtNode_IfElse* node)
    {
    }
    virtual void visit(StmtNode_Switch* node)
    {
    }
private:
    Thread *m_thread;
};

ASTFunction::ASTFunction(StmtNodePtr stmt, IType *type):
    m_stmt(stmt), m_type(type) {}
void ASTFunction::call(Thread *thread)
{
    StmtNodeVisitor_Executor visitor(thread, m_stmt);
}

GlobalEnvironment::GlobalEnvironment():
    m_funcPreMain(new ASTFunction(
                StmtNodePtr(new StmtNode_Block()), 
                TypeSystem::instance()->getFunc(
                    TypeSystem::instance()->getType("void"), vector<IType*>())))
{
}

void run()
{
    Thread mainThread;
    GlobalEnvironment::instance()->getFuncPreMain()->call(&mainThread);
    GlobalEnvironment::instance()->getFunc("main")->call(&mainThread);
}
