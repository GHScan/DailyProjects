
#include "pch.h"

#include "AST.h"
#include "Runtime.h"
#include "TypeSystem.h"
#include "SymbolTable.h"

//---------- ExpNode

ExpNode_ConstantInt::ExpNode_ConstantInt(int v): value(v)
{
    type = TypeSystem::instance()->getType("int");
}
ExpNode_ConstantString::ExpNode_ConstantString(const string& s):
    str(StringPool::instance()->get(s))
{
    TypeSystem *ts = TypeSystem::instance();
    type = ts->getPointer(ts->getType("char"));
}

ExpNode_Variable::ExpNode_Variable(const string& name)
{
    auto symbol = SymbolTableManager::stack()->getSymbol(name);
    if (symbol == NULL) {
        isGlobal = true;
        symbol = SymbolTableManager::global()->getSymbol(name);
    }
    ASSERT(symbol != NULL);
    type = symbol->type;
    offset = symbol->off;
}

ExpNode_Conversion::ExpNode_Conversion(const ExpNodePtr& _left, IType* type):
    left(_left)
{
    this->type = type;
}

ExpNode_BinaryOp::ExpNode_BinaryOp(const ExpNodePtr& _left, const ExpNodePtr& _right, BinOp _op): 
    left(_left), right(_right), op(_op)
{
    type = left->type;
}

ExpNode_UnaryOp::ExpNode_UnaryOp(const ExpNodePtr& _left, UnaryOp _op): 
    left(_left), op(_op)
{
    type = left->type;
}

ExpNode_Addr::ExpNode_Addr(const ExpNodePtr& _left): left(_left)
{
    type = TypeSystem::instance()->getPointer(left->type);
}

ExpNode_Unref::ExpNode_Unref(const ExpNodePtr& _left): left(_left)
{
    auto ptype = dynamic_cast<PointerType*>(left->type);
    ASSERT(ptype != NULL);
    type = ptype->refType;
}

ExpNode_Field::ExpNode_Field(const ExpNodePtr& _left, const string& _fieldName):
    left(_left), fieldName(StringPool::instance()->get(_fieldName))
{
    auto table = SymbolTableManager::getTypeTable(left->type);
    auto symbol = table->getSymbol(_fieldName);
    type = symbol->type;
}

ExpNode_ArrayElem::ExpNode_ArrayElem(const ExpNodePtr& _left, const ExpNodePtr& _right): left(_left), right(_right)
{
    auto arrayType = dynamic_cast<ArrayType*>(left->type);
    ASSERT(arrayType != NULL);
    ASSERT(right->type == TypeSystem::instance()->getType("int"));
    type = arrayType->elemType;
}

ExpNode_Call::ExpNode_Call(const string& _name, const vector<ExpNodePtr>& _args): 
    name(StringPool::instance()->get(_name)), args(_args)
{
    auto ftype = dynamic_cast<FunctionType*>(SymbolTableManager::global()->getSymbol(_name)->type);
    ASSERT(ftype != NULL);
    type = ftype->retT;
    for (int i = 0; i < ftype->argsT.size(); ++i) {
        ASSERT(ftype->argsT[i] == _args[i]->type);
    }
}

ExpNode_Assign::ExpNode_Assign(const ExpNodePtr& _left, const ExpNodePtr& _right): left(_left), right(_right)
{
    type = left->type;
}

ExpNode_UnaryOp::UnaryOp ExpNode_UnaryOp::string2op(const string& str)
{
    if (str == "!") return UO_Not;
    else if (str == "++") return UO_Inc;
    else if (str == "--") return UO_Dec;
    else ASSERT(0);
}

ExpNode_BinaryOp::BinOp ExpNode_BinaryOp::string2op(const string& str)
{
    if (str == "+") return BO_Add;
    else if (str == "-") return BO_Sub;
    else if (str == "*") return BO_Mul;
    else if (str == "/") return BO_Div;
    else if (str == "%") return BO_Mod;

    else if (str == "<") return BO_Less;
    else if (str == "<=") return BO_LessEq;
    else if (str == "==") return BO_Equal;
    else if (str == ">") return BO_Greater;
    else if (str == ">=") return BO_GreaterEq;

    else if (str == "&&") return BO_And;
    else if (str == "||") return BO_Or;
    else ASSERT(0);
}

//---------- StmtNode
StmtNode_Exp::StmtNode_Exp(const ExpNodePtr& _exp): exp(_exp)
{
}

StmtNode_Block::StmtNode_Block(const vector<StmtNodePtr>& _stmts):
    stmts(_stmts)
{
}

StmtNode_DefineLocal::StmtNode_DefineLocal(const string& _name, IType *_type):
    name(StringPool::instance()->get(_name)), type(_type)
{
}
