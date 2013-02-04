
#include "pch.h"

#include "AST.h"
#include "Runtime.h"

ExpNode_ConstantString::ExpNode_ConstantString(const string& s):
    str(StringPool::instance()->get(s))
{
}

ExpNode_Variable::ExpNode_Variable(const string& _name):
    name(StringPool::instance()->get(_name))
{
}

ExpNode_Field::ExpNode_Field(const string& _fieldName):
    fieldName(StringPool::instance()->get(_fieldName))
{
}

ExpNode_Call::ExpNode_Call(const string& _name):
    name(StringPool::instance()->get(_name))
{
}

StmtNode_Define::StmtNode_Define(const string& _name, IType *_type):
    name(StringPool::instance()->get(_name)), type(_type)
{
}
