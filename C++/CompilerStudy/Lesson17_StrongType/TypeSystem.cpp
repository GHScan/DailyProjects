
#include "pch.h"
#include "TypeSystem.h"

TypeSystem::TypeSystem()
{
    addType("void", new SimpleType(1));
    addType("char", new SimpleType(1));
    addType("int", new SimpleType(4));
}
TypeSystem::~TypeSystem()
{
    for (auto p : m_name2Type) delete p.second;
    for (auto p : m_type2Pointer) delete p.second;
    for (auto p : m_type2Array) {
        for (auto p2 : p.second) delete p2.second;
    }
}

IType* TypeSystem::addType(const string& name, IType *type)
{
    ASSERT(m_name2Type.count(name) == 0);
    return m_name2Type[name] = type;
}
IType* TypeSystem::getType(const string& name)
{
    if (m_name2Type.count(name) == 0) return NULL;
    return m_name2Type[name];
}
IType* TypeSystem::getPointer(IType *type)
{
    if (m_type2Pointer.count(type) == 0) {
        m_type2Pointer[type] = new PointerType(type);
    }
    return m_type2Pointer[type];
}
IType* TypeSystem::getArray(IType *elemType, int n)
{
    if (m_type2Array[elemType].count(n) == 0) {
        m_type2Array[elemType][n] = new ArrayType(elemType, n);
    }
    return m_type2Array[elemType][n];
}
IType* TypeSystem::getFunc(IType* ret, const vector<IType*>& args)
{
    string sig = format("%x,", ret);
    for (auto t : args) sig += format("%x,", t);
    if (m_type2Func.count(sig) == 0) {
        m_type2Func[sig] = new FunctionType(ret, args);
    }
    return m_type2Func[sig];
}
