
#include "pch.h"
#include "TypeSystem.h"
#include "SymbolTable.h"

void SymbolTable::addSymbol(const string& name, IType *type)
{
    SymbolDescrib describ = {(int)m_id2Symbol.size(), m_off, type};
    m_id2Symbol.push_back(describ);
    m_name2Symbol[name] = &m_id2Symbol.back();
    m_off += type->getSize();
}
const SymbolDescrib* SymbolTable::getSymbol(const string& name)
{
    if (m_name2Symbol.count(name) > 0) return m_name2Symbol[name];
    if (m_parent == NULL) return NULL;
    else return m_parent->getSymbol(name);
}
const SymbolDescrib* SymbolTable::getSymbol(int idx)
{
    return &m_id2Symbol[idx];
}

void SymbolTableStack::push()
{
    SymbolTable* back = m_stack.empty() ? NULL : m_stack.back().get();
    m_stack.push_back(SymbolTablePtr(new SymbolTable(back)));
}
void SymbolTableStack::pop()
{
    m_stack.pop_back();
}
void SymbolTableStack::addSymbol(const string& name, IType *type)
{
    m_stack.back()->addSymbol(name, type);
}
const SymbolDescrib* SymbolTableStack::getSymbol(const string& name)
{
    return m_stack.back()->getSymbol(name);
}
int SymbolTableStack::getOffset()
{
    return m_stack.back()->getOffset();
}
