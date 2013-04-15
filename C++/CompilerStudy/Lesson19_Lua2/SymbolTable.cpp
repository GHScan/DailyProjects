
#include "pch.h"

#include "SymbolTable.h"
#include "LuaFunction.h"

stack<SymbolTable*> SymbolTable::s_stack;

void SymbolTable::push(LuaFunctionMeta* meta) {
    int level = (int)s_stack.size();
    s_stack.push(new SymbolTable(meta, s_stack.empty() ? NULL : s_stack.top(), level));
}
void SymbolTable::pop() {
    delete s_stack.top();
    s_stack.pop();
}

SymbolTable::SymbolTable(LuaFunctionMeta* meta, SymbolTable* prev, int level):
    m_lastLocalIdx(0), m_meta(meta), m_prev(prev) {
    meta->level = level;
}
SymbolTable::~SymbolTable() {
}

void SymbolTable::declareLocal(const string& name) {
    ASSERT(m_blocks.back().count(name) == 0);
    m_blocks.back()[name] = m_lastLocalIdx++;
}
int SymbolTable::getLocalIdx(const string& name) const {
    for (auto iter = m_blocks.rbegin(); iter != m_blocks.rend(); ++iter) {
        auto iter2 = iter->find(name);
        if (iter2 != iter->end()) return iter2->second;
    }
    return -1;
}
bool SymbolTable::getUpValue(const string& name, pair<int, int>& uvInfo) {
    assert(getLocalIdx(name) == -1);
    SymbolTable *t = m_prev;
    while (t != NULL) {
        if ((uvInfo.second = t->getLocalIdx(name)) != -1) {
            uvInfo.first = t->meta()->level;
            return true;
        }
    }
    return false;
}

void SymbolTable::pushBlock() {
    m_blocks.push_back(map<string, int>());
}
void SymbolTable::popBlock() {
    m_lastLocalIdx -= (int)m_blocks.back().size();
    m_blocks.pop_back();
}
