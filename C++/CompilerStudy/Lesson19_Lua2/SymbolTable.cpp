
#include "pch.h"

#include "SymbolTable.h"
#include "LuaFunction.h"

stack<SymbolTable*> SymbolTable::s_stack;

void SymbolTable::push(const LuaFunctionMetaPtr& meta) {
    int level = (int)s_stack.size();
    s_stack.push(new SymbolTable(meta, s_stack.empty() ? NULL : s_stack.top(), level));
}
void SymbolTable::pop() {
    delete s_stack.top();
    s_stack.pop();
}

SymbolTable::SymbolTable(const LuaFunctionMetaPtr& meta, SymbolTable* prev, int level):
    m_lastLocalIdx(0), m_meta(meta), m_prev(prev) {
    meta->level = level;
    pushBlock();
}
SymbolTable::~SymbolTable() {
    popBlock();
}

void SymbolTable::declareLocal(const string& name) {
    ASSERT(m_blocks.back().count(name) == 0);
    m_blocks.back()[name] = m_lastLocalIdx++;
    m_meta->localCount = max(m_meta->localCount, m_lastLocalIdx);
}
int SymbolTable::genInternalLocal(const string& name) {
    int idx = -1;
    for (int i = 0; i < 1000; ++i) {
        string realName = format("%s_%d", name.c_str(), i);
        if (m_blocks.back().count(realName) == 0) {
            declareLocal(realName);
            idx = getLocalIdx(realName);
            break;
        }
    }
    ASSERT(idx != -1);
    return idx;
}

int SymbolTable::getLocalIdx(const string& name) const {
    for (auto iter = m_blocks.rbegin(); iter != m_blocks.rend(); ++iter) {
        auto iter2 = iter->find(name);
        if (iter2 != iter->end()) return iter2->second;
    }
    return -1;
}
int SymbolTable::getUpValueIdx(const string& name) {
    assert(getLocalIdx(name) == -1);
    auto iter = m_upValues.find(name);
    if (iter != m_upValues.end()) {
        return iter->second;
    }

    SymbolTable *t = m_prev;
    while (t != NULL) {
        int localIdx = -1;
        if ((localIdx = t->getLocalIdx(name)) != -1) {
            m_upValues[name] = (int)m_meta->upValues.size();
            m_meta->upValues.push_back(make_pair(t->meta()->level, localIdx));
            return m_upValues[name];
        }
    }
    return -1;
}

void SymbolTable::pushBlock() {
    m_blocks.push_back(map<string, int>());
}
void SymbolTable::popBlock() {
    m_lastLocalIdx -= (int)m_blocks.back().size();
    m_blocks.pop_back();
}
