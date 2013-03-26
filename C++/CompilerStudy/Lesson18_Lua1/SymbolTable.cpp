
#include "pch.h"
#include "SymbolTable.h"

void SymbolTable::push() {
    s_stack.push_back(SymbolTable());
}
void SymbolTable::pop() {
    s_stack.pop_back();
}
SymbolTable* SymbolTable::top() {
    return &s_stack.back();
}
vector<SymbolTable> SymbolTable::s_stack;

SymbolTable::SymbolTable(): m_lastLocalIdx(0) {
}
void SymbolTable::beginBlock() {
    m_blocks.push_back(map<string, int>());
}
void SymbolTable::endBlock() {
    m_lastLocalIdx -= (int)m_blocks.back().size();
    m_blocks.pop_back();
}

void SymbolTable::declareLocal(const string& name) {
    ASSERT(m_blocks.back().count(name) == 0);
    m_blocks.back()[name] = m_lastLocalIdx++;
}
int SymbolTable::getLocalIndex(const string& name) {
    for (int i = (int)m_blocks.size() - 1; i >= 0; --i) {
        auto iter = m_blocks[i].find(name);
        if (iter != m_blocks[i].end()) return iter->second;
    }
    return -1;
}
int SymbolTable::getUpValueIndex(const string& name) {
    assert(getLocalIndex(name) == -1);
    auto iter = m_upValues.find(name);
    if (iter == m_upValues.end()) {
        for (int i = (int)s_stack.size() - 2; i >= 0; --i) {
            if (s_stack[i].getLocalIndex(name) != -1) {
                int uvIdx = (int)m_upValues.size();
                m_upValues[name] = uvIdx;
                return uvIdx;
            }
        }
        return -1;
    } else {
        return iter->second;
    }
}
