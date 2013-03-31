
#include "pch.h"
#include "SymbolTable.h"
#include "LuaFunction.h"

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

SymbolTable::SymbolTable(): m_lastLocalIdx(0), m_maxLocalIdx(0) {
    beginBlock();
}
SymbolTable::~SymbolTable() {
    endBlock();
}
void SymbolTable::beginBlock() {
    m_blocks.push_back(map<string, int>());
}
void SymbolTable::endBlock() {
    m_lastLocalIdx -= (int)m_blocks.back().size();
    m_blocks.pop_back();
}
int SymbolTable::getBlockSize() const {
    return (int)m_blocks.back().size();
}
int SymbolTable::getBlockOff() const {
    int r = 0;
    for (int i = 0; i < (int)m_blocks.size() - 1; ++i) {
        r += (int)m_blocks[i].size();
    }
    return r;
}

void SymbolTable::declareLocal(const string& name) {
    ASSERT(m_blocks.back().count(name) == 0);
    m_blocks.back()[name] = m_lastLocalIdx++;
    m_maxLocalIdx = max(m_maxLocalIdx, m_lastLocalIdx);
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
            auto lidx = s_stack[i].getLocalIndex(name);
            if (lidx != -1) {
                int uvIdx = (int)m_upValues.size();
                LuaFunctionMeta::stack()->top()->upValues.push_back(make_pair(i, lidx));
                m_upValues[name] = uvIdx;
                return uvIdx;
            }
        }
        return -1;
    } else {
        return iter->second;
    }
}

int SymbolTable::getLocalCount() const {
    return m_maxLocalIdx;
}
