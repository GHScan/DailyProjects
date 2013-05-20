
#include "pch.h"
#include "SymbolTable.h"

SymbolTable* SymbolTable::topTable() {
    return s_tables.back();
}
void SymbolTable::pushTable(FuncMetaPtr &meta) {
    s_tables.push_back(new SymbolTable(meta));
}
void SymbolTable::popTable() {
    delete s_tables.back();
    s_tables.pop_back();
}

SymbolTable::SymbolTable(FuncMetaPtr &meta): 
    m_meta(meta), m_maxLocalIdx(0), m_curLocalIdx(0) {
    pushBlock();
}
SymbolTable::~SymbolTable() {
    popBlock();
    ASSERT(m_blockNames.empty() && m_curLocalIdx == 0);
}

FuncMetaPtr& SymbolTable::getMeta() {
    return m_meta;
}

void SymbolTable::pushBlock() {
    m_blockNames.push_back(map<string, int>());
}
void SymbolTable::popBlock() {
    m_curLocalIdx -= (int)m_blockNames.back().size();
    m_blockNames.pop_back();
}
pair<int, int> SymbolTable::getLocalOffSize() {
    int off = 0;
    for (int i = 0; i < (int)m_blockNames.size() - 1; ++i) {
        off += (int)m_blockNames[i].size();
    }
    return make_pair(off, off + (int)m_blockNames.back().size());
}
void SymbolTable::declareLocal(const string& name) {
    ASSERT(m_blockNames.back().count(name) == 0);
    m_blockNames.back()[name] = m_curLocalIdx++;
    m_maxLocalIdx = max(m_maxLocalIdx, m_curLocalIdx);
}
int SymbolTable::getLocalIdx(const string& name) {
    for (auto riter = m_blockNames.rbegin(); riter != m_blockNames.rend(); ++riter) {
        auto iter = riter->find(name);
        if (iter != riter->end()) return iter->second;
    }
    return -1;
}
vector<SymbolTable*> SymbolTable::s_tables;
