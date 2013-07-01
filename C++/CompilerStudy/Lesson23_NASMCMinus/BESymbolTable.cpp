
#include "pch.h"
#include "BESymbolTable.h"
#include "BEType.h"

BESymbolTable::BESymbolTable(BESymbolTable *prevTable): 
    m_prevTable(prevTable), m_startOff(prevTable == NULL ? 0 : prevTable->getEndOff()), m_endOff(0) {
    m_endOff = m_startOff;
}
BESymbolTable::~BESymbolTable() {
}

BESymbol* BESymbolTable::declare(const string &name, BEType *type) {
    ASSERT(m_symbols.count(name) == 0);
    BESymbol symbol = {this, name, type, m_endOff};
    m_endOff += symbol.type->size;
    return &(m_symbols[name] = symbol);
}
void BESymbolTable::undeclare(const string& name) {
    auto iter = m_symbols.find(name);
    ASSERT(iter != m_symbols.end());
    ASSERT(m_endOff == iter->second.off + iter->second.type->size);
    m_endOff = iter->second.off;
    m_symbols.erase(iter);
}
BESymbol* BESymbolTable::get(const string &name) {
    auto iter = m_symbols.find(name);
    if (iter != m_symbols.end()) return &iter->second;
    if (m_prevTable != NULL) return m_prevTable->get(name);
    return NULL;
}
int BESymbolTable::getStartOff() const {
    return m_startOff;
}
int BESymbolTable::getEndOff() const {
    return m_endOff;
}
