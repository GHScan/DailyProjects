
#include "pch.h"
#include "BEVariable.h"
#include "BESymbolTable.h"

BEVariableManager::~BEVariableManager() {
}
BEVariable* BEVariableManager::createVariable(BESymbol *symbol, bool isInMemory, BEx86Register *reg) {
    ASSERT(m_vars.count(symbol->name) == 0);
    BEVariable var = { reg, symbol, isInMemory };
    return &(m_vars[symbol->name] = var);
}
void BEVariableManager::destroy(const string &name) {
    auto iter = m_vars.find(name);
    ASSERT(iter != m_vars.end());
    m_vars.erase(iter);
}
BEVariable* BEVariableManager::get(const string &name) {
    auto iter = m_vars.find(name);
    if (iter == m_vars.end()) return NULL;
    return &iter->second;
}
