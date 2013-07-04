
#include "pch.h"
#include "BEx86FileBuilder.h"
#include "BEConstant.h"
#include "BESymbolTable.h"
#include "BEx86FunctionBuilder.h"

BEx86FileBuilder::BEx86FileBuilder():
    m_constantPool(new BEConstantPool()), m_globalSymbolTable(new BESymbolTable(NULL)) {
}
BEx86FileBuilder::~BEx86FileBuilder() {
    for (auto p : m_funcBuilders) delete p.second;
    delete m_constantPool;
    delete m_globalSymbolTable;
}

BEx86FunctionBuilder* BEx86FileBuilder::createFunctionBuilder(const string &name) { 
    ASSERT(m_funcBuilders.count(name) == 0);
    return m_funcBuilders[name] = new BEx86FunctionBuilder(this);
}
BEx86FunctionBuilder* BEx86FileBuilder::getFunctionBuilder(const string &name) {
    if (m_funcBuilders.count(name) == 0) return NULL;
    return m_funcBuilders[name];
}

void BEx86FileBuilder::setAsExternSymbol(const string &name) {
    m_externSymbols.insert(name);
}
bool BEx86FileBuilder::isExternSymbol(const string &name) {
    return m_externSymbols.count(name) > 0;
}
