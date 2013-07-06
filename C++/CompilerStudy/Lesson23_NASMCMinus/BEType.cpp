
#include "pch.h"
#include "BEType.h"

const BEType* BETypeManager::get(const string &name) const {
    auto iter = m_types.find(name);
    if (iter == m_types.end()) return NULL;
    return iter->second;
}
const BEType* BETypeManager::getFunc() const {
    return get("func");
}

BETypeManager::BETypeManager() {
    m_types["func"] = new BEType("func", 4);
    m_types["void"] = new BEType("void", 1);
    m_types["char"] = new BEType("char", 1);
    m_types["int"] = new BEType("int", 4);
    m_types["char*"] = new BEType_Array(get("char"), 0);
}
BETypeManager::~BETypeManager() {
    for (auto nameType : m_types) delete nameType.second;
}
