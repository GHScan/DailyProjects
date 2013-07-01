
#include "pch.h"
#include "BEType.h"

const BEType* BETypeManager::getType(const string &typeName) const {
    auto iter = m_types.find(typeName);
    if (iter == m_types.end()) return NULL;
    return &iter->second;
}

BETypeManager::BETypeManager() {
    m_types["int"] = BEType("int", 4);
    m_types["char*"] = BEType("char*", 4);
    m_types["void"] = BEType("void", 1);
}
BETypeManager::~BETypeManager() {
}
