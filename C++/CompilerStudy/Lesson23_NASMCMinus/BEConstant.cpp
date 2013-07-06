
#include "pch.h"
#include "BEConstant.h"
#include "BEType.h"
#include "IDGenerator.h"

BEConstantString::BEConstantString(const string &_str) : 
    BEConstant(IDGenerator::instance()->generateName("str"), BETypeManager::instance()->get("char*")), str(_str){
}

BEConstantInt::BEConstantInt(int _num): 
    BEConstant(IDGenerator::instance()->generateName("int"), BETypeManager::instance()->get("int")), num(_num){
}

BEConstant* BEConstantPool::get(int num) {
    auto iter = m_constInts.find(num);
    if (iter != m_constInts.end()) return &iter->second;
    return &(m_constInts[num] = BEConstantInt(num));
}
BEConstant* BEConstantPool::get(const string &str) {
    auto iter = m_constStrs.find(str);
    if (iter != m_constStrs.end()) return &iter->second;
    return &(m_constStrs[str] = BEConstantString(str));
}
