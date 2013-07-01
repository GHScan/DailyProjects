
#include "pch.h"
#include "BEConstant.h"
#include "BEType.h"

BEConstantString::BEConstantString(const string &_str)
    : BEConstant(BETypeManager::instance()->getType("char*")), str(_str){
}

BEConstantInt::BEConstantInt(int _num): 
    BEConstant(BETypeManager::instance()->getType("int")), num(_num){
}

BEConstant* BEContantPool::getConstant(int num) {
    auto iter = m_constInts.find(num);
    if (iter != m_constInts.end()) return &iter->second;
    return &(m_constInts[num] = BEConstantInt(num));
}
BEConstant* BEContantPool::getConstant(const string &str) {
    auto iter = m_constStrs.find(str);
    if (iter != m_constStrs.end()) return &iter->second;
    return &(m_constStrs[str] = BEConstantString(str));
}
