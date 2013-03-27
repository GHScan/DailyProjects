
#include "pch.h"
#include "Ast.h"
#include "LuaFunction.h"
#include "SymbolTable.h"

// ExpNode
ConstExpNode::ConstExpNode(LuaFunctionMeta *meta, const LuaValue& v):
    index(meta->getConstIndex(v)) {
}

const string& LocalVarExpNode::getName(const LuaFunctionMeta *meta) const {
    return meta->nameTable[nameIdx];
}

const string& UpValueVarExpNode::getName(const LuaFunctionMeta *meta) const {
    return meta->nameTable[nameIdx];
}

// StmtNode
RangeForStmtNode::RangeForStmtNode(LuaFunctionMeta *meta, const string& name) {
    SymbolTable::top()->declareLocal(name);
    index = SymbolTable::top()->getLocalIndex(name);
    nameIdx = meta->getNameIndex(name);
}
const string& RangeForStmtNode::getName(const LuaFunctionMeta *meta) const {
    return meta->nameTable[nameIdx];
}

void IteraterForStmtNode::pushName(LuaFunctionMeta *meta, const string& name) {
    SymbolTable::top()->declareLocal(name);
    indexs.push_back(SymbolTable::top()->getLocalIndex(name));
    nameIdxs.push_back(meta->getNameIndex(name));
}
const string& IteraterForStmtNode::getName(const LuaFunctionMeta *meta, int off) const {
    return meta->nameTable[nameIdxs[off]];
}
