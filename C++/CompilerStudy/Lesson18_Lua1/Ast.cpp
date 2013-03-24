
#include "pch.h"
#include "Ast.h"
#include "LuaFunction.h"

const string& LocalVarExpNode::getName(const LuaFunctionMeta *meta) const {
    return meta->nameTable[nameIdx];
}

const string& UpValueVarExpNode::getName(const LuaFunctionMeta *meta) const {
    return meta->nameTable[nameIdx];
}

const string& RangeForStmtNode::getName(const LuaFunctionMeta *meta) const {
    return meta->nameTable[nameIdx];
}

const string& IteraterForStmtNode::getName(const LuaFunctionMeta *meta, int idx) const {
    return meta->nameTable[nameIdxs[idx]];
}
