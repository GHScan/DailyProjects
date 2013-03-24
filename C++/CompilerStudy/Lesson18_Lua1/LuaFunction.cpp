
#include "pch.h"
#include "LuaFunction.h"

LuaFunction::LuaFunction(LuaFunctionMeta *meta): m_meta(meta) {
}
LuaFunction::~LuaFunction() {
    delete m_meta;
}
void LuaFunction::call(const vector<LuaValue>& args, vector<LuaValue>& rets) {
}
