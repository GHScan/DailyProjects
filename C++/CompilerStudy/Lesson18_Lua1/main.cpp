
#include "pch.h"

#include "Runtime.h"
#include "LuaValue.h"
#include "Function.h"
#include "LuaTable.h"

bool doFile(const char *fname);

static void buildin_print(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    for (auto &arg : args) {
        printf("%s\t", arg.toString().c_str());
    }
    puts("");
}

void registerFunctions() {
    string names[] = {
        "print",
    };
    void (*funcs[])(const vector<LuaValue>& args, vector<LuaValue>& rets) = {
        &buildin_print,
    };
    for (int i = 0; i < COUNT_OF(names); ++i) {
        Runtime::instance()->getGlobalTable()->set(
                LuaValue(names[i]), 
                LuaValue(CFunction::create(funcs[i])));
    }
}

int main() {
    registerFunctions();
    doFile("test.lua");
    return 0;
}
