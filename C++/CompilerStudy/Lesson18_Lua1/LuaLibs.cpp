
#include "pch.h"
#include "LuaLibs.h"

void runfile(const char *fname) {
    Runtime::instance()->setGlobalTable(LuaTable::create());

    openLib_buildin();
    openLib_string();
    openLib_table();
    openLib_math();
    openLib_os();
    openLib_io();

    try {
        vector<LuaValue> args, rets;
        loadFile(fname)->call(args, rets);
    } catch (const exception& e) {
        printf("unhandled excpetion: \n%s", e.what());
    }

    Runtime::instance()->setGlobalTable(NULL);
}
