
#include "pch.h"
#include "LuaLibs.h"

void runfile(int argc, char *argv[]) {
    ASSERT(argc >= 2);

    Runtime::instance()->setGlobalTable(LuaTable::create());

    openLib_buildin();
    openLib_string();
    openLib_table();
    openLib_math();
    openLib_os();
    openLib_io();

    try {
        vector<LuaValue> args, rets;
        for (int i = 2; i < argc; ++i) args.push_back(LuaValue(argv[i]));
        loadFile(argv[1])->call(args, rets);
    } catch (const exception& e) {
        printf("unhandled excpetion: \n%s", e.what());
    }

    Runtime::instance()->setGlobalTable(NULL);
}
