
#include "pch.h"
#include "LuaLibs.h"

void runfile(int argc, char *argv[]) {
    LuaVM::create();

    openLib_buildin();

    vector<LuaValue> args, rets;
    callFunc(loadFile("test.lua"), args, rets);

    LuaVM::destroy();
}
