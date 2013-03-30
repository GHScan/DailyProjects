
#include "pch.h"
#include "LuaLibs.h"
#include "LuaValue.h"
#include "Function.h"

int main() {
    openLib_buildin();
    openLib_string();
    openLib_table();
    openLib_math();
    openLib_os();
    openLib_io();
    {
        vector<LuaValue> args, rets;
        loadFile("test.lua")->call(args, rets);
    }
    return 0;
}
