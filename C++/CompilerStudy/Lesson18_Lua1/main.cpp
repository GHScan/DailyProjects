
#include "pch.h"
#include "LuaLibs.h"
#include "LuaValue.h"
#include "Function.h"

int main(int argc, char *argv[]) {
    if (argc < 2) {
        puts("Usage : lua file");
        return 1;
    }

    openLib_buildin();
    openLib_string();
    openLib_table();
    openLib_math();
    openLib_os();
    openLib_io();
    {
        vector<LuaValue> args, rets;
        loadFile(argv[1])->call(args, rets);
    }
    return 0;
}
