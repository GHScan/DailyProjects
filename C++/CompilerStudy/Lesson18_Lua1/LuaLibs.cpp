
#include "pch.h"
#include "LuaLibs.h"

void openLib_all() {
    openLib_buildin();
    openLib_string();
    openLib_table();
    openLib_math();
    openLib_os();
    openLib_io();
}

void dofile(const char *fname) {
    vector<LuaValue> args, rets;
    loadFile(fname)->call(args, rets);
}
