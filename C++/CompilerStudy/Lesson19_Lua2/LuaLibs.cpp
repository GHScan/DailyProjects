
#include "pch.h"
#include "LuaLibs.h"
#include "ByteCode.h"

void runfile(int argc, char *argv[]) {
    LuaVM::create();

    openLib_buildin();
    openLib_math();
    openLib_string();
    openLib_table();
    openLib_os();
    openLib_io();

    {
        auto func = loadFile(argv[1]);
        vector<LuaValue> args, rets;
        for (int i = 2; i < argc; ++i) args.push_back(LuaValue(argv[i]));

        {
            ofstream fo("dis.txt");
            disassemble(fo, static_cast<LuaFunction*>(func.getFunction())->meta.get());
        }

        callFunc(func, args, rets);
    }

    LuaVM::destroy();
}
