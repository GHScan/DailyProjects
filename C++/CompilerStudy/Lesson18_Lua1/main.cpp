
#include "pch.h"
#include "LuaLibs.h"

int main() {
    openLib_buildin();
    doFile("test.lua");
    return 0;
}
