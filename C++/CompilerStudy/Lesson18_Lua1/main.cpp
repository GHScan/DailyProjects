
#include "pch.h"

#include "LuaLibs.h"

int main(int argc, char *argv[]) {
    if (argc < 2) {
        puts("Usage : lua file");
        return 1;
    }

    openLib_all();
    dofile(argv[1]);
    return 0;
}
