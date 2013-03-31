
#include "pch.h"

#include "LuaLibs.h"

int main(int argc, char *argv[]) {
    if (argc < 2) {
        puts("Usage : lua file");
        return 1;
    }

#ifdef CHECK_MEMORY_LEAKS
#ifdef _MSC_VER
    _CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);
#endif
#endif

    runfile(argv[1]);
    return 0;
}
