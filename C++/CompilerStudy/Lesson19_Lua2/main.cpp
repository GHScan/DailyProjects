
#include "pch.h"

#include "LuaLibs.h"

int main(int argc, char *argv[])
{
    if (argc < 2) {
        printf("Usage : %s filename\n", argv[0]);
        return 1;
    }

#ifdef CHECK_MEMORY_LEAKS
#ifdef _MSC_VER
    _CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);
#endif
#endif

    try {
        runfile(argc, argv);
    } catch(const exception& e) {
        cout << "unhandled exception : \n" << e.what() << endl;
    }

    return 0;
}
