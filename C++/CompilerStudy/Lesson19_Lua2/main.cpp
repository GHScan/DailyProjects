
#include "pch.h"

#include "LuaLibs.h"

int main(int argc, char *argv[])
{
    if (argc < 2) {
        printf("Usage : %s filename\n", argv[0]);
        return 1;
    }

    try {
        runfile(argc, argv);
    } catch(const exception& e) {
        cout << "unhandled exception : \n" << e.what() << endl;
    }
    return 0;
}
