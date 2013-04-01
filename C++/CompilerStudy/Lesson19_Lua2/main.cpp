
#include "pch.h"

bool parseFile(const char *fname);

int main(int argc, char *argv[])
{
    if (argc < 2) {
        puts("Usage : lua file");
        return 1;
    }

    parseFile(argv[1]);
    return 0;
}
