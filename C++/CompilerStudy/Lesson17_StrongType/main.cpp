
#include "pch.h"

#include <time.h>

static void registerBuildin()
{
}

void parseFile(const char *fname);

int main(int argc, char *argv[])
{
    if (argc == 1) {
        puts("usage : interpreter file1 [file2 ...]");
        return 0;
    }

    try
    {
        registerBuildin();
        for (int i = 1; i < argc; ++i) parseFile(argv[i]);
        //GlobalEnvironment::instance()->getFunc("main")->call(vector<Value>());
    }
    catch (const exception& e) {
        cout << "Exception : " << e.what() << endl;
    }
}
