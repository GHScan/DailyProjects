#include "pch.h"

#include <fstream>

#include "SInterpreter.h"
#include "STypes.h"
#include "SObjectManager.h"
#include "SPairList.h"
#include "ScopedValue.h"

int main(int argc, char *argv[]) {
    SInterpreter s;

    if (argc == 1) {
        s.interpret(cin);
    } else {
        for (int i = 1; i < argc; ++i) {
            ifstream fi(argv[i]);
            s.interpret(fi);
        }
    }
}
