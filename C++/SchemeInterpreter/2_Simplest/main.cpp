#include "pch.h"

#include <fstream>
#include "ScmInterpreter.h"

int main(int argc, char *argv[]) {
    ScmInterpreter interpreter;

    if (argc == 1) {
        interpreter.interpret(cin);
    } else {
        for (int i = 1; i < argc; ++i) {
            ifstream fi(argv[i]);
            interpreter.interpret(fi);
        }
    }
}
