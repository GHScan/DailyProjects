#include "pch.h"

#include "Atom.h"

extern void parseFile(const char *fname);

int main() {
    auto ap = new AtomPool();

    parseFile("test.js");

    DELETE(ap);
}
