#include "pch.h"

extern void test_md5();
extern void tool_md5(int argc, char *argv[]);

static void runUnitTests() {
    test_md5();
}

int main(int argc, char *argv[]) {
    // runUnitTests();
    tool_md5(argc, argv);
}
