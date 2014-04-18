#include "pch.h"

extern void test_md5();
extern void test_sha1();

extern void tool_md5(int argc, char *argv[]);
extern void tool_sha1(int argc, char *argv[]);

static void runUnitTests() {
    test_md5();
    test_sha1();
}

int main(int argc, char *argv[]) {
    runUnitTests();
    tool_sha1(argc, argv);
}
