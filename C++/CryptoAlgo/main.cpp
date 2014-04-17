#include "pch.h"

extern void test_base64();
void tool_base64(int argc, char *argv[]);

static void runUnitTests() {
    test_base64();
}

int main(int argc, char *argv[]) {
    (void)runUnitTests;

    tool_base64(argc, argv);
}
