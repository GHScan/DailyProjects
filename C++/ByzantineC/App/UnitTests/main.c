#include <stdio.h>

#include "UnitTest.h"

int main(int argc, char *argv[]) {
    by_TEST_CASE_DRIVER(argc < 2  ? "*" : argv[1]);
    by_TEST_CASE_DEPENDENT(testArray);
}
