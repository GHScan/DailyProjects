#include "pch.h"

#include "SVM.h"

int main(int argc, char *argv[]) {
    if (argc == 1) {
        SVM vm(stdin, false, 64 * 1024);
        vm.run();
    } else {
        FILE *f = fopen(argv[1], "r");
        ASSERT(f);

        SVM vm(f, false, 64 * 1024);
        vm.run();

        fclose(f);
    }
}
