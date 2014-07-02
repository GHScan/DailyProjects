#include "pch.h"

#include "SVM.h"

int main(int argc, char *argv[]) {
    if (argc == 1) {
        SVM vm(stdin, false);
        vm.run();
    } else {
        FILE *f = fopen(argv[1], "r");
        ASSERT(f);

        SVM vm(f, false);
        vm.run();

        fclose(f);
    }
}
