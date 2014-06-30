#include "pch.h"

#include "SVM.h"

int main() {
    SVM vm(stdin, false, 64 * 1024);
    vm.run();
}
