#include "pch.h" 

#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <math.h>

#include <execinfo.h>
#include <dlfcn.h>

void printSymName(void *sym) {
    void *h = dlopen(NULL, RTLD_NOW);
    Dl_info info;
    dladdr(sym, &info);
    printf("%s:%s\n", info.dli_fname, info.dli_sname);
    dlclose(h);
}

void printBacktrace() {
    const int N = 32;
    void *retAddrs[N] = {0};
    int n = backtrace(retAddrs, N);
    char **strs = backtrace_symbols(retAddrs, n);
    puts("@ backtrace : ");
    for (int i = 0; i < n; ++i) puts(strs[i]);
    free(strs);
}

void func() { printBacktrace(); }
void func2() { func(); }
void func3() { func2(); }

int g_int;
int main() {
    func3();
    printSymName((void*)&main);
    printSymName(&g_int);
}
