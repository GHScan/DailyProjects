#include <stdlib.h>

#include "Mem.h"
#include "Assert.h"

void* byMem_alloc(int nbytes, const char *file, int line) {
    void *p = malloc(nbytes);
    if (!p) {
        by_abort("", "Malloc failed!", file, line);
    }
    return p;
}

void* byMem_resize(void *p, int nbytes, const char *file, int line) {
    if (p == NULL || nbytes == 0) {
        by_abort("", "Invalid params for resize", file, line);
    }
    p = realloc(p, nbytes);
    if (p == NULL) {
        by_abort("", "Realloc failed!", file, line);
    }
    return p;
}

void byMem_free(void *p, const char *file, int line) {
    if (p == NULL) {
        by_abort("", "Invalid params for free", file, line);
    }
    free(p);
}
