#ifndef ALLOCATOR_H
#define ALLOCATOR_H

#include <memory.h>

typedef struct byAllocator {
    struct byAllocatorVtable *vtable;
} byAllocator;

struct byAllocatorVtable {
    void*   (*alloc) (byAllocator *this, int elemSize, int alignemnt, int n);
    void    (*free) (byAllocator *this, void *p, int elemSize, int alignment, int n);
    void    (*destroy) (byAllocator *this);
};

static void* byAllocator_alloc(byAllocator *a, int elemSize, int alignment, int n) {
    return a->vtable->alloc(a, elemSize, alignment, n);
}

static void byAllocator_free(byAllocator *a, void *p, int elemSize, int alignment, int n) {
    a->vtable->free(a, p, elemSize, alignment, n);
}

static void byAllocator_destroy(byAllocator *a) {
    a->vtable->destroy(a);
}

#endif
