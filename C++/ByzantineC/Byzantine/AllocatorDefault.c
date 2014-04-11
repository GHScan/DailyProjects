#include "Assert.h"
#include "Mem.h"
#include "AllocatorDefault.h"

typedef struct byAllocatorDefault {
    byAllocator base;
} byAllocatorDefault;

static struct byAllocatorVtable g_vtable = {
    (void*(*)(byAllocator*,int,int,int))&byAllocatorDefault_alloc,
    (void(*)(byAllocator*,void*,int,int,int))&byAllocatorDefault_free,
    (void(*)(byAllocator*))&byAllocatorDefault_destroy,
};

byAllocatorDefault* byAllocatorDefault_create() {
    byAllocatorDefault *p;
    by_NEW(p);
    p->base.vtable = &g_vtable;
    return p;
}

void* byAllocatorDefault_alloc(byAllocatorDefault *this, int elemSize, int alignment, int n) {
    by_ASSERT(this != NULL, "");

    void *p = byMem_alloc(elemSize * n, __FILE__, __LINE__);
    by_ASSERT((long)p % alignment == 0, "Should be aligned");
    return p;
}

void byAllocatorDefault_free(byAllocatorDefault *this, void *p, int elemSize, int alignment, int n) {
    by_ASSERT(this != NULL, "");

    byMem_free(p, __FILE__, __LINE__);
}

void byAllocatorDefault_destroy(byAllocatorDefault *this) {
    by_ASSERT(this != NULL, "");

    by_FREE(this);
}

byAllocator* byAllocatorDefault_queryInterface(byAllocatorDefault *this) {
    by_ASSERT(this != NULL, "");

    return (byAllocator*)this;
}

