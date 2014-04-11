#ifndef INTERNAL_ALLOCATOR_DEFAULT_H
#define INTERNAL_ALLOCATOR_DEFAULT_H

#include "Allocator.h"

typedef struct byAllocatorDefault byAllocatorDefault;

byAllocatorDefault* byAllocatorDefault_create();
void*   byAllocatorDefault_alloc(byAllocatorDefault *this, int elemSize, int alignment, int n);
void    byAllocatorDefault_free(byAllocatorDefault *this, void *p, int elemSize, int alignment, int n);
void    byAllocatorDefault_destroy(byAllocatorDefault *this);
byAllocator* byAllocatorDefault_queryInterface(byAllocatorDefault *this);

#endif
