
#ifndef IMPL_malloc
#define IMPL_malloc

#include <assert.h>

#include "mm.h"
#include "memlib.h"

typedef struct {
    unsigned int allocated : 1;
    unsigned int reserved : 2;
    unsigned int dwCount : 29;
} Tag;

#define ASSERT assert
//#define ASSERT(b) do { if(!(b)) printf("assert failed:(%d) %s", __LINE__, #b); } while(0)

#define MAX(a, b) ((a)>=(b) ? (a):(b))
#define MIN(a, b) ((a)<=(b) ? (a):(b))

#define DWSIZE 8
#define ALIGN_DWCOUNT(n) (((n) + DWSIZE - 1) / DWSIZE)
#define NEXTB(b) ((b) + (b)[0].dwCount * 2)
#define PREVB(b) ((b) - (b)[-1].dwCount * 2)
#define SETB(b, _dwCount, _allocated) ((b)->dwCount = (_dwCount), (b)->allocated = (_allocated))
#define SETBF(b) (*((b) + (b)->dwCount * 2 - 1) = *(b))
#define P2B(p) ((Tag*)p - 1)
#define B2P(b) (b + 1)
#define NEW_CHUNK_DWCOUNT ((1 << 10) / DWSIZE)
#define MIN_BLOCK_DWCOUNT 2

static Tag* g_firstBlock;

static void dumpAllocator() {
    Tag *b = g_firstBlock;
    for (; b->dwCount > 0; b = NEXTB(b)) {
        if (b->allocated) printf("[%d]", b->dwCount * DWSIZE);
        else printf("(%d)", b->dwCount * DWSIZE);
    }
    puts("");
}

static void merge(Tag *b) {
    ASSERT(b->dwCount > 0 && b->allocated == 0);

    Tag *mergeB = b;
    int mergeDwCount = b->dwCount;

    Tag *nextB = NEXTB(b);
    Tag *prevB = PREVB(b);

    if (nextB->allocated == 0) mergeDwCount += nextB->dwCount;
    if (b[-1].allocated == 0) {
        mergeB = prevB;
        mergeDwCount += prevB->dwCount;
    }

    mergeB->dwCount = mergeDwCount;
    SETBF(mergeB);
}

static void split(Tag *b, int dwCount) {
    ASSERT(b->dwCount - dwCount >= MIN_BLOCK_DWCOUNT);

    int leftDwCount = b->dwCount - dwCount;
    b->dwCount = dwCount;
    SETBF(b);

    b = NEXTB(b);
    SETB(b, leftDwCount, 0);
    SETBF(b);
}

static int extendHeap(int minDwCount) {
    int totalDwCount = ((char*)mem_sbrk(0) - (char*)g_firstBlock + 4) / DWSIZE;
    int dwCount = MAX(minDwCount, totalDwCount / 4);

    Tag *b = (Tag*)mem_sbrk(dwCount * DWSIZE);
    if (b == (void*)-1) return -1;

    --b;
    SETB(b, dwCount, 0);
    SETBF(b);

    Tag *nb = NEXTB(b);
    SETB(nb, 0, 1);

    merge(b);
    return 0;
}

int mm_init(void) {
    Tag *b = (Tag*)mem_sbrk(DWSIZE);
    if (b == (void*)-1) return -1;

    SETB(b, 0, 1);
    SETB(b + 1, 0, 1);
    g_firstBlock = b + 1;

    (void)dumpAllocator;
    return extendHeap(256);
}

void *mm_malloc(size_t size) {
    int dwCount = ALIGN_DWCOUNT(size + DWSIZE);

    Tag *b = g_firstBlock;
    for (; b->dwCount > 0; b = NEXTB(b)) {
        if (b->allocated == 0 && b->dwCount >= dwCount) break;
    }
    if (b->dwCount == 0) {
        if (b[-1].allocated == 0) b = PREVB(b);
        if (extendHeap(dwCount - b->dwCount) == -1) return NULL;
    }

    if (b->dwCount - dwCount >= MIN_BLOCK_DWCOUNT) {
        split(b, dwCount);
    }

    ASSERT(b->allocated == 0);
    ASSERT(b->dwCount >= dwCount);

    b->allocated = 1;
    SETBF(b);

    return B2P(b); 
}

void mm_free(void *ptr) {
    Tag *b = P2B(ptr);
    b->allocated = 0;
    SETBF(b);
    merge(b);
}

void *mm_realloc(void *ptr, size_t size) {
    if (ptr == NULL) return mm_malloc(size);
    if (size == 0) return mm_free(ptr), NULL;

    int dwCount = ALIGN_DWCOUNT(size + DWSIZE);

    Tag *b = P2B(ptr);
    Tag *nextB = NEXTB(b);

    if (dwCount > b->dwCount && nextB->allocated == 0 && b->dwCount + nextB->dwCount >= dwCount) {
        b->dwCount += nextB->dwCount;
        SETBF(b);
    }
    if (b->dwCount >= dwCount) {
        if (b->dwCount - dwCount >= MIN_BLOCK_DWCOUNT) {
            split(b, dwCount);
        }
        return B2P(b);
    } else {
        void *newPtr = mm_malloc(size);
        memcpy(newPtr, ptr, b->dwCount * DWSIZE - DWSIZE);
        mm_free(ptr);
        return newPtr;
    }
}

#endif
