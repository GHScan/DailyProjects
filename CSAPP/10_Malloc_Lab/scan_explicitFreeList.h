#ifndef IMPL_malloc
#define IMPL_malloc

#include <assert.h>

#include "mm.h"
#include "memlib.h"

typedef struct _Header {
    unsigned int allocated : 1;
    unsigned int prevAllocated : 1;
    unsigned int reserved : 1;
    unsigned int dwSize : 29;
    struct _Header *prev, *next;
} Header;
typedef struct {
    unsigned int allocated : 1;
    unsigned int prevAllocated : 1;
    unsigned int reserved : 1;
    unsigned int dwSize : 29;
} Footer;

/*
   allocated block: (dwSize:29, prevAllocated:1, allocated:1), payload... ; (prevAllocated:1, ...)
   free block: (dwSize:29, prevAllocated:1, allocated:1), (prev:32), (next:32), ..., (footer:32); 
 * */

#define ASSERT assert
//#define ASSERT(b) do { if(!(b)) { printf("assert failed:(%d) %s", __LINE__, #b); *(int*)0 = 0;} } while(0)

#define max(a, b) ((a)>=(b) ? (a):(b))
#define min(a, b) ((a)<=(b) ? (a):(b))


#define alignDwSize(bytes) ((bytes + 7) / 8)
#define MIN_BLOCK_DWSIZE alignDwSize(sizeof(Header) + sizeof(Footer))
#define MAX_EXTEND_DWSIZE ((1 << 20) / 8)

#define prevFooter(header) ((Footer*)((char*)header - sizeof(Footer)))
#define prevHeader(header) ((Header*)((char*)header - prevFooter(header)->dwSize * 8))
#define nextHeader(header) ((Header*)((char*)header + header->dwSize * 8))
#define header2Footer(header) prevFooter(nextHeader(header))

#define header2Ptr(header) ((char*)(header) + 4)
#define ptr2Header(p) ((Header*)((char*)(p) - 4))

#define setDwSize(header, _dwSize) { (header)->dwSize = _dwSize; header2Footer(header)->dwSize = _dwSize; }
#define setAllocated(header, b) {(header)->allocated = b; header2Footer(header)->allocated = b; nextHeader(header)->prevAllocated = b;}

#define linkAfter(toHeader, header) {(header)->prev = (toHeader); (header)->next = (toHeader)->next; (header)->prev->next = (header); (header)->next->prev = (header);}
#define unlink(header) { (header)->prev->next = (header)->next; (header)->next->prev = (header)->prev; }

static Header* g_freeList;

static void dumpAllBlocks() {
    Header *h = (Header*)((char*)g_freeList + 36);
    printf("@blocks{");
    for (; h->dwSize > 0; h = nextHeader(h)) {
        if (h->allocated) printf("[%d]", h->dwSize * 8);
        else printf("(%d)", h->dwSize * 8);
    }
    puts("}");
}
static void dumpFreeList() {
    Header *h = g_freeList->next;
    printf("@freelist{");
    for (; h->next != NULL; h = h->next) {
        printf("(%d)", h->dwSize);
    }
    puts("}");
}
static void checkAllocator() {
    Header *h = g_freeList->next;
    for (; h->next != NULL; h = h->next) {
        ASSERT(h->allocated == 0);
    }

    h = (Header*)((char*)g_freeList + 36);
    for (; h->dwSize > 0; h = nextHeader(h)) {
        if (h->allocated == 0) {
            ASSERT(h->allocated == header2Footer(h)->allocated);
            ASSERT(h->dwSize == header2Footer(h)->dwSize);
        }
        ASSERT(h->allocated == nextHeader(h)->prevAllocated);
    }
}

static void merge(Header *h);
static int extendHeap(int minDwSize) {
    int lastFreeBlockDwSize = ((Header*)((char*)mem_sbrk(0) - 4))->prevAllocated ? 0 : ((Footer*)((char*)mem_sbrk(0) - 8))->dwSize;
    minDwSize = max(0, minDwSize - lastFreeBlockDwSize);

    int totalDwSize = ((char*)mem_sbrk(0) - (char*)g_freeList + 4) / 8;
    int dwSize = max(min(totalDwSize / 4, MAX_EXTEND_DWSIZE), MIN_BLOCK_DWSIZE);
    dwSize = max(minDwSize, dwSize);

    char *p = mem_sbrk(dwSize * 8);
    if (p == (void*)-1) return -1;

    Header *h = (Header*)(p - 4);
    setDwSize(h, dwSize);
    setAllocated(h, 0);
    linkAfter(g_freeList, h);

    Header *nh = nextHeader(h);
    nh->allocated = 1;
    nh->dwSize = 0;

    merge(h);
    return 0;
}
// h must has been unlink
static void split(Header *h, int dwSize) {
    ASSERT(h->dwSize - dwSize >= MIN_BLOCK_DWSIZE);

    int leftDwSize = h->dwSize - dwSize;

    if (h->allocated) {
        h->dwSize = dwSize;
    } else {
        setDwSize(h, dwSize);
        setAllocated(h, 0);
    }

    Header *h2 = nextHeader(h);
    h2->prevAllocated = h->allocated;
    setDwSize(h2, leftDwSize);
    setAllocated(h2, 0);
    linkAfter(g_freeList, h2);
}
static void merge(Header *h) {
    ASSERT(h->allocated == 0);

    Header *nh = nextHeader(h);
    if (nh->allocated && h->prevAllocated) return;

    unlink(h);

    Header *mergeH = h;
    int mergeDwSize = h->dwSize;
    if (nh->allocated == 0) {
        unlink(nh);
        mergeDwSize += nh->dwSize;
    }
    if (h->prevAllocated == 0) {
        mergeH = prevHeader(h);
        unlink(mergeH);
        mergeDwSize += mergeH->dwSize;
    }

    setDwSize(mergeH, mergeDwSize);
    setAllocated(mergeH, 0);
    linkAfter(g_freeList, mergeH);
}

int mm_init(void) {
    Header *h = (Header*)mem_sbrk(5 * 8);
    if (h == (void*)-1) return -1;
    g_freeList = h;

    setDwSize(h, 2);
    setAllocated(h, 0);

    Header *h2 = nextHeader(h);
    setDwSize(h2, 2);
    setAllocated(h2, 0);

    h->next = h2;
    h->prev = NULL;
    h2->prev = h;
    h2->next = NULL;

    Header *h3 = nextHeader(h2);
    h3->allocated = 1;
    h3->dwSize = 0;
    h3->prevAllocated = 1;
    memcpy((char*)h3 + 4, h3, 4);

    (void)dumpAllBlocks;
    (void)dumpFreeList;
    (void)checkAllocator;

    return extendHeap(128);
}

void *mm_malloc(size_t size) {
    int dwSize = max(alignDwSize(size + 4), MIN_BLOCK_DWSIZE);

    Header *fit = NULL;
    Header *h = g_freeList->next;
    for (; h->next != NULL; h = h->next) {
        if (h->dwSize >= dwSize) {
            if (fit == NULL || h->dwSize < fit->dwSize) fit = h;
            if (fit->dwSize == dwSize) break;
        }
    }
    h = fit;

    if (h == NULL) {
        if (extendHeap(max(dwSize, 256)) == -1) return NULL;
        h = g_freeList->next;
        ASSERT(h->allocated == 0 && h->dwSize >= dwSize);
    }

    unlink(h);
    if (h->dwSize - dwSize >= MIN_BLOCK_DWSIZE) {
        split(h, dwSize);
    }
    setAllocated(h, 1);

    return header2Ptr(h);
}

void mm_free(void *ptr) {
    Header *h = ptr2Header(ptr);
    // add footer
    setDwSize(h, h->dwSize);
    setAllocated(h, 0);
    linkAfter(g_freeList, h);

    merge(h);
}

void *mm_realloc(void *ptr, size_t size) {
    if (ptr == NULL) return mm_malloc(size);
    if (size == 0) return mm_free(ptr), NULL;

    int dwSize = max(alignDwSize(size + 4), MIN_BLOCK_DWSIZE);

    Header *h = ptr2Header(ptr);
    Header *nh = nextHeader(h);
    if (h->dwSize < dwSize && nh->allocated == 0 && h->dwSize + nh->dwSize >= dwSize) {
        unlink(nh);
        setAllocated(nh, 1);
        h->dwSize += nh->dwSize;
    }

    void *newPtr;
    if (h->dwSize >= dwSize) {
        if (h->dwSize - dwSize >= MIN_BLOCK_DWSIZE) {
            split(h, dwSize);
        }
        newPtr = header2Ptr(h);
    } else {
        newPtr = mm_malloc(size);
        memcpy(newPtr, ptr, h->dwSize * 8 - 4);
        mm_free(ptr);
    }
    return newPtr;
}

#endif
