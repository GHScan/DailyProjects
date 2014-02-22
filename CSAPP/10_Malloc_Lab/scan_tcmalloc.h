#ifndef IMPL_malloc
#define IMPL_malloc

#include <assert.h>

#include "mm.h"
#include "memlib.h"

#define ASSERT assert
//#define ASSERT(b) do { if(!(b)) { printf("assert failed:(%d) %s", __LINE__, #b); *(int*)0 = 0;} } while(0)

#define max(a, b) ((a)>=(b) ? (a):(b))
#define min(a, b) ((a)<=(b) ? (a):(b))

#define DWSIZE 8
#define PAGE_SIZE  (4 * 1024)

#define PD_SIZE 32

#define PAGE_ENTRY_TYPE_BLOCK 1
#define PAGE_ENTRY_TYPE_MPAGE 0

#define BLOCK_CLASS_FENCE_0 (DWSIZE * 128)
#define BLOCK_CLASS_FENCE_1 (DWSIZE * 192)
#define BLOCK_CLASS_FENCE_2 (DWSIZE * 256)
#define BLOCK_CLASS_SPACE_0 DWSIZE
#define BLOCK_CLASS_SPACE_1 (DWSIZE * 2)
#define BLOCK_CLASS_SPACE_2 (DWSIZE * 4)
#define BLOCK_CLASS_COUNT_0 (BLOCK_CLASS_FENCE_0 / BLOCK_CLASS_SPACE_0)
#define BLOCK_CLASS_COUNT_01 (((BLOCK_CLASS_FENCE_1 - BLOCK_CLASS_FENCE_0) / BLOCK_CLASS_SPACE_1) + BLOCK_CLASS_COUNT_0)
#define BLOCK_CLASS_COUNT_012 (((BLOCK_CLASS_FENCE_2 - BLOCK_CLASS_FENCE_1) / BLOCK_CLASS_SPACE_2) + BLOCK_CLASS_COUNT_01)

#define MPAGE_CLASS_COUNT 256

#define boundupToDwSize(bytes) ((bytes + DWSIZE - 1) / DWSIZE * DWSIZE)

#define ptr2PageIdx(ptr) (((char*)ptr - g_pageBase) / PAGE_SIZE)
#define pageIdx2PDE(pageIdx) (g_PDBase + ((pageIdx >> 10) & 0x3ff))
#define pageIdx2Ptr(pageIdx) (g_pageBase + pageIdx * PAGE_SIZE)
#define pageIdx2BlockMeta(pageIdx) ((PageEntry_BlockMeta*)*pageIdx2PDE(pageIdx) + (pageIdx & 0x3ff))
#define pageIdx2MPageMeta(pageIdx) ((PageEntry_MPageMeta*)*pageIdx2PDE(pageIdx) + (pageIdx & 0x3ff))

#define isPrevMPageMeta(meta) ((meta - 1)->type == PAGE_ENTRY_TYPE_MPAGE)
#define prevMPageMeta(meta) (meta - (meta - 1)->pageCount)
#define nextMPageMeta(meta) (meta + meta->pageCount)
#define setMPageMetaFooter(meta) { *((meta) + (meta)->pageCount - 1) = *(meta); }
#define setMPageMeta(meta, _allocated, _pageCount) {meta->type = PAGE_ENTRY_TYPE_MPAGE; meta->allocated = _allocated; meta->pageCount = _pageCount; setMPageMetaFooter(meta); }
#define pageCount2mpageClassIdx(pageCount) (pageCount < MPAGE_CLASS_COUNT ? pageCount - 1 : MPAGE_CLASS_COUNT - 1)

typedef struct {
    unsigned int type : 1;
    unsigned int blockClassIdx : 8;
} PageEntry_BlockMeta;

typedef struct {
    unsigned int type : 1;
    unsigned int allocated : 1;
    unsigned int pageCount : 14;
} PageEntry_MPageMeta;

typedef struct _Block{
    struct _Block* next;
} Block;
typedef struct _MPage {
    struct _MPage *prev, *next;
} MPage;

static char* g_pageBase;
static char** g_PDBase;
static Block** g_blockFreeLists;
static MPage** g_mpageFreeLists;

//////////////////////////////
static void ensurePDEExist() {
    char *newPagePtr = (char*)mem_sbrk(0);
    int pageIdx = ptr2PageIdx(newPagePtr);
    char **pde = pageIdx2PDE(pageIdx);
    if (*pde == NULL) {
        *pde = (char*)mem_sbrk(PAGE_SIZE);
        if (*pde == (void*)-1) ASSERT(0);
        // memset(*pde, 0, PAGE_SIZE);
    }
}
static int newPage() {
    ensurePDEExist();

    char *ptr = mem_sbrk(PAGE_SIZE);
    if (ptr == (void*)-1) return ASSERT(0), 0;
    return ptr2PageIdx(ptr);
}
//////////////////////////////
static int block_size2BlockClassIdx(int size) {
    if (size <= BLOCK_CLASS_FENCE_0) {
        return size / BLOCK_CLASS_SPACE_0;
    } else if (size <= BLOCK_CLASS_FENCE_1) {
        return (size - BLOCK_CLASS_FENCE_0) / BLOCK_CLASS_SPACE_1 + BLOCK_CLASS_COUNT_0;
    } else {
        ASSERT(size <= BLOCK_CLASS_FENCE_2);
        return (size - BLOCK_CLASS_FENCE_1) / BLOCK_CLASS_SPACE_2 + BLOCK_CLASS_COUNT_01;
    }
}
static int block_blockClassIdx2BlockSize(int blockClassIdx) {
    if (blockClassIdx < BLOCK_CLASS_COUNT_0) {
        return (blockClassIdx + 1) * BLOCK_CLASS_SPACE_0;
    } else if (blockClassIdx < BLOCK_CLASS_COUNT_01) {
        return (blockClassIdx - BLOCK_CLASS_COUNT_0) * BLOCK_CLASS_SPACE_1 + BLOCK_CLASS_FENCE_0;
    } else {
        ASSERT(blockClassIdx < BLOCK_CLASS_COUNT_012);
        return (blockClassIdx - BLOCK_CLASS_COUNT_01) * BLOCK_CLASS_SPACE_2 + BLOCK_CLASS_FENCE_1;
    }
}
static void block_newPage(int blockClassIdx) {
    int pageIdx = newPage();

    PageEntry_BlockMeta *meta = pageIdx2BlockMeta(pageIdx);
    meta->type = PAGE_ENTRY_TYPE_BLOCK;
    meta->blockClassIdx = blockClassIdx;

    int blockSize = block_blockClassIdx2BlockSize(blockClassIdx);

    char *pagePtr = pageIdx2Ptr(pageIdx);
    Block** freeList = g_blockFreeLists + blockClassIdx;
    int off = 0;
    for (; off < PAGE_SIZE; off += blockSize) {
        Block* b = (Block*)(pagePtr + off);
        b->next = *freeList;
        *freeList = b;
    }
}
static void* block_malloc(int blockClassIdx) {
    Block** freeList = g_blockFreeLists + blockClassIdx;
    // TODO: search larger blockClassIdx first
    if (*freeList == NULL) block_newPage(blockClassIdx);
    ASSERT(*freeList != NULL);
    Block *b = *freeList;
    *freeList = b->next;
    return b;
}
static void block_free(PageEntry_BlockMeta *meta, char *ptr) {
    ASSERT(meta->type == PAGE_ENTRY_TYPE_BLOCK);
    Block* b = (Block*)ptr;
    Block** freeList = g_blockFreeLists + meta->blockClassIdx;
    b->next = *freeList;
    *freeList = b;
}
//////////////////////////////
static void mpage_unlinkMPage(PageEntry_MPageMeta *meta, char *ptr) {
    int mpageClassIdx = pageCount2mpageClassIdx(meta->pageCount);
    MPage **freeList = g_mpageFreeLists + mpageClassIdx;
    MPage *mpage = (MPage*)ptr;

    if (mpage->next != NULL) mpage->next->prev = mpage->prev;
    if (mpage->prev != NULL) mpage->prev->next = mpage->next;
    if (*freeList == mpage) *freeList = mpage->next;
}
static void mpage_linkMPageToFreeList(PageEntry_MPageMeta *meta, char *ptr) {
    int mpageClassIdx = pageCount2mpageClassIdx(meta->pageCount);
    MPage **freeList = g_mpageFreeLists + mpageClassIdx;
    MPage *mpage = (MPage*)ptr;

    if (*freeList != NULL) (*freeList)->prev = mpage;
    mpage->next = *freeList;
    mpage->prev = NULL;
}
static void mpage_mergeMPage(PageEntry_MPageMeta *meta, char *ptr) {
    ASSERT(meta->allocated == 0);

    PageEntry_MPageMeta *nextMeta = nextMPageMeta(meta);
    if (nextMeta->allocated) nextMeta = NULL;
    PageEntry_MPageMeta *prevMeta = isPrevMPageMeta(meta) ? prevMPageMeta(meta) : NULL;
    if (prevMeta->allocated) prevMeta = NULL;

    if (prevMeta == NULL && nextMeta == NULL) return;

    mpage_unlinkMPage(meta, ptr);

    PageEntry_MPageMeta *mergeMeta = meta;
    int mergePageCount = meta->pageCount;
    char *mergePtr = ptr;

    if (nextMeta != NULL) {
        mergePageCount += nextMeta->pageCount;
        mpage_unlinkMPage(nextMeta, ptr + meta->pageCount * PAGE_SIZE);
    }
    if (prevMeta != NULL) {
        mergePageCount += prevMeta->pageCount;
        mergeMeta = prevMeta;
        mergePtr = ptr - prevMeta->pageCount * PAGE_SIZE;
        mpage_unlinkMPage(prevMeta, mergePtr);
    }

    setMPageMeta(mergeMeta, 0, mergePageCount);
    mpage_linkMPageToFreeList(mergeMeta, mergePtr);
}
static void mpage_splitMPage(PageEntry_MPageMeta *meta, char *ptr, int pageCount) {
}
static void mpage_newMPage(int pageCount) {
    int pageIdx = newPage();
    int i = 1;
    for (; i < pageCount; ++i) newPage();
    char *ptr = pageIdx2Ptr(pageIdx);

    PageEntry_MPageMeta *meta = pageIdx2MPageMeta(pageIdx);
    setMPageMeta(meta, 0, pageCount);
    mpage_linkMPageToFreeList(meta, ptr);
}
static void* mpage_malloc(int pageCount) {
    int mpageClassIdx = pageCount2mpageClassIdx(pageCount);
    MPage **freeList = g_mpageFreeLists + mpageClassIdx;
    // TODO: search larger mpageClassIdx first
    if (*freeList == NULL) mpage_newMPage(pageCount);
    ASSERT(*freeList != NULL);

    MPage *mpage = *freeList;
    int pageIdx = ptr2PageIdx(mpage);
    PageEntry_MPageMeta *meta = pageIdx2MPageMeta(pageIdx);
    setMPageMeta(meta, 1, meta->pageCount);
    mpage_unlinkMPage(meta, (char*)mpage);

    return mpage;
}
static void mpage_free(PageEntry_MPageMeta *meta, char *ptr) {
    ASSERT(meta->type == PAGE_ENTRY_TYPE_MPAGE);
    setMPageMeta(meta, 0, meta->pageCount);
    mpage_linkMPageToFreeList(meta, ptr);
    mpage_mergeMPage(meta, ptr);
}
//////////////////////////////
int mm_init(void) {
    char *ptr = mem_sbrk(PAGE_SIZE);
    if (ptr == (void*)-1) return -1;

    g_pageBase = mem_sbrk(0);

    g_PDBase = (char**)ptr;
    memset(g_PDBase, 0, sizeof(*g_PDBase) * PD_SIZE);
    ptr += sizeof(*g_PDBase) * PD_SIZE;

    g_blockFreeLists = (Block**)ptr;
    memset(g_blockFreeLists, 0, sizeof(*g_blockFreeLists) * BLOCK_CLASS_COUNT_012);
    ptr += sizeof(*g_blockFreeLists) * BLOCK_CLASS_COUNT_012;

    g_mpageFreeLists = (MPage**)ptr;
    memset(g_mpageFreeLists, 0, sizeof(*g_mpageFreeLists) * MPAGE_CLASS_COUNT);
    ptr += sizeof(*g_mpageFreeLists) * MPAGE_CLASS_COUNT;

    return 0;
}

void *mm_malloc(size_t size) {
    int boundedSize = boundupToDwSize(size);
    if (boundedSize <= BLOCK_CLASS_FENCE_2) {
        return block_malloc(block_size2BlockClassIdx(boundedSize));
    } else {
        return mpage_malloc((boundedSize + PAGE_SIZE - 1) / PAGE_SIZE);
    }
}

void mm_free(void *ptr) {
    int pageIdx = ptr2PageIdx(ptr);

    PageEntry_BlockMeta *meta = pageIdx2BlockMeta(pageIdx);
    if (meta->type == PAGE_ENTRY_TYPE_BLOCK) {
        block_free(meta, ptr);
    } else {
        mpage_free((PageEntry_MPageMeta*)meta, ptr);
    }
}

void *mm_realloc(void *ptr, size_t size) {
    return NULL;
}

#endif
