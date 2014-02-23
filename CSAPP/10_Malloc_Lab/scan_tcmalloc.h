#ifndef IMPL_malloc
#define IMPL_malloc

#include <assert.h>

#include "config.h"
#include "mm.h"
#include "memlib.h"

//#define ASSERT assert
#define ASSERT(b) do { if(!(b)) { printf("assert failed:(%d) %s", __LINE__, #b); *(int*)0 = 0;} } while(0)

#define max(a, b) ((a)>=(b) ? (a):(b))
#define min(a, b) ((a)<=(b) ? (a):(b))

#define DWSIZE 8
#define PAGE_SIZE_BITW 12

#define PAGE_SIZE  (1 << PAGE_SIZE_BITW)
#define PTE_COUNT_BITW (PAGE_SIZE_BITW - 2)
#define PTE_COUNT_PER_PAGE (1 << PTE_COUNT_BITW)
#define PTE_INDEX_MASK ((1 << PTE_COUNT_BITW) - 1)
#define PDE_INDEX_MASK ((1 << (32 - PAGE_SIZE_BITW - PTE_COUNT_BITW)) - 1)

#define size2PageCount(size) ((size + PAGE_SIZE - 1) / PAGE_SIZE)

#define PD_SIZE ((size2PageCount(MAX_HEAP) + PTE_COUNT_PER_PAGE - 1) / PTE_COUNT_PER_PAGE)

#define PAGE_ENTRY_TYPE_BLOCK 2
#define PAGE_ENTRY_TYPE_MPAGE 1
#define PAGE_ENTRY_TYPE_PAGETABLE 0

#define BLOCK_CLASS_FENCE_0 (PAGE_SIZE * 2 / 8)
#define BLOCK_CLASS_FENCE_1 (PAGE_SIZE * 3 / 8)
#define BLOCK_CLASS_FENCE_2 (PAGE_SIZE * 4 / 8)
#define BLOCK_CLASS_SPACE_0 DWSIZE
#define BLOCK_CLASS_SPACE_1 (DWSIZE * 2)
#define BLOCK_CLASS_SPACE_2 (DWSIZE * 4)
#define BLOCK_CLASS_COUNT_0 (BLOCK_CLASS_FENCE_0 / BLOCK_CLASS_SPACE_0)
#define BLOCK_CLASS_COUNT_01 (((BLOCK_CLASS_FENCE_1 - BLOCK_CLASS_FENCE_0) / BLOCK_CLASS_SPACE_1) + BLOCK_CLASS_COUNT_0)
#define BLOCK_CLASS_COUNT_012 (((BLOCK_CLASS_FENCE_2 - BLOCK_CLASS_FENCE_1) / BLOCK_CLASS_SPACE_2) + BLOCK_CLASS_COUNT_01)

#define MPAGE_CLASS_COUNT 256

#define boundupToDwSize(bytes) ((bytes + DWSIZE - 1) / DWSIZE * DWSIZE)

#define ptr2PageIdx(ptr) (((char*)ptr - g_pageBase) / PAGE_SIZE)
#define getMaxPageCount()  ptr2PageIdx(mem_sbrk(0))
#define pageIdx2PDE(pageIdx) (g_PDBase + ((pageIdx >> PTE_COUNT_BITW) & PDE_INDEX_MASK))
#define pageIdx2Ptr(pageIdx) (g_pageBase + pageIdx * PAGE_SIZE)
#define pageIdx2BlockMeta(pageIdx) ((PageEntry_BlockMeta*)*pageIdx2PDE(pageIdx) + (pageIdx & PTE_INDEX_MASK))
#define pageIdx2MPageMeta(pageIdx) ((PageEntry_MPageMeta*)*pageIdx2PDE(pageIdx) + (pageIdx & PTE_INDEX_MASK))

#define prevMPageMeta(meta) (meta - (meta - 1)->pageCount)
#define nextMPageMeta(meta) (meta + meta->pageCount)
#define setMPageMetaFooter(meta) { *((meta) + (meta)->pageCount - 1) = *(meta); }
#define setMPageMeta(meta, _allocated, _pageCount) {meta->type = PAGE_ENTRY_TYPE_MPAGE; meta->allocated = _allocated; meta->pageCount = _pageCount; setMPageMetaFooter(meta); }
#define pageCount2mpageClassIdx(pageCount) (pageCount < MPAGE_CLASS_COUNT ? pageCount - 1 : MPAGE_CLASS_COUNT - 1)

typedef struct {
    unsigned int type : 2;
    unsigned int blockClassIdx : 30;
} PageEntry_BlockMeta;

typedef struct {
    unsigned int type : 2;
    unsigned int allocated : 1;
    unsigned int pageCount : 29;
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
static void block_dumpFreeLists();
static void mpage_dumpFreeLists();
static int block_blockClassIdx2BlockSize(int blockClassIdx);
static void dumpPageEntryMeta() {
    printf("@pageEntryMeta {");
    int maxPageCount = getMaxPageCount();
    int pageIdx = 0;
    while (pageIdx < maxPageCount) {
        PageEntry_MPageMeta *mpageMeta = pageIdx2MPageMeta(pageIdx);
        if (mpageMeta->type == PAGE_ENTRY_TYPE_PAGETABLE) {
            printf("(pt)");
            ++pageIdx;
        }
        else if (mpageMeta->type == PAGE_ENTRY_TYPE_MPAGE) {
            printf("(mp,#%d,%d)", mpageMeta->pageCount, mpageMeta->allocated);
            pageIdx += mpageMeta->pageCount;
        } else {
            PageEntry_BlockMeta *blockMeta = (PageEntry_BlockMeta*)mpageMeta;
            printf("(b,#%d)", block_blockClassIdx2BlockSize(blockMeta->blockClassIdx));
            ++pageIdx;
        }
    }
    puts("}");
}
static void dumpAllocator() {
    printf("@dumpAllocator- "); dumpPageEntryMeta();
    printf("\t- "); block_dumpFreeLists();
    printf("\t- "); mpage_dumpFreeLists();
    puts("");
}
static void mpage_checkFreeLists();
static void checkAllocator() {
    mpage_checkFreeLists();
}
//////////////////////////////
static void buildPDE(int n) {
    char **pde = g_PDBase;
    while (*pde != NULL) ++pde;
    ASSERT(pde - g_PDBase + n <= PD_SIZE);

    int i = 0;
    for (; i < n; ++i, ++pde) {
        *pde = (char*)mem_sbrk(PAGE_SIZE);
        if (*pde == (void*)-1) ASSERT(0);

        int pageIdx = ptr2PageIdx(*pde);
        PageEntry_BlockMeta *meta = pageIdx2BlockMeta(pageIdx);
        meta->type = PAGE_ENTRY_TYPE_PAGETABLE;
    }
}
static int newPage() {
    char *ptr = mem_sbrk(PAGE_SIZE);
    if (ptr == (void*)-1) ASSERT(0);
    return ptr2PageIdx(ptr);
}
//////////////////////////////
static int block_size2BlockClassIdx(int size) {
    if (size <= BLOCK_CLASS_FENCE_0) {
        return (size + BLOCK_CLASS_SPACE_0 - 1) / BLOCK_CLASS_SPACE_0 - 1;
    } else if (size <= BLOCK_CLASS_FENCE_1) {
        return (size - BLOCK_CLASS_FENCE_0 + BLOCK_CLASS_SPACE_1 - 1) / BLOCK_CLASS_SPACE_1 + BLOCK_CLASS_COUNT_0 - 1;
    } else {
        ASSERT(size <= BLOCK_CLASS_FENCE_2);
        return (size - BLOCK_CLASS_FENCE_1 + BLOCK_CLASS_SPACE_2 - 1) / BLOCK_CLASS_SPACE_2 + BLOCK_CLASS_COUNT_01 - 1;
    }
}
static int block_blockClassIdx2BlockSize(int blockClassIdx) {
    if (blockClassIdx < BLOCK_CLASS_COUNT_0) {
        return (blockClassIdx + 1) * BLOCK_CLASS_SPACE_0;
    } else if (blockClassIdx < BLOCK_CLASS_COUNT_01) {
        return (blockClassIdx - BLOCK_CLASS_COUNT_0 + 1) * BLOCK_CLASS_SPACE_1 + BLOCK_CLASS_FENCE_0;
    } else {
        ASSERT(blockClassIdx < BLOCK_CLASS_COUNT_012);
        return (blockClassIdx - BLOCK_CLASS_COUNT_01 + 1) * BLOCK_CLASS_SPACE_2 + BLOCK_CLASS_FENCE_1;
    }
}
static int block_getFreeListLength(int blockClassIdx) {
    int len = 0;
    Block *block = g_blockFreeLists[blockClassIdx];
    for (; block != NULL; block = block->next) ++len;
    return len;
}
static void block_dumpFreeLists() {
    printf("@block_freeLists{");
    int blockClassIdx = 0;
    for (; blockClassIdx < BLOCK_CLASS_COUNT_012; ++blockClassIdx) {
        int len = block_getFreeListLength(blockClassIdx);
        if (len > 0) printf("(#%d,%d)", block_blockClassIdx2BlockSize(blockClassIdx), len);
    }
    puts("}");
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
    for (; off + blockSize <= PAGE_SIZE; off += blockSize) {
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
static int mpage_getFreeListLength(int mpageClassIdx) {
    int len = 0;
    MPage *mpage = g_mpageFreeLists[mpageClassIdx];
    for (; mpage != NULL; mpage = mpage->next) ++len;
    return len;
}
static void mpage_dumpFreeLists() {
    printf("@mpage_freeLists{");
    int mpageClassIdx = 0;
    for (; mpageClassIdx < MPAGE_CLASS_COUNT; ++mpageClassIdx) {
        int len = mpage_getFreeListLength(mpageClassIdx);
        if (len > 0) printf("(#%d,%d)", mpageClassIdx + 1, len);
    }
    puts("}");
}
static void mpage_checkFreeLists() {
    int mpageClassIdx = 0;
    for (; mpageClassIdx < MPAGE_CLASS_COUNT; ++mpageClassIdx) {
        MPage *mpage = g_mpageFreeLists[mpageClassIdx];
        for (; mpage != NULL; mpage = mpage->next) {
            int pageIdx = ptr2PageIdx(mpage);
            PageEntry_MPageMeta *meta = pageIdx2MPageMeta(pageIdx);
            ASSERT(meta->type == PAGE_ENTRY_TYPE_MPAGE);
            ASSERT(pageCount2mpageClassIdx(meta->pageCount) == mpageClassIdx);
            ASSERT(meta->allocated == 0);

            PageEntry_MPageMeta *footer = meta + meta->pageCount - 1;
            ASSERT(meta->type == footer->type);
            ASSERT(meta->pageCount == footer->pageCount);
            ASSERT(meta->allocated == footer->allocated);
        }
    }
}
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
    *freeList = mpage;
}
static void mpage_mergeMPage(PageEntry_MPageMeta *meta, char *ptr) {
    ASSERT(meta->allocated == 0);

    int pageIdx = ptr2PageIdx(ptr);
    int maxPageCount = getMaxPageCount();

    PageEntry_MPageMeta *nextMeta = NULL;
    if (pageIdx + meta->pageCount < maxPageCount) {
        nextMeta = nextMPageMeta(meta);
        if (nextMeta->type != PAGE_ENTRY_TYPE_MPAGE || nextMeta->allocated) nextMeta = NULL;
    }

    PageEntry_MPageMeta *prevMeta = NULL;
    if (pageIdx > 0 && (meta - 1)->type == PAGE_ENTRY_TYPE_MPAGE) {
        prevMeta = prevMPageMeta(meta);
        if (prevMeta->allocated) prevMeta = NULL;
    }

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
    ASSERT(meta->pageCount > pageCount && meta->allocated == 1);

    PageEntry_MPageMeta *newMeta = meta + pageCount;
    setMPageMeta(newMeta, 0, meta->pageCount - pageCount);
    mpage_linkMPageToFreeList(newMeta, ptr + pageCount * PAGE_SIZE);

    setMPageMeta(meta, meta->allocated, pageCount);
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
    MPage *mpage = NULL;
    int mpageClassIdx = pageCount2mpageClassIdx(pageCount);
    for (; mpageClassIdx < MPAGE_CLASS_COUNT - 1; ++mpageClassIdx) {
        if ((mpage = g_mpageFreeLists[mpageClassIdx]) != NULL) break;
    }
    if (mpage == NULL) {
        mpage = g_mpageFreeLists[MPAGE_CLASS_COUNT - 1];
        for (; mpage != NULL; mpage = mpage->next) {
            int pageIdx = ptr2PageIdx(mpage);
            PageEntry_MPageMeta *meta = pageIdx2MPageMeta(pageIdx);
            if (meta->pageCount >= pageCount) break;
        }
    }

    if (mpage == NULL) {
        mpage_newMPage(pageCount);
        mpageClassIdx = pageCount2mpageClassIdx(pageCount);
        mpage = g_mpageFreeLists[mpageClassIdx];
    }
    ASSERT(mpage != NULL);

    int pageIdx = ptr2PageIdx(mpage);
    PageEntry_MPageMeta *meta = pageIdx2MPageMeta(pageIdx);

    setMPageMeta(meta, 1, meta->pageCount);
    mpage_unlinkMPage(meta, (char*)mpage);

    if (meta->pageCount > pageCount) {
        mpage_splitMPage(meta, (char*)mpage, pageCount);
    }
    ASSERT(meta->pageCount == pageCount);

    return mpage;
}
static void mpage_free(PageEntry_MPageMeta *meta, char *ptr) {
    ASSERT(meta->type == PAGE_ENTRY_TYPE_MPAGE);
    setMPageMeta(meta, 0, meta->pageCount);
    mpage_linkMPageToFreeList(meta, ptr);
    mpage_mergeMPage(meta, ptr);
}
static void* mpage_realloc(PageEntry_MPageMeta *meta, char *ptr, int pageCount) {
    if (pageCount > meta->pageCount) {
        int pageIdx = ptr2PageIdx(ptr);
        int maxPageCount = getMaxPageCount();

        PageEntry_MPageMeta *nextMeta = NULL;
        if (pageIdx + meta->pageCount < maxPageCount) {
            nextMeta = nextMPageMeta(meta);
            if (nextMeta->type != PAGE_ENTRY_TYPE_MPAGE || nextMeta->allocated) nextMeta = NULL;
        }

        if (nextMeta != NULL && meta->pageCount + nextMeta->pageCount >= pageCount) {
            mpage_unlinkMPage(nextMeta, ptr + meta->pageCount * PAGE_SIZE);
            setMPageMeta(meta, 1, meta->pageCount + nextMeta->pageCount);
        }
    }
    if (meta->pageCount > pageCount) {
        mpage_splitMPage(meta, ptr, pageCount);
        return ptr;
    } else if (meta->pageCount == pageCount) {
        return ptr;
    } else {
        return NULL;
    }
}
//////////////////////////////
int mm_init(void) {
    int PD_SPACE = sizeof(*g_PDBase) * (PD_SIZE + 1);
    int BLOCK_FREELISTS_SPACE = sizeof(*g_blockFreeLists) * BLOCK_CLASS_COUNT_012;
    int MPAGE_FREELISTS_SPACE = sizeof(*g_mpageFreeLists) * MPAGE_CLASS_COUNT;
    int TOTAL_SPACE = size2PageCount(PD_SPACE + BLOCK_FREELISTS_SPACE + MPAGE_FREELISTS_SPACE) * PAGE_SIZE;

    char *ptr = mem_sbrk(TOTAL_SPACE);
    if (ptr == (void*)-1) return -1;

    g_pageBase = mem_sbrk(0);

    g_PDBase = (char**)ptr;
    memset(g_PDBase, 0, PD_SPACE);
    ptr += PD_SPACE;

    g_blockFreeLists = (Block**)ptr;
    memset(g_blockFreeLists, 0, BLOCK_FREELISTS_SPACE);
    ptr += BLOCK_FREELISTS_SPACE;

    g_mpageFreeLists = (MPage**)ptr;
    memset(g_mpageFreeLists, 0, MPAGE_FREELISTS_SPACE);
    ptr += MPAGE_FREELISTS_SPACE;

    ASSERT(ptr <= g_pageBase);

    buildPDE(PD_SIZE);

    (void)dumpAllocator;
    (void)checkAllocator;

    return 0;
}

void *mm_malloc(size_t size) {
    int boundedSize = boundupToDwSize(size);
    void *ptr;
    if (boundedSize <= BLOCK_CLASS_FENCE_2) {
        ptr = block_malloc(block_size2BlockClassIdx(boundedSize));
    } else {
        ptr = mpage_malloc(size2PageCount(boundedSize));
    }

    return ptr;
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
    if (ptr == NULL) return mm_malloc(size);
    if (size == 0) return mm_free(ptr), NULL;

    int boundedSize = boundupToDwSize(size);

    void *newPtr = NULL;
    int oldSize;

    int pageIdx = ptr2PageIdx(ptr);
    PageEntry_BlockMeta *meta = pageIdx2BlockMeta(pageIdx);
    if (meta->type == PAGE_ENTRY_TYPE_BLOCK) {
        oldSize = block_blockClassIdx2BlockSize(meta->blockClassIdx);
        if (oldSize >= boundedSize) newPtr = ptr;
    } else {
        oldSize = ((PageEntry_MPageMeta*)meta)->pageCount * PAGE_SIZE;
        newPtr = mpage_realloc((PageEntry_MPageMeta*)meta, ptr, size2PageCount(boundedSize));
    }

    if (newPtr == NULL) {
        newPtr = mm_malloc(size);
        memcpy(newPtr, ptr, oldSize);
        mm_free(ptr);
    }

    return newPtr;
}

#endif
