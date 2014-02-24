
#ifndef IMPL_malloc
#define IMPL_malloc

#include <assert.h>
#include <math.h>

#include <iostream>

#include "config.h"
#include "mm.h"
#include "memlib.h"

#define ASSERT assert
//#define ASSERT(b) do { if(!(b)) { printf("assert failed:(%d) %s", __LINE__, #b); *(int*)0 = 0;} } while(0)

//////////////////////////////
static constexpr int roundUp(int align, int n) {
    return (n + align - 1) / align * align;
}
//////////////////////////////
static const int ALIGN_SIZE = 8;
static const int HEAP_SIZE = MAX_HEAP;

static const int PAGE_SIZE_BITW = 12;
static const int PAGE_SIZE = 1 << PAGE_SIZE_BITW;
static const int PTE_SIZE_BITW = 2;
static const int PTE_SIZE = 1 << PTE_SIZE_BITW;
static const int PAGE_TABLE_SIZE_BITW = PAGE_SIZE_BITW - PTE_SIZE_BITW;
static const int PAGE_TABLE_SIZE = 1 << PAGE_TABLE_SIZE_BITW;
static constexpr int toPageCount(int size) {
    return roundUp(PAGE_SIZE, size) / PAGE_SIZE;
}
static constexpr int toPageTableCount(int pageCount) {
    return roundUp(PAGE_TABLE_SIZE, pageCount) / PAGE_TABLE_SIZE;
}
static const int PAGE_DIRECTORY_SIZE = toPageTableCount(toPageCount(HEAP_SIZE));

static const int BLOCK_CLASS_SPACE_0 = ALIGN_SIZE;
static const int BLOCK_CLASS_SPACE_1 = ALIGN_SIZE * 2;
static const int BLOCK_CLASS_SPACE_2 = ALIGN_SIZE * 4;
static const int BLOCK_CLASS_FENCE_0 = PAGE_SIZE * 2 / 8;
static const int BLOCK_CLASS_FENCE_1 = PAGE_SIZE * 3 / 8;
static const int BLOCK_CLASS_FENCE_2 = PAGE_SIZE * 4 / 8;
static const int BLOCK_CLASS_COUNT_0 = BLOCK_CLASS_FENCE_0 / BLOCK_CLASS_SPACE_0;
static const int BLOCK_CLASS_COUNT_01 = (BLOCK_CLASS_FENCE_1 - BLOCK_CLASS_FENCE_0) / BLOCK_CLASS_SPACE_1 + BLOCK_CLASS_COUNT_0;
static const int BLOCK_CLASS_COUNT_012 = (BLOCK_CLASS_FENCE_2 - BLOCK_CLASS_FENCE_1) / BLOCK_CLASS_SPACE_2 + BLOCK_CLASS_COUNT_01;

static const int MPAGE_CLASS_COUNT = 256;
//////////////////////////////
static class PageManager *g_pageMgr;
static class BlockFreeListManager *g_blockFreeListMgr;
static class MPageFreeListManager *g_mpageFreeListMgr;
//////////////////////////////
class BlockMetaPageTableEntry;
class MPageMetaPageTableEntry;
struct PageTableEntry {
    enum Type {
        Null,
        PageTable,
        BlockMeta,
        MPageMeta,
    };
    BlockMetaPageTableEntry* toBlockMeta() { return (BlockMetaPageTableEntry*)this; }
    BlockMetaPageTableEntry* castToBlockMeta() { return type == BlockMeta ? (BlockMetaPageTableEntry*)this : NULL; }
    MPageMetaPageTableEntry* toMPageMeta() { return (MPageMetaPageTableEntry*)this; }
    MPageMetaPageTableEntry* castToMPageMeta() { return type == MPageMeta ? (MPageMetaPageTableEntry*)this : NULL; }
    unsigned int type : 2;
};
struct BlockMetaPageTableEntry {
    void reset(int _classIdx) {
        type = PageTableEntry::BlockMeta;
        classIdx = _classIdx;
    }
    unsigned int type : 2;
    unsigned int classIdx : 30;
};
struct MPageMetaPageTableEntry {
    void _reset(int _allocated, int _pageCount) {
        type = PageTableEntry::MPageMeta; 
        allocated = _allocated; 
        pageCount = _pageCount;
    }
    unsigned int type : 2;
    unsigned int allocated : 1;
    unsigned int pageCount : 29;
};
static_assert(sizeof(PageTableEntry) == PTE_SIZE, "");
static_assert(sizeof(BlockMetaPageTableEntry) == PTE_SIZE, "");
static_assert(sizeof(MPageMetaPageTableEntry) == PTE_SIZE, "");
//////////////////////////////
class PageManager {
public:
    PageManager(char *firstPageBase): mFirstPageBase(firstPageBase), mPageCount(0) {
        memset(mPageDirectory, 0, sizeof(mPageDirectory));
        mNullPTE.type = PageTableEntry::Null;
    }
    int getPageIndex(void *ptr) {
        return ((char*)ptr - mFirstPageBase) / PAGE_SIZE;
    }
    char* getPagePtr(int pageIdx) {
        return mFirstPageBase + pageIdx * PAGE_SIZE;
    }
    PageTableEntry* getPageTableEntry(int pageIdx) {
        if (pageIdx < 0 || pageIdx >= mPageCount) return &mNullPTE;

        PageTableEntry *pageTable = mPageDirectory[(unsigned)pageIdx >> PAGE_TABLE_SIZE_BITW];
        ASSERT(pageTable != NULL);
        return pageTable + pageIdx % PAGE_TABLE_SIZE;
    }
    int createPage(int pageCount) {
        preparePageTable(pageCount);

        int pageIdx = mPageCount;
        sbrk(pageCount * PAGE_SIZE);
        return pageIdx;
    }
private:
    void preparePageTable(int newPageCount) {
        int oldPageTableCount = toPageTableCount(mPageCount);
        int newPageTableCount = toPageTableCount(mPageCount + newPageCount);
        if (oldPageTableCount == newPageTableCount) return;

        newPageTableCount = toPageTableCount(mPageCount + newPageCount + (newPageTableCount - oldPageTableCount));

        for (; oldPageTableCount < newPageTableCount; ++oldPageTableCount) {
            ASSERT(oldPageTableCount < PAGE_DIRECTORY_SIZE);

            int pageIdx = mPageCount;
            mPageDirectory[oldPageTableCount] = (PageTableEntry*)sbrk(PAGE_SIZE);
            getPageTableEntry(pageIdx)->type = PageTableEntry::PageTable;
        }
    }
    char* sbrk(int inc) {
         char *p = (char*)mem_sbrk(inc);
         if (p == (void*)-1) ASSERT(0);
         mPageCount = getPageIndex(mem_sbrk(0));
         return p;
    }
private:
    PageTableEntry* mPageDirectory[PAGE_DIRECTORY_SIZE];
    PageTableEntry mNullPTE;
    char *mFirstPageBase;
    int mPageCount;
};
//////////////////////////////
struct Block {
    Block *next;
};
class BlockFreeListManager {
public:
    BlockFreeListManager() {
        memset(mFreeLists, 0, sizeof(mFreeLists));
    }
    void* allocMem(int size) {
        int classIdx = size2ClassIdx(size);
        Block*& freeList = mFreeLists[classIdx];
        if (freeList == NULL) createPage(classIdx);
        ASSERT(freeList != NULL);
        Block* block = freeList;
        freeList = block->next;
        return block;
    }
    void freeMem(void *ptr, BlockMetaPageTableEntry *entry) {
        Block *block = (Block*)ptr;
        Block*& freeList = mFreeLists[entry->classIdx];
        block->next = freeList;
        freeList = block;
    }
    static int size2ClassIdx(int size) {
        ASSERT(size <= BLOCK_CLASS_FENCE_2);
        if (size <= BLOCK_CLASS_FENCE_0) {
            return (size + BLOCK_CLASS_SPACE_0 - 1) / BLOCK_CLASS_SPACE_0 - 1;
        } else if (size <= BLOCK_CLASS_FENCE_1) {
            return (size - BLOCK_CLASS_FENCE_0 + BLOCK_CLASS_SPACE_1 - 1) / BLOCK_CLASS_SPACE_1 + BLOCK_CLASS_COUNT_0 - 1;
        } else {
            return (size - BLOCK_CLASS_FENCE_1 + BLOCK_CLASS_SPACE_2 - 1) / BLOCK_CLASS_SPACE_2 + BLOCK_CLASS_COUNT_01 - 1;
        }
    }
    static int classIdx2Size(int classIdx) {
        ASSERT(classIdx < BLOCK_CLASS_COUNT_012);
        if (classIdx < BLOCK_CLASS_COUNT_0) {
            return (classIdx + 1) * BLOCK_CLASS_SPACE_0;
        } else if (classIdx < BLOCK_CLASS_COUNT_01) {
            return (classIdx + 1 - BLOCK_CLASS_COUNT_0) * BLOCK_CLASS_SPACE_1 + BLOCK_CLASS_FENCE_0;
        } else {
            return (classIdx + 1 - BLOCK_CLASS_COUNT_01) * BLOCK_CLASS_SPACE_2 + BLOCK_CLASS_FENCE_1;
        }
    }
private:
    void createPage(int classIdx) {
        int pageIdx = g_pageMgr->createPage(1);
        g_pageMgr->getPageTableEntry(pageIdx)->toBlockMeta()->reset(classIdx);

        char *ptr = g_pageMgr->getPagePtr(pageIdx);
        int size = classIdx2Size(classIdx);
        Block*& freeList = mFreeLists[classIdx];

        for (int off = 0; off + size <= PAGE_SIZE; off += size) {
            Block *block = (Block*)(ptr + off);
            block->next = freeList;
            freeList = block;
        }
    }
private:
    Block* mFreeLists[BLOCK_CLASS_COUNT_012];
};
//////////////////////////////
struct MPage {
    MPage *prev, *next;
    MPageMetaPageTableEntry *entry;
};
struct MPageMeta {
    MPageMeta(void *_ptr, int _pageIdx, MPageMetaPageTableEntry *_entry): ptr(_ptr), pageIdx(_pageIdx), entry(_entry) { }
    int isValid() { return ptr != NULL; }
    MPageMeta getPrevFreeMeta() {
        if (MPageMetaPageTableEntry *footer = g_pageMgr->getPageTableEntry(pageIdx - 1)->castToMPageMeta()) {
            if (!footer->allocated) {
                int prevPageIdx = pageIdx - footer->pageCount;
                return MPageMeta(g_pageMgr->getPagePtr(prevPageIdx), prevPageIdx, g_pageMgr->getPageTableEntry(prevPageIdx)->toMPageMeta());
            }
        }
        return MPageMeta(NULL, -1, NULL);
    }
    MPageMeta getNextFreeMeta() {
        int nextPageIdx = pageIdx + entry->pageCount;
        if (MPageMetaPageTableEntry *nextPageEntry = g_pageMgr->getPageTableEntry(nextPageIdx)->castToMPageMeta()) {
            if (!nextPageEntry->allocated) {
                return MPageMeta(g_pageMgr->getPagePtr(nextPageIdx), nextPageIdx, nextPageEntry);
            }
        }
        return MPageMeta(NULL, -1, NULL);
    }
    MPageMetaPageTableEntry* getFooterEntry() {
        return g_pageMgr->getPageTableEntry(pageIdx + entry->pageCount - 1)->toMPageMeta();
    }
    void setProperty(int allocated, int pageCount) {
        entry->_reset(allocated, pageCount);
        *getFooterEntry() = *entry;
        if (!allocated) {
            ((MPage*)ptr)->entry = entry;
        }
    }
    MPage* getMPage() { return (MPage*)ptr; }

    void *const ptr;
    int const pageIdx;
    MPageMetaPageTableEntry *const entry;
};
class MPageFreeListManager {
public:
    MPageFreeListManager() {
        memset(mFreeLists, 0, sizeof(mFreeLists));
    }
    void* allocMem(int size) {
        int pageCount = toPageCount(size);
        int classIdx = pageCount2ClassIdx(pageCount);

        MPage *mpage = NULL;
        for (int i = classIdx; i < MPAGE_CLASS_COUNT - 1; ++i) {
            if ((mpage = mFreeLists[i]) != NULL) break;
        }
        if (mpage == NULL) {
            mpage = mFreeLists[MPAGE_CLASS_COUNT - 1];
            for (; mpage != NULL; mpage = mpage->next) {
                if (mpage->entry->pageCount >= pageCount) break;
            }
        }

        if (mpage == NULL) {
            createPage(pageCount);
            mpage = mFreeLists[classIdx];
        }
        ASSERT(mpage != NULL);

        MPageMeta meta(mpage, g_pageMgr->getPageIndex(mpage), mpage->entry);
        meta.setProperty(1, mpage->entry->pageCount);
        unlink(&meta);

        if (mpage->entry->pageCount > pageCount) {
            split(&meta, pageCount);
        }
        ASSERT(mpage->entry->pageCount == pageCount);

        return mpage;
    }
    void freeMem(void* ptr, int pageIdx, MPageMetaPageTableEntry *entry) {
        MPageMeta meta(ptr, pageIdx, entry);
        meta.setProperty(0, entry->pageCount);
        linkToFreeList(&meta);
        merge(&meta);
    }
    void* reallocMem(void* ptr, int pageIdx, MPageMetaPageTableEntry *entry, int size) {
        MPageMeta meta(ptr, pageIdx, entry);
        int pageCount = toPageCount(size);

        if (pageCount > entry->pageCount) {
            MPageMeta nextMeta(meta.getNextFreeMeta());
            if (nextMeta.isValid() && entry->pageCount + nextMeta.entry->pageCount >= pageCount) {
                unlink(&nextMeta);
                meta.setProperty(1, entry->pageCount + nextMeta.entry->pageCount);
            }
        }

        if (entry->pageCount > pageCount) {
            split(&meta, pageCount);
            return ptr;
        } else if (entry->pageCount == pageCount) {
            return ptr;
        } else {
            return NULL;
        }
    }
    static int pageCount2ClassIdx(int pageCount) {
        if (pageCount >= MPAGE_CLASS_COUNT) return MPAGE_CLASS_COUNT - 1;
        return pageCount - 1;
    }
private:
    void createPage(int pageCount) {
        int pageIdx = g_pageMgr->createPage(pageCount);
        MPageMeta meta(g_pageMgr->getPagePtr(pageIdx), pageIdx, g_pageMgr->getPageTableEntry(pageIdx)->toMPageMeta());
        meta.setProperty(0, pageCount);
        linkToFreeList(&meta);
    }
    void linkToFreeList(MPageMeta* meta) {
        MPage*& freeList = mFreeLists[pageCount2ClassIdx(meta->entry->pageCount)];
        MPage *mpage = meta->getMPage();

        mpage->next = freeList;
        mpage->prev = NULL;
        if (freeList != NULL) freeList->prev = mpage;
        freeList = mpage;
    }
    void unlink(MPageMeta *meta) {
        MPage*& freeList = mFreeLists[pageCount2ClassIdx(meta->entry->pageCount)];
        MPage *mpage = meta->getMPage();

        if (mpage->next != NULL) mpage->next->prev = mpage->prev;
        if (mpage->prev != NULL) mpage->prev->next = mpage->next;
        if (freeList == mpage) freeList = mpage->next;
    }
    void split(MPageMeta *meta, int pageCount) {
        ASSERT(meta->entry->pageCount > pageCount && meta->entry->allocated);

        int newPageIdx = meta->pageIdx + pageCount;
        MPageMeta newMeta(g_pageMgr->getPagePtr(newPageIdx), newPageIdx, g_pageMgr->getPageTableEntry(newPageIdx)->toMPageMeta());
        newMeta.setProperty(0, meta->entry->pageCount - pageCount);
        linkToFreeList(&newMeta);

        meta->setProperty(1, pageCount);
    }
    void merge(MPageMeta *meta) {
        MPageMeta prevMeta(meta->getPrevFreeMeta()), nextMeta(meta->getNextFreeMeta());
        if (!prevMeta.isValid() && !nextMeta.isValid()) return;

        unlink(meta);

        MPageMeta *mergeMeta = meta;
        int mergePageCount = meta->entry->pageCount;

        if (nextMeta.isValid()) {
            unlink(&nextMeta);
            mergePageCount += nextMeta.entry->pageCount;
        }
        if (prevMeta.isValid()) {
            unlink(&prevMeta);
            mergePageCount += prevMeta.entry->pageCount;
            mergeMeta = &prevMeta;
        }

        mergeMeta->setProperty(0, mergePageCount);
        linkToFreeList(mergeMeta);
    }
private:
    MPage* mFreeLists[MPAGE_CLASS_COUNT];
};
//////////////////////////////
int mm_init(void) {
    int totalSpace = sizeof(PageManager) + sizeof(BlockFreeListManager) + sizeof(MPageFreeListManager);
    totalSpace = roundUp(PAGE_SIZE, totalSpace);

    char *ptr = (char*)mem_sbrk(totalSpace);
    if (ptr == (void*)-1) return -1;

    g_pageMgr = new (ptr) PageManager((char*)mem_sbrk(0));
    ptr += sizeof(*g_pageMgr);

    g_blockFreeListMgr = new (ptr) BlockFreeListManager();
    ptr += sizeof(*g_blockFreeListMgr);

    g_mpageFreeListMgr = new (ptr) MPageFreeListManager();
    ptr += sizeof(*g_mpageFreeListMgr);

    return 0;
}

void *mm_malloc(size_t size) {
    void *newPtr;
    if (size <= BLOCK_CLASS_FENCE_2) {
        newPtr = g_blockFreeListMgr->allocMem(size);
    } else {
        newPtr = g_mpageFreeListMgr->allocMem(size);
    }
    return newPtr;
}

void mm_free(void *ptr) {
    int pageIdx = g_pageMgr->getPageIndex(ptr);
    PageTableEntry *entry = g_pageMgr->getPageTableEntry(pageIdx);
    if (BlockMetaPageTableEntry *blockEntry = entry->castToBlockMeta()) {
        g_blockFreeListMgr->freeMem(ptr, blockEntry);
    } else {
        g_mpageFreeListMgr->freeMem(ptr, pageIdx, entry->toMPageMeta());
    }
}

void *mm_realloc(void *ptr, size_t size) {
    if (ptr == NULL) return mm_malloc(size);
    if (size == 0) return mm_free(ptr), (void*)NULL;

    void *newPtr = NULL;
    int oldSize = 0;

    int pageIdx = g_pageMgr->getPageIndex(ptr);
    PageTableEntry *entry = g_pageMgr->getPageTableEntry(pageIdx);
    if (BlockMetaPageTableEntry *blockEntry = entry->castToBlockMeta()) {
        oldSize = g_blockFreeListMgr->classIdx2Size(blockEntry->classIdx);
        if (oldSize >= (int)size) newPtr = ptr;
    } else {
        MPageMetaPageTableEntry *mpageEntry  = entry->toMPageMeta();
        oldSize = mpageEntry->pageCount * PAGE_SIZE;
        newPtr = g_mpageFreeListMgr->reallocMem(ptr, pageIdx, mpageEntry, size);
    }

    if (newPtr == NULL) {
        newPtr = mm_malloc(size);
        memcpy(newPtr, ptr, oldSize);
        mm_free(ptr);
    }

    return newPtr;
}

#endif
