#include "pch.h"
#include "FixsizeAllocator.h"

class SizeNAllocator
{
public:
    SizeNAllocator(int entrySize, int blockSize);
    ~SizeNAllocator();
    void* alloc();
    void free(void *p);
private:
    void allocBlock();
private:
    struct Entry
    {
        Entry *next;
        char data[1];
    };
private:
    int m_blockSize, m_entrySize;
    Entry *m_freeList;
    vector<void*> m_blocks;
};
SizeNAllocator::SizeNAllocator(int entrySize, int blockSize):
    m_freeList(NULL), m_blockSize(blockSize), m_entrySize(entrySize)
{
}
SizeNAllocator::~SizeNAllocator()
{
    for (auto &p : m_blocks) ::free(p);
}
void* SizeNAllocator::alloc()
{
    if (m_freeList == NULL) allocBlock();
    void *r = m_freeList;
    m_freeList = m_freeList->next;
    return r;
}
void SizeNAllocator::free(void *p)
{
    Entry *entry = (Entry*)p;
    entry->next = m_freeList;
    m_freeList = entry;
}
void SizeNAllocator::allocBlock()
{
    assert(m_entrySize >= 4);
    Entry *block = (Entry*)::malloc(m_blockSize * m_entrySize);
    Entry *ent = block;
    for (int i = 0; i < m_blockSize - 1; ++i) {
        Entry *nextEnt = (Entry*)((char*)ent + m_entrySize);
        ent->next = nextEnt;
        ent = nextEnt;
    }
    ent->next = m_freeList;
    m_freeList = block;
    m_blocks.push_back(block);
}

FixsizeAllocator::FixsizeAllocator()
{
    int sa[] = {4,8,16,24,32,40,48,64,80,96,128};
    for (auto sz : sa) {
        m_sizeNs.push_back(sz);
        m_sizeNAllocators.push_back(new SizeNAllocator(sz, (1<<20)/sz));
    }
}
FixsizeAllocator::~FixsizeAllocator()
{
    for (auto &p : m_sizeNAllocators) delete p;
}
void* FixsizeAllocator::malloc(int size)
{
    size += 1;
    auto iter = lower_bound(m_sizeNs.begin(), m_sizeNs.end(), size);
    if (iter == m_sizeNs.end()) {
        char *p = (char*)::malloc(size);
        p[0] = -1;
        return p + 1;
    }
    else {
        int idx = int(iter - m_sizeNs.begin());
        char *p = (char*)m_sizeNAllocators[idx]->alloc();
        p[0] = idx;
        return p + 1;
    }
}
void FixsizeAllocator::free(void *_p)
{
    char *p = (char*)_p;
    if (p[-1] == -1) {
        ::free(p - 1);
    }
    else {
        m_sizeNAllocators[p[-1]]->free(p - 1);
    }
}
