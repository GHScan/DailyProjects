// vim: fileencoding=gbk

#ifndef MEMORYPOOL_H
#define MEMORYPOOL_H

#include <cassert>

#include <algorithm>

template<int _size>
class FixSizeMemoryPool
{
private:
    struct Node
    {
        union
        {
            Node *next;
            char buf[_size];
        };
    };
    struct Block
    {
        Block *next;
        Node nodes[1];
    };

public:
    FixSizeMemoryPool():
        m_blockHead(NULL), m_freeList(NULL), m_allocCnt(0), m_blockNodeCnt(4){}
    ~FixSizeMemoryPool()
    {
        assert(m_allocCnt == 0);
        m_freeList = NULL;
        while (m_blockHead != NULL) {
            Block *b = m_blockHead;
            m_blockHead = m_blockHead->next;
            ::free(b);
        }
    }
    void* _malloc()
    {
        if (m_freeList == NULL) allocBlock();
        assert(m_freeList != NULL);
        Node *n = m_freeList;
        m_freeList = m_freeList->next;
        ++m_allocCnt;
        return n;
    }
    void _free(void* p)
    {
        Node *n = (Node*)p;
        n->next = m_freeList;
        m_freeList = n;
        --m_allocCnt;
    }
    void swap(FixSizeMemoryPool &o)
    {
        std::swap(m_blockHead, o.m_blockHead);
        std::swap(m_freeList, o.m_freeList);
        std::swap(m_allocCnt, o.m_allocCnt);
        std::swap(m_blockNodeCnt, o.m_blockNodeCnt);
    }

private:
    void allocBlock()
    {
        assert(m_freeList == NULL);
        Block* b = (Block*)::malloc((m_blockNodeCnt - 1) * _size + sizeof(Block));
        b->next = m_blockHead;
        m_blockHead = b;
        for (int i = 0; i < m_blockNodeCnt; ++i) {
            b->nodes[i].next = m_freeList;
            m_freeList = &b->nodes[i];
        }
        m_blockNodeCnt = m_blockNodeCnt * 3 / 2;
    }

private:
    Block *m_blockHead;
    Node *m_freeList;
    int m_allocCnt;
    int m_blockNodeCnt;
};

namespace std
{
    template<int _size>
    inline void swap(FixSizeMemoryPool<_size>& a, FixSizeMemoryPool<_size>& b)
    {
        a.swap(b);
    }
}

#endif // #ifndef MEMORYPOOL_H
