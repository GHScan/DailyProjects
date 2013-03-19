
#include "pch.h"

#include <assert.h>
#include <memory.h>

#include "SkipListArray.h"

struct SLANode
{
    struct Joint
    {
        int gap;
        SLANode *prev, *next;
    };

    int value;
    Joint joints[1];

    void insertAfter(int levelIdx, SLANode *n2)
    {
        auto &j1 = joints[levelIdx], &j2 = n2->joints[levelIdx];
        j2.prev = this, j2.next = j1.next;
        if (j1.next) j1.next->joints[levelIdx].prev = n2;
        j1.next = n2;
    }
    void removeAfter(int levelIdx)
    {
        auto &j1 = joints[levelIdx];
        auto &j2 = j1.next->joints[levelIdx];
        if (j2.next) j2.next->joints[levelIdx].prev = this;
        j1.next = j2.next;
    }
};

static SLANode* allocNode(int value, int level)
{
    int size = sizeof(SLANode) + sizeof(SLANode::Joint) * (level - 1);
    SLANode *n = (SLANode*)malloc(size);
    memset(n, 0, size);
    n->value = value;
    return n;
}
static void freeNode(SLANode *n)
{
    free(n);
}

SkipListArray::SkipListArray(int maxLevel):
    m_maxLevel(maxLevel), m_size(0)
{
    m_head = allocNode(0, m_maxLevel);
    m_tail = allocNode(0, m_maxLevel);
    for (int i = 0; i < m_maxLevel; ++i) m_head->insertAfter(i, m_tail);
}
SkipListArray::~SkipListArray()
{
    SLANode *n = m_head;
    while (n != NULL) {
        SLANode *temp = n;
        n = n->joints[0].next;
        freeNode(temp);
    }
}

void SkipListArray::insert(int off, int value)
{
    assert(off <= m_size);
    insert(m_head, m_maxLevel - 1, off, value);
}
pair<SLANode*, int> SkipListArray::insert(SLANode *head, int levelIdx, int off, int value)
{
    while (head->joints[levelIdx].gap < off) {
        off -= head->joints[levelIdx].gap + 1;
        head = head->joints[levelIdx].next;
    }
    if (levelIdx == 0) {
        assert(off == 0);
        int level = 1;
        while (level < m_maxLevel && rand() < (RAND_MAX / 2)) ++level;
        SLANode *newN = allocNode(value, level);
        head->insertAfter(0, newN);
        return make_pair(level > 1 ? newN : NULL, level - 1);
    }
    else {
        auto r = insert(head, levelIdx - 1, off, value);
        SLANode *newN = r.first;
        if (newN != NULL) {
            head->insertAfter(levelIdx, newN);
            newN->joints[levelIdx].gap = head->joints[levelIdx].gap - off;
            head->joints[levelIdx].gap = off;
            if (--r.second == 0) r.first = NULL;
        }
        else ++head->joints[levelIdx].gap;
        return r;
    }
}

void SkipListArray::erase(int off)
{
    assert(off < m_size);
    --m_size;
    erase(m_head, m_maxLevel - 1, off);
}
void SkipListArray::erase(SLANode *head, int levelIdx, int off)
{
    while (head->joints[levelIdx].gap < off) {
        off -= head->joints[levelIdx].gap + 1;
        head = head->joints[levelIdx].next;
    }
    if (head->joints[levelIdx].gap == off) {
        SLANode *n = head->joints[levelIdx].next;
        for (int i = 0; i <= levelIdx; ++i) {
            auto prevN = n->joints[i].prev;
            prevN->removeAfter(i);
            prevN->joints[i].gap += n->joints[i].gap;
        }
        freeNode(n);
    }
    else {
        erase(head, levelIdx - 1, off);
        --head->joints[levelIdx].gap;
    }
}

int SkipListArray::operator [] (int off) const
{
    return index(off)->value;
}
int& SkipListArray::operator [](int off)
{
    return index(off)->value;
}
SLANode* SkipListArray::index(int off) const
{
    assert(off <= m_size);
    SLANode *n = m_head;
    for (int levelIdx = m_maxLevel - 1; levelIdx >= 0; --levelIdx) {
        while (n->joints[levelIdx].gap < off) {
            off -= n->joints[levelIdx].gap + 1;
            n = n->joints[levelIdx].next;
        }
        if (n->joints[levelIdx].gap == off) return n->joints[levelIdx].next;
    }
    assert(0);
    return NULL;
}

vector<int> SkipListArray::toList() const
{
    vector<int> r;
    SLANode *n = m_head->joints[0].next;
    while (n != m_tail) {
        r.push_back(n->value);
        n = n->joints[0].next;
    }
    return r;
}
