
#include "pch.h"

#include <limits>

#include "SkipList_Int2_v2.h"

struct SLNode_v2
{
    struct Joint
    {
        SLNode_v2 *prev, *next;
    };

    int key, value;
    Joint joints[1];

    void insertAfter(int levelIdx, SLNode_v2 *n2)
    {
        Joint &j1 = joints[levelIdx], &j2 = n2->joints[levelIdx];
        j2.prev = this, j2.next = j1.next;
        if (j1.next) j1.next->joints[levelIdx].prev = n2;
        j1.next = n2;
    }
    void removeAfter(int levelIdx)
    {
        Joint &j1 = joints[levelIdx], &j2 = j1.next->joints[levelIdx];
        if (j2.next) j2.next->joints[levelIdx].prev = this;
        j1.next = j2.next;
    }
};
static SLNode_v2* allocNode(int key, int value, int level)
{
    SLNode_v2 *n = (SLNode_v2*)malloc(sizeof(SLNode_v2) + sizeof(SLNode_v2*) * 2 * (level - 1));
    n->key = key, n->value = value;
    for (int i = 0; i < level; ++i) n->joints[i].prev = n->joints[i].next = NULL;
    return n;
}
static void freeNode(SLNode_v2 *n)
{
    free(n);
}

SkipList_Int2_v2::SkipList_Int2_v2(int maxLevel):
    m_maxLevel(maxLevel), m_size(0)
{
    m_head = allocNode(numeric_limits<int>::min(), 0, m_maxLevel);
    m_tail = allocNode(numeric_limits<int>::max(), 0, m_maxLevel);
    for (int i = 0; i < m_maxLevel; ++i) m_head->insertAfter(i, m_tail);
}
SkipList_Int2_v2::~SkipList_Int2_v2()
{
    SLNode_v2 *n = m_head;
    while (n != NULL) {
        SLNode_v2 *temp = n;
        n = n->joints[0].next;
        freeNode(temp);
    }
}

void SkipList_Int2_v2::set(int key, int value)
{
    insert(m_head, m_maxLevel - 1, key, value);
}
pair<SLNode_v2*, int> SkipList_Int2_v2::insert(SLNode_v2 *head, int levelIdx, int key, int value)
{
    SLNode_v2 *n = head->joints[levelIdx].next;
    while (key > n->key) n = n->joints[levelIdx].next;
    if (key == n->key) {
        n->value = value;
        return make_pair((SLNode_v2*)NULL, 0);
    }
    else {
        n = n->joints[levelIdx].prev;
        if (levelIdx == 0) {
            int level = 1;
            while (level < m_maxLevel && rand() < (RAND_MAX / 2)) ++level;
            SLNode_v2 *newN = allocNode(key, value, level);
            n->insertAfter(0, newN);
            ++m_size;
            return make_pair(newN, level - 1);
        }
        else {
            auto r = insert(n, levelIdx - 1, key, value);
            if (r.first != NULL) {
                n->insertAfter(levelIdx, r.first);
                if (--r.second == 0) r.first = NULL;
            }
            return r;
        }
    }
}
int SkipList_Int2_v2::get(int key, int def) const
{
    SLNode_v2 *n = NULL;
    find(key, n);
    if (n->key == key) return n->value;
    return def;
}
void SkipList_Int2_v2::erase(int key)
{
    SLNode_v2 *n = NULL;
    int levelIdx = find(key, n);
    if (n->key != key) return;
    --m_size;
    for (int i = 0; i <= levelIdx; ++i) n->joints[i].prev->removeAfter(i);
    freeNode(n);
}
int SkipList_Int2_v2::find(int key, SLNode_v2 *&n) const
{
    n = m_head;
    for (int levelIdx = m_maxLevel - 1; levelIdx >= 0; --levelIdx) {
        n = n->joints[levelIdx].next;
        while (key > n->key) n = n->joints[levelIdx].next;
        if (key == n->key) return levelIdx;
        else n = n->joints[levelIdx].prev;
    }
    return m_maxLevel - 1;
}

vector<pair<int, int> > SkipList_Int2_v2::toList() const
{
    vector<pair<int, int> > r;
    SLNode_v2 *n = m_head->joints[0].next;
    while (n != m_tail) {
        r.push_back(make_pair(n->key, n->value));
        n = n->joints[0].next;
    }
    return r;
}
