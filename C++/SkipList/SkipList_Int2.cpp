
#include "pch.h"

#include <time.h>

#include <limits>

#include "SkipList_Int2.h"
#include "MemoryPool.h"

struct SLNode
{
    SLNode *prev, *next, *lower;
    int key, value;
};

FixSizeMemoryPool<sizeof(SLNode)> g_pool;
//#define USE_MEMPOOL

static SLNode* allocNode(int key, int value)
{
#ifdef USE_MEMPOOL
    SLNode* r = (SLNode*)g_pool.malloc();
#else
    SLNode* r = (SLNode*)malloc(sizeof(SLNode));
#endif
    r->next = r->prev = r->lower = NULL;
    r->key = key, r->value = value;
    return r;
}
static void freeNode(SLNode *n)
{
#ifdef USE_MEMPOOL
    g_pool.free(n);
#else
    free(n);
#endif
}
inline static void insertAfter(SLNode *n1, SLNode *n2)
{
    n2->next = n1->next, n2->prev = n1;
    if (n1->next) n1->next->prev = n2;
    n1->next = n2;
}
inline static SLNode* removeAfter(SLNode *n)
{
    SLNode *n2 = n->next;
    if (n2->next) n2->next->prev = n;
    n->next = n2->next;
    return n2;
}

SkipList_Int2::SkipList_Int2():
    m_maxLevel(1), m_size(0)
{
    m_head = allocNode(numeric_limits<int>::min(), 0);
    m_tail = allocNode(numeric_limits<int>::max(), 0);
    insertAfter(m_head, m_tail);
}
SkipList_Int2::~SkipList_Int2()
{
    while (m_maxLevel > 1) removeLevel();
}

void SkipList_Int2::set(int key, int value)
{
    insert(m_head, key, value);
    tryAddLevel();
}
SLNode* SkipList_Int2::insert(SLNode *head, int key, int value)
{
    SLNode *n = head;
    while (key > n->next->key) n = n->next;
    if (key == n->next->key) {
        while (n != NULL) {
            n->value = value;
            n = n->next;
        }
        return NULL;
    }
    else {
        if (n->lower == NULL) {
            SLNode *newN = allocNode(key, value);
            insertAfter(n, newN);
            ++m_size;
            return newN;
        }
        else {
            SLNode *lower = insert(n->lower, key, value);
            if (lower == NULL || rand() < (RAND_MAX / 2)) return NULL;
            SLNode *newN = allocNode(key, value);
            insertAfter(n, newN);
            newN->lower = lower;
            return newN;
        }
    }
}
int SkipList_Int2::get(int key, int def) const
{
    SLNode *n = find(key);
    if (n->next->key == key) return n->next->value;
    return def;
}
void SkipList_Int2::erase(int key)
{
    SLNode *n = find(key);
    if (n->next->key != key) return;
    --m_size;
    n = n->next;
    while (n != NULL) {
        SLNode *temp = n;
        n = n->lower;
        removeAfter(temp->prev);
        freeNode(temp);
    }
}
SLNode* SkipList_Int2::find(int key) const
{
    SLNode *n = m_head;
    for (;;) {
        while (key > n->next->key) n = n->next;
        if (key == n->next->key) return n;
        else {
            if (n->lower == NULL) return n;
            n = n->lower;
        }
    }
    return NULL;
}

void SkipList_Int2::tryRemoveLevel()
{
    if (m_size < (1 << (m_maxLevel - 1))) removeLevel();
}
void SkipList_Int2::tryAddLevel()
{
    if (m_size > (1 << (m_maxLevel))) addLevel();
}
void SkipList_Int2::addLevel()
{
    SLNode *newHead = allocNode(numeric_limits<int>::min(), 0);
    newHead->lower = m_head;

    SLNode *n = m_head->next, *n2 = newHead;
    while (n != m_tail) {
        if (rand() < (RAND_MAX / 2)) {
            SLNode *newN = allocNode(n->key, n->value);
            insertAfter(n2, newN);
            newN->lower = n;
            n2 = newN;
        }
        n = n->next;
    }
    SLNode *newTail = allocNode(numeric_limits<int>::max(), 0);
    insertAfter(n2, newTail);
    newTail->lower = m_tail;
    m_head = newHead, m_tail = newTail;
    ++m_maxLevel;
}
void SkipList_Int2::removeLevel()
{
    --m_maxLevel;
    SLNode *n = m_head;
    m_head = m_head->lower, m_tail = m_tail->lower;
    while (n != NULL) {
        SLNode *temp = n;
        n = n->next;
        freeNode(temp);
    }
}

vector<pair<int, int> > SkipList_Int2::toList() const
{
    vector<pair<int, int> > r;
    SLNode *head = m_head;
    while (head->lower != NULL) head = head->lower;
    SLNode *n = head->next;
    while (n->next != NULL) {
        r.push_back(make_pair(n->key, n->value));
        n = n->next;
    }
    return r;
}
