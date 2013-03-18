
#include "pch.h"

#include <time.h>

#include <limits>

#include "SkipList_Int2.h"
#include "MemoryPool.h"

//#define USE_MEMPOOL

struct SLNode
{
    SLNode *prev, *next, *lower;
    int key, *value;
    SLNode(int _key, int *_value): key(_key), value(_value), prev(NULL), next(NULL), lower(NULL){}

    void insertAfter(SLNode *o)
    {
        o->next = next, o->prev = this;
        if (next) next->prev = o;
        next = o;
    }
    SLNode* removeAfter()
    {
        SLNode *r = next;
        if (r->next) r->next->prev = this;
        next = r->next;
        return r;
    }
    static void* operator new (size_t size);
    static void operator delete(void *p);
};

FixSizeMemoryPool<sizeof(SLNode)> g_pool;

void* SLNode::operator new (size_t size)
{
#ifdef USE_MEMPOOL
    return g_pool._malloc();
#else
    return malloc(sizeof(SLNode));
#endif
}
void SLNode::operator delete(void *p)
{
#ifdef USE_MEMPOOL
    g_pool._free(p);
#else
    free(p);
#endif
}

SkipList_Int2::SkipList_Int2():
    m_maxLevel(1), m_size(0)
{
    m_head = new SLNode(numeric_limits<int>::min(), new int(0));
    m_tail = new SLNode(numeric_limits<int>::max(), new int(0));
    m_head->insertAfter(m_tail);
}
SkipList_Int2::~SkipList_Int2()
{
    while (m_maxLevel > 0) removeLevel();
}

void SkipList_Int2::set(int key, int value)
{
    insert(m_head, key, value);
    tryAddLevel();
}
SLNode* SkipList_Int2::insert(SLNode *head, int key, int value)
{
    SLNode *n = head->next;
    while (key > n->key) n = n->next;
    if (key == n->key) {
        *n->value = value;
        return NULL;
    }
    else {
        n = n->prev;
        if (n->lower == NULL) {
            SLNode *newN = new SLNode(key, new int(value));
            n->insertAfter(newN);
            ++m_size;
            return newN;
        }
        else {
            SLNode *lower = insert(n->lower, key, value);
            if (lower == NULL || rand() < (RAND_MAX / 2)) return NULL;
            SLNode *newN = new SLNode(key, lower->value);
            n->insertAfter(newN);
            newN->lower = lower;
            return newN;
        }
    }
}
int SkipList_Int2::get(int key, int def) const
{
    SLNode *n = find(key);
    if (n->key == key) return *n->value;
    return def;
}
void SkipList_Int2::erase(int key)
{
    SLNode *n = find(key);
    if (n->key != key) return;
    delete n->value;
    --m_size;
    while (n != NULL) {
        SLNode *temp = n;
        n = n->lower;
        temp->prev->removeAfter();
        delete temp;
    }
    //tryRemoveLevel();
}
SLNode* SkipList_Int2::find(int key) const
{
    SLNode *n = m_head->next;
    for (;;) {
        while (key > n->key) n = n->next;
        if (key == n->key) return n;
        else {
            if (n->lower == NULL) return n->prev;
            n = n->prev->lower->next;
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
    SLNode *newHead = new SLNode(numeric_limits<int>::min(), m_head->value);
    newHead->lower = m_head;

    SLNode *n = m_head->next, *n2 = newHead;
    while (n != m_tail) {
        if (rand() < (RAND_MAX / 2)) {
            SLNode *newN = new SLNode(n->key, n->value);
            n2->insertAfter(newN);
            newN->lower = n;
            n2 = newN;
        }
        n = n->next;
    }
    SLNode *newTail = new SLNode(numeric_limits<int>::max(), m_tail->value);
    n2->insertAfter(newTail);
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
        if (m_maxLevel == 0) delete temp->value;
        delete temp;
    }
}

vector<pair<int, int> > SkipList_Int2::toList() const
{
    vector<pair<int, int> > r;
    SLNode *head = m_head;
    while (head->lower != NULL) head = head->lower;
    SLNode *n = head->next;
    while (n->next != NULL) {
        r.push_back(make_pair(n->key, *n->value));
        n = n->next;
    }
    return r;
}
