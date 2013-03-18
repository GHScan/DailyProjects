
#include "pch.h"

#include <time.h>

#include <limits>

#include "SkipList.h"

static Node* allocNode(int key, int value)
{
    Node* r = (Node*)malloc(sizeof(Node));
    r.next = r.prev = r.lower = NULL;
    return r;
}
static void freeNode(Node *node)
{
    free(node);
}

SkipList_Int2::SkipList_Int2():
    m_maxLevel(1)
{
    m_head = allocNode(numeric_limits<int>::min(), 0);
    m_tail = allocNode(numeric_limits<int>::max(), 0);
    m_head.next = m_tail;
    m_tail.prev = m_head;
}
SkipList_Int2::~SkipList_Int2()
{
}

void SkipList_Int2::set(int key, int value)
{
}
int SkipList_Int2::get(int key, int def)
{
    if (empty()) return def;
    Ndoe *head = m_head, *tail = m_tail;
    for (;;) {
        Node *cur = head->next;
        while ()
    }
}
void SkipList_Int2::erase(int key)
{
}
