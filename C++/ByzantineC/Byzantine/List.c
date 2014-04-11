#include "Assert.h"
#include "List.h"
#include "Mem.h"

typedef struct Node {
    void *value;
    struct Node *prev, *next;
} Node;

typedef struct byList {
    Node head;
    int size;
    byAllocator *allocator;
} byList;

byList*     byList_create(byAllocator *allocator) {
    by_ASSERT(allocator != NULL, "");

    byList *p;
    by_NEW(p); // replace with byAllocator
    p->head.prev = p->head.next = &p->head;
    p->size = 0;
    p->allocator = allocator;
    return p;
}

void        byList_destroy(byList *this) {
    by_ASSERT(this != NULL, "");

    for (Node *n = this->head.next, *next; n != &this->head; n = next) {
        next = n->next;
        by_FREE(n);
    }
    by_FREE(this);
}

void        byList_pushBack(byList *this) {
}

void        byList_pushFront(byList *this) {
}

byListIter* byList_insert(byList *this, byListIter *it) {
}

void*       byList_popBack(byList *this) {
}

void*       byList_popFront(byList *this) {
}

byListIter* byList_erase(byList *this, byListIter *it) {
}

void        byList_clear(byList *this) {
}

void        byList_reverse(byList *this) {
}

void        byList_sort(byList *this) {
}

int         byList_size(byList *this) {
}

void        byList_splice(byList *this, byListIter *pos, byList *from, byListIter *begin, byListIter *end) {
}

int         byList_empty(byList *this) {
}

void*       byList_front(byList *this) {
}

void*       byList_back(byList *this) {
}


byListIter* byList_beginIter(byList *this) {
}

byListIter* byList_endIter(byList *this) {
}

byListIter* byList_nextIter(byList *this, byListIter *it) {
}

