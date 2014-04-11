#ifndef LIST_H
#define LIST_H

#include "Allocator.h"

typedef struct byList byList;
typedef struct byListIter {
    void *value;
} byListIter;

byList*     byList_create(byAllocator *allocator);
void        byList_destroy(byList *this);
void        byList_pushBack(byList *this);
void        byList_pushFront(byList *this);
byListIter* byList_insert(byList *this, byListIter *it);
void*       byList_popBack(byList *this);
void*       byList_popFront(byList *this);
byListIter* byList_erase(byList *this, byListIter *it);
void        byList_clear(byList *this);
void        byList_reverse(byList *this);
void        byList_sort(byList *this);
void        byList_splice(byList *this, byListIter *pos, byList *from, byListIter *begin, byListIter *end);
int         byList_size(byList *this);
int         byList_empty(byList *this);
void*       byList_front(byList *this);
void*       byList_back(byList *this);

byListIter* byList_beginIter(byList *this);
byListIter* byList_endIter(byList *this);
byListIter* byList_nextIter(byList *this, byListIter *it);

#endif
