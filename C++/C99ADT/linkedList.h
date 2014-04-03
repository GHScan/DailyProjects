#ifndef LINKED_LIST_H
#define LINKED_LIST_H

typedef struct LinkedList_T LinkedList_T;
typedef struct LinkedListIter_T {
    void *value;
} LinkedListIter_T;

LinkedList_T*       LinkedList_create();
void                LinkedList_destroy(LinkedList_T **o);
void                LinkedList_pushFront(LinkedList_T *o, void *p);
void*               LinkedList_popFront(LinkedList_T *o);
void                LinkedList_pushBack(LinkedList_T *o, void *p);
void*               LinkedList_popBack(LinkedList_T *o);
void*               LinkedList_front(LinkedList_T *o);
void*               LinkedList_back(LinkedList_T *o);
int                 LinkedList_size(LinkedList_T *o);
LinkedListIter_T*   LinkedList_begin(LinkedList_T *o);
LinkedListIter_T*   LinkedList_end(LinkedList_T *o);
LinkedListIter_T*   LinkedList_next(LinkedList_T *o, LinkedListIter_T *it);
LinkedListIter_T*   LinkedList_insert(LinkedList_T *o, LinkedListIter_T *it, void *p);
LinkedListIter_T*   LinkedList_remove(LinkedList_T *o, LinkedListIter_T *it);

#endif
