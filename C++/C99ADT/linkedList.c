#include "pch.h"

#include "utils.h"
#include "linkedList.h"

static const char *TYPE = "LinkedList_T";

typedef struct Node_T {
    void *value;
    struct Node_T *prev, *next;
} Node_T;

typedef struct LinkedList_T {
    const char *type;
    Node_T *head, *tail;
    int size;
} LinkedList_T;

static Node_T* 
createNode(void *value, Node_T *prev, Node_T *next) {
    Node_T *n = malloc(sizeof(Node_T));
    n->value = value;
    n->prev = prev;
    n->next = next;
    return n;
}

LinkedList_T*       
LinkedList_create() {
    LinkedList_T *o = malloc(sizeof(LinkedList_T));
    o->type = TYPE;

    o->head = createNode(0, 0, 0);
    o->tail = o->head;
    o->size = 0;

    assert(o && o->type == TYPE);
    return o;
}

void                
LinkedList_destroy(LinkedList_T **p) {
    assert(p && *p && (*p)->type == TYPE);

    LinkedList_T *o = *p;
    for (Node_T *n = o->head, *next; n; n = next) {
        next = n->next;
        free(n);
    }

    free(o);
    *p = 0;
}

void                
LinkedList_pushFront(LinkedList_T *o, void *p) {
    assert(o && o->type == TYPE);

    Node_T *n = createNode(p, 0, o->head);
    n->next->prev = n;
    o->head = n;
    ++o->size;
}

void*               
LinkedList_popFront(LinkedList_T *o) {
    assert(o && o->type == TYPE);
    assert(o->size > 0);

    Node_T *n = o->head;
    n->next->prev = 0;
    o->head = n->next;

    void *r = n->value;
    free(n);
    --o->size;

    return r;
}

void                
LinkedList_pushBack(LinkedList_T *o, void *p) {
    assert(o && o->type == TYPE);

    Node_T *tail = o->tail;
    Node_T *n = createNode(p, tail->prev, tail);
    if (tail->prev) tail->prev->next = n;
    else o->head = n;
    tail->prev = n;

    ++o->size;
}

void*               
LinkedList_popBack(LinkedList_T *o) {
    assert(o && o->type == TYPE);
    assert(o->size > 0);

    Node_T *n = o->tail->prev;
    if (n->prev) n->prev->next = n->next;
    else o->head = n->next;
    n->next->prev = n->prev;

    void *r = n->value;
    free(n);
    --o->size;

    return r;
}

void*               
LinkedList_front(LinkedList_T *o) {
    assert(o && o->type == TYPE);
    assert(o->size > 0);

    return o->head->value;
}

void*               
LinkedList_back(LinkedList_T *o) {
    assert(o && o->type == TYPE);
    assert(o->size > 0);

   return o->tail->prev->value;
}

int                 
LinkedList_size(LinkedList_T *o) {
    assert(o && o->type == TYPE);

    return o->size;
}

LinkedListIter_T*   
LinkedList_begin(LinkedList_T *o) {
    assert(o && o->type == TYPE);

    return (LinkedListIter_T*)o->head;
}

LinkedListIter_T*   
LinkedList_end(LinkedList_T *o) {
    assert(o && o->type == TYPE);

    return (LinkedListIter_T*)o->tail;
}

LinkedListIter_T*   
LinkedList_next(LinkedList_T *o, LinkedListIter_T *it) {
    assert(o && o->type == TYPE);

    return (LinkedListIter_T*)((Node_T*)it)->next;
}

LinkedListIter_T*   
LinkedList_insert(LinkedList_T *o, LinkedListIter_T *it, void *p) {
    assert(o && o->type == TYPE);

    Node_T *pos = (Node_T*)it;
    Node_T *n = createNode(p, pos->prev, pos);
    if (n->prev) n->prev->next = n;
    else o->head = n;
    n->next->prev = n;
    ++o->size;

    return (LinkedListIter_T*)n;
}

LinkedListIter_T*   
LinkedList_remove(LinkedList_T *o, LinkedListIter_T *it) {
    assert(o && o->type == TYPE);

    Node_T *n = (Node_T*)it;
    if (n->prev) n->prev->next = n->next;
    else o->head = n->next;
    n->next->prev = n->prev;

    Node_T *r = n->next;
    free(n);
    --o->size;

    return (LinkedListIter_T*)r;
}
