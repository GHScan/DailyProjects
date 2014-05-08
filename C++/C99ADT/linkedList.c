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
    Node_T *dummy;
    int size;
} LinkedList_T;

static Node_T* 
createNode(void *value, Node_T *prev, Node_T *next) {
    Node_T *n = malloc(sizeof(*n));
    n->value = value;
    n->next = next;
    n->prev = prev;
    return n;
}

static void 
destroyNode(Node_T *n) {
    free(n);
}

LinkedList_T*       
LinkedList_create() {
    LinkedList_T *o = malloc(sizeof(*o));

    o->type = TYPE;
    o->size = 0;
    o->dummy = createNode(0, 0, 0);
    o->dummy->next = o->dummy->prev = o->dummy;

    return o;
}

void                
LinkedList_destroy(LinkedList_T **p) {
    assert(p != 0);
    assert(*p != 0 && (*p)->type == TYPE);

    for (Node_T *n = (*p)->dummy->next, *next; n != (*p)->dummy; n = next) {
        next = n->next;
        destroyNode(n);
    }
    destroyNode((*p)->dummy);
    free(*p);

    *p = 0;
}

void                
LinkedList_pushFront(LinkedList_T *o, void *p) {
    assert(o != 0 && o->type == TYPE);

    LinkedList_insert(o, LinkedList_begin(o), p);
}

void*               
LinkedList_popFront(LinkedList_T *o) {
    assert(o != 0 && o->type == TYPE && o->size > 0);

    LinkedListIter_T *it = LinkedList_begin(o);
    void* value = it->value;
    LinkedList_remove(o, it);
    return value;
}

void                
LinkedList_pushBack(LinkedList_T *o, void *p) {
    assert(o != 0 && o->type == TYPE);

    LinkedList_insert(o, LinkedList_end(o), p);
}

void*               
LinkedList_popBack(LinkedList_T *o) {
    assert(o != 0 && o->type == TYPE && o->size > 0);

    LinkedListIter_T *it = (LinkedListIter_T*)o->dummy->prev;
    void *value = it->value;
    LinkedList_remove(o, it);
    return value;
}

void*               
LinkedList_front(LinkedList_T *o) {
    assert(o != 0 && o->type == TYPE && o->size > 0);

    return o->dummy->next->value;
}

void*               
LinkedList_back(LinkedList_T *o) {
    assert(o != 0 && o->type == TYPE && o->size > 0);

    return o->dummy->prev->value;
}

int                 
LinkedList_size(LinkedList_T *o) {
    assert(o != 0 && o->type == TYPE);

    return o->size;
}

LinkedListIter_T*   
LinkedList_begin(LinkedList_T *o) {
    assert(o != 0 && o->type == TYPE);

    return (LinkedListIter_T*)o->dummy->next;
}

LinkedListIter_T*   
LinkedList_end(LinkedList_T *o) {
    assert(o != 0 && o->type == TYPE);

    return (LinkedListIter_T*)o->dummy;
}

LinkedListIter_T*   
LinkedList_next(LinkedList_T *o, LinkedListIter_T *it) {
    assert(o != 0 && o->type == TYPE);
    assert(it != 0);

    return (LinkedListIter_T*)((Node_T*)it)->next;
}

LinkedListIter_T*   
LinkedList_insert(LinkedList_T *o, LinkedListIter_T *it, void *p) {
    assert(o != 0 && o->type == TYPE);
    assert(it != 0);

    Node_T *n = (Node_T*)it;
    Node_T *prev = n->prev;
    Node_T *newN = createNode(p, prev, n);
    prev->next = n->prev = newN;
    ++o->size;

    return (LinkedListIter_T*)newN;
}

LinkedListIter_T*   
LinkedList_remove(LinkedList_T *o, LinkedListIter_T *it) {
    assert(o != 0 && o->type == TYPE && o->size > 0);
    assert(it != 0);

    Node_T *n = (Node_T*)it;
    Node_T *prev = n->prev, *next = n->next;
    prev->next = next;
    next->prev = prev;

    destroyNode(n);
    --o->size;

    return (LinkedListIter_T*)next;
}
