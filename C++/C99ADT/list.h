#ifndef LIST_H
#define LIST_H

typedef struct List_T List_T;
typedef struct ListIter_T {
    void *value;
} ListIter_T;

List_T*         List_create(int capacity);
void            List_destroy(List_T **o);
void            List_push(List_T *o, void *p);
void*           List_pop(List_T *o);
void            List_insert(List_T *o, int idx, void *p);
void            List_remove(List_T *o, int idx);
void*           List_front(List_T *o);
void*           List_back(List_T *o);
void*           List_element(List_T *o, int idx);
void            List_setElement(List_T *o, int idx, void *p);
int             List_size(List_T *o);
void            List_resize(List_T *o, int size, void *initv);
int             List_capacity(List_T *o);
ListIter_T*     List_begin(List_T *o);
ListIter_T*     List_end(List_T *o);
ListIter_T*     List_next(List_T *o, ListIter_T *it);

#endif
