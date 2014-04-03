#ifndef BST_H
#define BST_H

typedef struct BST_T BST_T;
typedef struct BSTIter_T {
    const void *key;
    void *value;
} BSTIter_T;
typedef int (*BST_Cmp)(const void *a, const void *b);

BST_T*          BST_create(BST_Cmp cmp);
void            BST_destroy(BST_T **o);
void            BST_insert(BST_T *o, void *k, void *v);
int             BST_remove(BST_T *o, void *k);
void**          BST_get(BST_T *o, void *k);
int             BST_size(BST_T *o);
BSTIter_T*      BST_begin(BST_T *o);
BSTIter_T*      BST_end(BST_T *o);
BSTIter_T*      BST_next(BST_T *o, BSTIter_T *it);

#endif
