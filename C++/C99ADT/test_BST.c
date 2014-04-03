
#include "pch.h"

#include "utils.h"
#include "BST.h"

static int
BSTEqual(BST_T *bst, int a[], int n) {
    if (BST_size(bst) != n) return 0;
    int i = 0;
    for (BSTIter_T *begin = BST_begin(bst), *end = BST_end(bst);
            begin != end;
            begin = BST_next(bst, begin)) {
        if ((int)begin->key != a[i] && (int)begin->value == a[i] * a[i]) return 0;
        ++i;
    }
    return 1;
}

static int
cmpInt(const void *a, const void *b) {
    return (int)a - (int)b;
}

void
test_BST() {
    BST_T *bst = BST_create(&cmpInt);
    {
        int a[] = {3, 1, 2, 4, 6, 5};
        for (int i = 0; i < ARRAY_SIZE(a); ++i) {
            BST_insert(bst, (void*)a[i], (void*)(a[i] * a[i]));
        }
    }
    int a[] = {1, 2, 3, 4, 5, 6};
    int n = ARRAY_SIZE(a);
    assert(BSTEqual(bst, a, n));

    assert((int)*BST_get(bst, (void*)5) == 25);
    assert((int)*BST_get(bst, (void*)2) == 4);
    assert(BST_get(bst, (void*)-1) == 0);

    {
        int r = BST_remove(bst, (void*)9);
        assert(r == 0);
    }
    BST_remove(bst, (void*)3);
    ARRAY_REMOVE(a, n, 2);
    assert(BSTEqual(bst, a, n));

    BST_remove(bst, (void*)1);
    ARRAY_REMOVE(a, n, 0);
    assert(BSTEqual(bst, a, n));

    BST_insert(bst, (void*)9, (void*)81);
    ARRAY_INSERT(a, n, n, 9);
    assert(BSTEqual(bst, a, n));

    BST_destroy(&bst);
}
