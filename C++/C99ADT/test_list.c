#include "pch.h"

#include "utils.h"
#include "list.h"

static int 
listEqual(List_T *l, int a[], int n) {
    if (List_size(l) != n) return 0;

    int i = 0;
    for (ListIter_T *begin = List_begin(l), *end = List_end(l);
            begin != end;
            begin = List_next(l, begin)) {
        if ((int)begin->value != a[i]) return 0;
        ++i;
    }
    return 1;
}

void
test_list() {
    {
        List_T *l = List_create(1);
        assert(List_capacity(l) >= 1);
        assert(List_size(l) == 0);

        int a[] = {3, 1, 2, 4, 5, 8};
        int n = ARRAY_SIZE(a);
        for (int i = 0; i < n; ++i) {
            List_push(l, (void*)a[i]);
        }
        assert(listEqual(l, a, n));

        {
            void *p = List_pop(l);
            --n;
            assert((int)p == 8);
        }
        assert(listEqual(l, a, n));

        ARRAY_INSERT(a, n, 1, 10);
        List_insert(l, 1, (void*)10);
        assert(listEqual(l, a, n));

        assert(List_size(l) == n);
        for (int i = 0; i < n; ++i) {
            assert((int)List_element(l, i) == a[i]);
        }

        List_remove(l, 0);
        List_remove(l, List_size(l) - 1);
        ARRAY_REMOVE(a, n, 0);
        ARRAY_REMOVE(a, n, n - 1);
        assert(listEqual(l, a, n));

        List_resize(l, List_size(l) + 5, (void*)10);
        List_setElement(l, 0, (void*)23);
        assert((int)List_front(l) + (int)List_back(l) == 33);

        List_destroy(&l);
    }
    {
        List_T *l = List_create(10);
        List_resize(l, 5, (void*)0);
        List_setElement(l, 0, (void*)13);

        assert(List_size(l) == 5);
        assert(List_capacity(l) == 10);
        assert((int)List_element(l, 0) + (int)List_element(l, 4) == 13);

        List_destroy(&l);
    }
}
