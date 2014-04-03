#include "pch.h"

#include "utils.h"
#include "linkedList.h"

static int
listEqual(LinkedList_T *l, int a[], int n) {
    if (LinkedList_size(l) != n) return 0;
    
    int i = 0;
    for (LinkedListIter_T *begin = LinkedList_begin(l), *end = LinkedList_end(l); 
            begin != end;
            begin = LinkedList_next(l, begin)) {
        if ((int)begin->value != a[i]) return 0;
        ++i;
    }

    return 1;
}

void
test_linkedList() {
    {
        LinkedList_T *l = LinkedList_create();
        assert(LinkedList_size(l) == 0);

        int a[] = {3, 1, 5, 8, 4, 2};
        int n = ARRAY_SIZE(a);
        for (int i = 1; i < n - 1; ++i) LinkedList_pushBack(l, (void*)a[i]);
        assert(listEqual(l, a + 1, n - 2));

        LinkedList_pushFront(l, (void*)a[0]);
        assert(listEqual(l, a, n - 1));

        LinkedList_insert(l, LinkedList_end(l), (void*)a[n - 1]);
        assert(listEqual(l, a, n));

        {
            LinkedListIter_T *it = LinkedList_begin(l);
            for (int i = 0; i < 3; ++i) it = LinkedList_next(l, it);
            LinkedList_remove(l, it);
            ARRAY_REMOVE(a, n, 3);
        }
        assert(listEqual(l, a, n));

        {
            int i = (int)LinkedList_popFront(l);
            assert(i == a[0]);
            ARRAY_REMOVE(a, n, 0);
        }
        assert(listEqual(l, a, n));

        {
            int i = (int)LinkedList_popBack(l);
            assert(i == a[n - 1]);
            ARRAY_REMOVE(a, n, n - 1);
        }
        assert(listEqual(l, a, n));

        LinkedList_destroy(&l);
    }
    {
        LinkedList_T *l = LinkedList_create();
        LinkedList_pushBack(l, (void*)3);
        assert(LinkedList_size(l) == 1);
        assert((int)LinkedList_begin(l)->value == 3);

        LinkedList_insert(l, LinkedList_begin(l), (void*)2);
        {
            int a[] = {2, 3};
            assert(listEqual(l, a, ARRAY_SIZE(a)));
        }
        assert((int)LinkedList_back(l) == 3);

        LinkedList_remove(l, LinkedList_begin(l));
        assert(LinkedList_size(l) == 1);
        assert((int)LinkedList_front(l) == 3);
        assert((int)LinkedList_back(l) == 3);

        LinkedList_popBack(l);
        assert(LinkedList_size(l) == 0);

        LinkedList_destroy(&l);
    }
}
