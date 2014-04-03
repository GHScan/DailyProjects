#include "pch.h"

#include "utils.h"
#include "hashTable.h"

static int
hashTableEqual(HashTable_T *h, int a[], int n) {
    if (HashTable_size(h) != n) return 0;
    for (int i = 0; i < n; ++i) {
        void **p = HashTable_get(h, (void*)a[i]);
        if (p == 0 || (int)*p != a[i] * a[i]) return 0;
    }
    for (HashTableIter_T *begin = HashTable_begin(h), *end = HashTable_end(h);
            begin != end;
            begin = HashTable_next(h, begin)) {
        int r;
        ARRAY_FIND(r, a, n, (int)begin->key);
        if (r == -1) return 0;
    }
    return 1;
}

static unsigned intHash(const void *k) {
    int i = (int)k;
    return HashTable_hashBuf(&i, sizeof(i));
}
static int intEqual(const void *a, const void *b) {
    return (int)a == (int)b;
}

void 
test_hashTable() {
    {
        HashTable_T *h = HashTable_create(intHash, intEqual);
        int a[] = {3, 1, 5, 4, 8, 2};
        int n = ARRAY_SIZE(a);
        for (int i = 0; i < n; ++i) HashTable_insert(h, (void*)a[i], (void*)(a[i] * a[i]));
        assert(hashTableEqual(h, a, n));

        assert((int)*HashTable_get(h, (void*)3) == 9);
        assert(HashTable_get(h, (void*)7) == 0);

        HashTable_remove(h, (void*)4);
        ARRAY_REMOVE(a, n, 3);
        assert(hashTableEqual(h, a, n));

        HashTable_remove(h, (void*)3);
        ARRAY_REMOVE(a, n, 0);
        assert(hashTableEqual(h, a, n));

        HashTable_remove(h, (void*)7);
        assert(hashTableEqual(h, a, n));

        HashTable_insert(h, (void*)7, (void*)49);
        ARRAY_INSERT(a, n, 0, 7);
        assert(hashTableEqual(h, a, n));

        HashTable_destroy(&h);
    }
    {
        HashTable_T *h = HashTable_create(intHash, intEqual);
        for (int i = 0; i < 1000; i += 3) {
            HashTable_insert(h, (void*)i, (void*)intHash((const void*)i));
        }
        assert(HashTable_size(h) == 334);
        for (int i = 0; i < 1000; ++i) {
            if (i % 3 == 0) {
                assert((unsigned)*HashTable_get(h, (void*)i) == intHash((const void*)i));
            } else {
                assert(HashTable_get(h, (void*)i) == 0);
            }
        }

        HashTable_destroy(&h);
    }
}
