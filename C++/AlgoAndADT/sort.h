#include "pch.h"

#include "Utils.h"

template<typename T>
static void bubbleSort(T *begin, T *end) {
    for (; begin < end; ++begin) {
        for (T *p = end - 1; p > begin; --p) {
            if (p[0] < p[-1]) swap(p[0], p[-1]);
        }
    }
}

template<typename T>
static void bubbleSort2(T *begin, T *end) {
    for (; begin < end; ++begin) {
        bool swaped = false;
        for (T *p = end - 1; p > begin; --p) {
            if (p[0] < p[-1]) {
                swap(p[0], p[-1]);
                swaped = true;
            }
        }
        if (!swaped) break;
    }
}

template<typename T>
static void selectionSort(T *begin, T *end) {
    for (; begin < end; ++begin) {
        for (T *p = begin + 1; p < end; ++p) {
            if (p[0] < begin[0]) swap(p[0], begin[0]);
        }
    }
}

template<typename T>
static void insertionSort(T *begin, T *end) {
    for (T *start = begin + 1; start < end; ++start) {
        for (T *p = start - 1; p >= begin; --p) {
            if (p[0] <= p[1]) break;
            swap(p[0], p[1]);
        }
    }
}

template<typename T>
static void shellSort(T *begin, T *end) {
    for (int gap = (end - begin) / 2; gap > 0; gap /= 2) {
        for (T *start = begin + gap; start < end; ++start) {
            for (T *p = start - gap; p >= begin; p -= gap) {
                if (p[0] <= p[gap]) break;
                swap(p[0], p[gap]);
            }
        }
    }
}

template<typename T>
static void _mergeCombine(T *begin, T *end, T *begin2, T *end2, T *dest) {
    while (begin != end && begin2 != end2) {
        if (begin[0] < begin2[0]) *dest++ = *begin++;
        else *dest++ = *begin2++;
    }
    for (; begin < end; ++begin) *dest++ = *begin;
    for (; begin2 < end2; ++begin2) *dest++ = *begin2;
}
template<typename T>
static void _mergeSortInplace(T *begin, T *end, T *temp);
template<typename T>
static void _mergeSortTo(T *begin, T *end, T *dest) {
    int count = end - begin;
    if (count <= 1) {
        if (count == 1) dest[0] = begin[0];
        return;
    }
    T *mid = begin + count / 2;
    _mergeSortInplace(begin, mid, dest);
    _mergeSortInplace(mid, end, dest);
    _mergeCombine(begin, mid, mid, end, dest);
}
template<typename T>
static void _mergeSortInplace(T *begin, T *end, T *temp) {
    int count = end - begin;
    if (count <= 1) return;
    T *mid = begin + count / 2, *tmid = temp + count / 2, *tend = temp + count;
    _mergeSortTo(begin, mid, temp);
    _mergeSortTo(mid, end, tmid);
    _mergeCombine(temp, tmid, tmid, tend, begin);
}
template<typename T>
static void mergeSort(T *begin, T *end) {
    static vector<T> _temp;
    _temp.resize(end - begin);
    _mergeSortInplace(begin, end, &_temp[0]);
}

template<typename T>
static void pushHeap(T *begin, T *end) {
    for (int cur = end - begin, parent; cur > 1; cur = parent) {
        parent = cur / 2;
        if (begin[parent - 1] < begin[cur - 1]) swap(begin[parent - 1], begin[cur - 1]);
        else break;
    }
}
template<typename T>
static void popHeap(T *begin, T *end) {
    int count = end - begin;
    swap(begin[0], begin[--count]);
    for (int cur = 1; cur * 2 <= count; ) {
        int max = cur * 2;
        int right = max + 1;
        if (right <= count && begin[max - 1] < begin[right - 1]) max = right;
        if (begin[cur - 1] < begin[max - 1]) {
            swap(begin[cur - 1], begin[max - 1]);
            cur = max;
        } else break;
    }
}
template<typename T>
static void heapSort(T *begin, T *end) {
    for (T *p = begin + 1; p <= end; ++p) pushHeap(begin, p);
    for (T *p = end; p > begin; --p) popHeap(begin, p);
}

template<typename T>
static void quickSort(T *begin, T *end) {
    if (end - begin <= 1) return;
    T *cur = begin;
    for (T *p = begin + 1; p < end; ++p) {
        if (p[0] <= begin[0]) swap(p[0], *++cur);
    }
    swap(begin[0], cur[0]);
    quickSort(begin, cur);
    quickSort(cur + 1, end);
}

template<typename T>
static void quickSort2(T *begin, T *end) {
    if (end - begin <= 1) return;
    swap(begin[0], begin[(end - begin) / 2]);
    T *cur = begin;
    for (T *p = begin + 1; p < end; ++p) {
        if (p[0] <= begin[0]) swap(p[0], *++cur);
    }
    swap(begin[0], cur[0]);
    quickSort2(begin, cur);
    quickSort2(cur + 1, end);
}

template<typename T>
static void quickSort3(T *begin, T *end) {
    if (end - begin <= 1) return;
    swap(begin[0], begin[myrand(0, end - begin)]);
    T *cur = begin;
    for (T *p = begin + 1; p < end; ++p) {
        if (p[0] <= begin[0]) swap(p[0], *++cur);
    }
    swap(begin[0], cur[0]);
    quickSort3(begin, cur);
    quickSort3(cur + 1, end);
}

template<typename T>
static void quickSort4(T *begin, T *end) {
    if (end - begin <= 8) {
        insertionSort(begin, end);
        return;
    }
    swap(begin[0], begin[(end - begin) / 2]);
    T *cur = begin;
    for (T *p = begin + 1; p < end; ++p) {
        if (p[0] <= begin[0]) swap(p[0], *++cur);
    }
    swap(begin[0], cur[0]);
    quickSort4(begin, cur);
    quickSort4(cur + 1, end);
}

template<typename T>
static void quickSort5(T *begin, T *end) {
    if (end - begin <= 8) {
        insertionSort(begin, end);
        return;
    }
    swap(begin[0], begin[myrand(0, end - begin)]);
    T *cur = begin;
    for (T *p = begin + 1; p < end; ++p) {
        if (p[0] <= begin[0]) swap(p[0], *++cur);
    }
    swap(begin[0], cur[0]);
    quickSort5(begin, cur);
    quickSort5(cur + 1, end);
}

template<typename T>
static void quickSort6(T *begin, T *end) {
    if (end - begin <= 8) {
        insertionSort(begin, end);
        return;
    }

    swap(begin[0], begin[(end - begin) / 2]);
    if (end[-1] < begin[0]) swap(begin[0], end[-1]);
    ++end[-1]; // Warning: hack!!! integer specific and may overflow

    T *lower = begin + 1, *upper = end - 2;
    for (; lower <= upper; ) {
        while (lower[0] <= begin[0]) ++lower;
        while (upper[0] > begin[0]) --upper;
        if (lower <= upper) swap(lower[0], upper[0]);
    }
    swap(begin[0], upper[0]);

    --end[-1]; // Warning: hack!!!

    quickSort6(begin, upper);
    quickSort6(upper + 1, end);
}
//////////////////////////////

struct FuncItem {
    const char *name;
    void (*f)(int*, int*);
    int limit;
};

static void timeSortFuncs(vector<FuncItem> &funcs) {
    int lens[] = {
        1, 5, 17, 31, 63, 95, 127, 257, 1025, 2045, 4097, 8190, 16*1024-1, 16*1024, 256*1024+1, 1024*1025,
    };
    for (int len : lens) {
        vector<int> v(len);
        for (int &i : v) i = myrand(0, len);

        bool sorted = ((len - 1) & len) == 0;
        printf("@@@@ len = %.3fK, %s\n", len / 1024.0, sorted ? "sorted" : "");
        for (FuncItem &item : funcs) {
            if (item.limit > 0 && len > item.limit) continue;

            // warn up cache
            if (sorted) sort(v.begin(), v.end());
            else random_shuffle(v.begin(), v.end());
            item.f(&v[0], &v[0] + v.size());
            assertSorted(&v[0], &v[0] + v.size());

            vector<double> times(8);
            for (double &time : times) {
                if (sorted) sort(v.begin(), v.end());
                else random_shuffle(v.begin(), v.end());

                time = getTime();
                item.f(&v[0], &v[0] + v.size());
                time = getTime() - time;
                assertSorted(&v[0], &v[0] + v.size());
            }

            sort(times.begin(), times.end());
            if (len >= 256) {
                printf("\t%16s : %.6fs\n", item.name, accumulate(times.begin(), times.end(), 0.0));
            }
        }
    }
}

static int ANSICqsortCmp(const void *a, const void *b) { return *(int*)a - *(int*)b;}
static void ANSICqsort(int *begin, int *end) {
    qsort(begin, end - begin, sizeof(begin[0]), ANSICqsortCmp);
}

int main() {
#define ITEM(f, limit) {#f, (void(*)(int*,int*))f, limit}
    vector<FuncItem> items = {
        ITEM(bubbleSort, 1024 * 16),
        ITEM(bubbleSort2, 1024 * 16),
        ITEM(selectionSort, 1024 * 16),
        ITEM(insertionSort, 1024 * 16),
        ITEM(shellSort, 0),
        ITEM(heapSort, 0),
        ITEM(mergeSort, 0),
        ITEM(quickSort, 0),
        ITEM(quickSort2, 0),
        ITEM(quickSort3, 0),
        ITEM(quickSort4, 0),
        ITEM(quickSort5, 0),
        ITEM(quickSort6, 0),
        ITEM(sort, 0),
        ITEM(ANSICqsort, 0),
    };
#undef ITEM

    srand(time(nullptr));
    timeSortFuncs(items);
}
