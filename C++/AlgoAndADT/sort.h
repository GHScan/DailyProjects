#include "pch.h"

#include <set>

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
static void _mergeSortTo2(T *begin, T *end, T *dest) {
    int count = end - begin;
    if (count <= 1) {
        if (count == 1) dest[0] = begin[0];
        return;
    }

    int largerHalf = count - count / 2;
    T *dend = dest + count;
    _mergeSortTo2(begin, begin + largerHalf, dend - largerHalf);
    _mergeSortTo2(begin + largerHalf, end, begin);
    _mergeCombine(begin, end - largerHalf, dend - largerHalf, dend, dest);
}
template<typename T>
static void mergeSort2(T *begin, T *end) {
    static vector<T> _temp;
    _temp.resize(end - begin);

    int count = end - begin;
    T *temp = &_temp[0];
    T *mid = begin + count / 2, *tmid = temp + count / 2, *tend = temp + count;
    _mergeSortTo2(begin, mid, temp);
    _mergeSortTo2(mid, end, tmid);
    _mergeCombine(temp, tmid, tmid, tend, begin);
}

template<typename T>
static void _mergeSortInplace3(T *begin, T *end, T *temp);
template<typename T>
static void _mergeSortTo3(T *begin, T *end, T *dest) {
    int count = end - begin;
    if (count <= 1) {
        if (count == 1) dest[0] = begin[0];
        return;
    }
    T *mid = begin + count / 2;
    _mergeSortInplace3(begin, mid, dest);
    _mergeSortInplace3(mid, end, dest);
    _mergeCombine(begin, mid, mid, end, dest);
}
template<typename T>
static void _mergeSortInplace3(T *begin, T *end, T *temp) {
    int count = end - begin;
    if (count <= 8) {
        insertionSort(begin, end);
        return;
    }
    T *mid = begin + count / 2, *tmid = temp + count / 2, *tend = temp + count;
    _mergeSortTo3(begin, mid, temp);
    _mergeSortTo3(mid, end, tmid);
    _mergeCombine(temp, tmid, tmid, tend, begin);
}
template<typename T>
static void mergeSort3(T *begin, T *end) {
    static vector<T> _temp;
    _temp.resize(end - begin);
    _mergeSortInplace3(begin, end, &_temp[0]);
}

template<typename T>
struct MergeCombine4State {
    T *begin, *end;
    T *begin2, *end2;
    T *dest;
    MergeCombine4State(T *_begin, T *_end, T *_begin2, T *_end2, T *_dest):
        begin(_begin), end(_end), begin2(_begin2), end2(_end2), dest(_dest){}
    MergeCombine4State(){}
};
template<typename T>
struct MergeSortInplace4State {
    T *begin, *end, *temp;
    int state;
    MergeSortInplace4State(T *_begin, T *_end, T *_temp): begin(_begin), end(_end), temp(_temp), state(0){}
    MergeSortInplace4State(){}
};
template<typename T>
struct MergeSortTo4State {
    T *begin, *end, *dest;
    int state;
    MergeSortTo4State(T *_begin, T *_end, T *_dest): begin(_begin), end(_end), dest(_dest), state(0){}
    MergeSortTo4State(){}
};
template<typename T> 
struct MergeSort4State {
    enum {
        T_MergeCombine,
        T_MergeSortInplace,
        T_MergeSortTo,
    } type;
    union {
        MergeCombine4State<T> combine;
        MergeSortInplace4State<T> inplace;
        MergeSortTo4State<T> to;
    };
    MergeSort4State(const MergeCombine4State<T>& _combine): type(T_MergeCombine), combine(_combine){}
    MergeSort4State(const MergeSortInplace4State<T>& _inplace): type(T_MergeSortInplace), inplace(_inplace){}
    MergeSort4State(const MergeSortTo4State<T>& _to): type(T_MergeSortTo), to(_to){}
    MergeSort4State(){}
};
template<typename T>
static void mergeSort4(T *begin, T *end) {
    static vector<T> _temp;
    _temp.resize(end - begin);

    MergeSort4State<T> _stack[32];
    MergeSort4State<T> *top = _stack;

    *top++ = MergeSort4State<T>(MergeSortInplace4State<T>(begin, end, &_temp[0]));

    while (top > _stack) {
        MergeSort4State<T>& s = top[-1];
        switch (s.type) {
            case MergeSort4State<T>::T_MergeCombine: {
                    MergeCombine4State<T>& combine = s.combine;
                    T *begin = combine.begin, *end = combine.end;
                    T *begin2 = combine.begin2, *end2 = combine.end2;
                    T *dest = combine.dest;
                    while (begin != end && begin2 != end2) {
                        if (begin[0] < begin2[0]) *dest++ = *begin++;
                        else *dest++ = *begin2++;
                    }
                    for (; begin != end; ++begin) *dest++ = *begin;
                    for (; begin2 != end2; ++begin2) *dest++ = *begin2;
                    --top;
                 }
                break;
            case MergeSort4State<T>::T_MergeSortInplace: {
                    MergeSortInplace4State<T>& inplace = s.inplace;
                    T *begin = inplace.begin, *end = inplace.end, *temp = inplace.temp;
                    int count = end - begin;
                    switch (inplace.state) {
                        case 0:
                            if (count <= 1) {
                                --top;
                            } else {
                                inplace.state = 1;
                                T *mid = begin + count / 2;
                                *top++ = MergeSort4State<T>(MergeSortTo4State<T>(begin, mid, temp));
                            }
                            break;
                        case 1: {
                                inplace.state = 2;
                                T *mid = begin + count / 2, *tmid = temp + count / 2;
                                *top++ = MergeSort4State<T>(MergeSortTo4State<T>(mid, end, tmid));
                            }
                            break;
                        case 2: {
                                T *tmid = temp + count / 2, *tend = temp + count;
                                top[-1] = MergeSort4State<T>(MergeCombine4State<T>(temp, tmid, tmid, tend, begin));
                            }
                            break;
                        default: assert(0); break;
                    }
                 }
                break;
            case MergeSort4State<T>::T_MergeSortTo: {
                    MergeSortTo4State<T>& to = s.to;
                    T *begin = to.begin, *end = to.end, *dest = to.dest;
                    int count = end - begin;
                    T *mid = begin + count / 2;
                    switch (to.state) {
                        case 0:
                            if (count <= 1) {
                                if (count == 1) dest[0] = begin[0];
                                --top;
                            } else {
                                to.state = 1;
                                *top++ = MergeSort4State<T>(MergeSortInplace4State<T>(begin, mid, dest));
                            }
                            break;
                        case 1: {
                                to.state = 2;
                                *top++ = MergeSort4State<T>(MergeSortInplace4State<T>(mid, end, dest));
                            }
                            break;
                        case 2: {
                                top[-1] = MergeSort4State<T>(MergeCombine4State<T>(begin, mid, mid, end, dest));
                            }
                            break;
                        default: assert(0); break;
                    }
            }
                break;
            default: assert(0); break;
        }
    }
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
static void quickSort1(T *begin, T *end) {
    if (end - begin <= 1) return;

    T *lo = begin + 1, *hi = end - 1;
    for (T *p = lo; p <= hi; ) {
        if (*p < *begin) swap(*p++, *lo++);
        else if (*p > *begin) swap(*p, *hi--);
        else ++p;
    }
    swap(*begin, lo[-1]);

    quickSort1(begin, lo - 1);
    quickSort1(hi + 1, end);
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

static int ANSICqsortCmp(const void *a, const void *b) { return *(int*)a - *(int*)b;}
static void ANSICqsort(int *begin, int *end) {
    qsort(begin, end - begin, sizeof(begin[0]), ANSICqsortCmp);
}

template<typename T>
static void stdSetSort(T *begin, T *end) {
    multiset<int> ints;
    for (T *p = begin; p != end; ++p) ints.insert(*p);
    for (int i : ints) *begin++ = i;
}

template<typename T>
class BSTSorter {
public:
    BSTSorter(T *begin, T *end): mRoot(nullptr), mAllocatorBase(nullptr), mAllocatorNodes(nullptr) {
        setupAllocator(end - begin);

        if (end - begin > 32 && isSorted(begin, end)) {
            binaryInserts(begin, end);
        } else {
            sequentialInserts(begin, end);
        }
        traverse(mRoot, begin);

        cleanupAllocator();
    }
private:
    struct Node {
        Node *left, *right;
        T value;
        Node(int _value): left(nullptr), right(nullptr), value(_value){}
    };
private:
    bool isSorted(T *begin, T *end) {
        int step = max(1, (end - begin) / 32);
        for (; begin + step < end; begin += step) {
            if (!(begin[0] <= begin[step])) return false;
        }
        return true;
    }
    void sequentialInserts(T *begin, T *end) {
        for (; begin != end; ++begin) insert(*begin);
    }
    void binaryInserts(T *begin, T *end) {
        if (end - begin <= 0) return;
        T *mid = begin + (end - begin) / 2;
        insert(*mid);
        binaryInserts(begin, mid);
        binaryInserts(mid + 1, end);
    }
    void insert(const T &value) {
        Node **p = &mRoot;
        for (; *p != nullptr; ) {
            if (value < (*p)->value) p = &(*p)->left;
            else p = &(*p)->right;
        }
        *p = createNode(value);
    }
    T* traverse(Node *n, T *begin) {
        if (n->left != nullptr) begin = traverse(n->left, begin);
        *begin++ = n->value;
        if (n->right != nullptr) begin = traverse(n->right, begin);
        return begin;
    }
private:
    Node* createNode(const T &value) {
        return new (mAllocatorNodes++)Node(value);
    }
    void setupAllocator(int n) {
        mAllocatorBase = malloc(n * sizeof(Node));
        mAllocatorNodes = (Node*)mAllocatorBase;
    }
    void cleanupAllocator() {
        free(mAllocatorBase);
    }
private:
    Node *mRoot;
    void *mAllocatorBase;
    Node *mAllocatorNodes;
};

template<typename T>
static void bstSort(T *begin, T *end) {
    BSTSorter<T>(begin, end);
}
//////////////////////////////

struct FuncItem {
    const char *name;
    void (*f)(int*, int*);
    int limit;
};

static void timeSortFuncs(vector<FuncItem> &funcs) {
    setCpuAffinity(1);

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

            // warm up cache
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
                printf("\t%16s : %.6fs\n", item.name, accumulate(times.begin(), times.begin() + 3, 0.0) / 3);
            }
        }
    }
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
        ITEM(mergeSort2, 0),
        ITEM(mergeSort3, 0),
        ITEM(mergeSort4, 0),
        ITEM(quickSort, 0),
        ITEM(quickSort1, 0),
        ITEM(quickSort2, 0),
        ITEM(quickSort3, 0),
        ITEM(quickSort4, 0),
        ITEM(quickSort5, 0),
        ITEM(quickSort6, 0),
        ITEM(sort, 0),
        ITEM(stable_sort, 0),
        ITEM(ANSICqsort, 0),
        ITEM(stdSetSort, 0),
        ITEM(bstSort, 0),
    };
#undef ITEM

    srand(time(nullptr));
    timeSortFuncs(items);
}
