#include "pch.h"

#include <set>

#include "Utils.h"

#define INSERTION_CUTOFF 32

template<typename T>
static void bubbleSort(T *begin, T *end) {
    for (; begin < end; ++begin) {
        for (T *p = end - 1; p > begin; --p) {
            if (p[0] < p[-1]) swap(p[0], p[-1]);
        }
    }
}

template<typename T>
static void bubbleSort_orderedSpecial(T *begin, T *end) {
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
static void insertionSort_swap(T *begin, T *end) {
    for (T *p = begin + 1; p < end; ++p) {
        for (T *q = p - 1; q >= begin && q[0] > q[1]; --q) {
            swap(q[0], q[1]);
        }
    }
}

template<typename T>
static void insertionSort(T *begin, T *end) {
    for (T *p = begin + 1; p < end; ++p) {
        T v = p[0], *q = p - 1;
        for (; q >= begin && q[0] > v; --q) {
            q[1] = q[0];
        }
        q[1] = v;
    }
}

template<typename T>
static void insertionSort_guard(T *begin, T *end) {
    for (T *p = begin + 1; p < end; ++p) {
        T v = p[0], *q, backup = *begin;
        *begin = v;
        for (q = p - 1; q[0] > v; --q) q[1] = q[0];
        *begin = backup;
        if (q[0] > v) {
            q[1] = q[0];
            --q;
        }
        q[1] = v;
    }
}

template<typename T>
static void shellSort_2n(T *begin, T *end) {
    for (int gap = int(end - begin) / 2; gap > 0; gap /= 2) {
        for (T *start = begin + gap; start < end; ++start) {
            for (T *p = start - gap; p >= begin; p -= gap) {
                if (p[0] <= p[gap]) break;
                swap(p[0], p[gap]);
            }
        }
    }
}

template<typename T>
static void shellSort_3n(T *begin, T *end) {
    int h;
    for (h = 1; h < end - begin; h = h * 3 + 1);
    for (h /= 3; h > 0; h /= 3) {
        for (T *p = begin + h; p != end; ++p) {
            for (T *q = p - h; q >= begin && q[0] > q[h]; q -= h) {
                swap(q[0], q[h]);
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
static void _mergeSortMove(T *begin, T *end, T *dest) {
    int count = int(end - begin);
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
    int count = int(end - begin);
    if (count <= 1) return;
    T *mid = begin + count / 2, *tmid = temp + count / 2, *tend = temp + count;
    _mergeSortMove(begin, mid, temp);
    _mergeSortMove(mid, end, tmid);
    _mergeCombine(temp, tmid, tmid, tend, begin);
}
template<typename T>
static void mergeSort_moveAndInplace(T *begin, T *end) {
    static vector<T> _temp;
    _temp.resize(end - begin);
    _mergeSortInplace(begin, end, &_temp[0]);
}

template<typename T>
static void _mergeSortMove2(T *begin, T *end, T *dest) {
    int count = int(end - begin);
    if (count <= 1) {
        if (count == 1) dest[0] = begin[0];
        return;
    }

    int largerHalf = count - count / 2;
    T *dend = dest + count;
    _mergeSortMove2(begin, begin + largerHalf, dend - largerHalf);
    _mergeSortMove2(begin + largerHalf, end, begin);
    _mergeCombine(begin, end - largerHalf, dend - largerHalf, dend, dest);
}
template<typename T>
static void mergeSort_move(T *begin, T *end) {
    static vector<T> _temp;
    _temp.resize(end - begin);

    int count = int(end - begin);
    T *temp = &_temp[0];
    T *mid = begin + count / 2, *tmid = temp + count / 2, *tend = temp + count;
    _mergeSortMove2(begin, mid, temp);
    _mergeSortMove2(mid, end, tmid);
    _mergeCombine(temp, tmid, tmid, tend, begin);
}

template<typename T>
static void _mergeSortInplace3(T *begin, T *end, T *temp);
template<typename T>
static void _mergeSortMove3(T *begin, T *end, T *dest) {
    int count = int(end - begin);
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
    int count = int(end - begin);
    if (count <= INSERTION_CUTOFF) {
        insertionSort(begin, end);
        return;
    }
    T *mid = begin + count / 2, *tmid = temp + count / 2, *tend = temp + count;
    _mergeSortMove3(begin, mid, temp);
    _mergeSortMove3(mid, end, tmid);
    _mergeCombine(temp, tmid, tmid, tend, begin);
}
template<typename T>
static void mergeSort_moveAndInplace_insertion(T *begin, T *end) {
    static vector<T> _temp;
    _temp.resize(end - begin);
    _mergeSortInplace3(begin, end, &_temp[0]);
}

#ifdef _MSC_VER
template<typename T>
static void mergeSort_iteration(T *begin, T *end) {
    return mergeSort_move(begin, end);
}
#else
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
struct MergeSortMove4State {
    T *begin, *end, *dest;
    int state;
    MergeSortMove4State(T *_begin, T *_end, T *_dest): begin(_begin), end(_end), dest(_dest), state(0){}
    MergeSortMove4State(){}
};
template<typename T> 
struct MergeSort4State {
    enum {
        T_MergeCombine,
        T_MergeSortInplace,
        T_MergeSortMove,
    } type;
    union {
        MergeCombine4State<T> combine;
        MergeSortInplace4State<T> inplace;
        MergeSortMove4State<T> to;
    };
    MergeSort4State(const MergeCombine4State<T>& _combine): type(T_MergeCombine), combine(_combine){}
    MergeSort4State(const MergeSortInplace4State<T>& _inplace): type(T_MergeSortInplace), inplace(_inplace){}
    MergeSort4State(const MergeSortMove4State<T>& _to): type(T_MergeSortMove), to(_to){}
    MergeSort4State(){}
};
template<typename T>
static void mergeSort_iteration(T *begin, T *end) {
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
                                *top++ = MergeSort4State<T>(MergeSortMove4State<T>(begin, mid, temp));
                            }
                            break;
                        case 1: {
                                inplace.state = 2;
                                T *mid = begin + count / 2, *tmid = temp + count / 2;
                                *top++ = MergeSort4State<T>(MergeSortMove4State<T>(mid, end, tmid));
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
            case MergeSort4State<T>::T_MergeSortMove: {
                    MergeSortMove4State<T>& to = s.to;
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
#endif

template<typename T>
static void pushHeap(T *begin, T *end) {
    for (int cur = int(end - begin), parent; cur > 1; cur = parent) {
        parent = cur / 2;
        if (begin[parent - 1] < begin[cur - 1]) swap(begin[parent - 1], begin[cur - 1]);
        else break;
    }
}
template<typename T>
static void popHeap(T *begin, T *end) {
    int count = int(end - begin);
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
static void pushHeap1(T *begin, T *end) {
    int count = int(end - begin--);
    for (int cur = count, parent; cur > 1; cur = parent) {
        parent = cur / 2;
        if (begin[parent] < begin[cur]) swap(begin[parent], begin[cur]);
        else break;
    }
}
template<typename T>
static void popHeap1(T *begin, T *end) {
    int count = int(end - begin--);
    swap(begin[1], begin[count--]);
    for (int cur = 1; cur * 2 <= count; ) {
        int max = cur * 2;
        int right = max + 1;
        if (right <= count && begin[max] < begin[right]) max = right;
        if (begin[cur] < begin[max]) {
            swap(begin[cur], begin[max]);
            cur = max;
        } else break;
    }
}
template<typename T>
static void heapSort1(T *begin, T *end) {
    for (T *p = begin + 1; p <= end; ++p) pushHeap1(begin, p);
    for (T *p = end; p > begin; --p) popHeap1(begin, p);
}

template<typename T>
static void siftDown(T *begin, int l, int n) {
    int v = begin[l];
    for (int c = l * 2; c <= n; c = l * 2) {
        if (c + 1 <= n && begin[c] < begin[c + 1]) ++c;
        if (begin[c] <= v) break;
        begin[l] = begin[c];
        l = c;
    }
    begin[l] = v;
}
template<typename T>
static void makeHeap(T *begin, T *end) {
    int count = int(end - begin--);
    for (int i = count / 2; i >= 1; --i) {
        siftDown(begin, i, count);
    }
}
template<typename T>
static void heapSort_optimal(T *begin, T *end) {
    makeHeap(begin, end);

    int count = int(end - begin--);
    for (int i = count; i > 1; ) {
        swap(begin[1], begin[i]);
        siftDown(begin, 1, --i);
    }
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
static void quickSort_insertion(T *begin, T *end) {
    if (end - begin <= INSERTION_CUTOFF) {
        insertionSort(begin, end);
        return;
    }
    T *mid = begin;
    for (T *p = begin + 1; p != end; ++p) {
        if (*p <= *begin) swap(*p, *++mid);
    }
    swap(*begin, *mid);
    quickSort_insertion(begin, mid);
    quickSort_insertion(mid + 1, end);
}

template<typename T>
static void quickSort_guard(T *begin, T *end) {
    if (end - begin <= 1) return;
    T *mid = end;
    for (T *p = end - 1; p >= begin; ) {
        while (*p < *begin) --p;
        swap(*p--, *--mid);
    }
    quickSort_guard(begin, mid);
    quickSort_guard(mid + 1, end);
}

template<typename T>
static void quickSort_split2Segment(T *begin, T *end) {
    if (end - begin <= INSERTION_CUTOFF) return;
    T *mid = begin;
    for (T *p = begin + 1; p != end; ++p) {
        if (*p <= *begin) swap(*p, *++mid);
    }
    swap(*begin, *mid);
    quickSort_split2Segment(begin, mid);
    quickSort_split2Segment(mid + 1, end);
}
template<typename T>
static void quickSort_segmentThenPostInsertion(T *begin, T *end) {
    quickSort_split2Segment(begin, end);
    insertionSort(begin, end);
}

template<typename T>
static void quickSort_3way(T *begin, T *end) {
    if (end - begin <= 1) return;

    T *lo = begin + 1, *hi = end - 1;
    for (T *p = lo; p <= hi; ) {
        if (*p < *begin) swap(*p++, *lo++);
        else if (*p > *begin) swap(*p, *hi--);
        else ++p;
    }
    swap(*begin, lo[-1]);

    quickSort_3way(begin, lo - 1);
    quickSort_3way(hi + 1, end);
}

template<typename T>
static void quickSort_swapMid(T *begin, T *end) {
    if (end - begin <= 1) return;
    swap(begin[0], begin[(end - begin) / 2]);
    T *cur = begin;
    for (T *p = begin + 1; p < end; ++p) {
        if (p[0] <= begin[0]) swap(p[0], *++cur);
    }
    swap(begin[0], cur[0]);
    quickSort_swapMid(begin, cur);
    quickSort_swapMid(cur + 1, end);
}

template<typename T>
static void quickSort_guard_swapMid(T *begin, T *end) {
    if (end - begin <= 1) return;
    swap(begin[0], begin[(end - begin) / 2]);
    T *mid = end;
    for (T *p = end - 1; p >= begin; ) {
        while (*p < *begin) --p;
        swap(*p--, *--mid);
    }
    quickSort_guard_swapMid(begin, mid);
    quickSort_guard_swapMid(mid + 1, end);
}

template<typename T>
static void quickSort_guard_swapMid_insertion(T *begin, T *end) {
    if (end - begin <= INSERTION_CUTOFF) {
        insertionSort(begin, end);
        return;
    }

    swap(begin[0], begin[(end - begin) / 2]);
    T *mid = end;
    for (T *p = end - 1; p >= begin; ) {
        while (*p < *begin) --p;
        swap(*p--, *--mid);
    }
    quickSort_guard_swapMid_insertion(begin, mid);
    quickSort_guard_swapMid_insertion(mid + 1, end);
}

template<typename T>
static void quickSort_swapRand(T *begin, T *end) {
    if (end - begin <= 1) return;
    swap(begin[0], begin[myrand(0, int(end - begin))]);
    T *cur = begin;
    for (T *p = begin + 1; p < end; ++p) {
        if (p[0] <= begin[0]) swap(p[0], *++cur);
    }
    swap(begin[0], cur[0]);
    quickSort_swapRand(begin, cur);
    quickSort_swapRand(cur + 1, end);
}

template<typename T>
static void quickSort_swapMid_insertion(T *begin, T *end) {
    if (end - begin <= INSERTION_CUTOFF) {
        insertionSort(begin, end);
        return;
    }
    swap(begin[0], begin[(end - begin) / 2]);
    T *cur = begin;
    for (T *p = begin + 1; p < end; ++p) {
        if (p[0] <= begin[0]) swap(p[0], *++cur);
    }
    swap(begin[0], cur[0]);
    quickSort_swapMid_insertion(begin, cur);
    quickSort_swapMid_insertion(cur + 1, end);
}

template<typename T>
static void quickSort_swapRand_insertion(T *begin, T *end) {
    if (end - begin <= INSERTION_CUTOFF) {
        insertionSort(begin, end);
        return;
    }
    swap(begin[0], begin[myrand(0, int(end - begin))]);
    T *cur = begin;
    for (T *p = begin + 1; p < end; ++p) {
        if (p[0] <= begin[0]) swap(p[0], *++cur);
    }
    swap(begin[0], cur[0]);
    quickSort_swapRand_insertion(begin, cur);
    quickSort_swapRand_insertion(cur + 1, end);
}

template<typename T>
static void quickSort_hack(T *begin, T *end) {
    if (end - begin <= INSERTION_CUTOFF) {
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

    quickSort_hack(begin, upper);
    quickSort_hack(upper + 1, end);
}

template<typename T>
static void quickSort_optimal(T *begin, T *end) {
    if (end - begin <= INSERTION_CUTOFF) {
        insertionSort(begin, end);
        return;
    }

    swap(begin[0], begin[(end - begin) / 2]);
    if (begin[0] > end[-1]) swap(begin[0], end[-1]);

    T *lo = begin + 1, *hi = end - 2;
    for (; lo <= hi; ) {
        while (*lo < *begin) ++lo;
        while (*hi > *begin) --hi;
        if (lo <= hi) swap(*lo++, *hi--);
    }

    swap(begin[0], lo[-1]);
    quickSort_optimal(begin, lo - 1);
    quickSort_optimal(lo, end);
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
        setupAllocator(int(end - begin));

        if (int(end - begin) > 32 && isOrdered(begin, end)) {
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
    static bool isOrdered(T *begin, T *end) {
        int step = max(1, int(end - begin) / 32);
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

        bool ordered = ((len - 1) & len) == 0;
        printf("@@@@ len = %.3fK, %s\n", len / 1024.0, ordered ? "ordered" : "");
        for (FuncItem &item : funcs) {
            if (item.limit > 0 && len > item.limit) continue;

            // warm up cache
            if (ordered) sort(v.begin(), v.end());
            else random_shuffle(v.begin(), v.end());
            item.f(&v[0], &v[0] + v.size());
            assertOrdered(&v[0], &v[0] + v.size());

            vector<double> times(8);
            for (double &time : times) {
                if (ordered) sort(v.begin(), v.end());
                else random_shuffle(v.begin(), v.end());

                time = getTime();
                item.f(&v[0], &v[0] + v.size());
                time = getTime() - time;
                assertOrdered(&v[0], &v[0] + v.size());
            }

            sort(times.begin(), times.end());
            if (len >= 256) {
                printf("\t%-40s : %.6fs\n", item.name, accumulate(times.begin(), times.begin() + 3, 0.0) / 3);
            }
        }
    }
}

int main() {
#define ITEM(f, limit) {#f, (void(*)(int*,int*))f, limit}
    vector<FuncItem> items = {
        ITEM(bubbleSort, 1024 * 16),
        ITEM(bubbleSort_orderedSpecial, 1024 * 16),
        ITEM(selectionSort, 1024 * 16),
        ITEM(insertionSort, 1024 * 16),
        ITEM(insertionSort_swap, 1024 * 16),
        ITEM(insertionSort_guard, 1024 * 16),
        ITEM(shellSort_2n, 0),
        ITEM(shellSort_3n, 0),
        ITEM(heapSort, 0),
        ITEM(heapSort1, 0),
        ITEM(heapSort_optimal, 0),
        ITEM(mergeSort_iteration, 0),
        ITEM(mergeSort_move, 0),
        ITEM(mergeSort_moveAndInplace, 0),
        ITEM(mergeSort_moveAndInplace_insertion, 0),
        ITEM(quickSort, 0),
        ITEM(quickSort_3way, 0),
        ITEM(quickSort_guard, 0),
        ITEM(quickSort_guard_swapMid, 0),
        ITEM(quickSort_guard_swapMid_insertion, 0),
        ITEM(quickSort_insertion, 0),
        ITEM(quickSort_segmentThenPostInsertion, 0),
        ITEM(quickSort_swapMid, 0),
        ITEM(quickSort_swapMid_insertion, 0),
        ITEM(quickSort_swapRand, 0),
        ITEM(quickSort_swapRand_insertion, 0),
        ITEM(quickSort_optimal, 0),
        ITEM(quickSort_hack, 0),
        ITEM(ANSICqsort, 0),
        ITEM(sort, 0),
        ITEM(stable_sort, 0),
        ITEM(bstSort, 0),
        ITEM(stdSetSort, 0),
    };
#undef ITEM

    srand((int)time(nullptr));
    timeSortFuncs(items);
}
