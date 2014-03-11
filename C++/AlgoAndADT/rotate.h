#include "pch.h"

#include "Utils.h"

template<typename T>
static void myReverse(T *begin, T *end) {
    for (T *last = end - 1; begin < last; ) {
        swap(*begin++, *last--);
    }
}
template<typename T>
static void myRotate1(T *begin, T *mid, T *end) {
    myReverse(begin, mid);
    myReverse(mid, end);
    myReverse(begin, end);
}

template<typename T>
void swapRange(T *begin, T *end, T *begin2) {
    for (; begin != end; ) {
        swap(*begin++, *begin2++);
    }
}
template<typename T>
static void myRotate2(T *begin, T *mid, T *end) {
    int count1 = mid - begin, count2 = end - mid;
    if (count1 == 0 || count2 == 0) return;
    if (count1 <= count2) {
        T *tmid = end - count1;
        swapRange(begin, mid, tmid);
        myRotate2(begin, mid, tmid);
    } else {
        T *tmid = begin + count2;
        swapRange(begin, tmid, mid);
        myRotate2(tmid, mid, end);
    }
}

int gcd(int a, int b) {
    if (a > b) swap(a, b);
    while (a > 0) {
        int t = b % a;
        b = a;
        a = t;
    }
    return b;
}
template<typename T>
static void myRotate3(T *begin, T *mid, T *end) {
    int count = end - begin;
    int step = mid - begin;
    if (step == 0 || count == 0) return;
    int _gcd = gcd(count, step);
    for (int start = 0; start < _gcd; ++start) {
        int idx = start;
        int t = begin[idx];
        for (;;) {
            int nidx = (idx + step) % count;
            if (nidx == start) break;
            begin[idx] = begin[nidx];
            idx = nidx;
        }
        begin[idx] = t;
    }
}

struct FuncItem {
    const char *name;
    void (*f)(int*, int*, int*);
};

static void correctnessTest(vector<FuncItem> &items) {
    int lens[] = {
        1, 3, 7, 9, 14, 23, 88, 100, 255, 256, 1099,
    };
    for (FuncItem &item : items) {
        printf("%s: correctness test...\n", item.name);

        for (int len : lens) {
            vector<int> data(len);
            for (int &i : data) i = myrand(0, len);
            vector<int> dupdata;

            for (int i = 0; i < 32; ++i) {
                dupdata.assign(data.begin(), data.end());
                int k = myrand(0, len);
                item.f(&data[0], &data[0] + k, &data[0] + data.size());
                rotate(dupdata.begin(), dupdata.begin() + k, dupdata.end());
                assert(equal(data.begin(), data.end(), dupdata.begin()));
            }
        }
    }
}

static void performanceTest(vector<FuncItem> &items) {
    vector<int> data(16 * 1024);
    vector<int> klist(4 * 1024);
    for (int &k : klist) k = myrand(0, klist.size());

    for (FuncItem &item : items) {
        {
            Timer t(item.name);
            for (int k : klist) {
                item.f(&data[0], &data[0] + k, &data[0] + data.size());
            }
        }
    }
}

int main() {
    srand(time(nullptr));

#define ITEM(f) {#f, (void(*)(int*, int*, int*))f}
    vector<FuncItem> items = {
        ITEM(myRotate1),
        ITEM(myRotate2),
        ITEM(myRotate3),
    };
#undef ITEM

    correctnessTest(items);
    performanceTest(items);
}
