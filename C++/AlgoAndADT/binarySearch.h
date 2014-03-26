#include "pch.h"

#include "Utils.h"

template<typename IterT, typename T>
static IterT lowerBound(IterT begin, IterT end, const T &val) {
    IterT last = end - 1;
    if (val <= *begin) return begin;
    if (val > *last) return end;

    while (last - begin > 1) {
        IterT mid = begin + (last - begin) / 2;
        if (*mid < val) begin = mid;
        else last = mid;
    }

    return last;
}
template<typename IterT, typename T>
static IterT upperBound(IterT begin, IterT end, const T &val) {
    IterT last = end - 1;
    if (val < *begin) return begin;
    if (val >= *last) return end;

    while (last - begin > 1) {
        IterT mid = begin + (last - begin) / 2;
        if (*mid <= val) begin = mid;
        else last = mid;
    }

    return last;
}
template<typename IterT, typename T>
static bool binarySearch(IterT begin, IterT end, const T &val) {
    for (; begin < end; ) {
        IterT mid = begin + (end - begin) / 2;
        if (*mid == val) return true;
        else if (*mid < val) begin = mid + 1;
        else end = mid;
    }
    return false;
}

template<typename IterT, typename T>
static IterT lowerBound2(IterT begin, IterT end, const T &val) {
    assert(begin < end);

    if (begin[0] >= val) return begin;
    if (val > end[-1]) return end;
    assert(begin[0] < val && val <= end[-1]);

    int range = (end - begin) - 1;
    range |= range >> 1;
    range |= range >> 2;
    range |= range >> 4;
    range |= range >> 8;
    range |= range >> 16;
    range = (range + 1) / 2;
    assert(range >= 0 && range < (end - begin));

    if (val > begin[range]) {
        begin = end - 1 - range;
    }
    assert(begin[0] < val && val <= begin[range]);

#define CASE(n) case (1 << n): if (val > begin[1 << (n - 1)]) begin += 1 << (n - 1);
    switch (range) {
        CASE(30); CASE(29); CASE(28); CASE(27); CASE(26); CASE(25); CASE(24); CASE(23); CASE(22); CASE(21);
        CASE(20); CASE(19); CASE(18); CASE(17); CASE(16); CASE(15); CASE(14); CASE(13); CASE(12); CASE(11);
        CASE(10); CASE(9); CASE(8); CASE(7); CASE(6); CASE(5); CASE(4); CASE(3); CASE(2); CASE(1);
        case (1 << 0): break;
        default: assert(0); break;
    }
#undef CASE

    return begin + 1;
}

template<typename IterT>
static void assertNthElement(IterT begin, IterT mid, IterT end) {
    for (IterT p = begin; p != mid; ++p) {
        if (*p <= *mid);
        else fprintf(stderr, "%s(%d): assert failed", __FILE__, __LINE__), exit(1);
    }
    for (IterT p = mid + 1; p != end; ++p) {
        if (*p >= *mid);
        else fprintf(stderr, "%s(%d): assert failed", __FILE__, __LINE__), exit(1);
    }
}

template<typename IterT>
static void nthElement(IterT begin, IterT mid, IterT end) {
    for (IterT begin2 = begin, end2 = end; end2 - begin2 > 1; ) {
        IterT cur = begin2;
        for (IterT p = begin2 + 1; p != end2; ++p) {
            if (*p <= *begin2) swap(*p, *++cur);
        }
        swap(*begin2, *cur);
        if (mid == cur) break;
        else if (mid < cur) {
            end2 = cur;
        } else {
            begin2 = cur + 1;
        }
    }
}

template<typename IterT>
static void nthElement2(IterT begin, IterT mid, IterT end) {
    IterT cur = begin;
    for (IterT p = begin + 1; p != end; ++p) {
        if (*p <= *begin) swap(*p, *++cur);
    }
    swap(*begin, *cur);
    if (mid == cur) return;
    else if (mid < cur) nthElement2(begin, mid, cur);
    else nthElement2(cur + 1, mid, end);
}

static void correctnessTest() {
    int lens[] = {
        1, 3, 7, 11, 32, 66, 109, 254, 510, 512, 1024, 1025, 2049,
    };
    for (int len : lens) {
        vector<int> data(len);
        vector<int> data1;

        for (int i = 0; i < 32; ++i) {
            for (int &d : data) d = myrand(0, len);
            sort(data.begin(), data.end());

            for (int i = 0; i < 1024; ++i) {
                int k = myrand(0, len);
                assert(lower_bound(data.begin(), data.end(), k) == lowerBound(data.begin(), data.end(), k));
                assert(lower_bound(data.begin(), data.end(), k) == lowerBound2(data.begin(), data.end(), k));
                assert(upper_bound(data.begin(), data.end(), k) == upperBound(data.begin(), data.end(), k));
                assert(binary_search(data.begin(), data.end(), k) == binarySearch(data.begin(), data.end(), k));
            }
        }

        for (int i = 0; i < 1024; ++i) {
            random_shuffle(data.begin(), data.end());
            int k = myrand(0, len);

            data1.assign(data.begin(), data.end());
            nthElement(data1.begin(), data1.begin() + k, data1.end());
            assertNthElement(data1.begin(), data1.begin() + k, data1.end());

            data1.assign(data.begin(), data.end());
            nthElement2(data1.begin(), data1.begin() + k, data1.end());
            assertNthElement(data1.begin(), data1.begin() + k, data1.end());
        }
    }
}

struct FuncItem {
    const char *name;
    int*(*f)(int*, int*, int&);
};

static void benchmark_lowerBound(const vector<FuncItem> &funcs) {
    int lens[] = {
        1, 17, 31, 18, 19, 33, 62, 192, 2000, 4097, 8000, 16100,
    };

    for (int len : lens) {
        printf("len=%d\n", len);

        vector<int> array;
        for (int i = 0; i < len; ++i) array.push_back(i * 2 + 10);
        int *begin = &array[0], *end = &array[0] + array.size();

        vector<int> query;
        for (int i = 0; i < len * 2; ++i) query.push_back(myrand(0, len * 2 + 20));

        for (auto &func : funcs) {

            vector<double> times(8);
            for (double &time : times) {

                time = getTime();
                for (int n = 0; n < 512; ++n) {
                    for (int q : query) func.f(begin, end, q);
                }
                time = getTime() - time;
            }

            sort(times.begin(), times.end());
            printf("\t%s : %f\n", func.name, accumulate(times.begin(), times.begin() + 3, 0.0) / 3);
        }
    }
}

int main() {
    srand(time(nullptr));
    setCpuAffinity(1);

    correctnessTest();

    {
#define ITEM(f) {#f, (int*(*)(int*, int*, int&))f, }
        vector<FuncItem> funcs = {
            ITEM((std::lower_bound<int*, int&>)),
            ITEM((lowerBound<int*, int&>)),
            ITEM((lowerBound2<int*, int&>)),
        };
#undef ITEM
        benchmark_lowerBound(funcs);
    }

    puts("finish");
}
