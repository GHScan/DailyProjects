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

int main() {
    srand(time(nullptr));

    correctnessTest();
}
