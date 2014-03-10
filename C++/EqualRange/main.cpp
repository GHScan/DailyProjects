#include "pch.h"

#include <assert.h>

#include <functional>

template<typename IterT, typename T>
IterT lowerBound(IterT begin, IterT end, const T& val) {
    IterT last = end - 1;
    if (val <= *begin) return begin;
    if (val > *last) return end;

    while (last - begin > 1) {
        IterT mid = begin + (last - begin) / 2;
        if (val > *mid) begin = mid;
        else last = mid;
    }

    return last;
}
template<typename IterT, typename T>
IterT upperBound(IterT begin, IterT end, const T& val) {
    IterT last = end - 1;
    if (val < *begin) return begin;
    if (val >= *last) return end;

    while (last - begin > 1) {
        IterT mid = begin + (last - begin) / 2;
        if (val >= *mid) begin = mid;
        else last = mid;
    }

    return last;
}

#define COUNT(a) (sizeof(a)/sizeof((a)[0]))

int main() {
    int a1[] = {3, };
    int a2[] = {3, 4, 9, 9, 9, 9, };
    int a3[] = {2, 2, 5, 7, 8, 10, 10, };
    int a4[] = {1, 4, 5, 5, 5, 6, 9, 10};

    for (int i = -2; i < 12; ++i) {
        assert(lowerBound(a1, a1 + COUNT(a1), i) == lower_bound(a1, a1 + COUNT(a1), i));
        assert(lowerBound(a2, a2 + COUNT(a2), i) == lower_bound(a2, a2 + COUNT(a2), i));
        assert(lowerBound(a3, a3 + COUNT(a3), i) == lower_bound(a3, a3 + COUNT(a3), i));
        assert(lowerBound(a4, a4 + COUNT(a4), i) == lower_bound(a4, a4 + COUNT(a4), i));

        assert(upperBound(a1, a1 + COUNT(a1), i) == upper_bound(a1, a1 + COUNT(a1), i));
        assert(upperBound(a2, a2 + COUNT(a2), i) == upper_bound(a2, a2 + COUNT(a2), i));
        assert(upperBound(a3, a3 + COUNT(a3), i) == upper_bound(a3, a3 + COUNT(a3), i));
        assert(upperBound(a4, a4 + COUNT(a4), i) == upper_bound(a4, a4 + COUNT(a4), i));
    }
}
