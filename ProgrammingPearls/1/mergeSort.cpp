#include <assert.h>

#include <iostream>
#include <vector>
using namespace std;

#include "Timer.h"

template<typename T>
static void binMerge(T *begin1, T *end1, T *begin2, T *end2, T *out) {
    assert(begin1 <= end1 && begin2 <= end2);
    while (begin1 != end1 && begin2 != end2) {
        if (*begin1 <= *begin2) *out++ = *begin1++;
        else *out++ = *begin2++;
    }
    while (begin1 != end1) *out++ = *begin1++;
    while (begin2 != end2) *out++ = *begin2++;
}
template<typename T>
static void mergeSort(T *begin, T *end, T *bufbegin);
template<typename T>
static void mergeSortTo(T *begin, T *end, T *obegin) {
    assert(begin <= end);
    int len = end - begin;
    T *oend = obegin + len;
    switch (len) {
        case 0: return;
        case 1: obegin[0] = begin[0]; return;
        case 2:
                if (!(begin[0] <= begin[1])) {
                    obegin[0] = begin[1]; obegin[1] = begin[0];
                } else {
                    obegin[0] = begin[0]; obegin[1] = begin[1];
                }
                return;
        default: break;
    }

    T *mid = begin + len / 2, *omid = obegin + len / 2;
    mergeSort(begin, mid, obegin);
    mergeSort(mid, end, omid);
    binMerge(begin, mid, mid, end, obegin);
}
template<typename T>
static void mergeSort(T *begin, T *end, T *bufbegin) {
    assert(begin <= end);
    int len = end - begin;
    T *bufend = bufbegin + len;
    switch (len) {
        case 0: return;
        case 1: return;
        case 2: {
                    if (!(begin[0] <= begin[1])) swap(begin[0], begin[1]);
                    return;
                }
        default: break;
    }

    T *mid = begin + len / 2, *bufmid = bufbegin + len / 2;
    mergeSortTo(begin, mid, bufbegin);
    mergeSortTo(mid, end, bufmid);
    binMerge(bufbegin, bufmid, bufmid, bufend, begin);
}

template<typename T>
static void mergeSortTo_v2(T *begin, T *end, T *obegin) {
    assert(begin <= end);
    int len = end - begin;
    T *oend = obegin + len;
    switch (len) {
        case 0: return;
        case 1: obegin[0] = begin[0]; return;
        case 2: {
                    if (begin[0] <= begin[1]) {
                        obegin[0] = begin[0]; obegin[1] = begin[1];
                    } else {
                        obegin[0] = begin[1]; obegin[1] = begin[0];
                    }
                    return;
                }
        default: break;
    }

    int halfLen = (len + 1) / 2;
    mergeSortTo_v2(begin, begin + halfLen, oend - halfLen);
    mergeSortTo_v2(begin + halfLen, end, begin);
    binMerge(begin, begin + len - halfLen, oend - halfLen, oend, obegin);
}
template<typename T>
static void mergeSort_v2(T *begin, T *end, T *bufbegin) {
    assert(begin <= end);
    int len = end - begin;
    T *bufend = bufbegin + len;
    switch (len) {
        case 0: return;
        case 1: return;
        case 2: {
                    if (!(begin[0] <= begin[1])) swap(begin[0], begin[1]);
                    return;
                }
        default: break;
    }

    T *mid = begin + len / 2, *bufmid = bufbegin + len / 2;
    mergeSortTo_v2(begin, mid, bufbegin);
    mergeSortTo_v2(mid, end, bufmid);
    binMerge(bufbegin, bufmid, bufmid, bufend, begin);
}

int main() {
    vector<int> nums;
    for (int i; cin >> i; ) nums.push_back(i);
    vector<int> buf(nums.size());

    {
        Timer _t(stderr, "mergeSort");
        mergeSort(&nums[0], &nums[0] + nums.size(), &buf[0]);
        //mergeSort_v2(&nums[0], &nums[0] + nums.size(), &buf[0]);
    }

    for (auto i : nums) cout << i << '\n';
} 
