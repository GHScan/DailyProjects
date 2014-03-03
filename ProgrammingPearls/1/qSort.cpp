#include <assert.h>
#include <stdio.h>

#include <algorithm>
#include <vector>
#include <iostream>
using namespace std;

#include "Timer.h"

template<typename T>
T* partition(T *begin, T *end) {
    assert(begin < end);
    T *it = begin;
    T *mid = ++begin;
    for (; begin != end; ++begin) {
        if (*begin <= *it) swap(*begin, *mid++);
    }
    --mid;
    swap(*it, *mid);
    return mid;
}
template<typename T>
static void qSort(T *begin, T *end) {
    assert(begin <= end);
    int len = end - begin;
    switch (len) {
        case 0: case 1: return;
        case 2: {
                    if (!(begin[0] <= begin[1])) swap(begin[0], begin[1]);
                    return;
                }
        default: break;
    }

    T *mid = partition(begin, end);
    qSort(begin, mid);
    qSort(mid + 1, end);
}

template<typename T>
T* partition2(T *begin, T *end) {
    assert(begin < end);
    T *it = begin++;
    while (begin < end) {
        while (begin < end && *begin <= *it) ++begin;
        while (begin < end && end[-1] > *it) --end;
        if (begin < end) swap(*begin, end[-1]);
    }
    --begin;
    swap(*it, *begin);
    return begin;
}
template<typename T>
static void qSort2(T *begin, T *end) {
    assert(begin <= end);
    int len = end - begin;
    switch (len) {
        case 0: case 1: return;
        case 2: {
                    if (!(begin[0] <= begin[1])) swap(begin[0], begin[1]);
                    return;
                }
        default: break;
    }

    T *mid = partition2(begin, end);
    qSort2(begin, mid);
    qSort2(mid + 1, end);
}

int main() {
    vector<int> nums;

    for (int n; cin >> n;) nums.push_back(n);

    {
        Timer _t(stderr, "qSort");
        qSort(&nums[0], &nums[0] + nums.size());
        // qSort2(&nums[0], &nums[0] + nums.size());
    }

    for (auto i : nums) cout << i << '\n';
}
