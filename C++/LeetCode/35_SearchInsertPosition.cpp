#include "stdafx.h"

//------------------------------
class Solution {
public:
    int searchInsert(int A[], int n, int target) {
        if (n == 0) return 0;
        else if (A[0] >= target) return 0;
        else if (A[n - 1] < target) return n;

        int first = 1, last = n - 1;
        while (first < last) {
            int mid = (first + last) / 2;
            if (A[mid] < target) first = mid + 1;
            else last = mid;
        }
        return last;
    }
};

//------------------------------
#define countOf(a) int(sizeof(a) / sizeof(a[0]))
#include <algorithm>

int main() {
    Solution s;
    {
        int a[] = {1};
        for (int i = 0; i < 3; ++i) {
            assert(s.searchInsert(a, 0, i) + a == lower_bound(a, a + 0, i));
        }
    }
    {
        int a[] = { 1 };
        for (int i = 0; i < 3; ++i) {
            assert(s.searchInsert(a, countOf(a), i) + a == lower_bound(a, a + countOf(a), i));
        }
    }
    {
        int a[] = { 1, 3, 5, };
        for (int i = 0; i < 7; ++i) {
            assert(s.searchInsert(a, countOf(a), i) + a == lower_bound(a, a + countOf(a), i));
        }
    }
}
