#include "stdafx.h"

//------------------------------
class Solution {
public:
    int search(int A[], int n, int target) {
        if (n < 2) {
            return n == 1 && A[0] == target ? 0 : -1;
        } else if (A[0] < A[n - 1]) {
            return binarySearch(A, 0, n, target);
        }

        int pivot = findPivot(A, n);
        return target >= A[0] ? binarySearch(A, 0, pivot, target) : binarySearch(A, pivot, n, target);
    }
private:
    int findPivot(int A[], int n) {
        assert(n > 1 && A[0] > A[n - 1]);
        int first = 1, last = n - 1;
        while (first < last) {
            int mid = (first + last) / 2;
            if (A[mid] > A[0]) first = mid + 1;
            else last = mid;
        }
        return first;
    }
    int binarySearch(int A[], int begin, int end, int target) {
        while (begin < end) {
            int mid = (begin + end) / 2;
            if (target == A[mid]) return mid;
            else if (target < A[mid]) end = mid;
            else begin = mid + 1;
        }
        return -1;
    }
};

//------------------------------
#define countOf(a) int(sizeof(a) / sizeof(a[0]))
#include <algorithm>

int main() {
    Solution s;
    {
        int a[] = { 1 };
        assert(s.search(a, 1, 0) == -1);
        assert(s.search(a, 1, 1) == 0);
        assert(s.search(a, 1, 2) == -1);
        assert(s.search(a, 0, 0) == -1);
        assert(s.search(a, 0, 1) == -1);
        assert(s.search(a, 0, 2) == -1);
    }
    {
        int a[] = {1, 3, };
        assert(s.search(a, countOf(a), 1) == 0);
    }
    {
        int a[] = { 1, 3, 5, 7 };
        for (int time = 0; time < countOf(a); ++time) {
            rotate(a, a + 1, a + countOf(a));
            for (int i = 0; i < 8; ++i) {
                int idx = s.search(a, countOf(a), i);
                assert(idx == -1 || a[idx] == i);
            }
        }
    }
}
