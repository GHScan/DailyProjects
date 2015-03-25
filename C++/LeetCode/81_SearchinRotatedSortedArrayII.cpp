#include "stdafx.h"

#include <assert.h>

//-------------------------------------------------------------------------
class Solution {
public:
    bool search(int A[], int n, int target) {
        if (A == nullptr || n <= 0) return false;
        if (A[0] == A[n - 1]) {
            for (int i = 0; i < n; ++i) {
                if (A[i] == target) return true;
            }
            return false;
        } else if (A[0] < A[n - 1]) {
            return binarySearch(A, n, target);
        } else {
            int pivot = findPivot(A, n, target);
            return binarySearch(A, pivot, target) || binarySearch(A + pivot, n - pivot, target);
        }
    }
private:
    int findPivot(int A[], int n, int target) {
        assert(A[0] > A[n - 1]);
        int first = 0, last = n - 1;
        while (first + 1 < last) {
            int mid = first + (last - first) / 2;
            if (A[mid] >= A[0]) first = mid;
            else last = mid;
        }
        return last;
    }
    bool binarySearch(int A[], int n, int target) {
        int begin = 0, end = n;
        while (begin < end) {
            int mid = begin + (end - begin) / 2;
            if (target == A[mid]) return true;
            else if (target < A[mid]) end = mid;
            else begin = mid + 1;
        }
        return false;
    }
};
//-------------------------------------------------------------------------
#define countOf(a) (sizeof(a) / sizeof(a[0]))

int main() {
    Solution so;

    cout << "#######################" << endl;
    cout << so.search(nullptr, 0, 1) << endl;
    {
        int a[] = {0};
        cout << "#######################" << endl;
        for (int i = -1; i < 2; ++i) {
            cout << so.search(a, countOf(a), i) << endl;
        }
    }
    {
        int a[] = { 0, 1, 2 };
        cout << "#######################" << endl;
        for (int i = -1; i < 4; ++i) {
            cout << so.search(a, countOf(a), i) << endl;
        }
    }
    {
        int a[] = { 0, 0, 1, 2 };
        cout << "#######################" << endl;
        for (int i = -1; i < 4; ++i) {
            cout << so.search(a, countOf(a), i) << endl;
        }
    }
    {
        int a[] = { 0, 1, 2, 2 };
        cout << "#######################" << endl;
        for (int i = -1; i < 4; ++i) {
            cout << so.search(a, countOf(a), i) << endl;
        }
    }
    {
        int a[] = { 2, 2, 0, 1 };
        cout << "#######################" << endl;
        for (int i = -1; i < 4; ++i) {
            cout << so.search(a, countOf(a), i) << endl;
        }
    }
    {
        int a[] = { 2, 0, 1, 2 };
        cout << "#######################" << endl;
        for (int i = -1; i < 4; ++i) {
            cout << so.search(a, countOf(a), i) << endl;
        }
    }
    {
        int a[] = { 0, 0, 0, 0};
        cout << "#######################" << endl;
        for (int i = -1; i < 2; ++i) {
            cout << so.search(a, countOf(a), i) << endl;
        }
    }
}
