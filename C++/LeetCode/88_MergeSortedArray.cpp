#include "stdafx.h"

#include <assert.h>

//-------------------------------------------------------------------------
class Solution {
public:
    void merge(int A[], int m, int B[], int n) {
        int dest = m + n;
        while (m > 0 && n > 0) {
            if (A[m - 1] >= B[n - 1]) A[--dest] = A[--m];
            else A[--dest] = B[--n];
        }
        while (m > 0) A[--dest] = A[--m];
        while (n > 0) A[--dest] = B[--n];
    }
};
//-------------------------------------------------------------------------
#define countOf(a) (sizeof(a) / sizeof(a[0]))

static void printResult(int A[], int n) {
    for (int i = 0; i < n; ++i) cout << A[i] << ',';
    cout << endl;
}

int main() {
    Solution so;
    {
        int a[7] = {3, 5, 8}, b[] = {1, 2, 3, 3};
        so.merge(a, 3, b, 4);
        printResult(a, 7);
    }
    {
        int a[7] = { 3, 5, 8 }, b[] = { 1, 2, 4, 9 };
        so.merge(a, 3, b, 4);
        printResult(a, 7);
    }
    {
        int a[7] = { 3, 5, 8 }, b[] = { 3, 5, 7, 9 };
        so.merge(a, 3, b, 4);
        printResult(a, 7);
    }
    {
        int a[7] = { 3, 5, 8 }, b[] = { 8, 8, 9, 10};
        so.merge(a, 3, b, 4);
        printResult(a, 7);
    }
}
