#include "stdafx.h"

//------------------------------
class Solution {
public:
    void sortColors(int A[], int n) {
        int last0 = -1, first2 = n;
        for (int i = last0 + 1; i < first2;) {
            if (A[i] == 0) swap(A[++last0], A[i]), ++i;
            else if (A[i] == 2) swap(A[--first2], A[i]);
            else ++i;
        }
    }
};
//------------------------------
#define countOf(a) (sizeof(a) / sizeof(a[0]))

static void printResult(int A[], int n) {
    for (int i = 0; i < n; ++i) cout << A[i] << ',';
    cout << endl;
}

int main() {
    Solution s;
    { 
        int A[] = {0};
        s.sortColors(A, 0);
        printResult(A, 0);
    }
    {
        int A[] = { 0 };
        s.sortColors(A, countOf(A));
        printResult(A, countOf(A));
    }
    {
        int A[] = { 0, 1 };
        s.sortColors(A, countOf(A));
        printResult(A, countOf(A));
    }
    {
        int A[] = { 0, 2 };
        s.sortColors(A, countOf(A));
        printResult(A, countOf(A));
    }
    {
        int A[] = { 1, 2 };
        s.sortColors(A, countOf(A));
        printResult(A, countOf(A));
    }
    {
        int A[] = { 0, 1, 2 };
        s.sortColors(A, countOf(A));
        printResult(A, countOf(A));
    }
    {
        int A[] = { 0, 1, 2 };
        s.sortColors(A, countOf(A));
        printResult(A, countOf(A));
    }
    {
        int A[] = { 0, 2, 2 };
        s.sortColors(A, countOf(A));
        printResult(A, countOf(A));
    }
    {
        int A[] = { 2, 0, 2 };
        s.sortColors(A, countOf(A));
        printResult(A, countOf(A));
    }
    {
        int A[] = { 2, 2, 0 };
        s.sortColors(A, countOf(A));
        printResult(A, countOf(A));
    }
    {
        int A[] = { 2, 1, 2, 0 };
        s.sortColors(A, countOf(A));
        printResult(A, countOf(A));
    }
    {
        int A[] = { 2, 1, 2, 0, 1 };
        s.sortColors(A, countOf(A));
        printResult(A, countOf(A));
    }
    {
        int A[] = { 2, 1, 2, 0, 1, 0, 2 };
        s.sortColors(A, countOf(A));
        printResult(A, countOf(A));
    }
    {
        int A[] = { 2, 0, 1, 1, 2, 1, 2, 1, 2, 0, 1, 0, 2 };
        s.sortColors(A, countOf(A));
        printResult(A, countOf(A));
    }
}
