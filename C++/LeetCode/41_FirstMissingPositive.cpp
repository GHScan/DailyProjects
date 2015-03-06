#include "stdafx.h"

//------------------------------
#include <algorithm>

class Solution {
public:
    int firstMissingPositive(int A[], int n) {
        for (int i = 0; i < n; ++i) {
            for (;;) {
                int v = A[i];
                if (v <= 0 || v > n || A[v - 1] == v) break;
                swap(A[i], A[v - 1]);
            }
        }
        for (int i = 1; i <= n; ++i) {
            if (A[i - 1] != i) return i;
        }
        return n + 1;
    }
};

//------------------------------
#define countOf(a) int(sizeof(a) / sizeof(a[0]))

int main() {
    Solution s;
    {
        int a[] = {0};
        cout << s.firstMissingPositive(a, 0) << endl;
        cout << s.firstMissingPositive(a, 1) << endl;

        a[0] = 1;
        cout << s.firstMissingPositive(a, 0) << endl;
        cout << s.firstMissingPositive(a, 1) << endl;
    }
    {
        int a[] = {-3, 0, -1, -2};
        cout << s.firstMissingPositive(a, countOf(a)) << endl;
    }
    {
        int a[] = { -3, 0, -1, -2, 1 };
        cout << s.firstMissingPositive(a, countOf(a)) << endl;
    }
    {
        int a[] = { 0, 1, 3, };
        cout << s.firstMissingPositive(a, countOf(a)) << endl;
    }
    {
        int a[] = { 1, 3,  };
        cout << s.firstMissingPositive(a, countOf(a)) << endl;
    }
    {
        int a[] = { 2, 3, 4 };
        cout << s.firstMissingPositive(a, countOf(a)) << endl;
    }
    {
        int a[] = { 2, 4 };
        cout << s.firstMissingPositive(a, countOf(a)) << endl;
    }
    {
        int a[] = { 1, 2, 0 };
        cout << s.firstMissingPositive(a, countOf(a)) << endl;
    }
    {
        int a[] = { 3, 4, -1, 1 };
        cout << s.firstMissingPositive(a, countOf(a)) << endl;
    }
    {
        int a[] = { 1, 1 };
        cout << s.firstMissingPositive(a, countOf(a)) << endl;
    }
    {
        int a[] = { 1, 2 };
        cout << s.firstMissingPositive(a, countOf(a)) << endl;
    }
    {
        int a[] = { 3, 4, -1, 1, };
        cout << s.firstMissingPositive(a, countOf(a)) << endl;
    }
    
}
