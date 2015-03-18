#include "stdafx.h"

//------------------------------
class Solution {
public:
    int removeDuplicates(int A[], int n) {
        int len = 0;
        for (int i = 0; i < n; ) {
            int v = A[i];
            A[len++] = v;
            if (i + 1 < n && A[i + 1] == v) {
                A[len++] = v;
                for (i += 2; i < n && A[i] == v; ++i);
            } else {
                ++i;
            }
        }
        return len;
    }
};
//------------------------------
#define countOf(a) (sizeof(a) / sizeof(a[0]))

int main() {
    Solution s;
    {
        int A[] = {1};
        cout << s.removeDuplicates(A, 0) << endl;
        cout << s.removeDuplicates(A, countOf(A)) << endl;
    }
    {
        int A[] = { 1, 1 };
        cout << s.removeDuplicates(A, countOf(A)) << endl;
    }
    {
        int A[] = { 1, 1, 1 };
        cout << s.removeDuplicates(A, countOf(A)) << endl;
    }
    {
        int A[] = { 1, 1, 1, 1 };
        cout << s.removeDuplicates(A, countOf(A)) << endl;
    }
    {
        int A[] = { 1, 1, 1, 1, 2 };
        cout << s.removeDuplicates(A, countOf(A)) << endl;
    }
    {
        int A[] = { 1, 1, 1, 1, 2, 2 };
        cout << s.removeDuplicates(A, countOf(A)) << endl;
    }
    {
        int A[] = { 1, 1, 1, 1, 2, 2, 2, 3, 4, 4, 5, 6, 7, 7, 8 };
        cout << s.removeDuplicates(A, countOf(A)) << endl;
    }
}
