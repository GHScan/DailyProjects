#include "stdafx.h"

//------------------------------
class Solution {
public:
    bool canJump(int A[], int n) {
        if (n == 0) return false;

        int dis = 1;
        for (int i = n - 2; i >= 0; --i, ++dis) {
            dis = A[i] >= dis ? 0 : dis;
        }
        return dis == 1;
    }
};

//------------------------------
#define countOf(a) int(sizeof(a) / sizeof(a[0]))

int main() {
    Solution s;
    {
        int a[] = { 2, 3, 1, 1, 4 };
        cout << s.canJump(a, 0) << endl;
        cout << s.canJump(a, countOf(a)) << endl;
    }
    {
        int a[] = { 2, };
        cout << s.canJump(a, countOf(a)) << endl;
    }
    {
        int a[] = { 2, 0, 0, 0, 4 };
        cout << s.canJump(a, countOf(a)) << endl;
    }
    {
        int a[] = { 5, 0, 0, 0, 4 };
        cout << s.canJump(a, countOf(a)) << endl;
    }
    {
        int a[] = { 1, 1, 1, 1, 4 };
        cout << s.canJump(a, countOf(a)) << endl;
    }
    {
        int a[] = { 1, 1, 1, 0, 4 };
        cout << s.canJump(a, countOf(a)) << endl;
    }
    {
        int a[] = { 1, 1, 2, 0, 4 };
        cout << s.canJump(a, countOf(a)) << endl;
    }
    {
        vector<int> a(25000, 0);
        for (int i = 0; i < (int)a.size(); ++i) {
            a[i] = 25000 - i;
        }
        a.push_back(1);
        a.push_back(0);
        cout << s.canJump(&a[0], (int)a.size()) << endl;
    }

}
