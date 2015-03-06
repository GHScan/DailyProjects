#include "stdafx.h"

//------------------------------
#include <algorithm>

class Solution {
public:
    int trap(int A[], int n) {
        vector<int> leftHighest(n, 0);
        {
            int highest = -1;
            for (int i = 0; i < n; ++i) {
                leftHighest[i] = highest;
                highest = max(highest, A[i]);
            }
        }

        int water = 0;

        int rightHighest = -1;
        for (int i = n - 1; i >= 0; --i) {
            water += max(0, min(rightHighest, leftHighest[i]) - A[i]);
            rightHighest = max(rightHighest, A[i]);
        }

        return water;
    }
};

//------------------------------
#define countOf(a) int(sizeof(a) / sizeof(a[0]))

int main() {
    Solution s;
    {
        int a[] = {0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1};
        cout << s.trap(a, countOf(a)) << endl;
    }
    {
        int a[] = { 0, };
        cout << s.trap(a, countOf(a)) << endl;
    }
    {
        int a[] = { 0, 0, 3, 0, 0};
        cout << s.trap(a, countOf(a)) << endl;
    }
    {
        int a[] = { 0, 0, 3, 0, 1 };
        cout << s.trap(a, countOf(a)) << endl;
    }
}
