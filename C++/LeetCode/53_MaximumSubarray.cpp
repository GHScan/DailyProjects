#include "stdafx.h"

//------------------------------
#include <algorithm>
#include <limits>

class Solution {
public:
    int maxSubArray(int A[], int n) {
        int sum = 0, maxSum = numeric_limits<int>::min();
        for (int i = 0; i < n; ++i) {
            sum = sum > 0 ? sum + A[i] : A[i];
            maxSum = max(maxSum, sum);
        }
        return maxSum;
    }
};

//------------------------------
#define countOf(a) int(sizeof(a) / sizeof(a[0]))

int main() {
    Solution s;
    {
        int a[] = { -3, };
        cout << s.maxSubArray(a, 0) << endl;
        cout << s.maxSubArray(a, countOf(a)) << endl;
    }
    {
        int a[] = { -3, -2, };
        cout << s.maxSubArray(a, countOf(a)) << endl;
    }
    {
        int a[] = { -3, 4, -2, };
        cout << s.maxSubArray(a, countOf(a)) << endl;
    }
    {
        int a[] = { -3, 4, -2, 3, };
        cout << s.maxSubArray(a, countOf(a)) << endl;
    }
    {
        int a[] = { -2, 1, -3, 4, -1, 2, 1, -5, 4, };
        cout << s.maxSubArray(a, countOf(a)) << endl;
    }
}
