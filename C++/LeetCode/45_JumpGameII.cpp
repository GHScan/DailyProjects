#include "stdafx.h"

//------------------------------
#include <limits>

class Solution {
public:
    int jump(int A[], int n) {
        if (n == 0) return numeric_limits<int>::max();

        vector<int> points(1, n - 1);
        for (int i = n - 2; i >= 0; --i) {
            if (i + A[i] < points.back()) continue;
            while (points.size() >= 2 && points[points.size() - 2] <= i + A[i]) {
                points.pop_back();
            }
            points.push_back(i);
        }
        
        return points.back() == 0 ? (int)points.size() - 1 : numeric_limits<int>::max();
    }
};

//------------------------------
#define countOf(a) int(sizeof(a) / sizeof(a[0]))

int main() {
    Solution s;
    {
        int a[] = { 2, 3, 1, 1, 4 };
        cout << s.jump(a, 0) << endl;
        cout << s.jump(a, countOf(a)) << endl;
    }
    {
        int a[] = { 2, };
        cout << s.jump(a, countOf(a)) << endl;
    }
    {
        int a[] = { 2, 0, 0, 0, 4 };
        cout << s.jump(a, countOf(a)) << endl;
    }
    {
        int a[] = { 5, 0, 0, 0, 4 };
        cout << s.jump(a, countOf(a)) << endl;
    }
    {
        int a[] = { 1, 1, 1, 1, 4 };
        cout << s.jump(a, countOf(a)) << endl;
    }
    {
        int a[] = { 1, 1, 1, 0, 4 };
        cout << s.jump(a, countOf(a)) << endl;
    }
    {
        int a[] = { 1, 1, 2, 0, 4 };
        cout << s.jump(a, countOf(a)) << endl;
    }
    {
        vector<int> a(25000, 0);
        for (int i = 0; i < (int)a.size(); ++i) {
            a[i] = 25000 - i;
        }
        a.push_back(1);
        a.push_back(0);
        cout << s.jump(&a[0], (int)a.size()) << endl;
    }

}
