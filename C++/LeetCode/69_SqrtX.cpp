#include "stdafx.h"

//------------------------------
class Solution {
public:
    int sqrt(int x) {
        if (x <= 0) return 0;

        int first = 0, last = 46341;
        while (first + 1 < last) {
            int mid = first + (last - first) / 2;
            if (mid * mid <= x) first = mid;
            else last = mid;
        }
        return first;
    }
};
//------------------------------

int main() {
    Solution s;
    int ints[] = {
        -4, -2, -1, 0, 1, 2, 4, 8, 25, 31, 32, 49, 64, 65, 80, 81, 100, 120, 121, 2147395599, 2147483646, 2147483647,
    };
    for (auto i : ints) cout << i << "=>" << s.sqrt(i) << endl;
}
