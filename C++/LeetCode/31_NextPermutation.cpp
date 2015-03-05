#include "stdafx.h"

//------------------------------
class Solution {
public:
    void nextPermutation(vector<int> &num) {
        if (num.size() < 2) return;

        int *begin = &num[0];
        int *end = begin + num.size();
        int *it = end - 2;
        for (; it >= begin && it[0] >= it[1]; --it);

        if (it < begin) {
            reverse(begin, end);
            return;
        }

        int *pos = it + 1;
        for (; pos + 1 < end && pos[1] > it[0]; ++pos);
        swap(*it, *pos);
        reverse(it + 1, end);
    }
private:
    void reverse(int *begin, int *end) {
        for (; begin < end; ++begin, --end) {
            swap(begin[0], end[-1]);
        }
    }
};


//------------------------------
static void printResult(vector<int> const &result) {
    for (auto i : result) cout << i << ',';
    cout << endl;
}

int main() {
    Solution s;
    {
        vector<int> v{ 1, 2, 3 };
        for (int i = 0; i < 6; ++i) {
            s.nextPermutation(v);
            printResult(v);
        }
    }
    {
        vector<int> v{ 1, 2, 3 };
        s.nextPermutation(v);
        printResult(v);

        v = vector<int>{3, 2, 1};
        s.nextPermutation(v);
        printResult(v);

        v = vector<int>{1, 1, 5};
        s.nextPermutation(v);
        printResult(v);

        v = vector<int>{1, };
        s.nextPermutation(v);
        printResult(v);

        v = vector<int>{1, 1};
        s.nextPermutation(v);
        printResult(v);

        v = vector<int>{};
        s.nextPermutation(v);
        printResult(v);
    }
}
