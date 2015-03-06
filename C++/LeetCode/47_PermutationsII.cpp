#include "stdafx.h"

//------------------------------
#include <algorithm>

class Solution {
public:
    vector<vector<int>> permuteUnique(vector<int> &num) {
        vector<vector<int>> result;

        sort(num.begin(), num.end());
        do {
            result.push_back(num);
        } while(nextPermutation(num));

        return result;
    }
private:
    bool nextPermutation(vector<int> &num) {
        if (num.size() < 2) return false;

        int *begin = &num[0];
        int *end = begin + num.size();
        int *it = end - 2;
        for (; it >= begin && it[0] >= it[1]; --it);

        if (it < begin) return false;

        int *pos = it + 1;
        for (; pos + 1 < end && pos[1] > it[0]; ++pos);
        swap(*it, *pos);
        reverse(it + 1, end);

        return true;
    }
};

//------------------------------
#define countOf(a) int(sizeof(a) / sizeof(a[0]))

static void printResult(vector<vector<int>> &result) {
    cout << "#######" << endl;
    for (auto &v : result) {
        for (auto i : v) cout << i << ',';
        cout << endl;
    }
}

int main() {
    Solution s;
    printResult(s.permuteUnique(vector<int>{}));
    printResult(s.permuteUnique(vector<int>{1}));
    printResult(s.permuteUnique(vector<int>{1, 2}));
    printResult(s.permuteUnique(vector<int>{1, 2, 3}));
    printResult(s.permuteUnique(vector<int>{1, 1, 2}));
    printResult(s.permuteUnique(vector<int>{1, 1, 1}));
}
