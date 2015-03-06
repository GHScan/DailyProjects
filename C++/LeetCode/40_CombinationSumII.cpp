#include "stdafx.h"

//------------------------------
#include <algorithm>
#include <functional>

class Solution {
public:
    vector<vector<int> > combinationSum2(vector<int> &num, int target) {
        vector<vector<int>> result;
        if (num.empty()) return result;

        sort(num.begin(), num.end(), greater<int>());

        vector<pair<int, int>> numCounts;
        for (int i = 0; i < (int)num.size(); ) {
            int n = num[i];
            int count = 1;
            for (++i; i < (int)num.size() && num[i] == n; ++i, ++count);
            numCounts.push_back(make_pair(n, count));
        }

        vector<int> path;
        combination(result, path, &numCounts[0], &numCounts[0] + numCounts.size(), target);
        return result;
    }
private:
    void combination(vector<vector<int>> &result, vector<int> &path, pair<int, int> *begin, pair<int, int> *end, int target) {
        if (target == 0) {
            if (!path.empty()) result.push_back(vector<int>(path.rbegin(), path.rend()));
            return;
        }
        if (begin == end) return;

        int num = begin->first, maxCount = begin->second;
        int count = 0;
        for (; target >= 0 && count <= maxCount; ++count) {
            combination(result, path, begin + 1, end, target);
            path.push_back(num);
            target -= num;
        }

        while (count-- > 0) path.pop_back();
    }
};

//------------------------------
static void printResult(vector<vector<int>> const &result) {
    cout << "######" << endl;
    for (auto &v : result) {
        for (auto i : v) cout << i << ',';
        cout << endl;
    }
}

int main() {
    Solution s;
    printResult(s.combinationSum2(vector<int>{}, 0));
    printResult(s.combinationSum2(vector<int>{}, 3));
    printResult(s.combinationSum2(vector<int>{3}, 0));
    printResult(s.combinationSum2(vector<int>{1, 1, 1, 1,}, 2));
    printResult(s.combinationSum2(vector<int>{1, 1, 2, 2}, 3));
    printResult(s.combinationSum2(vector<int>{10, 1, 2, 7, 6, 1, 5}, 8));
}
