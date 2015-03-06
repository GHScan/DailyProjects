#include "stdafx.h"

//------------------------------
#include <algorithm>
#include <functional>

class Solution {
public:
    vector<vector<int> > combinationSum(vector<int> &candidates, int target) {
        vector<vector<int>> result;
        if (candidates.empty()) return result;

        sort(candidates.begin(), candidates.end(), greater<int>());

        vector<int> path;
        combination(result, path, &candidates[0], &candidates[0] + candidates.size(), target);
        return result;
    }
private:
    void combination(vector<vector<int>> &result, vector<int> &path, int *beginCandi, int *endCandi, int target) {
        if (target == 0) {
            if (!path.empty()) result.push_back(vector<int>(path.rbegin(), path.rend()));
            return;
        }
        if (beginCandi == endCandi) return;

        int count = 0;
        for (; target >= 0; ++count) {
            combination(result, path, beginCandi + 1, endCandi, target);
            path.push_back(*beginCandi);
            target -= *beginCandi;
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
    printResult(s.combinationSum(vector<int>{ }, 0));
    printResult(s.combinationSum(vector<int>{ 1,}, 0));
    printResult(s.combinationSum(vector<int>{ }, 3));
    printResult(s.combinationSum(vector<int>{ 1, }, 3));
    printResult(s.combinationSum(vector<int>{ 1, 2, 3,}, 3));
    printResult(s.combinationSum(vector<int>{ 2, 3, 6, 7 }, 7));
}
